load_single_ads_file <- function(ads_file, sample_ratio, drop_cols, sel_cols) {
  
  print(paste0("Loading: ", ads_file))
  
  dt <- fread(ads_file, header = TRUE, quote = "\"",
              stringsAsFactors = TRUE, na.strings = "",
              drop = drop_cols, select = sel_cols,
              encoding = "UTF-8",
              showProgress = FALSE)
  
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * nrow(dt))
    dt <- dt[sample(.N, sample_size)]
  }
  
  return(dt)
}

#Se pasa el directorio donde estan los archivos
load_ads_data_files <- function(ads_dir, sample_ratio,
                                drop_cols, sel_cols,
                                from_when) {
  #Se ordena por temporalidad
  ads_files <- sort(dir(ads_dir))
  
  # si hay un from_when parte de ahi 
  if (!is.null(from_when)) {
    ads_files <- ads_files[ads_files >= from_when]
  }
  
  ads_data <- list()
  for (ads_file in ads_files) {
    if (substr(ads_file, 1, 7) >= "2022_07") {
      sample_ratio <- 1
    }
    file_loc <- paste0(ads_dir, ads_file)
    ads_data[[length(ads_data) + 1]] <- load_single_ads_file(file_loc, sample_ratio, drop_cols, sel_cols)
  }
  
  ads_data <- rbindlist(ads_data)
  ads_data$ad_id <- as.numeric(as.character(ads_data$ad_id))
  
  return(ads_data)
}

load_competition_data <- function(comp_dir, sample_ratio = 1,
                                  drop_cols = NULL, sel_cols = NULL,
                                  from_when = NULL) {
  
  ads_data <- load_ads_data_files(paste0(DATA_PATH, "ads_data/"),
                                  sample_ratio = sample_ratio,
                                  sel_cols = sel_cols,
                                  drop_cols = drop_cols,
                                  from_when = from_when)
  
  contacts <- fread(paste0(DATA_PATH, "train_contacts.csv"), sep=",")
  contacts$ad_id <- as.numeric(as.character(contacts$ad_id))
  ads_data <- merge(ads_data, contacts[,c("ad_id", "contacts")], by="ad_id", all.x=TRUE)
  ads_data$contacts <- ifelse(!is.na(ads_data$contacts), ads_data$contacts, 0)
  ads_data$contacts <- ifelse(ads_data$created_on < strptime("2022-07-01",
                                                             format = "%Y-%m-%d",
                                                             tz = "UTC"),
                              ads_data$contacts, NA)
  
  return(ads_data)
}

gen_bog_vars <- function(char_vector, min_wordLengths, min_bound) {
  
  corpus <- VCorpus(VectorSource(char_vector))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) removePunctuation(x, ucp = TRUE)))
  dt.mat <- DocumentTermMatrix(corpus,
                               control = list(stopwords = FALSE,
                                              wordLengths = c(min_wordLengths, Inf),
                                              bounds = list(global=c(min_bound, Inf))))
  
  var_names <- paste("title", colnames(dt.mat), sep="_")
  dt.mat <- sparseMatrix(i=dt.mat$i, j=dt.mat$j, x=dt.mat$v, dims = dim(dt.mat))
  colnames(dt.mat) <- var_names
  return(dt.mat)
}

find_k_means <- function(dataframe,start_k,finish_k,iter.max=30,  nstart=20,algorithm="MacQueen"){
  evol_variabilidad <- data.frame()
  for (i in c(start_k:finish_k)) {
    clusters <-kmeans(dataframe, centers=i, iter.max=iter.max,  nstart=nstart,algorithm=algorithm)
    evol_variabilidad <- rbind(evol_variabilidad,
                               data.frame(k=i,
                                          var=clusters$tot.withinss))
  }
  return(evol_variabilidad)
}

one_hot_sparse <- function(data_set) {
  
  data_set <- as.data.table(data_set)
  
  require(Matrix)
  
  created <- FALSE
  
  if (sum(sapply(data_set, is.numeric)) > 0) {  # Si hay, se pasa los numéricos a una matriz esparsa
    out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
    created <- TRUE
  }
  
  if (sum(sapply(data_set, is.logical)) > 0) {  # Si hay, se pasa los lógicos a esparsa y lo unimos con la matriz anterior
    if (created) {
      out_put_data <- cbind2(out_put_data,
                             as(as.matrix(data_set[,sapply(data_set, is.logical),
                                                   with = FALSE]), "dgCMatrix"))
    } else {
      out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
      created <- TRUE
    }
  }
  
  #Se identifican las columnas que son factor (OJO: el data.frame no debería tener character)
  fact_variables <- names(which(sapply(data_set, is.factor)))
  
  # Para cada columna factor se aplica one hot encoding
  i <- 0
  
  for (f_var in fact_variables) {
    
    f_col_names <- levels(data_set[[f_var]])
    f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
    j_values <- as.numeric(data_set[[f_var]])  # Se pone como valor de j, el valor del nivel del factor
    
    if (sum(is.na(j_values)) > 0) {  # En categóricas, se trata a NA como una categoría más
      j_values[is.na(j_values)] <- length(f_col_names) + 1
      f_col_names <- c(f_col_names, paste(f_var, "NA", sep = "_"))
    }
    
    if (i == 0) {
      fact_data <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                x = rep(1, nrow(data_set)),
                                dims = c(nrow(data_set), length(f_col_names)))
      fact_data@Dimnames[[2]] <- f_col_names
    } else {
      fact_data_tmp <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                    x = rep(1, nrow(data_set)),
                                    dims = c(nrow(data_set), length(f_col_names)))
      fact_data_tmp@Dimnames[[2]] <- f_col_names
      fact_data <- cbind(fact_data, fact_data_tmp)
    }
    
    i <- i + 1
  }
  
  if (length(fact_variables) > 0) {
    if (created) {
      out_put_data <- cbind(out_put_data, fact_data)
    } else {
      out_put_data <- fact_data
      created <- TRUE
    }
  }
  return(out_put_data)
}


random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {
  
  rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
    rep(min_nrounds, size)
  } else {
    sample(c(min_nrounds:max_nrounds),
           size = size, replace = TRUE)
  },
  max_depth = if (min_max_depth == max_max_depth) {
    rep(min_max_depth, size)
  } else {
    sample(c(min_max_depth:max_max_depth),
           size = size, replace = TRUE)
  },
  eta = if (min_eta == max_eta) {
    rep(min_eta, size)
  } else {
    round(runif(size, min_eta, max_eta), 7)
  },
  gamma = if (min_gamma == max_gamma) {
    rep(min_gamma, size)
  } else {
    round(runif(size, min_gamma, max_gamma), 7)
  },
  colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
    rep(min_colsample_bytree, size)
  } else {
    round(runif(size, min_colsample_bytree, max_colsample_bytree), 7)
  },
  min_child_weight = if (min_min_child_weight == max_min_child_weight) {
    rep(min_min_child_weight, size)
  } else {
    round(runif(size, min_min_child_weight, max_min_child_weight), 7)
  },
  subsample = if (min_subsample == max_subsample) {
    rep(min_subsample, size)
  } else {
    round(runif(size, min_subsample, max_subsample), 7)
  })
  
  return(rgrid)
}


train_xgboost <- function(data_train, data_val, rgrid) {
  watchlist <- list(train = data_train, valid = data_val)
  predicted_models <- list()
  
  for (i in seq_len(nrow(rgrid))) {
    print(i)
    print(rgrid[i,])
    
    trained_model <- xgb.train(data = data_train,
                               params=as.list(rgrid[i, c("max_depth",
                                                         "eta",
                                                         "gamma",
                                                         "colsample_bytree",
                                                         "subsample",
                                                         "min_child_weight")]),
                               nrounds = rgrid[i, "nrounds"],
                               watchlist = watchlist,
                               objective = "binary:logistic",
                               eval.metric = "auc",
                               print_every_n = 10)
    
    perf_tr <- tail(trained_model$evaluation_log, 1)$train_auc
    perf_vd <- tail(trained_model$evaluation_log, 1)$valid_auc
    print(c(perf_tr, perf_vd))
    
    predicted_models[[i]] <- list(results = data.frame(rgrid[i,],
                                                       perf_tr = perf_tr,
                                                       perf_vd = perf_vd),
                                  model = trained_model)
    
    rm(trained_model)
    gc()
  }
  return(predicted_models)
}


result_table <- function(pred_models, less_is_better = FALSE) {
  
  if (less_is_better == TRUE) {
    order_coef <- 1
  } else {
    order_coef <- -1
  }
  
  res_table <- data.frame()
  i <- 1
  
  for (m in pred_models) {
    res_table <- rbind(res_table, data.frame(i = i, m$results))
    i <- i + 1
  }
  
  res_table <- res_table[order(order_coef * res_table$perf_vd),]
  
  return(res_table)
}

