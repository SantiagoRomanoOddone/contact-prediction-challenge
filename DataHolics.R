rm(list=ls())

##~~~~~~~~~~~~~ Se importan las librerías ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(xgboost)
library(data.table)
library(tm)
library(Matrix)
library(dplyr)
library(ggplot2)
library(clue)
library(viridis)
library(stargazer)

setwd('/Users/santiagoromano/Documents/Mim/MineriaDatos/MdD_TP1')

#Se importan las funciones definidas en el sript "functions.R"
source("functions.R")

##~~~~~~~~~~~~~ Se cargan los datos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA_PATH <- "/Users/santiagoromano/Documents/Mim/MineriaDatos/MdD_TP1/competition_data/"

#Se cargan los datos desde el 2021
ads_data <- load_competition_data(DATA_PATH, sample_ratio = 1 , from_when = '2021')

#Se eliminan los datos de segunda mitad de Junio 2022 

#Se remueven los registros del dia + = a 16
ads_data <- ads_data[!((strftime(ads_data$created_on, "%Y-%m", tz="UTC") == "2022-06") & (strftime(ads_data$created_on, "%d", tz="UTC") >= 16)),]

#Se transforma la variable a predecir en 1 o 0, dependiendo de la cantidad de contactos
ads_data$contacts <- ifelse(ads_data$contacts >= 3, 1, 0)

# Se evalua el desbalanceo
mean(ads_data$contacts, na.rm= TRUE)

##~~~~~~~~~~~~~ Se guarda una variable que identifique training, validation y testing ~~~~~~~~~~~~~

ads_data$train_val_eval <- ifelse(ads_data$created_on >= strptime("2022-07-01", format = "%Y-%m-%d", tz = "UTC"), "eval", "train")

#Se define el conjunto de validación como 5% final de los datos de entrenamiento para validar los modelos
n_train <- sum(ads_data$train_val_eval == "train")  # Número de registros con etiqueta "train"
n_valid <- round(0.05 * n_train)  # Número de registros a asignar como "valid"

#Se obtienen los índices de los registros que cumplen la condición y se le asigna la etiqueta "valid"
idx_train_valid <- which(ads_data$train_val_eval == "train")
idx_to_assign_valid <- tail(idx_train_valid, n_valid)
ads_data[idx_to_assign_valid, "train_val_eval"] <- "valid"


##~~~~~~~~~~~~~ Se aplica feature engineering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Se crean atributos a partir de la fecha para poder captar comportamientos estilo estacionalidades
ads_data$day <- as.integer(strftime(ads_data$created_on, format = "%d", tz = "UTC")) #Dia
ads_data$month <- as.integer(strftime(ads_data$created_on, format = "%m", tz = "UTC")) #Mes
ads_data$year <- as.integer(strftime(ads_data$created_on, format = "%Y", tz = "UTC")) #Anio
ads_data$week_day <- as.integer(strftime(ads_data$created_on, format = "%w", tz = "UTC")) #Dia de la semana
ads_data$year_week <- as.integer(strftime(ads_data$created_on, format = "%W", tz = "UTC")) #Semana del anio


dataplot <- ads_data %>%
  filter(train_val_eval == "train") %>% group_by(year, month) %>% summarise(prop_contacts = mean(contacts))
dataplot$prop_contacts <- dataplot$prop_contacts * 100
dataplot$date <- strftime(paste("2000",dataplot$month,"01", sep = "-"), "%Y-%m-%d", tz = "UTC")

ggplot(dataplot, 
      aes(x = as.Date(date), y = prop_contacts, fill = factor(year))) +
 geom_col(alpha = 0.5, position="dodge2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m", expand = c(0.01, 0)) + 
  theme_bw() + labs(x = "Mes", y = "% Contactos" ,fill = "Year")+
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Estacionalidad mensual")

#Se analiza la variabilidad de los atributos
mean(is.na(ads_data$place_l6)) #place_l6 tiene 96% de NA
mean(is.na(ads_data$place_l5)) #place_l5 tiene 84% de NA

#Si existe NA se coloca 0 sinó 1
ads_data$place_l6 <- ifelse(is.na(ads_data$place_l5), 0, 1)
ads_data$place_l5 <- ifelse(is.na(ads_data$place_l6), 0, 1)

#Se agregan combinaciones de atributos y ratios
ads_data <- ads_data %>% mutate(bed_and_bath = bathrooms + bedrooms,
                                rooms_bed = rooms/bedrooms,
                                rooms_bath = rooms/bathrooms,
                                bed_bath = bedrooms/bathrooms,
                                surfacecovered_surfacetotal = surface_covered/surface_total,
                                surfacecovered_rooms = surface_covered/rooms,
                                surfaceuncovered =  surface_total-surface_covered,
                                price_m2_covered = price_usd/surface_covered,
                                price_m2 = price_usd/surface_total,
                                price_m2_uncovered = price_usd/surfaceuncovered,
                                price_bath = price_usd/bathrooms,
                                price_bedrooms = price_usd/bedrooms,
                                price_bed_and_bath = price_usd/bed_and_bath,
                                price_rooms = price_usd/rooms)

ratios = c("rooms_bed", "rooms_bath", "bed_bath", "surfacecovered_surfacetotal", "surfacecovered_rooms",
           "surfaceuncovered","price_m2_covered", "price_m2","price_m2_uncovered","price_bath",
           "price_bedrooms","price_bed_and_bath","price_rooms")

#Algunos denomidadores son 0, se corrijen los Inf
ads_data[, ratios] <- lapply(ads_data[, ..ratios], function(x) ifelse(is.infinite(x), NA, x))

#Se aplica Bin-counting en description
ads_data$pileta <- grepl(paste(c("pileta", "piscina"), collapse = "|"), tolower(ads_data$description))
ads_data$luminoso <- grepl(paste(c("luminoso", "vista"), collapse = "|"), tolower(ads_data$description))
ads_data$amplio <- grepl(paste(c("amplio", "espacioso"), collapse = "|"), tolower(ads_data$description))
ads_data$ubicacion <- grepl(paste(c("Ubicación", "Ubicado" , 'Ubicada'), collapse = "|"), chartr("ÁÉÍÓÚ", "AEIOU",tolower(ads_data$description)))
ads_data$estacionamiento <- grepl(paste(c("estacionamiento", "garaje"), collapse = "|"), tolower(ads_data$description))
ads_data$vista <- grepl(paste(c("vista", "terraza" ), collapse = "|"), tolower(ads_data$description))
ads_data$jardin <- grepl(paste(c("jardin", "patio" , 'Ubicada'), collapse = "|"), chartr("ÁÉÍÓÚ", "AEIOU",tolower(ads_data$description)))                        
ads_data$seguridad <- grepl(paste(c("seguridad", "seguro", 'segura'), collapse = "|"), tolower(ads_data$description))
ads_data$amueblado <- grepl(paste(c("amueblado", "equipado"), collapse = "|"), tolower(ads_data$description))

#Se aplica Bin-counting en short_description
ads_data$pileta_short <- grepl(paste(c("pileta", "piscina"), collapse = "|"), tolower(ads_data$short_description))
ads_data$luminoso_short <- grepl(paste(c("luminoso", "vista"), collapse = "|"), tolower(ads_data$short_description))
ads_data$amplio_short <- grepl(paste(c("amplio", "espacioso"), collapse = "|"), tolower(ads_data$short_description))
ads_data$ubicacion_short <- grepl(paste(c("Ubicación", "Ubicado" , 'Ubicada'), collapse = "|"), chartr("ÁÉÍÓÚ", "AEIOU",tolower(ads_data$short_description)))
ads_data$estacionamiento_short <- grepl(paste(c("estacionamiento", "garaje"), collapse = "|"), tolower(ads_data$short_description))
ads_data$vista_short <- grepl(paste(c("vista", "terraza" ), collapse = "|"), tolower(ads_data$short_description))
ads_data$jardin_short <- grepl(paste(c("jardin", "patio" , 'Ubicada'), collapse = "|"), chartr("ÁÉÍÓÚ", "AEIOU",tolower(ads_data$short_description)))                        
ads_data$seguridad_short <- grepl(paste(c("seguridad", "seguro", 'segura'), collapse = "|"), tolower(ads_data$short_description))
ads_data$amueblado_short <- grepl(paste(c("amueblado", "equipado"), collapse = "|"), tolower(ads_data$short_description))

#Se aplica Bin-counting en title
ads_data$departamento_title <- grepl(paste(c("departamento", "aepartamento"), collapse = "|"), tolower(ads_data$title))
ads_data$casa_title <- grepl("casa", tolower(ads_data$title))
ads_data$venta_title <- grepl(paste(c("venta", "oportunidad", "oferta"), collapse = "|"), tolower(ads_data$title))
ads_data$alquiler_title <- grepl(paste(c("alquiler", "temporario"), collapse = "|"), tolower(ads_data$title))

#Se le aplica logaritmo a price_usd
ads_data$price_usd <- ifelse(ads_data$price_usd > 0, log(ads_data$price_usd), NA)

#Se crean atributos utilizando aprendizaje no supervisado
train_data <- ads_data[ads_data$train_val_eval == "train"]

#Se revisan los NA
mean(is.na(train_data$price_usd)) #0.01302779
mean(is.na(train_data$price_bedrooms)) #0.3321437
mean(is.na(train_data$lat)) # 0.104975
mean(is.na(train_data$lon)) #0.104973
mean(is.na(train_data$price_bed_and_bath)) #0.0.3534283
mean(is.na(train_data$operation_venta))
mean(is.na(train_data$operation_alquileres))
mean(is.na(train_data$property_type_departamento))
mean(is.na(train_data$property_type_casa))
mean(is.na(train_data$property_type_lote))
mean(is.na(train_data$property_type_ocal))
mean(is.na(train_data$property_type_oficina))

gc()

kmeans_columns_to_keep <- c('price_usd','lat','lon', 'operation_venta', 'operation_alquileres',
                            'price_bedrooms')

train_data_clusters <- train_data[, ..kmeans_columns_to_keep]
train_data_clusters <- train_data_clusters[complete.cases(train_data_clusters)]

#Se escala y se eliminan los outliers
train_data_clusters <- as.data.frame(sapply(train_data_clusters, function(data) (abs(data-mean(data))/sd(data))))    
#Se saca si es mayor a 4 por los outliers 
train_data_clusters <- train_data_clusters[rowSums(train_data_clusters>4) == 0,]

kmeans_results <- find_k_means(train_data_clusters,5,20)

plot(c(1:18), kmeans_results$var, type="o",
     xlab="# Clusters", ylab="tot.withinss")

#Se enterna KMeans
clusters_model <- kmeans(train_data_clusters,
                   centers=6, iter.max=3000, nstart=10)

#Se evalúa la diferencia entre grupos
train_data$cluster <- factor(cl_predict(clusters_model, train_data[, ..kmeans_columns_to_keep]))


train_data %>% group_by(cluster) %>% summarise(prom_contactos = mean(contacts, na.rm=TRUE),
                                  cant_obs = n(),
                                  cant_obs_pct = n()/nrow(.))

#Se predice el cluster para todo el dataframe
ads_data$cluster <- factor(cl_predict(clusters_model, ads_data[, ..kmeans_columns_to_keep]))

#Se pasa la fecha a numeric
ads_data$created_on <- as.numeric(ads_data$created_on)

##~~~~~~~~~~~~~ Se aplica one_hot_encoding y se pasa a matriz rala ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
train_val_eval <- ads_data$train_val_eval
# Se quitan los atributos innecesario
cols_to_delete <- c("ad_id", "date", "title", "description", "short_description", "development_name","train_val_eval"
                    , 'currency_id')
columns_to_keep <- setdiff(names(ads_data), cols_to_delete)

ads_data_sparse <- one_hot_sparse(ads_data[, ..columns_to_keep])
gc()

#saveRDS(ads_data_sparse,"feature_eng_only\\modelo_v4_dataholics.RDS")
#ads_data_sparse <- readRDS("feature_eng_only\\modelo_v4_dataholics.RDS")

##~~~~~~~~~~~~~ Se entrena modelo de xgboost ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dtrain <- xgb.DMatrix(data = ads_data_sparse[train_val_eval == "train", colnames(ads_data_sparse) != "contacts"],
                      label = ads_data_sparse[train_val_eval == "train", colnames(ads_data_sparse) == "contacts"])

dvalid <- xgb.DMatrix(data = ads_data_sparse[train_val_eval == "valid", colnames(ads_data_sparse) != "contacts"],
                      label = ads_data_sparse[train_val_eval == "valid", colnames(ads_data_sparse) == "contacts"])

rgrid <- random_grid(size = 7,
                     min_nrounds = 180, max_nrounds = 230, #cantidad de arboles (0,Inf)
                     min_max_depth = 14, max_max_depth = 18, #profundidad de los arboles (0,Inf)
                     min_eta = 0.1, max_eta = 0.15, #learning rate (0,1]
                     min_gamma = 3, max_gamma = 6, #minima reduccion del error necesaria para generar corte (0,Inf)
                     min_min_child_weight = 10, max_min_child_weight = 16, #numero minimo de obs en una hoja para crear hijo (0,Inf)
                     min_colsample_bytree = 0.6, max_colsample_bytree = 1, #columnas sampleadas por arbol (0,1]
                     min_subsample = 0.5, max_subsample = 0.75) #observaciones sampleadas por arbol (0,1]


predicted_models <- train_xgboost(dtrain, dvalid, rgrid)

#Se guardan los resultados en un dataframe de una forma comoda de verlo
res_table <- result_table(predicted_models)
print(res_table)

melted_res_table <- melt(res_table[, !names(res_table) %in% c("perf_tr")], id.vars = c("i", "perf_vd"))
ggplot(melted_res_table, aes(x = value, y = perf_vd)) + 
  geom_line() + 
  facet_wrap(variable ~ ., scales = "free") +
  theme_bw()

#Se elige el mejor modelo
best_model <- predicted_models[[res_table[1,"i"]]]$model

#Se analizan las variables con mayor poder predictivo
importance_matrix = xgb.importance(colnames(dtrain), model = best_model)
xgb.plot.importance(importance_matrix[1:30,])

##~~~~~~~~~~~~~ Se rentrena con train + validation para predecir eval ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dall <- xgb.DMatrix(data = ads_data_sparse[train_val_eval != "eval", colnames(ads_data_sparse) != "contacts"],
                    label = ads_data_sparse[train_val_eval != "eval", colnames(ads_data_sparse) == "contacts"])
final_model <- xgb.train(data = dall,
                         nrounds = res_table[1, "nrounds"],
                         params=as.list(res_table[1, c("max_depth",
                                                       "eta",
                                                       "gamma",
                                                       "colsample_bytree",
                                                       "subsample",
                                                       "min_child_weight")]),
                         watchlist = list(train = dall),
                         objective = "binary:logistic",
                         eval.metric = "auc",
                         print_every_n = 10)

#saveRDS(final_model, "final_model_dataholics.RDS")
#final_model <- readRDS("final_model_dataholics.RDS")

##Se predice en eval y se guarda submissions
preds <- data.frame(ad_id = ads_data[train_val_eval == "eval", "ad_id"],
                    contacts = predict(final_model,
                                       ads_data_sparse[train_val_eval == "eval", colnames(ads_data_sparse) != "contacts"]))


#Se arma para subir a Kaggle
options(scipen=10)
write.table(preds, "modelo_dataholic_v3.csv", sep=",", row.names=FALSE, quote=FALSE)
options(scipen=0)

