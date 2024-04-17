# contact-prediction-challenge

Properatti is a real estate portal that offers a novel solution for both real estate agencies and individual property owners looking to sell or rent properties. Operating in several countries, including Argentina, Colombia, Ecuador, Peru, and Uruguay, Properatti establishes partnerships with major real estate agencies, agents, and construction companies to list their properties. Their unique business model revolves around delivering high-quality leads. They charge advertisers based on the actual contacts they receive from potential buyers or renters rather than traditional advertising formats like banners or pop-ups. This approach aligns the incentives of sellers, users, and Properatti, resulting in a clean and user-friendly platform.

The goal of the competition is to develop a model that predicts whether real estate listings published in July, August, and September 2022 will receive at least three contacts during the first 15 days of publication. Properatti, a real estate portal, is looking to improve its services by forecasting listing performance, which could help them better target their marketing efforts and communicate the expected results to their partner agencies.

To build your predictive model, you have access to several datasets:

**Ads Data (132 files):** These files contain information about Properatti listings published in Argentina, Colombia, Ecuador, and Peru during 2020, 2021, and part of 2022 (up to 2022-09-15). Each file represents data for a given month and country, with each record corresponding to a specific property listing. These files contain predictor variables that you should use to make predictions.

**Contacts Data (1 file):** This file contains ad IDs and the number of contacts each ad received in the first 15 days of publication. It only includes information for listings created before June 16, 2022. You will need to merge the ads data with this contacts data to generate your training dataset.

**Sample Submission (1 file):** This is an example of how your submission should be structured for Kaggle's platform. It should have two columns: ad_id (listing IDs for your predictions) and contacts (the predicted probability that a listing will receive at least three contacts in the first 15 days).


**You need to use these features from the ads data to predict whether a listing will receive at least three contacts in the first 15 days!.**


## The results obtained in our validation showed a performance of 96.35%, while in the [competition](https://www.kaggle.com/competitions/contact-prediction-challenge-2023/overview), we achieved 96.23%.

Regarding the distribution of time dedicated to each stage of the process, it could be described as follows:

**Exploratory analysis and creating graphs:** 25% of the time was spent on exploring and visualizing the data to better understand its structure and relationships.

**Variable selection and feature engineering:** The most extensive stage was variable selection and creating new features, occupying 50% of the time. This includes data preparation and building relevant features to improve the model's performance.

**Model validation system:** 10% of the time was dedicated to establishing a model validation system to assess the model's performance, ensuring it is robust and accurate.

**Learning algorithms and hyperparameter optimization:** Finally, 15% of the time was used to implement learning algorithms and fine-tune hyperparameters to enhance the model's effectiveness.

**These proportions reflect a balanced and efficient working strategy throughout the model development process.**
