# contact-prediction-challenge

ROMANOODDONE is a real estate portal that offers a novel solution for both real estate agencies and individual property owners looking to sell or rent properties. Operating in several countries, including Argentina, Colombia, Ecuador, Peru, and Uruguay, ROMANOODDONE establishes partnerships with major real estate agencies, agents, and construction companies to list their properties. Their unique business model revolves around delivering high-quality leads. They charge advertisers based on the actual contacts they receive from potential buyers or renters rather than traditional advertising formats like banners or pop-ups. This approach aligns the incentives of sellers, users, and ROMANOODDONE, resulting in a clean and user-friendly platform.

The goal of the competition is to develop a model that predicts whether real estate listings published in July, August, and September 2022 will receive at least three contacts during the first 15 days of publication. ROMANOODDONE, a real estate portal, is looking to improve its services by forecasting listing performance, which could help them better target their marketing efforts and communicate the expected results to their partner agencies.

To build your predictive model, you have access to several datasets:

**Ads Data (132 files):** These files contain information about ROMANOODDONE listings published in Argentina, Colombia, Ecuador, and Peru during 2020, 2021, and part of 2022 (up to 2022-09-15). Each file represents data for a given month and country, with each record corresponding to a specific property listing. These files contain predictor variables that you should use to make predictions.

**Contacts Data (1 file):** This file contains ad IDs and the number of contacts each ad received in the first 15 days of publication. It only includes information for listings created before June 16, 2022. You will need to merge the ads data with this contacts data to generate your training dataset.

**Sample Submission (1 file):** This is an example of how your submission should be structured for Kaggle's platform. It should have two columns: ad_id (listing IDs for your predictions) and contacts (the predicted probability that a listing will receive at least three contacts in the first 15 days).

The ads data consists of 26 variables that can be used as predictors. Here's an overview of these variables:

**ad_id:** Listing ID.
operation: Purpose of the listing (e.g., sale, rent, development).
**place_l1:** Country.
**place_l2:** Province.
**place_l3:** Neighborhood.
**place_l4:** More detailed location information.
**place_l5:** Even more detailed location information.
**place_l6:** Further detailed location information.
**lat:** Latitude of the property.
**lon:** Longitude of the property.
**price:** Published price.
**currency_id:** Currency of the published price.
**price_usd:** Price in US dollars.
**rooms:** Number of rooms (optional).
**bedrooms:** Number of bedrooms (optional).
**bathrooms:** Number of bathrooms (optional).
**surface_total:** Total square meters of the property.
**surface_covered:** Covered square meters of the property.
**title:** Listing title.
**description:** Listing description.
**property_type:** Property type (e.g., apartment, house).
**created_on:** Publication date.
**development_name:** Name of the real estate development (if applicable).
**current_state:** Condition of the real estate development.
**short_description:** Brief listing description.
**property_is_development:** Indicates whether the property belongs to a real estate development.
You need to use these features from the ads data to predict whether a listing will receive at least three contacts in the first 15 days.
