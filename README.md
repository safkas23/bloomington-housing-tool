# Bloomington Housing Price + Rent Prediction Tool

A machine learning pipeline predicting median rent across Monroe County, Indiana census tracts using U.S. Census ACS data.

## Hypothesis
Proximity to Indiana University campus is a significant predictor of rental prices in Bloomington, IN.

## Result
Hypothesis supported. Lasso regression (best model, RMSE = 0.099) retained both campus zone variables with negative coefficients — tracts farther from IU predict significantly lower rent.

## Scripts (run in order)
- `01_data_pull.R` — pulls ACS data via tidycensus, computes distance to IU
- `02_cleaning.R` — handles missing values, engineers features
- `03_eda.R` — exploratory analysis and visualizations
- `04_modeling.R` — trains lasso, random forest, XGBoost, linear regression; lasso wins
- `05_interpretability.R` — coefficient analysis, hypothesis test, dependence plots

## Data
U.S. Census Bureau ACS 2018–2022 5-year estimates, Monroe County, IN (33 census tracts)

## App
Live Shiny app: https://safkas23.shinyapps.io/bloomington-housing-tool
