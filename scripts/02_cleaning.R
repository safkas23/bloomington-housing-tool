library(tidyverse)
library(sf)

# load data
monroe_wide <- readRDS("data/processed/monroe_wide.rds")

cat("Loaded", nrow(monroe_wide), "tracts\n")
cat("Columns:", paste(names(monroe_wide), collapse = ", "), "\n")


# inspect missing values
cat("\nMissing values per column:\n")
monroe_wide %>%
  st_drop_geometry() %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  print()

# handle missing values
n_before <- nrow(monroe_wide)

monroe_clean <- monroe_wide %>%
  filter(!is.na(median_rent))

n_after <- nrow(monroe_clean)
cat("\nDropped", n_before - n_after, "tract(s) with missing median_rent\n")
cat("Remaining tracts:", n_after, "\n")

# outlier check
cat("\nRent distribution:\n")
monroe_clean %>%
  st_drop_geometry() %>%
  summarise(
    min    = min(median_rent),
    q25    = quantile(median_rent, 0.25),
    median = median(median_rent),
    mean   = round(mean(median_rent)),
    q75    = quantile(median_rent, 0.75),
    max    = max(median_rent)
  ) %>%
  print()

rent_mean <- mean(monroe_clean$median_rent)
rent_sd   <- sd(monroe_clean$median_rent)

outliers <- monroe_clean %>%
  st_drop_geometry() %>%
  filter(abs(median_rent - rent_mean) > 2.5 * rent_sd) %>%
  select(GEOID, NAME, median_rent, dist_to_iu_mi)

if (nrow(outliers) > 0) {
  cat("\nPotential outlier tracts:\n")
  print(outliers)
} else {
  cat("\nNo extreme outliers detected (±2.5 SD threshold)\n")
}

# feature engineering
monroe_clean <- monroe_clean %>%
  mutate(
    
    # distance bands
    campus_zone = case_when(
      dist_to_iu_mi <= 1   ~ "near_campus",
      dist_to_iu_mi <= 3   ~ "mid_distance",
      TRUE                 ~ "far_suburbs"
    ) %>% factor(levels = c("near_campus", "mid_distance", "far_suburbs")),
    
    # housing age
    housing_age = 2022 - median_year_built,
    
    # income-to-rent ratio (affordability)
    # Monthly income vs monthly rent
    income_to_rent = (median_income / 12) / median_rent,
    
    # log rent
    log_rent = log(median_rent),
    
    # log income
    log_income = log(median_income),
    
    # log distance
    log_dist_to_iu = log(dist_to_iu_mi + 0.01)  # +0.01 avoids log(0)
    
  )

cat("\nNew features added:\n")
monroe_clean %>%
  st_drop_geometry() %>%
  count(campus_zone) %>%
  print()

cat("\nIncome-to-rent ratio (monthly income / monthly rent):\n")
monroe_clean %>%
  st_drop_geometry() %>%
  summarise(
    min    = round(min(income_to_rent, na.rm = TRUE), 2),
    median = round(median(income_to_rent, na.rm = TRUE), 2),
    max    = round(max(income_to_rent, na.rm = TRUE), 2)
  ) %>%
  print()

# select and order model columns
monroe_model <- monroe_clean %>%
  select(
    # identifiers
    GEOID,
    NAME,
    # target variables
    median_rent,
    log_rent,
    # core features
    dist_to_iu_mi,
    log_dist_to_iu,
    campus_zone,
    median_income,
    log_income,
    pct_renters,
    pct_bachelors,
    housing_age,
    income_to_rent,
    total_pop,
    geometry
  )

cat("\nFinal model-ready dataset:\n")
cat("Rows:", nrow(monroe_model), "\n")
cat("Columns:", ncol(monroe_model), "\n")
glimpse(monroe_model %>% st_drop_geometry())


# save
saveRDS(monroe_model, "data/processed/monroe_model_ready.rds")
cat("\nSaved: data/processed/monroe_model_ready.rds\n")

cat("\nCleaning complete!\n")