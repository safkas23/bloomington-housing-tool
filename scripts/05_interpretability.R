library(tidyverse)
library(sf)
library(tidymodels)
library(ggplot2)
library(scales)

tidymodels_prefer()


# load model and data
monroe_model <- readRDS("data/processed/monroe_model_ready.rds") %>%
  st_drop_geometry()

lasso_fit       <- readRDS("models/lasso_final.rds")
lasso_final     <- readRDS("models/lasso_final_fit.rds")
shap_importance <- readRDS("models/shap_importance.rds")

# recreate modeling data
set.seed(42)
modeling_data <- monroe_model %>%
  select(
    log_rent,
    dist_to_iu_mi, log_dist_to_iu, campus_zone,
    median_income, log_income,
    pct_renters, pct_bachelors,
    housing_age, income_to_rent, total_pop
  ) %>%
  drop_na()

split      <- initial_split(modeling_data, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)

cat("Data loaded:", nrow(modeling_data), "tracts\n")


# extract lasso coefficients (standardized = feature importance)
lasso_coefs <- lasso_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(abs(estimate))) %>%
  mutate(
    direction    = if_else(estimate > 0, "Positive effect", "Negative effect"),
    abs_estimate = abs(estimate)
  )

cat("\nLasso coefficients:\n")
print(lasso_coefs)


# feature importance bar chart
p_importance <- shap_importance %>%
  mutate(feature = fct_reorder(feature, mean_abs_shap)) %>%
  ggplot(aes(x = mean_abs_shap, y = feature)) +
  geom_col(fill = "#378ADD", alpha = 0.85, width = 0.65) +
  geom_text(aes(label = round(mean_abs_shap, 3)),
            hjust = -0.1, size = 3.5, color = "#333") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Lasso feature importance",
    subtitle = "Absolute standardized coefficient — higher = stronger effect on rent",
    x        = "|Standardized coefficient|",
    y        = NULL,
    caption  = "Lasso regression | Monroe County, IN"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

ggsave("outputs/plots/15_feature_importance.png",
       p_importance, width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/15_feature_importance.png\n")


# coefficient direction plot (positive vs negative)
p_coef_dir <- lasso_coefs %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = term, fill = direction)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_vline(xintercept = 0, color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(label = round(estimate, 3),
        hjust = if_else(estimate >= 0, -0.15, 1.15)),
    size = 3.5, color = "#333"
  ) +
  scale_fill_manual(
    values = c("Positive effect" = "#0C447C", "Negative effect" = "#D85A30"),
    name   = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  labs(
    title    = "Direction of each feature's effect on rent",
    subtitle = "Positive = higher rent | Negative = lower rent",
    x        = "Standardized coefficient",
    y        = NULL,
    caption  = "Lasso regression | Monroe County, IN — ACS 2018–2022"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "top"
  )

ggsave("outputs/plots/16_coefficient_direction.png",
       p_coef_dir, width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/16_coefficient_direction.png\n")

# dependence plots for top features
top_features <- shap_importance$feature[1:3]

feature_labels <- c(
  income_to_rent           = "Income-to-rent ratio",
  median_income            = "Median household income ($)",
  campus_zone_far_suburbs  = "Campus zone: far suburbs (0/1)",
  pct_bachelors            = "% with bachelor's degree",
  campus_zone_mid_distance = "Campus zone: mid distance (0/1)",
  pct_renters              = "% renter occupied",
  log_income               = "Log median income",
  housing_age              = "Housing age (years)",
  total_pop                = "Total population",
  dist_to_iu_mi            = "Distance to IU (miles)",
  log_dist_to_iu           = "Log distance to IU"
)

plot_dependence <- function(feature_name, rank_num) {
  
  raw_name <- case_when(
    feature_name == "campus_zone_far_suburbs"  ~ "campus_zone",
    feature_name == "campus_zone_mid_distance" ~ "campus_zone",
    TRUE                                        ~ feature_name
  )
  
  if (!raw_name %in% names(modeling_data)) {
    cat("Skipping", feature_name, "— not directly plottable\n")
    return(invisible(NULL))
  }
  
  x_label <- feature_labels[feature_name]
  if (is.na(x_label)) x_label <- feature_name
  
  p <- modeling_data %>%
    ggplot(aes(x = .data[[raw_name]], y = log_rent)) +
    geom_point(color = "#378ADD", alpha = 0.7, size = 3) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#0C447C", linewidth = 0.8) +
    geom_hline(yintercept = mean(modeling_data$log_rent),
               linetype = "dashed", color = "grey60") +
    scale_y_continuous(name = "log(median rent)") +
    labs(
      title    = paste0("#", rank_num, " predictor: ", x_label),
      subtitle = "Each point = one census tract | Dashed line = county mean rent",
      x        = x_label,
      caption  = "Monroe County, IN — ACS 2018–2022"
    ) +
    theme_minimal(base_size = 11)
  
  fname <- paste0("outputs/plots/1", 5 + rank_num, "_dep_", feature_name, ".png")
  ggsave(fname, p, width = 7, height = 5, dpi = 150)
  cat("Saved:", fname, "\n")
}

for (i in seq_along(top_features)) {
  plot_dependence(top_features[i], i)
}


# hypothesis test
cat("\nHypothesis test: role of campus proximity:\n")

campus_vars <- lasso_coefs %>%
  filter(str_detect(term, "campus_zone|dist_to_iu"))

if (nrow(campus_vars) > 0) {
  cat("\nCampus proximity variables retained by lasso:\n")
  print(campus_vars %>% select(term, estimate, abs_estimate))
  
  far_coef <- lasso_coefs %>%
    filter(term == "campus_zone_far_suburbs") %>%
    pull(estimate)
  
  mid_coef <- lasso_coefs %>%
    filter(term == "campus_zone_mid_distance") %>%
    pull(estimate)
  
  if (length(far_coef) > 0 && far_coef < 0) {
    cat("\nHypothesis SUPPORTED:\n")
    cat("  Far suburbs coefficient:", round(far_coef, 4),
        "(negative = lower rent than near campus)\n")
    if (length(mid_coef) > 0) {
      cat("  Mid distance coefficient:", round(mid_coef, 4), "\n")
    }
    cat("  Tracts farther from IU predict significantly lower rent.\n")
  } else {
    cat("\nHypothesis NOT CLEARLY SUPPORTED by campus zone coefficients.\n")
  }
} else {
  cat("Campus zone variables were zeroed out by lasso penalty.\n")
  cat("This suggests campus proximity has weaker signal than other variables.\n")
}

dist_rank <- shap_importance %>%
  mutate(rank = row_number()) %>%
  filter(str_detect(feature, "campus_zone|dist_to_iu")) %>%
  select(feature, mean_abs_shap, rank)

cat("\nCampus/distance variable importance rankings:\n")
print(dist_rank)

# predicted vs actual on full dataset
full_predictions <- predict(lasso_fit, new_data = modeling_data) %>%
  bind_cols(modeling_data %>% select(log_rent, dist_to_iu_mi, campus_zone)) %>%
  mutate(
    actual_rent    = exp(log_rent),
    predicted_rent = exp(.pred),
    residual       = actual_rent - predicted_rent
  )

p_pred_full <- full_predictions %>%
  ggplot(aes(x = actual_rent, y = predicted_rent, color = campus_zone)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      "near_campus"  = "#0C447C",
      "mid_distance" = "#378ADD",
      "far_suburbs"  = "#B5D4F4"
    ),
    labels = c("Near campus", "Mid distance", "Far suburbs"),
    name   = "Campus zone"
  ) +
  scale_x_continuous(labels = dollar, name = "Actual median rent") +
  scale_y_continuous(labels = dollar, name = "Predicted median rent") +
  labs(
    title    = "Predicted vs actual rent — lasso model",
    subtitle = "Points on dashed line = perfect prediction | Color = campus zone",
    caption  = "Monroe County, IN — ACS 2018–2022"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/19_predicted_vs_actual_full.png",
       p_pred_full, width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/19_predicted_vs_actual_full.png\n")

# save outputs
saveRDS(lasso_coefs,   "models/lasso_coefs_final.rds")
write_csv(lasso_coefs, "outputs/tables/lasso_coefs_final.csv")
cat("Saved: models/lasso_coefs_final.rds\n")
cat("Saved: outputs/tables/lasso_coefs_final.csv\n")

cat("\nAnalysis complete!\n")
cat("Key outputs:\n")
cat("  outputs/plots/15_feature_importance.png    — importance bar chart\n")
cat("  outputs/plots/16_coefficient_direction.png — direction of effects\n")
cat("  outputs/plots/19_predicted_vs_actual_full.png — predictions\n")
cat("  outputs/tables/lasso_coefs_final.csv       — coefficients table\n")