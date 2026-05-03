install.packages(c("tidymodels", "ranger", "xgboost", "glmnet", "vip"))
library(tidyverse)
library(sf)
library(tidymodels)
library(ranger)     
library(xgboost)     
library(glmnet)       
library(vip)          

tidymodels_prefer() 


# load data
monroe_model <- readRDS("data/processed/monroe_model_ready.rds") %>%
  st_drop_geometry()

cat("Loaded", nrow(monroe_model), "tracts\n")


# define features and outcome
modeling_data <- monroe_model %>%
  select(
    log_rent,      
    dist_to_iu_mi,    
    log_dist_to_iu,
    campus_zone,
    median_income,
    log_income,
    pct_renters,
    pct_bachelors,
    housing_age,
    income_to_rent,
    total_pop
  ) %>%
  drop_na()

cat("Modeling dataset:", nrow(modeling_data), "rows,",
    ncol(modeling_data), "columns\n")

# train/test split
set.seed(42)
split      <- initial_split(modeling_data, prop = 0.8)
train_data <- training(split)
test_data  <- testing(split)

cat("Train:", nrow(train_data), "tracts | Test:", nrow(test_data), "tracts\n")

# 5-fold cv on training data
cv_folds <- vfold_cv(train_data, v = 5, repeats = 3)

# recipe (preprocessing)
housing_recipe <- recipe(log_rent ~ ., data = train_data) %>%
  step_dummy(campus_zone) %>%           
  step_normalize(all_numeric_predictors()) %>%  
  step_nzv(all_predictors())            

cat("\nRecipe defined\n")


# model specifications

# linear regression (baseline)
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# lasso regression
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# random forest
rf_spec <- rand_forest(
  mtry  = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# xgboost
xgb_spec <- boost_tree(
  trees         = 500,
  tree_depth    = tune(),
  learn_rate    = tune(),
  loss_reduction = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# workflows
lm_wf <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(lm_spec)

lasso_wf <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(lasso_spec)

rf_wf <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(rf_spec)

xgb_wf <- workflow() %>%
  add_recipe(housing_recipe) %>%
  add_model(xgb_spec)


# tune hyperparameters
cat("\nTuning lasso...\n")
lasso_grid <- grid_regular(penalty(range = c(-4, 0)), levels = 20)
lasso_tune <- tune_grid(
  lasso_wf,
  resamples = cv_folds,
  grid      = lasso_grid,
  metrics   = metric_set(rmse, rsq, mae)
)

cat("Tuning random forest...\n")
rf_grid <- grid_random(
  mtry(range  = c(2, 8)),
  min_n(range = c(2, 10)),
  size = 15
)
rf_tune <- tune_grid(
  rf_wf,
  resamples = cv_folds,
  grid      = rf_grid,
  metrics   = metric_set(rmse, rsq, mae)
)

cat("Tuning XGBoost...\n")
xgb_grid <- grid_random(
  tree_depth(range    = c(2, 6)),
  learn_rate(range    = c(-3, -1)),
  loss_reduction(range = c(-5, 0)),
  size = 20
)
xgb_tune <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid      = xgb_grid,
  metrics   = metric_set(rmse, rsq, mae)
)


# fit baseline linear model
cat("Fitting baseline linear model...\n")
lm_cv <- fit_resamples(
  lm_wf,
  resamples = cv_folds,
  metrics   = metric_set(rmse, rsq, mae)
)


# compare models
collect_best <- function(tune_result, model_name) {
  collect_metrics(tune_result) %>%
    filter(.metric == "rmse") %>%
    slice_min(mean, n = 1) %>%
    mutate(model = model_name) %>%
    select(model, rmse = mean, std_err = std_err)
}

lm_results <- collect_metrics(lm_cv) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Linear regression") %>%
  select(model, rmse = mean, std_err = std_err)

model_comparison <- bind_rows(
  lm_results,
  collect_best(lasso_tune, "Lasso"),
  collect_best(rf_tune,    "Random forest"),
  collect_best(xgb_tune,   "XGBoost")
) %>%
  arrange(rmse)

cat("\nModel comparison:\n")
print(model_comparison)

# plot model comparison
p_compare <- model_comparison %>%
  mutate(model = fct_reorder(model, rmse)) %>%
  ggplot(aes(x = model, y = rmse, fill = model)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = rmse - std_err, ymax = rmse + std_err),
                width = 0.2) +
  scale_fill_manual(values = c(
    "Linear regression" = "#B5D4F4",
    "Lasso"             = "#378ADD",
    "Random forest"     = "#185FA5",
    "XGBoost"           = "#0C447C"
  )) +
  labs(
    title    = "Model comparison — 5-fold CV RMSE",
    subtitle = "Lower is better | Error bars = ±1 SE",
    x        = NULL,
    y        = "RMSE (log rent scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("outputs/plots/12_model_comparison.png",
       p_compare, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/12_model_comparison.png\n")


# finalize best model
cat("\nFinalizing best model...\n")

best_lasso_params <- select_best(lasso_tune, metric = "rmse")

final_wf <- lasso_wf %>%
  finalize_workflow(best_lasso_params)

final_fit <- last_fit(final_wf, split)

cat("\nFinal model test set performance:\n")
collect_metrics(final_fit) %>% print()


# predicted vs actual plot
test_predictions <- collect_predictions(final_fit) %>%
  mutate(
    actual_rent    = exp(.pred + mean(monroe_model$log_rent, na.rm = TRUE)),
    predicted_rent = exp(.pred)
  )

# raw log scale predictions
p_pred_actual <- collect_predictions(final_fit) %>%
  ggplot(aes(x = log_rent, y = .pred)) +
  geom_point(color = "#378ADD", size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  labs(
    title    = "Predicted vs actual rent (log scale)",
    subtitle = "Points on the dashed line = perfect prediction",
    x        = "Actual log(rent)",
    y        = "Predicted log(rent)",
    caption  = "XGBoost model | Test set"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/13_predicted_vs_actual.png",
       p_pred_actual, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/13_predicted_vs_actual.png\n")


# variable importance
vip_plot <- final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 10, aesthetics = list(fill = "#378ADD", alpha = 0.8)) +
  labs(
    title    = "Variable importance — XGBoost",
    subtitle = "Which features matter most for predicting rent?",
    x        = "Importance",
    y        = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/14_variable_importance.png",
       vip_plot, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/14_variable_importance.png\n")


# finalize lasso (interpretability)
best_lasso_params <- select_best(lasso_tune, metric = "rmse")

final_lasso_wf <- lasso_wf %>%
  finalize_workflow(best_lasso_params)

lasso_fit <- fit(final_lasso_wf, data = train_data)

# lasso coefficients
lasso_coefs <- lasso_fit %>%
  extract_fit_parsnip() %>%
  tidy() %>%
  filter(term != "(Intercept)", estimate != 0) %>%
  arrange(desc(abs(estimate)))

cat("\nLasso coefficients (non-zero):\n")
print(lasso_coefs)

write_csv(lasso_coefs, "outputs/tables/lasso_coefficients.csv")
cat("Saved: outputs/tables/lasso_coefficients.csv\n")


# save models
saveRDS(final_fit, "models/lasso_final_fit.rds")
saveRDS(lasso_fit, "models/lasso_final.rds")      
saveRDS(final_wf,  "models/lasso_workflow.rds") 
cat("Models saved to models/\n")


# save lasso feature importance

# feature importance
lasso_importance <- lasso_coefs %>%
  transmute(
    feature       = term,
    mean_abs_shap = abs(estimate)
  ) %>%
  arrange(desc(mean_abs_shap))

saveRDS(lasso_importance, "models/shap_importance.rds")
write_csv(lasso_importance, "outputs/tables/lasso_importance.csv")
cat("Saved: models/shap_importance.rds\n")
cat("Saved: outputs/tables/lasso_importance.csv\n")

cat("\nAll models saved!\n")
cat("models/ contains:\n")
print(list.files("models"))

file.remove("models/xgboost_final.rds")
file.remove("models/xgboost_workflow.rds")

file.copy("models/lasso_final.rds",     "app/lasso_final.rds",     overwrite = TRUE)
file.copy("models/shap_importance.rds", "app/shap_importance.rds", overwrite = TRUE)
cat("Copied model files to app/\n")