install.packages("patchwork")
library(tidyverse)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)

monroe_model <- readRDS("data/processed/monroe_model_ready.rds")

cat("Loaded", nrow(monroe_model), "tracts for EDA\n")


# median rent
p_rent_dist <- monroe_model %>%
  st_drop_geometry() %>%
  ggplot(aes(x = median_rent)) +
  geom_histogram(bins = 12, fill = "#378ADD", color = "white", linewidth = 0.3) +
  geom_vline(aes(xintercept = median(median_rent)),
             color = "#0C447C", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = dollar) +
  labs(
    title    = "Distribution of median rent across tracts",
    subtitle = paste("Median:", dollar(median(monroe_model$median_rent)),
                     "| n =", nrow(monroe_model), "tracts"),
    x        = "Median gross rent",
    y        = "Count",
    caption  = "Dashed line = median"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/05_rent_distribution.png",
       p_rent_dist, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/05_rent_distribution.png\n")


# log rent distribution
p_log_rent <- monroe_model %>%
  st_drop_geometry() %>%
  ggplot(aes(x = log_rent)) +
  geom_histogram(bins = 12, fill = "#5DCAA5", color = "white", linewidth = 0.3) +
  labs(
    title    = "Distribution of log(median rent)",
    subtitle = "Log transformation reduces right skew for modeling",
    x        = "log(median rent)",
    y        = "Count"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/06_log_rent_distribution.png",
       p_log_rent, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/06_log_rent_distribution.png\n")


# correlation matrix of numeric features
cor_data <- monroe_model %>%
  st_drop_geometry() %>%
  select(
    median_rent, dist_to_iu_mi, median_income,
    pct_renters, pct_bachelors, housing_age,
    income_to_rent, total_pop
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p_cor <- cor_data %>%
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(correlation, 2)),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low     = "#0C447C",
    mid     = "white",
    high    = "#D85A30",
    midpoint = 0,
    limits  = c(-1, 1),
    name    = "Correlation"
  ) +
  labs(
    title    = "Correlation matrix — housing features",
    subtitle = "Red = positive, Blue = negative correlation with median rent",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/plots/07_correlation_matrix.png",
       p_cor, width = 8, height = 7, dpi = 150)
cat("Saved: outputs/plots/07_correlation_matrix.png\n")

cat("\nCorrelations with median_rent:\n")
cor_data %>%
  filter(var1 == "median_rent", var2 != "median_rent") %>%
  arrange(desc(abs(correlation))) %>%
  mutate(correlation = round(correlation, 3)) %>%
  print()


# rent vs key predictors
plot_scatter <- function(data, x_var, x_label) {
  data %>%
    st_drop_geometry() %>%
    filter(!is.na(.data[[x_var]])) %>%
    ggplot(aes(x = .data[[x_var]], y = median_rent)) +
    geom_point(alpha = 0.7, color = "#378ADD", size = 2.5) +
    geom_smooth(method = "lm", se = TRUE,
                color = "#0C447C", linewidth = 0.8) +
    scale_y_continuous(labels = dollar) +
    labs(x = x_label, y = "Median rent") +
    theme_minimal(base_size = 11)
}

s1 <- plot_scatter(monroe_model, "dist_to_iu_mi",  "Distance to IU (miles)")
s2 <- plot_scatter(monroe_model, "median_income",  "Median household income")
s3 <- plot_scatter(monroe_model, "pct_renters",    "% renter occupied")
s4 <- plot_scatter(monroe_model, "pct_bachelors",  "% with bachelor's degree")
s5 <- plot_scatter(monroe_model, "housing_age",    "Housing age (years)")
s6 <- plot_scatter(monroe_model, "income_to_rent", "Income-to-rent ratio")

p_grid <- (s1 + s2) / (s3 + s4) / (s5 + s6) +
  plot_annotation(
    title    = "Median rent vs key predictors",
    subtitle = "Monroe County census tracts — each point is one tract",
    theme    = theme_minimal(base_size = 13)
  )

ggsave("outputs/plots/08_scatter_grid.png",
       p_grid, width = 10, height = 12, dpi = 150)
cat("Saved: outputs/plots/08_scatter_grid.png\n")

# hypothesis plot: rent vs distance
p_hypothesis <- monroe_model %>%
  st_drop_geometry() %>%
  ggplot(aes(x = dist_to_iu_mi, y = median_rent, color = campus_zone)) +
  geom_point(aes(size = total_pop), alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8,
              aes(group = 1), color = "grey40", linetype = "dashed") +
  scale_color_manual(
    values = c(
      "near_campus"  = "#0C447C",
      "mid_distance" = "#378ADD",
      "far_suburbs"  = "#B5D4F4"
    ),
    labels = c("Near campus (≤1 mi)",
               "Mid distance (1–3 mi)",
               "Far suburbs (>3 mi)"),
    name = "Campus zone"
  ) +
  scale_y_continuous(labels = dollar) +
  scale_size_continuous(name = "Population", labels = comma) +
  labs(
    title    = "Rent vs. distance to IU campus",
    subtitle = "Hypothesis: proximity to campus drives higher rents",
    x        = "Distance to IU campus (miles)",
    y        = "Median gross rent",
    caption  = "Monroe County, IN — ACS 2018–2022"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/09_hypothesis_plot.png",
       p_hypothesis, width = 9, height = 6, dpi = 150)
cat("Saved: outputs/plots/09_hypothesis_plot.png\n")

# campus zone boxplot
p_zone_box <- monroe_model %>%
  st_drop_geometry() %>%
  ggplot(aes(x = campus_zone, y = median_rent, fill = campus_zone)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c(
    "near_campus"  = "#0C447C",
    "mid_distance" = "#378ADD",
    "far_suburbs"  = "#B5D4F4"
  )) +
  scale_x_discrete(labels = c(
    "near_campus"  = "Near campus\n(≤1 mi)",
    "mid_distance" = "Mid distance\n(1–3 mi)",
    "far_suburbs"  = "Far suburbs\n(>3 mi)"
  )) +
  scale_y_continuous(labels = dollar) +
  labs(
    title    = "Rent distribution by campus proximity zone",
    subtitle = "Do near-campus tracts command a rent premium?",
    x        = NULL,
    y        = "Median gross rent"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggsave("outputs/plots/10_zone_boxplot.png",
       p_zone_box, width = 7, height = 5, dpi = 150)
cat("Saved: outputs/plots/10_zone_boxplot.png\n")


# spatial map: rent choropleth
p_rent_map <- monroe_model %>%
  ggplot(aes(fill = median_rent)) +
  geom_sf(color = "white", linewidth = 0.4) +
  scale_fill_gradient(
    low    = "#E6F1FB",
    high   = "#0C447C",
    name   = "Median rent ($)",
    labels = dollar,
    na.value = "grey80"
  ) +
  labs(
    title    = "Median gross rent by census tract",
    subtitle = "Monroe County, IN — ACS 2018–2022",
    caption  = "Source: U.S. Census Bureau"
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right")

ggsave("outputs/plots/11_rent_choropleth.png",
       p_rent_map, width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/11_rent_choropleth.png\n")

# summary statistics table
summary_table <- monroe_model %>%
  st_drop_geometry() %>%
  summarise(
    across(
      c(median_rent, dist_to_iu_mi, median_income,
        pct_renters, pct_bachelors, housing_age),
      list(
        mean   = ~ round(mean(.x, na.rm = TRUE), 2),
        median = ~ round(median(.x, na.rm = TRUE), 2),
        sd     = ~ round(sd(.x, na.rm = TRUE), 2),
        min    = ~ round(min(.x, na.rm = TRUE), 2),
        max    = ~ round(max(.x, na.rm = TRUE), 2)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(everything(),
               names_to  = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

write_csv(summary_table, "outputs/tables/eda_summary_stats.csv")
cat("Saved: outputs/tables/eda_summary_stats.csv\n")

cat("\nSummary statistics:\n")
print(summary_table)


cat("\nEDA complete!\n")
cat("Plots saved to outputs/plots/\n")