library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(tidymodels)
library(ggplot2)
library(scales)

tidymodels_prefer()

# load data and models 
monroe_model    <- readRDS("monroe_model_ready.rds")
lasso_fit       <- readRDS("lasso_final.rds")       # fitted lasso workflow
shap_importance <- readRDS("shap_importance.rds")   # lasso |coefficients|

zone_summary <- monroe_model %>%
  st_drop_geometry() %>%
  group_by(campus_zone) %>%
  summarise(
    n_tracts        = n(),
    avg_rent        = round(mean(median_rent, na.rm = TRUE)),
    med_rent        = round(median(median_rent, na.rm = TRUE)),
    avg_income      = round(mean(median_income, na.rm = TRUE)),
    avg_pct_rent    = round(mean(pct_renters, na.rm = TRUE), 3),
    avg_pct_bach    = round(mean(pct_bachelors, na.rm = TRUE), 3),
    avg_housing_age = round(mean(housing_age, na.rm = TRUE)),
    .groups = "drop"
  )

# median rent for comparison
county_median_rent <- median(monroe_model$median_rent, na.rm = TRUE)

# lasso test set RMSE
lasso_rmse_log <- 0.105

zone_colors <- c(
  "near_campus"  = "#0C447C",
  "mid_distance" = "#378ADD",
  "far_suburbs"  = "#B5D4F4"
)

zone_labels <- c(
  "near_campus"  = "Near campus (≤1 mi)",
  "mid_distance" = "Mid distance (1–3 mi)",
  "far_suburbs"  = "Far suburbs (>3 mi)"
)

server <- function(input, output, session) {
  

  # rent predictor
  
  # build prediction input from sliders
  prediction_input <- eventReactive(input$predict_btn, {
    tibble(
      dist_to_iu_mi  = input$dist_to_iu,
      log_dist_to_iu = log(input$dist_to_iu + 0.01),
      campus_zone    = factor(input$campus_zone,
                              levels = c("near_campus", "mid_distance", "far_suburbs")),
      median_income  = input$median_income,
      log_income     = log(input$median_income),
      pct_renters    = input$pct_renters,
      pct_bachelors  = input$pct_bachelors,
      housing_age    = input$housing_age,
      income_to_rent = (input$median_income / 12) / 1100,
      total_pop      = 3500 
    )
  }, ignoreNULL = FALSE)
  
  # run lasso prediction
  rent_prediction <- reactive({
    req(prediction_input())
    pred_log     <- predict(lasso_fit, new_data = prediction_input())$.pred
    pred_dollars <- exp(pred_log)
    # 90% CI using lasso test RMSE on log scale
    list(
      point = round(pred_dollars),
      lower = round(exp(pred_log - 1.645 * lasso_rmse_log)),
      upper = round(exp(pred_log + 1.645 * lasso_rmse_log))
    )
  })
  
  output$predicted_rent <- renderText({
    pred <- rent_prediction()
    paste0("$", format(pred$point, big.mark = ","), " / month")
  })
  
  output$confidence_interval <- renderText({
    pred <- rent_prediction()
    paste0("90% confidence interval: $",
           format(pred$lower, big.mark = ","), " – $",
           format(pred$upper, big.mark = ","))
  })
  
  # value boxes
  output$rent_vs_median <- renderValueBox({
    pred  <- rent_prediction()
    diff  <- pred$point - county_median_rent
    sign  <- if (diff >= 0) "+" else ""
    color <- if (diff >= 0) "red" else "green"
    valueBox(
      value    = paste0(sign, "$", format(abs(diff), big.mark = ",")),
      subtitle = "vs. county median rent",
      icon     = icon("arrows-alt-v"),
      color    = color
    )
  })
  
  output$affordability <- renderValueBox({
    pred  <- rent_prediction()
    ratio <- round((pred$point * 12) / input$median_income * 100, 1)
    color <- if (ratio > 30) "red" else "green"
    valueBox(
      value    = paste0(ratio, "%"),
      subtitle = "of income spent on rent",
      icon     = icon("piggy-bank"),
      color    = color
    )
  })
  
  output$zone_avg <- renderValueBox({
    zone_avg <- zone_summary %>%
      filter(campus_zone == input$campus_zone) %>%
      pull(avg_rent)
    valueBox(
      value    = paste0("$", format(zone_avg, big.mark = ",")),
      subtitle = "zone average rent",
      icon     = icon("map-marker"),
      color    = "blue"
    )
  })
  
  # similar tracts table
  output$similar_tracts <- renderTable({
    monroe_model %>%
      st_drop_geometry() %>%
      filter(campus_zone == input$campus_zone) %>%
      arrange(abs(dist_to_iu_mi - input$dist_to_iu)) %>%
      slice_head(n = 5) %>%
      transmute(
        `Tract`          = str_extract(NAME, "Census Tract [0-9.]+"),
        `Median rent`    = dollar(median_rent),
        `Distance to IU` = paste0(round(dist_to_iu_mi, 1), " mi"),
        `Median income`  = dollar(median_income),
        `% Renters`      = percent(pct_renters, accuracy = 1)
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  

  # rent map
  output$tract_map <- renderPlot({
    
    var     <- input$map_variable
    palette <- tolower(input$map_palette)
    
    var_labels <- c(
      median_rent   = "Median rent ($)",
      dist_to_iu_mi = "Distance to IU (miles)",
      pct_renters   = "% renter occupied",
      median_income = "Median income ($)",
      housing_age   = "Housing age (years)",
      pct_bachelors = "% bachelor's degree"
    )
    
    label_fns <- list(
      median_rent   = dollar,
      dist_to_iu_mi = function(x) paste0(round(x, 1), " mi"),
      pct_renters   = percent,
      median_income = dollar,
      housing_age   = function(x) paste0(round(x), " yrs"),
      pct_bachelors = percent
    )
    
    monroe_model %>%
      ggplot(aes(fill = .data[[var]])) +
      geom_sf(color = "white", linewidth = 0.4) +
      scale_fill_viridis_c(
        option   = palette,
        name     = var_labels[[var]],
        labels   = label_fns[[var]],
        na.value = "grey80"
      ) +
      labs(
        title   = paste(var_labels[[var]], "by census tract"),
        caption = "Monroe County, IN — ACS 2018–2022"
      ) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right",
        plot.title      = element_text(face = "bold", margin = margin(b = 8)),
        plot.margin     = margin(10, 10, 10, 10)
      )
  })
  
  output$map_summary_table <- renderTable({
    zone_summary %>%
      transmute(
        `Campus zone`     = zone_labels[campus_zone],
        `Tracts`          = n_tracts,
        `Avg rent`        = dollar(avg_rent),
        `Median rent`     = dollar(med_rent),
        `Avg income`      = dollar(avg_income),
        `Avg % renters`   = percent(avg_pct_rent, accuracy = 1),
        `Avg housing age` = paste0(avg_housing_age, " yrs")
      )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  

  # price drivers
  output$shap_importance_plot <- renderPlot({
    shap_importance %>%
      mutate(feature = fct_reorder(feature, mean_abs_shap)) %>%
      ggplot(aes(x = mean_abs_shap, y = feature)) +
      geom_col(fill = "#378ADD", alpha = 0.85, width = 0.65) +
      geom_text(aes(label = round(mean_abs_shap, 3)),
                hjust = -0.1, size = 3.5, color = "#333") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title    = "Lasso feature importance",
        subtitle = "|Standardized coefficient| — higher = stronger effect on rent",
        x        = "|Coefficient|",
        y        = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$correlation_plot <- renderPlot({
    monroe_model %>%
      st_drop_geometry() %>%
      select(median_rent, dist_to_iu_mi, median_income,
             pct_renters, pct_bachelors, housing_age, income_to_rent) %>%
      cor(use = "pairwise.complete.obs") %>%
      as.data.frame() %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
      filter(var2 == "median_rent", var1 != "median_rent") %>%
      mutate(
        var1      = fct_reorder(var1, r),
        direction = if_else(r >= 0, "positive", "negative")
      ) %>%
      ggplot(aes(x = r, y = var1, fill = direction)) +
      geom_col(alpha = 0.85, width = 0.65) +
      geom_vline(xintercept = 0, color = "grey40") +
      geom_text(aes(label = round(r, 2),
                    hjust = if_else(r >= 0, -0.2, 1.2)),
                size = 3.5, color = "#333") +
      scale_fill_manual(values = c("positive" = "#0C447C",
                                   "negative" = "#D85A30")) +
      scale_x_continuous(limits = c(-1, 1),
                         expand = expansion(mult = 0.1)) +
      labs(
        title    = "Correlation with median rent",
        subtitle = "Pearson r — Monroe County census tracts",
        x        = "Correlation (r)",
        y        = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position    = "none",
            panel.grid.major.y = element_blank())
  })
  
  output$driver_scatter <- renderPlot({
    feature <- input$driver_feature
    
    x_labels <- c(
      dist_to_iu_mi  = "Distance to IU campus (miles)",
      median_income  = "Median household income ($)",
      pct_renters    = "% renter-occupied",
      pct_bachelors  = "% with bachelor's degree",
      housing_age    = "Housing age (years)",
      income_to_rent = "Income-to-rent ratio"
    )
    
    x_scales <- list(
      median_income  = scale_x_continuous(labels = dollar),
      pct_renters    = scale_x_continuous(labels = percent),
      pct_bachelors  = scale_x_continuous(labels = percent),
      dist_to_iu_mi  = scale_x_continuous(),
      housing_age    = scale_x_continuous(),
      income_to_rent = scale_x_continuous()
    )
    
    p <- monroe_model %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[feature]]), !is.na(median_rent)) %>%
      ggplot(aes(x = .data[[feature]], y = median_rent,
                 color = campus_zone, size = total_pop)) +
      geom_point(alpha = 0.8) +
      geom_smooth(aes(group = 1), method = "lm", se = TRUE,
                  color = "grey30", linewidth = 0.8, alpha = 0.15) +
      scale_color_manual(values = zone_colors, labels = zone_labels,
                         name = "Campus zone") +
      scale_size_continuous(name = "Population", labels = comma,
                            guide = "none") +
      scale_y_continuous(labels = dollar) +
      x_scales[[feature]] +
      labs(
        title    = paste("Median rent vs.", x_labels[[feature]]),
        subtitle = "Each point = one census tract | Color = campus zone",
        x        = x_labels[[feature]],
        y        = "Median gross rent",
        caption  = "Monroe County, IN — ACS 2018–2022"
      ) +
      theme_minimal(base_size = 12)
    
    print(p)
  })
  
  
  # compare areas
  compare_data <- reactive({
    req(input$zone_a, input$zone_b)
    validate(
      need(input$zone_a != input$zone_b,
           "Please select two different zones to compare.")
    )
    monroe_model %>%
      st_drop_geometry() %>%
      filter(campus_zone %in% c(input$zone_a, input$zone_b))
  })
  
  output$compare_rent_plot <- renderPlot({
    compare_data() %>%
      ggplot(aes(x = campus_zone, y = median_rent,
                 fill = campus_zone, color = campus_zone)) +
      geom_boxplot(alpha = 0.4, outlier.shape = NA, width = 0.5) +
      geom_jitter(width = 0.12, size = 3, alpha = 0.8) +
      scale_fill_manual(values  = zone_colors, labels = zone_labels) +
      scale_color_manual(values = zone_colors, labels = zone_labels) +
      scale_x_discrete(labels  = zone_labels) +
      scale_y_continuous(labels = dollar) +
      labs(
        title    = "Rent distribution by zone",
        subtitle = "Each dot = one census tract",
        x        = NULL,
        y        = "Median gross rent"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  output$compare_stats_table <- renderTable({
    zone_summary %>%
      filter(campus_zone %in% c(input$zone_a, input$zone_b)) %>%
      transmute(
        Zone           = zone_labels[campus_zone],
        `Tracts`       = n_tracts,
        `Avg rent`     = dollar(avg_rent),
        `Avg income`   = dollar(avg_income),
        `% Renters`    = percent(avg_pct_rent, accuracy = 1),
        `% Bachelors`  = percent(avg_pct_bach, accuracy = 1),
        `Avg home age` = paste0(avg_housing_age, " yrs")
      ) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Metric")
  }, striped = TRUE, bordered = TRUE, colnames = FALSE)
  
  output$compare_radar_plot <- renderPlot({
    vars_to_compare <- c(
      "median_rent", "median_income", "pct_renters",
      "pct_bachelors", "housing_age", "dist_to_iu_mi"
    )
    
    var_labels_compare <- c(
      median_rent   = "Median rent",
      median_income = "Median income",
      pct_renters   = "% Renters",
      pct_bachelors = "% Bachelors",
      housing_age   = "Housing age",
      dist_to_iu_mi = "Distance to IU"
    )
    
    compare_data() %>%
      group_by(campus_zone) %>%
      summarise(across(all_of(vars_to_compare),
                       ~ mean(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      pivot_longer(-campus_zone,
                   names_to  = "variable",
                   values_to = "value") %>%
      group_by(variable) %>%
      mutate(value_scaled = (value - min(value)) /
               (max(value) - min(value) + 1e-6)) %>%
      ungroup() %>%
      mutate(variable = var_labels_compare[variable]) %>%
      ggplot(aes(x = variable, y = value_scaled,
                 fill = campus_zone, color = campus_zone)) +
      geom_col(position = "dodge", alpha = 0.75, width = 0.65) +
      scale_fill_manual(values  = zone_colors, labels = zone_labels,
                        name = NULL) +
      scale_color_manual(values = zone_colors, labels = zone_labels,
                         name = NULL) +
      scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
      labs(
        title    = "Zone comparison — all variables (normalized 0–1)",
        subtitle = "Values scaled within each variable for fair comparison",
        x        = NULL,
        y        = "Relative value (scaled)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        axis.text.x     = element_text(angle = 20, hjust = 1)
      )
  })
  
} # end server