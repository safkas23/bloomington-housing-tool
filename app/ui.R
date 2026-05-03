library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  skin = "blue",
  
  
  # header
  dashboardHeader(
    title = "Bloomington Housing Tool"
  ),
  

  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Rent Predictor", tabName = "predictor", icon = icon("home")),
      menuItem("Rent Map",       tabName = "map",       icon = icon("map")),
      menuItem("Price Drivers",  tabName = "drivers",   icon = icon("chart-bar")),
      menuItem("Compare Areas",  tabName = "compare",   icon = icon("balance-scale"))
    )
  ),
  

  # body
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f9f9f9; }
        .box { border-radius: 8px; }
        .prediction-box { font-size: 2.2em; font-weight: bold;
                          color: #185FA5; text-align: center; padding: 10px; }
        .ci-box { font-size: 1em; color: #666;
                  text-align: center; margin-top: -8px; }
        .info-text { color: #555; font-size: 0.95em; }
      "))
    ),
    
    tabItems(
      

      # rent predictor
      tabItem(
        tabName = "predictor",
        
        fluidRow(
          box(
            title = "Property Details", status = "primary",
            solidHeader = TRUE, width = 4,
            
            selectInput(
              "campus_zone",
              "Distance to IU campus:",
              choices = c(
                "Near campus (≤1 mile)"    = "near_campus",
                "Mid distance (1–3 miles)" = "mid_distance",
                "Far suburbs (>3 miles)"   = "far_suburbs"
              )
            ),
            
            sliderInput(
              "dist_to_iu",
              "Exact distance to IU (miles):",
              min = 0.4, max = 10, value = 2, step = 0.1
            ),
            
            sliderInput(
              "median_income",
              "Neighborhood median income ($):",
              min = 14000, max = 120000, value = 55000, step = 1000,
              pre = "$", sep = ","
            ),
            
            sliderInput(
              "pct_renters",
              "% renter-occupied in area:",
              min = 0.05, max = 1.0, value = 0.45, step = 0.01
            ),
            
            sliderInput(
              "pct_bachelors",
              "% with bachelor's degree:",
              min = 0.0, max = 0.6, value = 0.25, step = 0.01
            ),
            
            sliderInput(
              "housing_age",
              "Housing age (years):",
              min = 5, max = 75, value = 40, step = 1
            ),
            
            actionButton(
              "predict_btn", "Predict Rent",
              class = "btn-primary btn-lg btn-block",
              icon  = icon("calculator")
            )
          ),
          
          column(
            width = 8,
            
            fluidRow(
              box(
                title = "Predicted Rent", status = "success",
                solidHeader = TRUE, width = 12,
                
                div(class = "prediction-box",
                    textOutput("predicted_rent")),
                div(class = "ci-box",
                    textOutput("confidence_interval")),
                
                hr(),
                
                fluidRow(
                  valueBoxOutput("rent_vs_median", width = 4),
                  valueBoxOutput("affordability",  width = 4),
                  valueBoxOutput("zone_avg",        width = 4)
                )
              )
            ),
            
            fluidRow(
              box(
                title = "Similar Tracts in Monroe County",
                status = "info", solidHeader = TRUE, width = 12,
                tableOutput("similar_tracts")
              )
            )
          )
        )
      ),
      

      # rent map
      tabItem(
        tabName = "map",
        
        fluidRow(
          box(
            title = "Map Controls", status = "primary",
            solidHeader = TRUE, width = 3,
            
            selectInput(
              "map_variable",
              "Color map by:",
              choices = c(
                "Median rent"         = "median_rent",
                "Distance to IU"      = "dist_to_iu_mi",
                "% Renter occupied"   = "pct_renters",
                "Median income"       = "median_income",
                "Housing age"         = "housing_age",
                "% Bachelor's degree" = "pct_bachelors"
              )
            ),
            
            selectInput(
              "map_palette",
              "Color palette:",
              choices = c("Blues", "Plasma", "Viridis", "Inferno")
            ),
            
            hr(),
            p(class = "info-text",
              "Each shape is one census tract in Monroe County.",
              "Grey tracts have suppressed Census data.")
          ),
          
          box(
            title = "Monroe County Census Tract Map",
            status = "info", solidHeader = TRUE, width = 9,
            plotOutput("tract_map", height = "500px")
          )
        ),
        
        fluidRow(
          box(
            title = "Tract Summary Statistics",
            status = "warning", solidHeader = TRUE, width = 12,
            tableOutput("map_summary_table")
          )
        )
      ),
      

      # price drivers
      tabItem(
        tabName = "drivers",
        
        fluidRow(
          box(
            title = "Lasso Feature Importance",
            status = "primary", solidHeader = TRUE, width = 6,
            p(class = "info-text",
              "Absolute standardized lasso coefficients.",
              "Higher = stronger effect on predicted rent.",
              "Lasso won model comparison with RMSE = 0.099."),
            plotOutput("shap_importance_plot", height = "350px")
          ),
          
          box(
            title = "Correlation with Median Rent",
            status = "info", solidHeader = TRUE, width = 6,
            plotOutput("correlation_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Explore a Feature",
            status = "warning", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(4,
                     selectInput(
                       "driver_feature",
                       "Select feature to explore:",
                       choices = c(
                         "Distance to IU (miles)"  = "dist_to_iu_mi",
                         "Median income"           = "median_income",
                         "% Renter occupied"       = "pct_renters",
                         "% Bachelor's degree"     = "pct_bachelors",
                         "Housing age"             = "housing_age",
                         "Income-to-rent ratio"    = "income_to_rent"
                       )
                     )
              )
            ),
            
            plotOutput("driver_scatter", height = "350px")
          )
        )
      ),
      

      # compare areas
      tabItem(
        tabName = "compare",
        
        fluidRow(
          box(
            title = "Select Two Campus Zones to Compare",
            status = "primary", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(6,
                     selectInput(
                       "zone_a", "Zone A:",
                       choices = c(
                         "Near campus (≤1 mile)"    = "near_campus",
                         "Mid distance (1–3 miles)" = "mid_distance",
                         "Far suburbs (>3 miles)"   = "far_suburbs"
                       ),
                       selected = "near_campus"
                     )
              ),
              column(6,
                     selectInput(
                       "zone_b", "Zone B:",
                       choices = c(
                         "Near campus (≤1 mile)"    = "near_campus",
                         "Mid distance (1–3 miles)" = "mid_distance",
                         "Far suburbs (>3 miles)"   = "far_suburbs"
                       ),
                       selected = "far_suburbs"
                     )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Rent Comparison",
            status = "success", solidHeader = TRUE, width = 6,
            plotOutput("compare_rent_plot", height = "300px")
          ),
          
          box(
            title = "Key Statistics Side-by-Side",
            status = "info", solidHeader = TRUE, width = 6,
            tableOutput("compare_stats_table")
          )
        ),
        
        fluidRow(
          box(
            title = "All Variables — Zone Comparison",
            status = "warning", solidHeader = TRUE, width = 12,
            plotOutput("compare_radar_plot", height = "350px")
          )
        )
      )
      
    ) # end tabs
  ) # end body
) # end page