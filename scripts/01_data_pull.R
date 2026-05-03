library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)

# cache shapefiles
options(tigris_use_cache = TRUE)
# create folder
dirs <- c("data/raw", "data/processed", "outputs/plots", "outputs/tables", "models")
purrr::walk(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

# Variable codebook:
#   B25064_001 = median gross rent (dollars)
#   B19013_001 = median household income
#   B25003_003 = renter-occupied housing units
#   B25003_001 = total occupied housing units
#   B25035_001 = median year structure built
#   B01003_001 = total population
#   B15003_022 = bachelor's degree

monroe_data <- get_acs(
geography = "tract",
variables = c(
median_rent       = "B25064_001",
median_income     = "B19013_001",
renter_units      = "B25003_003",
total_units       = "B25003_001",
median_year_built = "B25035_001",
total_pop         = "B01003_001",
bachelors_degree  = "B15003_022"
),
state    = "IN",
county   = "Monroe",
geometry = TRUE,
year     = 2022
)
cat("Pulled", nrow(monroe_data), "rows across", n_distinct(monroe_data$GEOID), "census tracts.\n")

# raw data
saveRDS(monroe_data, "data/raw/monroe_census.rds")
cat("Saved raw data to data/raw/monroe_census.rds\n")

# reshape: wide format
monroe_wide <- monroe_data %>%
select(GEOID, NAME, variable, estimate, geometry) %>%
pivot_wider(
names_from  = variable,
values_from = estimate
) %>%
mutate(
pct_renters   = renter_units / total_units,
pct_bachelors = bachelors_degree / total_pop
)

cat("Wide format: ", nrow(monroe_wide), "tracts,", ncol(monroe_wide), "columns.\n")


# distance to IU

# IU sample gates
iu_campus <- st_sfc(
st_point(c(-86.5264, 39.1653)),
crs = 4326
)
monroe_wide <- monroe_wide %>%
st_transform(crs = 4326) %>%
mutate(
centroid      = st_centroid(geometry),
dist_to_iu_mi = as.numeric(st_distance(centroid, iu_campus)) / 1609.34
) %>%
select(-centroid)
cat("Distance to IU campus added. Range:",
round(min(monroe_wide$dist_to_iu_mi, na.rm = TRUE), 2), "–",
round(max(monroe_wide$dist_to_iu_mi, na.rm = TRUE), 2), "miles\n")

# processed data
saveRDS(monroe_wide, "data/processed/monroe_wide.rds")
cat("Saved processed data to data/processed/monroe_wide.rds\n")

# data check
cat("\nSummary of key variables:\n")
monroe_wide %>%
st_drop_geometry() %>%
select(median_rent, median_income, pct_renters, dist_to_iu_mi, median_year_built) %>%
summary() %>%
print()
cat("\nTracts with missing median_rent (Census-suppressed):",
sum(is.na(monroe_wide$median_rent)), "\n")


# MAPS

# map 1: median rent by tract
p1 <- monroe_wide %>%
ggplot(aes(fill = median_rent)) +
geom_sf(color = "white", linewidth = 0.3) +
scale_fill_viridis_c(
option   = "plasma",
name     = "Median rent ($)",
na.value = "grey80",
labels   = scales::dollar
) +
labs(
title    = "Median gross rent by census tract",
subtitle = "Monroe County, IN — ACS 2018–2022",
caption  = "Source: U.S. Census Bureau ACS 5-year estimates"
) +
theme_minimal(base_size = 12) +
theme(legend.position = "right")

ggsave("outputs/plots/01_rent_map.png", p1, width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/01_rent_map.png\n")

# map 2: distance to IU
p2 <- monroe_wide %>%
ggplot(aes(fill = dist_to_iu_mi)) +
geom_sf(color = "white", linewidth = 0.3) +
scale_fill_viridis_c(
option   = "mako",
name     = "Miles from\nIU campus",
direction = -1
) +
labs(
title    = "Distance to IU campus by census tract",
subtitle = "Monroe County, IN",
caption  = "Reference point: IU Sample Gates (39.1653, -86.5264)"
) +
theme_minimal(base_size = 12)

ggsave("outputs/plots/02_distance_map.png", p2, width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/02_distance_map.png\n")

# map 3: percent renter-occupied
p3 <- monroe_wide %>%
ggplot(aes(fill = pct_renters)) +
geom_sf(color = "white", linewidth = 0.3) +
scale_fill_viridis_c(
option = "inferno",
name   = "% renter\noccupied",
labels = scales::percent,
na.value = "grey80"
) +
labs(
title    = "Share of renter-occupied units by tract",
subtitle = "Monroe County, IN — ACS 2018–2022"
) +
theme_minimal(base_size = 12)

ggsave("outputs/plots/03_renter_share_map.png", p3, width = 8, height = 6, dpi = 150)
cat("Saved: outputs/plots/03_renter_share_map.png\n")

# scatter: rent vs distance to IU
p4 <- monroe_wide %>%
st_drop_geometry() %>%
filter(!is.na(median_rent), !is.na(dist_to_iu_mi)) %>%
ggplot(aes(x = dist_to_iu_mi, y = median_rent)) +
geom_point(aes(size = total_pop), alpha = 0.6, color = "#378ADD") +
geom_smooth(method = "lm", se = TRUE, color = "#0C447C", linewidth = 1) +
scale_y_continuous(labels = scales::dollar) +
scale_size_continuous(name = "Population", labels = scales::comma) +
labs(
title    = "Rent vs. distance to IU campus",
subtitle = "Each point = one census tract. Size = tract population.",
x        = "Distance to IU campus (miles)",
y        = "Median gross rent",
caption  = "Hypothesis: closer tracts command higher rents due to student demand"
) +
theme_minimal(base_size = 12)

ggsave("outputs/plots/04_rent_vs_distance.png", p4, width = 8, height = 5, dpi = 150)
cat("Saved: outputs/plots/04_rent_vs_distance.png\n")

cat("\nData pull complete!\n")
