##### Load Libraries and constants -----
library(tidyverse)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


#source functions and constants
source("src/functions.R")

start_yr <- 1975
# ----- USER INPUTS -----
#select the year and the variable to be filtered
selected_year <- 2022
selected_variable <- "Capacity|Electricity|Wind"
denominator_variable <- "Population" #or NULL
model_region <- "R5" # Options: "ISO", "R5", "R10", "GCAM32"
model_type <- "historical" #options: historical or scenario

# ----- DATA SELECTION -----
#filter the data
data_model <- if (model_type == "historical") data_hist_full else data_scen_full
# Filter numerator data
numerator_data <- data_model %>%
  filter(variable == selected_variable, year == selected_year) %>%
  select(region, value, unit) %>%
  distinct(region, .keep_all = TRUE) %>%
  rename(numerator = value, numerator_unit = unit)

if (!is.null(denominator_variable) && denominator_variable != "none") {
  denominator_data <- data_model %>%
    filter(variable == denominator_variable, year == selected_year) %>%
    select(region, value, unit) %>%
    distinct(region, .keep_all = TRUE) %>%
    rename(denominator = value, denominator_unit = unit)
  
  data_ratio <- numerator_data %>%
    left_join(denominator_data, by = "region") %>%
    mutate(
      ratio_value = numerator / denominator,
      ratio_unit = paste0(numerator_unit, " / ", denominator_unit)
    )
} else {
  data_ratio <- numerator_data %>%
    mutate(
      ratio_value = numerator,
      ratio_unit = numerator_unit
    )
}



# ----- Load Region Mappings -----
r5_map <- read.csv("mappings/regionmappingr5.csv",  sep = ";", stringsAsFactors = FALSE) %>%
  rename(adm0_a3 = CountryCode, R5 = RegionCode)%>%
  mutate(adm0_a3 = toupper(adm0_a3))

r10_map <- read.csv("mappings/iso_r10.csv",  sep = ",", stringsAsFactors = FALSE) %>%
  rename(adm0_a3 = iso, R10 = region)%>%
  mutate(adm0_a3 = toupper(adm0_a3))

# Load and clean GCAM32 mapping
gcam_iso_map <- read.csv("mappings/iso_EI_GCAM_regID.csv", skip = 6)
gcam_names <- read.csv("mappings/GCAM_region_names.csv", skip = 6)

gcam_map <- gcam_iso_map %>%
  rename(adm0_a3 = iso) %>%
  left_join(gcam_names, by = "GCAM_region_ID") %>%
  rename(GCAM32 = region) %>%
  select(adm0_a3, GCAM32)%>%
  mutate(adm0_a3 = toupper(adm0_a3))

# Merge region mappings into one
region_map <- full_join(r5_map, r10_map, by = "adm0_a3") %>%
  full_join(gcam_map, by = "adm0_a3")

# Define helper to choose the region column to join on
get_region_column <- function(model_region) {
  switch(model_region,
         "ISO" = "region",
         "R5" = "R5",
         "R10" = "R10",
         "GCAM32" = "GCAM32",
         stop("Invalid model_region"))
}

region_col <- get_region_column(model_region)


# ----- LOAD WORLD MAP -----

# Load world map and add region mapping to it
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(adm0_a3 = toupper(adm0_a3)) %>%
  mutate(adm0_a3 = ifelse(adm0_a3 == "SDS", "SSD", adm0_a3)) %>%   #FIX SOUTH SUDAN CODE
  left_join(region_map, by = "adm0_a3")

# Join per capita data based on selected region type (R5/R10/GCAM32/ISO)
map_data <- world %>%
  left_join(data_ratio, by = setNames("region", region_col))


# Now inspect:
map_data %>%
  select(adm0_a3, name, R5, !!sym(region_col), ratio_value)%>%
  filter(!is.na(R5)) %>%
  head(10)

# ----- PLOT CHOROPLETH MAP -----

# ----- Create Outline Layer Based on model_region -----
get_region_outline <- function(map_data, region_col) {
  if (region_col %in% c("R5", "R10", "GCAM32")) {
    region_outline <- map_data %>%
      filter(!is.na(.data[[region_col]])) %>%
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")) %>% 
      group_by(region = .data[[region_col]]) %>%
      summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
      sf::st_make_valid()  
    return(region_outline)
  } else {
    return(NULL)  # No outline for ISO (country level)
  }
}

# Run the function to get the outline
region_outline <- get_region_outline(map_data, region_col)


# ----- PLOT CHOROPLETH MAP -----
ggplot() +
  geom_sf(data = map_data, aes(fill = ratio_value), color = NA) +  # no black borders
  
  # Conditionally add regional outlines
  {if (!is.null(region_outline)) 
    geom_sf(data = region_outline, fill = NA, color = "gray60", size = 0.5)} +
  
  coord_sf(
    xlim = c(-170, 170),
    ylim = c(-60, 85),
    expand = FALSE,
    datum = NA,            
    label_axes = "--"   
  ) +
  
  scale_fill_gradientn(
    colors = c("#ffffb2", "#fd8d3c", "#e34a33"),
    name = unique(data_ratio$ratio_unit),
    limits = c(0, NA),
    na.value = "grey80"
  ) +
  
  theme_minimal() +
  labs(
    title = paste0("Ratio: ", selected_variable, " / ", denominator_variable, " (", selected_year, ")"),
    caption = "Source: IAMC Historical Data"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),                      
    panel.background = element_rect(fill = "white", color = NA) 
  )
