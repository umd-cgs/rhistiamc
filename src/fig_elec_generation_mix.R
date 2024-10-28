library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# # Read the data (if not already done via main.R)
# data_hist <- read.csv("output/historical_iso.csv", na.strings = "NA")
# colnames(data) <- gsub("^X", "", colnames(data))
# data <- data |>
#   pivot_longer(cols = -c(model, variable, region, unit, scenario),
#                names_to = "year",
#                values_to = "value") |>
#   arrange(model, region, year, variable, scenario) |>
#   filter(!is.na(value)) |>
#   mutate(value=as.numeric(value),
#          year=as.numeric(year))
# 
# # Define the list of countries, or use the data.frame defined in main.R
# countries <- data.frame(region="dummy",iso="CHN",priority="dummy",scen1=NA,scen2=NA,scen3=NA)

vars_elec <- c("Secondary Energy|Electricity|Solar",
               "Secondary Energy|Electricity|Wind",
               "Secondary Energy|Electricity|Biomass",
               # "Secondary Energy|Electricity|Captive Coal",
               "Secondary Energy|Electricity|Coal",
               "Secondary Energy|Electricity|Hydro",
               "Secondary Energy|Electricity|Nuclear",
               "Secondary Energy|Electricity|Gas",
               "Secondary Energy|Electricity|Other Fossil",
               "Secondary Energy|Electricity|Renewables",
               "Secondary Energy|Electricity|Ammonia",
               "Secondary Energy|Electricity|Other",
               "Secondary Energy|Electricity|Geothermal")

# Loop over each country
for (ctry in countries$iso) {
  #check if scenario data is available and should be added to the plot
  if(is.na(countries[countries$iso == ctry,]$scen1)){
    addscen <- F
  } else { addscen <- T}
  # Filter the data for the current country
  
  ctry_data <- data_hist %>%
    filter(region == ctry) %>%
    filter(model %in%  c("EMBER","GEM and CGS")) %>%
    filter(variable %in% vars_elec) %>%
    mutate(value = ej2twh*value )
  
  #add scenario data if defined for the country in question
  if(addscen){
    ctry_data <- rbind(ctry_data, 
                  data_scen|>filter(variable %in% vars_elec,
                                    year %in% c(2030,2035),
                                    region==countries[countries$iso==ctry,]$region,
                                    scenario %in% c(countries[countries$iso==ctry,]$scen1,
                                                    countries[countries$iso==ctry,]$scen2,
                                                    countries[countries$iso==ctry,]$scen3)) |>
                 mutate(value=ej2twh*value,year=case_when(
                   scenario == countries[countries$iso==ctry,]$scen2 ~ year-1,
                   scenario == countries[countries$iso==ctry,]$scen3 ~ year+1,
                   .default=year),scenario = case_when(
                     scenario == countries[countries$iso==ctry,]$scen1 ~ countries[countries$iso==ctry,]$name1,
                     scenario == countries[countries$iso==ctry,]$scen2 ~ countries[countries$iso==ctry,]$name2,
                     scenario == countries[countries$iso==ctry,]$scen3 ~ countries[countries$iso==ctry,]$name3
                   )
                 ))
  }
  
  # Simplify the variable column
  ctry_data <- ctry_data %>%
    mutate(Fuel = case_when(
      variable == "Secondary Energy|Electricity|Solar" ~ "Solar",
      variable == "Secondary Energy|Electricity|Wind" ~ "Wind",
      variable == "Secondary Energy|Electricity|Biomass" ~ "Biomass",
      variable == "Secondary Energy|Electricity|Coal" ~ "Coal",
      variable == "Secondary Energy|Electricity|Captive Coal" ~ "Captive Coal",
      variable == "Secondary Energy|Electricity|Hydro" ~ "Hydro",
      variable == "Secondary Energy|Electricity|Nuclear" ~ "Nuclear",
      variable == "Secondary Energy|Electricity|Gas" ~ "Gas",
      variable == "Secondary Energy|Electricity|Other Fossil" ~ "Oil",
      variable == "Secondary Energy|Electricity|Renewables" ~ "Renewables",
      variable == "Secondary Energy|Electricity|Ammonia" ~ "Ammonia",
      variable == "Secondary Energy|Electricity|Other" ~ "Other",
      variable == "Secondary Energy|Electricity|Geothermal" ~ "Geothermal",
      TRUE ~ "Other"
    ))
  
  # Ensure the year column is numeric
  ctry_data$year <- as.numeric(ctry_data$year)
  
  # Set the order of the factors to match the image
  ctry_data$Fuel <- factor(ctry_data$Fuel, levels = c("Wind", "Solar","Renewables", "Biomass", "Hydro", "Geothermal", "Nuclear","Other","Ammonia", "Oil", "Gas", "Captive Coal", "Coal"))

  # Create the plot
  plot <- ggplot()  +
    geom_area(data =
                ctry_data |> filter(scenario=="historical",year<2023), aes(x = year, y = value, fill = Fuel, group = Fuel),
              alpha = 0.7) +
    scale_fill_manual(values = c(
      "Wind" = "skyblue",
      "Solar" = "goldenrod1",
      "Renewables" = "goldenrod",
      "Biomass" = "olivedrab",
      "Hydro" = "blue",
      "Geothermal" = "olivedrab1",
      "Nuclear" = "purple",
      "Other" = "grey",
      "Ammonia" = "red",
      "Oil" = "grey",
      "Gas" = "darkorange",
      "Captive Coal" = "#444444",
      "Coal" = "black"
    ),breaks=c("Wind","Solar","Renewables","Biomass","Hydro","Geothermal","Nuclear","Other","Ammonia","Oil","Gas","Captive Coal","Coal")
    ) +
    labs(
         title = paste("Secondary Energy Production in", ctry), #, "(2000 - 2021)"),
         x = "",
         y = "Electricity Generation (TWh)",
         fill = "Energy Source") +
    theme_bw() +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8))

  if(addscen){
    plot <- plot +
      geom_col(data=ctry_data|>filter(scenario !="historical") ,
               aes(x=year,y=value,fill=Fuel),alpha=0.7) +
      geom_text(data=ctry_data|>filter(year < 2032, scenario!="historical")|>group_by(year,scenario)|>summarize(value=sum(value)),
                aes(x=2030+(year-2030)*2,y=value,label=scenario),angle=90,hjust="right") +
      scale_x_continuous(breaks = c(seq(2000,2030,10),2035),minor_breaks=seq(2005,2015,10))
  }
  
  ctry_data |> filter(year %in% c(2023,2030)) |> group_by(year) |>
    summarize(value=sum(value))
  # Print the plot
  print(plot)
  
  ggsave(filename = file.path('figures', paste0(ctry, "_electricity_generation.png")), plot = plot, width = 6, height = 5)
#  ggsave(filename = file.path('figures', paste0(ctry, "_electricity_generation.png")), plot = plot, width = 4, height = 3.7)
  }

