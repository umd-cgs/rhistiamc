library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)


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

# Loop over each model
for (mod in models_selected) {
  
  # Loop over each region
  for (reg in regions_selected) {
    
    # NOTE: These figures can only accommodate up to three scenarios at once, so 
    # it disregards the later ones if more than three supplied 
    for (cnt in seq(1:min(3,length(scenarios_selected)))) {
  
      # Filter the data for the current region
      
      reg_data <- data_hist %>%
        filter(region == reg) %>%
        filter(model %in%  c("EMBER","GEM and CGS")) %>%
        filter(variable %in% vars_elec) %>%
        mutate(value = ej2twh*value )
      
      reg_data_scen <- data_scen|>filter(variable %in% vars_elec,
                                         year %in% c(2030,2035),
                                         model == mod,
                                         region==reg,
                                         scenario %in% c(scenarios_selected[1],
                                                         scenarios_selected[2],
                                                         scenarios_selected[3])) |>
        mutate(value=ej2twh*value,
               year=case_when(
                 scenario == scenarios_selected[2] ~ year-1,
                 scenario == scenarios_selected[3] ~ year+1,
                 .default=year))
      
      # Include the model name in the figure only if the model has data for this region 
      mod_in_flnm <- ifelse(nrow(reg_data_scen) > 0, mod, "")
      
      #add scenario data if defined for the region in question
      reg_data <- bind_rows(reg_data, reg_data_scen)
      
      # Simplify the variable column
      reg_data <- reg_data %>%
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
      reg_data$year <- as.numeric(reg_data$year)
      
      # Set the order of the factors to match the image
      reg_data$Fuel <- factor(reg_data$Fuel, levels = c("Wind", "Solar","Renewables", "Biomass", "Hydro", "Geothermal", "Nuclear","Other","Ammonia", "Oil", "Gas", "Captive Coal", "Coal"))
    
      # Create the plot
      plot <- ggplot()  +
        geom_area(data =
                    reg_data |> filter(scenario=="historical",year<2023), aes(x = year, y = value, fill = Fuel, group = Fuel),
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
             title = paste("Secondary Energy Production in", reg), #, "(2000 - 2021)"),
             x = "",
             y = "Electricity Generation (TWh)",
             fill = "Energy Source") +
        theme_bw() +
        theme(legend.title = element_text(size = 10),
              legend.text = element_text(size = 8))
    
      plot <- plot +
        geom_col(data=reg_data|>filter(scenario !="historical") ,
                 aes(x=year,y=value,fill=Fuel),alpha=0.7) +
        geom_text(data=reg_data|>filter(year < 2032, scenario!="historical")|>group_by(year,scenario)|>summarize(value=sum(value)),
                  aes(x=2030+(year-2030)*2,y=value,label=scenario),angle=90,hjust="right") +
        scale_x_continuous(breaks = c(seq(2000,2030,10),2035),minor_breaks=seq(2005,2015,10))
      
      reg_data |> filter(year %in% c(2023,2030)) |> group_by(year) |>
        summarize(value=sum(value))

    }  # end of for loop across scenarios

    # Print the plot
    print(plot)
    
    
    
    ggsave(filename = file.path('figures', paste0(reg, "_", mod_in_flnm, "_electricity_generation.png")), plot = plot, width = 6, height = 5)
    #  ggsave(filename = file.path('figures', paste0(reg, "_electricity_generation.png")), plot = plot, width = 4, height = 3.7)
    
        
  } # end of for loop across regions

} # end of for loop across models
