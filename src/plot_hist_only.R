##### Load Libraries and constants -----
library(tidyverse)
library(ggplot2)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
#source functions and constants
source("src/functions.R")

start_yr <- 1975

##### read in data -------

#read in country and global data
#you need to download the file historical_iso.csv from https://drive.google.com/open?id=117cTkVRekeu3vHYrFkH93zstpCqGxkM8&usp=drive_fs
#and save it in the 'output' folder, so that the following line works out
data_hist_full <- read.csv(paste0("output/historical_iso.csv"), na.strings = "NA")
#clean up year values in columns
colnames(data_hist_full) <- gsub("^X", "", colnames(data_hist_full))
#make it into default 'long' dataframe format
data_hist_full <- data_hist_full |>
  pivot_longer(cols = -c(model, variable, region, unit, scenario),
               names_to = "year",
               values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
  #remove NAs 
  filter(!is.na(value)) |> 
  #make value and year numeric
  mutate(value=as.numeric(value),
         year=as.numeric(year))



##### Primary energy mix -------
#define primary energy variables to use
pes <- c(
  "Primary Energy|Coal",
  "Primary Energy|Oil",
  "Primary Energy|Gas",
  "Primary Energy|Bio and Geo (input-equivalent)",
  "Primary Energy|Nuclear (input-equivalent)",
  "Primary Energy|Solar (input-equivalent)",
  "Primary Energy|Hydro (input-equivalent)",
  "Primary Energy|Wind (input-equivalent)"
)

#put together dataframe of plot data: filter GBR and variables
plot <- data_hist_full|>filter(region=="GBR",variable %in% pes) |>
  #make variable names shorter
  mutate(variable=gsub(pattern = "Primary Energy\\|",replacement="",x=variable))

#change order of variables, such that order of Fouquet plot gets reproduced
plot$variable = factor(x = plot$variable,levels=rev(c("Bio and Geo (input-equivalent)",
            "Coal","Gas","Oil","Nuclear (input-equivalent)","Hydro (input-equivalent)",
            "Wind (input-equivalent)","Solar (input-equivalent)")))

#make area plot
ggplot()+
  geom_area(data=plot|>filter(year>1975),
            #convert from EJ to Mtoe
            aes(x=year,y=value*23.9,fill=variable),stat = 'identity',alpha=0.7)+theme_bw()+
  #reproduce approximately color scale
  scale_fill_manual(values=rev(c("darkgreen","darkgrey","yellow","darkred","purple","darkblue","blue","orange")))+
  ylab("Primary Energy (Mtoe 'input-equivalent')")+xlab("")+ylim(0,250)+xlim(1975,2026)+
  scale_x_continuous(breaks=c(1975,2000,2025),labels=c(1975,2000,2025))
#save to file
  ggsave("figures/PE_UK.png",width=4,height=4)  
plot$variable <- gsub(pattern = " \\(input-equivalent\\)",replacement = "",x=plot$variable)
plot$variable = factor(x = plot$variable,levels=rev(c("Bio and Geo",
                            "Coal","Gas","Oil","Nuclear","Hydro","Wind","Solar")))
  
# small version with default color scheme
  ggplot()+
    geom_area(data=plot|>filter(year>1984),
              #convert from EJ to Mtoe
              aes(x=year,y=value*23.9,fill=variable),stat = 'identity',alpha=0.7)+theme_bw()+
#use color scheme defined in src/functions.R
        fillScalePeFuel+
    ylab("Primary Energy (Mtoe)")+xlab("")+ylim(0,250)#+xlim(1975,2026)#+
      #save to file with appropriate dimensions
  ggsave("figures/PE_UK_small.png",width=6,height=2)  
  
    
  ### electricity generation mix -----
  elecs <- c(
    "Secondary Energy|Electricity|Biomass",
    "Secondary Energy|Electricity|Coal",
    "Secondary Energy|Electricity|Oil",
    "Secondary Energy|Electricity|Gas",
    "Secondary Energy|Electricity|Nuclear",
    "Secondary Energy|Electricity|Solar",
    "Secondary Energy|Electricity|Hydro",
    "Secondary Energy|Electricity|Wind"
  )

  #use electricity generation from EI, plus Biomass electricity from EMBER (only starting in 2000, but was small before anyway)
  plot2 <- data_hist_full|>filter(region=="GBR",variable %in% elecs,model!="EMBER") |>
    #make variable names shorter
    mutate(variable=gsub(pattern = "Secondary Energy\\|Electricity\\|",replacement="",x=variable))|>
    rbind(data_hist_full|>filter(region=="GBR",variable %in% elecs[1],model=="EMBER") |>
            #make variable names shorter
            mutate(variable=gsub(pattern = "Secondary Energy\\|Electricity\\|",replacement="",x=variable)))
    
  
  #change order of variables, such that order of Fouquet plot gets reproduced
  plot2$variable = factor(x = plot2$variable,levels=rev(c("Biomass",
                                                        "Coal","Gas","Oil","Nuclear","Hydro",
                                                        "Wind","Solar")))
  #make area plot
  ggplot()+
    geom_area(data=plot2|>filter(year>1984),
              #convert from EJ to TWh
              aes(x=year,y=value*277.7,fill=variable),stat = 'identity',alpha=0.7)+theme_bw()+
    #use default color scale defined in src/functions.R
    fillScalePeFuel+
    ylab("Electricity generation (TWh)")+xlab("")+xlim(1975,2026)+
    scale_x_continuous(breaks=c(1990,2000,2010,2020),labels=c(1990,2000,2010,2020))
  #save to file
  ggsave("figures/El_UK.png",width=6,height=4)
  
  ### calculate technology shares for electricity mix in IEA NZE scenario in 2050 and 2023 -----
  #show all datapoints for 2050 that include 'Secondary Energy|Electricity'
  data_hist_full|>filter(year==2050,scenario=="Net Zero Emissions by 2050 Scenario",
                         variable %in% grep("Secondary Energy\\|Electricity",unique(data_hist_full$variable),value=T))
  #check that sum of components is equal to total (and display decimal by multiplication with 10)
  data_hist_full|>filter(year==2050,scenario=="Net Zero Emissions by 2050 Scenario",variable %in% grep("Secondary Energy\\|Electricity",unique(data_hist_full$variable),value=T)) |>
    filter(!variable %in% c("Secondary Energy|Electricity",
                            "Secondary Energy|Electricity|Fossil|w/ CC","Secondary Energy|Electricity|Fossil|w/o CC")) |>
   summarize(value=sum(value)*10)    
  
  #calculate share of total, only for datapoints with unique contribution
  data_hist_full|>filter(year==2050,scenario=="Net Zero Emissions by 2050 Scenario",variable %in% grep("Secondary Energy\\|Electricity",unique(data_hist_full$variable),value=T)) |>
    filter(!variable %in% c("Secondary Energy|Electricity",
                            "Secondary Energy|Electricity|Fossil|w/ CC","Secondary Energy|Electricity|Fossil|w/o CC")) |>
    mutate(value=100*
             value/285.8)
  
  #2023 data from EMBER
  #check that total equal sum of unique components
  data_hist_full|>filter(region=="World",year==2023,model=="EMBER",variable=="Secondary Energy|Electricity")
  data_hist_full|>filter(region=="World",year==2023,model=="EMBER",variable %in% grep("Secondary Energy\\|Electricity",unique(data_hist_full$variable),value=T)) |>
    filter(!variable %in% c("Secondary Energy|Electricity",grep("Share",unique(data_hist_full$variable),value=T))) |> 
    summarize(value=sum(value)*10)
  
  data_hist_full|>filter(region=="World",year==2023,model=="EMBER",variable %in% grep("Secondary Energy\\|Electricity",unique(data_hist_full$variable),value=T)) |>
    filter(!variable %in% c("Secondary Energy|Electricity",grep("Share",unique(data_hist_full$variable),value=T))) |> 
    mutate(value=100*value/106.1)
  
  
  
  
  # script to plot wind capacity 
  #select the year and the variable to be filtered
  selected_year <- 2020
  selected_variable <- "Secondary Energy|Electricity|Wind"
  
  #filter the data
  data_filtered <- data_hist_full %>%
       filter(variable == selected_variable, year == selected_year) %>%
         select(region, value)
  
  #preprocess the world population data available publicly
  global_population <- read.csv("output/global_population.csv", skip = 3)
  colnames(global_population) <- gsub("^X", "", colnames(global_population))
  colnames(global_population) <- trimws(colnames(global_population))
  global_population <- global_population[, colnames(global_population) != ""]
  global_population_full <- global_population %>%
       pivot_longer(
           cols = starts_with("19") | starts_with("20"), # Select only year columns
           names_to = "year",
           values_to = "population"
         ) %>%
      mutate(year = as.numeric(year))
  
 #merge historical data with population data
  data_per_capita <- data_filtered %>%
      left_join(global_population_full%>% filter(year == selected_year), by = c("region" = "Country.Code")) %>%
      mutate(per_capita_value = value / population)
  #Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  map_data <- world %>%
    left_join(data_per_capita, by = c("iso_a3" = "region"))
  
  #plot choropleth map
    ggplot(data = map_data) +
          geom_sf(aes(fill = per_capita_value), color = "white", size = 0.2) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = paste0(selected_variable, "\n(kW per person)")) +
       theme_minimal() +
         labs(title = paste0("Per-Capita ", selected_variable, " by Country (", selected_year, ")"),
                     caption = "Data Source: IAMC Historical Data") +
     theme(legend.position = "bottom")
  