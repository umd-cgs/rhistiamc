#### 0. Load Libraries and constants -----
library(tidyverse)
library(dplyr)
library(readxl)
library(quitte) # Download from https://pik-piam.r-universe.dev/quitte#
library(readxl)
library(countrycode)
library(openxlsx)
library(zoo) #function na.approx to linearly interpolate

#source functions and constants
source("src/functions.R")


# Choose the desired regions for the analysis, in addition to by country: Model name / number of regions
# model_regions <- "gcam32"   # Using GCAM core regions
# model_regions <- "r10"  # R10: ten regions making up the world
model_regions <- "r5"  # R5: five regions making up the world

#### 1. Update, load hist & scen data -----


# PUT the IAM_data.xlsx, downloaded from the following link, and 
# any other processed scenarios results into the runs/ folder 
# within this local repo

# https://data.ece.iiasa.ac.at/ngfs-phase-4/#/downloads

# IF it's desired to use the provided historical data in the IAMC format, rather 
# than re-create it using process_hist_data, then download it from:
# !!! ADD IN THIS WEBSITE LINK-- in our Github README !!!-----------
# into your local output/ folder

## CHANGE ABOVE!!##

#### THEN read in historic data in wide format and convert to long
data_hist <- read.csv("output/historical_iso.csv", na.strings = "NA")
colnames(data_hist) <- gsub("^X", "", colnames(data_hist))

data_hist <- data_hist |>
  pivot_longer(cols = -c(model, variable, region, unit, scenario),
               names_to = "year",
               values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |> 
  mutate(value=as.numeric(value),
         year=as.numeric(year))

# Use model_regions setting from the constants step to choose region mapping
if (model_regions == "gcam32"){
  reg_map <- read.csv("mappings/iso_EI_GCAM_regID.csv",skip = 6) |>
    left_join(read.csv("mappings/GCAM_region_names.csv",skip = 6))|>
    mutate(iso=toupper(iso))
  
  ## check_match(data_iso, reg_map, "iso")
  
} else if(model_regions == "r5"){
  ## aggregate to R5: five regions making up the world
  reg_map <- read.csv2("mappings/regionmappingR5.csv") |> 
    rename(iso=CountryCode,region=RegionCode)
  
} else if(model_regions == "r10"){ 
  ## aggregate to R10: ten regions making up the world
  reg_map <- read.csv("mappings/iso_r10.csv") 
  
}

# Add aggregated regions to the historical data
data_hist <- data_hist |>
  bind_rows(aggregate_regions(data_hist, reg_map) |>
              # Remove regions that are in the iso and World data:
              filter(!(region %in% unique(data_hist$region)))) |>
  unique()

data_hist <- data_hist |>
  mutate(region = gsub("R10AFRICA", "Africa (R10)", region),
         region = gsub("R10CHINA+", "China+ (R10)", region),
         region = gsub("R10EUROPE", "Europe (R10)", region),
         region = gsub("R10INDIA+", "India+ (R10)", region),
         region = gsub("R10LATIN_AM", "Latin America (R10)", region),
         region = gsub("R10MIDDLE_EAST", "Middle East (R10)", region),
         region = gsub("R10NORTH_AM", "North America (R10)", region),
         region = gsub("R10PAC_OECD", "Pacific OECD (R10)", region),
         region = gsub("R10REF_ECON", "Reforming Economies (R10)", region),
         region = gsub("R5_ASIA", "Asia (R5)", region),
         region = gsub("R5_LAM", "Latin America (R5)", region),
         region = gsub("R5_MAF", "Middle East & Africa (R5)", region),
         region = gsub("R5_OECD", "OECD & EU (R5)", region),
         region = gsub("R5_REF", "Reforming Economies (R5)", region))

  

#add EU27BX data
eu27bx <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE")
data_hist <- data_hist |> rbind(
  data_hist |> filter(!variable %in% unique(data_hist[data_hist$region =="EU27BX",]$variable),
                      region %in% eu27bx) |> group_by(model,scenario,unit,variable,year) |> 
    summarize(value=sum(value)) |> mutate(region="EU27BX"))

#### AND read in scenario data
# Choose which scenario data to read in
data_scen_option <- "ngfs_phase_IV"
# data_scen_option <- 

if (data_scen_option == "ngfs_phase_IV") { 
  
  #use the publicly available 3 model scenario set from phase 4
  #file from download page from https://data.ene.iiasa.ac.at/ngfs/#/workspaces
  data_scen <- read_xlsx(file.path("runs/", "IAM_data.xlsx"))
  colnames(data_scen) <- tolower(colnames(data_scen))
  
  data_scen <- data_scen |>
    pivot_longer(cols = -c(model, variable, region, unit, scenario),
                 names_to = "year",
                 values_to = "value") |>
    arrange(model, region, year, variable, scenario) |>
    filter(!is.na(value)) |> 
    mutate(value=as.numeric(value),
           year=as.numeric(year))
  
  
  
} else {

  # # Define another set of scenario data processed in IAMC format:
  # data_scen <-
  
}


####### 2. (Optionally) Make country adjustments ----
#### read in country-specific scenario data for a few special countries
country_adjust <-T
if(country_adjust){

  # # Read in country-provided data and overwrite or modify 
  # # some of the scenario results, eg. for historical years
  # data_scen_c <- 
  #   
  # data_scen <- 
  
}

#### 3. Define countries / regions of interest -----
# differentiated by priority

#first column: name
#second column: Iso code
#third column: priority group
#4th-6th column: scenario names to use: 4th is first (central), 5th shifted to left, 6th to right
#if there are 2 reference scenarios, these can be 4th and 5th or 5th and 6th 
#7th to 9th column: scenario display names

countries <- matrix(ncol=9,byrow = T,data=c(
  "South Korea","KOR",1,"o_1p5c","d_delfrag","BEP11","High Ambition","Low Ambition","11th BEP",
  "USA","USA",4,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Asia (R5)", "Asia (R5)", 1, "Nationally Determined Contributions (NDCs)", "Current Policies", "Net Zero 2050", "NDC", "Cpol", "NZ2050",
  "Latin America (R5)", "Latin America (R5)", 1, "Nationally Determined Contributions (NDCs)", "Current Policies", "Net Zero 2050", "NDC", "Cpol", "NZ2050",
  "Middle East & Africa (R5)", "Middle East & Africa (R5)", 1, "Nationally Determined Contributions (NDCs)", "Current Policies", "Net Zero 2050", "NDC", "Cpol", "NZ2050",
  "OECD & EU (R5)", "OECD & EU (R5)", 1, "Nationally Determined Contributions (NDCs)", "Current Policies", "Net Zero 2050", "NDC", "Cpol", "NZ2050",
  "Reforming Economies (R5)", "Reforming Economies (R5)", 1, "Nationally Determined Contributions (NDCs)", "Current Policies", "Net Zero 2050", "NDC", "Cpol", "NZ2050",
  "World","World",5,"High Ambition","Low Ambition",NA,"High Ambition","Low Ambition",NA))

countries <- data.frame(region=countries[,1],
                        iso=countries[,2],
                        priority=countries[,3],
                        scen1=countries[,4],
                        scen2=countries[,5],
                        scen3=countries[,6],
                        name1=countries[,7],
                        name2=countries[,8],
                        name3=countries[,9])


  
  #to use only selection, in-comment appropriate line below
  # countries <- countries |> filter(iso %in% c("ARG","SAU","TUR","RUS","MEX"))
  countries <- countries |> filter(iso %in% c("KOR","World"))
# countries <- countries |> filter(priority==1)

  
  
#### 4. Run the plot scripts -----
# in the order shown below (some build on the outputs of previous scripts). 


# start_yr <- 2000
  start_yr <- 1990
 
  ##### Electricity generation mix -----
  source("src/fig_elec_generation_mix.R")
  
  
  ##### Line plots  ---------------------
  #define set of variables to plot in fig_line_comparison.R
  
  models <- unique(data_scen$model); models
  # Select models to plot
  models <- models[grepl("GCAM",models)]; models
  
  scenarios <- unique(data_scen$scenario); scenarios
  # Select scenarios to plot
  scenarios <- scenarios[grepl("Below 2",scenarios) | 
                           grepl("Current Pol", scenarios) | 
                           grepl("NDC", scenarios) | 
                           grepl("Net Zero", scenarios)]; scenarios
  
  vars <- data.frame(
    vars=c(
      "Primary Energy|Gas"
      ),
    names=c("PE_gas"
    )
  )
  
  # vars <- data.frame(
  #   vars=c(
  #     "Temperature|Global Mean","Emissions|CO2|Energy and Industrial Processes","Emissions|CO2","Emissions|Kyoto Gases","Primary Energy|Oil","Primary Energy|Coal","Primary Energy|Gas",
  #     "Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind","Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Nuclear",
  #     "Secondary Energy|Electricity|Coal","Secondary Energy|Electricity|Gas","Final Energy|Industry","Final Energy|Industry|Solids|Coal", "Final Energy|Transportation",
  #     "Population","GDP|PPP", "Price|Carbon", "Final Energy|Residential and Commercial"
  #   ),
  #   names=c("Temp","Emi_co2_FFI","Emi_co2","Emi_kyo","PE_oil","PE_coal","PE_gas",
  #           "Elec_solar","Elec_wind","Elec_hydro","Elec_nuclear",
  #           "Elec_coal","Elec_gas","FE_ind", "FE_ind_coal","FE_trp" ,
  #           "Pop","GDP_ppp", "Carbon_price", "FE_rescom",
  #   )
  # )
  
  source("src/fig_line_comparison.R")
  
  