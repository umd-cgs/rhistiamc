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

#add EU27BX data
eu27bx <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE")
data_hist <- data_hist |> rbind(
  data_hist |> filter(!variable %in% unique(data_hist[data_hist$region =="EU27BX",]$variable),
                      region %in% eu27bx) |> group_by(model,scenario,unit,variable,year) |> 
    summarize(value=sum(value)) |> mutate(region="EU27BX"))

#### AND read in scenario data
# NOTE: change option to anything besides "gcam" to read in general multi-model results
data_scen_option <- "ngfs_phase_IV"
if (data_scen_option == "ngfs_phase_IV") { 
  
  #use the publicly available 3 model scenario set from phase 4
  #file from download page from https://data.ene.iiasa.ac.at/ngfs/#/workspaces
  data_scen <- read_xlsx(file.path("runs/", "IAM_data.xlsx"))
  
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

#### 3. Define countries of interest -----
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


#### most so far don't directly make use of the data loaded centrally here,
#### nor of the country set defined above.

# start_yr <- 2000
  start_yr <- 1990
  
  ##### sectoral emission shares ------
  source("src/emiss_hist_GHG_shares.R")
 
  ##### sectoral emissions and targets ----
  source("src/emiss_NDC_NGFS_inexpolation.R")
  
  ##### emission - comparing scenarios ----
  ##### makes use of targets defined in previous script
  # source("src/emiss_GHG_figures.R")
  source("src/emiss_GHG_figures_new.R")
  
  ##### waterfall of scenario emission reductions ----
  source("src/emiss_waterfall.R")
  
  ##### land use emission plots ----- 
  #seems currently not to work as it is looking for dat_land
  # source("src/emiss_land_use.R")

  ##### land use emission plots -----
  #seems currently not to work as it is looking for dat_ceds (which could just be ceds part of data_hist?)
  # source("src/emiss_nonCO2_bysector.R")
  
  ##### electricity generation mix -----
  source("src/elec_generation_mix.R")
  
  ##### electricity use by sector-----
  source("src/elec_sectoral_use.R")
  
  ##### electricity 2030 developement space: S+W and Efficiency ----
  # source("src/elec_VRE-Eff.R")
  
  ##### use and domestic supply of fossil fuels -----
  source("src/fossil_figures.R")

  ##### scale up of EVs compared to Norway and China -----
  source("src/trn_ev_figures.R")

  # by default not run
  if(F){
  ##### rank countries -----
  # Select which variables to rank
  variables_pp <- c(
    'Emissions|CO2|Buildings',
    'Emissions|CO2|Industry',
    'Emissions|CO2|Other',
    'Emissions|CO2|Supply|Electricity',
    'Emissions|CO2|Supply|Other',
    'Emissions|CO2|Transport',
    'Emissions|N2O',
    'Emissions|CH4', 
    'GDP',
    'Sales Share|Transportation|Passenger|LDV|BEV+PHEV',
    'Sales Share|Transportation|ICE',
    'Sales Share|Transportation|Passenger|LDV|BEV'
  )
  
  variables_abs <- c('Secondary Energy|Electricity|Wind|Share',
                     'Secondary Energy|Electricity|Solar|Share',
                     'Secondary Energy|Electricity|Solar+Wind|Share')

  source("src/historic_regions_ranking.R")
  
  
  ##### Per-person variable plots ------
  source('src/top_x_per_capita_bar.R')
  }