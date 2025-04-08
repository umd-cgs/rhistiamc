#### 0. Load Libraries and constants -----
library(tidyverse)
library(dplyr)
library(readxl)
#library(quitte) # Download from https://pik-piam.r-universe.dev/quitte#
library(readxl)
library(countrycode)
library(openxlsx)
library(zoo) #function na.approx to linearly interpolate
library(assertthat) # For explanatory error messages


#source functions and constants
source("src/functions.R")


#### 1. Define settings -------------

# # Define starting year for the plots 
# start_yr <- 2000
start_yr <- 1990

# # Choose the desired regions for the analysis
# model_regions <- "iso"   # ISO: Using country codes
# model_regions <- "gcam32"    # Using GCAM core regions
# model_regions <- "r10"   # R10: ten regions making up the world
model_regions <- "r5"   # R5: five regions making up the world


#### 2. Load data ------------------

##### 2.1 Historical data -----

# IF it's desired to use the provided historical data in the IAMC format, rather 
# than re-create it using src/process_hist_data.R, then download the files from:
# PUBLIC_output_rhistiamc:
# https://drive.google.com/open?id=117cTkVRekeu3vHYrFkH93zstpCqGxkM8&usp=drive_fs
# into your local output/ folder


#### THEN read in historic data in wide format and convert to long
data_hist_full <- read.csv(paste0("output/historical_",model_regions,".csv"), na.strings = "NA")
colnames(data_hist_full) <- gsub("^X", "", colnames(data_hist_full))

data_hist_full <- data_hist_full |>
  pivot_longer(cols = -c(model, variable, region, unit, scenario),
               names_to = "year",
               values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |> 
  mutate(value=as.numeric(value),
         year=as.numeric(year))


#add EU27BX data
eu27bx <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE")

data_hist_full <- data_hist_full |> rbind(
  data_hist_full |> filter(!variable %in% unique(data_hist_full[data_hist_full$region =="EU27BX",]$variable),
                      region %in% eu27bx) |> group_by(model,scenario,unit,variable,year) |> 
    summarize(value=sum(value)) |> mutate(region="EU27BX"))

# for GDP, keep only the OECD values
data_hist_full <- data_hist_full |>
  filter(!(grepl("IIASA GDP", model))) |>
  # and only keep one of the IEA WEO scenarios, since they have the same values as each other
  filter(!(grepl("GDP", variable) & scenario %in% c("Net Zero Emissions by 2050 Scenario", "Announced Pledges Scenario")))


# convert all USD values to a recent year
data_hist_full <- data_hist_full |>
  harmonize_usd_year(2021)


##### 2.2 Scenario data -------------------------------


# PUT the IAM_data.xlsx, downloaded from the following link, and 
# any other processed scenarios results into the runs/ folder 
# within this local repo

# https://data.ene.iiasa.ac.at/ngfs/#/downloads
# Login as guest > Downloads > 1729832902777-V5.0-NGFS-Phase-5.zip (or latest) > unzip to runs/ folder 

# Choose which scenario data to read in
data_scen_option <- "ngfs_phase_V"
# data_scen_option <- 

#use the publicly available 3 model scenario set from phase 5
if (data_scen_option == "ngfs_phase_V") { 
  

  data_scen_full <- read_xlsx(file.path("runs/", "IAM_data.xlsx"))
  colnames(data_scen_full) <- tolower(colnames(data_scen_full))
  
  data_scen_full <- data_scen_full |>
    pivot_longer(cols = -c(model, variable, region, unit, scenario),
                 names_to = "year",
                 values_to = "value") |>
    arrange(model, region, year, variable, scenario) |>
    filter(!is.na(value)) |> 
    mutate(value=as.numeric(value),
           year=as.numeric(year))
  
  data_scen_full <- data_scen_full |>
    mutate(region = gsub("GCAM 6.0 NGFS\\|", "", region),
           region = gsub("MESSAGEix-GLOBIOM 2.0-R12\\|", "", region),
           region = gsub("REMIND-MAgPIE 3.3-4.8\\|", "", region))
  

  
} else {

  # # Define another set of scenario data processed in IAMC format:
  # data_scen_full <-
  
}


# Adjust the format of the scenario data as necessary to match the hist_data

if (model_regions == "iso") {
  
  # Remove World region temporarily, since it won't map to an ISO using countrycode
  data_scen_world <- filter(data_scen_full, region == "World")
  data_scen_full <- filter(data_scen_full, region != "World")
  
  data_scen_full$iso <- countrycode(data_scen_full$region, 'country.name', 'iso3c')
  
  data_scen_full <- data_scen_full |>
    rename(region.name = region, region = iso) |>
    filter(!is.na(region))
  
  data_scen_full <- bind_rows(data_scen_full, 
                              data_scen_world |>
                                mutate(region.name = "World"))
  
  
} else if (model_regions == "r10") {
  
  data_scen_full <- data_scen_full |>
    mutate(region = gsub("Africa (R10)", "R10AFRICA", region),
           region = gsub("China+ (R10)", "R10CHINA+", region),
           region = gsub("Europe (R10)", "R10EUROPE", region),
           region = gsub("India+ (R10)", "R10INDIA+", region),
           region = gsub("Latin America (R10)", "R10LATIN_AM", region),
           region = gsub("Middle East (R10)", "R10MIDDLE_EAST", region),
           region = gsub("North America (R10)", "R10NORTH_AM", region),
           region = gsub("Pacific OECD (R10)", "R10PAC_OECD", region),
           region = gsub("Reforming Economies (R10)", "R10REF_ECON", region))
  
  
} else if (model_regions == "r5") {
  
  data_scen_full <- data_scen_full |>
    mutate(region = gsub("Asia \\(R5\\)", "R5_ASIA", region),
           region = gsub("Latin America \\(R5\\)", "R5_LAM", region),
           region = gsub("Middle East & Africa \\(R5\\)", "R5_MAF", region),
           region = gsub("OECD & EU \\(R5\\)", "R5_OECD", region),
           region = gsub("Reforming Economies \\(R5\\)", "R5_REF", region))
}

# convert all USD values to a recent year
data_scen_full <- data_scen_full |>
  harmonize_usd_year(2021)


#### 3. Choose scenarios, models, regions ---------------
# of interest for focus in this analysis and plotting

scenarios_avail <- unique(data_scen_full$scenario); scenarios_avail
# Select scenarios to plot (all or a subset)
scenarios_selected <- scenarios_avail[c(1:2,7:6)]; scenarios_selected
# scenarios_selected <- scenarios_avail[c(1:length(scenarios_avail))]; scenarios_selected




models_avail <- unique(data_scen_full$model); models_avail
# Select models to plot

# models_selected <- models_avail[grepl("GCAM",models_avail)]; models_selected
models_selected <- c("GCAM 6.0 NGFS","MESSAGEix-GLOBIOM 2.0-M-R12-NGFS","REMIND-MAgPIE 3.3-4.8"); models_selected


if (model_regions %in% c("r10", "r5")){
  regions_avail <- unique((filter(data_hist_full, grepl(toupper(model_regions), region) | region == "World"))$region); regions_avail
} else {
  regions_avail <- unique(data_hist_full$region); regions_avail
}

# Select all regions or choose a subset
regions_selected <- regions_avail; regions_selected
#  # For China (CHN) when using model_regions <- "iso"
# regions_selected <- regions_avail[c(44, 248)]; regions_selected

# regions_overlapping <- check_match(data_scen_full, data_hist_full, "region", opt = "i")

regions_missing <- regions_selected[!(regions_selected %in% unique(data_scen_full$region))]
if (length(regions_missing) != 0) {
  print("These regions names do not appear in data_scen_full: ")
  print(regions_missing)
}

# Filter scenario and historical data accordingly
data_scen <- data_scen_full |>
  filter(model %in% models_selected,
         scenario %in% scenarios_selected,
         region %in% regions_selected)

data_hist <- data_hist_full |>
  filter(region %in% regions_selected)

#define set of variables to plot in fig_line_comparison.R
vars <- data.frame(
  vars=c(
    "Primary Energy|Gas"
  ),
  # Aliases for the variables to include in the saved filenames
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
#   # Aliases for the variables to include in the saved filenames
#   names=c("Temp","Emi_co2_FFI","Emi_co2","Emi_kyo","PE_oil","PE_coal","PE_gas",
#           "Elec_solar","Elec_wind","Elec_hydro","Elec_nuclear",
#           "Elec_coal","Elec_gas","FE_ind", "FE_ind_coal","FE_trp" ,
#           "Pop","GDP_ppp", "Carbon_price", "FE_rescom",
#   )
# )


  
#### 4. Run the plot scripts -----
  
  ##### Line plots  ---------------------
  
  source("src/fig_line_comparison.R")
  
 
  ##### Electricity generation mix -------
  # NOTE: These figures can only accommodate up to three scenarios at once, so 
  # it disregards the later ones if more than three supplied 
  
  source("src/fig_elec_generation_mix.R")
  

  
