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

# Set google drive location
os <- get_os()[[1]]

dir_output <- "unset"
dir_data <- "unset"
dir_runs <- "unset"

if(os == "windows"){
  dir_output <- "G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/output_both_repos/"
  dir_data <- "G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/data_pathways/"
  dir_runs <- "G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/runs_pathways/"
  
} else if (os == "macosx") {
  dir_output <- "/Volumes/GoogleDrive/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/output_both_repos/"
  dir_data <- "/Volumes/GoogleDrive/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/data_pathways/"
  dir_runs <- "/Volumes/GoogleDrive/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/runs_pathways/"
}

dir_output = ifelse(dir.exists(dir_output), dir_output, "output/")
print("Using the processed results in this output directory: "); dir_output

dir_data = ifelse(dir.exists(dir_data), dir_data, "data/")
print("Using the un-processed data in this directory: "); dir_data

dir_runs = ifelse(dir.exists(dir_runs), dir_runs, "runs/")
print("Using the IAM runs in this directory: "); dir_runs


#### 1. Update, load hist & scen data -----

# CHECK if the historical data processed outputs (from the hist_iamc repo) have 
# been updated at this location:

# G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/output_both_repos
# https://drive.google.com/drive/u/0/folders/1TVv7d-kgyqqhK3ojneWNsZjW01YTxEVp

# IF SO, put the generated datasets (Rds files with source specific, more detailed data, 
# and csv files with harmonized set of IAMC variables) into the output/ folder 
# within this local repo


# COPY also any new IAM scenario processed results from this location:

# G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/runs_pathways
# https://drive.google.com/drive/u/0/folders/1yxX9ZwjVGIM4xJlgrB3ikTexivBM1ZpM

# into the runs/ folder within this local repo


# COPY also any new unprocessed historical data from this location:

# G:/Shared drives/CGS GCAM/Digital Infrastructure/historic_data repo/data_pathways
# https://drive.google.com/open?id=1QwuytFoRzQ_ETWFwhDhDAsaweAi50OEt&usp=drive_fs

# into the data folder within this repo

#### THEN read in historic data in wide format and convert to long
data_hist <- read.csv(file.path(dir_output, "historical_iso.csv"), na.strings = "NA")
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

#add World data
data_hist <- data_hist |> rbind(
  read.csv(file.path(dir_output, "historical_ref_gcam32.csv")) |> filter(region=="World") |>
    pivot_longer(cols =c(-variable,-unit,-region,-model,-scenario),names_to="year") |>
    mutate(year=as.numeric(substr(year,2,5)))
)

#### AND read in scenario data
# NOTE: change option to anything besides "gcam" to read in general multi-model results
data_scen_option <- "gcam"
if(data_scen_option == "gcam"){
  # data_scen <- read_csv(file.path(dir_runs,"NGFS_GCAM_Results_Phase5_5-17_reprocess_2024Jun14.csv")) |>
  data_scen <- read_xlsx(file.path(dir_runs,"/1718738055145-NGFS_GCAM_Results_GDP_endo_v2_6-18.xlsx")) |>
    pivot_longer(cols = c(-"Scenario",-"Model",-"Region",-"Variable",-"Unit"), 
                 names_to = "year", values_to = "value") |>
    mutate(year = as.integer(sub("X", "", year)),
           value = as.numeric(value))  |> 
    rename_with(tolower)
  # add downscaled data
  data_ds <- read_xlsx(file.path(dir_runs,'NGFS_phase5_downscaled_GCAM_2050_relevantVars.xlsx'))|>
    pivot_longer(cols = c(-"Scenario",-"Model",-"Region",-"Variable",-"Unit"), 
                 names_to = "year", values_to = "value") |>
    mutate(year = as.integer(sub("X", "", year)),
           value = as.numeric(value))  |> 
    rename_with(tolower)
  
  ##### make emission variable set consistent  ----
  ##### with the historical data and GCAM internal results:
  data_ds <- data_ds |> mutate(variable=case_when(
    variable=="Emissions|Kyoto Gases (incl. indirect LULUCF)" ~ "Emissions|Kyoto Gases (incl. all LULUCF)",
    variable=="Emissions|CO2|LULUCF Direct+Indirect" ~ "Emissions|CO2|AFOLU (incl. all LULUCF)",
    variable=="Emissions|CO2" ~ "Emissions|CO2 (incl. all LULUCF)",
    .default = variable
  ))
  #calculate a few additional emissions variables to have better comparability with model data, and historic data
  data_ds <-
    data_ds |> rbind(
      data_ds |> filter(variable %in% c("Emissions|CO2|LULUCF Indirect","Emissions|Kyoto Gases (incl. all LULUCF)",
                                        "Emissions|CO2 (incl. all LULUCF)","Emissions|CO2|AFOLU (incl. all LULUCF)",
                                        "Emissions|CO2|Energy", "Emissions|CO2|Industrial Processes")) |> select(-unit)|>
        pivot_wider(names_from = variable,values_fill = 0) |>
        mutate(`Emissions|CO2|AFOLU` =`Emissions|CO2|AFOLU (incl. all LULUCF)`-`Emissions|CO2|LULUCF Indirect`,
               `Emissions|CO2` = `Emissions|CO2 (incl. all LULUCF)` - `Emissions|CO2|LULUCF Indirect`,
               `Emissions|Kyoto Gases`=`Emissions|Kyoto Gases (incl. all LULUCF)` - `Emissions|CO2|LULUCF Indirect`,
               `Emissions|CO2|Energy and Industrial Processes`=`Emissions|CO2|Energy`+`Emissions|CO2|Industrial Processes`) |>
        pivot_longer(cols=c(-model,-scenario,-region,-year),names_to = "variable") |>
        filter(variable %in% c("Emissions|CO2|AFOLU","Emissions|CO2","Emissions|Kyoto Gases","Emissions|CO2|Energy and Industrial Processes"))|>
        mutate(unit=case_when(
          variable %in% c("Emissions|CO2|AFOLU","Emissions|CO2","Emissions|CO2|Energy and Industrial Processes") ~ "Mt CO2/yr",
          variable =="Emissions|Kyoto Gases" ~ "Mt CO2-equiv/yr"
        )) |> filter(year>2019) # filter out pre 2020 data, as LULUCF Indirect is only estimated from 2020 onward
    )
  
  #rename all countries to country names, but also keep EU27
  data_ds <- rbind(data_ds |>filter(region=="EU27")|>mutate(region="EU27BX"),
                   data_ds |> filter(!region %in% c("EU27","Downscaling|Countries without IEA statistics")) |>
                     mutate(region=countrycode(region,origin='iso3c',destination='country.name'))) |>
    mutate(region = case_when(
      region=="United States" ~ "USA",
      .default = region
    ))
  # check for which GCAM region we can make use of downscaled data to add estimates for emissions incl. indirect LULUCF          
  gcam_singles <- intersect(unique(data_ds$region),unique(data_scen$region))
  data_ds |> filter(variable=="Emissions|CO2|LULUCF Indirect",region %in% gcam_singles, year==2020, scenario=="o_2c")
  
  gcam_indirect <- rbind(data_ds|>filter(region %in% c("China","USA"),variable=="Emissions|CO2|LULUCF Indirect"),
                         data_scen|>filter(region %in% c("China","USA"),variable %in% c("Emissions|CO2|AFOLU","Emissions|CO2","Emissions|Kyoto Gases"),year>2019))|>
    select(-unit,-model) |>    pivot_wider(names_from = variable,values_fill = 0) |>
    mutate(`Emissions|CO2|AFOLU (incl. all LULUCF)` =`Emissions|CO2|AFOLU`+`Emissions|CO2|LULUCF Indirect`,
           `Emissions|CO2 (incl. all LULUCF)` = `Emissions|CO2` + `Emissions|CO2|LULUCF Indirect`,
           `Emissions|Kyoto Gases (incl. all LULUCF)`=`Emissions|Kyoto Gases` + `Emissions|CO2|LULUCF Indirect`) |>
    pivot_longer(cols=c(-scenario,-region,-year),names_to = "variable") |>
    filter(variable %in% c("Emissions|CO2|LULUCF Indirect","Emissions|CO2|AFOLU (incl. all LULUCF)","Emissions|CO2 (incl. all LULUCF)","Emissions|Kyoto Gases (incl. all LULUCF)"))|>
    mutate(model="GCAM 6.0",unit=case_when(
      variable %in% c("Emissions|CO2|AFOLU (incl. all LULUCF)","Emissions|CO2 (incl. all LULUCF)","Emissions|CO2|LULUCF Indirect") ~ "Mt CO2/yr",
      variable =="Emissions|Kyoto Gases (incl. all LULUCF)" ~ "Mt CO2-equiv/yr"
    )) 
  
  
  #iso country codes of all countries for which downscaled data exists, and which are part of a "single-country" gcam region (mac,pri,vir und o-nam don't have downscaled data)
  # gcam_singles_isos <- c("ARG","BRA","CAN","CHN","HKG","COL","IND","IDN","MEX","PAK","RUS","ZAF","KOR","TWN","USA")
  
  #create harmonized country data set, consisting of model native gcam regions, and downscaled data for countries that
  # do not have an individual gcam region (or are part of one like HKG for China), and also keep the land-use indirect emission estimate for those regions to calculate 
  # inventory fitting data (although only available for CHN and USA), as well as the additional variables that can be calculated with these.
  #Finally, also add the Kyoto totals with the new name
  data_scen  <- rbind(data_ds|>filter(!region %in% c(gcam_singles,"Hongkong SAR China")),
                      gcam_indirect,
                      data_scen,
                      data_scen|>filter(!region %in% unique(gcam_indirect$region),variable=="Emissions|Kyoto Gases")|>
                        mutate(variable="Emissions|Kyoto Gases (incl. all LULUCF)"))
  
  #####calculate excl LUC Kyoto Gas variable ----
  data_scen <- rbind(data_scen,
                     data_scen |> filter(variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU"))|>
                       select(-unit)|>pivot_wider(names_from = variable) |>
                       mutate(`Emissions|Kyoto Gases (excl. LUC)`=`Emissions|Kyoto Gases` - `Emissions|CO2|AFOLU`)|>
                       pivot_longer(cols=c(-model,-scenario,-region,-year),names_to = "variable") |>
                       filter(variable %in% c("Emissions|Kyoto Gases (excl. LUC)"))|>
                       mutate(unit= "Mt CO2-equiv/yr"))
  
} else {
  #if not only using gcam, an alternative option is to use the publicly available 3 model scenario set from phase 4
  #file from download page from https://data.ene.iiasa.ac.at/ngfs/#/workspaces
  data_scen <- read_xlsx(file.path(dir_runs, "IAM_data.xlsx"))
}
#save original data scenario to allow for comparison
data_scen_og <- data_scen

####### 2. (Optionally) Make country adjustments ----
#### read in country-specific scenario data for a few special countries
country_adjust <-T
if(country_adjust){
  
  ##### India----
  # flat 2025-2030 
  data_ind <- read.csv(file.path(dir_runs,'India_High_Amb_IAMCformat_7-11.csv')) |>
    pivot_longer(cols= c(-"Scenario",-"Model",-"Region",-"Variable",-"Unit"),names_to = "year") |>
    mutate(year=as.integer(sub("X","",year)),value = as.numeric(value)) |>
    rename_with(tolower) |> filter(region=="India")
  
  #calculate excl LUC Kyoto Gas variable
  data_ind <- rbind(data_ind,
                    data_ind |> filter(variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU"))|>
                      select(-unit)|>pivot_wider(names_from = variable) |>
                      mutate(`Emissions|Kyoto Gases (excl. LUC)`=`Emissions|Kyoto Gases` - `Emissions|CO2|AFOLU`,
                             `Emissions|Kyoto Gases (incl. all LULUCF)`=`Emissions|Kyoto Gases`)|>
                      pivot_longer(cols=c(-model,-scenario,-region,-year),names_to = "variable") |>
                      filter(variable %in% c("Emissions|Kyoto Gases (excl. LUC)",
                                             "Emissions|Kyoto Gases (incl. all LULUCF)"))|>
                      mutate(unit= "Mt CO2-equiv/yr"))
  
  data_scen <- rbind(data_ind,data_scen)
  
  #### Canada ----
  #: Kowan's scenarios
  #### updated on 2024-09-23 to reflect variable name changes, see below
  data_scen_c <- read_csv(file.path(dir_runs,"/Canada_results_v2.csv")) %>% ##Kowan's output for Canada scenarios
    pivot_longer(cols = c(-"Scenario",-"Model",-"Region",-"Variable",-"Unit"), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(sub("X", "", year)),
           value = as.numeric(value)) |>
    rename_with(tolower)
  data_scen_c <- rbind(data_scen_c|>filter(!variable %in% c("Emissions|GHG","Emissions|CO2|Electricity")),
                       data_scen_c|>filter(variable %in% c("Carbon Sequestration|DAC","Carbon Sequestration|LULUCF",
                                                           "Emissions|CO2","Emissions|GHG","Emissions|CO2|Electricity",
                                                           "Secondary Energy|Electricity|Gas|w/ CCS","Secondary Energy|Electricity|Gas|w/o CCS",
                                                           "Secondary Energy|Electricity|Biomass|w/ CCS","Secondary Energy|Electricity|Biomass|w/o CCS",
                                                           "Secondary Energy|Electricity|Oil|w/o CCS"))|>
                         select(-unit) |>    pivot_wider(names_from = variable,values_fill = 0) |>
                         mutate(`Emissions|CO2|Energy|Supply|Electricity` =`Emissions|CO2|Electricity`,
                                `Emissions|CO2 (incl. all LULUCF)` = `Emissions|CO2`  + `Carbon Sequestration|DAC` + `Carbon Sequestration|LULUCF`,
                                `Secondary Energy|Electricity|Oil` = `Secondary Energy|Electricity|Oil|w/o CCS`,
                                `Secondary Energy|Electricity|Gas` = `Secondary Energy|Electricity|Gas|w/o CCS`  + `Secondary Energy|Electricity|Gas|w/ CCS`,
                                `Secondary Energy|Electricity|Biomass` = `Secondary Energy|Electricity|Biomass|w/o CCS`  + `Secondary Energy|Electricity|Biomass|w/ CCS`,
                                `Emissions|Kyoto Gases (excl. LUC)`=`Emissions|GHG` + `Carbon Sequestration|DAC`,
                                `Emissions|Kyoto Gases`=`Emissions|GHG` + `Carbon Sequestration|DAC` + `Carbon Sequestration|LULUCF`,
                                `Emissions|Kyoto Gases (incl. all LULUCF)`=`Emissions|GHG` + `Carbon Sequestration|DAC` + `Carbon Sequestration|LULUCF`) |>
                         pivot_longer(cols=c(-model,-scenario,-region,-year),names_to = "variable") |>
                         filter(variable %in% c("Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2 (incl. all LULUCF)","Secondary Energy|Electricity|Oil",
                                                "Secondary Energy|Electricity|Gas","Secondary Energy|Electricity|Biomass","Emissions|Kyoto Gases (incl. all LULUCF)",
                                                "Emissions|Kyoto Gases (excl. LUC)","Emissions|Kyoto Gases"))|>
                         mutate(unit=case_when(
                           variable %in% c("Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2 (incl. all LULUCF)") ~ "Mt CO2/yr",
                           variable %in% c("Secondary Energy|Electricity|Oil","Secondary Energy|Electricity|Gas","Secondary Energy|Electricity|Biomass") ~ "EJ/yr",
                           variable %in% c("Emissions|Kyoto Gases (incl. all LULUCF)","Emissions|Kyoto Gases","Emissions|Kyoto Gases (excl. LUC)") ~ "Mt CO2-equiv/yr"
                         ))) 
  data_scen <- rbind(data_scen_c, data_scen)
  
  #### China ----
  # China team scenarios
  data_scen_ch <- read_xlsx(file.path(dir_runs,"data_cgs_synthesis_results_0627_2.xlsx")) %>% 
    pivot_longer(cols = c(-"Scenario",-"Model",-"Region",-"Variable",-"Unit"), names_to = "year", values_to = "value") %>%
    mutate(year = as.integer(sub("X", "", year)),
           value = as.numeric(value)) |>
    rename_with(tolower)
  #calculate excl LUC Kyoto Gas variable
  data_scen_ch <- rbind(data_scen_ch,
                        data_scen_ch |> filter(variable %in% c("Emissions|Kyoto Gases","Emissions|CO2|AFOLU"))|>
                          select(-unit)|>pivot_wider(names_from = variable) |>
                          mutate(`Emissions|Kyoto Gases (excl. LUC)`=`Emissions|Kyoto Gases` - `Emissions|CO2|AFOLU`)|>
                          pivot_longer(cols=c(-model,-scenario,-region,-year),names_to = "variable") |>
                          filter(variable %in% c("Emissions|Kyoto Gases (excl. LUC)"))|>
                          mutate(unit= "Mt CO2-equiv/yr"))
  #calculate incl all LULUCF variable 
  data_scen_ch <- data_scen_ch |> rbind(
    rbind(data_ds|>filter(region %in% c("China"),variable=="Emissions|CO2|LULUCF Indirect"),
          data_scen_ch|>filter(region %in% c("China"),variable %in% c("Emissions|CO2|AFOLU","Emissions|CO2","Emissions|Kyoto Gases"),year>2019))|>
      select(-unit,-model) |>    pivot_wider(names_from = variable,values_fill = 0) |>
      mutate(`Emissions|CO2|AFOLU (incl. all LULUCF)` =`Emissions|CO2|AFOLU`+`Emissions|CO2|LULUCF Indirect`,
             `Emissions|CO2 (incl. all LULUCF)` = `Emissions|CO2` + `Emissions|CO2|LULUCF Indirect`,
             `Emissions|Kyoto Gases (incl. all LULUCF)`=`Emissions|Kyoto Gases` + `Emissions|CO2|LULUCF Indirect`) |>
      pivot_longer(cols=c(-scenario,-region,-year),names_to = "variable") |>
      filter(variable %in% c("Emissions|CO2|LULUCF Indirect","Emissions|CO2|AFOLU (incl. all LULUCF)","Emissions|CO2 (incl. all LULUCF)","Emissions|Kyoto Gases (incl. all LULUCF)"))|>
      mutate(model="GCAM 6.0",unit=case_when(
        variable %in% c("Emissions|CO2|AFOLU (incl. all LULUCF)","Emissions|CO2 (incl. all LULUCF)","Emissions|CO2|LULUCF Indirect") ~ "Mt CO2/yr",
        variable =="Emissions|Kyoto Gases (incl. all LULUCF)" ~ "Mt CO2-equiv/yr"
      ))) 
  
  data_scen <- rbind(data_scen_ch, data_scen)
  
  
  #### Indonesia ----
  #Adjust Emissions for 
  data_indpower <- data_scen |> 
    filter(region=="Indonesia", scenario %in% c("d_delfrag","o_2c"),
           variable %in% c("Emissions|CO2|Energy|Supply|Electricity"))
  #overwrite with value from excel sheet - "CIPP pathway +Captive BAU",  and interpolated for 2040
  data_indpower[data_indpower$scenario=="d_delfrag" & data_indpower$year %in% seq(2020,2035,5),]  
  data_indpower[data_indpower$scenario=="d_delfrag" & data_indpower$year %in% seq(2020,2040,5),]$value <-
    c(288,401,381,341,200)
  data_indpower[data_indpower$scenario=="d_delfrag" & data_indpower$year %in% seq(2015,2050,5),]  
  ##difference to original
  diff <- data_indpower[data_indpower$scenario=="d_delfrag" & data_indpower$year %in% seq(2020,2040,5),]$value  -
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value
  ## add difference to Electricity CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total Kyoto
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total Kyoto excl. LUC
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  
  #overwrite with values from excel sheet - 1.5 JETP, and interpolated for 2040
  data_indpower[data_indpower$scenario=="o_2c" & data_indpower$year %in% seq(2020,2035,5),]  
  data_indpower[data_indpower$scenario=="o_2c" & data_indpower$year %in% seq(2020,2040,5),]$value <-
    c(288,382,334,255,160)
  data_indpower[data_indpower$scenario=="o_2c" & data_indpower$year %in% seq(2015,2050,5),]  
  ##difference to original
  diff <- data_indpower[data_indpower$scenario=="o_2c" & data_indpower$year %in% seq(2020,2040,5),]$value  -
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value
  ## add difference to Electricity CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|Energy|Supply|Electricity") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total Kyoto
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  ## add difference to total Kyoto excl. LUC
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2040,5),]$value + diff  
  
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2050,5),]
  
  #overwrite land-use emissions data
  
  lu_dat <- read.csv(file.path(dir_runs,'/luc data.csv')) |> pivot_longer(cols=seq(2,27),names_to="year")|>
    mutate(year=as.numeric(substr(year,start=2,stop=5))) |> rename(scenario=X) |> 
    mutate(scenario=case_when(
      scenario=="Land use high" ~ "o_2c",
      scenario=="Land use low" ~ "d_delfrag"
    ))
  ##difference to original
  diff <- lu_dat[lu_dat$scenario=="d_delfrag" & lu_dat$year %in% seq(2020,2060,5),]$value  -
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value
  ## add difference to AFOLU CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  ## add difference to total CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  ## add difference to total Kyoto
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
              data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  
  ##difference to original
  diff <- lu_dat[lu_dat$scenario=="o_2c" & lu_dat$year %in% seq(2020,2060,5),]$value  -
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value
  ## add difference to AFOLU CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2|AFOLU") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  ## add difference to total CO2
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CO2") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  ## add difference to total Kyoto
  data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
              data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value <-
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2060,5),]$value + diff  
  
  
  overwrite_ch4 <- T
  if(overwrite_ch4){
    #overwrite CH4 data  --- delfrag
    diff_indch4 <- c(14.5,14.5,13,11,9,8) -
      data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
                  data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value 
    ## add difference to total CO2
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value <-
      data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
                  data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value + diff_indch4  
    ## add difference to total Kyoto
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value <-
      data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
                  data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value + 
      diff_indch4 *gwp100[gwp100$entity=="CH4",]$gwp 
    ## add difference to total Kyoto (excl. LUC)
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value <-
      data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases (excl. LUC)") & 
                  data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2025,2050,5),]$value + 
      diff_indch4 *gwp100[gwp100$entity=="CH4",]$gwp 
    # #overwrite CH4 data  --- o_2c
    # diff_indch4 <- c(14.25,13,11,9,8,7.5) -
    #   data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
    #               data_scen$scenario=="o_2c" & data_scen$year %in% seq(2025,2050,5),]$value 
    # ## add difference to total CO2
    # data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
    #             data_scen$scenario=="o_2c" & data_scen$year %in% seq(2025,2050,5),]$value <-
    #   data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
    #               data_scen$scenario=="o_2c" & data_scen$year %in% seq(2025,2050,5),]$value + diff_indch4  
    # ## add difference to total Kyoto
    # data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
    #             data_scen$scenario=="o_2c" & data_scen$year %in% seq(2025,2050,5),]$value <-
    #   data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|Kyoto Gases") & 
    #               data_scen$scenario=="o_2c" & data_scen$year %in% seq(2025,2050,5),]$value + 
    #   diff_indch4 *gwp100[gwp100$entity=="CH4",]$gwp 
    
    #check
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
                data_scen$scenario=="d_delfrag" & data_scen$year %in% seq(2020,2050,5),]
    data_scen[data_scen$region=="Indonesia" & data_scen$variable %in% c("Emissions|CH4") & 
                data_scen$scenario=="o_2c" & data_scen$year %in% seq(2020,2050,5),]
  }
  
  #rename adjusted scenarios:
  data_scen <- data_scen |> mutate(scenario = case_when(
    region=="Indonesia" & scenario == "o_2c" ~ "o_2c_adj",
    region=="Indonesia" & scenario == "d_delfrag" ~ "d_delfrag_adj",
    .default = scenario
  ))
  
  
  ###### Adjust Electricity Generation for Indonesia ----
  ind_power <- read_xlsx(file.path(dir_runs, 'CIPP AND PLEXOS GEN.xlsx')) |>
    pivot_longer(cols=seq(6,11),names_to = "year") |> rename_with(tolower) |>
    mutate(year=as.numeric(year),value=value/ej2twh,unit="EJ/yr") |> select (-technology) |>
    rename(variable='variable gcam')|>
    mutate(model="GCAM 6.0 NGFS", scenario=case_when(
      scenario=="1P5_Flexibility" ~ 'o_2c_adj',
      scenario=="LOW" ~ 'd_delfrag_adj',
      .default=scenario),region="Indonesia")
  
  #overwrite scenario data, and add country target data
  data_scen <- rbind ( overwrite(ind_power|>filter(scenario != "CIPP"),data_scen),
                       ind_power|>filter(scenario=="CIPP"))
  
  data_kor <- read_xlsx(file.path('data','KOR_BEP11.xlsx'),sheet = "Sheet2",col_names = T)|>
    pivot_longer(cols =seq(2,7),names_to = "variable") |>rename(year=`0`)|>
    mutate(model="BEP11",scenario="BEP11",region="South Korea",unit="EJ",value=value/ej2twh)
  
  data_scen <- rbind(data_scen,data_kor)
  
  ##### Brazil ----
  #costum historical data for Brazil
  # add historical emissions data for Brazil, both from offical ministry data (-2020)
  # https://www.gov.br/mcti/pt-br/acompanhe-o-mcti/sirene/emissoes/emissoes-de-gee-por-setor-1, and from
  # independent SEEG consortium (-2022) https://seeg.eco.br/
  hist_bra <- rbind(read_xlsx(file.path(dir_data, "Brazil_emissions.xlsx"),sheet = "MCTI") |>
                      rename(year=Ano,value=Valores) |> mutate(value=value/1000,sector=case_when(
                        Setor == "1. Energia" ~ "Energy",
                        Setor == "2. Processos industriais e uso de produtos (IPPU)" ~ "Industrial Processes",
                        Setor == "3. Agropecuária" ~ "Agriculture",
                        Setor == "4. Uso da Terra, Mudança do Uso da Terra e Florestas (LULUCF)" ~ "AFOLU",
                        Setor == "5. Resíduos" ~ "Waste")) |> select(-Setor) |> pivot_wider(names_from = sector) |>
                      mutate(model="MICT",  `Emissions|Kyoto Gases (excl. LUC)`=`Energy`+`Industrial Processes`+`Agriculture`+`Waste`,
                             `Emissions|Kyoto Gases`=`Emissions|Kyoto Gases (excl. LUC)`+`AFOLU`) |> pivot_longer(cols = !c(year,model),names_to = "variable"),
                    read_xlsx(file.path(dir_data, "Brazil_emissions.xlsx"),sheet="SEEG") |> rename(year=Categoria,Agriculture=Agropecuária,
                                                                                   Energy=Energia,`Industrial Processes`=`Processos Industriais`,Waste=Resíduos,AFOLU=`Mudança de Uso da Terra e Floresta`,
                                                                                   `Emissions|Kyoto Gases`=Total) |> mutate(model="SEEG", `Emissions|Kyoto Gases (excl. LUC)`=`Emissions|Kyoto Gases`-`AFOLU`)|>
                      pivot_longer(cols = !c(year,model),names_to = "variable") |> mutate(value=value/1e6)) |> mutate(region="BRA",scenario="historical",unit="Mt CO2eq/yr") |>
    #combine data from MICT and SEEG for plotting in emissions graph
    filter(variable %in% c("Emissions|Kyoto Gases","Emissions|Kyoto Gases (excl. LUC)"),(model=="MICT" | (model=="SEEG" & year>2020)))
  
  
  data_hist <- rbind(data_hist,hist_bra)
  hist_bra <- hist_bra |>filter(variable=="Emissions|Kyoto Gases") |> mutate(variable="Emissions|Kyoto Gases (incl. all LULUCF)")
  data_hist <- rbind(data_hist,hist_bra)
  
  #### Japan ----
  # add bottom-up power sector results
  jpn_bu <- read_xlsx(file.path(dir_runs, "Japan_power_sector_BU.xlsx"),sheet="GCAM FINAL",range = "F70:Z92") |>
    rename(variable=`...1`) |>select(-Unit) |> pivot_longer(cols = -variable, names_to = "year") |>
    mutate(year=as.numeric(substr(year,start = 1,stop=4)))|> filter(year %in% seq(2020,2035,5))|>
    mutate(value=value/1000/ej2twh,unit="EJ/yr",model="Bottom up",scenario="Very high ambition",region="Japan")
  data_scen <- rbind(data_scen,jpn_bu)
  
  
  
  #### Philippenes -----
  # add Philippines Energy Plan power sector results
  num_dat <-18
  phl_ep <- rbind(data.frame(scenario=rep("CES-1",num_dat),
                             variable=(rep(paste0("Secondary Energy|Electricity|",c("Coal","Gas","Oil","Nuclear","Geothermal","Hydro","Wind","Solar","Biomass")),2)),
                             year=c(rep(2030,num_dat/2),rep(2040,num_dat/2)),value=c(64.08,33.90,0.88,0,15.35,12.82,23.94,15.08,2.81,
                                                                                     58.45,41.59,0.88,19.36,18.40,25.22,56.32,56.52,2.45)),
                  data.frame(scenario=rep("CES-2",num_dat),
                             variable=(rep(paste0("Secondary Energy|Electricity|",c("Coal","Gas","Oil","Nuclear","Geothermal","Hydro","Wind","Solar","Biomass")),2)),
                             year=c(rep(2030,num_dat/2),rep(2040,num_dat/2)),value=c(52.65,30.77,0.88,0,15.31,12.77,36.65,16.14,2.81,
                                                                                     51.96,36.74,0.89,19.36,15.77,23.14,95.33,33.15,2.45)))|>
    group_by(scenario,variable)|>
    complete(year=2030:2040)|>
    mutate(value=na.approx(value)) |>filter(year %in% seq(2030,2040,5)) |> ungroup() |>
    mutate(model="PHL Energy Plan",unit="EJ/yr",region="Philippines",value=value/ej2twh)
  
  data_scen <- rbind(data_scen,phl_ep)
  
  
  # adjust Philippines Emissions to account for oil mismatch _______________
  # Downscaling[GCAM 6.0 NGFS] o_1p5c   Philippines Primary Energy|Oil EJ/yr  2020 0.689
  # Downscaling[GCAM 6.0 NGFS] o_1p5c   Philippines Primary Energy|Oil EJ/yr  2025 2.04 
  # 3 PHL    EJ    Primary Energy|Oil Stat. Rev. World Energy Data historical  2020 0.755
  # 4 PHL    EJ    Primary Energy|Oil Stat. Rev. World Energy Data historical  2021 0.813
  # 5 PHL    EJ    Primary Energy|Oil Stat. Rev. World Energy Data historical  2022 0.892  
  # 3 PHL    Mt CO2 Emissions|CO2|Oil OWID  historical  2020  46.3
  # 4 PHL    Mt CO2 Emissions|CO2|Oil OWID  historical  2021  50.0
  # 5 PHL    Mt CO2 Emissions|CO2|Oil OWID  historical  2022  53.4
  offset <- 1.4 * (53.4/0.892+50.0/0.813+46.3/0.755)/3
  emi_vars <- c("Emissions|Kyoto Gases","Emissions|Kyoto Gases (incl. all LULUCF)","Emissions|Kyoto Gases (excl. LUC)",
                "Emissions|CO2","Emissions|CO2 (incl. all LULUCF)","Emissions|CO2|Energy","Emissions|CO2|Energy and Industrial Processes")
  offset_scen_phl <- data_scen|> filter(region=="Philippines",year>2024,variable=="Primary Energy|Oil")|>
    mutate( value_off=value/2.04*offset) |> select (-variable,-unit,-value)
  #correct all top level emission values with the time-varying offset
  corr_scen_phl <- data_scen |> filter(region=="Philippines",variable %in% emi_vars) |>left_join(offset_scen_phl)
  corr_scen_phl[is.na(corr_scen_phl$value_off),]$value_off <- 0
  corr_scen_phl <- corr_scen_phl |> mutate(value=value-value_off) |> select(-value_off)
  #put full data set together
  data_scen <- rbind(data_scen|>filter(region!="Philippines"),
                     data_scen|>filter(region=="Philippines",!variable %in% emi_vars),
                     corr_scen_phl)
  ####  Nigeria  -----
  # adjust Nigeria emissions based on bottom up non-CO2 assumptions
  #values from cells I39:O39 https://docs.google.com/spreadsheets/d/1Tg6FDhtNwzEhU3JZPQ3rsGmsvTfTZZPU/edit?gid=1610044659#gid=1610044659
  data_scen[data_scen$region=="Nigeria" & data_scen$variable=="Emissions|Kyoto Gases (excl. LUC)" & data_scen$scenario=="o_1p5c",]$value <-
    c(366,356,325,300,273,241,210)
  #I48:O48
  data_scen[data_scen$region=="Nigeria" & data_scen$variable=="Emissions|Kyoto Gases (excl. LUC)" & data_scen$scenario=="d_delfrag",]$value <-
    c(366,373,388,398,421,437,444)
  #rename adjusted scenarios:
  data_scen <- data_scen |> mutate(scenario = case_when(
    region=="Nigeria" & scenario == "o_1p5c" ~ "o_1p5c_adj",
    region=="Nigeria" & scenario == "d_delfrag" ~ "d_delfrag_adj",
    .default = scenario
  ))
}

###Write out data for IND, IDN, 1p5C, CurPol, and adjusted 2C, only variable subset

data_wo <- rbind(data_scen |> filter(region=="India",scenario%in% c("h_cpol","o_1p5c")),
                 data_scen |> filter(region=="Indonesia",scenario%in% c("h_cpol","o_1p5c")),
                 data_scen |> filter(region=="Indonesia",scenario%in% c("o_2c"),
                                     variable%in% c(unique(ind_power$variable),"Emissions|CO2",
                                                    "Emissions|Kyoto Gases",
                                                    "Emissions|CH4",
                                                    "Emissions|N2O",
                                                    "Emissions|F-Gases",
                                                    "Emissions|CO2|AFOLU",
                                                    "Emissions|CO2|Energy|Supply|Electricity"
                                     )))  


ggplot()+
  geom_line(data = data_wo|>filter(variable=="Emissions|F-Gases"),aes(x=year,y=value,color=scenario,linetype=region))
# write.csv(data_wo |> select(model,scenario,region,variable,unit,year,value) |> pivot_wider(names_from = year),
#           file = "output/India_Indonesia_scenario.csv",quote = F,row.names = F)


#### 3. Define countries of interest -----
# differentiated by priority

#communication June 10 - 
#first column: name
#second column: Iso code
#third column: priority group
#4th-6th column: scenario names to use: 4th is first (central), 5th shifted to left, 6th to right
#if there are 2 reference scenarios, these can be 4th and 5th or 5th and 6th 
#scen1 and 2 can be either from GCAM or other, while scen3 should only be non-GCAM scenario (or NA)
#7th to 9th column: scenario display names
#10th column: Emission metric to use: 1 = "Kyoto Gases (incl. all LULUCF)", 2 = "Kyoto Gases", 3 = "Kyoto Gases (excl. LUC)", 4 = "CO2", 5 = "CO2|Energy and Industrial Processes"
#the difference between 1 and 2 is that 1 includes indirect LULUCF CO2 sinks (closest to country inventory data), while 2 is the IAM convention
countries <- matrix(ncol=9,byrow = T,data=c(
  "Brazil","BRA",1,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "China","CHN",1,"Accelerated_netzero","Cpol_netzero",NA,"High Ambition","Low Ambition",NA, 
  "Indonesia","IDN",1,"o_2c_adj","d_delfrag_adj","CIPP","High Ambition","Low Ambition","CIPP",
  "India","IND",1,"India_High_Amb","d_delfrag","o_2c","High Ambition","Low Ambition","Very High Amb.",
  "Japan","JPN",1,"o_1p5c","d_delfrag","Very high ambition","High Ambition","Low Ambition","Very High Ambition",
  "Mexico","MEX",1,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "South Korea","KOR",1,"o_1p5c","d_delfrag","BEP11","High Ambition","Low Ambition","11th BEP",
  "Colombia","COL",2,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Canada","CAN",2,"Netzero_SSP2","CurPol_SSP2",NA,"High Ambition","Low Ambition",NA,
  "Argentina","ARG",2,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Nigeria","NGA",2,"o_1p5c_adj","d_delfrag_adj",NA,"High Ambition","Low Ambition",NA,
  "Philippines","PHL",2,"CES-1","o_2c","CES-2","CES-1","High Ambition","CES-2",
  "Thailand","THA",2,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Turkey","TUR",2,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Vietnam","VNM",2,"o_2c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "South Africa","ZAF",2,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Australia","AUS",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Congo","DRC",3,NA,NA,NA,NA,NA,NA,
  "United Kingdom","GBR",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "European Union","EUR",3,NA,NA,NA,NA,NA,NA,
  "EU27BX","EU27BX",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Croatia","HRV",3,NA,NA,NA,NA,NA,NA,
  "Egypt","EGY",3,NA,NA,NA,NA,NA,NA,
  "Kenya","KEN",3,NA,NA,NA,NA,NA,NA,
  "Malaysia","MYS",3,NA,NA,NA,NA,NA,NA,
  "Pakistan","PAK",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Saudi Arabia","SAU",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Algeria","DZA",3,NA,NA,NA,NA,NA,NA,
  "Azerbaijan","AZE",3,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "Kazakhstan","KAZ",3,NA,NA,NA,NA,NA,NA,
  "Norway","NOR",3,NA,NA,NA,NA,NA,NA,
  "Russia","RUS",4,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
  "USA","USA",4,"o_1p5c","d_delfrag",NA,"High Ambition","Low Ambition",NA,
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


if(T){
  source("src/overview_analysis.R")
}


  
  #to use only selection, in-comment appropriate line below
# countries <- countries |> filter(iso=="CAN")
# countries <- countries |> filter(iso %in% c("NGA","PHL","THA"))
  # countries <- countries |> filter(iso %in% c("ARG","SAU","TUR","RUS","MEX"))
  countries <- countries |> filter(iso %in% c("AUS","BRA","CAN","CHN",
                                              "EU27BX","IND","IDN","JPN","KOR",
                                              "MEX","ZAF","GBR","USA",
                                              "ARG","SAU","TUR","RUS","World"))
# countries <- countries |> filter(iso %in% c("JPN","KOR"))
# countries <- countries |> filter(priority==1)
# countries <- countries |> filter(!is.na(scen1))  # Countries with scenarios
  
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