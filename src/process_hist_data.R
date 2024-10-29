
#script to provide a set of historical data sources.
#1. each source is first read in and brought into a clean, but source-specific data frame structure,
#with a source identifying name (ceds, prim, ener, ...)
#2. these are then brought to harmonized IAMC template form and variable name (dat_ener, dat_prim, ...)
#3. and 4. these are written out both on iso and GCAM 32 region level.
#5. a template for doing plots with scenario data is at the end.

### todos:
# add more emissions detail? SO2, subsectors? (from CEDS, PRIMAP-hist)
# add 2023 estimates for capacity additions from BNEF, etc.
# add forecast projections for solar capacity additions from BNEF


### Load Libraries and constants -----
library(tidyverse)
library(dplyr)
library(readxl)
library(quitte) # Download from https://pik-piam.r-universe.dev/quitte#
library(readxl)
library(countrycode)

#source functions and constants
source("src/functions.R")


# Start year for harmonized datasets
starty <- 1990 - 1 # can be adjusted for even shorter or longer historic time series in IAMC format

# Choose the desired regions for the historical data to be aggregated up to, in 
# addition to by country: Model name / number of regions
# model_regions <- "gcam32"   # Using GCAM core regions
model_regions <- "r10"
# model_regions <- "r5"

# Change save_option to False to skip re-saving the raw data as .Rds files
save_option <- T 

### Process the data -----

# CHECK if the historical data processed outputs (from the hist_iamc repo) have 
# been updated at this location: output_both_repos
# https://drive.google.com/drive/u/0/folders/1TVv7d-kgyqqhK3ojneWNsZjW01YTxEVp

# IF SO, put the generated datasets (Rds files with source specific, more detailed data, 
# and csv files with harmonized set of IAMC variables) into the output/ folder 
# within this local repo

#### 1. read in __________________________ ####### 

###### emissions: ghg - PRIMAP hist --------------------------------------------------
# https://zenodo.org/records/13752654  
# - Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_no_rounding_13-Sep-2024
prim <- read.csv("data/Guetschow_et_al_2024a-PRIMAP-hist_v2.6_final_no_rounding_13-Sep-2024.csv")
unique(prim$entity)
unique(prim$area..ISO3.)
prim <- prim |> pivot_longer(cols = c(-source,-scenario..PRIMAP.hist.,-provenance,-area..ISO3.,-entity,-unit,-category..IPCC2006_PRIMAP.),names_to = "year")
prim$year <- substr(prim$year,2,5)

prim <- prim |> rename(region=area..ISO3.)
prim$year <- parse_number(prim$year)



###### emissions: co2 - CEDS --------------------------------------------------
# https://zenodo.org/records/10904361
# CEDS v_2024_04_01 Release Emission Data
ceds <- rbind(read.csv("data/CEDS_v_2024_04_01_aggregate/CH4_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,56)),
              read.csv("data/CEDS_v_2024_04_01_aggregate//CO2_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/N2O_CEDS_emissions_by_country_v2024_04_01.csv")|>
               pivot_longer(cols = seq(4,56)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/SO2_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/BC_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/CO_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/NH3_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/NMVOC_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/NOx_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)),
              read.csv("data/CEDS_v_2024_04_01_aggregate/OC_CEDS_emissions_by_country_v2024_04_01.csv")|>
                pivot_longer(cols = seq(4,276)))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))

#sectoral ceds data (so far only for CO2, but could be expanded to more gases, at least for CH4 could be interesting):
ceds_s <-read.csv("data/CEDS_v_2024_04_01_aggregate/CO2_CEDS_emissions_by_country_sector_v2024_04_01.csv") |>
  pivot_longer(cols=seq(5,277))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5))) %>%
  filter(year > starty)

ceds_m <-read.csv("data/CEDS_v_2024_04_01_aggregate/CH4_CEDS_emissions_by_country_sector_v2024_04_01.csv") |>
  pivot_longer(cols=seq(5,57))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))  %>%
  filter(year > starty)

ceds_n <-read.csv("data/CEDS_v_2024_04_01_aggregate/N2O_CEDS_emissions_by_country_sector_v2024_04_01.csv") |>
  pivot_longer(cols=seq(5,57))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))%>%
  filter(year > starty)

#compile reference historic IAMC dataset
# data <- prim |> reference



###### emissions: co2 - OWID ---------
#https://github.com/owid/co2-data?tab=readme-ov-file
owid_co2_data <- read_csv("data/owid-co2-data.csv")

# Pivot the CO2 dataset
owid_co2_data <- owid_co2_data %>%
  pivot_longer(
    cols = population:last_col(), 
    names_to = "variable", 
    values_to = "value"
  )





###### emissions: co2 luc - GCB ---------
# Global Carbon Budget - National Land Use Change Carbon Emissions
# https://www.icos-cp.eu/science-and-impact/global-carbon-budget/2023 
# use data from xlsx, just saving the respective sheets into following csv:
# National_LandUseChange_Carbon_Emissions_2023v1.0_BLUE.csv
# National_LandUseChange_Carbon_Emissions_2023v1.0_H&C.csv
# National_LandUseChange_Carbon_Emissions_2023v1.0_OSCAR.csv

#get header info
header_data <- read.csv("data/National_LandUseChange_Carbon_Emissions_2023v1.0_BLUE.csv", skip = 7, nrows = 1, header = FALSE)
header_data <- header_data[, 1:202]
column_names <- as.character(header_data[1, ])
#change country names with special characters
column_names[1] <- "year"
column_names[85] <- "Cote d'Ivoire"
column_names[181] <- "Turkiye"

#reading in data from three different models
blue <- read.csv("data/National_LandUseChange_Carbon_Emissions_2023v1.0_BLUE.csv", skip = 9, header = FALSE)
names(blue) <- column_names
blue <- blue[, 1:202]

oscar <-read.csv("data/National_LandUseChange_Carbon_Emissions_2023v1.0_H&C.csv", skip = 9, header = FALSE)
names(oscar) <- column_names
oscar <- oscar[, 1:202]

hc <- read.csv("data/National_LandUseChange_Carbon_Emissions_2023v1.0_OSCAR.csv", skip = 9, header = FALSE)
names(hc) <- column_names
hc <- hc[, 1:202]

#filter out unwanted years and columns
blue<- blue %>%
  filter(year > starty) %>% select(-c(DISPUTED, OTHER))

oscar<- oscar %>%
  filter(year > starty) %>% select(-c(DISPUTED, OTHER))

hc<- hc %>%
  filter(year > starty) %>% select(-c(DISPUTED, OTHER))

blue <- blue %>%
  pivot_longer(cols = -year, names_to = "Country", values_to = "value") %>% 
  mutate(value = value * 3.664, model = "BLUE") #convert to million tonnes of CO2

hc <- hc %>%
  pivot_longer(cols = -year, names_to = "Country", values_to = "value") %>% 
  mutate(value = value * 3.664, model = "H&C") #convert to million tonnes of CO2

oscar <- oscar %>%
  pivot_longer(cols = -year, names_to = "Country", values_to = "value") %>% 
  mutate(value = value * 3.664, model = "OSCAR") #convert to million tonnes of CO2

#combine all df into one
land <- rbind(blue, oscar, hc)
land<- arrange(land, year, Country)



###### emissions: ch4 - IEA ---------
##### https://www.iea.org/data-and-statistics/data-tools/methane-tracker
file_path <- "data/IEA-MethaneEmissionsComparison-World.csv"
iea_ch4 <- read.csv(file_path)

iea_ch4 <- iea_ch4 %>%
  filter(segment == "Total") %>%
  select(-segment, -reason, -emissionsRank, -energyRank, -notes)

iea_ch4 <- iea_ch4 %>%
  rename(value = "emissions..kt.")

iea_ch4 <- iea_ch4 %>%
  mutate(baseYear = ifelse(baseYear == "2019-2021", "2021", baseYear)) %>%
  rename(year = baseYear)


###### emissions: ch4 - CLIMATE TRACE ---------
ct_waste <- rbind(waste1 <- read.csv("data/solid-waste-disposal_country_emissions.csv"),
                  waste2 <-read.csv("data/wastewater-treatment-and-discharge_country_emissions.csv"),
                  waste3 <- read.csv("data/incineration-and-open-burning-of-waste_country_emissions.csv"),
                  waste4 <- read.csv("data/biological-treatment-of-solid-waste-and-biogenic_country_emissions.csv"))

ct_waste <- ct_waste %>%
  rename(iso=iso3_country) %>% mutate(year=parse_number(substr(start_time,0,4))) %>%
  select(-c(sector, temporal_granularity, created_date, modified_date, start_time, end_time)) %>%
  filter(gas == "ch4") %>% na.omit()



###### energy: elec - EMBER --------------------------------------------------
# https://ember-climate.org/data-catalogue/monthly-electricity-data/
# - monthly_full_release_long_format-4.csv
# - yearly_full_release_long_format.csv

ember = read.csv("data/yearly_full_release_long_format_05_2024.csv")
emberm = read.csv("data/monthly_full_release_long_format.csv")

#create output in convenient cap (capacity), geny and genm (generation) formats (to be used in VRE-Eff script)
ecap <- ember |> mutate(Country.code=case_when(
  Area== "World" ~ "World",
  .default=Country.code
),Area.type=case_when(
  Area== "World" ~ "Country",
  .default=Area.type  
)) |>
  filter(Area.type == "Country",
         Category == "Capacity",
         # Subcategory == "Fuel",
         Unit == "GW") |>
  select(Area,Country.code,Year,Category,Variable,Value,Unit) |>
  rename(region=Area,iso=Country.code,year=Year,variable =Category,
         fuel=Variable,value=Value,unit=Unit)|>
  pivot_wider(names_from = fuel)|>mutate(Total=Clean+Fossil) |>
  pivot_longer(cols=seq(6,21),names_to = "fuel")

edem <- ember |> mutate(Country.code=case_when(
  Area== "World" ~ "World",
  .default=Country.code
),Area.type=case_when(
  Area== "World" ~ "Country",
  .default=Area.type  
)) |>
  filter(Area.type == "Country",
         Category == "Electricity demand",
         Subcategory == "Demand") |>
  select(Area,Country.code,Year,Category,Variable,Value,Unit) |>
  rename(region=Area,iso=Country.code,year=Year,variable =Category,
         fuel=Variable,value=Value,unit=Unit)


egeny <- ember |>  mutate(Country.code=case_when(
  Area== "World" ~ "World",
  .default=Country.code
),Area.type=case_when(
  Area== "World" ~ "Country",
  .default=Area.type  
)) |>
  filter(Area.type == "Country",
         Category == "Electricity generation",
         # Subcategory == "Fuel",
         Unit == "TWh") |>
  select(Area,Country.code,Year,Category,Variable,Value,Unit) |>
  rename(region=Area,iso=Country.code,year=Year,variable =Category,
         fuel=Variable,value=Value,unit=Unit)|> 
  mutate(fuel=case_when(
    fuel=="Total Generation" ~ "Total",
    .default=fuel
  ))

egenm <- emberm |>  mutate(Country.code=case_when(
  Area== "World" ~ "World",
  .default=Country.code
),Area.type=case_when(
  Area== "World" ~ "Country",
  .default=Area.type  
)) |>
  filter(Area.type == "Country",
         Category == "Electricity generation",
         # Subcategory == "Fuel",
         Unit == "TWh") |>
  select(Area,Country.code,Date,Category,Variable,Value,Unit) |>
  rename(region=Area,iso=Country.code,year=Date,variable =Category,
         fuel=Variable,value=Value,unit=Unit) |> 
  #additional adjustment of Date for easier plotting and calculations
  mutate(year=as.double(gsub("-",".",substr(as.Date.character(year),1,7))))|>
  mutate(fuel=case_when(
    fuel=="Total Generation" ~ "Total",
    .default=fuel
  ))

 ##### fill in yearly data for 2023 where available


#fill in december 2023 for datasets that do have all month until nov 2023
switch_option <- FALSE  # Set switch_option to TRUE or FALSE as needed
if (switch_option) {
    for(rg in unique(egenm$region)){
    if(max(egenm[egenm$region==rg,]$year)==2023.11){
      rg
      egenm<- rbind(egenm,egenm|>filter(region==rg,year==2023.11)|>mutate(year=2023.12))
      for(fu in unique(egenm[egenm$region==rg & egenm$year==2023.12,]$fuel)){
        #calculate 23/12 as product of 22/12 and average 9-11 multiplier from 22 to 23 for the particular fuel (will not be 100% consistent for summation, but good enough)
        egenm[egenm$region==rg & egenm$year==2023.12 & egenm$fuel==fu,]$value <- egenm[egenm$region==rg & egenm$year==2022.12 & egenm$fuel==fu,]$value *
          (egenm[egenm$region==rg & egenm$year==2023.09 & egenm$fuel==fu,]$value+egenm[egenm$region==rg & egenm$year==2023.10 & egenm$fuel==fu,]$value+egenm[egenm$region==rg & egenm$year==2023.11 & egenm$fuel==fu,]$value)/
          (egenm[egenm$region==rg & egenm$year==2022.09 & egenm$fuel==fu,]$value+egenm[egenm$region==rg & egenm$year==2022.10 & egenm$fuel==fu,]$value+egenm[egenm$region==rg & egenm$year==2022.11 & egenm$fuel==fu,]$value)
        }
      }
    }
  }
#add 2023 year based on monthly data for all countries with complete or near-complete monthly data

switch_option <- FALSE  # Set switch_option to TRUE or FALSE as needed
if (switch_option) {
  for(rg in unique(egenm$region)){
  if(max(egeny[egeny$region==rg,]$year) ==2022 & max(egenm[egenm$region==rg,]$year)>2023.11){
    egeny<- rbind(egeny,egeny|>filter(year==2022,region==rg)|>mutate(year=2023))
    for(fu in unique(egeny[egeny$region==rg & egeny$year==2023,]$fuel)){
      #calculate 2023 as 2022 yearly data times sum of monthly 2023 divided by sum of monthly 2022
      egeny[egeny$region==rg & egeny$year==2023 & egeny$fuel==fu,]$value <- egeny[egeny$region==rg & egeny$year==2022 & egeny$fuel==fu,]$value *
        sum(egenm[egenm$region==rg & egenm$year %in% c(2023.01,2023.02,2023.03,2023.04,2023.05,2023.06,2023.07,2023.08,2023.09,2023.10,2023.11,2023.12) & egenm$fuel==fu,]$value)/
        sum(egenm[egenm$region==rg & egenm$year %in% c(2022.01,2022.02,2022.03,2022.04,2022.05,2022.06,2022.07,2022.08,2022.09,2022.10,2022.11,2022.12) & egenm$fuel==fu,]$value)
      }
    }
  }
}
 eemi <- ember |> mutate(Country.code=case_when(
   Area== "World" ~ "World",
   .default=Country.code
 ),Area.type=case_when(
   Area== "World" ~ "Country",
   .default=Area.type  
 )) |>
  filter(Area.type == "Country",
         Category == "Power sector emissions") |>
  select(Area,Country.code,Year,Category,Variable,Value,Unit) |>
  rename(region=Area,iso=Country.code,year=Year,variable =Category,
         fuel=Variable,value=Value,unit=Unit)|> 
  mutate(fuel=case_when(
    fuel=="Total emissions" ~ "Total",
    .default=fuel
  ))|>filter(fuel %in% c("Total","Gas","Coal","Fossil"))

#EU total emissions
eemi_eu <- ember |> 
  filter(Area == "EU",
         Category == "Power sector emissions") |>
  select(Year,Category,Variable,Value,Unit) |>
  rename(year=Year,variable =Category,
         fuel=Variable,value=Value,unit=Unit)|> 
  mutate(region="EU27",iso="EU27BX",fuel=case_when(
    fuel=="Total emissions" ~ "Total",
    .default=fuel
  ))|>filter(fuel %in% c("Total","Gas","Coal","Fossil"))

if(save_option == T){
  save(ecap,egeny,egenm,eemi,file="output/ember.Rds")
}


###### energy: other - EI SRWED --------------------------------------------------
# Energy Institute's Statistical Review of World Energy Data
# https://www.energyinst.org/statistical-review/resources-and-data-downloads
# This link corresponds to multiple reports from the Energy Institute. Our analysis draws two files 
# - Key reports - Statistical Review of World Energy Data.XLSX
# Consolidated Dataset Narrow format downloaded as - Statistical Review of World Energy Narrow File.csv
# 2024-06-20 version: includes 2023 data points, files have _06_24 suffix
ener <- read.csv("data/Statistical Review of World Energy Narrow File_06_24.csv") |>
  rename(region=Country,year=Year,iso=ISO3166_alpha3,value=Value) |>
  select(region,year,iso,Var,value)

#check USA implied net oil trade to compare with sheet "Oil - Trade movements" to 
# see which GJ per barrel value leads to reproduction of pattern (net exporter in 2020 and 2022)
ener|>filter(iso=="USA",Var %in% c("oilcons_ej","oilprod_kbd"))|>pivot_wider(names_from = Var)|>
  mutate(exp=oilprod_kbd*5.8*365/1000000-oilcons_ej) |> filter(year>2010)
# implies that 5.8 GJ per barrel of oil is a good value. On the other hand, file:///C:/Users/bertram/Downloads/Approximate%20conversion%20factors%20-%20tables.pdf
# says that 1 boe is 6.118 GJ, so using an average value of 6 on average seems good approach
# difference probably due to accounting for products vs. crude (see tables in file.)

#check gas trade: this works well, see sheet "Gas - Inter-regional trade"
ener|>filter(iso=="USA",Var %in% c("gascons_ej","gasprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=gasprod_ej-gascons_ej) |> filter(year>2009)

#would make sense to also at some point use the LNG import and export data:
lng <- rbind(read_xlsx(path = "data/Statistical Review of World Energy Data_06_24.xlsx",sheet="Gas - LNG imports bcm",range = "A3:Y38")|>
               mutate(var="lng_imp_ej"),
             read_xlsx(path = "data/Statistical Review of World Energy Data_06_24.xlsx",sheet="Gas - LNG exports bcm",range = "A3:Y31")|>
               mutate(var="lng_exp_ej")) |>
  pivot_longer(cols = seq(2,24),names_to = "year") |>
  mutate(region=`Billion cubic metres`,value=value/bcm2ej)

#check coal trade: this works ok, see sheet "Coal - Trade movements"
ener|>filter(iso=="USA",Var %in% c("coalcons_ej","coalprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=coalprod_ej-coalcons_ej) |> filter(year>2009)

#check coal trade: this works ok, see sheet "Coal - Trade movements"
ener|>filter(iso=="IND",Var %in% c("coalcons_ej","coalprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=coalprod_ej-coalcons_ej) |> filter(year>2009)

###### energy: IEA WEO 2023 --------------------------------------------------
# https://www.iea.org/ - World energy Outloook 2023 Dataset 
# https://www.iea.org/data-and-statistics/data-product/world-energy-outlook-2023-free-dataset-2#data-files
# The above link corresponds to two datasets 'Region' and 'World' which will be used for our analysis
# Region - WEO2023_AnnexA_Free_Dataset_Regions.csv
# World - WEO2023_AnnexA_Free_Dataset_World.csv
iea23 <- read.csv("data/WEO2023_AnnexA_Free_Dataset_Regions.csv")|>
  mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW))
iea23 <- rbind(iea23, ## add all available data on Net Zero scenarios, plus additional variables for others
               read.csv("data/WEO2023_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO=="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW))|>select(-X),
               read.csv("data/WEO2023_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO!="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW)) |> 
                 filter(!var %in% unique(iea23$var))|>select(-X)
)|>
  select(-CATEGORY,-PRODUCT,-FLOW,-PUBLICATION)|>
  rename(unit=UNIT,region=REGION,year=YEAR,value=VALUE,scenario=SCENARIO)



###### energy: OWID ---------
# https://github.com/owid/energy-data?tab=readme-ov-file
owid_energy_data <- read_csv("data/owid-energy-data.csv")

# Pivot the CO2 dataset
owid_energy_data <- owid_energy_data %>%
  pivot_longer(
    cols = population:last_col(), 
    names_to = "variable", 
    values_to = "value"
  )


###### trn: ev - IEA GEVO --------------------------------------------------
#https://www.iea.org/data-and-statistics/data-product/global-ev-outlook-2024
iea_ev <- read.csv("data/IEA Global EV Data 2024.csv")


###### climate: temp - NASA --------------------------------------------------
#https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv 
nasa_temp <- read.csv("data/GLB.Ts+dSST.csv",skip = 1)

###### climate: temp - CRU --------------------------------------------------
#https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT5.0Analysis_gl.txt
# Read the data from the file
file_path <- "data/HadCRUT5.0Analysis_gl.txt"

# Read the data from the file
data <- read_lines(file_path)

# Initialize an empty dataframe
df <- data.frame(Year = integer(), Mean_Temperature = double(), stringsAsFactors = FALSE)

# Process the data
for (i in seq(1, length(data), by = 2)) {
  # Split the lines into components
  temp_data <- str_split(data[i], "\\s+")[[1]]
  
  # Extract the year and mean temperature
  year <- as.integer(temp_data[1])
  mean_temp <- as.numeric(temp_data[14])
  
  # Append to the dataframe
  crut <- rbind(df, data.frame(Year = year, Mean_Temperature = mean_temp))
}



###### socio: gdp and population - IIASA ---------
# https://data.ece.iiasa.ac.at/ssp/#/downloads
file_path <- "data/1710759470883-ssp_basic_drivers_release_3.0.1_full.xlsx"

# Read the data sheet
iiasa_data <- read_excel(file_path, sheet = "data")


iiasa_data <- iiasa_data %>%
  filter(Variable %in% c("GDP|PPP", "Population")) %>%
  pivot_longer(cols = starts_with("19") | starts_with("20") | starts_with("21"), names_to = "year", values_to = "value") %>%
  drop_na(value)

iiasa_data <- iiasa_data %>%
  filter(!(grepl("R9", Region) | grepl("R10", Region) 
                         | grepl("R5", Region)))


# Convert all column names to lowercase
colnames(iiasa_data) <- tolower(colnames(iiasa_data))

# Apply the function to the 'region' column
iiasa_data$region <- sapply(iiasa_data$region, remove_brackets)

# Add an ISO column using the 'countrycode' function
iiasa_data$iso <- countrycode(iiasa_data$region, 'country.name', 'iso3c')
iiasa_data <- iiasa_data |>
  mutate(iso = ifelse(region == "World", "World", iso))

# Select specific columns
iiasa_data <- iiasa_data %>%
  select(iso, variable, unit, year, value, model, scenario)

#change scenario name for historical data
iiasa_data <- iiasa_data |> mutate(scenario=case_when(
  scenario == "Historical Reference" ~ "historical",
  .default = scenario
))

#remove NA values, start at year 2000
iiasa_data <- iiasa_data %>%
  filter(!is.na(iso) & year > starty)

# View the updated data
head(iiasa_data)


#### 2.a Convert to IAMC variables _____________ ##########

##For our analysis, we require certain variables to be mapped according to the documentation of IAMC.
##The mapping files are created using the documentation which can be found on the following link:
##-- https://data.ene.iiasa.ac.at/ar6/#/docs


###### PRIMAP hist -------------------------------------
dat_prim <- prim |> filter(entity %in% c("KYOTOGHG (AR4GWP100)","KYOTOGHG (AR5GWP100)","CO2","CH4","N2O","FGASES (AR4GWP100)","FGASES (AR5GWP100)"),
                           year > starty,category..IPCC2006_PRIMAP. %in% c("0", "M.0.EL","M.LULUCF"),
                           scenario..PRIMAP.hist. %in% c("HISTCR","HISTTP")) |> select(-provenance,-source) |>
  mutate(unit = case_when(
    entity=="CH4" ~ "Mt CH4",
    entity=="N2O" ~ "kt N2O",
    entity=="CO2" ~ "Mt CO2",
    .default="Mt CO2eq"
  )) |>
  mutate(value = case_when(
    entity=="N2O" ~ value,
    .default=value/1000
  ))|>
  mutate(entity = case_when(
    #define emission categories: most from HISTCR, but for AFOLu, also use IAM default version without indirect LULUCF (HISTTP)
    #afterwards (see rbind below), total CO2 and total Kyoto also get calculated with the HISTTP LULUCF to get to a consistent set with / without indirect LULUCF (Grassi effect)
    entity=="KYOTOGHG (AR4GWP100)" & category..IPCC2006_PRIMAP.=="0" & scenario..PRIMAP.hist. =="HISTCR" ~ "Emissions|Kyoto Gases (incl. all LULUCF)",
    entity=="KYOTOGHG (AR4GWP100)" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|Kyoto Gases (excl. LUC)",
    entity=="KYOTOGHG (AR5GWP100)" & category..IPCC2006_PRIMAP.=="0" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|Kyoto Gases|AR5 (incl. all LULUCF)",
    entity=="KYOTOGHG (AR5GWP100)" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR" ~ "Emissions|Kyoto Gases|AR5 (excl. LUC)",
    entity=="CO2" & category..IPCC2006_PRIMAP.=="0" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|CO2 (incl. all LULUCF)",
    entity=="CO2" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|CO2|Energy and Industrial Processes",
    entity=="CO2" & category..IPCC2006_PRIMAP.=="M.LULUCF" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|CO2|AFOLU (incl. all LULUCF)",
    entity=="CO2" & category..IPCC2006_PRIMAP.=="M.LULUCF" & scenario..PRIMAP.hist. =="HISTTP"~ "Emissions|CO2|AFOLU",
    entity=="CH4" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|CH4",
    entity=="N2O" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|N2O",
    entity=="FGASES (AR4GWP100)" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR"~ "Emissions|F-Gases",
    entity=="FGASES (AR5GWP100)" & category..IPCC2006_PRIMAP.=="M.0.EL" & scenario..PRIMAP.hist. =="HISTCR" ~ "Emissions|F-Gases|AR5"
  ))|> filter(!is.na(entity))|>
  select(-category..IPCC2006_PRIMAP.,-scenario..PRIMAP.hist.)|>
  mutate(model="PRIMAP-hist",scenario="historical")|>
  rename(variable=entity,iso=region) |>
  #rename EARTH to World
  mutate(iso = ifelse(iso == "EARTH", "World", iso)) |>
  #filter out country groups (only keep EU27BX and World)
  filter(!iso %in% c("LDC","AOSIS","ANNEXI","BASIC","NONANNEXI","UMBRELLA"))



dat_prim <-  rbind(dat_prim,
                  dat_prim|>select(-unit) |> filter(variable %in% c("Emissions|CO2|AFOLU","Emissions|CO2|Energy and Industrial Processes","Emissions|Kyoto Gases (excl. LUC)"))|>
                    pivot_wider(names_from=variable,values_fill = 0)|>mutate(
                      `Emissions|CO2`=`Emissions|CO2|AFOLU`+`Emissions|CO2|Energy and Industrial Processes`,
                      `Emissions|Kyoto Gases`=`Emissions|CO2|AFOLU`+`Emissions|Kyoto Gases (excl. LUC)`
                    ) |> pivot_longer(cols=c(-iso,-year,-model,-scenario),names_to = 'variable')|>
                    filter(variable %in% c("Emissions|CO2","Emissions|Kyoto Gases"))|>
                    mutate(unit = case_when(
                      variable=="Emissions|Kyoto Gases" ~ "Mt CO2eq",
                      variable=="Emissions|CO2" ~ "Mt CO2"
                    )))

#addition of international shipping and aviation emissions to dat_prim further below
#search for:
# dat_prim <- rbind(dat_prim,
                # dat_ceds_int...)


###### CEDS ##############
dat_ceds <- ceds |> filter(year > starty)|> select (-units)|>
  mutate(unit = case_when(
    entity=="CH4" ~ "Mt CH4",
    entity=="N2O" ~ "kt N2O",
    entity=="SO2" ~ "kt SO2",
    entity=="BC" ~ "Kt C",
    entity=="CO" ~ "kt CO",
    entity=="NH3" ~ "kt NH3",
    entity=="NMVOC" ~ "Kt NMVOC",
    entity=="NOx" ~ "kt NOx",
    entity=="OC" ~ "kt C",
    .default="Mt CO2"
  )) |>
  mutate(value = case_when(
    entity=="N2O" ~ value,
    .default=value/1000
  ))|>
  mutate(entity = case_when(
    entity=="CO2" ~ "Emissions|CO2|Energy and Industrial Processes",
    entity=="CH4" ~ "Emissions|CH4",
    entity=="N2O" ~ "Emissions|N2O",
    entity=="SO2" ~ "Emissions|SO2",
    entity=="BC" ~ "Emissions|BC",
    entity=="CO" ~ "Emissions|CO",
    entity=="NH3" ~ "Emissions|NH3",
    entity=="NMVOC" ~ "Emissions|NMVOC",
    entity=="NOx" ~ "Emissions|NOx",
    entity=="OC" ~ "Emissions|OC"
  ))|>
  mutate(model="ceds",scenario="historical")|>
  rename(variable=entity)

map_ceds <- read.csv("mappings/ceds_sector_mapping.csv") %>% gather_map()
map_ceds_chn <- read.csv("mappings/ceds_sector_mapping_China.csv") %>% gather_map() ##used for China memo to map heat to electricity, not other energy supply. may want to use for other countries

#add iamc2 mappings
# ceds_iamc2 <- read.csv("mappings/aggregated_sector.csv")
# ceds_iamc2 <- ceds_iamc2 %>%
#   filter(Source == "CEDSv2021_04_21") %>%
#   select(-c("Source", "IPPC.Code")) %>%
#   rename("sector" = "Sector.Names")
#   
# map_ceds <- left_join(map_ceds, ceds_iamc2, by = "sector")
# map_ceds$IAMC <- gsub("Supply", "Energy", map_ceds$IAMC)
# 
# map_ceds <- map_ceds %>%
#   mutate(Aggregated.Sector = case_when(
#     sector == "2C1_Iron-steel-alloy-prod" ~ "Iron and Steel", 
#     sector == "2C3_Aluminum-production" ~ "Aluminum",
#     sector == "2C4_Non-Ferrous-other-metals" ~ "Non-Ferrous Metals",
#     sector == "1A3aii_Domestic-aviation" ~ "Transport",
#     sector == "1A3di_International-shipping" ~ "Transport", 
#     TRUE ~ Aggregated.Sector
#   )) %>%
#   mutate(IAMC = case_when(
#     sector == "5A_Solid-waste-disposal" ~ "Waste",
#     sector == "5C_Waste-combustion" ~ "Waste",
#     sector == "5D_Wastewater-handling" ~ "Waste",
#     sector == "5E_Other-waste-handling" ~ "Waste",
#     TRUE ~ IAMC)
#   ) %>%
#   mutate(IAMC2 = paste0(IAMC,"|",Aggregated.Sector))
# 
# map_ceds <- map_ceds %>%
#   mutate(IAMC2 = case_when(
#     IAMC2 == "Transport|Transport" ~ "Transport", 
#     IAMC2 == "Energy|Other|Solid Fuels" ~ "Energy|Coal", 
#     IAMC2 == "Energy|Other|Oil and Gas" ~ "Energy|Oil and Gas", 
#     IAMC2 == "Agriculture|Aggregate Sources and Non-CO2 Emissions Sources on Land" ~ "Agriculture|Soil Emissions", 
#     IAMC2 == "Energy|Electricity|Energy Industries" ~ "Energy|Electricity",
#     IAMC2 == "Energy|Other|Energy Industries" ~ "Energy|Other",
#     IAMC2 == "Buildings|Other Energy Sector" ~ "Buildings",
#     IAMC2 == "Energy|Other|Gas Distribution" ~ "Energy|Gas Distribution",
#     IAMC2 == "Energy|Other|Gas Production" ~ "Energy|Gas Production",
#     IAMC2 == "Energy|Other|Fugitive Other" ~ "Energy|Fugitive Other",
#     TRUE ~ IAMC2
#   ))

#add sectoral data based on IAMC mappings

dat_ceds <- dat_ceds |> rbind(test <- ceds_s |> left_join(map_ceds_chn |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CO2",value=value/1000)|> rename(variable=var))  

dat_ceds <- dat_ceds |> rbind(test <- ceds_m |> left_join(map_ceds_chn |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CH4",value=value/1000)|> rename(variable=var)) 

dat_ceds <- dat_ceds |> rbind(ceds_n |> left_join(map_ceds_chn |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt N2O",value=value/1000)|> rename(variable=var))  


#add sectoral data based on IAMC2 mappings

# dat_ceds <- dat_ceds |> rbind(ceds_s |> left_join(map_ceds |> select(sector,IAMC2))|> 
#   group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#   ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#   select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#               unit="Mt CO2",value=value/1000)|> rename(variable=IAMC2))  
# 
# dat_ceds <- dat_ceds |> rbind(ceds_m |> left_join(map_ceds |> select(sector,IAMC2))|> 
#                                 group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#                                 ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#                                 select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#                                                                unit="Mt CH4",value=value/1000)|> rename(variable=IAMC2)) 
# 
# dat_ceds <- dat_ceds |> rbind(ceds_n |> left_join(map_ceds |> select(sector,IAMC2))|> 
#                                 group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#                                 ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#                                 select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#                                                                unit="Mt N2O",value=value/1000)|> rename(variable=IAMC2))  

#emissions from international aviation and shipping to add to PRIMAP-hist dataset
dat_ceds_int <- dat_ceds|> filter(iso=="GLOBAL")

#calculate World total, and drop "GLOBAL" values (which represent the residual not assigned to any region in particular)
dat_ceds <- rbind(dat_ceds |> group_by(variable,year,unit,model,scenario) |> summarize(value = sum(value, na.rm = T)) |> ungroup() |> mutate(iso="World"),
              dat_ceds |> filter(iso !="GLOBAL"))


#optional: would be better to directly include into the prim object, but harder to do. Given data comparison 
if(F){
#adding international shipping and aviation emissions to World total in PRIMAP_hist
setdiff(unique(dat_prim$variable),unique(dat_ceds$variable))
intersect(unique(dat_prim$variable),unique(dat_ceds$variable))

dat_prim <- rbind(dat_prim |> filter(iso !="World"),
                  #World data in variables that do not get adjusted 
                  dat_prim |> filter(iso=="World",
                                     variable %in% c("Emissions|F-Gases","Emissions|F-Gases|AR5",
                                                     "Emissions|CO2|AFOLU","Emissions|CO2|AFOLU (incl. all LULUCF)")),
                  #World data that needs adjustment to include int. aviation and shipping 
                  rbind(dat_prim |> filter(iso=="World",
                                     !variable %in% c("Emissions|F-Gases","Emissions|F-Gases|AR5",
                                                                        "Emissions|CO2|AFOLU","Emissions|CO2|AFOLU (incl. all LULUCF)"))|>
                          select(-iso),
                  #add int. emissions from CEDS, use 2022 for 2023
                  dat_ceds_int |> rbind(dat_ceds_int |> filter(year==2022) |> mutate(year=2023)) |> 
                    filter(variable %in% c("Emissions|CH4","Emissions|N2O","Emissions|CO2|Energy and Industrial Processes"))|>
                  select(-unit,-iso)|>pivot_wider(names_from = variable) |> 
                  mutate(`Emissions|CO2`=`Emissions|CO2|Energy and Industrial Processes`,
                         `Emissions|CO2 (incl. all LULUCF)`=`Emissions|CO2|Energy and Industrial Processes`,
                         `Emissions|Kyoto Gases`=`Emissions|CO2|Energy and Industrial Processes`+
                           gwp100[gwp100$entity=="CH4",]$gwp*`Emissions|CH4` + gwp100[gwp100$entity=="N2O",]$gwp*`Emissions|N2O`,
                         `Emissions|Kyoto Gases (excl. LUC)`=`Emissions|CO2|Energy and Industrial Processes`+
                           gwp100[gwp100$entity=="CH4",]$gwp*`Emissions|CH4` + gwp100[gwp100$entity=="N2O",]$gwp*`Emissions|N2O`,
                         `Emissions|Kyoto Gases (incl. all LULUCF)`=`Emissions|CO2|Energy and Industrial Processes`+
                           gwp100[gwp100$entity=="CH4",]$gwp*`Emissions|CH4` + gwp100[gwp100$entity=="N2O",]$gwp*`Emissions|N2O`,
                         `Emissions|Kyoto Gases|AR5 (excl. LUC)`=`Emissions|CO2|Energy and Industrial Processes`+
                           gwp100[gwp100$entity=="CH4",]$gwp*`Emissions|CH4` + gwp100[gwp100$entity=="N2O",]$gwp*`Emissions|N2O`,
                         `Emissions|Kyoto Gases|AR5 (incl. all LULUCF)`=`Emissions|CO2|Energy and Industrial Processes`+
                           gwp100[gwp100$entity=="CH4",]$gwp*`Emissions|CH4` + gwp100[gwp100$entity=="N2O",]$gwp*`Emissions|N2O`)|>
                  pivot_longer(cols=c(-model,-scenario,-year),names_to = 'variable')|>
                  mutate(unit=case_when(
                    variable == "Emissions|CH4" ~ "Mt CH4",
                    variable == "Emissions|N2O" ~ "kt N2O",
                    variable %in% c("Emissions|CO2 (incl. all LULUCF)",
                                    "Emissions|CO2|Energy and Industrial Processes",
                                    "Emissions|CO2")  ~ "Mt CO2",
                    .default = "Mt CO2eq"
                  ))) |> group_by(variable,unit,year,scenario)|>
                  summarise(value=sum(value)) |> ungroup() |> 
                  mutate(model="PRIMAP-hist",iso="World"))
}

###### OWID Emissions ------------------------------------
dat_owid_co2 <- owid_co2_data %>%
  filter(!is.na(value))

# Load the mapping dataset
mapping_data <- read_csv("mappings/owid_co2_mapping.csv")

# Merge the pivoted dataset with the mapping data
dat_owid_co2 <- dat_owid_co2 %>%
  left_join(mapping_data, by = "variable") %>%
  rename(variable_iamc = IAMC) %>%
  # Change population variable to units of million individuals from individuals
  # and change 
  mutate(value = ifelse(variable_iamc == "Population", value * 10^(-6), ifelse(variable_iamc == "GDP|PPP", value * 10^(-9), value)),
         unit = ifelse(unit == "persons", "million", ifelse(unit == "MTCO2", "Mt CO2", ifelse(unit == "$ (Year 2011)", "billion US$2011/yr", unit)))) |>
  mutate(iso_code = ifelse(country == "World", "World", iso_code)) |>
  select(country, year, iso_code,variable_iamc, value, unit) %>%
  rename(variable = variable_iamc)

# Remove rows where variable is NA and 
# where iso is NA (for super-country groupings such as worldwide values)
dat_owid_co2 <- dat_owid_co2 %>%
  filter(!is.na(variable),
         !is.na(iso_code))

# Add 'Model' column
dat_owid_co2 <- dat_owid_co2 %>%
  mutate(model = "OWID")

# Add 'Scenario' column based on year
dat_owid_co2 <- dat_owid_co2 %>%
  # mutate(scenario = ifelse(year < 2024, "historical", "projection")) %>%
  mutate(scenario =  "historical") %>%
  filter(value != 0)%>%
  rename(iso = iso_code) %>%
  select(iso, variable, unit, year, value, model, scenario)

dat_owid_co2 <- dat_owid_co2 %>% filter(year > starty)



###### GCB  ------------------------------------
#create new columns
dat_land <- land %>%
  mutate(model = paste0("GCB_",model), scenario = "historical", unit = "Mt CO2/yr", variable = "Emissions|CO2|LUC", iso = countrycode(Country, "country.name", "iso3c"))

dat_land$iso <- ifelse(is.na(dat_land$iso) & dat_land$Country == "Global", "World",
                       ifelse(is.na(dat_land$iso) & dat_land$Country == "EU27", "EU27",
                              dat_land$iso))
dat_land <- dat_land %>%
  select(-Country)



###### IEA CH4  ------------------------------------

dat_ch4 <- iea_ch4 %>%
  mutate(
    type = case_when(
      type == "Energy" ~ "Emissions|CH4|Energy",
      type == "Agriculture" ~ "Emissions|CH4|Agriculture",
      type == "Waste" ~ "Emissions|CH4|Waste",
      type == "Other" ~ "Emissions|CH4|Other"),
    country = ifelse(region == "World", "World", country)) |>
  select(-region)

dat_ch4 <- dat_ch4 %>%
  group_by(country, year, type) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

# Add ISO column for country names using the countrycode library
dat_ch4 <- dat_ch4 %>%
  mutate(iso = countrycode(country, 'country.name', 'iso3c')) |>
  mutate(iso = ifelse(country == "World", "World", iso)) |>
  # Remove rows where iso is NA (eg. for super-country groupings)
  filter(!is.na(iso))

# Add unit column as "KtCH4"
dat_ch4 <- dat_ch4 %>%
  mutate(unit = "KtCH4")

# Rename columns as specified
dat_ch4 <- dat_ch4 %>%
  rename(variable = type)

# Add a new column 'scenario' with the value historical, and model 'IEA_Methane'
dat_ch4 <- dat_ch4 %>%
  mutate(scenario = "historical", model = "IEA_Methane")

dat_ch4 <- dat_ch4 %>%
  select(iso, variable, unit, year, value, model, scenario)

dat_ch4$year <- as.numeric(dat_ch4$year)



###### CLIMATE TRACE  -------------------------------------
dat_ct <- ct_waste %>%
  #convert to Mt CH4
  mutate(value = emissions_quantity/1000000) %>%
  mutate(variable = case_when(
    subsector == "solid-waste-disposal" ~ "Emissions|CH4|Waste|Solid Waste",
    subsector == "wastewater-treatment-and-discharge" ~ "Emissions|CH4|Waste|Wastewater",
    subsector == "incineration-and-open-burning-of-waste" ~ "Emissions|CH4|Waste|Other",
    subsector == "biological-treatment-of-solid-waste-and-biogenic" ~ "Emissions|CH4|Waste|Other"
  )) %>%
  select(-c(subsector, gas, emissions_quantity, emissions_quantity_units)) %>%
  group_by(iso, year, variable) %>%
  summarize(value = sum(value)) %>%
  ungroup()

ct_totals <- dat_ct %>%
  group_by(iso, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "Emissions|CH4|Waste")

dat_ct <- bind_rows(dat_ct, ct_totals) %>%
  arrange(iso, year) %>%
  mutate(unit = "Mt CH4", model = "CT", scenario = "historical")

# Add in World, since this is not included in the original datasets
dat_ct <- dat_ct |>
  rbind(dat_ct |> 
          group_by(year, variable, unit, model, scenario) |>
          summarize(value = sum(value, na.rm = T)) |>
          ungroup() |>
          mutate(iso = "World"))



###### EMBER ######
dat_ecap <- ecap |> filter(year > starty)|> select (-variable,-region)|>
  mutate(fuel = case_when(
    fuel=="Other Fossil" ~ "Capacity|Electricity|Other Fossil",
    fuel=="Other Renewables" ~ "Capacity|Geothermal",
    fuel=="Bioenergy" ~ "Capacity|Electricity|Biomass",
    fuel=="Coal" ~ "Capacity|Electricity|Coal",
    fuel=="Gas" ~ "Capacity|Electricity|Gas",
    fuel=="Hydro" ~ "Capacity|Electricity|Hydro",
    fuel=="Nuclear" ~ "Capacity|Electricity|Nuclear",
    fuel=="Solar" ~ "Capacity|Electricity|Solar",
    fuel=="Wind" ~ "Capacity|Electricity|Wind",
    fuel=="Total" ~ "Capacity|Electricity",
    .default="NA"
  ))|> filter(fuel!="NA")|>
  mutate(model="EMBER",scenario="historical")|>
  rename(variable=fuel)


dat_egeny <- egeny |> filter(year > starty)|> select (-variable,-region)|>
  mutate(fuel = case_when(
    fuel=="Other Fossil" ~ "Secondary Energy|Electricity|Other Fossil",
    fuel=="Other Renewables" ~ "Secondary Energy|Electricity|Geothermal",
    fuel=="Bioenergy" ~ "Secondary Energy|Electricity|Biomass",
    fuel=="Coal" ~ "Secondary Energy|Electricity|Coal",
    fuel=="Gas" ~ "Secondary Energy|Electricity|Gas",
    fuel=="Hydro" ~ "Secondary Energy|Electricity|Hydro",
    fuel=="Nuclear" ~ "Secondary Energy|Electricity|Nuclear",
    fuel=="Solar" ~ "Secondary Energy|Electricity|Solar",
    fuel=="Wind" ~ "Secondary Energy|Electricity|Wind",
    fuel=="Total" ~ "Secondary Energy|Electricity",
    .default="NA"
  ))|> filter(fuel!="NA")|>
  mutate(model="EMBER",scenario="historical",value=value/ej2twh)|>
  rename(variable=fuel)|>mutate(unit="EJ/yr")

total_per_country_year <- dat_egeny %>%
  group_by(iso, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE))

# Calculate the share in Total for each fuel type and create a new dataframe
dat_egeny_shares <- dat_egeny %>%
  inner_join(total_per_country_year, by = c("iso", "year")) %>%
  mutate(value = (value / total_value) * 100,
         variable = paste(variable, "Share", sep = "|"),
         unit = "percentage",
         model = "EMBER",
         scenario = "historical") %>%
  select(iso, year, variable, value, unit,model,scenario)

egeny_solar_wind <- dat_egeny_shares %>%
  filter(variable %in% c("Secondary Energy|Electricity|Wind|Share", "Secondary Energy|Electricity|Solar|Share")) %>%
  group_by(iso, year, unit, model, scenario) %>%
  summarise(value = sum(value)) %>%
  mutate(variable = "Secondary Energy|Electricity|Solar+Wind|Share") %>%
  ungroup()

dat_egeny_shares <-  rbind(dat_egeny_shares,egeny_solar_wind)

#add data on captive generation for Indonesia, data from Maria Borrero
dat_egeny <- rbind(dat_egeny,data.frame(year=seq(1993,2023),value=c(0.21,0.87,0.87,1.29,1.29,3.10,4.23,
                                                                    5.84,5.84,5.84,5.84,6.02,6.02,6.37,6.55,6.76,6.97,
                                                                    6.97,10.25,10.25,10.82,13.50,14.81,17.27,24.98,28.44,36.60,
                                                                    44.55,56.64,69.21,88.51)/ej2twh,model="GEM and CGS",variable=
                                          "Secondary Energy|Electricity|Captive Coal",iso="IDN",unit="EJ/yr",
                                        scenario="historical"))

dat_egeny <- dat_egeny %>% filter(year > starty)


dat_eemi <- eemi |> filter(year > starty)|> select (-variable,-region)|>
  mutate(fuel = case_when(
    fuel=="Coal" ~ "Emissions|CO2|Energy|Supply|Electricity|Coal",
    fuel=="Gas" ~ "Emissions|CO2|Energy|Supply|Electricity|Gas",
    fuel=="Fossil" ~ "Emissions|CO2|Energy|Supply|Electricity|Fossil",
    fuel=="Total" ~ "Emissions|CO2|Energy|Supply|Electricity",
    .default="NA"
  ))|> filter(fuel!="NA")|>
  mutate(model="EMBER",scenario="historical")|>
  rename(variable=fuel)



###### EI SRWED -------------------------------------

dat_ener <- ener |> left_join(read.csv("mappings/map_ei_iamc.csv"),by = join_by(Var==EI)) |>
  filter(!is.na(IAMC),!IAMC=="",year>starty)|>mutate(value=value*factor) |> 
  select(year,iso,value,unit,IAMC) |> 
  mutate(iso = ifelse(iso == "WLD", "World", iso),
         model="Stat. Rev. World Energy Data",   ## Statistical Review of World Energy Data"
         scenario="historical") |> rename(variable=IAMC)

###### OWID Energy  ------------------------------------
dat_owid_energy <- owid_energy_data %>%
  filter(!is.na(value))

# Load the mapping dataset
mapping_data <- read_csv("mappings/owid_energy_mapping.csv")

# Merge the pivoted dataset with the mapping data
dat_owid_energy <- dat_owid_energy %>%
  left_join(mapping_data, by = "variable") %>%
  rename(variable_iamc = IAMC) %>%
  # Change population variable to units of million individuals from individuals
  mutate(value = ifelse(variable_iamc == "Population", value * 10^(-6), value),
         unit = ifelse(unit == "persons", "million", unit)) |>
  select(country, year, iso_code,variable_iamc, value, unit) %>%
  rename(variable = variable_iamc)

# Remove rows where variable is NULL
dat_owid_energy <- dat_owid_energy %>%
  filter(!is.na(variable))

# Add 'Model' column
dat_owid_energy <- dat_owid_energy %>%
  mutate(model = "OWID Energy")

# Add 'Scenario' column based on year
dat_owid_energy <- dat_owid_energy %>%
  mutate(scenario = ifelse(year < 2024, "historical", "projection")) %>%
  filter(value != 0) %>%
  rename(iso = iso_code) %>%
  mutate(iso = ifelse(country == "World", "World", iso)) |>
  select(iso, variable, unit, year, value, model, scenario)

dat_owid_energy <- dat_owid_energy %>% filter(year > starty)

dat_owid_energy <- dat_owid_energy %>% filter(!is.na(iso))

#for time being, do not keep this, as it is wrongly mapped:
# the mapping needs to be corrected to use correct IAMC variable names, and should include a mapping to units, as well as conversion factor
dat_owid_energy <- NULL



###### IEA GEVO -------------------------------------
iea_ev_share <- iea_ev %>%
  filter(parameter == "EV sales share" & mode != "Cars")%>%
  mutate(parameter = case_when(
    mode == "Cars" ~ paste("Sales Share|Passenger|LDV", sep=""),
    mode == "Buses" ~ paste("Sales Share|Passenger|Bus", sep=""),
    mode == "Trucks" ~ paste("Sales Share|Freight|>3.5t", sep=""),
    mode == "Vans" ~ paste("Sales Share|Freight|<3.5t", sep=""),
    TRUE ~ parameter  # Keep the original parameter if no condition matches
  )) %>%
  mutate(parameter = paste(parameter, powertrain, sep="|"))%>%
  rename(scenario = category) %>%
  rename(variable = parameter) %>%
  mutate(iso = countrycode(region, "country.name", "iso3c"))%>%
  select(region, iso, scenario, year, variable, unit, value) |>
  mutate(iso = case_when(
    region =="World" ~ "World",
    region =="EU27" ~ "EU27BX",
    .default = iso
  )) |>
  filter(!is.na(iso))


iea_ev <- iea_ev %>%
  filter(!(parameter == "EV sales share" & mode != "Cars"))

# Filter rows where the parameter column contains specific strings
iea_ev <- iea_ev %>%
  filter(parameter %in% c("EV sales share", "EV sales", "EV stock", "EV stock share"))


# Filter rows and add a new 'variable' column
enhanced_iea_ev <- iea_ev %>%
  filter(parameter %in% c("EV sales share", "EV sales", "EV stock", "EV stock share")) %>%
  mutate(variable = case_when(
    parameter == "EV stock share" ~ "Stock Share|Transportation",
    parameter == "EV sales share" ~ "Sales Share|Transportation",
    parameter == "EV sales" ~ "Sales|Transportation",
    parameter == "EV stock" ~ "Stock|Transportation"
  )) %>%
  mutate(variable = case_when(
    mode == "Cars" ~ paste(variable, "|Passenger|LDV", sep=""),
    mode == "Buses" ~ paste(variable, "|Passenger|Bus", sep=""),
    mode == "Trucks" ~ paste(variable, "|Freight|>3.5t", sep=""),
    mode == "Vans" ~ paste(variable, "|Freight|<3.5t", sep=""),
    TRUE ~ variable  # Default case to handle other modes or missing values
  ))%>%
  mutate(variable = paste(variable, powertrain, sep="|"))%>%
  mutate(value = if_else(unit == "Vehicles", value / 1e6, value),
         unit = if_else(unit == "Vehicles", "Million Vehicles", unit)) %>%
  rename(scenario = category)


# Step 1: Filter rows for "EV sales share"
ice_sales_share_rows <- enhanced_iea_ev %>%
  filter(parameter == "EV sales share") %>%
  mutate(value = 100 - value,   # Step 2: Update value
         parameter = "ICE sales share",  # Step 3: Update parameter
         variable = "Sales Share|Transportation|ICE")  # Update variable

# Step 4: Append new rows to the original DataFrame
enhanced_iea_ev <- bind_rows(enhanced_iea_ev, ice_sales_share_rows)


# Prepare data for EV Sales and EV Sales Share
ev_sales_data <- enhanced_iea_ev %>%
  filter(parameter == "EV sales" & mode == 'Cars' )%>%
  select(region, year, value, scenario) %>%
  rename(ev_sales = value)

ev_sales_share_data <- enhanced_iea_ev %>%
  filter(parameter == "EV sales share") %>%
  select(region, year, value, scenario) %>%
  rename(ev_sales_share = value)

# Merge the data on country, region, year, and category
total_ice_sales_data <- left_join(ev_sales_data, ev_sales_share_data, by = c("region", "year", "scenario"))

# Calculate Total ICE Sales
total_ice_sales_data <- total_ice_sales_data %>%
  mutate(total_ice_sales = (ev_sales / (ev_sales_share)) * (100 - ev_sales_share)) %>%
  select(region, year, scenario, total_ice_sales)

# Group and Summarize by country, region, and year
total_ice_sales_summary <- total_ice_sales_data %>%
  group_by(region, year,scenario) %>%
  summarise(
    value = sum(total_ice_sales, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    variable = "Sales|Transportation|ICE|Total",
    unit = "Million Vehicles"
  )

# View the final data
head(total_ice_sales_summary)

enhanced_iea_ev <- enhanced_iea_ev %>%
  select(region, scenario, year, variable, unit, value)

total_ice_sales_summary <- total_ice_sales_summary %>%
  select(region, scenario, year, variable, unit, value)

# Combine with selected_enhanced_iea_ev
dat_iea_ev <- bind_rows(enhanced_iea_ev, total_ice_sales_summary)
dat_iea_ev <- distinct(dat_iea_ev)
dat_iea_ev$iso <- countrycode(dat_iea_ev$region, "country.name", "iso3c")
dat_iea_ev <- dat_iea_ev |> mutate(iso = case_when(region == "World" ~ "World",
                                                   region == "EU27" ~ "EU27BX",
                                                   .default = iso))
dat_iea_ev <- na.omit(dat_iea_ev)

dat_iea_ev <- dat_iea_ev %>%
  select(region,iso, year, variable, unit, value, scenario )


dat_iea_ev <- dat_iea_ev %>%
  mutate(variable = case_when(
    variable == "Sales Share|Transportation|Passenger|LDV|EV" ~ "Sales Share|Transportation|Passenger|LDV|BEV+PHEV",
    TRUE ~ variable  # Keep all other variables unchanged
  ))

sales_data <- dat_iea_ev %>%
  # Make sure the data is in the right format, you might need to convert data types or reshape the data
  mutate(value = as.numeric(value)) %>%
  filter(variable %in% c("Sales|Transportation|Passenger|LDV|BEV", 
                         "Sales|Transportation|Passenger|LDV|PHEV", 
                         "Sales|Transportation|Passenger|LDV|FCEV", 
                         "Sales|Transportation|ICE|Total")) %>%
  group_by(region,iso, year, scenario) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = 'drop'  # This ensures the grouped dataframe is ungrouped after summarisation
  ) %>%
  mutate(variable = "Sales|Transportation|Total",
         unit = "Million Vehicles")

dat_iea_ev <- bind_rows(dat_iea_ev, sales_data)

bev_sales_data <- dat_iea_ev %>%
  filter(variable == "Sales|Transportation|Passenger|LDV|BEV") %>%
  select(region, iso, year, scenario, value) %>%
  rename(bev_sales = value)

total_sales_data <- dat_iea_ev %>%
  filter(variable == "Sales|Transportation|Total") %>%
  select(region, iso, year, scenario, value) %>%
  rename(total_sales = value)

# Merge the data frames on region, iso, year, and scenario
sales_data_merged <- left_join(bev_sales_data, total_sales_data, by = c("region", "iso", "year", "scenario"))
sales_data_merged <- na.omit(sales_data_merged)

# Calculate the BEV sales share
sales_data_merged <- sales_data_merged %>%
  mutate(value = (bev_sales / total_sales) * 100) %>%
  select(region, iso, year, scenario, value) %>%
  mutate(variable = "Sales Share|Transportation|Passenger|LDV|BEV",
         unit = "Percent")

# View the results
dat_iea_ev <- bind_rows(dat_iea_ev, sales_data_merged)
dat_iea_ev <- distinct(dat_iea_ev)
dat_iea_ev <- bind_rows(dat_iea_ev, iea_ev_share)
# 
# dat_iea_ev <- dat_iea_ev %>%
#   select(-(region)) %>%
#   mutate(model = "IEA_GEVO") %>%
#   select(iso, variable, unit, year, value, model, scenario)

dat_iea_ev <- dat_iea_ev %>%
  select(-(region)) %>%
  mutate(model = "IEA_GEVO") %>%
  mutate(scenario = case_when(
    scenario == "Historical" ~ "historical",
    .default = scenario
  )) %>%
  select(iso, variable, unit, year, value, model, scenario)

###### NASA  -------------------------------------

dat_nasa <-nasa_temp %>%
  select(Year, J.D) %>%
  mutate(model = "GISStemp",
         variable = "Temperature|Global Mean",
         iso = "World",
         unit = "Celsius",
         scenario = "historical") %>%
  rename(value = J.D, 
         year = Year) %>%
  filter(year > starty)

###### CRU  -------------------------------------

dat_crut <- crut %>%
  mutate(
    model = "HadCrut",
    unit = "Celsius",
    scenario = "historical",
    variable = "Temperature|Global Mean",
    iso = "World"
  ) |> rename(value=Mean_Temperature,year=Year)


#### 2.b combine iso based data sets #####
data_iso <- rbind(dat_ener,dat_prim,dat_ceds,dat_ecap,dat_egeny,dat_egeny_shares, dat_eemi,dat_iea_ev,
              dat_nasa,dat_land,dat_ch4, iiasa_data ,dat_crut,dat_owid_energy, dat_owid_co2, dat_ct)

data_iso <- data_iso %>%
  filter(!is.na(iso),
         !is.na(value),
         !is.na(year))

data_iso <- data_iso |>
  rename(region=iso)

#### 2.c separate World data ################# 
data_World <- data_iso |> filter(region == "World")

data_iso <- data_iso |> filter(region != "World")




#### 2.d convert to model regions ####
#### eg. GCAM 32 regions 
##For our analysis, we require certain variables to be mapped according to the documentation of IAMC.
##The mapping files are created using the documentation which can be found on the following link:
##-- https://data.ene.iiasa.ac.at/ar6/#/docs
# GCAM region mapping, including the other regions from Energy Institute
#there is a bit of overlap between o-afr and o-wafr/eafr for a few oil and coal variables


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

data_reg <- aggregate_regions(data_iso, reg_map)
  
#adjust global values for statistical review data with O-AFR

###### special case IEA, not fully mappable, thus added here #####

# Model region mapping
##For our analysis, we require certain variables to be mapped according to the documentation of IAMC.
##The mapping files are created using the documentation which can be found on the following link:
##-- https://data.ene.iiasa.ac.at/ar6/#/docs

map_iea <- read.csv("mappings/map_IEAWEO23_iamc.csv")


#historic data
datieah <- iea23 |> filter(year<2030,scenario=="Stated Policies Scenario") |>
  mutate(region=case_when(
    region=="United States" ~ "USA",
    .default=region))|>
  filter(region %in% c(unique(reg_map$region), 
                       "Europe", "European Union", # Remove these regions if preferred
                       "World")) |>
  mutate(model="IEA WEO 2023",scenario="historical")|>
  filter(!is.na(var))

# bring to GCAM region mapping and IAMC format
datieah <- datieah |> select(-unit) |> left_join(map_iea,by=join_by(var==WEO)) |>
  filter(IAMC!="")|>
  rename(unit = Unit_IAMC) |>
  mutate(Conversion = as.numeric(Conversion)) |>
  mutate(value = value*Conversion)|>  
  select(year,IAMC,unit,value,region,model,scenario) |>
  na.omit(IAMC) |> 
  rename(variable=IAMC)


#scenario data
datieas <- iea23 |> filter(year>2021) |>
  mutate(region=case_when(
    region=="United States" ~ "USA",
    .default=region
    ))|>
  filter(region %in% c(unique(reg_map$region), 
                       "Europe", "European Union", # Remove these regions if preferred
                       "World")) |>
  mutate(model="IEA WEO 2023")|>
  filter(!is.na(var))

# bring to GCAM region mapping and IAMC format
datieas <- datieas |> select(-unit) |> left_join(map_iea,by=join_by(var==WEO)) |>
  filter(IAMC!="")|>
  rename(unit = Unit_IAMC) |>
  mutate(Conversion = as.numeric(Conversion)) |>
  mutate(value = value*Conversion) |>
  select(year,IAMC,unit,value,region,model,scenario) |>
  na.omit(IAMC) |> 
  rename(variable=IAMC)



### 3 write and remove data #### 

#### 3.a combine and write multiple datasets --------------
# for use in main.R, etc

# Convert to wide formats before writing


# ### ISO with World:
# 
# write_this <- rbind(data_iso, data_World) |>
#   group_by(model, scenario, region, variable, value, unit) |>
#   arrange(year) |>
#   pivot_wider(names_from = year, values_from = value) |>
#   select(-c("2024")) |> #no data in this column
#   unique() |>
#   ungroup()
# 
# #write out iso and World IAMC file
# write.csv(write_this, "output/historical_iso.csv",row.names = F,quote = F)


### ISO with World (with all IEA scenarios):

write_this <- rbind(data_iso, datieah, datieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  select(-c("2024")) |> #no data in this column
  unique() |>
  ungroup()

#write out iso and World IAMC file
write.csv(write_this, "output/historical_iso.csv",row.names = F,quote = F)



# ### Aggregate regions and World (only with historical IEA / without IEA future scenarios):
# 
# write_this <- rbind(data_reg, datieah, data_World) |>
#   group_by(model, scenario, region, variable, value, unit) |>
#   arrange(year) |>
#   pivot_wider(names_from = year, values_from = value) |>
#   select(-c("2024")) |> #no data in this column
#   unique() |>
#   ungroup()
# 
# #write out IAMC file
# write.csv(write_this, 
#           file.path("output",paste0("historical_", model_regions, ".csv")),
#           row.names = F,quote = F)


### Aggregate regions and World (with all IEA scenarios):

write_this <- rbind(data_reg, datieah, datieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  select(-c("2024")) |> #no data in this column
  unique() |>
  ungroup()


#write out IAMC file
write.csv(write_this, 
          file.path("output",paste0("historical_ref_", model_regions, ".csv")),
          row.names = F,quote = F)



#### 3.b save and remove original source R objects ####
if(save_option == T){
  save(prim,ceds,eemi,eemi_eu,file = "output/emissions.Rds")
  save(ember,emberm,ecap,egeny,egenm,ener,iea_ev, file = "output/energy.Rds")
}
rm(ecap,eemi,egeny,egenm,prim,ceds,ener,iea_ev)


