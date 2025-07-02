
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
# R.version.string

# install.packages("languageserver")
# install.packages("jsonlite", type = "source")

# 
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

#install.packages("quitte", repos = c("https://pik-piam.r-universe.dev", "https://cloud.r-project.org"))
sessionInfo()
# .libPaths()
# install.packages(c("pacman"))

### Load Libraries and constants -----
library(pacman)
p_load(tidyverse,dplyr,readxl,readxl,countrycode,remotes,stringr)



library(quitte) # Download from https://pik-piam.r-universe.dev/quitte#



#source functions and constants
source("src/functions.R")


# Start year for harmonized datasets
starty <- 1976 - 1 # can be adjusted for even shorter or longer historic time series in IAMC format

# Choose the desired regions for the historical data to be aggregated up to, in 
# addition to by country: Model name / number of regions
#model_regions <- "gcam32"   # Using GCAM core regions
model_regions <- "r10"
#model_regions <- "r5"

# Change save_option to False to skip re-saving the raw data as .Rds files
save_option <- T 

### Process the data -----

#### 1. Read in data _______________________ ####### 

###### emissions: ghg - PRIMAP hist --------------------------------------------------

prim <- read.csv("data/Guetschow_et_al_2025-PRIMAP-hist_v2.6.1_final_no_rounding_13-Mar-2025.csv")
unique(prim$entity)
unique(prim$area..ISO3.)
prim <- prim |> pivot_longer(cols = c(-source,-scenario..PRIMAP.hist.,-provenance,-area..ISO3.,-entity,-unit,-category..IPCC2006_PRIMAP.),names_to = "year")
prim$year <- substr(prim$year,2,5)

prim <- prim |> rename(region=area..ISO3.)
prim$year <- parse_number(prim$year)



###### emissions: co2 - CEDS --------------------------------------------------

#CEDS_v_2025_03_18 Release emission data
ceds <- rbind(read.csv("data/CEDS_v_2025_03_18_aggregate/CH4_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,57)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/CO2_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/N2O_CEDS_estimates_by_country_v_2025_03_18.csv")|>
               pivot_longer(cols = seq(4,57)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/SO2_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/BC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/CO_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/NH3_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/NMVOC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/NOx_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/CEDS_v_2025_03_18_aggregate/OC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))

#sectoral ceds data (so far only for CO2, but could be expanded to more gases, at least for CH4 could be interesting):
ceds_s <-read.csv("data/CEDS_v_2025_03_18_aggregate/CO2_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,278))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5))) %>%
  filter(year > starty)

ceds_m <-read.csv("data/CEDS_v_2025_03_18_aggregate/CH4_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,58))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))  %>%
  filter(year > starty)

ceds_n <-read.csv("data/CEDS_v_2025_03_18_aggregate/N2O_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,58))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))%>%
  filter(year > starty)

#compile reference historic IAMC dataset
# data <- prim |> reference



###### emissions: co2 - OWID ---------

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
# there is an API (and additional GHGs) available, 
# but currently this uses manually-downloaded data

# Select CH4 as the Emissions Type, then download the Waste section as a CSV and unzip it

ct_waste <- rbind(read.csv("data/solid-waste-disposal_country_emissions_v4_2_0_05_25.csv"),
                  read.csv("data/industrial-wastewater-treatment-and-discharge_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/domestic-wastewater-treatment-and-discharge_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/incineration-and-open-burning-of-waste_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/biological-treatment-of-solid-waste-and-biogenic_country_emissions_v4_2_0_05_2025.csv"))

ct_waste <- ct_waste %>%
  rename(iso=iso3_country) |>
  mutate(year=parse_number(substr(start_time,0,4)),
         emissions_quantity_units = "tonnes") |>
  select(-c(sector, temporal_granularity, created_date, modified_date, start_time, end_time)) |>
  filter(gas == "ch4") |>
  na.omit()



###### energy: elec - EMBER --------------------------------------------------

# - monthly_full_release_long_format-4.csv
# - yearly_full_release_long_format.csv

ember = read.csv("data/yearly_full_release_long_format_05_2025.csv")
emberm = read.csv("data/monthly_full_release_long_format_05_2025.csv")

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


###### energy: other - EI SRWED --------------------------------------------------
# Energy Institute's Statistical Review of World Energy Data

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

###### energy: IEA WEO 2024 --------------------------------------------------


# Region - WEO2024_AnnexA_Free_Dataset_Regions.csv
# World - WEO2024_AnnexA_Free_Dataset_World.csv
iea24 <- read.csv("data/WEO2024_AnnexA_Free_Dataset_Regions.csv")|>
  mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW))
iea24 <- rbind(iea24, ## add all available data on Net Zero scenarios, plus additional variables for others
               read.csv("data/WEO2024_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO=="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW)),#|>select(-X),
               read.csv("data/WEO2024_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO!="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW)) |> 
                 filter(!var %in% unique(iea24$var))#|>select(-X)
)|>
  select(-CATEGORY,-PRODUCT,-FLOW,-PUBLICATION)|>
  rename(unit=UNIT,region=REGION,year=YEAR,value=VALUE,scenario=SCENARIO)



###### energy: OWID ---------

# In ReadMe: Download our complete Energy dataset : CSV
owid_energy_data <- read_csv("data/owid-energy-data.csv")

# Pivot the CO2 dataset
owid_energy_data <- owid_energy_data %>%
  pivot_longer(
    cols = population:last_col(), 
    names_to = "variable", 
    values_to = "value"
  )


###### trn: ev - IEA GEVO --------------------------------------------------

iea_ev <- readxl::read_excel("data/GlobalEVDataExplorer2025.xlsx")
#This dataset had added a few new aggregate regions: Asia Pacific, Central and South America, EU27, Europe, Middle East and Caspian, North America, Rest of the world, World
# Asia pacific+Central and South America+ Europe+ Middle East and Caspian+North America+Rest of the world = World(roughly)
# Rename and drop columns to match expected structure
iea_ev <- iea_ev %>%
  rename(region = region_country) %>%
  select(-`Aggregate group`)
###### trn: ev - Robbie --------------------------------------------------

robbie_ev <- read.csv("data/all_carsales_monthly_05_2025.csv") |>
  mutate(variable = "all_carsales_monthly",
         Value = Value * 10^(-6),
         unit = "million")

###### trn: service - OECD --------------------------------------------------
# tkt (ton-kilometer traveled) and pkt (passenger-kilometer traveled)
#https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CTransport%23TRA%23&pg=0&fc=Topic&bp=true&snb=22&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TRENDS%40DF_TRENDSFREIGHT&df[ag]=OECD.ITF&df[vs]=1.0&dq=.A.....&to[TIME_PERIOD]=false&pd=%2C
#https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CTransport%23TRA%23&pg=0&fc=Topic&bp=true&snb=22&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TRENDS%40DF_TRENDSPASS&df[ag]=OECD.ITF&df[vs]=1.0&dq=.A.....&to[TIME_PERIOD]=false&pd=%2C
# Click on the "Download" button > "Unfiltered data in tabular dataset (CSV)
OECD_f <- read.csv("data/OECD.ITF,DSD_TRENDS@DF_TRENDSFREIGHT,1.0+all.csv")
OECD_p <- read.csv("data/OECD.ITF,DSD_TRENDS@DF_TRENDSPASS,1.0+all.csv")
OECD <- bind_rows(OECD_f, OECD_p)

###### climate: temp - NASA --------------------------------------------------

nasa_temp <- read.csv("data/GLB.Ts+dSST_05_2025.csv",skip = 1)

###### climate: temp - CRU --------------------------------------------------

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
    entity=="CH4" ~ "Mt CH4/yr",
    entity=="N2O" ~ "kt N2O/yr",
    entity=="CO2" ~ "Mt CO2/yr",
    .default="Mt CO2-equiv/yr"
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
                      variable=="Emissions|Kyoto Gases" ~ "Mt CO2-equiv/yr",
                      variable=="Emissions|CO2" ~ "Mt CO2/yr"
                    )))

#addition of international shipping and aviation emissions to dat_prim further below
#search for:
# dat_prim <- rbind(dat_prim,
                # dat_ceds_int...)


###### CEDS ##############
dat_ceds <- ceds |> filter(year > starty)|> select (-units)|>
  mutate(unit = case_when(
    entity=="CH4" ~ "Mt CH4/yr",
    entity=="N2O" ~ "kt N2O/yr",
    entity=="SO2" ~ "kt SO2/yr",
    entity=="BC" ~ "Mt BC/yr",
    entity=="CO" ~ "Mt CO/yr",
    entity=="NH3" ~ "Mt NH3/yr",
    entity=="NMVOC" ~ "Mt VOC/yr",
    entity=="NOx" ~ "Mt NO2/yr",
    entity=="OC" ~ "Mt OC/yr",
    .default="Mt CO2/yr"
  )) |>
  mutate(value = case_when(
    entity=="N2O" ~ value,
    entity=="SO2" ~ value,
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
    entity=="NMVOC" ~ "Emissions|VOC",
    entity=="NOx" ~ "Emissions|NOx",
    entity=="OC" ~ "Emissions|OC"
  ))|>
  mutate(model="ceds",scenario="historical")|>
  rename(variable=entity)

## Decide whether to map heat production to electricity, rather than report as own category, which may be appropriate for eg. China figures
switch_count_heat_as_elec <- F
if (switch_count_heat_as_elec == T) {
  map_ceds_chn <- read.csv("mappings/ceds_sector_mapping_China.csv") %>% gather_map() 
} else {
  map_ceds <- read.csv("mappings/ceds_sector_mapping.csv") %>% gather_map()
}

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

dat_ceds <- dat_ceds |> rbind(test <- ceds_s |> left_join(map_ceds |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CO2/yr",value=value/1000)|> rename(variable=var))  

dat_ceds <- dat_ceds |> rbind(test <- ceds_m |> left_join(map_ceds |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CH4/yr",value=value/1000)|> rename(variable=var)) 

dat_ceds <- dat_ceds |> rbind(ceds_n |> left_join(map_ceds |> select(sector,var))|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt N2O/yr",value=value/1000)|> rename(variable=var))  


#add sectoral data based on IAMC2 mappings

# dat_ceds <- dat_ceds |> rbind(ceds_s |> left_join(map_ceds |> select(sector,IAMC2))|> 
#   group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#   ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#   select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#               unit="Mt CO2/yr",value=value/1000)|> rename(variable=IAMC2))  
# 
# dat_ceds <- dat_ceds |> rbind(ceds_m |> left_join(map_ceds |> select(sector,IAMC2))|> 
#                                 group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#                                 ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#                                 select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#                                                                unit="Mt CH4/yr",value=value/1000)|> rename(variable=IAMC2)) 
# 
# dat_ceds <- dat_ceds |> rbind(ceds_n |> left_join(map_ceds |> select(sector,IAMC2))|> 
#                                 group_by(iso,entity,year,IAMC2,units) |> summarize(value=sum(value))|>
#                                 ungroup() |> mutate(IAMC2 = paste0("Emissions|",entity,"|",IAMC2))|>
#                                 select(-entity,-units)|>mutate(model="ceds",scenario="historical",
#                                                                unit="Mt N2O/yr",value=value/1000)|> rename(variable=IAMC2))  

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
                    variable == "Emissions|CH4" ~ "Mt CH4/yr",
                    variable == "Emissions|N2O" ~ "kt N2O/yr",
                    variable %in% c("Emissions|CO2 (incl. all LULUCF)",
                                    "Emissions|CO2|Energy and Industrial Processes",
                                    "Emissions|CO2")  ~ "Mt CO2/yr",
                    .default = "Mt CO2-equiv/yr"
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
         unit = ifelse(unit == "persons", "million", ifelse(unit == "MTCO2", "Mt CO2/yr", ifelse(unit == "MTCH4", "Mt CH4/yr", ifelse(unit == "$ (Year 2011)", "billion US$2011/yr", unit))))) |>
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
  #convert to Mt CH4/yr
  mutate(value = emissions_quantity/1000000) %>%
  mutate(variable = case_when(
    subsector == "solid-waste-disposal" ~ "Emissions|CH4|Waste|Solid Waste",
    subsector == "industrial-wastewater-treatment-and-discharge" ~ "Emissions|CH4|Waste|Wastewater|Industrial",
    subsector == "domestic-wastewater-treatment-and-discharge" ~ "Emissions|CH4|Waste|Wastewater|Domestic",
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
  mutate(unit = "Mt CH4/yr", model = "CT", scenario = "historical")

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
    fuel=="Other Renewables" ~ "Capacity|Electricity|Geothermal",
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

# Calculate the share in Total for each fuel type and create a new dataframe
dat_egeny_shares <- dat_egeny |>
  group_by(across(-c(variable, value))) |>
  mutate(value = ifelse(!is.na(value),
                        (value / value[variable == "Secondary Energy|Electricity"]) * 100,
                        NA),
         variable = paste(variable, "Share", sep = "|"),
         unit = "%") |>
  filter(variable != "Secondary Energy|Electricity|Share") |>
  select(iso, year, variable, value, unit,model,scenario)

egeny_solar_wind <- dat_egeny_shares %>%
  filter(variable %in% c("Secondary Energy|Electricity|Wind|Share", "Secondary Energy|Electricity|Solar|Share")) %>%
  group_by(iso, year, unit, model, scenario) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  mutate(variable = "Secondary Energy|Electricity|Solar+Wind|Share") %>%
  ungroup()

dat_egeny_shares <-  dat_egeny_shares |>
  bind_rows(egeny_solar_wind) |>
  filter(!is.na(value))

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
  rename(variable=fuel)|>
  mutate(unit = "Mt CO2/yr") 



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
#  Prepare data
iea_ev_clean <- iea_ev %>%
  mutate(
    iso = countrycode(region, "country.name", "iso3c"),
    iso = case_when(region == "World" ~ "World",
                    region == "EU27" ~ "EU27BX",
                    TRUE ~ iso),
    mode_group = case_when(
      mode %in% c("Cars", "Vans") ~ "Light-Duty Vehicle",
      mode == "Buses" ~ "Bus",
      mode == "Trucks" ~ "Truck",
      TRUE ~ NA_character_
    ),
    scenario = if_else(category == "Historical", "historical", category),
    value = case_when(
      unit == "Vehicles" ~ value / 1e6,
      unit == "percent" ~ value,            
      TRUE ~ value
    ),
    unit = case_when(
      unit == "Vehicles" ~ "million",
      TRUE ~ unit
    ),
    model = "IEA_GEVO"
  ) %>%
  filter(!is.na(iso), !is.na(mode_group))

#  EV stocks
ev_stock <- iea_ev_clean %>%
  filter(parameter == "EV stock", powertrain %in% c("BEV", "PHEV", "FCEV")) %>%
  mutate(variable = case_when(
    powertrain == "BEV" ~ paste0("Stocks|Transportation|", mode_group, "|Battery-Electric"),
    powertrain == "PHEV" ~ paste0("Stocks|Transportation|", mode_group, "|Plug-in Hybrid"),
    powertrain == "FCEV" ~ paste0("Stocks|Transportation|", mode_group, "|Fuel-Cell-Electric")
  )) %>%
  group_by(iso, variable, unit, year, model, scenario, mode_group) %>%
  summarise(value = sum(value), .groups = "drop")%>%
  select(iso, variable, unit, year, value, model, scenario, mode_group)

#  EV sales
ev_sales <- iea_ev_clean %>%
  filter(parameter == "EV sales", powertrain %in% c("BEV", "PHEV", "FCEV")) %>%
  mutate(variable = case_when(
    powertrain == "BEV" ~ paste0("Sales|Transportation|", mode_group, "|Battery-Electric"),
    powertrain == "PHEV" ~ paste0("Sales|Transportation|", mode_group, "|Plug-in Hybrid"),
    powertrain == "FCEV" ~ paste0("Sales|Transportation|", mode_group, "|Fuel-Cell-Electric")
  )) %>%
  group_by(iso, variable, unit, year, model, scenario, mode_group) %>%
  summarise(value = sum(value), .groups = "drop")%>%
  select(iso, variable, unit, year, value, model, scenario, mode_group)
#  Sales share (for ICE calculation)
ev_sales_share <- iea_ev_clean %>%
  filter(parameter == "EV sales share") %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(ev_share = mean(value, na.rm = TRUE), .groups = "drop")
#  Stock share (for ICE calculation)

ev_stock_share <- iea_ev_clean %>%
  filter(parameter == "EV stock share") %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(ev_share = mean(value, na.rm = TRUE), .groups = "drop")

#  Total EV stock by mode
ev_stock_total <- ev_stock %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(ev_stock = sum(value), .groups = "drop")

#  Calculate total stock from share
stock_total <- left_join(ev_stock_total, ev_stock_share,
                         by = c("iso", "year", "scenario", "mode_group")) %>%
  filter(!is.na(ev_share), ev_share > 0) %>%
  mutate(
    value = ev_stock / (ev_share / 100),
    variable = paste0("Stocks|Transportation|", mode_group),
    unit = "million",
    model = "IEA_GEVO"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario, mode_group)



#  Total EV sales by mode
ev_sales_total <- ev_sales %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(ev_sales = sum(value), .groups = "drop")


#  Calculate total sales of all node groups from share
sales_total <- left_join(ev_sales_total, ev_sales_share,
                         by = c("iso", "year", "scenario", "mode_group")) %>%
  filter(!is.na(ev_share), ev_share > 0) %>%
  mutate(
    value = ev_sales / (ev_share / 100),
    variable = paste0("Sales|Transportation|", mode_group),
    unit = "million",
    model = "IEA_GEVO"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario, mode_group)



#  Calculate ICE stock
ice_stock <- left_join(stock_total, ev_stock_total,
                       by = c("iso", "year", "scenario", "mode_group")) %>%
  mutate(
    value = value - ev_stock,
    variable = paste0("Stocks|Transportation|", mode_group, "|Internal Combustion"),
    unit = "million",
    model = "IEA_GEVO"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

#  Calculate ICE sales
ice_sales <- left_join(sales_total, ev_sales_total,
                       by = c("iso", "year", "scenario", "mode_group")) %>%
  mutate(
    value = value - ev_sales,
    variable = paste0("Sales|Transportation|", mode_group, "|Internal Combustion"),
    unit = "million",
    model = "IEA_GEVO"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# Code for the extra share variables

# Compute BEV sales share
bev_sales_share <- ev_sales %>%
  filter(grepl("Battery-Electric", variable)) %>%
  group_by(iso, year, scenario, model, mode_group) %>%
  summarise(bev_sales = sum(value), .groups = "drop") %>%
  left_join(
    sales_total %>%
      rename(total_sales = value) %>%
      select(iso, year, scenario, mode_group, total_sales),
    by = c("iso", "year", "scenario", "mode_group")
  ) %>%
  mutate(
    value = 100 * bev_sales / total_sales,
    variable = paste0("Sales Share|Transportation|", mode_group, "|Battery-Electric"),
    unit = "%",
    model = "IEA_GEVO"  
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)



# compute BEV stock share
bev_stock_share <- ev_stock %>%
  filter(grepl("Battery-Electric", variable)) %>%
  group_by(iso, year, scenario, model, mode_group) %>%
  summarise(bev_stock = sum(value), .groups = "drop") %>%
  left_join(
    stock_total %>%
      rename(total_stock = value) %>%
      select(iso, year, scenario, mode_group, total_stock),
    by = c("iso", "year", "scenario", "mode_group")
  ) %>%
  mutate(
    value = 100 * bev_stock / total_stock,
    variable = paste0("Stock Share|Transportation|", mode_group, "|Battery-Electric"),
    unit = "%",
    model = "IEA_GEVO"  
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# Calculate sales share for BEV+PHEV
bevphev_sales_share <- iea_ev_clean %>%
  filter(parameter == "EV sales share", mode %in% c("Cars", "Vans")) %>%
  group_by(iso, year, scenario, model) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    variable = "Sales Share|Transportation|Light-Duty Vehicle|BEV+PHEV",
    unit = "%"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

#Calculate stock share for BEV+PHEV
bevphev_stock_share <- iea_ev_clean %>%
  filter(parameter == "EV stock share", mode %in% c("Cars", "Vans")) %>%
  group_by(iso, year, scenario, model) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    variable = "Stock Share|Transportation|Light-Duty Vehicle|BEV+PHEV",
    unit = "%"
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)


ev_stock_clean <- ev_stock %>%
  filter(!grepl("^Stocks\\|Transportation\\|[^|]+$", variable)) # Only rows with powertrain

#  Combine final output
dat_iea_ev <- bind_rows(
  ev_sales %>% select(-mode_group),
  ev_stock_clean %>% select(-mode_group), 
  sales_total %>% select(-mode_group),                       
  stock_total %>% select(-mode_group),
  ice_sales,
  ice_stock,
  bev_sales_share,
  bev_stock_share,
  bevphev_sales_share,
  bevphev_stock_share
) %>%
  distinct()


###### Robbie  -------------------------------------

dat_rb <- robbie_ev |>
  mutate(model = "Robbie",
         scenario = "historical",
         iso = countrycode(Country, 'country.name', 'iso3c')) |>
  rename(value = Value)
  
# Transform into yearly sales data

dat_rb <- dat_rb |>
  separate_wider_position(YYYYMM, c(year = 4, month = 2))
  
# Check that there is full monthly coverage for each year  

check <- dat_rb |>
  count(Country, year, Fuel)

missing_months <- filter(check, n != 12)

# Average both UK regions, as they are similar and shouldn't be double-counted:
dat_rb <- dat_rb |>
  group_by(year, month, Fuel, iso) |>
  mutate(value = mean(value)) |>
  ungroup() |>
  select(-Country) |>
  unique() |>
  filter(!is.na(value))

dat_rb <- dat_rb |>
  group_by(iso, year, variable, Fuel, unit, model, scenario) |>
  summarise(value = sum(value, na.rm = T)) |>
  ungroup()

# # Check to make sure that everything but "Other" is included below:
# unique(dat_robbie$Fuel)[!(unique(dat_robbie$Fuel) %in% c("BatteryElectric", "Hydrogen", "InternalCombustion", "ICE", "Diesel", "LPG", "NonPluginHybrid", "Non_PluginHybrid", "Hybrid", "Petrol", "Ethanol_Petrol", "PetrolBlend", "Ethanol", "PluginHybrid"))]

# Standardize variable names
dat_rb <- dat_rb |>
  mutate(variable = "Sales|Transportation|Light-Duty Vehicle") |>
  mutate(variable = case_when(Fuel == "BatteryElectric" ~ paste0(variable, "|Battery-Electric"),
                              Fuel == "Hydrogen" ~ paste0(variable, "|Fuel-Cell-Electric"),
                              Fuel %in% c("InternalCombustion", "ICE", "Diesel", "LPG", "NonPluginHybrid", "Non_PluginHybrid", "Hybrid", "Petrol", "Ethanol_Petrol", "PetrolBlend", "Ethanol") ~ paste0(variable, "|Internal Combustion"),
                              Fuel == "PluginHybrid" ~ paste0(variable, "|Plug-in Hybrid"),
                              .default = paste0(variable, "|Other")))  |>
  select(-Fuel)

# Combine with the total sales
dat_robbie_sales <- dat_rb |>
  bind_rows(dat_rb |>
              mutate(variable = "Sales|Transportation|Light-Duty Vehicle")) |>
  group_by(across(c(-value))) |>
  summarise(value = sum(value, na.rm = T))

# Combine BEV sales with half of PHEV sales for estimate of "full electric" sales
dat_robbie_sales <- dat_robbie_sales |>
  bind_rows(dat_rb |>
              filter(grepl("Battery-Electric", variable) | grepl("Plug-in Hybrid", variable)) |>
              mutate(value = ifelse(grepl("Plug-in Hybrid", variable), value / 2, value)) |>
              mutate(variable = "Sales|Transportation|Light-Duty Vehicle|BEV + 1/2*PHEV")) |>
  group_by(across(c(-value))) |>
  summarise(value = sum(value, na.rm = T))


# Sales Share
dat_robbie_sales_share <- dat_robbie_sales |>
  group_by(across(c(-variable, -value))) |>
  mutate(value = 100 * value / value[variable == "Sales|Transportation|Light-Duty Vehicle"]) |>
  ungroup() |>
  filter(variable != "Sales|Transportation|Light-Duty Vehicle") |>
  mutate(variable = gsub("Sales", "Sales Share", variable),
         unit = "%")


# Stocks for EVs only (assuming negligible retirement up to today)
dat_robbie_stocks_ev <- dat_robbie_sales |>
  filter(grepl("Electric", variable) | grepl("EV", variable) |grepl("Plug", variable)) |>
  group_by(across(c(-year, -value))) |>
  arrange(year) |>
  # Yearly sales summed cumulatively
  mutate(stocks = cumsum(value)) |>
  ungroup() |>
  mutate(variable = gsub("Sales", "Stocks", variable)) |>
  select(-value) |>
  rename(value = stocks)

dat_robbie <- bind_rows(dat_robbie_sales, dat_robbie_sales_share, dat_robbie_stocks_ev) |>
  mutate(year = as.numeric(year))


###### OECD trn -------------------------------------

dat_oecd <- OECD %>%
  select(c("REF_AREA", "Reference.area", "Measure", "Unit.of.measure", "Transport.mode", "TIME_PERIOD", "OBS_VALUE", "Observation.status", "Unit.multiplier","Vehicle.type","MEASURE")) %>%
  rename("iso" = "REF_AREA", "region" = "Reference.area", "measure" = "Measure","MEASURE"="MEASURE", "unit" = "Unit.of.measure", "transport mode" = "Transport.mode", "year" = "TIME_PERIOD", "value" = "OBS_VALUE", "Observation status" = "Observation.status", "unit multiplier" = "Unit.multiplier","Vehicle type"="Vehicle.type") %>%
  filter(!is.na(value)) %>%
  mutate(MEASURE = str_to_sentence(tolower(MEASURE)),
         variable = paste("Energy Service|Transportation", MEASURE, `transport mode`,`Vehicle type`, sep = "|")) %>%
  mutate(unit = paste(`unit multiplier`, unit, sep = " ")) %>%
  select(iso, variable, unit, year, value) %>%
  mutate(model = "OECD", scenario = "historical") %>%
  arrange(iso, variable, unit, year, value, model, scenario)

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
              dat_robbie, dat_oecd, dat_nasa,dat_land,dat_ch4, iiasa_data ,dat_crut,dat_owid_energy, dat_owid_co2, dat_ct)

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


# Find single-country regions, if there are any:

ctry_list <- tibble("country" = unique(data_iso$region), "single" = NA)

for (ctry in ctry_list$country){
  reg_name_match <- reg_map[reg_map$region == reg_map[reg_map$iso == ctry,]$region, ]
  tst_single <- (nrow(reg_name_match) == 1)
  ctry_list$single[ctry_list$country == ctry] = tst_single
}

single_ctry_reg <- unique(ctry_list[ctry_list$single == T,]$country)

# Separate out intensive data that isn't from a single-country region,
# since not suitable to summing aggregation

data_reg <- data_iso |> 
  filter(!(grepl("share|Share", variable) & !(region %in% single_ctry_reg))) |>
  aggregate_regions(reg_map) 
         

  
#adjust global values for statistical review data with O-AFR

###### special case IEA, not fully mappable, thus added here #####

# Model region mapping
##For our analysis, we require certain variables to be mapped according to the documentation of IAMC.
##The mapping files are created using the documentation which can be found on the following link:
##-- https://data.ene.iiasa.ac.at/ar6/#/docs

map_iea <- read.csv("mappings/map_IEAWEO24_iamc.csv")

dat_iea24 <- iea24 |>
  mutate(region=case_when(
    region=="United States" ~ "USA",
    .default=region))|>
  filter(region %in% c(unique(reg_map$region), 
                       "Europe", "European Union", # Remove these regions if preferred
                       "World")) |>
  mutate(model="IEA WEO 2024")|>
  filter(!is.na(var))

# bring to GCAM region mapping and IAMC format
dat_iea24 <- dat_iea24 |>
  select(-unit) |> 
  left_join(map_iea,by=join_by(var==WEO)) |>
  filter(IAMC!="")|>
  rename(unit = Unit_IAMC) |>
  mutate(Conversion = as.numeric(Conversion)) |>
  mutate(value = value*Conversion)|>  
  select(year,IAMC,unit,value,region,model,scenario) |>
  na.omit(IAMC) |> 
  rename(variable=IAMC)

# Sum across detailed variables
dat_iea24 <- dat_iea24 |>
  bind_rows(dat_iea24 |>
              filter(variable %in% c("Secondary Energy|Electricity|Solar|PV", 
                                     "Secondary Energy|Electricity|Solar|CSP")) |>
              mutate(variable = "Secondary Energy|Electricity|Solar")) |>
  bind_rows(dat_iea24 |>
              filter(variable %in% c("Capacity|Electricity|Solar|PV", 
                                     "Capacity|Electricity|Solar|CSP")) |>
              mutate(variable = "Capacity|Electricity|Solar")) |>
  bind_rows(dat_iea24 |>
              filter(variable %in% c("Capacity|Electricity|Coal|w/ CCS", 
                                     "Capacity|Electricity|Coal|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Coal")) |>
  bind_rows(dat_iea24 |>
              filter(variable %in% c("Capacity|Electricity|Gas|w/ CCS", 
                                     "Capacity|Electricity|Gas|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Gas")) |>
  bind_rows(dat_iea24 |>
              filter(variable %in% c("Capacity|Electricity|Fossil|w/ CCS", 
                                     "Capacity|Electricity|Fossil|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Fossil")) |>
  group_by(across(-value)) |>
  summarise(value = sum(value, na.rm = T)) |>
  ungroup()

#historic data
dat_ieah <- dat_iea24 |> 
  filter(year<2030, scenario=="Stated Policies Scenario") |>
  mutate(scenario = "historical")

#scenario data
dat_ieas <- dat_iea24 |> filter(year>2022) 
 


### 3 Verify variable names ####
### to make sure that outputted variable names match IAMC Common Definitions format

template <- read_csv("template/common-definitions-template.csv", comment = "#", col_select = c(1:4))

switch_variable_check <- F
if (switch_variable_check == T) {
  
  # See the variables in our results that aren't in the template
  check_match(data_iso, template, "variable")
  check_match(dat_ieah, template, "variable")
  
  # See the variables that match
  check_match(data_iso, template, "variable", opt = "i")
  check_match(dat_ieah, template, "variable", opt = "i")
  
}

### 4 write and remove data #### 

#### 4.a combine and write multiple datasets --------------
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


##### with ISO and World: -----
# (and with all IEA scenarios)

write_this <- rbind(data_iso, dat_ieah, dat_ieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(across(where(is.list), ~ ifelse(lengths(.) == 0, NA, unlist(.)))) |>  # Remove NULL lists
  mutate(across(starts_with("20") | starts_with("19"), ~ as.numeric(.))) |>
  # select(-c("2024")) |> #no data in this column
  unique() |>
  ungroup()


#write out iso and World IAMC file
write.csv(write_this, "output/historical_iso.csv",row.names = F,quote = F)



# ### Aggregate regions and World (only with historical IEA / without IEA future scenarios):
# 
# write_this <- rbind(data_reg, dat_ieah, data_World) |>
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


##### with aggregate regions and World: -----
# (and with all IEA scenarios)

write_this <- rbind(data_reg, dat_ieah, dat_ieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(across(starts_with("20") | starts_with("19"), ~ as.numeric(.))) |>
  # select(-c("2024")) |> #no data in this column
  unique() |>
  ungroup()


#write out IAMC file
write.csv(write_this, 
          file.path("output",paste0("historical_", model_regions, ".csv")),
          row.names = F,quote = F)



#### 4.b save and remove original source R objects ####
if(save_option == T){
  save(prim,ceds,eemi,eemi_eu,file = "output/emissions.Rds")
  save(ember,emberm,ecap,egeny,egenm,ener,iea_ev, file = "output/energy.Rds")
}
rm(ecap,eemi,egeny,egenm,prim,ceds,ener,iea_ev, dat_iea_ev)


