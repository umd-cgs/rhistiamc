
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

### Load Libraries and constants -----

# install.packages("languageserver")
# install.packages("jsonlite", type = "source")

# 
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

#install.packages("quitte", repos = c("https://pik-piam.r-universe.dev", "https://cloud.r-project.org"))
sessionInfo()
# .libPaths()
# install.packages(c("pacman"))

library(pacman)
p_load(tidyverse,dplyr,readxl,readxl,countrycode,remotes,stringr)



library(quitte) # Download from https://pik-piam.r-universe.dev/quitte#



#source functions and constants
source("src/functions.R")


# Start year for harmonized datasets
starty <- 1990 - 1 # can be adjusted for even shorter or longer historic time series in IAMC format
# starty <- 1750 - 1  # earliest year in any source (CEDS, PRIMAP); each dataset contributes from its own first year

# All region schemes are run in a loop and produce separate output files:
# "gcam32_v7", "gcam32_v8", "r10", "r5", "gcamEurope"

# Change save_option to False to skip re-saving the raw data as .Rds files
save_option <- T

### Process the data -----

#### 1. Read in data _______________________ ####### 

###### emissions: ghg - PRIMAP hist --------------------------------------------------

prim <- read.csv("data/raw_historical/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_no_rounding_22-Aug-2025.csv")
unique(prim$entity)
unique(prim$area..ISO3.)
prim <- prim |> pivot_longer(cols = c(-source,-scenario..PRIMAP.hist.,-provenance,-area..ISO3.,-entity,-unit,-category..IPCC2006_PRIMAP.),names_to = "year")
prim$year <- substr(prim$year,2,5)

prim <- prim |> rename(region=area..ISO3.)
prim$year <- parse_number(prim$year)



###### emissions: co2 - CEDS --------------------------------------------------

#CEDS_v_2025_03_18 Release emission data
ceds <- rbind(read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/CH4_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,57)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/CO2_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/N2O_CEDS_estimates_by_country_v_2025_03_18.csv")|>
               pivot_longer(cols = seq(4,57)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/SO2_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/BC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/CO_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/NH3_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/NMVOC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/NOx_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)),
              read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/OC_CEDS_estimates_by_country_v_2025_03_18.csv")|>
                pivot_longer(cols = seq(4,277)))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))

#sectoral ceds data (so far only for CO2, CH4, and N2O, but could be expanded to more gases):
ceds_c <-read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/CO2_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,278))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5))) %>%
  filter(year > starty)

ceds_m <-read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/CH4_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,58))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))  %>%
  filter(year > starty)

ceds_n <-read.csv("data/raw_historical/CEDS_v_2025_03_18_aggregate/N2O_CEDS_estimates_by_country_sector_v_2025_03_18.csv") |>
  pivot_longer(cols=seq(5,58))|>
  rename(iso=country,entity=em,year=name)|>mutate(iso = toupper(iso),year=parse_number(substr(year,2,5)))%>%
  filter(year > starty)

#compile reference historic IAMC dataset
# data <- prim |> reference



###### emissions: co2 - OWID ---------

owid_co2_data <- read_csv("data/raw_historical/owid-co2-data.csv")

# Pivot the CO2 dataset
owid_co2_data <- owid_co2_data %>%
  pivot_longer(
    cols = population:last_col(), 
    names_to = "variable", 
    values_to = "value"
  )





###### emissions: co2 luc - GCB ---------
# Global Carbon Budget - National Land Use Change Carbon Emissions
file_path <- "data/raw_historical/National_LandUseChange_Carbon_Emissions_2024v1.01.xlsx"
#get header info and change column names
header_data <- read_excel(file_path, sheet = "BLUE", skip = 7, n_max = 1, col_names = FALSE, .name_repair = "minimal")
column_names <- as.character(unlist(header_data[1, 1:202]))
column_names[1] <- "year"
column_names[85] <- "Cote d'Ivoire"
column_names[181] <- "Turkiye"

#Function to process each sheet with different models
#filter out unwanted years and columns
process_sheet <- function(sheet_name, model_name) {
  df <- read_excel(file_path, sheet = sheet_name, skip = 8, col_names = FALSE, .name_repair = "minimal")[, 1:202]
  names(df) <- column_names
  df %>%
    filter(year > starty) %>%
    select(-any_of(c("DISPUTED", "OTHER"))) %>%
    pivot_longer(cols = -year, names_to = "Country", values_to = "value") %>%
    mutate(value = value * 3.664, model = model_name)
}
# Process each model sheet 
blue  <- process_sheet("BLUE", "BLUE")
hc    <- process_sheet("H&C2023", "H&C")
oscar <- process_sheet("OSCAR", "OSCAR")
luce  <- process_sheet("LUCE", "LUCE")

#Combine and arrange
land <- bind_rows(blue, oscar, hc, luce)
land<- arrange(land, year, Country)



###### forest: tree cover loss - GFW (Global Forest Watch) --------------------
# Source: https://www.globalforestwatch.org/dashboards/country/IDN/
# global.xlsx sheet "Country tree cover loss"; use threshold = 30 (% canopy).
gfw_forest <- read_excel("data/raw_historical/global.xlsx",
                         sheet = "Country tree cover loss") |>
  filter(threshold == 30) |>
  select(country, starts_with("tc_loss_ha_")) |>
  pivot_longer(cols = starts_with("tc_loss_ha_"),
               names_to = "year", values_to = "value") |>
  mutate(year = parse_number(year))


###### emissions: ch4 - IEA ---------

file_path <- "data/raw_historical/IEA Methane Emissions 2024.csv"
iea_ch4 <- read.csv(file_path)

iea_ch4 <- iea_ch4 %>%
  filter(segment == "Total") %>%
  select(-segment, -reason, -emissionsRank, -energyRank, -notes)

iea_ch4 <- iea_ch4 %>%
  rename(value = "emissions..kt.")

iea_ch4 <- iea_ch4 %>%
  mutate(baseYear = case_when(
    baseYear == "2019-2021" ~ "2021",
    baseYear == "2022-2024" ~ "2024",
    TRUE ~ baseYear
  )) %>%
  rename(year = baseYear)



###### emissions: ch4 - CLIMATE TRACE ---------
# there is an API (and additional GHGs) available, 
# but currently this uses manually-downloaded data

# Select CH4 as the Emissions Type, then download the Waste section as a CSV and unzip it

ct_waste <- rbind(read.csv("data/raw_historical/solid-waste-disposal_country_emissions_v4_2_0_05_25.csv"),
                  read.csv("data/raw_historical/industrial-wastewater-treatment-and-discharge_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/raw_historical/domestic-wastewater-treatment-and-discharge_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/raw_historical/incineration-and-open-burning-of-waste_country_emissions_v4_2_0_05_2025.csv"),
                  read.csv("data/raw_historical/biological-treatment-of-solid-waste-and-biogenic_country_emissions_v4_2_0_05_2025.csv"))

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
ember  <- read.csv("data/raw_historical/release_generation_yearly_global_08_2026.csv")
emberm <- read.csv("data/raw_historical/release_generation_monthly_global_08_2026.csv")
#create output in convenient cap (capacity), geny and genm (generation) formats (to be used in VRE-Eff script)

ember_prep <- function(x) {
  x |>
    mutate(ISO.3.code = case_when(Area == "World" ~ "World", .default = ISO.3.code),
           Area.type  = case_when(Area == "World" ~ "Country or economy",
                                  .default = Area.type)) |>
    filter(Area.type == "Country or economy")
}

# Capacity is published on the "Total generation" row now, so the old
# pivot_wider() -> mutate(Total = Clean + Fossil) -> pivot_longer(seq(6,21))
# round-trip is no longer needed. Checked against 5,504 area-years: the
# published total equals Clean + Fossil exactly.
ecap <- ember_prep(ember) |>
  filter(!is.na(Capacity..GW.)) |>
  select(Area, ISO.3.code, Year, Electricity.source, Capacity..GW.) |>
  rename(region = Area, iso = ISO.3.code, year = Year,
         fuel = Electricity.source, value = Capacity..GW.) |>
  mutate(unit = "GW", variable = "Capacity",
         fuel = case_when(fuel == "Total generation" ~ "Total", .default = fuel))

edem <- ember_prep(ember) |>
  filter(Electricity.source == "Demand", !is.na(Generation..TWh.)) |>
  select(Area, ISO.3.code, Year, Electricity.source, Generation..TWh.) |>
  rename(region = Area, iso = ISO.3.code, year = Year,
         fuel = Electricity.source, value = Generation..TWh.) |>
  mutate(unit = "TWh", variable = "Electricity demand")

enet <- ember_prep(ember) |>
  filter(Electricity.source == "Net imports", !is.na(Generation..TWh.)) |>
  select(Area, ISO.3.code, Year, Electricity.source, Generation..TWh.) |>
  rename(region = Area, iso = ISO.3.code, year = Year,
         fuel = Electricity.source, value = Generation..TWh.) |>
  mutate(unit = "TWh", variable = "Net imports")


egeny <- ember_prep(ember) |>
  filter(!is.na(Generation..TWh.)) |>
  select(Area, ISO.3.code, Year, Electricity.source, Generation..TWh.) |>
  rename(region = Area, iso = ISO.3.code, year = Year,
         fuel = Electricity.source, value = Generation..TWh.) |>
  mutate(unit = "TWh", variable = "Electricity generation",
         fuel = case_when(fuel == "Total generation" ~ "Total", .default = fuel))

egenm <- ember_prep(emberm) |>
  filter(!is.na(Generation..TWh.)) |>
  select(Area, ISO.3.code, Date, Electricity.source, Generation..TWh.) |>
  rename(region = Area, iso = ISO.3.code, year = Date,
         fuel = Electricity.source, value = Generation..TWh.) |>
  mutate(year = as.double(gsub("-", ".", substr(as.Date.character(year), 1, 7))),
         unit = "TWh", variable = "Electricity generation",
         fuel = case_when(fuel == "Total generation" ~ "Total", .default = fuel))




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
eemi <- ember_prep(ember) |>
  filter(!is.na(Emissions..MtCO2e.)) |>
  select(Area, ISO.3.code, Year, Electricity.source, Emissions..MtCO2e.) |>
  rename(region = Area, iso = ISO.3.code, year = Year,
         fuel = Electricity.source, value = Emissions..MtCO2e.) |>
  mutate(unit = "mtCO2", variable = "Power sector emissions",
         fuel = case_when(fuel == "Total generation" ~ "Total", .default = fuel)) |>
  filter(fuel %in% c("Total", "Gas", "Coal", "Fossil"))

eemi_eu <- ember |>
  filter(Area == "EU", !is.na(Emissions..MtCO2e.)) |>
  select(Year, Electricity.source, Emissions..MtCO2e.) |>
  rename(year = Year, fuel = Electricity.source, value = Emissions..MtCO2e.) |>
  mutate(region = "EU27", iso = "EU27BX",
         unit = "mtCO2", variable = "Power sector emissions",
         fuel = case_when(fuel == "Total generation" ~ "Total", .default = fuel)) |>
  filter(fuel %in% c("Total", "Gas", "Coal", "Fossil"))



###### energy: other - EI SRWED --------------------------------------------------
# Energy Institute's Statistical Review of World Energy Data
# https://www.energyinst.org/statistical-review/resources-and-data-downloads
# This link corresponds to multiple reports from the Energy Institute. Our analysis draws two files 
# - Key reports - Statistical Review of World Energy Data.XLSX
# Consolidated Dataset Narrow format downloaded as - Statistical Review of World Energy Narrow File.csv
# 2026-06-30 version (75th edition): includes 2025 data points.
# Note: filename uses "Narrow format" (earlier vintages used "Narrow File").
ener_2026 <- read.csv("data/raw_historical/Statistical Review of World Energy Narrow format_06_26.csv") |>
  rename(region=Country,year=Year,iso=ISO3166_alpha3,value=Value) |>
  select(region,year,iso,Var,value)

#check USA implied net oil trade to compare with sheet "Oil - Trade movements" to 
# see which GJ per barrel value leads to reproduction of pattern (net exporter in 2020 and 2022)
ener_2026|>filter(iso=="USA",Var %in% c("oil_tes_ej","oilprod_kbd"))|>pivot_wider(names_from = Var)|>
  mutate(exp=oilprod_kbd*5.8*365/1000000-oil_tes_ej) |> filter(year>2010)
# implies that 5.8 GJ per barrel of oil is a good value. On the other hand, file:///C:/Users/bertram/Downloads/Approximate%20conversion%20factors%20-%20tables.pdf
# says that 1 boe is 6.118 GJ, so using an average value of 6 on average seems good approach
# difference probably due to accounting for products vs. crude (see tables in file.)

#check gas trade: this works well, see sheet "Gas - Inter-regional trade"
ener_2026|>filter(iso=="USA",Var %in% c("gas_tes_ej","gasprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=gasprod_ej-gas_tes_ej) |> filter(year>2009)

#check coal trade: this works ok, see sheet "Coal - Trade movements"
ener_2026|>filter(iso=="USA",Var %in% c("coal_tes_ej","coalprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=coalprod_ej-coal_tes_ej) |> filter(year>2009)

#check coal trade: this works ok, see sheet "Coal - Trade movements"
ener_2026|>filter(iso=="IND",Var %in% c("coal_tes_ej","coalprod_ej"))|>pivot_wider(names_from = Var)|>
  mutate(exp=coalprod_ej-coal_tes_ej) |> filter(year>2009)


###### energy: EI SRWED gross trade ---------------------
# (Key Reports workbook) 
# The consolidated narrow-format file carries no trade variables at all.
# Gross exports exist only in the Key Reports workbook, and only for aggregate
# regions plus a few key countries. We take the World totals (= "sum of all
# exports", which is what is usually wanted) and, where a row label maps
# unambiguously to an ISO3 country, that country too.

ei_xlsx <- "data/raw_historical/Statistical Review of World Energy Data_06_26.xlsx"

# Row labels in the workbook -> ISO3. Regional/composite rows are deliberately
# omitted: they overlap and cannot be aggregated safely.
ei_trade_iso <- tribble(
  ~sheet_fuel, ~label,             ~iso,
  "oil",       "Total World",      "WLD",
  "oil",       "Canada",           "CAN",
  "oil",       "Mexico",           "MEX",
  "oil",       "US",               "USA",
  "oil",       "Russia",           "RUS",
  "oil",       "Saudi Arabia",     "SAU",
  "gas",       "World",            "WLD",
  "gas",       "US",               "USA",
  "gas",       "Brazil",           "BRA",
  "gas",       "Russian Federation","RUS",
  "gas",       "China",            "CHN",
  "gas",       "India",            "IND",
  "coal",      "Total World",      "WLD",
  "coal",      "Canada",           "CAN",
  "coal",      "US",               "USA",
  "coal",      "Colombia",         "COL",
  "coal",      "Russia",           "RUS",
  "coal",      "South Africa",     "ZAF",
  "coal",      "Australia",        "AUS",
  "coal",      "China",            "CHN",
  "coal",      "Indonesia",        "IDN",
  "coal",      "Mongolia",         "MNG"
)

#' Read one EI trade sheet into long form.
#'
#' The sheets are presentation tables, not data tables: a title row, a header
#' row of years followed by two or three growth/share columns that repeat the
#' final year, then blocks of rows under section headings, then footnotes.
#' We keep only the strictly increasing run of year columns, and only the rows
#' inside the requested section.
read_ei_trade_sheet <- function(path, sheet, hdr_row = 3, section = NULL,
                                total_label = NULL) {
  
  raw <- suppressMessages(
    readxl::read_xlsx(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  )
  
  yr <- suppressWarnings(as.numeric(unlist(raw[hdr_row, ])))
  ok <- which(!is.na(yr) & yr >= 1900 & yr <= 2100)
  if (length(ok) == 0) stop("read_ei_trade_sheet(): no year header found in '", sheet, "'")
  # drop the trailing growth/share columns, which repeat the last year
  ok <- ok[c(TRUE, diff(yr[ok]) > 0)]
  years <- yr[ok]
  
  raw_label <- as.character(raw[[1]])
  lab <- trimws(gsub("[0-9*\u2020^\u2666]+$", "", raw_label))
  
  # restrict to the export section, if the sheet has one
  rows <- seq_len(nrow(raw))
  if (!is.null(section)) {
    start <- which(lab == section)
    if (length(start) == 0) stop("read_ei_trade_sheet(): section '", section,
                                 "' not found in '", sheet, "'")
    rows <- rows[rows > start[1]]
  }
  rows <- rows[!is.na(lab[rows]) & lab[rows] != ""]
  
  # NOTE: keep value-less rows here. On the gas sheet the region headings carry
  # no data of their own, and dropping them at this stage loses the hierarchy.
  lapply(rows, function(i) {
    tibble(row = i, label = lab[i], raw_label = raw_label[i],
           year = years, value = suppressWarnings(as.numeric(unlist(raw[i, ok]))))
  }) |> bind_rows() -> out
  
  if (!is.null(total_label) && !any(out$label == total_label))
    stop("read_ei_trade_sheet(): '", total_label, "' not found in '", sheet, "'")
  
  out
}

# --- oil: thousand barrels daily, exports section, 1980
ei_trade_oil <- read_ei_trade_sheet(ei_xlsx, "Oil trade movements",
                                    section = "Exports", total_label = "Total World") |>
  filter(!is.na(value)) |>
  mutate(sheet_fuel = "oil", value = value * kbd2ej)

# --- coal: exajoules, exports section, 2000
ei_trade_coal <- read_ei_trade_sheet(ei_xlsx, "Coal - Trade movements",
                                     section = "Exports", total_label = "Total World") |>
  filter(!is.na(value)) |>
  mutate(sheet_fuel = "coal")

# --- gas: bcm, one block per region, 2000
# Gas has no single "Exports" section; each region block carries its own
# "Total exports" row, and the World block a "Total trade" row. Attribute each
# total to the region heading above it.
ei_trade_gas_raw <- read_ei_trade_sheet(ei_xlsx, "Gas - Trade movements")

# Each region block carries its own "Total exports" row; the World block a
# "Total trade" row. Region headings sit flush left, while "of which:" detail
# rows are indented -- indentation is what separates the two, since some detail
# rows repeat a region name ("of which: Russian Federation").
gas_subrows <- c("Pipeline imports", "LNG imports", "Total imports",
                 "Pipeline exports", "LNG exports", "Total exports",
                 "Inter-regional pipeline trade", "LNG trade", "Total trade")

ei_trade_gas <- ei_trade_gas_raw |>
  mutate(block = if_else(grepl("^\\s", raw_label) | label %in% gas_subrows,
                         NA_character_, label)) |>
  arrange(row) |>
  group_by(year) |>
  fill(block, .direction = "down") |>
  ungroup() |>
  filter(label %in% c("Total exports", "Total trade"), !is.na(value)) |>
  transmute(label = block, year, value = value / bcm2ej, sheet_fuel = "gas")

ei_trade_gross <- bind_rows(ei_trade_oil, ei_trade_coal, ei_trade_gas) |>
  inner_join(ei_trade_iso, by = join_by(sheet_fuel, label)) |>
  select(sheet_fuel, iso, year, value)

# quick check: the World rows must be there for all three fuels
stopifnot(all(c("oil", "gas", "coal") %in%
                unique(ei_trade_gross$sheet_fuel[ei_trade_gross$iso == "WLD"])))


###### energy: IEA WEO 2025 --------------------------------------------------

# Region - WEO2025_AnnexA_Free_Dataset_Regions.csv
# World - WEO2025_AnnexA_Free_Dataset_World.csv
iea25 <- read.csv("data/raw_historical/WEO2025_AnnexA_Free_Dataset_Regions.csv")|>
  mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW))
iea25 <- rbind(iea25, ## add all available data on Net Zero scenarios, plus additional variables for others
               read.csv("data/raw_historical/WEO2025_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO=="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW)),#|>select(-X),
               read.csv("data/raw_historical/WEO2025_AnnexA_Free_Dataset_World.csv") |> filter(SCENARIO!="Net Zero Emissions by 2050 Scenario")|>
                 mutate(var = paste0(CATEGORY,"-",PRODUCT,"-",FLOW)) |>
                 filter(!paste0(var, "\r", UNIT) %in% unique(paste0(iea25$var, "\r", iea25$UNIT)))#|>select(-X)
)|>
  select(-CATEGORY,-PRODUCT,-FLOW,-PUBLICATION)|>
  rename(unit=UNIT,region=REGION,year=YEAR,value=VALUE,scenario=SCENARIO)



###### energy: OWID ---------

# In ReadMe: Download our complete Energy dataset : CSV
owid_energy_data <- read_csv("data/raw_historical/owid-energy-data.csv")

# Pivot the CO2 dataset
owid_energy_data <- owid_energy_data %>%
  pivot_longer(
    cols = population:last_col(), 
    names_to = "variable", 
    values_to = "value"
  )


###### trn: ev - IEA GEVO --------------------------------------------------

iea_ev <- readxl::read_excel("data/raw_historical/GlobalEVDataExplorer2026.xlsx")
#This dataset had added a few new aggregate regions: Asia Pacific, Central and South America, EU27, Europe, Middle East and Caspian, North America, Rest of the world, World
# Asia pacific+Central and South America+ Europe+ Middle East and Caspian+North America+Rest of the world = World(roughly)
# Rename and drop columns to match expected structure
iea_ev <- iea_ev %>%
  rename(region = region_country) %>%
  select(-`Aggregate group`)
###### trn: ev - Robbie --------------------------------------------------

robbie_ev <- read.csv("data/raw_historical/all_carsales_monthly_05_2025.csv") |>
  mutate(variable = "all_carsales_monthly",
         Value = Value * 10^(-6),
         unit = "million")

###### trn: service - OECD --------------------------------------------------
# tkt (ton-kilometer traveled) and pkt (passenger-kilometer traveled)
#https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CTransport%23TRA%23&pg=0&fc=Topic&bp=true&snb=22&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TRENDS%40DF_TRENDSFREIGHT&df[ag]=OECD.ITF&df[vs]=1.0&dq=.A.....&to[TIME_PERIOD]=false&pd=%2C
#https://data-explorer.oecd.org/vis?lc=en&fs[0]=Topic%2C0%7CTransport%23TRA%23&pg=0&fc=Topic&bp=true&snb=22&df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_TRENDS%40DF_TRENDSPASS&df[ag]=OECD.ITF&df[vs]=1.0&dq=.A.....&to[TIME_PERIOD]=false&pd=%2C
# Click on the "Download" button > "Unfiltered data in tabular dataset (CSV)
OECD_f <- read.csv("data/raw_historical/OECD.ITF,DSD_TRENDS@DF_TRENDSFREIGHT,1.0+all.csv")
OECD_p <- read.csv("data/raw_historical/OECD.ITF,DSD_TRENDS@DF_TRENDSPASS,1.0+all.csv")
OECD <- bind_rows(OECD_f, OECD_p)

##### trn: service - OWID ---------------------------------------------------
##### Source: https://ourworldindata.org/grapher/air-passenger-kilometers.csv?v=1&csvType=full&useColumnShortNames=true
owid_air <- read.csv("data/raw_historical/air-passenger-kilometers.csv")

###### climate: temp - NASA --------------------------------------------------

nasa_temp <- read.csv("data/raw_historical/GLB.Ts+dSST_05_2025.csv",skip = 1)

###### climate: temp - CRU --------------------------------------------------

# Read the data from the file
file_path <- "data/raw_historical/HadCRUT5.0Analysis_gl.txt"

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



###### socio: gdp, population - IIASA ---------

file_path <- "data/raw_historical/1710759470883-ssp_basic_drivers_release_3.0.1_full.xlsx"

# Read the data sheet
iiasa_data <- read_excel(file_path, sheet = "data")


iiasa_data <- iiasa_data %>%
  filter(Variable %in% c("GDP|PPP", "Population")) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "year", values_to = "value") %>%
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
dat_prim <- prim |>
  rename(category = category..IPCC2006_PRIMAP.,
         scen_prim = scenario..PRIMAP.hist.)

dat_prim <- dat_prim |> filter(entity %in% c("KYOTOGHG (AR4GWP100)","KYOTOGHG (AR5GWP100)","CO2","CH4","N2O","FGASES (AR4GWP100)","FGASES (AR5GWP100)"),
                           year > starty,category %in% c("0", "M.0.EL","M.LULUCF"),
                           scen_prim %in% c("HISTCR","HISTTP")) |> select(-provenance,-source) |>
  mutate(unit = case_when(
    entity=="CH4" ~ "Mt CH4/yr",
    entity=="N2O" ~ "kt N2O/yr",
    entity=="CO2" ~ "Mt CO2/yr",
    .default="Mt CO2-equiv/yr"
  )) |>
  mutate(value = case_when(
    entity=="N2O" ~ value,
    .default=value/1000
  ))

dat_prim <- dat_prim |>
  mutate(entity = case_when(
    #define emission categories: 
    #country-reporting (HISTCR)
    entity=="KYOTOGHG (AR4GWP100)" & category=="0" & scen_prim =="HISTCR" ~ "Emissions|Kyoto Gases (incl. all LULUCF)",
    entity=="KYOTOGHG (AR4GWP100)" & category=="M.0.EL" & scen_prim =="HISTCR"~ "Emissions|Kyoto Gases (excl. LUC)",
    entity=="KYOTOGHG (AR5GWP100)" & category=="0" & scen_prim =="HISTCR"~ "Emissions|Kyoto Gases|AR5 (incl. all LULUCF)",
    entity=="KYOTOGHG (AR5GWP100)" & category=="M.0.EL" & scen_prim =="HISTCR" ~ "Emissions|Kyoto Gases|AR5 (excl. LUC)",
    entity=="CO2" & category=="0" & scen_prim =="HISTCR"~ "Emissions|CO2 (incl. all LULUCF)",
    entity=="CO2" & category=="M.0.EL" & scen_prim =="HISTCR"~ "Emissions|CO2|Energy and Industrial Processes",
    entity=="CO2" & category=="M.LULUCF" & scen_prim =="HISTCR"~ "Emissions|CO2|AFOLU",
    entity=="CH4" & category=="M.0.EL" & scen_prim =="HISTCR"~ "Emissions|CH4",
    entity=="N2O" & category=="M.0.EL" & scen_prim =="HISTCR"~ "Emissions|N2O",
    entity=="FGASES (AR4GWP100)" & category=="M.0.EL" & scen_prim =="HISTCR"~ "Emissions|F-Gases",
    entity=="FGASES (AR5GWP100)" & category=="M.0.EL" & scen_prim =="HISTCR" ~ "Emissions|F-Gases|AR5",
    # Add in some Third-Party reported results as well
    entity=="KYOTOGHG (AR4GWP100)" & category=="0" & scen_prim =="HISTTP" ~ "Emissions|Kyoto Gases (incl. all LULUCF)",
    entity=="KYOTOGHG (AR4GWP100)" & category=="M.0.EL" & scen_prim =="HISTTP"~ "Emissions|Kyoto Gases (excl. LUC)",
    entity=="KYOTOGHG (AR5GWP100)" & category=="0" & scen_prim =="HISTTP"~ "Emissions|Kyoto Gases|AR5 (incl. all LULUCF)",
    entity=="KYOTOGHG (AR5GWP100)" & category=="M.0.EL" & scen_prim =="HISTTP" ~ "Emissions|Kyoto Gases|AR5 (excl. LUC)",
    entity=="CO2" & category=="0" & scen_prim =="HISTTP"~ "Emissions|CO2 (incl. all LULUCF)",
    entity=="CO2" & category=="M.0.EL" & scen_prim =="HISTTP"~ "Emissions|CO2|Energy and Industrial Processes",
    entity=="CO2" & category=="M.LULUCF" & scen_prim =="HISTTP"~ "Emissions|CO2|AFOLU"
  ))|> 
  # Show that AFOLU value in Primap-Hist are actually just the LULUCF (Land) values (missing some agriculture emissions)
  bind_rows(dat_prim |>
              filter(entity=="CO2" & category=="M.LULUCF") |>
              mutate(entity = "Emissions|CO2|AFOLU|Land")
  ) |>
  filter(!is.na(entity))|>
  mutate(scen_prim = case_when(
    scen_prim =="HISTCR" ~ "PRIMAP-hist",
    scen_prim =="HISTTP" ~ "PRIMAP-hist [TP]"
  )) |>
  select(-category)|>
  mutate(scenario="historical")|>
  rename(variable=entity,iso=region, model =scen_prim) |>
  #rename EARTH to World
  mutate(iso = ifelse(iso == "EARTH", "World", iso)) |>
  #filter out country groups (only keep EU27BX and World)
  filter(!iso %in% c("LDC","AOSIS","ANNEXI","BASIC","NONANNEXI","UMBRELLA"))

# Drop 2024/2025 values for selected variables (data quality issues - extrapolated/incomplete in PRIMAP v2.7)
dat_prim <- dat_prim |>
  filter(!(year %in% c(2024, 2025) &
           variable %in% c("Emissions|Kyoto Gases (incl. all LULUCF)",
                           "Emissions|Kyoto Gases (excl. LUC)",
                           "Emissions|Kyoto Gases|AR5 (incl. all LULUCF)",
                           "Emissions|Kyoto Gases|AR5 (excl. LUC)",
                           "Emissions|CH4",
                           "Emissions|N2O",
                           "Emissions|F-Gases",
                           "Emissions|F-Gases|AR5",
                           "Emissions|CO2|Energy and Industrial Processes",
                           "Emissions|CO2 (incl. all LULUCF)",
                           "Emissions|CO2|AFOLU",
                           "Emissions|CO2|AFOLU|Land")))


#total CO2 and total Kyoto also get calculated with the LULUCF to get to a consistent set with / without indirect LULUCF (Grassi effect)
dat_prim <-  rbind(dat_prim,
                  dat_prim|>select(-unit) |> filter(variable %in% c("Emissions|CO2|AFOLU|Land","Emissions|CO2|Energy and Industrial Processes","Emissions|Kyoto Gases (excl. LUC)"))|>
                    pivot_wider(names_from=variable,values_fill = 0)|>mutate(
                      `Emissions|CO2`=`Emissions|CO2|AFOLU|Land`+`Emissions|CO2|Energy and Industrial Processes`,
                      `Emissions|Kyoto Gases`=`Emissions|CO2|AFOLU|Land`+`Emissions|Kyoto Gases (excl. LUC)`
                    ) |> pivot_longer(cols=c(-iso,-year,-model,-scenario),names_to = 'variable')|>
                    filter(variable %in% c("Emissions|CO2","Emissions|Kyoto Gases"))|>
                    mutate(unit = case_when(
                      variable=="Emissions|Kyoto Gases" ~ "Mt CO2-equiv/yr",
                      variable=="Emissions|CO2" ~ "Mt CO2/yr"
                    )))

# Drop 2024/2025 for the derived totals as well — pivot_wider(values_fill = 0) above
# silently substitutes 0 for the rows we filtered earlier, producing underestimated totals.
dat_prim <- dat_prim |>
  filter(!(year %in% c(2024, 2025) &
           variable %in% c("Emissions|CO2", "Emissions|Kyoto Gases")))

#addition of international shipping and aviation emissions to dat_prim further below
#search for:
# dat_prim <- rbind(dat_prim,
                # dat_ceds_int...)


###### CEDS ##############

# Process the CEDS total emission values 
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
  map_ceds <- read.csv("mappings/ceds_sector_mapping_China.csv") %>% gather_map() 
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

dat_ceds <- dat_ceds |> rbind(ceds_c |> left_join(map_ceds, by = c("sector"), relationship = "many-to-many")|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CO2/yr",value=value/1000)|> rename(variable=var))  

dat_ceds <- dat_ceds |> rbind(ceds_m |> left_join(map_ceds, by = c("sector"), relationship = "many-to-many")|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt CH4/yr",value=value/1000)|> rename(variable=var)) 

dat_ceds <- dat_ceds |> rbind(ceds_n |> left_join(map_ceds, by = c("sector"), relationship = "many-to-many")|> 
                                group_by(iso,entity,year,var,units) |> summarize(value=sum(value))|>
                                ungroup() |> mutate(var = paste0("Emissions|",entity,"|",var))|>
                                select(-entity,-units)|>mutate(model="ceds",scenario="historical",
                                                               unit="Mt N2O/yr",value=value/1000)|> rename(variable=var))  


#add sectoral data based on IAMC2 mappings

# dat_ceds <- dat_ceds |> rbind(ceds_c |> left_join(map_ceds |> select(sector,IAMC2))|> 
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
                                                     "Emissions|CO2|AFOLU","Emissions|CO2|AFOLU|Land")),
                  #World data that needs adjustment to include int. aviation and shipping 
                  rbind(dat_prim |> filter(iso=="World",
                                     !variable %in% c("Emissions|F-Gases","Emissions|F-Gases|AR5",
                                                                        "Emissions|CO2|AFOLU","Emissions|CO2|AFOLU|Land"))|>
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



###### GFW Forest ------------------------------------
dat_forest <- gfw_forest |>
  mutate(iso = countrycode(country, "country.name", "iso3c")) |>
  filter(!is.na(iso), year > starty) |>
  mutate(value    = value / 1e6,        # ha -> million ha
         variable = "Forest Area Change|Deforestation",
         unit     = "million ha/yr",
         model    = "GFW",
         scenario = "historical") |>
  select(iso, variable, unit, year, value, model, scenario)

# add World total (sum across mapped countries)
dat_forest <- dat_forest |>
  bind_rows(dat_forest |>
              group_by(year, variable, unit, model, scenario) |>
              summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
              mutate(iso = "World"))



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
# Convert from kt to Mt and update unit
dat_ch4 <- dat_ch4 %>%
  mutate(value = value / 1000,
         unit = "Mt CH4/yr")


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
    fuel=="Other fossil" ~ "Capacity|Electricity|Other Fossil",
    fuel=="Other renewables" ~ "Capacity|Electricity|Geothermal",
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
    fuel=="Other fossil" ~ "Secondary Energy|Electricity|Other Fossil",
    fuel=="Other renewables" ~ "Secondary Energy|Electricity|Geothermal",
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

# Ember reports net imports positive = net importer; IAMC Trade variables are
# positive = net exporter, so the sign is flipped here.
dat_enet <- enet |> filter(year > starty) |> select(-variable, -region, -fuel) |>
  mutate(variable = "Trade|Secondary Energy|Electricity [Volume]",
         value = -value / ej2twh,
         unit = "EJ/yr",
         model = "EMBER", scenario = "historical")

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

dat_ener_2026 <- ener_2026 |> left_join(read.csv("mappings/map_ei_26_iamc.csv"),by = join_by(Var==EI)) |>
  filter(!is.na(IAMC),!IAMC=="",year>starty)|>mutate(value=value*factor) |>
  select(year,iso,value,unit,IAMC) |>
  mutate(iso = ifelse(iso == "WLD", "World", iso),
         model="Stat. Rev. World Energy Data_2026",   ## Statistical Review of World Energy Data"
         scenario="historical") |> rename(variable=IAMC)

###### EI SRWED: fossil trade -------------------------------------------------

# (A) NET trade, derived as production - total energy supply, for every country
#     in the narrow file. This is the comprehensive route: it covers ~57 (oil),
#     ~63 (gas) and ~50 (coal) reporters, against the handful in the workbook.
#
#     Caveats worth keeping in mind:
#      - Net, not gross. A country that both imports and exports nets out.
#      - Stock changes, statistical differences and (for oil) refinery gains
#        are absorbed into the residual.
#      - Production and supply are not measured on the same calorific basis, so
#        the world total does not close exactly. See calibration below.

ei_calibrate_trade <- FALSE   # historical annual data has genuine imbalances from
# inventory changes; set TRUE to force World net trade to zero

ener_wide <- ener_2026 |>
  filter(Var %in% c("oilprod_kbd", "oil_tes_ej",
                    "gasprod_ej",  "gas_tes_ej",
                    "coalprod_ej", "coal_tes_ej",
                    "biofuels_prod_pj", "biofuels_tes_pj")) |>
  select(iso, year, Var, value) |>
  pivot_wider(names_from = Var, values_from = value) |>
  mutate(oilprod_ej  = oilprod_kbd * kbd2ej,
         biofuels_prod_ej = biofuels_prod_pj / 1000,
         biofuels_tes_ej  = biofuels_tes_pj  / 1000)

# Global calibration. EI reports production and supply on slightly different
# calorific bases, so summing (prod - TES) over the world leaves a residual:
# about +7% of supply for oil (the 6 GJ/bbl factor is high against a products
# basis; the balance implies ~5.6), +4% for coal in recent years, <1% for gas.
# Scaling production to the world supply total each year removes that bias and
# makes net trade sum to zero globally, as the IAMC definition expects.
ei_calib <- ener_wide |>
  filter(iso == "WLD") |>
  transmute(year,
            f_oil  = oil_tes_ej  / oilprod_ej,
            f_gas  = gas_tes_ej  / gasprod_ej,
            f_coal = coal_tes_ej / coalprod_ej,
            f_biof = biofuels_tes_ej / biofuels_prod_ej)

if (!ei_calibrate_trade) {
  ei_calib <- ei_calib |> mutate(across(starts_with("f_"), ~ 1))
}

dat_ei_trade_net <- ener_wide |>
  left_join(ei_calib, by = "year") |>
  mutate(across(starts_with("f_"), ~ replace_na(.x, 1))) |>
  transmute(
    iso, year,
    `Trade|Primary Energy|Oil [Volume]`             = oilprod_ej      * f_oil  - oil_tes_ej,
    `Trade|Primary Energy|Gas [Volume]`             = gasprod_ej      * f_gas  - gas_tes_ej,
    `Trade|Primary Energy|Coal [Volume]`            = coalprod_ej     * f_coal - coal_tes_ej,
    `Trade|Secondary Energy|Liquids|Biomass [Volume]` =
      biofuels_prod_ej * f_biof - biofuels_tes_ej
  ) |>
  mutate(`Trade|Primary Energy|Fossil [Volume]` =
           rowSums(across(c(`Trade|Primary Energy|Oil [Volume]`,
                            `Trade|Primary Energy|Gas [Volume]`,
                            `Trade|Primary Energy|Coal [Volume]`)), na.rm = TRUE) *
           # only report the aggregate where at least one component exists
           if_else(rowSums(!is.na(across(c(`Trade|Primary Energy|Oil [Volume]`,
                                           `Trade|Primary Energy|Gas [Volume]`,
                                           `Trade|Primary Energy|Coal [Volume]`)))) > 0,
                   1, NA_real_)) |>
  pivot_longer(cols = -c(iso, year), names_to = "variable", values_to = "value") |>
  filter(!is.na(value))

# (B) GROSS exports from the Key Reports workbook
dat_ei_trade_gross <- ei_trade_gross |>
  mutate(variable = paste0("Trade|Primary Energy|",
                           recode(sheet_fuel, oil = "Oil", gas = "Gas", coal = "Coal"),
                           "|Gross Exports [Volume]")) |>
  select(iso, year, variable, value)

dat_ener_2026_trade <- bind_rows(dat_ei_trade_net, dat_ei_trade_gross) |>
  filter(year > starty) |>
  mutate(iso   = if_else(iso == "WLD", "World", iso),
         unit  = "EJ/yr",
         model = "Stat. Rev. World Energy Data_2026",
         scenario = "historical") |>
  select(year, iso, value, unit, variable, model, scenario)


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
read_gevo <- function(path, model_name) {
  read_excel(path) %>%
  rename(region = region_country) %>%
  select(-`Aggregate group`) %>%
  mutate(
    iso = countrycode(region, "country.name", "iso3c"),
    iso = case_when(region == "World" ~ "World",
                    region %in% c("EU27", "European Union") ~ "EU27BX",
                    TRUE ~ iso),
    mode_group = case_when(
      mode %in% c("Cars", "Vans", "2 and 3 wheelers") ~ "Light-Duty Vehicle",
      mode == "Buses" ~ "Bus",
      mode == "Trucks" ~ "Truck",
      TRUE ~ NA_character_
    ),
    sub_mode = case_when(
      mode == "Cars" ~ "Car",
      mode == "Vans" ~ "Van",
      mode == "2 and 3 wheelers" ~ "Two-Wheeler",
      TRUE ~ "" 
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
    model = model_name
  ) %>%
  filter(!is.na(iso), !is.na(mode_group))
}

derive_gevo <- function(iea_ev, model_name) {

# === EV STOCKS ===
ev_stock <- iea_ev %>%
  filter(parameter == "EV stock", powertrain %in% c("BEV", "PHEV", "FCEV")) %>%
  mutate(variable = paste0(
    "Stocks|Transportation|", mode_group,
    ifelse(sub_mode != "", paste0("|", sub_mode), ""),
    "|", case_when(
      powertrain == "BEV" ~ "Battery-Electric",
      powertrain == "PHEV" ~ "Plug-in Hybrid",
      powertrain == "FCEV" ~ "Fuel-Cell-Electric"
    )
  )) %>%
  group_by(iso, variable, unit, year, model, scenario, mode_group, sub_mode) %>%
  summarise(value = sum(value), .groups = "drop")

# === EV SALES ===
ev_sales <- iea_ev %>%
  filter(parameter == "EV sales", powertrain %in% c("BEV", "PHEV", "FCEV")) %>%
  mutate(variable = paste0(
    "Sales|Transportation|", mode_group,
    ifelse(sub_mode != "", paste0("|", sub_mode), ""),
    "|", case_when(
      powertrain == "BEV" ~ "Battery-Electric",
      powertrain == "PHEV" ~ "Plug-in Hybrid",
      powertrain == "FCEV" ~ "Fuel-Cell-Electric"
    )
  )) %>%
  group_by(iso, variable, unit, year, model, scenario, mode_group, sub_mode) %>%
  summarise(value = sum(value), .groups = "drop")


# === EV SALES SHARE (used for total calc) ===
ev_sales_share <- iea_ev %>%
  filter(parameter == "EV sales share") %>%
  group_by(iso, year, scenario, mode_group, sub_mode) %>%
  summarise(ev_share = mean(value, na.rm = TRUE), .groups = "drop")

# === EV STOCK SHARE (used for total calc) ===
ev_stock_share <- iea_ev %>%
  filter(parameter == "EV stock share") %>%
  group_by(iso, year, scenario, mode_group, sub_mode) %>%
  summarise(ev_share = mean(value, na.rm = TRUE), .groups = "drop")

# === TOTAL STOCK ===
ev_stock_total <- ev_stock %>%
  group_by(iso, year, scenario, mode_group, sub_mode) %>%
  summarise(ev_stock = sum(value), .groups = "drop")

stock_total <- left_join(ev_stock_total, ev_stock_share,
                         by = c("iso", "year", "scenario", "mode_group", "sub_mode")) %>%
  filter(!is.na(ev_share), ev_share > 0) %>%
  mutate(
    value = ev_stock / (ev_share / 100),
    variable = paste0("Stocks|Transportation|", mode_group,
                      ifelse(sub_mode != "", paste0("|", sub_mode), "")),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario, mode_group, sub_mode)

# === AGGREGATE TOTAL STOCK BY MODE GROUP (LDV, Bus, Truck) ===
stock_total_agg <- stock_total %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    variable = paste0("Stocks|Transportation|", mode_group),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# === TOTAL SALES ===
ev_sales_total <- ev_sales %>%
  group_by(iso, year, scenario, mode_group, sub_mode) %>%
  summarise(ev_sales = sum(value), .groups = "drop")

sales_total <- left_join(ev_sales_total, ev_sales_share,
                         by = c("iso", "year", "scenario", "mode_group", "sub_mode")) %>%
  filter(!is.na(ev_share), ev_share > 0) %>%
  mutate(
    value = ev_sales / (ev_share / 100),
    variable = paste0("Sales|Transportation|", mode_group,
                      ifelse(sub_mode != "", paste0("|", sub_mode), "")),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario, mode_group, sub_mode)
# === AGGREGATE TOTAL SALES BY MODE GROUP (LDV, Bus, Truck) ===
sales_total_agg <- sales_total %>%
  group_by(iso, year, scenario, mode_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    variable = paste0("Sales|Transportation|", mode_group),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# === ICE STOCK & SALES ===
ice_stock <- left_join(stock_total, ev_stock_total,
                       by = c("iso", "year", "scenario", "mode_group", "sub_mode")) %>%
  mutate(
    value = value - ev_stock,
    variable = paste0(
      "Stocks|Transportation|", mode_group,
      ifelse(sub_mode != "", paste0("|", sub_mode), ""),
      "|Internal Combustion"
    ),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

ice_sales <- left_join(sales_total, ev_sales_total,
                       by = c("iso", "year", "scenario", "mode_group", "sub_mode")) %>%
  mutate(
    value = value - ev_sales,
    variable = paste0(
      "Sales|Transportation|", mode_group,
      ifelse(sub_mode != "", paste0("|", sub_mode), ""),
      "|Internal Combustion"
    ),
    unit = "million",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# === BEV SHARES ===
bev_sales_share <- ev_sales %>%
  filter(grepl("Battery-Electric", variable)) %>%
  group_by(iso, year, scenario, model, mode_group, sub_mode) %>%
  summarise(bev_sales = sum(value), .groups = "drop") %>%
  left_join(
    sales_total %>% rename(total_sales = value) %>%
      select(iso, year, scenario, mode_group, sub_mode, total_sales),
    by = c("iso", "year", "scenario", "mode_group", "sub_mode")
  ) %>%
  mutate(
    value = 100 * bev_sales / total_sales,
    variable = paste0(
      "Sales Share|Transportation|", mode_group,
      ifelse(sub_mode != "", paste0("|", sub_mode), ""),
      "|Battery-Electric"
    ),
    unit = "%",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)


bev_stock_share <- ev_stock %>%
  filter(grepl("Battery-Electric", variable)) %>%
  group_by(iso, year, scenario, model, mode_group, sub_mode) %>%
  summarise(bev_stock = sum(value), .groups = "drop") %>%
  left_join(
    stock_total %>% rename(total_stock = value) %>%
      select(iso, year, scenario, mode_group, sub_mode, total_stock),
    by = c("iso", "year", "scenario", "mode_group", "sub_mode")
  ) %>%
  mutate(
    value = 100 * bev_stock / total_stock,
    variable = paste0(
      "Stock Share|Transportation|", mode_group,
      ifelse(sub_mode != "", paste0("|", sub_mode), ""),
      "|Battery-Electric"
    ),
    unit = "%",
    model = model_name
  ) %>%
  select(iso, variable, unit, year, value, model, scenario)

# === Combine all outputs ===
ev_stock_clean <- ev_stock %>%
  filter(!grepl("^Stocks\\|Transportation\\|[^|]+$", variable))  # only with powertrain

bind_rows(
  ev_sales %>% select(-mode_group, -sub_mode),
  ev_stock_clean %>% select(-mode_group, -sub_mode),
  sales_total %>% select(-mode_group, -sub_mode),
  stock_total %>% select(-mode_group, -sub_mode),
  sales_total_agg,
  stock_total_agg,
  ice_sales,
  ice_stock,
  bev_sales_share,
  bev_stock_share
) %>%
  distinct()
}

iea_ev <- read_gevo("data/raw_historical/GlobalEVDataExplorer2026.xlsx", "IEA_GEVO")
dat_iea_ev <- derive_gevo(iea_ev, "IEA_GEVO")

# Graft the 2030 projection from the previous vintage (GEVO 2025). GEVO 2026 nolonger publishes 2030 
dat_iea_ev_2025 <- read_gevo("data/raw_historical/GlobalEVDataExplorer2025.xlsx", "IEA_GEVO_2025") %>%
  derive_gevo("IEA_GEVO_2025") %>%
  filter(year == 2030, scenario == "Projection-STEPS")

dat_iea_ev <- bind_rows(dat_iea_ev, dat_iea_ev_2025)




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


###### OWID trn - aviation --------------------------------

dat_owid_air <- owid_air %>%
  rename("region" = "Entity", "iso" = "Code", "year" = "Year", "value" = "X9.1.2...Passenger.volume..passenger.kilometres...by.mode.of.transport...IS_RDP_PFVOL...Air.transport") %>%
  mutate(iso = ifelse(iso == "" | is.na(iso), NA, iso)) %>%
  filter(!is.na(value)) %>%
  mutate(variable = "Energy Service|Transportation|Passenger|Aviation",
         unit = "Millions Passenger-kilometres",
         value = value / 1e6) %>%
  filter(!is.na(iso)) %>%
  select(iso, variable, unit, year, value) %>%
  mutate(model = "OWID", scenario = "historical") %>%
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
data_iso <- rbind(dat_ener_2026,dat_ener_2026_trade,dat_prim,dat_ceds,dat_ecap,dat_egeny,dat_enet,dat_egeny_shares, dat_eemi,dat_iea_ev,
              dat_robbie, dat_oecd, dat_owid_air, dat_nasa,dat_land,dat_ch4, iiasa_data ,dat_crut,dat_owid_energy, dat_owid_co2, dat_ct, dat_forest)

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


for (model_regions in c("gcam32_v7", "gcam32_v8", "r10", "r5", "gcamEurope")) {

message("Processing region scheme: ", model_regions)

# Choose region mapping
if (model_regions == "gcam32_v8"){
  reg_map <- read.csv("mappings/iso_EI_GCAM_regID_v8.csv", skip = 6) |>
    left_join(read.csv("mappings/GCAM_region_names_v8.csv", skip = 6)) |>
    mutate(iso = toupper(iso))

  ## check_match(data_iso, reg_map, "iso")

} else if (model_regions == "gcam32_v7"){
  reg_map <- read.csv("mappings/iso_EI_GCAM_regID_v7.csv", skip = 6) |>
    left_join(read.csv("mappings/GCAM_region_names_v7.csv", skip = 6)) |>
    mutate(iso = toupper(iso))

  ## check_match(data_iso, reg_map, "iso")
  
} else if(model_regions == "r5"){
  ## aggregate to R5: five regions making up the world
  reg_map <- read.csv2("mappings/regionmappingR5.csv") |> 
    rename(iso=CountryCode,region=RegionCode)
  
} else if(model_regions == "r10"){ 
  ## aggregate to R10: ten regions making up the world
  reg_map <- read.csv("mappings/iso_r10.csv") 
  
} else if(model_regions == "gcamEurope"){
  reg_map <- read.csv("mappings/iso_gcam_europe.csv", skip = 3)
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

map_iea <- read.csv("mappings/map_IEAWEO25_iamc.csv")

dat_iea25 <- iea25 |>
  mutate(region=case_when(
    region=="United States" ~ "USA",
    .default=region))|>
  filter(region %in% c(unique(reg_map$region), 
                       "Europe", "European Union", # Remove these regions if preferred
                       "World")) |>
  mutate(model="IEA WEO 2025")|>
  filter(!is.na(var))

# bring to GCAM region mapping and IAMC format
map_iea_active <- map_iea |> filter(IAMC != "")

unmatched <- map_iea_active |>
  anti_join(dat_iea25 |> distinct(var, unit), by = join_by(WEO == var, Unit_WEO == unit))
if (nrow(unmatched) > 0) {
  stop("map_IEAWEO25_iamc.csv: ", nrow(unmatched),
       " mapping row(s) match no (var, unit) pair in the WEO data:\n",
       paste0("  ", unmatched$WEO, " [", unmatched$Unit_WEO, "] -> ", unmatched$IAMC,
              collapse = "\n"))
}

dat_iea25 <- dat_iea25 |>
  left_join(map_iea_active, by = join_by(var == WEO, unit == Unit_WEO),
            relationship = "many-to-many") |>
  filter(!is.na(IAMC)) |>
  select(-unit) |>
  rename(unit = Unit_IAMC) |>
  mutate(Conversion = as.numeric(Conversion)) |>
  mutate(value = value*Conversion)|>
  select(year,IAMC,unit,value,region,model,scenario) |>
  na.omit(IAMC) |>
  rename(variable=IAMC)

# Sum across detailed variables
dat_iea25 <- dat_iea25 |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Secondary Energy|Electricity|Solar|PV", 
                                     "Secondary Energy|Electricity|Solar|CSP")) |>
              mutate(variable = "Secondary Energy|Electricity|Solar")) |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Capacity|Electricity|Solar|PV", 
                                     "Capacity|Electricity|Solar|CSP")) |>
              mutate(variable = "Capacity|Electricity|Solar")) |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Capacity|Electricity|Coal|w/ CCS", 
                                     "Capacity|Electricity|Coal|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Coal")) |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Capacity|Electricity|Gas|w/ CCS", 
                                     "Capacity|Electricity|Gas|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Gas")) |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Capacity|Electricity|Fossil|w/ CCS",
                                     "Capacity|Electricity|Fossil|w/o CCS")) |>
              mutate(variable = "Capacity|Electricity|Fossil")) |>
  bind_rows(dat_iea25 |>
              filter(variable %in% c("Primary Energy|Biomass|Solids",
                                     "Primary Energy|Biomass|Liquids",
                                     "Primary Energy|Biomass|Gases",
                                     "Primary Energy|Biomass|Traditional")) |>
              mutate(variable = "Primary Energy|Biomass")) |>
  group_by(across(-value)) |>
  summarise(value = sum(value, na.rm = T)) |>
  ungroup()

#historic data
dat_ieah <- dat_iea25 |> 
  filter(scenario=="Historical") |>
  mutate(scenario = "historical")

#scenario data
dat_ieas <- dat_iea25 |>
  filter(
    scenario == "Current Policies Scenario" |
    scenario == "Stated Policies Scenario"
  )


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
# write.csv(write_this, "data/processed_historical/historical_iso.csv",row.names = F,quote = F)


##### with ISO and World: -----
# (and with all IEA scenarios)

write_this <- rbind(data_iso, dat_ieah, dat_ieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(across(where(is.list), ~ ifelse(lengths(.) == 0, NA, unlist(.)))) |>  # Remove NULL lists
  mutate(across(matches("^[0-9]{4}$"), ~ as.numeric(.))) |>
  unique() |>
  ungroup()


#write out iso and World IAMC file (only once, independent of region scheme)
if (model_regions == "gcam32_v7") {
  write.csv(write_this, "data/processed_historical/historical_iso.csv",row.names = F,quote = F)
}



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
#           file.path("data/processed_historical",paste0("historical_", model_regions, ".csv")),
#           row.names = F,quote = F)


##### with aggregate regions and World: -----
# (and with all IEA scenarios)

write_this <- rbind(data_reg, dat_ieah, dat_ieas, data_World) |>
  group_by(model, scenario, region, variable, value, unit) |>
  arrange(year) |>
  pivot_wider(names_from = year, values_from = value) |>
  mutate(across(matches("^[0-9]{4}$"), ~ as.numeric(.))) |>
  unique() |>
  ungroup()


#write out IAMC file
write.csv(write_this,
          file.path("data/processed_historical",paste0("historical_", model_regions, ".csv")),
          row.names = F,quote = F)

} # end loop over model_regions


#### 4.b save and remove original source R objects ####
if(save_option == T){
  dir.create("data/raw_historical/combined", showWarnings = F)
  
  save(prim, ceds, ceds_c, ceds_m, ceds_n, owid_co2_data, eemi, eemi_eu, land, iea_ch4, ct_waste,
       file = "data/raw_historical/combined/emissions.Rds")
  
  save(ember,emberm,ecap,egeny,egenm,ener_2026,iea_ev, robbie_ev, OECD, owid_air, owid_energy_data, iea25,
       file = "data/raw_historical/combined/energy.Rds")
  
  save(nasa_temp, crut, iiasa_data,
       file = "data/raw_historical/combined/climate_and_socio.Rds")
}

rm(prim, ceds, ceds_c, ceds_m, ceds_n, owid_co2_data, eemi, eemi_eu, land, iea_ch4, ct_waste, ember,emberm,ecap,egeny,egenm,ener_2026,iea_ev, robbie_ev, OECD, owid_air, owid_energy_data, iea25, nasa_temp, crut, iiasa_data)


