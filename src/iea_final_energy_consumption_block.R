## iea_final_energy_consumption_block.R --------------------------------------
## Reads the raw IEA "total final consumption by source" files (Industry /
## Residential / Transport, one file per country) and converts them into
## IAMC-format Final Energy variables, per Mohammed's request in Slack.
##
## UPDATED for the corrected multi-year files (Clara's re-upload, Sep 2026):
##   * Files are now full historical (one row per fuel PER YEAR), not a
##     single-year snapshot. Year is read from the data's Year column.
##   * Filename pattern changed to
##       "... <sector> total final consumption by source in <Country>.csv"
##     (sector is lowercase; year no longer in the filename).
##   * The batch also contains "total final energy consumption in <Country>.csv"
##     files that break down by SECTOR, not by fuel. Those are excluded
##     (they lack "by source" in the name) since Mohammed's 11 variables are
##     fuel breakdowns within a sector.
##
## Insert this block into process_hist_data.R alongside the other raw source
## ingestion blocks, then bind_rows its output into data_iso the same way.

library(dplyr)
library(stringr)
library(countrycode)

#### 1. Config ---------------------------------------------------------------

## CHANGED: point at the corrected multi-year files. Update this path to
## wherever the new batch is finally stored (see note below).
iea_fe_dir <- "data/raw_historical/iea_final_energy"

# TJ -> EJ conversion (1 EJ = 1,000,000 TJ)
tj2ej <- 1e-6

# Map each raw source-category name to its IAMC fuel suffix.
# Uses partial matching (grepl) since the exact wording differs by sector
# (e.g. "Oil and oil products" in Industry files vs "Oil products" elsewhere).
# Categories not matched here (Heat, Biofuels and waste, Solar/wind/renewables,
# Non-energy use, Other non-specified) return NA: they get no line-item
# variable but ARE still included in the sector total in section 4.
map_fuel_category <- function(category, sector) {
  case_when(
    grepl("^Electricity", category, ignore.case = TRUE) ~ "Electricity",
    grepl("Natural gas", category, ignore.case = TRUE)   ~ "Gases",
    grepl("^Coal", category, ignore.case = TRUE)         ~ "Solids|Coal",
    grepl("^Oil", category, ignore.case = TRUE) & sector == "Transport" ~ "Liquids|Oil",
    grepl("^Oil", category, ignore.case = TRUE)          ~ "Liquids",
    TRUE ~ NA_character_
  )
}

# Map each raw sector name (from the filename) to its IAMC sector name.
# CHANGED: new filenames use lowercase sector tokens (industry/residential/
# transport). str_to_title() normalises them so this lookup still works.
map_sector_name <- function(sector) {
  case_when(
    sector == "Industry"    ~ "Industry",
    sector == "Residential" ~ "Residential and Commercial",
    sector == "Transport"   ~ "Transportation",
    TRUE ~ NA_character_
  )
}


#### 2. Read and parse every file ---------------------------------------------

## CHANGED: only take the by-fuel files ("... by source in <Country>.csv").
## This naturally excludes the by-sector "total final energy consumption
## in <Country>.csv" files, which have no "by source" in the name.
iea_fe_files <- list.files(
  iea_fe_dir,
  pattern = "total final consumption by source in .+\\.csv$",
  full.names = TRUE
)

parse_one_file <- function(path) {
  
  fname <- basename(path)
  
  # CHANGED: new filename pattern
  #   "... <sector> total final consumption by source in <Country>.csv"
  # sector is lowercase; there is no year in the filename any more.
  sector_raw <- str_match(fname, "- (\\w+) total final consumption by source")[, 2]
  country    <- str_match(fname, " in (.+)\\.csv$")[, 2]
  
  # Normalise sector to Title case so map_sector_name()/map_fuel_category()
  # (which expect "Industry"/"Transport"/"Residential") keep working.
  sector <- str_to_title(sector_raw)
  
  df <- tryCatch(
    read.csv(path, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # First column holds the fuel-source category name; its header text
  # varies per file (it repeats the sector/country), so grab it by
  # position rather than by name.
  names(df)[1] <- "category"
  
  # Guard: the multi-year files must carry Value and Year columns.
  if (!all(c("Value", "Year") %in% names(df))) {
    warning("Skipping ", fname, ": missing Value/Year column.")
    return(NULL)
  }
  
  df |>
    mutate(
      sector  = sector,
      country = country,
      value   = as.numeric(Value) * tj2ej,          # TJ -> EJ
      year    = as.character(as.numeric(Year))       # year from the DATA now
    ) |>
    filter(!is.na(value), !is.na(year)) |>
    select(category, sector, country, year, value)
}

iea_fe_raw <- iea_fe_files |>
  lapply(parse_one_file) |>
  bind_rows()

message(sprintf("Parsed %d rows from %d files (%d years, %d countries).",
                nrow(iea_fe_raw),
                length(iea_fe_files),
                dplyr::n_distinct(iea_fe_raw$year),
                dplyr::n_distinct(iea_fe_raw$country)))


#### 3. Build the per-fuel variables ------------------------------------------

iea_fe_components <- iea_fe_raw |>
  mutate(
    fuel_suffix = map_fuel_category(category, sector),
    sector_iamc = map_sector_name(sector)
  ) |>
  filter(!is.na(fuel_suffix), !is.na(sector_iamc)) |>
  mutate(variable = paste0("Final Energy|", sector_iamc, "|", fuel_suffix)) |>
  # CHANGED: with multi-year data a (country, year, variable) key can now
  # receive more than one raw category (e.g. "Oil and oil products" and
  # "Oil products" both map to Liquids). Sum them so the key is unique.
  group_by(country, year, variable) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")


#### 4. Build the sector totals ------------------------------------------------
## Total = sum of ALL fuel categories reported for that sector/country/year,
## including Heat/Biofuels/Renewables even though they don't get their own
## line item in Mohammed's list -- the total should still reflect them.

iea_fe_totals <- iea_fe_raw |>
  mutate(sector_iamc = map_sector_name(sector)) |>
  filter(!is.na(sector_iamc)) |>
  group_by(country, year, sector_iamc) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(variable = paste0("Final Energy|", sector_iamc)) |>
  select(country, year, variable, value)


#### 5. Combine, add ISO codes, finish IAMC formatting -------------------------

iea_final_energy <- bind_rows(iea_fe_components, iea_fe_totals) |>
  mutate(
    iso = countrycode(country, "country.name", "iso3c",
                      custom_match = c("Europe" = "EUR")),
    model    = "IEA",
    scenario = "historical",
    unit     = "EJ/yr"
  ) |>
  filter(!is.na(iso)) |>
  rename(region = iso) |>
  mutate(value = as.character(value)) |>
  select(region, variable, unit, year, value, model, scenario)

message(sprintf(
  "Built %d IAMC rows across %d variables and %d countries.",
  nrow(iea_final_energy),
  n_distinct(iea_final_energy$variable),
  n_distinct(iea_final_energy$region)
))

# Flag any country names that didn't resolve to an ISO code, so they can be
# fixed via custom_match rather than silently dropped (e.g. "Europe" is not
# a country and needs a decision on how it should be represented).
unmatched_countries <- iea_fe_raw |>
  distinct(country) |>
  mutate(iso = countrycode(country, "country.name", "iso3c",
                           custom_match = c("Europe" = "EUR"))) |>
  filter(is.na(iso))

if (nrow(unmatched_countries) > 0) {
  message("Countries that did not resolve to an ISO code (dropped):")
  print(unmatched_countries)
}


#### 6. Sanity check: confirm the target variable list is covered -------------

target_vars <- c(
  "Final Energy|Residential and Commercial",
  "Final Energy|Residential and Commercial|Electricity",
  "Final Energy|Residential and Commercial|Gases",
  "Final Energy|Residential and Commercial|Solids|Coal",
  "Final Energy|Industry",
  "Final Energy|Industry|Electricity",
  "Final Energy|Industry|Gases",
  "Final Energy|Industry|Solids|Coal",
  "Final Energy|Industry|Liquids",
  "Final Energy|Transportation",
  "Final Energy|Transportation|Liquids|Oil"
)

missing_vars <- setdiff(target_vars, unique(iea_final_energy$variable))
if (length(missing_vars) > 0) {
  message("These requested variables have NO data from this source ",
          "(may be expected, e.g. the coal gap in some sectors/countries):")
  print(missing_vars)
}

## NOTE on the Transport oil suffix: map_fuel_category() gives Transport's oil
## the "|Oil" suffix ("Final Energy|Transportation|Liquids|Oil") per Mohammed's
## naming, and plain "|Liquids" for Industry. That matches target_vars above.

#### 7. Merge into main historical data frame ----------------------------------
## Uncomment once verified against a live process_hist_data.R run.
## Use local() isolation as with the other inserted blocks:
# iea_final_energy <- local({
#   source("src/iea_final_energy_consumption_block.R", local = TRUE)
#   iea_final_energy
# })
# data_iso <- data_iso |> bind_rows(iea_final_energy)
