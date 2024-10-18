# This script defines functions for use in other scripts
# Ryna Cui, April 2020
# Alicia Zhao
# Jenna Behrendt, July 2021
# Andy Miller, June 2024


# check operating system ----------

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "macosx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


# convert quantities  --------------------


#gwp setting: using AR4, if other is used, you have to change here and below in hard-coded (AR4GWP100) variable choices
gwp100 <- data.frame(variable=c("Emissions|CO2","Emissions|CH4","Emissions|N2O","Emissions|F-Gases","Emissions|Kyoto Gases","Emissions|HFC","Emissions|CO2|Energy and Industrial Processes"),
                     entity=c("CO2","CH4","N2O",     "FGas", "Emissions|Kyoto Gases","HFC","CO2"),
                     gwp=c(1    , 25  , 298/1000   ,1     ,1,                   1.43,1)) # AR4 (HFC number taken from waterfall script??)
#  gwp=c(1    , 28  , 265/1000   ,1     ,1, 9.9999)) # AR5, HFC to check
#  gwp=c(1    , 29.8, 273/1000   ,1     ,1, 9.9999)) # AR5 CH4 is for fossil, non-fossil has 27.2

# functions to convert energy units

#conversion factors:
ej2twh <- 277.77777777778
bcm2ej <- 36 #see "Approximate conversion factors.pdf" from IEA 

TWh_to_EJ <- 0.0036
EJ_to_GWh <- 1000/TWh_to_EJ
TWh_to_EJ <- 1000/EJ_to_GWh
hr_per_yr <- 8760


conv_EJ_TWh <- function (data, EJ){
  data %>%
    mutate(TWh = EJ / TWh_to_EJ)
}

conv_TWh_EJ <- function (data, TWh){
  data %>%
    mutate(EJ = TWh * TWh_to_EJ)
}


# functions to convert capacity to generation

conv_GW_EJ <- function (data, cf, GW){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036
  
  data %>%
    mutate(EJ = GW * (cf * hr_per_yr * EJ_to_GWh))
}

conv_EJ_GW <- function (data, cf, EJ){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036
  
  data %>%
    mutate(gw = EJ / (cf * hr_per_yr * EJ_to_GWh))
}


# interpolate quantities -----------------
approx_fun <- function(year, value, rule = 1) {
  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year)$y,
             error = function(e) NA)
    
  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
}


# Function to remove brackets and their content
remove_brackets <- function(text) {
  gsub("\\s*\\(.*?\\)\\s*", "", text)
}

# format all maps into a long table -------------------
gather_map <- function(df){
  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    pivot_longer(cols = -all_of(untouched_cols), names_to = "identifier", values_to = "var") %>%
    select(-identifier) %>%
    filter(!is.na(var), var != "") %>%
    return()
}

# Variation of the function which formats all maps into a long table,
# where a single entry of each item that isn't mapped to anything is preserved, 
# to show that no variables were accidentally left out
gather_map_all <- function(df){
  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    # rows with var1 left blank are considered to be purposefully excluded
    mutate(var1 = if_else(is.na(var1) | var1 == "", "excluded", var1)) %>%
    pivot_longer(cols = -all_of(untouched_cols), names_to = "identifier", values_to = "var") %>%
    select(-identifier) %>%
    filter(!is.na(var), var != "") %>%
    mutate(var = ifelse(var == "excluded", NA, var)) %>%
    return()
}

##windowsFonts(`Gill Sans MT` = windowsFont("Gill Sans MT"))

# define chart format
theme_panel <- theme_bw() +   ##base_family = 'Gill Sans MT') + 
  theme(text=element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 10, vjust = -0.1, colour = "black"),
        axis.title.y = element_text(size = 12, vjust = -0.1),
        axis.title.x = element_text(size = 12, vjust = -0.1),
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 9, colour = "black"), ##size = 12
        strip.background = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_blank(),
        # panel.grid.minor.y = element_blank(),
        # panel.grid.minor.x = element_blank(),
        # panel.spacing.x = unit(1, "lines"),
        # panel.spacing.y = unit(2, "lines"),
        panel.spacing = unit(2, "lines"),
        #strip.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(panel.border = element_rect(colour = "black")) 


# check for excluded and included variables -----------
# find the unique values of x$colmn_x that are not covered in y$colmn_y,
# unless opt is set to "i", in which case it returns the ones that are included
check_match <- function(x, y, colmn_x, colmn_y = NULL, opt = "e") {
  
  colmn_y <- ifelse(is.null(colmn_y), colmn_x, colmn_y)
  
  x <- as_tibble(x)
  y <- as_tibble(y)
  
  loc_x <- which(names(x) == colmn_x)
  loc_y <- which(names(y) == colmn_y)
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  val_x <- as.data.frame(x[,loc_x])
  val_y <- as.data.frame(y[,loc_y])
  
  if(opt == "e") {
    excl = unique(val_x[!(val_x[,1] %in% val_y[,1]),1])
    lst <- ifelse(length(excl) == 0, print("There are no variables in the first one that are not in the second one."),
                  print(list("The following are in the first one, but not in the second one:", excl)))
  }
  if(opt == "i") {
    incl = unique(val_x[(val_x[,1] %in% val_y[,1]),1])
    lst <- ifelse(length(incl) == 0, print("There are no variables in the first one that are included in the second one."),
                  print(list("The following are in both:", incl)))
  }
}

# check how the world value compares ---------------
# to the sum of the other regions (three-letter ISO countries)
check_world <- function(x) {
  
  nativeWorldName <- unique(x$iso)[grepl("WLD", unique(x$iso)) | grepl("WORLD", unique(x$iso)) | 
                  grepl("GLO", unique(x$iso)) | grepl("Glo", unique(x$iso)) | 
                  grepl("Wor", unique(x$iso))| grepl("EARTH", unique(x$iso))]
  
  print(paste0("This world ISO found: ", nativeWorldName))
  
  x_native <- x |>
    filter(iso == nativeWorldName) |>
    rename(value_native = value)
  

  x_calc <- x |>
    filter(!(iso %in% c(nativeWorldName))) |>
    # Keep only three-letter ISO entries
    filter(grepl("^...$", iso)) |>
    group_by(year, unit, variable, model, scenario) |>
    summarise(value = sum(value, na.rm = T)) |>
    ungroup() |> 
    rename(value_calc = value)

   reg_disregard <- unique((x |>  filter(!(grepl("^...$", iso))))$iso)
  
  print("These regions disregarded from calculated global sum: ")
  print(nativeWorldName)
  print(reg_disregard)
   
  x_combo <- x_native |>
    left_join(x_calc) |>
    mutate(diff = value_native - value_calc,
           diff_pct = 100*(value_native - value_calc)/value_native)
  
  print("Comparison for year 2019: ")
  print(head(x_combo |> filter(year == 2019) |> arrange(desc(diff_pct)), n = 15))
  
  return(x_combo)
  
}

# capitalize first letter of every variable ------------------
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# categorize sectors -----

## color by sector
sector.color <- c(
  "Power" = "#FEE12B",
  "Electricity" = "#FEE12B",
  "Liquids refining" = "#A4A4A4",  
  "Refining" = "#A4A4A4",
  "Industry" = "#F8991F",
  "Other energy supply" = "#787878",
  "Other Energy Supply" = "#787878",
  "H2 supply" = "#6A0DAD",
  "Residential and commercial" = "#6BCAF1",
  "Buildings" = "#6BCAF1",
  "Transportation" = "#0288A7",
  "Industrial processes" = "#D78423", ##added
  "Solvent use" = "#c47106", ##added
  "Fertilizer" = "#008080",   ##added
  "Cement" = "#D78423",   ##added
  "AFOLU CO2" = "#88CEB9",
  "AFOLU" = "#88CEB9",   ##added 
  "AFOLU Sinks" = "#00FA9A",
  "AFOLU Sources" = "#EEE48C",
  "NonCO2" = "#335f7a",
  "nonCO2" = "#335f7a",
  "Other" = "light grey",
  "Total GHG 2020" = "light grey",
  "Total GHG 2035" = "light grey")

sector.list <- names(sector.color)

fillScaleSector <- scale_fill_manual(name = "Sector",
                                     values = sector.color,
                                     na.translate = FALSE,
                                     guide = guide_legend(reverse = F, ncol = 1))


# categorize technologies --------
tech.list <- c("Other", "Geothermal", "Solar", "Wind", "Biomass w/ CCS", "Biomass w/o CCS",
               "Hydro", "Nuclear",
               "Gas w/ CCS", "Gas w/o CCS", "Natural gas_CCS", "Natural gas",
               "Oil w/ CCS", "Oil w/o CCS", "Coal w/ CCS","Coal w/o CCS", "Coal.total",
               "Biomass 1st generation", "Traditional biomass", "Biomass traditional")

standard_tech_group <- function(data, technology) {
  data %>%
    mutate(tech = technology,
           tech = replace(tech, grepl("wind", technology), "wind"),
           tech = replace(tech, grepl("biomass", technology), "biomass"),
           tech = replace(tech, grepl("PV", technology), "solar"),
           tech = replace(tech, grepl("pv", technology), "solar"),
           tech = replace(tech, grepl("CSP", technology), "solar"),
           tech = replace(tech, grepl("geothermal", technology), "geothermal"),
           tech = replace(tech, grepl("hydro", technology), "hydro"),
           tech = replace(tech, grepl("Gen", technology), "nuclear"),
           tech = replace(tech, grepl("coal", technology), "coal"),
           tech = replace(tech, grepl("gas", technology), "gas"),
           tech = replace(tech, grepl("refined liquids", technology), "oil"),
           tech = replace(tech, grepl("oil", technology), "oil"),
           tech = replace(tech, grepl("CCS", technology), paste(tech[grepl("CCS", technology)], "CCS", sep = " w/ "))) %>%
    mutate(tech = factor(tech, levels = tech.list))
}

## color by tech
tech.color <- c( "Biomass w/o CCS" = "#88CEB9", "Biomass w/ CCS" = "white",
                 "Solar" = "#FEE12B",
                 "Wind" = "#6BCAF1",
                 "Geothermal" = "#335f7a",
                 "Hydro" = "#0288A7",
                 "Coal w/o CCS" = "#4A4A4A", "Coal w/ CCS" = "white",
                 "Nuclear" = "#F8991F",
                 "Gas w/o CCS" = "#A4A4A4", "Gas w/ CCS" = "white",
                 "Oil w/o CCS" = "#787878", "Oil w/ CCS" = "white",
                 # "other" = "firebrick3")
                 "Other" = "plum")


colScaleTech <- scale_colour_manual(name = "Technology",
                                    values = tech.color,
                                    na.translate = FALSE,
                                    guide = guide_legend(reverse = F, ncol = 1))
fillScaleTech <- scale_fill_manual(name = "Technology",
                                   values = tech.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

## categorize pattern by tech 
tech.pattern <- c("Biomass w/o CCS" = "none", "Biomass w/ CCS" = "stripe",
                  "Solar" = "none",
                  "Wind" = "none",
                  "Geothermal" = "none",
                  "Hydro" = "none",
                  "Coal w/o CCS" = "none", "Coal w/ CCS" = "stripe",
                  "Nuclear" = "none",
                  "Gas w/o CCS" = "none", "Gas w/ CCS" = "stripe",
                  "Oil w/o CCS" = "none", "Oil w/ CCS" = "stripe")
# "other" = "firebrick3"))

# patternScaleTech <- scale_pattern_manual(name = "Technology",
#                                          values = tech.pattern,
#                                          na.translate = FALSE,
#                                          guide = guide_legend(reverse = F, ncol = 1))

# pattern color by tech
tech.pattern.color <- c("Biomass w/o CCS" = "#88CEB9", "Biomass w/ CCS" = "#88CEB9",
                        "Solar" = "#FEE12B",
                        "Wind" = "#6BCAF1",
                        "Geothermal" = "#335f7a",
                        "Hydro" = "#0288A7",
                        "Coal w/o CCS" = "#4A4A4A", "Coal w/ CCS" = "#4A4A4A",
                        "Nuclear" = "#F8991F",
                        "Gas w/o CCS" = "#A4A4A4", "Gas w/ CCS" = "#B0B0B0",
                        "Oil w/o CCS" = "#787878", "Oil w/ CCS" = "#848484")
# "other" = "firebrick3")


# patternColorScaleTech <- scale_pattern_color_manual(name = "Technology",
#                                                     values = tech.pattern.color,
#                                                     na.translate = FALSE,
#                                                     guide = guide_legend(reverse = F, ncol = 1))

# categorize primary energy fuels -------------------
pe_fuel.list <- c(  "Other", "Geothermal", "Solar", "Wind", 
                    "Biomass Modern",    ##"Natural gas", "Coal.total", "Biomass 1st generation
                    "Traditional biomass", "Biomass Traditional", 
                    "Hydro", "Nuclear", "Gas", "Oil", "Coal") 

pe_fuel.color <- c("Biomass Modern" = "#88CEB9", 
                   "Biomass Traditional" = "#AA9ECC",  ##added
                 "Solar" = "#FEE12B",
                 "Wind" = "#6BCAF1",
                 "Geothermal" = "#335f7a",
                 "Hydro" = "#0288A7",
                 "Coal" = "#4A4A4A",
                 "Nuclear" = "#F8991F",
                 "Gas" = "#A4A4A4", 
                 "Oil" = "#787878",
                 # "other" = "firebrick3")
                 "Other" = "plum")

colScalePeFuel <- scale_colour_manual(name = "Fuel",
                                    values = pe_fuel.color,
                                    na.translate = FALSE,
                                    guide = guide_legend(reverse = F, ncol = 1))
fillScalePeFuel <- scale_fill_manual(name = "Fuel",
                                   values = pe_fuel.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))


# categorize fuels ----------------
fuel.list <- c("Hydrogen","Electricity","Biomass","Heat", "Other", "Gases","Liquids","Coal")


## color by fuel
fuel.color <- c( "Biomass" = "#88CEB9",
                 "Electricity" = "#F49507",
                 "Hydrogen" = "#FFA384",
                 "Coal" = "#4A4A4A",
                 "Gases" = "#A4A4A4",
                 "Liquids" = "#787878",
                 "Heat" = "#CB5B3B",
                 "Other" = "#6BCAF1")

colScaleFuel <- scale_colour_manual(name = "Fuel",
                                    values = fuel.color,
                                    na.translate = FALSE,
                                    guide = guide_legend(reverse = F, ncol = 1))
fillScaleFuel <- scale_fill_manual(name = "Fuel",
                                   values = fuel.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

elec_pattern <- c("none", "none","none","stripe",
                  "none", "none", "stripe", "none",
                  "none", "stripe", "none")


# categorize ag demand -----------
ag_list <- c("Food","Other","Energy", "Feed","Livestock")

ag_color <- c("Energy" = "darkorange1",
              "Other" = "darkgreen",
              "Feed" = "darkred",
              "Food" = "#88CEB9",
              "Livestock" = "mediumorchid3")

fillScaleAg <- scale_fill_manual(name = "ag",
                                     values = ag_color, 
                                     na.translate = FALSE,
                                     guide = guide_legend(reverse = F, ncol = 1))
# categorize land cover -----------------
land_list <- c("Built-up Area","Energy Crops","Managed Forest", "Natural Forest","Other Arable Land", "Other Land", "Pasture")

land_color <- c("Built-up Area" = "darkred",
                "Energy Crops"= "darkorange1",
                "Managed Forest" = "darkgreen", 
                "Natural Forest" = "olivedrab3",
                "Other Arable Land" = "goldenrod1", 
                "Other Land" = "hot pink", 
                "Pasture" = "cornflowerblue")

fillScaleLand <- scale_fill_manual(name = "land",
                                   values = land_color, 
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

colScaleLand <- scale_colour_manual(name = "sector",
                                      values = land_color, 
                                      na.translate = FALSE,
                                      guide = guide_legend(reverse = F, ncol = 1))


# categorize air pollutant emissions -----

# define emis color
emis.list <- c("CO", "NOx", "Sulfur")

emis_color <- c("CO" = "mediumpurple", 
                  "NOx" = "olivedrab3", 
                  "SO2" = "cornflowerblue")

fillScaleemis <- scale_fill_manual(name = "emis",
                                    values = emis_color, 
                                    na.translate = FALSE,
                                    guide = guide_legend(reverse = F, ncol = 1))

# color by scenario -------------------

target.list <- c("Current Policies", "NDC Continued", "NDC to 2 C","2 C","1.5 C")

target.color <- c("Current Policies" = "chocolate3",
                  "NDC Continued" = "mediumpurple", 
                  "2 C" = "olivedrab3",  
                  "NDC to 2 C" = "olivedrab3",
                  "1.5 C" = "cornflowerblue")

colScaleTarget <- scale_colour_manual(name = "Scenario", 
                                      values = target.color, 
                                      guide = guide_legend(reverse = T, ncol = 1))
scen.list <- c("d", "o")

scen.line <- c("o" = "solid",
               "d" = "dashed")

lineScaleScen <- scale_linetype_manual(name = "Scenario", 
                                       values = scen.line,
                                       labels = c("d" = 'Disorderly', "o" = 'Orderly'),
                                       guide = guide_legend(reverse = T, ncol = 1))

standard_scen_name <- function (data, Scenario) {
  data %>%
    separate(Scenario, into = c("scen", "target"), sep = "_") %>%
    mutate(target = sub("delfrag", "2c", target),
           target = sub("rap", "1p5c", target),
           scen = sub("h", "o", scen)) %>%
    mutate(target = factor(target, levels = target.list),
           scen = factor(scen, levels = scen.list))
}


