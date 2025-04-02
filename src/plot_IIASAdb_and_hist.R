#Script to explore data in publicly accessible IIASA scenario databases, and do plots together with historical data


library(conflicted)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('lag', 'dplyr', quiet = TRUE)

library(arrow)
library(tidyverse)
library(reticulate)

# Change to the location of your local anaconda / miniconda installation, which includes python 
# (necessary in case you have multiple python instances on your system, else you can keep this outcommented)
# use_python(python = "C:/Users/andym/miniconda3/python.exe")

## Run the following lines only on first installation
# conda_update()
# conda_install(packages = "numpy")
# conda_install(packages = "matplotlib")
# conda_install(packages = "pyam")
# conda_install(packages = "setuptools-scm")

# indicate that we want to use a specific condaenv
use_condaenv("r-reticulate")

pyam <- import("pyam")

#Authentification and check access
#pyam$iiasa$set_config('username', 'pwdXXXXX')

iiasa_creds <- 'C:/Users/bertram/Documents/iiasa_credentials.yml' # two lines with 'username: name' and 'password: pwd'

conn <- pyam$iiasa$Connection(creds = iiasa_creds)
print(conn$valid_connections)
##### 1. explore database content ------
##### AR6 ------

#read in all data for Emissions|CO2
df = pyam$read_iiasa(
  name = 'ar6-public',
  variable=c(
    'Emissions|CO2'
  ),
  # region=c('World'),
  # model=c('GCAM 5.3'),
  creds = iiasa_creds
)
df <- df$data
#look at full set of models available
unique(df$model)
#look at full set of scenarios available (most will be only available for one or a handful of models)x
unique(df$scenario)
#look at full set of regions available: World, R5, R6, R10, and 66 single countries, but not native model regions
unique(df$region)

#read in all global data
df = pyam$read_iiasa(
  name = 'ar6-public',
  region=c(
    'World'
  ),
  creds = iiasa_creds
)
df <- df$data
#look at full set of regions available: World, R5, R6, R10, and 66 single countries, but not native model regions
vars_ar6 <- unique(df$variable)

##### NGFS ------

#read in all data for Emissions|CO2
df = pyam$read_iiasa(
  name = 'ngfs_phase_5',
  variable=c(
    'Emissions|CO2'
  ),
  # region=c('World'),
  # model=c('GCAM 5.3'),
  creds = iiasa_creds
)
df <- df$data
#look at full set of models available
unique(df$model)
#look at full set of scenarios available (most will be only available for one or a handful of models)x
unique(df$scenario)
#look at full set of regions available: World, R5, R6, R10, and 66 single countries, but not native model regions
unique(df$region)

#read in all global and one country data
df = pyam$read_iiasa(
  name = 'ngfs_phase_5',
  region=c(
    'CZE',
    'World'
  ),
  creds = iiasa_creds
)
df <- df$data
#look at full set of variables available
vars_ngfs <- unique(df[df$region=="World",]$variable)
vars_ngfs_dc <- unique(df[df$region=="CZE",]$variable)


##### 2.1 Load scenario data to plot -----
# select variables to download
vars <- c("Secondary Energy|Electricity|Coal",
          "Secondary Energy|Electricity|Gas",
          "Secondary Energy|Electricity|Oil",
          "Secondary Energy|Electricity|Hydro",
          "Secondary Energy|Electricity|Nuclear",
          "Secondary Energy|Electricity|Biomass",
          "Secondary Energy|Electricity|Wind",
          "Secondary Energy|Electricity|Solar",
          "Secondary Energy|Electricity|Geothermal",
          "Emissions|CH4", 
          "Emissions|CO2", 
          "Emissions|CO2|Energy|Supply|Electricity",
          "Emissions|CO2|Energy and Industrial Processes"
          )
#download data from NGFS dataset, only chosen variables, regions, scenarios and model
data_scen_full <- pyam$read_iiasa(
  name = 'ngfs_phase_5',
  variable=vars,
  region=c('GCAM 6.0 NGFS|USA','World'),
  model=c('GCAM 6.0 NGFS'),
  scenario =c("Net Zero 2050","Current Policies","Nationally Determined Contributions (NDCs)"),
  creds = iiasa_creds
)
data_scen_full <- data_scen_full$data
#make region name consistent with country data
data_scen_full <- data_scen_full |> mutate(region = case_when(
  region == "World" ~ "World",
  .default = gsub(x=region,pattern="GCAM 6.0 NGFS\\|",replacement="")
))

##### 2.2 Historical data -----

# IF it's desired to use the provided historical data in the IAMC format, rather 
# than re-create it using src/process_hist_data.R, then download the files from:
# PUBLIC_output_rhistiamc:
# https://drive.google.com/open?id=117cTkVRekeu3vHYrFkH93zstpCqGxkM8&usp=drive_fs
# into your local output/ folder


#### THEN read in historic data in wide format and convert to long
data_hist <- read.csv(paste0("output/historical_gcam32.csv"), na.strings = "NA")
colnames(data_hist) <- gsub("^X", "", colnames(data_hist))

data_hist <- data_hist |>
  pivot_longer(cols = -c(model, variable, region, unit, scenario),
               names_to = "year",
               values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |> 
  mutate(value=as.numeric(value),
         year=as.numeric(year))

##### 3 Plotting -----

#electricity mix

# NOTE: These figures can only accommodate up to three scenarios at once, so 
# it disregards the later ones if more than three supplied 
models_selected <- unique(data_scen_full$model)
data_scen <- data_scen_full[!duplicated(data_scen_full),]
regions_selected <- "USA"
scenarios_selected <- unique(data_scen_full$scenario)

#set latest year for scenario data to be plotted
endyear=2050
#do electricity plot
source("../rhistiamc/src/fig_elec_generation_mix.R")


#select variable, scenarios and region to plot
var <- "Emissions|CH4" 
var <- "Emissions|CO2" 
var <- "Emissions|CO2|Energy|Supply|Electricity"
var <- "Emissions|CO2|Energy and Industrial Processes" 
regions_selected <- "USA"
regions_selected <- "World"


#do line plot with selected variable and region, for specified variable
ggplot()+
  #plot historic data in black (default if color is not set), 
  #with different historic sources ('model' variable) differentiated by linetype
  geom_line(data=data_hist|>filter(variable==var,region==regions_selected,scenario=="historical",model!="OWID"),
            aes(x=year,y=value,linetype=model))+
  #plot scenario data in colors, can be defined below
  geom_line(data=data_scen_full|>filter(variable==var,region==regions_selected,scenario %in% scenarios_selected),
            aes(x=year,y=value,color=scenario))+
  geom_point(aes(x=2021,y=0.01),color="white")+
  #costumized colors if desired, else comment out
  scale_color_manual(values=c("red","blue","#009900"))+
  #nice general plot layout
  theme_bw()+
  #y-axis label and title (would need to be adjusted for CH4 plot...)
  ylab("Mt CO2/yr") + ggtitle(paste0(var," in ",regions_selected))
