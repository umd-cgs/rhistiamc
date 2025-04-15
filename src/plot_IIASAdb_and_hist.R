#Script to explore data in publicly accessible IIASA scenario databases via API, and do plots together with historical data

# documentation on how to set up using this link between R, python, and the IIASA API is here:
# https://pyam-iamc.readthedocs.io/en/stable/R_tutorials/pyam_R_tutorial.html

# more detailed documentation on how to use the API in general is here, 
# though this documentation is for python, so most commands are replacing the "." with a "$" in R:
# https://pyam-iamc.readthedocs.io/en/stable/tutorials/iiasa.html



library(conflicted)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('lag', 'dplyr', quiet = TRUE)

library(arrow)
library(tidyverse)
library(reticulate)

#source functions and constants
source("src/functions.R")

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
#see which database names exist (some of which you however might not have access to with your account)
conn$valid_connections

##### 1. explore database content ------
###### AR6 ------

#make connection to desired database
conn = pyam$iiasa$Connection("ar6-public")
#see which models exist
conn$models()
#see which scenarios exist
conn$scenarios()
#see subset of scenarios with specific name component
grep("SusDev",conn$scenarios(),value=T)
#see variables
conn$variables()
#see subset of variables with specific name component
grep("Livestock",conn$variables(),value=T)
#see regions
conn$regions()
#see subset of regions with specific name component
grep("America",conn$regions(),value=T)
#pretty bad that the database has included the "(R6)" in all region names of that disaggregation,
#but "(R5)" and "(R10)" only for the "Rest of the World" region, so using those regions is not straightforward...
#some indication (though some names are still different) here: https://github.com/IAMconsortium/common-definitions/blob/main/mappings/GCAM_7.1.yaml
grep("R",conn$regions(),value=T)

# for more detailed exploration, e.g. knowing all variables existing for GCAM scenarios in any version (wildcard *):
# takes longer the more data you query, thus try to always subset at least two dimensions across region, scenario, model, variable...
unique(pyam$read_iiasa(
  name = 'ar6-public',
  region=c('World'),
  model=c('GCAM*'),
  creds = iiasa_creds
)$data$variable)


###### NGFS ------

#make connection to desired database
conn = pyam$iiasa$Connection("ngfs_phase_5")
#see which models exist
conn$models()
#see which scenarios exist
conn$scenarios()
#see variables
conn$variables()
#see subset of variables with specific name component
grep("Carbon",conn$variables(),value=T)
#see regions
conn$regions()
#see subset of regions with specific name component
grep("GCAM",conn$regions(),value=T)

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

# Load historical data, after downloading the csv files (either gcam32, or ISO) from
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
