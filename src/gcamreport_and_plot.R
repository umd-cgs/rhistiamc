#this package is needed for the following command to install package from github,
#lines 3 and 6 only need to be executed the first time you use this script, or in case you need to update the gcamreport package
library(devtools)
#install from github, but use different branch "dev_commdef" that includes some bugfixes for GCAM 7.1 not yet in gcam-core
#you will be asked about updating other packages, which you probably can skip (just hit 'enter' without entering any number)
install_github('bc3LC/gcamreport', ref = "dev_commdef")

# load libraries needed for analysis (this has to be done every time)
library(gcamreport) #convert the gcam database into R readible format
library(tidyverse)  #generic data-wrangling package
library(ggplot2)    #package for plots

#source functions and constants
source("src/functions.R")

#you can check for all available variables in gcamreport, but the more you select, 
#the longer it takes, and some might also produce errors
#the ones selected below mostly worked (some unfortunately did not work for all scenarios for me)
available_variables()


##### 1. create reports for both databases --------
generate_report(db_path = "C:/Users/bertram/Documents/Projects/gcam7p1/output/", 
                #this needs to point to correct database name, the prj_name can be defined by you (saving data for later use)
                db_name = "database_basexdb", prj_name = "4scen_project_250225.dat", 
                #you can define which scenarios you want to extract, and whether to cut the time series before 2100
                GCAM_version = "v7.1", scenarios = c("Reference","Tax25","TaxDiff","Cap_CO2"), final_year = 2050,
                # you can outcomment the following line, to create the full reporting, or define a subset, e.g. "Agricultural *" will give you all variables starting with 'Agricultural'
                desired_variables =c("Emissions*","Secondary*","Primary*","Final*"),
                # ignore = c("Coal-Generation-Ceiling","Solar-Generation-Floor","Wind-Generation-Floor"),
                launch_ui = FALSE)
#
generate_report(db_path = "C:/Users/bertram/Documents/Projects/gcam7p1/output/", 
                #this needs to point to correct database name, the prj_name can be defined by you (saving data for later use)
                db_name = "database_basexdb3", prj_name = "4scen_project_250225_3.dat", 
                #you can define which scenarios you want to extract, and whether to cut the time series before 2100
                GCAM_version = "v7.1", scenarios = c("Cap_CO2_advS","Cap_CO2_advRen","Ref_PakCon","Cap_CO2_uct","Cap_CO2_ffict","Cap_CO2_uct_in"), final_year = 2050,
                # you can outcomment the following line, to create the full reporting, or define a subset, e.g. "Agricultural *" will give you all variables starting with 'Agricultural'
                desired_variables =c("Emissions*","Secondary*","Primary*","Final*"),
                # ignore = c("Coal-Generation-Ceiling","Solar-Generation-Floor","Wind-Generation-Floor"),
                launch_ui = FALSE)



##### 2.0 read in combined scenario data --------

data_scen_full1 <- rbind(read.csv("C:/Users/bertram/Documents/Projects/gcam7p1/output/4scen_project_250225_standardized.csv"),
                   read.csv("C:/Users/bertram/Documents/Projects/gcam7p1/output/4scen_project_250225_3_standardized.csv"))|>
  pivot_longer(cols=c(-Model,-Scenario,-Region,-Variable,-Unit),names_to = "year")|>rename_with(tolower) |>
  mutate(year=as.numeric(substr(year,2,5)))

load("C:/Users/bertram/Documents/Projects/gcam7p1/output/4scen_project_250225_standardized.RData")
data_scen_full <- report
load("C:/Users/bertram/Documents/Projects/gcam7p1/output/4scen_project_250225_3_standardized.RData")
data_scen_full <- data_scen_full |> rbind(report) |> pivot_longer(cols=c(-Model,-Scenario,-Region,-Variable,-Unit),names_to = "year")|>rename_with(tolower) |> 
  mutate(year=as.numeric(year))

##### 2.1 Historical data -----

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
scenarios_selected <- c("Cap_CO2","Reference","Cap_CO2_advRen")

#set latest year for scenario data to be plotted
endyear=2050
#do electricity plot
source("src/fig_elec_generation_mix.R")


#select variable, scenarios and region to plot
var <- "Secondary Energy|Electricity|Biomass|w/ CCS" 
regions_selected <- "China"

#do line plot with selected variable and region, for specified variable
ggplot()+
  #plot historic data in black (default if color is not set), 
  #with different historic sources ('model' variable) differentiated by linetype
  geom_line(data=data_hist|>filter(variable==var,region==regions_selected,scenario=="historical"),
            aes(x=year,y=value,linetype=model))+
  #plot scenario data in colors, can be defined below
  geom_line(data=data_scen_full|>filter(variable==var,region==regions_selected,scenario %in% scenarios_selected),
            aes(x=year,y=value,color=scenario))+
  #costumized colors if desired, else comment out
  scale_color_manual(values=c("red","blue","#009900"))+
  #nice general plot layout
  theme_bw()+
  #y-axis label and title
  ylab("EJ/yr") + ggtitle(paste0(var," in ",regions_selected))
