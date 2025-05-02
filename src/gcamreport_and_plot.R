#this package is needed for the following command to install package from github,
#lines 4 and 8 only need to be executed the first time you use this script, or in case you need to update the gcamreport package
#line 7 and 8 are in principle identical, but line 7 might change which could introduce errors, so 8 is saver
# library(devtools)
#install from github, but use different branch "dev_commdef" that includes some bugfixes for GCAM 7.1 not yet in gcam-core
#you will be asked about updating other packages, which you probably can skip (just hit 'enter' without entering any number)
# install_github('bc3LC/gcamreport', ref = "dev_commdef")
# install_github('christophbertram/gcamreport')

# load libraries needed for analysis (this has to be done every time)
library(gcamreport) #convert the gcam database into R readible format
library(tidyverse)  #generic data-wrangling package
library(ggplot2)    #package for plots

#source functions and constants
source("src/functions.R")

#you can check for all available variables in gcamreport (in the example here searching for all variables that include "Food"), but the more you select, 
#the longer it takes, and some might also produce errors
#the ones selected below  worked
vars <- available_variables()
grep("Food",vars,value=T)

##### 1. create reports for both databases --------
generate_report(db_path = "C:/Users/bertram/Documents/Projects/gcam7p1/output/", 
                #this needs to point to correct database name, the prj_name can be defined by you (saving data for later use)
                db_name = "database_basexdb", prj_name = "3scen_project_250428.dat", 
                #you can define which scenarios you want to extract, and whether to cut the time series before 2100
                GCAM_version = "v7.1", scenarios = c("Reference","Tax25","Cap_CO2"), final_year = 2050,
                # you can outcomment the following line, to create the full reporting, or define a subset, e.g. "Agricultural *" will give you all variables starting with 'Agricultural'
                # as the full reporting with all variables most often requires changes to the mapping (more difficult) we recommend only specifying a couple variables with wildcards*
                desired_variables =c("Agricultural*","Emissions*","Secondary*","Primary*","Final*"),
                # to speed up processing, select only a subset of regions, 
                # but always include USA and China!!!!
                desired_regions = c("USA","China","India"),
                #if your add-on files include new market names, you have to tell gcamreport to ignore those
                # ignore = c("Coal-Generation-Ceiling","Solar-Generation-Floor","Wind-Generation-Floor"),
                launch_ui = FALSE)

#
generate_report(db_path = "C:/Users/bertram/Documents/Projects/gcam7p1/output/", 
                #this needs to point to correct database name, the prj_name can be defined by you (saving data for later use)
                db_name = "database_basexdb3", prj_name = "4scen_project_250311_3.dat", 
                #you can define which scenarios you want to extract, and whether to cut the time series before 2100
                GCAM_version = "v7.1", scenarios = c("Cap_CO2_advS","Cap_CO2_advRen","Ref_PakCon","Cap_CO2_uct","Cap_CO2_ffict","Cap_CO2_uct_in"), final_year = 2050,
                # you can outcomment the following line, to create the full reporting, or define a subset, e.g. "Agricultural *" will give you all variables starting with 'Agricultural'
                desired_variables =c("Emissions*","Secondary*","Primary*","Final*"),
                #if your add-on files include new market names, you have to tell gcamreport to ignore those
                ignore = c("Coal-Generation-Ceiling","Solar-Generation-Floor","Wind-Generation-Floor"),
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
var <- "Emissions|CH4" 
var <- "Emissions|CO2" 
var <- "Emissions|CO2|Energy|Supply|Electricity"
var <- "Emissions|CO2|Energy and Industrial Processes" 
regions_selected <- "China"
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
  #y-axis label and title
  ylab("Mt CO2/yr") + ggtitle(paste0(var," in ",regions_selected))

#### Option B: R plots for data extracted manually from modelinterface -----------

#plot using ModelInterface output - with example of aggregation across dimension
#read in data that you saved in a csv file via excel (pulling the data from ModelInterface queries)
sec_emi <- read.csv("runs/emi_bysector_2scen.csv",skip=1) 
sec_emi <- sec_emi|> mutate(sector=paste0(sector,sector2,sector3)) |>
  select(-sector2,-sector3) |> pivot_longer(cols = c(-scenario,-region,-sector,-Units),names_to = "year") |> 
  mutate(year=as.numeric(substr(year,2,5)))

# clean up residential and commercial data: aggregate deciles
sec_emi <- sec_emi |>filter(!sector %in% grep("_d",unique(sec_emi$sector),value=T)) |> 
  rbind(
  #add cooling
  sec_emi |> filter(sector %in% grep("residcooling",unique(sec_emi$sector),value=T)) |>
    group_by(scenario,region,year,Units) |> summarize(value=sum(value)) |> ungroup() |>
    mutate(sector="residcoolingmodern")) |>
  rbind(
  #add heating
    sec_emi |> filter(sector %in% grep("residheating",unique(sec_emi$sector),value=T)) |>
      group_by(scenario,region,year,Units) |> summarize(value=sum(value)) |> ungroup() |>
      mutate(sector="residheatingmodern")) |>
  rbind(
    #add others
    sec_emi |> filter(sector %in% grep("residothers",unique(sec_emi$sector),value=T)) |>
      group_by(scenario,region,year,Units) |> summarize(value=sum(value)) |> ungroup() |>
      mutate(sector="residothersmodern"))

# shorten scenario name
sec_emi$scenario <- unlist(strsplit(sec_emi$scenario,split = ","))[seq(1,dim(sec_emi)[[1]]*2-1,2)]

#plot

ggplot()+
  geom_line(data=sec_emi,aes(x=year,y=value,color=sector),stat = "identity")+
  facet_grid(scenario~region)+theme_bw()


