
# Load libraries: -----------------


library(tidyverse)
library(ggplot2)
# library(arrow) # Difficult to install this on cluster
# library(readxl)


# Define settings: --------------

# Location of scenario results to analyze, relative to this R script:
 scen_file <- "toReplaceWithFileLoc"
 #scen_file <- "data/scenMIP_snapshot_1001.csv"


# Whether to plot 32 regions (R32), separated into two plots by by size of economy or
# 10 regions, as facets on a single plot (R10)
r_option <- "R32"

#define set of variables to plot
vars <- data.frame(
  vars=c(
    "Temperature|Global Mean","Emissions|CO2|Energy and Industrial Processes","Emissions|CO2","Emissions|Kyoto Gases","Primary Energy|Oil","Primary Energy|Coal","Primary Energy|Coal|Non-electricity (appr)","Primary Energy|Gas",
    "Secondary Energy|Electricity|Solar","Secondary Energy|Electricity|Wind","Secondary Energy|Electricity|Hydro","Secondary Energy|Electricity|Nuclear",
    "Secondary Energy|Electricity|Coal","Secondary Energy|Electricity|Gas","Final Energy|Industry","Final Energy|Industry|Solids|Coal", "Final Energy|Transportation", 
    "Population","GDP|PPP", "Price|Carbon", "Final Energy|Residential and Commercial", "Electrification|Transportation", "Electricity|Share|Solar+Wind"
),
names=c("Temp","Emi_co2_FFI","Emi_co2","Emi_kyo","PE_oil","PE_coal","PE_coal_nonElec","PE_gas",
        "Elec_solar","Elec_wind","Elec_hydro","Elec_nuclear",
        "Elec_coal","Elec_gas","FE_ind", "FE_ind_coal","FE_trp" , 
        "Pop","GDP_ppp", "Carbon_price", "FE_rescom", "Elec_trp","SW_share"
)
)


# Load scenario data
if (grepl("csv", scen_file)){
  data_smip <- read.csv(scen_file)
  
} else if (grepl("parquet", scen_file)){
  data_smip <- read_parquet(scen_file) |>
    filter(grepl("R10", region) | grepl("World", region))
}


if (!("year" %in% names(data_smip))){
  data_smip <- data_smip |>
    rename_with(tolower) |>
    pivot_longer(cols = -c(model, variable, region, unit, scenario),
                 names_to = "year",
                 values_to = "value") |>
    mutate(year=as.numeric(substr(year,start=2,stop=5))) 
    
}

data_smip <- data_smip |>
  rename_with(tolower) |>
  arrange(model, region, year, variable, scenario) |>
  filter(!is.na(value)) |>
  mutate(value=as.numeric(value)) |>
         #filtering out SSP2 VL variant, should not be needed later...
          filter(scenario!="scenmip_vl_290824") |>
  mutate(scenario = gsub("scenmip_","",scenario))

## Add any additional variables
data_smip <- data_smip |>
  bind_rows(data_smip |>
              filter(variable %in% c("Primary Energy|Coal", "Secondary Energy|Electricity|Coal")) |>
              group_by(model, scenario, region, year) |>
              mutate(value = value[variable == "Primary Energy|Coal"] - 1/0.39 * value[variable == "Secondary Energy|Electricity|Coal"],
                     variable = "Primary Energy|Coal|Non-electricity (appr)") |>
              unique())

## Add any additional variables
data_smip <- data_smip |>
  bind_rows(data_smip |>
              filter(variable %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity|Wind")) |>
              group_by(model, scenario, region, year) |>
              mutate(value = 100*(value[variable == "Secondary Energy|Electricity|Solar"] + value[variable == "Secondary Energy|Electricity|Wind"])/value[variable == "Secondary Energy|Electricity"],
                     variable = "Electricity|Share|Solar+Wind") |>
              unique())
## Add any additional variables
data_smip <- data_smip |>
  bind_rows(data_smip |>
              filter(model != "IMAGE 3.4",
                       variable %in% c("Final Energy|Transportation|Electricity", "Final Energy|Transportation")) |>
              group_by(model, scenario, region, year) |>
              mutate(value = 100*(value[variable == "Final Energy|Transportation|Electricity"])/value[variable == "Final Energy|Transportation"],
                     variable = "Electrification|Transportation") |>
              unique())


data_smip <- data_smip |>
  mutate(scenario = gsub("SSP5 - High Emissions", "h5", scenario),
         scenario = gsub("SSP3 - High Emissions", "h3", scenario),
         scenario = gsub("SSP2 - Medium Emissions", "m", scenario),
         scenario = gsub("SSP2 - Medium-Low Emissions", "ml", scenario),
         scenario = gsub("SSP1 - Low Emissions", "l1", scenario),
         scenario = gsub("SSP2 - Low Emissions", "l2", scenario),
         scenario = gsub("SSP1 - Low Overshoot", "lo1", scenario),
         scenario = gsub("SSP2 - Low Overshoot", "lo2", scenario),
         scenario = gsub("SSP1 - Very Low Emissions", "vl", scenario))

Scenarios <- unique(data_smip$scenario); Scenarios

scen_h5 <- Scenarios[grepl("h5", Scenarios)]
scen_h3 <- Scenarios[grepl("h3", Scenarios)]
scen_ml <- Scenarios[grepl("ml", Scenarios)]
scen_lo1 <- Scenarios[grepl("lo1", Scenarios)]
scen_lo2 <- Scenarios[grepl("lo2", Scenarios)]
scen_vl <- Scenarios[grepl("vl", Scenarios)]
scen_m <- Scenarios[grepl("m", Scenarios) & !(Scenarios %in% scen_ml) & !(grepl("SSP", Scenarios))]
scen_l1 <- Scenarios[grepl("l1", Scenarios)]
scen_l2 <- Scenarios[grepl("l2", Scenarios)]
scen_l <- Scenarios[grepl("l", Scenarios) & !(Scenarios %in% scen_ml) & !(Scenarios %in% scen_lo1) & !(Scenarios %in% scen_lo2) & !(Scenarios %in% scen_vl) & !(Scenarios %in% scen_l1) & !(Scenarios %in% scen_l2)]

# color by scenario
scen.color <- c( "#007200", "#008c00",
                 "#00c7b3", "#80ffff",
                 "#00c5ff", "#36d7ff",
                 "#80ffff", "#36d7ff",
                 "#94a2ff", "#b1bdff",
                 "#ce7497", "#eb8eb1",
                 "#ffb837", "#ffd555","#ffcf8f",
                 "#d27a00", "#f19400",
                 "#e12925", "#ff6b54",
                 "#830064", "#a0007e"
                 )

names(scen.color) <- c(scen_vl[1:2], scen_l1[1:2], scen_l2[1:2], scen_l[1:2], scen_lo1[1:2], scen_lo2[1:2], scen_ml[1:3], scen_m[1:2], scen_h3[1:2], scen_h5[1:2]) 


colScaleScen <- scale_colour_manual(name = "Scenario",
                                    values = scen.color,
                                    na.translate = FALSE,
                                    guide = guide_legend(reverse = F, ncol = 1))
fillScaleScen <- scale_fill_manual(name = "Scenario",
                                   values = scen.color,
                                   na.translate = FALSE,
                                   guide = guide_legend(reverse = F, ncol = 1))

print("These scenarios haven't been assigned attributes:")
print(paste0("color - ", Scenarios[!(Scenarios %in% names(scen.color))]))

# filter data: only medium, highest, and lowest per model
if(r_option == "R10"){
data_smip <- rbind(
              data_smip |> filter(scenario %in% c("m","vl","h5")),
              data_smip |> filter(scenario %in% c("lo2"),model %in% c("COFFEE 1.5","REMIND-MAgPIE 3.4-4.8"))
              )
}

Models <- unique(data_smip$model); Models



print(paste0("There are ", 
             length(unique(data_smip$region))-1, 
             " regions in the scenario data, not including World, and ",
             r_option, 
             " plots are being produced. Change r_option in this R script if a different setting is desired."))


### for global plots: -------------

vars_global <- vars |>
  mutate(use = ifelse(vars %in% c("Price|Carbon"), 0, 1))

scens_global <- Scenarios

### for regional plots: -------------

# Mark whether the regional plots should use each variable or not
vars_regional <- vars |>
  mutate(use = ifelse(vars %in% c("Temperature|Global Mean"), 0, 1))

scens_regional <- Scenarios

# #define set of emissions to plot, as well as file names to use
# vars_regional <- data.frame(vars=c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2","Primary Energy|Oil","Primary Energy|Coal","Primary Energy|Gas"
# ),
# names=c("Emi_co2_FFI","Emi_co2","PE_oil","PE_coal","PE_gas"
# ))

year_max_reg <- 2050

# Group economies by size and usage of energy
reg_heavy <- c("China","USA","EU-15","India","Japan","South Korea",
               "Indonesia","Brazil","Canada", "Africa_Eastern", "Middle East", "Russia")
reg_light <- unique((data_smip[!(data_smip$region %in% c("World", reg_heavy)),])$region)


# Load historical results ---------------------


#load historical data with gcam32 region mapping
if(r_option != "R10"){ data_hg <- read.csv("/scratch/zt1/project/cgsmodel/shared/historic_data repo/output/historical_ref_gcam32.csv") }
if(r_option == "R10"){data_hg <- read.csv("C:/Users/bertram/Documents/Data/hist/historic_data_dev_master_new/output/historical_ref_gcam32.csv") }

colnames(data_hg) <- gsub("^X", "", colnames(data_hg))
 
data_hg <- data_hg |>
   pivot_longer(cols = -c(model, variable, region, unit, scenario),
                names_to = "year",
                values_to = "value") |>
  arrange(model, region, year, variable, scenario) |>
   filter(!is.na(value)) |> 
   mutate(value=as.numeric(value),
          year=as.numeric(year))
if(r_option == "R10"){
data_hg <- data_hg |> filter(region =="World") |> 
             rbind(read.csv("C:/Users/bertram/Documents/Data/hist/historic_data_dev_master_new/output/historical_r10.csv") )
data_hg <- data_hg |>
  mutate(region = gsub("R10AFRICA", "Africa (R10)", region),
         region = gsub("R10CHINA+", "China+ (R10)", region),
         region = gsub("R10EUROPE", "Europe (R10)", region),
         region = gsub("R10INDIA+", "India+ (R10)", region),
         region = gsub("R10LATIN_AM", "Latin America (R10)", region),
         region = gsub("R10MIDDLE_EAST", "Middle East (R10)", region),
         region = gsub("R10NORTH_AM", "North America (R10)", region),
         region = gsub("R10PAC_OECD", "Pacific OECD (R10)", region),
         region = gsub("R10REF_ECON", "Reforming Economies (R10)", region),
         region = gsub("R10REST_ASIA", "Rest of Asia (R10)", region))

}

## Add any additional variables
data_hg <- data_hg |>
  bind_rows(data_hg |>
              filter(variable %in% c("Primary Energy|Coal", "Secondary Energy|Electricity|Coal"),
                     model == "Stat. Rev. World Energy Data") |>
              complete(nesting(scenario, region, unit, model, year), 
                       variable = c("Primary Energy|Coal", "Secondary Energy|Electricity|Coal"), 
                       fill = list(value = 0)) |>
              group_by(model, scenario, region, year) |>
              mutate(value = value[variable == "Primary Energy|Coal"] - 1/0.39 * value[variable == "Secondary Energy|Electricity|Coal"],
                     variable = "Primary Energy|Coal|Non-electricity (appr)") |>
              ungroup() |>
              mutate(value = ifelse(value < 0, 0, value)) |>
              unique())

#add PV and CSP for IEA data
data_hg <- data_hg |> bind_rows(data_hg |>
                                  filter(variable %in% c("Secondary Energy|Electricity|Solar|PV","Secondary Energy|Electricity|Solar|CSP"),
                                         model=="IEA WEO 2023")|>
                                  complete(nesting(scenario, region, unit, model, year), 
                                           variable = c("Secondary Energy|Electricity|Solar|PV","Secondary Energy|Electricity|Solar|CSP"), 
                                           fill = list(value = 0)) |>
                                  group_by(model, scenario, region, year) |>
                                  mutate(value = value[variable == "Secondary Energy|Electricity|Solar|PV"] + value[variable == "Secondary Energy|Electricity|Solar|CSP"],
                                         variable = "Secondary Energy|Electricity|Solar") |>
                                  ungroup() |>
                                  mutate(value = ifelse(value < 0, 0, value)) |>
                                  unique())

## Add any additional variables
data_hg <- data_hg |>
  bind_rows(data_hg |>
              filter(year %in% c(seq(2010,2021),2030,2050),variable %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Solar", "Secondary Energy|Electricity|Wind")) |>
              group_by(model, scenario, region, year) |>
              mutate(value = 100*(value[variable == "Secondary Energy|Electricity|Solar"] + value[variable == "Secondary Energy|Electricity|Wind"])/value[variable == "Secondary Energy|Electricity"],
                     variable = "Electricity|Share|Solar+Wind") |>
              unique())
## Add any additional variables
data_hg <- data_hg |>
  bind_rows(data_hg |>
              filter(variable %in% c("Final Energy|Transportation|Electricity", "Final Energy|Transportation")) |>
              group_by(model, scenario, region, year) |>
              mutate(value = 100*(value[variable == "Final Energy|Transportation|Electricity"])/value[variable == "Final Energy|Transportation"],
                     variable = "Electrification|Transportation") |>
              unique())

# # Rank regions by largest users of oil in primary energy
# regional_rank <- data_hg |>
#   filter(variable == "Primary Energy|Oil") |>
#   filter(year == 2022,
#          region != "World") |>
#   arrange(desc(value))
# 
# # Large economies and heavy users of energy
# reg_heavy <- regional_rank[1:16,]$region
# reg_light <- unique((data_smip[!(data_smip$region %in% c("World", reg_heavy)),])$region)


# Make figures -------------
plots <- list()


for(i in seq(1,dim(vars)[1])){

  ### plot global comparisons --------------------

  if (vars_global[i,]$use == 1){
    
  plots[[i*3-2]] <- ggplot()+
    #historic data
  geom_line(data=data_hg|>
              filter(
                variable==vars_global[i,]$vars,region=="World",
                scenario=="historical",
                model!="IEA WEO 2023"),
            aes(x=year,y=value,linetype=model))+
    #IEA WEO reference data
  geom_point(data=data_hg|>
               filter(
                 variable==vars_global[i,]$vars,
                 region=="World",
                 scenario!="historical",
                 year%in% c(2030,2050,2100)),
             aes(x=year,y=value,shape=scenario))+
    #(GCAM) scenario data
  geom_line(data=data_smip|>
              filter(variable==vars_global[i,]$vars,
                     region=="World", year >2004,
                     scenario %in% scens_global),
            aes(x=year,y=value,color=scenario,linetype = model))+
  colScaleScen + 
  ggtitle(paste0(vars_global[i,]$vars, " - ", "Global")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_global[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_global[i,]$vars,]$unit))))) +
  theme_bw()
  #add model letter to end 
  if(length(unique(data_smip$model))>1){
    plots[[i*3-2]] <- 
      plots[[i*3-2]] +
      geom_text(data=data_smip|>
                  filter(variable==vars_global[i,]$vars,
                         region=="World", 
                         year==2100,
                         scenario %in% scens_global),
                aes(x=year+5,y=value,label=substr(model,start=1,stop=1),color=scenario))
  }

print(plots[[i*3-2]])

ggsave(plots[[i*3-2]], filename=file.path("../figures",paste0(vars_global[i,]$names,".png")), width = 13, height = 6.5)

} # end of if statement for global plots

  
  
### plot regional comparisons --------------------

if (vars_regional[i,]$use == 1){

  if (r_option == "R10") {
    
    #### for R10 regions --------------
    
    plots[[i*3-1]] <- ggplot() +
      #historic data
      geom_line(data=data_hg|>
                  filter(
                    region %in% unique(data_smip$region),
                    variable==vars_regional[i,]$vars,
                    region != "World",
                    scenario=="historical",
                    model!="IEA WEO 2023", 
                    year <= year_max_reg), ## |>
                  ##mutate(region = factor(region, levels = reg_heavy)),
                aes(x=year,y=value,linetype=model)) +
      #IEA WEO reference data
      geom_point(data=data_hg|>
                   filter(
                     region %in% unique(data_smip$region),
                     variable==vars_regional[i,]$vars,
                     region != "World",
                     scenario!="historical",
                     year <= year_max_reg), ## |>
                   ##mutate(region = factor(region, levels = reg_heavy)),
                 aes(x=year,y=value,shape=scenario))+
      #GCAM scenario data
      geom_line(data=data_smip |> 
                  filter(
                    ##region %in% reg_heavy,
                    variable==vars_regional[i,]$vars,
                    region != "World", 
                    scenario %in% scens_regional, 
                    year <= year_max_reg), ## |>
                  ##mutate(region = factor(region, levels = reg_heavy)),
                aes(x=year,y=value,color=scenario,linetype = model))+
      colScaleScen + 
      theme_bw() + 
      ggtitle(paste0(vars_regional[i,]$vars, " - ", "R10 regions")) +
      ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
      facet_wrap(~region, scale = "free_y", nrow = 4)  +
      scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20))
    
    
    #add model letter to end 
    if(length(unique(data_smip$model))>1){
      plots[[i*3-1]] <- 
        plots[[i*3-1]] +
        geom_text(data=data_smip|>
                    filter(variable==vars_global[i,]$vars,
                           region!="World", 
                           year==2050,
                           scenario %in% scens_global),
                  aes(x=year+1,y=value,label=substr(model,start=1,stop=1),color=scenario),size=2)
    }
    
    print(plots[[i*3-1]])
    
    ggsave(plots[[i*3-1]], filename=file.path("../figures",paste0(vars_regional[i,]$names,"_R10 regions.png")), width = 13, height = 6.5)
    
    
    
  }  # end of if statement for R10 r_option
  
  else {    
    
#### for large economies ------------
plots[[i*3-1]] <- ggplot() +
  #historic data
  geom_line(data=data_hg|>
              filter(
                region %in% reg_heavy,
                variable==vars_regional[i,]$vars,
                region != "World",
                scenario=="historical",
                model!="IEA WEO 2023", 
                year <= year_max_reg) |>
              mutate(region = factor(region, levels = reg_heavy)),
            aes(x=year,y=value,linetype=model)) +
  #IEA WEO reference data
  geom_point(data=data_hg|>
               filter(
                 region %in% reg_heavy,
                 variable==vars_regional[i,]$vars,
                 region != "World",
                 scenario!="historical",
                 year <= year_max_reg) |>
               mutate(region = factor(region, levels = reg_heavy)),
             aes(x=year,y=value,shape=scenario))+
  #GCAM scenario data
  geom_line(data=data_smip |> 
              filter(
                region %in% reg_heavy,
                variable==vars_regional[i,]$vars,
                region != "World", 
                scenario %in% scens_regional, 
                year <= year_max_reg) |>
              mutate(region = factor(region, levels = reg_heavy)),
            aes(x=year,y=value,color=scenario))+
  colScaleScen + 
  theme_bw() + 
  ggtitle(paste0(vars_regional[i,]$vars, " - ", "Large economies")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
  facet_wrap(~region, scale = "free_y", nrow = 4)  +
  scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20))

print(plots[[i*3-1]])

ggsave(plots[[i*3-1]], filename=file.path("../figures",paste0(vars_regional[i,]$names,"_largeEconomies.png")), width = 13, height = 6.5)


### for smaller economies -------------------
plots[[i*3]] <- ggplot() +
  #historic data
  geom_line(data=data_hg|>
              filter(
                region %in% reg_light,
                variable==vars_regional[i,]$vars,
                region != "World",
                scenario=="historical",
                model!="IEA WEO 2023", 
                year <= year_max_reg),
            aes(x=year,y=value,linetype=model)) +
  #IEA WEO reference data
  geom_point(data=data_hg|>
               filter(
                 region %in% reg_light,
                 variable==vars_regional[i,]$vars,
                 region != "World",
                 scenario!="historical",
                 year <= year_max_reg),
             aes(x=year,y=value,shape=scenario))+
  #GCAM scenario data
  geom_line(data=data_smip |> 
              filter(
                region %in% reg_light,
                variable==vars_regional[i,]$vars,
                region != "World", 
                scenario %in% scens_regional, 
                year <= year_max_reg),
            aes(x=year,y=value,color=scenario))+
  colScaleScen + 
  theme_bw() + 
  ggtitle(paste0(vars_regional[i,]$vars, " - ", "Smaller economies")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
  facet_wrap(~region, scale = "free_y", nrow = 5)  +
  scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20))

print(plots[[i*3]])

ggsave(plots[[i*3]], filename=file.path("../figures",paste0(vars_regional[i,]$names,"_smallerEconomies.png")), width = 13, height = 6.5)

} # end of else statement for r_option

} # end of if statement for regional variables

} # end of for loop for plotting


