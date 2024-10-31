
# Load libraries: -----------------


library(tidyverse)
library(ggplot2)
# library(arrow) # Difficult to install this on cluster
# library(readxl)


# Define settings: --------------

# Whether to plot 32 regions (R32), separated into two plots by by size of economy or
# 10 or 5 regions, as facets on a single plot (R10 or R5)
# model_regions <- "r5" # Now defined in the combine_and_plot.R script


# print("These scenarios haven't been assigned attributes:")
# print(paste0("color - ", scenarios[!(scenarios %in% names(scen.color))]))

# models <- unique(data_scen$model); models
# scenarios <- unique(data_scen$scenario); scenarios


# Select desired data: -------------
# Defined in combine_and_plot.R
data_scn <- data_scen

data_hg <- data_hist

### for global plots: -------------

vars_global <- vars |>
  mutate(use = ifelse(vars %in% c("Price|Carbon"), 0, 1))

scens_global <- scenarios_selected

### for regional plots: -------------

# Mark whether the regional plots should use each variable or not
vars_regional <- vars |>
  mutate(use = ifelse(vars %in% c("Temperature|Global Mean"), 0, 1))

scens_regional <- scenarios_selected

# #define set of emissions to plot, as well as file names to use
# vars_regional <- data.frame(vars=c("Emissions|CO2|Energy and Industrial Processes","Emissions|CO2","Primary Energy|Oil","Primary Energy|Coal","Primary Energy|Gas"
# ),
# names=c("Emi_co2_FFI","Emi_co2","PE_oil","PE_coal","PE_gas"
# ))

year_max_reg <- 2050


# Load historical results ---------------------


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



# Make figures -------------
plots <- list()


for (i in seq(1,dim(vars)[1])){

  ### plot global comparisons --------------------

  if (vars_global[i,]$use == 1 & "World" %in% regions_selected) {
    
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
  geom_line(data=data_scn|>
              filter(variable==vars_global[i,]$vars,
                     region=="World", year >2004,
                     scenario %in% scens_global),
            aes(x=year,y=value,color=scenario,linetype = model))+
  ggtitle(paste0(vars_global[i,]$vars, " - ", "Global")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_global[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_global[i,]$vars,]$unit))))) +
  theme_bw()
  #add model letter to end 
  if(length(unique(data_scn$model))>1){
    plots[[i*3-2]] <- 
      plots[[i*3-2]] +
      geom_text(data=data_scn|>
                  filter(variable==vars_global[i,]$vars,
                         region=="World", 
                         year==2100,
                         scenario %in% scens_global),
                aes(x=year+5,y=value,label=substr(model,start=1,stop=1),color=scenario)) +
      guides(linetype=guide_legend(title="Model", order = 1),
             color=guide_legend(title="Scenario", order = 2),
             shape=guide_legend(title="IEA Projection", order = 3))
  }

print(plots[[i*3-2]])

ggsave(plots[[i*3-2]], filename=file.path("figures",paste0(vars_global[i,]$names,".png")), width = 13, height = 6.5)

} # end of if statement for global plots

  
  
### plot regional comparisons --------------------

if (vars_regional[i,]$use == 1){

  if (!(model_regions %in% c("iso", "gcam32"))) {
    #### for R10 or R5 regions or other ones with few regions --------------
    
    plots[[i*3-1]] <- ggplot() +
      #historic data
      geom_line(data=data_hg|>
                  filter(
                    region %in% unique(data_scn$region),
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
                     region %in% unique(data_scn$region),
                     variable==vars_regional[i,]$vars,
                     region != "World",
                     scenario!="historical",
                     year <= year_max_reg), ## |>
                   ##mutate(region = factor(region, levels = reg_heavy)),
                 aes(x=year,y=value,shape=scenario))+
      #GCAM scenario data
      geom_line(data=data_scn |> 
                  filter(
                    ##region %in% reg_heavy,
                    variable==vars_regional[i,]$vars,
                    region != "World", 
                    scenario %in% scens_regional, 
                    year <= year_max_reg), ## |>
                  ##mutate(region = factor(region, levels = reg_heavy)),
                aes(x=year,y=value,color=scenario,linetype = model))+
      theme_bw() + 
      ggtitle(paste0(vars_regional[i,]$vars, " - ", toupper(model_regions), " regions")) +
      ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
      facet_wrap(~region, scale = "free_y", nrow = 4)  +
      scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20)) + 
      guides(linetype=guide_legend(title="Model", order = 1),
             color=guide_legend(title="Scenario", order = 2),
             shape=guide_legend(title="IEA Projection", order = 3))
    
    
    #add model letter to end 
    if(length(unique(data_scn$model))>1){
      plots[[i*3-1]] <- 
        plots[[i*3-1]] +
        geom_text(data=data_scn|>
                    filter(variable==vars_global[i,]$vars,
                           region!="World", 
                           year==2050,
                           scenario %in% scens_global),
                  aes(x=year+1,y=value,label=substr(model,start=1,stop=1),color=scenario),size=2)
    }
    
    print(plots[[i*3-1]])
    
    ggsave(plots[[i*3-1]], filename=file.path("figures",paste0(vars_regional[i,]$names,"_", model_regions, "_regions.png")), width = 13, height = 6.5)
    
    
    
  }  # end of if statement for r10 / r5 model_regions
  
  else {    
    
    # Rank regions by largest users of oil in primary energy
    regional_rank <- data_hg |>
      filter(variable == "Primary Energy|Oil") |>
      filter(year == 2022,
             region != "World") |>
      arrange(desc(value))

    # Larger economies and heavy users of energy
    reg_heavy <- regional_rank[1:16,]$region
    reg_light <- unique((data_scn[!(data_scn$region %in% c("World", reg_heavy)),])$region)
    
    
    
#### for larger economies ------------
    
if (sum(!is.na(reg_heavy)) > 0) {
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
  geom_line(data=data_scn |> 
              filter(
                region %in% reg_heavy,
                variable==vars_regional[i,]$vars,
                region != "World", 
                scenario %in% scens_regional, 
                year <= year_max_reg) |>
              mutate(region = factor(region, levels = reg_heavy)),
            aes(x=year,y=value,color=scenario, linetype=model))+
  theme_bw() + 
  ggtitle(paste0(vars_regional[i,]$vars, " - ", "Larger economies")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
  facet_wrap(~region, scale = "free_y", nrow = 4)  +
  scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20)) +
  guides(linetype=guide_legend(title="Model", order = 1),
         color=guide_legend(title="Scenario", order = 2),
         shape=guide_legend(title="IEA Projection", order = 3))

print(plots[[i*3-1]])

ggsave(plots[[i*3-1]], filename=file.path("figures",paste0(vars_regional[i,]$names,"_", model_regions, "_largerEconomies.png")), width = 13, height = 6.5)

} # end of if statement for reg_heavy

    
### for smaller economies -------------------
    
if (sum(!is.na(reg_light)) > 0) {
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
  geom_line(data=data_scn |> 
              filter(
                region %in% reg_light,
                variable==vars_regional[i,]$vars,
                region != "World", 
                scenario %in% scens_regional, 
                year <= year_max_reg),
            aes(x=year,y=value,color=scenario, linetype=model))+
  theme_bw() + 
  ggtitle(paste0(vars_regional[i,]$vars, " - ", "Smaller economies")) +
  ylab(paste0(unique(c(unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit), unique(data_hg[data_hg$variable == vars_regional[i,]$vars,]$unit))))) +
  facet_wrap(~region, scale = "free_y", nrow = 5)  +
  scale_x_continuous(breaks = seq(2020,year_max_reg,20), minor_breaks = seq(2000,year_max_reg,5), labels = seq(2020,year_max_reg,20)) +
  guides(linetype=guide_legend(title="Model", order = 1),
         color=guide_legend(title="Scenario", order = 2),
         shape=guide_legend(title="IEA Projection", order = 3))

print(plots[[i*3]])

ggsave(plots[[i*3]], filename=file.path("figures",paste0(vars_regional[i,]$names,"_", model_regions, "_smallerEconomies.png")), width = 13, height = 6.5)

} # end of if statement for reg_light

} # end of else statement for model_regions

} # end of if statement for regional variables

} # end of for loop for plotting


