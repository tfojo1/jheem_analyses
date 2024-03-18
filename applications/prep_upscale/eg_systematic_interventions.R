

# You just need to do this once to set up dummy simsets on your local machine
if (1==2)
{
  source('test/set_up_dummy_ehe_sims.R')
  # load simulation data
  simset$save() # this will save them in the proper place in the file tree
}

extra.params.dist = join.distributions(
  black.black.idu.oe = Uniform.Distribution(9.11,9.13),
  hispanic.hispanic.idu.oe = Uniform.Distribution(1.04,1.06),
  other.other.idu.oe = Uniform.Distribution(1.04,1.06))


CALIBRATION.CODE = 'init.transmission.ehe' #for now, we are going to use 'uncalibrated' (ie, manually generated) simulations
LOCATIONS = c('C.26420','C.33100','C.16980') 

# BALTIMORE - 'C.12580'
# HOUSTON, MIAMI, CHICAGO

PREP.UPSCALE.INTERVENTION.CODES = c('prepu10p30msm', 
                                    'prepu20p30msm', 
                                    'prepu25p30msm', 
                                    'prepu35p30msm', 
                                    'prepu40p30msm', 
                                    'prepu10p50msm', 
                                    'prepu20p50msm', 
                                    'prepu25p50msm', 
                                    'prepu35p50msm', 
                                    'prepu40p50msm', 
                                    'prepu10p80msm', 
                                    'prepu20p80msm', 
                                    'prepu25p80msm', 
                                    'prepu35p80msm', 
                                    'prepu40p80msm') #@padma - update this

prep.max.intervention.code = 'prepu60p80msm'

prep.upscale.intervention.names = c(
  "10% Use 30% Persistence",
  "20% Use 30% Persistence",
  "25% Use 30% Persistence",
  "30% Use 30% Persistence",
  "40% Use 30% Persistence",
  "10% Use 50% Persistence",
  "20% Use 50% Persistence",
  "25% Use 50% Persistence",
  "30% Use 50% Persistence",
  "40% Use 50% Persistence",
  "10% Use 80% Persistence",
  "20% Use 80% Persistence",
  "25% Use 80% Persistence",
  "30% Use 80% Persistence",
  "40% Use 80% Persistence"
)

# Build a simset collection of the cities and interventions you want to consider
collection = create.simset.collection(version='ehe',
                                      calibration.code=CALIBRATION.CODE,
                                      locations = LOCATIONS,
                                      interventions = PREP.UPSCALE.INTERVENTION.CODES,
                                      n.sim = 100)

collection.max = create.simset.collection(version='ehe',
                                      calibration.code=CALIBRATION.CODE,
                                      locations = LOCATIONS,
                                      interventions = prep.max.intervention.code,
                                      n.sim = 100)

# Run all the interventions - you should just need to do this once
collection$run(start.year = 2025, end.year = 2035, 
               verbose = T,
               overwrite.prior = F) #NB: this last argument means it is not going to re-run interventions - so if you change the intervention, you need to set this to T to re-run

collection.max$run(start.year = 2025, end.year = 2035, 
                   verbose = T,
                   overwrite.prior = F)

collection.bl = create.simset.collection(version = 'ehe', 
                                         calibration.code = CALIBRATION.CODE,
                                         locations = LOCATIONS,
                                         interventions = "prepblmsm", n.sim = 100)

collection.bl$run(start.year = 2025, end.year = 2035, 
                  verbose = T,
                  overwrite.prior = F)

# Get statistics out of your run simsets
# Do this as often as you like
results.2035 = collection$get(outcomes = c('incidence','population','prep.uptake'),
                         dimension.values = list(year='2035'),
                         keep.dimensions = c('race', 'sex'))

results.2025 = collection$get(outcomes = c('incidence', 'population','prep.uptake'),
                              dimension.values = list(year='2025'),
                              keep.dimensions = c('race', 'sex'))

results.bl.2035 = collection.bl$get(outcomes = c('incidence','population','prep.uptake'),
                              dimension.values = list(year='2035'),
                              keep.dimensions = c('race', 'sex'))

results.bl.2025 = collection.bl$get(outcomes = c('incidence', 'population','prep.uptake'),
                              dimension.values = list(year='2025'),
                              keep.dimensions = c('race', 'sex'))

# you should be able to get your irr's by dividing the 'incidence' values by the 'population' values

crude_inc_2035 <- function(intervention.code = 1, intervention.name = "", location, bl = FALSE,
                           sex = "msm"){
  
  # year is set to 2035 for all calculations below
  
  # incidence & population calculations
  
  black.hisp.inc <- sum(rowMeans(results.2035[c("black","hispanic"),sex,,"incidence",location,intervention.code]))
  black.hisp.pop <- sum(rowMeans(results.2035[c("black","hispanic"),sex,,"population",location,intervention.code]))
  
  other.inc <- mean(results.2035[c("other"),sex,,"incidence",location,intervention.code])
  other.pop <- mean(results.2035[c("other"),sex,,"population",location,intervention.code])
  
  black.hisp.bl.inc <- sum(rowMeans(results.bl.2035[c("black","hispanic"),sex,,"incidence",location,]))
  black.hisp.bl.pop <- sum(rowMeans(results.bl.2035[c("black","hispanic"),sex,,"population",location,]))
  
  other.bl.inc <- mean(results.bl.2035["other",sex,,"incidence",location,])
  other.bl.pop <- mean(results.bl.2035["other",sex,,"population",location,])
  
  # incidence rate per 100k population
  
  per.pop <- 100000
  
  black.hisp.ir.2035 <- (black.hisp.inc/black.hisp.pop) * per.pop
  other.ir.2035 <- (other.inc/other.pop) * per.pop

  black.hisp.ir.bl.2035 <- (black.hisp.bl.inc/black.hisp.bl.pop) * per.pop
  other.ir.bl.2035 <- (other.bl.inc/other.bl.pop) * per.pop
  
  if(bl==TRUE){
    results_df <- data.frame(
      location = location,
      intervention = "baseline",
      blackhisp.ir =  black.hisp.ir.bl.2035, 
      other.ir = other.ir.bl.2035 
    ) 
  }
  if(bl==FALSE){
    results_df <- data.frame(
      location = location,
      intervention = intervention.name,
      black.hisp.ir.2035 = black.hisp.ir.2035,
      other.ir.2035 = other.ir.2035
    ) 
  }
  
  # transpose the data frame
  results_df <- as.data.frame(t(results_df))
  
  return(results_df)
}

# Crude Incidence results

# baseline
baseline_irr <- list()
for (i in LOCATIONS) {
  baseline_irr[[i]] <- crude_inc_2035(location=i,bl=T)
}

# intervention scenarios 
for(loc in LOCATIONS){
  for (i in 1:length(prep.upscale.intervention.names)) {
    print(crude_inc_2035(PREP.UPSCALE.INTERVENTION.CODES[i], prep.upscale.intervention.names[i], loc))
  }
}

upscale_table <- function(intervention.code = 1, intervention.name = "", location, bl = FALSE,
                          sex = "msm"){
  
  results_df <- as.data.frame(t(crude_inc_2035(intervention.code, intervention.name, location, bl, sex)))
  
  results_df$black.hisp.ir.2035 <- as.numeric(results_df$black.hisp.ir.2035)
  results_df$other.ir.2035 <- as.numeric(results_df$other.ir.2035)
  results_df$black.hisp.ir.bl <- as.numeric(baseline_irr[[location]][3,])
  results_df$other.ir.bl <- as.numeric(baseline_irr[[location]][4,])
  
  results_df$irr <- (results_df$black.hisp.ir.2035)/(results_df$other.ir.2035)
  results_df$irr.bl <- results_df$black.hisp.ir.bl/results_df$other.ir.bl
  results_df$abs.red <- results_df$irr - results_df$irr.bl
  results_df$rel.red <- results_df$abs.red/results_df$irr.bl
  
  return(t(results_df))
  
}


# IRR results
for(l in LOCATIONS){
  for (i in 1:length(prep.upscale.intervention.names)) {
    print(upscale_table(PREP.UPSCALE.INTERVENTION.CODES[i], prep.upscale.intervention.names[i], location = l))
  }
}

# minimum uptake and persistance (scenario 1 - 10% use, 30% pers)
# 
# scene1 <- data.frame()
# for(i in LOCATIONS){
#   scene1 <- rbind(scene1, t(upscale_table(PREP.UPSCALE.INTERVENTION.CODES[1],prep.upscale.intervention.names[1],i)))
# }
# 
# scene1 <- as.data.frame(scene1)
# scene1[,3:10] <- sapply(scene1[,3:10], as.numeric)
# 
# scene1 <- scene1[,c("location","irr","irr.bl")]
# 
# scene1_long <- pivot_longer(scene1, 
#                             cols = starts_with(c("irr", "irr.bl")), 
#                             names_to = "measurement", 
#                             values_to = "value")
# 
# scene1_long$measurement <- ifelse(scene1_long$measurement=="irr", 
#                                   "IRR - with intervention",
#                                   "IRR - no intervention")
# 
# scene1_long$location <- ifelse(scene1_long$location == LOCATIONS[1], "Baltimore",
#                           ifelse(scene1_long$location == LOCATIONS[2],"Houston", 
#                             ifelse(scene1_long$location == LOCATIONS[3], "Miami",
#                                    "Chicago")))
# 
# # biggest intervention (40% use, 80% pers)
# scene12 <- data.frame()
# for(i in LOCATIONS){
#   scene12 <- rbind(scene12, t(upscale_table(PREP.UPSCALE.INTERVENTION.CODES[15],
#                                           prep.upscale.intervention.names[15],i)))
# }
# 
# scene12 <- as.data.frame(scene12)
# scene12[,3:10] <- sapply(scene12[,3:10], as.numeric)
# 
# scene12 <- scene12[,c("location","irr","irr.bl")]
# 
# scene12_long <- pivot_longer(scene12, 
#                             cols = starts_with(c("irr", "irr.bl")), 
#                             names_to = "measurement", 
#                             values_to = "value")
# 
# scene12_long$measurement <- ifelse(scene12_long$measurement=="irr", 
#                                   "IRR - with intervention",
#                                   "IRR - no intervention")
# 
# scene12_long$location <- ifelse(scene12_long$location == LOCATIONS[1], "Baltimore",
#                                ifelse(scene12_long$location == LOCATIONS[2],"Houston", 
#                                       ifelse(scene12_long$location == LOCATIONS[3], "Miami",
#                                              "Chicago")))
# 
# p1 <- ggplot(scene1_long, aes(x = measurement, y = value, color = location)) +
#   geom_point() +
#   geom_line(aes(group=location)) +
#   labs(
#     title = "Comparison of IRR - 10% PrEP Use, 30% Persistence",
#     x = "Location",
#     y = "IRR",
#     color = "Measurement"
#   ) + theme_minimal()
# 
# p1
# 
# p12 <- ggplot(scene12_long, aes(x = measurement, y = value, color = location)) +
#   geom_point() +
#   geom_line(aes(group=location)) +
#   labs(
#     title = "Comparison of IRR - 40% PrEP Use, 80% Persistence",
#     x = "Location",
#     y = "IRR",
#     color = "Measurement"
#   ) + theme_minimal()
# 
# p12
# 
# # figure 3 -----
# 
# scene1_long$int <- rep("10% Additional PrEP Use, 30% Persistence", length(scene1_long$location))
# scene12_long$int <- rep("40% Additional PrEP Use, 80% Persistence", length(scene12_long$location))
# 
# combined_long <- rbind(scene1_long, scene12_long)
# 
# ggplot(combined_long, aes(x = measurement, y = value, color = location)) +
#   geom_point() +
#   geom_line(aes(group=location)) +
#   labs(
#     title = "Comparison of IRR - 40% PrEP Use, 80% Persistence",
#     x = "Location",
#     y = "IRR",
#     color = "Measurement"
#   ) +
#   facet_wrap(~int) +
#   scale_y_continuous(breaks = seq(1, 11, by = 1)) +
#   theme_minimal()


# figure 4 ------

# looking at the 10 year trend of the biggest intervention

# generating results till 2035 
results.all = collection$get(outcomes = c('incidence','population', 'prep.uptake'),
                             keep.dimensions = c('race','sex','year'))

results.bl.all = collection.bl$get(
  outcomes = c('incidence','population', 'prep.uptake'),
  keep.dimensions = c('race','sex','year'))


run.prep.code <- "prepu40p80msm"
results_inc <- list()
results_df <- data.frame()

for(l in LOCATIONS){
  for (i in 1:length(run.prep.code)) {
    for(yr in as.character(start.year:end.year)){
      # for(age in 1:5){
      bh.ir <- (sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,,"incidence",l,run.prep.code[i]]))/
                  sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,,"population",l,run.prep.code[i]]))) * 100000
      other.ir <- (mean(results.all[c("other"),"msm",yr,,"incidence",l,run.prep.code[i]])/
                     mean(results.all[c("other"),"msm",yr,,"population",l,run.prep.code[i]])) * 100000
      
      bh.bl.ir <- (sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,,"incidence",l,1]))/
        sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,,"population",l,1]))) * 100000
      other.bl.ir <- (mean(results.bl.all[c("other"),"msm",yr,,"incidence",l,1]))/
        mean(results.bl.all[c("other"),"msm",yr,,"population",l,1]) * 100000
      
      results_inc[[length(results_inc) + 1]] <- data.frame(
        year = yr,
        loc = l,
        # age_cat = age,
        intervention_code = run.prep.code[i],
        bh_ir = bh.ir,
        other_ir = other.ir,
        irr = bh.ir/other.ir,
        bh_bl_ir = bh.bl.ir,
        other_bl_ir = other.bl.ir,
        bl_irr = bh.bl.ir/other.bl.ir
      )
    }
    results_df <- do.call(rbind, results_inc)
  }
}

results_df$loc <- ifelse(results_df$loc == LOCATIONS[1], "Houston", 
                      ifelse(results_df$loc == LOCATIONS[2],"Miami","Chicago"))

library(ggplot2)

ggplot(results_df) +
  geom_line(aes(x = year, y = bh_bl_ir, group = loc), color = "grey4") +
  geom_point(aes(x = year, y = bh_bl_ir, group = loc), color = 'grey4') +
  geom_line(aes(x = year, y = bh_ir, color = loc, group = loc)) +
  geom_point(aes(x = year, y = bh_ir, color = loc, group = loc)) +
  facet_wrap(~loc, dir = "v")+
  labs(
    title = "Incidence Rate among Black/Hispanic MSM by Location",
    subtitle = "40% Additional PrEP Use, 80% PrEP Persistence",
    x = "Year",
    y = "Incidence Rate (per 100,000 population)",
    color = "Location",
    caption = "Black line refers to the baseline incidence rate."
  ) +
  theme_minimal()

ggplot(results_df) +
  geom_line(aes(x = year, y = bl_irr, group = loc), color = "grey4") +
  geom_point(aes(x = year, y = bl_irr, group = loc), color = 'grey4') +
  geom_line(aes(x = year, y = irr, color = loc, group = loc)) +
  geom_point(aes(x = year, y = irr, color = loc, group = loc)) +
  facet_wrap(~loc, dir = "v")+
  labs(
    title = "Incidence Rate Ratio by Location",
    subtitle = "40% Additional PrEP Use, 80% PrEP Persistence",
    x = "Year",
    y = "Incidence Rate Ratio",
    color = "Location",
    caption = "Black line refers to the baseline IRR."
  ) +
  theme_minimal()


# figure 5 ----- 
# looking at actual prep.uptake
# 
# run.prep.code <- "prepu40p80msm"
# results_inc <- list()
# results_df <- data.frame()
# 
# for(l in LOCATIONS){
#   for (i in 1:length(run.prep.code)) {
#     for(yr in as.character(start.year:end.year)){
#       # for(age in 1:5){
#       bh.p <- (sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]]))/
#                   sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,age,,"population",l,run.prep.code[i]]))) * 100
#       other.p <- (mean(results.all[c("other"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]])/
#                      mean(results.all[c("other"),"msm",yr,age,,"population",l,run.prep.code[i]])) * 100
#       
#       bh.bl.p <- (sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,age,,"prep.uptake",l,1]))/
#                      sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,age,,"population",l,1]))) * 100
#       other.bl.p <- (mean(results.bl.all[c("other"),"msm",yr,age,,"prep.uptake",l,1]))/
#         mean(results.bl.all[c("other"),"msm",yr,age,,"population",l,1]) * 100
#       
#       results_inc[[length(results_inc) + 1]] <- data.frame(
#         year = yr,
#         loc = l,
#         # age_cat = age,
#         intervention_code = run.prep.code[i],
#         bh_p = bh.p,
#         other_p = other.p,
#         bh_bl_p = bh.bl.p,
#         other_bl_p = other.bl.p
#       )
#     }
#     results_df <- do.call(rbind, results_inc)
#   }
# }
# 
# 
# results_df$loc <- ifelse(results_df$loc == LOCATIONS[1], "Baltimore",
#                          ifelse(results_df$loc == LOCATIONS[2],"Houston", 
#                                 ifelse(results_df$loc == LOCATIONS[3], "Miami",
#                                        "Chicago")))
# ggplot(results_df) +
#   geom_line(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_point(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_line(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   geom_point(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   facet_wrap(~age_cat)+
#   labs(
#     title = "Projected PrEP Uptake among Black/Hispanic MSM by Location",
#     subtitle = "40% Additional PrEP Use, 80% PrEP Persistence",
#     x = "Year",
#     y = "PrEP Uptake (%)",
#     color = "Location",
#     caption = "Black line refers to the baseline PrEP Uptake."
#   ) +
#   theme_minimal()
# 
# 
# run.prep.code <- "prepu10p30msm"
# results_inc <- list()
# results_df <- data.frame()
# 
# for(l in LOCATIONS){
#   for (i in 1:length(run.prep.code)) {
#     for(yr in as.character(start.year:end.year)){
#       
#       bh.p <- (sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,,"prep.uptake",l,run.prep.code[i]]))/
#                  sum(rowMeans(results.all[c("black","hispanic"),"msm",yr,,"population",l,run.prep.code[i]]))) * 100
#       other.p <- (mean(results.all[c("other"),"msm",yr,,"prep.uptake",l,run.prep.code[i]])/
#                     mean(results.all[c("other"),"msm",yr,,"population",l,run.prep.code[i]])) * 100
#       
#       bh.bl.p <- (sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,,"prep.uptake",l,1]))/
#                     sum(rowMeans(results.bl.all[c("black","hispanic"),"msm",yr,,"population",l,1]))) * 100
#       other.bl.p <- (mean(results.bl.all[c("other"),"msm",yr,,"prep.uptake",l,1]))/
#         mean(results.bl.all[c("other"),"msm",yr,,"population",l,1]) * 100
#       
#       results_inc[[length(results_inc) + 1]] <- data.frame(
#         year = yr,
#         loc = l,
#         intervention_code = run.prep.code[i],
#         bh_p = bh.p,
#         other_p = other.p,
#         bh_bl_p = bh.bl.p,
#         other_bl_p = other.bl.p
#       )
#     }
#     results_df <- do.call(rbind, results_inc)
#   }
# }
# 
# 
# results_df$loc <- ifelse(results_df$loc == LOCATIONS[1], "Baltimore",
#                          ifelse(results_df$loc == LOCATIONS[2],"Houston", 
#                                 ifelse(results_df$loc == LOCATIONS[3], "Miami",
#                                        "Chicago")))
# ggplot(results_df) +
#   geom_line(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_point(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_line(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   geom_point(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   facet_wrap(~loc)+
#   labs(
#     title = "Projected PrEP Uptake among Black/Hispanic MSM by Location",
#     subtitle = "10% Additional PrEP Use, 30% PrEP Persistence",
#     x = "Year",
#     y = "PrEP Uptake (%)",
#     color = "Location",
#     caption = "Black line refers to the baseline PrEP Uptake."
#   ) +
#   theme_minimal()

# ggplot(results_df, aes(x = year, y = irr, color = loc, group = loc)) +
#   geom_line() +
#   geom_point() +
#   labs(
#     title = "Incidence Rate Ratio by Location",
#     x = "Year",
#     y = "Incidence Rate Ratio",
#     color = "Location"
#   ) +
#   theme_minimal()
# 



# archives ----
# year_inc <- function(int.year, race, sex = "msm", intervention.code){
#   # returns average incidence race for int.year x race x sex group across simulations for the locations
#   # results.year = collection$get(outcomes = c('incidence','population'),
#   #                               dimension.values = list(year=c(int.year)),
#   #                               keep.dimensions = c('race', 'sex'))
#   if(int.year == 2025){
#     return(colMeans(results.2025[race,sex,,"incidence",,intervention.code]/
#                       results.2025[race,sex,,"population",,intervention.code]))
#   }
#   if(int.year == 2035){
#     return(colMeans(results.2035[race,sex,,"incidence",,intervention.code]/
#                       results.2035[race,sex,,"population",,intervention.code]))
#   }
# }
# 
# # to plot reduction in incidence by code
# plotinc_bycode <- function(race, intervention.code, intervention.name){
#   inc_df <- as.data.frame(year_inc(2025, race, "msm", intervention.code))
#   inc_df["year_2035"] <- as.data.frame(year_inc(2035, race, "msm", intervention.code))
#   colnames(inc_df) <- c("year_2025", "year_2035")
#   inc_df$location <- rownames(inc_df)
#   
#   inc_long <- pivot_longer(inc_df, 
#                                 cols = starts_with("year_"), 
#                                 names_to = c(".value", "Year"),  # separate entity and year
#                                 names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
#                                 values_to = "Value") %>% dplyr::rename(ir = year, year = Year)
#   
#   if(race == "other"){
#     race <- "Non-Black non-Hispanic"
#   }
#   
#   # print(inc_long)
#   
#   return(ggplot(data = inc_long, aes(year,ir*100000)) +
#     geom_point(aes(color = location)) +
#     geom_line(aes(group = location, color = location)) +
#     labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
#          title = paste("Reduction in HIV incidence from 2025 to 2035 with",
#                        intervention.name), 
#          subtitle = paste("Among the", race, "population")) +
#     theme_minimal())
# }
# 
# blackinc_plots <- list()
# hispinc_plots <- list()
# otherinc_plots <- list()
# 
# for (i in 1:length(prep.upscale.intervention.names)) {
#   blackinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_bycode("black",PREP.UPSCALE.INTERVENTION.CODES[i],
#                                                          prep.upscale.intervention.names[i])
#   hispinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_bycode("hispanic",PREP.UPSCALE.INTERVENTION.CODES[i], 
#                                                                        prep.upscale.intervention.names[i])
#   otherinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_bycode("other",PREP.UPSCALE.INTERVENTION.CODES[i],
#                                                                         prep.upscale.intervention.names[i])
# }
# 
# plotinc_bylocation <- function(intervention.codes, intervention.names, loc) {
#   all_combined_inc_long <- list()  # Initialize an empty list to store combined data for all races
#   
#   for (race in c("black", "hispanic", "other")) {
#     combined_inc_long <- data.frame()  # Initialize an empty dataframe to store combined data for current race
#     
#     for (i in seq_along(intervention.codes)) {
#       inc_df <- as.data.frame(year_inc(2025, race, "msm", intervention.codes[i]))
#       inc_df["year_2035"] <- as.data.frame(year_inc(2035, race, "msm", intervention.codes[i]))
#       colnames(inc_df) <- c("year_2025", "year_2035")
#       inc_df$location <- rownames(inc_df)
#       
#       inc_long <- pivot_longer(inc_df,
#                                cols = starts_with("year_"),
#                                names_to = c(".value", "Year"),  # separate entity and year
#                                names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
#                                values_to = "Value") %>% dplyr::rename(ir = year, year = Year)
#     
#       inc_long <- subset(inc_long, location == loc)
#   
#       inc_long$intervention_code <- rep(intervention.codes[i], length(inc_long$ir))
#       inc_long$race <- rep(race, length(inc_long$ir))
#       
#       # Append current inc_long to combined_inc_long
#       combined_inc_long <- rbind(combined_inc_long, inc_long)
#     }
#     
#     # Append combined_inc_long for current race to all_combined_inc_long list
#     all_combined_inc_long[[race]] <- combined_inc_long
#   }
#   
#   all_combined_inc_long <- do.call(rbind, all_combined_inc_long)
#   
#   print(all_combined_inc_long)
# 
#   p <- ggplot(data = all_combined_inc_long, aes(year, ir*100000, color = intervention_code)) +
#     geom_point(aes(color = intervention_code), show.legend = FALSE) +
#     geom_line(aes(group = intervention_code, color = intervention_code), show.legend = FALSE) +
#     labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
#          title = "Reduction in HIV incidence from 2025 to 2035 with Different Interventions",
#          ) +
#     facet_wrap(~race) +  
#     theme_minimal()
#   
#   return(p)
# }
# 
# cities <- c("Baltimore", "Houston",
#             "Miami", "Chicago")
# 
# combined_inc <- list()
# 
# for(l in 1:length(LOCATIONS)){
#   combined_inc[[LOCATIONS[l]]] <- plotinc_bylocation(PREP.UPSCALE.INTERVENTION.CODES, 
#                      prep.upscale.intervention.names, LOCATIONS[l]) +
#           labs(subtitle = paste("In", cities[l]))
#         
# }
# 
# library(patchwork)
# (combined_inc$C.12580 + combined_inc$C.26420)/
#   (combined_inc$C.33100 + combined_inc$C.16980)
# 
# 
# # # patchwork stuff ----
# # library(patchwork)
# # (blackinc_plots[[1]]+blackinc_plots[[2]]) / ((blackinc_plots[[3]]+blackinc_plots[[4]])+(blackinc_plots[[5]]))
# # (hispinc_plots[[1]]+hispinc_plots[[2]]) / ((hispinc_plots[[3]]+hispinc_plots[[4]])+(hispinc_plots[[5]]))
# # (otherinc_plots[[1]]+otherinc_plots[[2]]) / ((otherinc_plots[[3]]+otherinc_plots[[4]])+(otherinc_plots[[5]]))
# # 
# 
# 
# 
# 
# #   
# # blackinc_df <- as.data.frame(year_inc(2025, "black", intervention.code = "msmprepuse10")) %>% 
# #   dplyr::rename(year_2025 = `year_inc(2025, "black", intervention.code = "msmprepuse10")`)
# # blackinc_df["year_2035"] <- as.data.frame(year_inc(2035, "black", intervention.code = "msmprepuse10"))
# # blackinc_df$location <- rownames(blackinc_df)
# # 
# # blackinc_long <- pivot_longer(blackinc_df, 
# #                         cols = starts_with("year_"), 
# #                         names_to = c(".value", "Year"),  # separate entity and year
# #                         names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
# #                         values_to = "Value") %>% dplyr::rename(ir = year, year = Year)         
# # 
# # ggplot(data = blackinc_long, aes(year,ir*100000)) +
# #   geom_point(aes(color = location)) +
# #   geom_line(aes(group = location, color = location)) +
# #   labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
# #        title = paste("Reduction in HIV incidence from 2025 to 2035 with",
# #                      prep.upscale.intervention.names[1]), 
# #        subtitle = paste("Among the", race, "population")) +
# #   theme_minimal()
# # 
# # 
# # 
# 
# run.prep.code <- "prepu40p80msm"
# results_inc <- list()
# results_df <- data.frame()
# 
# for(l in LOCATIONS){
#   for (i in 1:length(run.prep.code)) {
#     for(yr in as.character(start.year:end.year)){
#       for(age in 1:5){
#         bh.p <- (mean(results.all[c("black"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]]))/
#                    (mean(results.all[c("black"),"msm",yr,age,,"population",l,run.prep.code[i]])) * 100
#         other.p <- (mean(results.all[c("other"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]])/
#                       mean(results.all[c("other"),"msm",yr,age,,"population",l,run.prep.code[i]])) * 100
#         
#         bh.bl.p <- (mean(results.bl.all[c("black"),"msm",yr,age,,"prep.uptake",l,1]))/
#                       (mean(results.bl.all[c("black"),"msm",yr,age,,"population",l,1])) * 100
#         other.bl.p <- (mean(results.bl.all[c("other"),"msm",yr,age,,"prep.uptake",l,1]))/
#           mean(results.bl.all[c("other"),"msm",yr,age,,"population",l,1]) * 100
#         
#         results_inc[[length(results_inc) + 1]] <- data.frame(
#           year = yr,
#           loc = l,
#           age_cat = age,
#           intervention_code = run.prep.code[i],
#           bh_p = bh.p,
#           other_p = other.p,
#           bh_bl_p = bh.bl.p,
#           other_bl_p = other.bl.p
#         )
#       }
#       results_df <- do.call(rbind, results_inc)
#     }}
# }
# 
# 
# results_df$loc <- ifelse(results_df$loc == LOCATIONS[1], "Baltimore",
#                          ifelse(results_df$loc == LOCATIONS[2],"Houston", 
#                                 ifelse(results_df$loc == LOCATIONS[3], "Miami",
#                                        "Chicago")))
# ggplot(results_df) +
#   geom_line(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_point(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_line(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   geom_point(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   facet_wrap(~age_cat)+
#   labs(
#     title = "Projected PrEP Uptake among Black MSM by Location",
#     subtitle = "40% Additional PrEP Use, 80% PrEP Persistence",
#     x = "Year",
#     y = "PrEP Uptake (%)",
#     color = "Location",
#     caption = "Black line refers to the baseline PrEP Uptake."
#   ) +
#   theme_minimal()
# 
# results_inc <- list()
# results_df <- data.frame()
# 
# for(l in LOCATIONS){
#   for (i in 1:length(run.prep.code)) {
#     for(yr in as.character(start.year:end.year)){
#       for(age in 1:5){
#         bh.p <- (mean(results.all[c("hispanic"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]]))/
#           (mean(results.all[c("hispanic"),"msm",yr,age,,"population",l,run.prep.code[i]])) * 100
#         other.p <- (mean(results.all[c("other"),"msm",yr,age,,"prep.uptake",l,run.prep.code[i]])/
#                       mean(results.all[c("other"),"msm",yr,age,,"population",l,run.prep.code[i]])) * 100
#         
#         bh.bl.p <- (mean(results.bl.all[c("hispanic"),"msm",yr,age,,"prep.uptake",l,1]))/
#           (mean(results.bl.all[c("hispanic"),"msm",yr,age,,"population",l,1])) * 100
#         other.bl.p <- (mean(results.bl.all[c("other"),"msm",yr,age,,"prep.uptake",l,1]))/
#           mean(results.bl.all[c("other"),"msm",yr,age,,"population",l,1]) * 100
#         
#         results_inc[[length(results_inc) + 1]] <- data.frame(
#           year = yr,
#           loc = l,
#           age_cat = age,
#           intervention_code = run.prep.code[i],
#           bh_p = bh.p,
#           other_p = other.p,
#           bh_bl_p = bh.bl.p,
#           other_bl_p = other.bl.p
#         )
#       }
#       results_df <- do.call(rbind, results_inc)
#     }}
# }
# 
# 
# results_df$loc <- ifelse(results_df$loc == LOCATIONS[1], "Baltimore",
#                          ifelse(results_df$loc == LOCATIONS[2],"Houston", 
#                                 ifelse(results_df$loc == LOCATIONS[3], "Miami",
#                                        "Chicago")))
# ggplot(results_df) +
#   geom_line(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_point(aes(x = year, y = bh_bl_p, group = loc), color = 'grey4') +
#   geom_line(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   geom_point(aes(x = year, y = bh_p, color = loc, group = loc)) +
#   facet_wrap(~age_cat)+
#   labs(
#     title = "Projected PrEP Uptake among Black MSM by Location",
#     subtitle = "40% Additional PrEP Use, 80% PrEP Persistence",
#     x = "Year",
#     y = "PrEP Uptake (%)",
#     color = "Location",
#     caption = "Black line refers to the baseline PrEP Uptake."
#   ) +
#   theme_minimal()
# 
