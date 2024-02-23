

# You just need to do this once to set up dummy simsets on your local machine
if (1==2)
{
    source('test/set_up_dummy_ehe_sims.R')
}

CALIBRATION.CODE = NULL #for now, we are going to use 'uncalibrated' (ie, manually generated) simulations
LOCATIONS = c('C.12580','C.33100') # for now, Baltimore and Miami

PREP.UPSCALE.INTERVENTION.CODES = c('msmprepuse10', 'msmprepuse25',
                                    'preppers30msm', 'preppers55msm', 'preppers80msm') #@padma - update this

prep.upscale.intervention.names = c(
  "Additional 10% PrEP Use",
  "Additional 25% PrEP Use",
  "30% PrEP Persistence",
  "55% PrEP Persistence",
  "80% PrEP Persistence"
)

# Build a simset collection of the cities and interventions you want to consider
collection = create.simset.collection(version='ehe',
                                      calibration.code=CALIBRATION.CODE,
                                      locations = LOCATIONS,
                                      interventions = PREP.UPSCALE.INTERVENTION.CODES)

# Run all the interventions - you should just need to do this once
collection$run(start.year = 2025, end.year = 2035, 
               verbose = T,
               overwrite.prior = F) #NB: this last argument means it is not going to re-run interventions - so if you change the intervention, you need to set this to T to re-run

# Get statistics out of your run simsets
# Do this as often as you like
results.2035 = collection$get(outcomes = c('incidence','population'),
                         dimension.values = list(year='2035'),
                         keep.dimensions = c('race', 'sex'))

results.2025 = collection$get(outcomes = c('incidence', 'population'),
                              dimension.values = list(year='2025'),
                              keep.dimensions = c('race', 'sex'))

# you should be able to get your irr's by dividing the 'incidence' values by the 'population' values

upscale_table <- function(intervention.code, intervention.name){
  # incidence difference
  
  ## black
  black.id <- colMeans((results.2025["black","msm",,"incidence",,intervention.code] - 
                          results.2035["black","msm",,"incidence",,intervention.code])/
                         results.2035["black","msm",,"incidence",,intervention.code])
 
  
  ## hispanic
  hisp.id <- colMeans((results.2025["hispanic","msm",,"incidence",,intervention.code] - 
                         results.2035["hispanic","msm",,"incidence",,intervention.code])/
                        results.2035["hispanic","msm",,"incidence",,intervention.code])
  
  ## both 
  black.hisp.id <- colMeans(colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,intervention.code])-
                              colSums(results.2035[c("black","hispanic"),"msm",,"incidence",,intervention.code])/
                              colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,intervention.code]))
  
  
  black.msm.ir <- results.2035["black","msm",,"incidence",,intervention.code] / 
    results.2035["black","msm",,"population",,intervention.code] 
  
  hispanic.msm.ir <- results.2035["hispanic","msm",,"incidence",,intervention.code] / 
    results.2035["hispanic","msm",,"population",,intervention.code] 
  
  other.msm.ir <- results.2035["other","msm",,"incidence",,intervention.code] / 
    results.2035["other","msm",,"population",,intervention.code] 
  
  black.hisp.msm.ir <- colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,intervention.code])/
    colSums(results.2025[c("black","hispanic"),"msm",,"population",,intervention.code])
  
  black.irr <- colMeans(black.msm.ir/other.msm.ir) # black msm vs other msm IRR
  hisp.irr <- colMeans(hispanic.msm.ir/other.msm.ir) # hisp msm vs other msm IRR
  black.hisp.irr <- colMeans(black.hisp.msm.ir/other.msm.ir) # black & hisp msm vs other msm IRR
  
  black.irr.ci <- quantile(black.irr, probs = c(0.025, 0.975))
  hisp.irr.ci <- quantile(hisp.irr, probs = c(0.025, 0.975))
  black.hisp.irr.ci <- quantile(black.hisp.irr, probs = c(0.025, 0.975))
  
  
  # creating a data frame for results
  results_df <- data.frame(
    intervention = intervention.name,
    reduction_Black_MSM = black.id,
    reduction_Hispanic_MSM = hisp.id,
    reduction_Black_and_Hispanic_MSM = black.hisp.id,
    irr_Black_MSM = black.irr,
    irr_Black_LCI = black.irr.ci[1],
    irr_Black_UCI = black.irr.ci[2],
    irr_Hispanic_MSM = hisp.irr,
    irr_Hispanic_LCI = hisp.irr.ci[1],
    irr_Hispanic_UCI = hisp.irr.ci[2],
    irr_Black_and_Hispanic_MSM = black.hisp.irr,
    irr_BlackHispanic_LCI = black.hisp.irr.ci[1],
    irr_BlackHispanic_UCI = black.hisp.irr.ci[2]
  ) 
  
  # transpose the data frame
  results_df <- as.data.frame(t(results_df))
  
  return(results_df)
  
}

for (i in 1:length(prep.upscale.intervention.names)) {
    print(upscale_table(PREP.UPSCALE.INTERVENTION.CODES[i], prep.upscale.intervention.names[i]))
}

year_inc <- function(int.year, race, sex = "msm", intervention.code){
  # returns average incidence race for int.year x race x sex group across simulations for the locations
  # results.year = collection$get(outcomes = c('incidence','population'),
  #                               dimension.values = list(year=c(int.year)),
  #                               keep.dimensions = c('race', 'sex'))
  if(int.year == 2025){
    return(colMeans(results.2025[race,sex,,"incidence",,intervention.code]/
                      results.2025[race,sex,,"population",,intervention.code]))
  }
  if(int.year == 2035){
    return(colMeans(results.2035[race,sex,,"incidence",,intervention.code]/
                      results.2035[race,sex,,"population",,intervention.code]))
  }
}

# to plot reduction in incidence by code
plotinc_bycode <- function(race, intervention.code, intervention.name){
  inc_df <- as.data.frame(year_inc(2025, race, "msm", intervention.code))
  inc_df["year_2035"] <- as.data.frame(year_inc(2035, race, "msm", intervention.code))
  colnames(inc_df) <- c("year_2025", "year_2035")
  inc_df$location <- rownames(inc_df)
  
  inc_long <- pivot_longer(inc_df, 
                                cols = starts_with("year_"), 
                                names_to = c(".value", "Year"),  # separate entity and year
                                names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
                                values_to = "Value") %>% dplyr::rename(ir = year, year = Year)
  
  if(race == "other"){
    race <- "Non-Black non-Hispanic"
  }
  
  # print(inc_long)
  
  return(ggplot(data = inc_long, aes(year,ir*100000)) +
    geom_point(aes(color = location)) +
    geom_line(aes(group = location, color = location)) +
    labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
         title = paste("Reduction in HIV incidence from 2025 to 2035 with",
                       intervention.name), 
         subtitle = paste("Among the", race, "population")) +
    theme_minimal())
}

blackinc_plots <- list()
hispinc_plots <- list()
otherinc_plots <- list()

for (i in 1:length(prep.upscale.intervention.names)) {
  blackinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_byloc("black",PREP.UPSCALE.INTERVENTION.CODES[i],
                                                         prep.upscale.intervention.names[i])
  hispinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_byloc("hispanic",PREP.UPSCALE.INTERVENTION.CODES[i], 
                                                                       prep.upscale.intervention.names[i])
  otherinc_plots[[prep.upscale.intervention.names[i]]] <- plotinc_byloc("other",PREP.UPSCALE.INTERVENTION.CODES[i],
                                                                        prep.upscale.intervention.names[i])
}

plotinc_bylocation <- function(intervention.codes, intervention.names, loc) {
  all_combined_inc_long <- list()  # Initialize an empty list to store combined data for all races
  
  for (race in c("black", "hispanic", "other")) {
    combined_inc_long <- data.frame()  # Initialize an empty dataframe to store combined data for current race
    
    for (i in seq_along(intervention.codes)) {
      inc_df <- as.data.frame(year_inc(2025, race, "msm", intervention.codes[i]))
      inc_df["year_2035"] <- as.data.frame(year_inc(2035, race, "msm", intervention.codes[i]))
      colnames(inc_df) <- c("year_2025", "year_2035")
      inc_df$location <- rownames(inc_df)
      
      inc_long <- pivot_longer(inc_df,
                               cols = starts_with("year_"),
                               names_to = c(".value", "Year"),  # separate entity and year
                               names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
                               values_to = "Value") %>% dplyr::rename(ir = year, year = Year)
    
      inc_long <- subset(inc_long, location == loc)
  
      inc_long$intervention_code <- rep(intervention.codes[i], length(inc_long$ir))
      inc_long$race <- rep(race, length(inc_long$ir))
      
      # Append current inc_long to combined_inc_long
      combined_inc_long <- rbind(combined_inc_long, inc_long)
    }
    
    # Append combined_inc_long for current race to all_combined_inc_long list
    all_combined_inc_long[[race]] <- combined_inc_long
  }
  
  all_combined_inc_long <- do.call(rbind, all_combined_inc_long)
  
  print(all_combined_inc_long)

  p <- ggplot(data = all_combined_inc_long, aes(year, ir*100000, color = intervention_code)) +
    geom_point(aes(color = intervention_code)) +
    geom_line(aes(group = intervention_code, color = intervention_code)) +
    labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
         title = "Reduction in HIV incidence from 2025 to 2035 with Different Interventions",
         subtitle = paste("In location", loc)) +
    facet_wrap(~race) +  
    theme_minimal()
  
  return(p)
}

for(l in LOCATIONS){
  print(plotinc_bylocation(PREP.UPSCALE.INTERVENTION.CODES, 
                     prep.upscale.intervention.names, l))
}

# patchwork stuff ----
library(patchwork)
(blackinc_plots[[1]]+blackinc_plots[[2]]) / ((blackinc_plots[[3]]+blackinc_plots[[4]])+(blackinc_plots[[5]]))
(hispinc_plots[[1]]+hispinc_plots[[2]]) / ((hispinc_plots[[3]]+hispinc_plots[[4]])+(hispinc_plots[[5]]))
(otherinc_plots[[1]]+otherinc_plots[[2]]) / ((otherinc_plots[[3]]+otherinc_plots[[4]])+(otherinc_plots[[5]]))





#   
# blackinc_df <- as.data.frame(year_inc(2025, "black", intervention.code = "msmprepuse10")) %>% 
#   dplyr::rename(year_2025 = `year_inc(2025, "black", intervention.code = "msmprepuse10")`)
# blackinc_df["year_2035"] <- as.data.frame(year_inc(2035, "black", intervention.code = "msmprepuse10"))
# blackinc_df$location <- rownames(blackinc_df)
# 
# blackinc_long <- pivot_longer(blackinc_df, 
#                         cols = starts_with("year_"), 
#                         names_to = c(".value", "Year"),  # separate entity and year
#                         names_pattern = "^(.*?)_(.*)$",  # regex pattern to separate entity and year
#                         values_to = "Value") %>% dplyr::rename(ir = year, year = Year)         
# 
# ggplot(data = blackinc_long, aes(year,ir*100000)) +
#   geom_point(aes(color = location)) +
#   geom_line(aes(group = location, color = location)) +
#   labs(x = "Year", y = "Incidence Rate (per 100,000 population)",
#        title = paste("Reduction in HIV incidence from 2025 to 2035 with",
#                      prep.upscale.intervention.names[1]), 
#        subtitle = paste("Among the", race, "population")) +
#   theme_minimal()
# 
# 
# 
