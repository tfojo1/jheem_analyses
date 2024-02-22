

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

# Reduction in HIV incidence among MSM from 2025 to 2035 in Black and Hispanic communities

# incidence difference
# 
# ## black
# black.id <- colMeans((results.2025["black","msm",,"incidence",,"msmprepuse10"] - 
#            results.2035["black","msm",,"incidence",,"msmprepuse10"])/
#            results.2035["black","msm",,"incidence",,"msmprepuse10"])
# 
# print(paste("Reduction in Incidence among Black MSM population with", intervention.name))
# black.id
# 
# ## hispanic
# print(paste("Reduction in Incidence among Hispanic MSM population with", intervention.name))
# hisp.id <- colMeans((results.2025["hispanic","msm",,"incidence",,"msmprepuse10"] - 
#            results.2035["hispanic","msm",,"incidence",,"msmprepuse10"])/
#   results.2035["hispanic","msm",,"incidence",,"msmprepuse10"])
# 
# ## both 
# print(paste("Reduction in Incidence among Black and Hispanic MSM population with", intervention.name))
# black.hisp.id <- colMeans(colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,"msmprepuse10"])-
#            colSums(results.2035[c("black","hispanic"),"msm",,"incidence",,"msmprepuse10"])/
#            colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,"msmprepuse10"]))
# 
# # incidence rate among MSM
# 
# # calculating IRR 
# 
# black.msm.ir <- results.2035["black","msm",,"incidence",,"msmprepuse10"] / 
#   results.2035["black","msm",,"population",,"msmprepuse10"] 
# 
# hispanic.msm.ir <- results.2035["hispanic","msm",,"incidence",,"msmprepuse10"] / 
#   results.2035["hispanic","msm",,"population",,"msmprepuse10"] 
# 
# other.msm.ir <- results.2035["other","msm",,"incidence",,"msmprepuse10"] / 
#   results.2035["other","msm",,"population",,"msmprepuse10"] 
# 
# black.hisp.msm.ir <- colSums(results.2025[c("black","hispanic"),"msm",,"incidence",,"msmprepuse10"])/
#   colSums(results.2025[c("black","hispanic"),"msm",,"population",,"msmprepuse10"])
# 
# colMeans(black.msm.ir/other.msm.ir) # black msm vs other msm IRR
# colMeans(hispanic.msm.ir/other.msm.ir) # hisp msm vs other msm IRR
# colMeans(black.hisp.msm.ir/other.msm.ir) # black & hisp msm vs other msm IRR
