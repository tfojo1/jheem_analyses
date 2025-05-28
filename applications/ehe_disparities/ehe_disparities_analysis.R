#EHE Intervention Effects on Racial Disparities in HIV Incidence
#Code to run interventions and output results.

#devtools::install_github("tfojo1/jheem2")
#devtools::install_github("tfojo1/locations")
#devtools::install_github("tfojo1/distributions")

library(dplyr)
library(tidyverse)
library(tidycensus)
library(jheem2)

source("../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R")

### Load simsets and plot baseline incidence through 2025
#load_simset <- function(date, msa){
#  load(paste("../jheem_analyses/applications/ehe_disparities/simset_",date,"_C.",msa,".Rdata",sep=""))
#  simset$save()
#  apply(simset$get(outcomes='incidence',keep.dimensions='year')/simset$get(outcomes='population',keep.dimensions='year'),1,median)*100000
#  simplot(simset,'incidence', split.by='race')
#}

#load_simset(date="2025_01-31", msa="12060") #Atlanta
#load_simset(date="2025_01-31", msa="12580") #Baltimore
#load_simset(date="2025_01-31", msa="16980") #Chicago
#load_simset(date="2025_01-31", msa="26420") #Houston
#load_simset(date="2025_01-31", msa="35620") #NYC

source("../jheem_analyses/commoncode/locations_of_interest.R")
MSAS.OF.INTEREST[16:17]

CALIBRATION.CODE="final.ehe"
LOCATIONS=MSAS.OF.INTEREST[16:17] #run in parallel (total=33)
INTERVENTIONS=c("noint", "fullint")
NSIM=1000

### Run a set of interventions and select relevant results
collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = INTERVENTIONS, n.sim = NSIM)

collection$run(2025, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=F)



#################################

#Examine parameter distributions for the joint intervention

x <- collection$get.parameters(c('testing.multiplier', 'unsuppressed.multiplier', 'uninitiated.multiplier'),summary.type = 'individual.simulations')

#Overall
apply(x[,,,2], 1, quantile, probs=0.025)
apply(x[,,,2], 1, quantile, probs=0.975)

#Baltimore
apply(x[,,1,2], 1, quantile, probs=0.025)
apply(x[,,1,2], 1, quantile, probs=0.975)

#Houston
apply(x[,,2,2], 1, quantile, probs=0.025)
apply(x[,,2,2], 1, quantile, probs=0.975)

#################################

#Output incidence and population estimates

results = collection$get(outcomes = c("new", "incidence", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))
results
dim(results)

results = collection$get(outcomes = c("new", "incidence", "population"),
                         dimension.values = list(year=2025),
                         keep.dimensions = c("race"))

#################################

#Add indicators of structural racism 

#1. Residential Segregation, 2020
#https://belonging.berkeley.edu/most-least-segregated-metro-regions-2020


#2. % Below FPL & Concentration of Poverty, 2020
#Pull in 5-year ACS data on population at or below FPL in 2020
crosswalk <- read.csv("../jheem_analyses/applications/ehe_disparities/cbsa2fipsxw.csv") %>%
  filter(principalcity=="yes") #include all counties that cover the principal city of each MSA

varlist <- c("S1701_C01_001E","S1701_C02_001E","S1701_C03_001E") #denominator, numerator, percent

fpl_msa <- get_acs(geography = "cbsa",
                   variables = varlist,
                   survey = "acs5",
                   output = "wide",
                   year = 2020) %>%
  mutate(cbsacode=as.numeric(GEOID))
fpl_msa <- right_join(fpl_msa, crosswalk, join_by(cbsacode==cbsacode)) %>%
  mutate(fpl_msa=S1701_C03_001E, den_county=S1701_C01_001E, pov_county=S1701_C02_001E, msa=NAME) %>%
  select(cbsacode, msa, statename, fpl_msa, den_county, pov_county)
fpl_state <- get_acs(geography = "state",
                     variables = varlist,
                     survey = "acs5",
                     output = "wide",
                     year = 2020) %>%
  mutate(den_state=S1701_C01_001E, pov_state=S1701_C02_001E) %>%
  select(NAME, den_state, pov_state)
#sum across counties within principal city
#calculate concentration of poverty as ratio of % people at or below FPL in principal city vs. state
fpl <- distinct(left_join(fpl_msa, fpl_state, join_by(statename==NAME))) %>%
  group_by(statename) %>%
  mutate(den_city=sum(den_county),
         pov_city=sum(pov_county),
         fpl_city=pov_city/den_city,
         fpl_state=pov_state/den_state,
         conc_pov=fpl_city/fpl_state)

#categorize into high, medium, low  


#################################

#Tables

#Sum incidence and population estimates by structural racism indicators

#Sum incidence and population estimates across all MSAs
total_results <- apply(results, setdiff(names(dim(results)), "location"), sum)
#total_results
dim.names = dimnames(results)
#dim.names
dim.names$location = c(dim.names$location, "Total")
oldresults = results
results = array(0, dim = sapply(dim.names, length), dimnames = dim.names)
results[, , , "Total", ] = total_results
results[, , , dimnames(oldresults)$location, ] = oldresults
#results

#Calculate percent of new infections by race/ethnicity, averaged across simulations
pct_black <- apply(results["black", , "incidence", , ], c("location", "intervention"), sum) / apply(results[ , , "incidence", , ], c("location", "intervention"), sum)
pct_hisp <- apply(results["hispanic", , "incidence", , ], c("location", "intervention"), sum) / apply(results[ , , "incidence", , ], c("location", "intervention"), sum)
pct_other <- apply(results["other", , "incidence", , ], c("location", "intervention"), sum) / apply(results[ , , "incidence", , ], c("location", "intervention"), sum)


#Calculate incidence rates
ir_black <- apply(results["black", , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results["black", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_hisp <- apply(results["hispanic", , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results["hispanic", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_other <- apply(results["other", , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results["other", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_total <- apply(results[ , , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results[ , , "population", , ], c("sim", "location", "intervention"), sum) 

#Take the median, 2.5th and 97.5th percentile of the IRs across simulations
ir_black_median = apply(ir_black, c("location", "intervention"), median)
ir_black_lower = apply(ir_black, c("location", "intervention"), quantile, probs=0.025)
ir_black_upper = apply(ir_black, c("location", "intervention"), quantile, probs=0.975)
ir_black_medianci = paste0(round(ir_black_median*100000, 0), " (", round(ir_black_lower*100000, 0), ", ", round(ir_black_upper*100000, 0), ")")
dim(ir_black_medianci) = dim(ir_black_median)
dimnames(ir_black_medianci) = dimnames(ir_black_median)

ir_hisp_median = apply(ir_hisp, c("location", "intervention"), median)
ir_hisp_lower = apply(ir_hisp, c("location", "intervention"), quantile, probs=0.025)
ir_hisp_upper = apply(ir_hisp, c("location", "intervention"), quantile, probs=0.975)
ir_hisp_medianci = paste0(round(ir_hisp_median*100000, 0), " (", round(ir_hisp_lower*100000, 0), ", ", round(ir_hisp_upper*100000, 0), ")")
dim(ir_hisp_medianci) = dim(ir_hisp_median)
dimnames(ir_hisp_medianci) = dimnames(ir_hisp_median)

ir_other_median = apply(ir_other, c("location", "intervention"), median)
ir_other_lower = apply(ir_other, c("location", "intervention"), quantile, probs=0.025)
ir_other_upper = apply(ir_other, c("location", "intervention"), quantile, probs=0.975)
ir_other_medianci = paste0(round(ir_other_median*100000, 0), " (", round(ir_other_lower*100000, 0), ", ", round(ir_other_upper*100000, 0), ")")
dim(ir_other_medianci) = dim(ir_other_median)
dimnames(ir_other_medianci) = dimnames(ir_other_median)

ir_total_median = apply(ir_total, c("location", "intervention"), median)
ir_total_lower = apply(ir_total, c("location", "intervention"), quantile, probs=0.025)
ir_total_upper = apply(ir_total, c("location", "intervention"), quantile, probs=0.975)
ir_total_medianci = paste0(round(ir_total_median*100000, 0), " (", round(ir_total_lower*100000, 0), ", ", round(ir_total_upper*100000, 0), ")")
dim(ir_total_medianci) = dim(ir_total_median)
dimnames(ir_total_medianci) = dimnames(ir_total_median)
ir_total_medianci

#Calculate IRD
ird_black <- ir_black - ir_other
ird_black_median = apply(ird_black, c("location", "intervention"), median)
ird_black_lower = apply(ird_black, c("location", "intervention"), quantile, probs=0.025)
ird_black_upper = apply(ird_black, c("location", "intervention"), quantile, probs=0.975)
ird_black_medianci = paste0(round(ird_black_median*100000, 0), " (", round(ird_black_lower*100000, 0), ", ", round(ird_black_upper*100000, 0), ")")
dim(ird_black_medianci) = dim(ird_black_median)
dimnames(ird_black_medianci) = dimnames(ird_black_median)

ird_hisp <- ir_hisp - ir_other
ird_hisp_median = apply(ird_hisp, c("location", "intervention"), median)
ird_hisp_lower = apply(ird_hisp, c("location", "intervention"), quantile, probs=0.025)
ird_hisp_upper = apply(ird_hisp, c("location", "intervention"), quantile, probs=0.975)
ird_hisp_medianci = paste0(round(ird_hisp_median*100000, 0), " (", round(ird_hisp_lower*100000, 0), ", ", round(ird_hisp_upper*100000, 0), ")")
dim(ird_hisp_medianci) = dim(ird_hisp_median)
dimnames(ird_hisp_medianci) = dimnames(ird_hisp_median)

#change change in IRR in each simulation
#Calculate IRR
log_irr_black <- log(ir_black / ir_other)
log_irr_black_median = apply(log_irr_black, c("location", "intervention"), median)
log_irr_black_lower = apply(log_irr_black, c("location", "intervention"), quantile, probs=0.025)
log_irr_black_upper = apply(log_irr_black, c("location", "intervention"), quantile, probs=0.975)
irr_black_medianci = paste0(round(exp(log_irr_black_median), 1), " (", round(exp(log_irr_black_lower), 1), ", ", round(exp(log_irr_black_upper), 1), ")")
dim(irr_black_medianci) = dim(log_irr_black_median)
dimnames(irr_black_medianci) = dimnames(log_irr_black_median)

log_irr_hisp <- log(ir_hisp / ir_other)
log_irr_hisp_median = apply(log_irr_hisp, c("location", "intervention"), median)
log_irr_hisp_lower = apply(log_irr_hisp, c("location", "intervention"), quantile, probs=0.025)
log_irr_hisp_upper = apply(log_irr_hisp, c("location", "intervention"), quantile, probs=0.975)
irr_hisp_medianci = paste0(round(exp(log_irr_hisp_median), 1), " (", round(exp(log_irr_hisp_lower), 1), ", ", round(exp(log_irr_hisp_upper), 1), ")")
dim(irr_hisp_medianci) = dim(log_irr_hisp_median)
dimnames(irr_hisp_medianci) = dimnames(log_irr_hisp_median)

#Combine and output to CSV
table = cbind("IR NoInt Overall" = ir_total_medianci[, "noint"],
              "IR NoInt Black" = ir_black_medianci[, "noint"],
              "IR NoInt Hispanic" = ir_hisp_medianci[, "noint"],
              "IR NoInt Other" = ir_other_medianci[, "noint"],
              "IRD NoInt Black" = ird_black_medianci[, "noint"],
              "IRD NoInt Hispanic" = ird_hisp_medianci[, "noint"],
              "IRR NoInt Black" = irr_black_medianci[, "noint"],
              "IRR NoInt Hispanic" = irr_hisp_medianci[, "noint"],
              "IR Int Overall" = ir_total_medianci[, "fullint"],
              "IR Int Black" = ir_black_medianci[, "fullint"],
              "IR Int Hispanic" = ir_hisp_medianci[, "fullint"],
              "IR Int Other" = ir_other_medianci[, "fullint"],
              "IRD Int Black" = ird_black_medianci[, "fullint"],
              "IRD Int Hispanic" = ird_hisp_medianci[, "fullint"],
              "IRR Int Black" = irr_black_medianci[, "fullint"],
              "IRR Int Hispanic" = irr_hisp_medianci[, "fullint"]
)

table


write.csv(table, file="../../code/jheem_analyses/applications/ehe_disparities/table.csv")


#################################

#Figures
#Estimated incidence rates by race/ethnicity under null vs. full intervention

#Atlanta
#no intervention
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.12060',intervention.code='noint',sub.version=NULL,n.sim=100)
load(file)
noint=simset
apply(noint$get(outcomes='incidence',keep.dimensions='year')/noint$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(noint,'incidence', split.by='race', summary.type = 'median.and.interval')

#full intervention (through 2030)
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.12060',intervention.code='fullint',sub.version=NULL,n.sim=100)
load(file)
int=simset
apply(int$get(outcomes='incidence',keep.dimensions='year')/int$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(int,'incidence', split.by='race', summary.type = 'median.and.interval')


#Baltimore
#no intervention
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.12580',intervention.code='noint',sub.version=NULL,n.sim=100)
load(file)
noint=simset
apply(noint$get(outcomes='incidence',keep.dimensions='year')/noint$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(noint,'incidence', split.by='race', summary.type = 'median.and.interval')

#full intervention (through 2030)
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.12580',intervention.code='fullint',sub.version=NULL,n.sim=100)
load(file)
int=simset
apply(int$get(outcomes='incidence',keep.dimensions='year')/int$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(int,'incidence', split.by='race', summary.type = 'median.and.interval')

#Houston
#no intervention
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.26420',intervention.code='noint',sub.version=NULL,n.sim=100)
load(file)
noint=simset
apply(noint$get(outcomes='incidence',keep.dimensions='year')/noint$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(noint,'incidence', split.by='race', summary.type = 'median.and.interval')

#full intervention (through 2030)
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.26420',intervention.code='fullint',sub.version=NULL,n.sim=100)
load(file)
int=simset
apply(int$get(outcomes='incidence',keep.dimensions='year')/int$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(int,'incidence', split.by='race', summary.type = 'median.and.interval')

#New York
#no intervention
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.35620',intervention.code='noint',sub.version=NULL,n.sim=100)
load(file)
noint=simset
apply(noint$get(outcomes='incidence',keep.dimensions='year')/noint$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(noint,'incidence', split.by='race', summary.type = 'median.and.interval')

#full intervention (through 2030)
file=get.simset.filename("ehe",calibration.code='full.with.covid2',location='C.35620',intervention.code='fullint',sub.version=NULL,n.sim=100)
load(file)
int=simset
apply(int$get(outcomes='incidence',keep.dimensions='year')/int$get(outcomes='population',keep.dimensions='year'),1,median)*100000
simplot(int,'incidence', split.by='race', summary.type = 'median.and.interval')
