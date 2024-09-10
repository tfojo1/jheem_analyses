source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-11_C.12580.Rdata')
sim.last = simset$last.sim()

# prev.lik = prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
# new.lik = new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
# 
# prev.lik$compute(sim.last, debug = T) # THIS ISN'T WORKING RIGHT NOW, SO USING SURVEILLANCE MANAGER METHOD BELOW 
# new.lik$compute(sim.last, debug = T)

## WEIGHT TO USE: 0.3214286 --> 0.3

## PREVALENCE, FROM SURVEILLANCE MANAGER ## 
x = 0
# total points 
x = x + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location["2018","C.12580"])
# one-way points
x = x + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__risk["2018","C.12580",])
total.plus.one.way.prev = x

y = 0
# total points
y = y + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location["2018","C.12580"])
# two-way points
y = y + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age__sex["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__sex["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__risk["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex__risk["2018","C.12580",,])
total.plus.two.way.prev = y

prev.weight = total.plus.one.way.prev/total.plus.two.way.prev # 0.3214286

## NEW DIAGNOSES, FROM SURVEILLANCE MANAGER ## 
x = 0
# total points 
x = x + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location["2018","C.12580"])
# one-way points
x = x + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race["2018","C.12580",])
x = x + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__risk["2018","C.12580",])
total.plus.one.way.new = x

y = 0
# total points
y = y + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location["2018","C.12580"])
# two-way points
y = y + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age__sex["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__sex["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__risk["2018","C.12580",,])
y = y + length(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex__risk["2018","C.12580",,])
total.plus.two.way.new = y

new.weight = total.plus.one.way.new/total.plus.two.way.new # 0.3214286

# weight for awareness would be 18 (as if it had one-way stratifications) - numerator from above 
# weight for new and prevalence 1/3 
# anything that's two-way stratified should be 1/3 (new/prev; maybe prep uptake); 
# anything that's 0-way stratified should have 18 (awareness, general mortality; test positivity; tests year-on-year)