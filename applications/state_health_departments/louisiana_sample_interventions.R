source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library("RColorBrewer")

pal = c(brewer.pal(n=12,"Paired")[5],
        #brewer.pal(n=12,"Paired")[2])# ,
        brewer.pal(n=12,"Paired")[4]) 

LOCATION = 'LA'
CALIBRATION.CODE = 'final.ehe.state'
YEARS.TO.CONSIDER = as.character(2026:2031)
#END.NAME = "supp.93"

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 
intervention.style.manager = create.style.manager(color.sim.by = "simset",
                                                  sim.palette = pal)

# LA plan: 
# Objective 2.1: Increase number of people screened for PrEP by 50% 
# Objective 2.2: Increease the number of people who know their status to 90% 

# PrEP uptake: 1.5x
# Awareness: 90% 

prep.uptake.1.5x = create.intervention.effect(quantity.name = "oral.prep.uptake",
                                             start.time = 2026, # Jan 1 2026
                                             effect.values = 1.5, 
                                             times = 2027, # one year rollout
                                             apply.effects.as = "multiplier",
                                             scale = "proportion", # capped at 50% anyway 
                                             allow.values.less.than.otherwise = F,
                                             allow.values.greater.than.otherwise = T)

testing.2x = create.intervention.effect(quantity.name = "general.population.testing",
                                        start.time = 2026, # Jan 1 2026
                                        effect.values = 2, 
                                        times = 2027, # one year rollout
                                        apply.effects.as = "multiplier", 
                                        scale = "rate", # to make this a rate ratio, to keep from going >1
                                        allow.values.less.than.otherwise = F,
                                        allow.values.greater.than.otherwise = T)

int.prep = create.intervention(prep.uptake.1.5x,
                               WHOLE.POPULATION,
                               code = "prep") 
int.testing = create.intervention(testing.2x,
                                  WHOLE.POPULATION,
                                  code = "testing") 
int.combined = create.intervention(prep.uptake.1.5x,
                                   testing.2x,
                                   WHOLE.POPULATION,code = "combined") 
# one intervention combining everything 
no.int = get.null.intervention()

INTERVENTIONS=c("noint", "prep","testing","combined")
sim.collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATION, interventions = INTERVENTIONS, n.sim=1000)

if(1==1){ # already ran collection
    sim.collection$run(2025, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=F)    
}

# simset.no.int = no.int$run(simset,end.year = 2035)
# simset.prep = int.prep$run(simset,end.year = 2035)
# simset.testing = int.testing$run(simset,end.year = 2035)
# simset.combined = int.combined$run(simset,end.year = 2035)


simset.baseline = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE,n.sim=1000)
simset.no.int = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "noint", n.sim=1000)
simset.prep = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "prep", n.sim=1000)
simset.testing = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "testing", n.sim=1000)
simset.combined = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "combined", n.sim=1000)

simplot(simset.baseline,
        #simset.prep,
        #simset.testing,
        simset.combined,
        outcomes = c("new"), 
        summary.type = "median.and.interval",
        #style.manager = source.style.manager, 
        style.manager = intervention.style.manager, 
        dimension.values = list(year = 2000:2035)) 

simplot(simset.baseline,
        #simset.prep,
        #simset.testing,
        #simset.combined,
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = intervention.style.manager,
        dimension.values = list(year = 2000:2030)) 

total.results = sim.collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                'suppression', 'population',
                                                'hiv.mortality',
                                                'sexual.transmission.rates','prep.uptake','testing'),
                                   output = 'numerator',
                                   dimension.values=list(year=2025:2035),
                                   keep.dimensions=c('year'),
                                   verbose = T)

total.incidence = array.access(total.results, outcome='incidence', drop = T)
#apply(total.incidence,c("year","intervention"),mean)

INT.NAME = "combined"

tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,'noint',drop=F], c('sim'), sum, na.rm=T)
tot.inf.WITH.int = apply(total.incidence[YEARS.TO.CONSIDER,,INT.NAME,drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections 2026-2031 with no intervention: ", 
             format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Infections 2026-2031 WITH intervention: ", 
             format(round(mean(tot.inf.WITH.int, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.WITH.int, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.WITH.int, probs=.975, na.rm=T)), big.mark=','),
             "]"))

abs.delta.int.tot.inf.noint1 = tot.inf.noint1 - tot.inf.WITH.int
rel.delta.int.tot.inf.noint1 = abs.delta.int.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Decrease in Infections 2026-2031 with intervention (",INT.NAME,"): ", 
             format(round(mean(abs.delta.int.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.int.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.int.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Decrease in Infections 2026-2031 with intervention (",INT.NAME,"): ", 
             format(round(mean(rel.delta.int.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.int.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.int.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))



## OLD INTERVENTIONS - SAMPLE FOR MEETING

# YBMSM = create.target.population(sex = "msm",
#                                  race = "black",
#                                  age = c("13-24 years","25-34 years"),
#                                  name = "YBMSM")

# increase prep 10%
# prep.10 = create.intervention.effect(quantity.name = "oral.prep.uptake",
#                                      start.time = 2026, # Jan 1 2026
#                                      effect.values = .10,
#                                      times = 2027, # one year rollout
#                                      apply.effects.as = "addend", # add 10% suppression to everyone
#                                      scale = "proportion",
#                                      allow.values.less.than.otherwise = F,
#                                      allow.values.greater.than.otherwise = T)
# 
# # analogous for suppression
# supp.10 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
#                                      start.time = 2026, # Jan 1 2026
#                                      effect.values = .10,
#                                      times = 2027, # one year rollout
#                                      apply.effects.as = "addend", # add 10% suppression to everyone
#                                      scale = "proportion",
#                                      allow.values.less.than.otherwise = F,
#                                      allow.values.greater.than.otherwise = T)
# 
# ## 
# supp.95 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
#                                      start.time = 2026, # Jan 1 2026
#                                      effect.values = .95,
#                                      times = 2027, # one year rollout
#                                      apply.effects.as = "value", # everyone at 90% suppression or greater
#                                      scale = "proportion",
#                                      allow.values.less.than.otherwise = F,
#                                      allow.values.greater.than.otherwise = T)
# 
# 
# simplot(#simset$last.sim(),
#     #simset,
#     simset.no.int,
#     simset.supp.10,
#     outcomes = c("new"), 
#     summary.type = "median.and.interval",
#     #style.manager = source.style.manager, 
#     style.manager = intervention.style.manager, 
#     dimension.values = list(year = 2000:2035)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "risk", split.by = "race", 
#     outcomes = c("new"), 
#     summary.type = "median.and.interval",
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "age", split.by = "sex", 
#     outcomes = c("new"), 
#     summary.type = "median.and.interval",
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     outcomes = c("diagnosed.prevalence"), 
#     summary.type = "median.and.interval",
#     style.manager = source.style.manager,
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "risk", split.by = "race",
#     outcomes = c("diagnosed.prevalence"), 
#     summary.type = "median.and.interval",
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "age", split.by = "sex", 
#     outcomes = c("diagnosed.prevalence"), 
#     summary.type = "median.and.interval",
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "race", 
#     outcomes = c("suppression"), 
#     summary.type = "median.and.interval",
#     style.manager = location.style.manager,
#     dimension.values = list(year = 2000:2030)) 
# 
# simplot(#simset$last.sim(),
#     simset,
#     facet.by = "risk", 
#     outcomes = c("suppression"), 
#     summary.type = "median.and.interval",
#     style.manager = location.style.manager,
#     dimension.values = list(year = 2000:2030)) 
# 
# 

