source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library("RColorBrewer")

pal = c(brewer.pal(n=12,"Paired")[5],
        #brewer.pal(n=12,"Paired")[2])# ,
        brewer.pal(n=12,"Paired")[4]) 

LOCATION = 'MS'
CALIBRATION.CODE = 'final.ehe.state'
YEARS.TO.CONSIDER = as.character(2026:2031)
#END.NAME = "supp.93"

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 
intervention.style.manager = create.style.manager(color.sim.by = "simset",
                                                  sim.palette = pal)

# MS plan: 
# PrEP 
    # Knowledge of PrEP: Increase the percentage of individuals aware of PrEP to 95% by 2025
    # Screening for PrEP: Increase the percentage of individuals who are screened for PrEP to 85% by 2025
    # Uptake of PrEP: Increase uptake of PrEP to 50% by 2025 - in model
    # Retention on PrEP at 4 months: Increase retention on PrEP to 75% by 2025 - "persistence" in model (check name)
# Viral suppression 
    # (within 6 months of initial diagnosis) to 90% by 2025.

# PrEP uptake: 85% screened * 50% uptake = 0.425
# 75% retention --> "oral.prep.persistence"
# VS: 90% (even though it's at 6 months, just using this)

prep.uptake.425 = create.intervention.effect(quantity.name = "oral.prep.uptake",
                                             start.time = 2026, # Jan 1 2026
                                             effect.values = 0.425, 
                                             times = 2027, # one year rollout
                                             apply.effects.as = "value",
                                             scale = "proportion",
                                             allow.values.less.than.otherwise = F,
                                             allow.values.greater.than.otherwise = T)

prep.persistence.75 = create.intervention.effect(quantity.name = "oral.prep.persistence",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .75, 
                                     times = 2027, # one year rollout
                                     apply.effects.as = "value",
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

supp.90 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .90,
                                     times = 2027, # one year rollout
                                     apply.effects.as = "value", # everyone at 90% suppression or greater
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

int.prep = create.intervention(prep.uptake.425,
                               prep.persistence.75,
                               WHOLE.POPULATION,code = "prep") 
int.supp.90 = create.intervention(supp.90,WHOLE.POPULATION,code = "supp.90") 
int.combined = create.intervention(prep.uptake.425,
                                   prep.persistence.75,
                                   supp.90,
                                   WHOLE.POPULATION,code = "combined") 
# one intervention combining everything 
no.int = get.null.intervention()

INTERVENTIONS=c("noint", "prep","supp.90","combined")
sim.collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATION, interventions = INTERVENTIONS, n.sim=1000)

if(1==1){ # already ran collection
    sim.collection$run(2025, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=F)    
}

# simset.no.int = no.int$run(simset,end.year = 2035)
# simset.prep.10 = int.prep.10$run(simset,end.year = 2035)
# simset.supp.10 = int.supp.10$run(simset,end.year = 2035)
# simset.supp.95 = int.supp.95$run(simset,end.year = 2035)


simset.baseline = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE,n.sim=1000)
simset.no.int = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "noint", n.sim=1000)
simset.prep = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "prep", n.sim=1000)
simset.supp.90 = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "supp.90", n.sim=1000)
simset.combined = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, intervention.code = "combined", n.sim=1000)

simplot(simset.baseline,
        #simset.prep,
        simset.supp.90,
        #simset.combined,
        outcomes = c("new"), 
        summary.type = "median.and.interval",
        #style.manager = source.style.manager, 
        style.manager = intervention.style.manager, 
        dimension.values = list(year = 2000:2035)) 

simplot(simset.baseline,
        #simset.prep,
        #simset.supp.90,
        simset.combined,
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

INT.NAME = "supp.90"

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

