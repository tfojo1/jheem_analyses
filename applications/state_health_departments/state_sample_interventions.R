source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library("RColorBrewer")

pal = c(brewer.pal(n=12,"Paired")[5],
        #brewer.pal(n=12,"Paired")[2])# ,
        brewer.pal(n=12,"Paired")[4]) 

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 
intervention.style.manager = create.style.manager(color.sim.by = "simset",
                                                  sim.palette = pal)


# increase prep 10%
prep.10 = create.intervention.effect(quantity.name = "oral.prep.uptake",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .10,
                                     times = 2027, # one year rollout
                                     apply.effects.as = "addend", # add 10% suppression to everyone
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

# analogous for suppression
supp.10 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .10,
                                     times = 2027, # one year rollout
                                     apply.effects.as = "addend", # add 10% suppression to everyone
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

## 
supp.95 = create.intervention.effect(quantity.name = "suppression.of.diagnosed",
                                     start.time = 2026, # Jan 1 2026
                                     effect.values = .95,
                                     times = 2027, # one year rollout
                                     apply.effects.as = "value", # everyone at 90% suppression or greater
                                     scale = "proportion",
                                     allow.values.less.than.otherwise = F,
                                     allow.values.greater.than.otherwise = T)

# YBMSM = create.target.population(sex = "msm",
#                                  race = "black",
#                                  age = c("13-24 years","25-34 years"),
#                                  name = "YBMSM")

int.prep.10 = create.intervention(prep.10,WHOLE.POPULATION,code = "prep.10") 
int.supp.10 = create.intervention(supp.10,WHOLE.POPULATION,code = "supp.10") 
int.supp.95 = create.intervention(supp.95,WHOLE.POPULATION,code = "supp.95") 
# can do a version with YBMSM

no.int = get.null.intervention()

# load a simset
simset = retrieve.simulation.set("ehe","MS","final.ehe.state",n.sim = 1000)

simset.no.int = no.int$run(simset,end.year = 2035)
simset.prep.10 = int.prep.10$run(simset,end.year = 2035)
simset.supp.10 = int.supp.10$run(simset,end.year = 2035)
simset.supp.95 = int.supp.95$run(simset,end.year = 2035)


simplot(#simset$last.sim(),
        #simset,
        simset.no.int,
        simset.supp.10,
        outcomes = c("new"), 
        summary.type = "median.and.interval",
        #style.manager = source.style.manager, 
        style.manager = intervention.style.manager, 
        dimension.values = list(year = 2000:2035)) 

simplot(#simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        summary.type = "median.and.interval",
        dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
    simset,
    facet.by = "age", split.by = "sex", 
    outcomes = c("new"), 
    summary.type = "median.and.interval",
    dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
    simset,
    outcomes = c("diagnosed.prevalence"), 
    summary.type = "median.and.interval",
    style.manager = source.style.manager,
    dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
    simset,
    facet.by = "risk", split.by = "race",
    outcomes = c("diagnosed.prevalence"), 
    summary.type = "median.and.interval",
    dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
    simset,
    facet.by = "age", split.by = "sex", 
    outcomes = c("diagnosed.prevalence"), 
    summary.type = "median.and.interval",
    dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
        simset,
        facet.by = "race", 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset$last.sim(),
        simset,
        facet.by = "risk", 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

