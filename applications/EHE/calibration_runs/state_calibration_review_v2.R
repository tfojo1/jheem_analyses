

# what made it get worse?
sim.good = simset.trans.4$subset(2) 
sim.last = simset.trans.4$last.sim()

simplot(sim.good,
        simset.trans.4,
        sim.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

trans.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions.state.expv$instantiate.likelihood('ehe','FL')

# says last sim is better for everything except total prev, AIDS diagnoses (heroin/cocaine basically the same)
trans.lik$compare.sims(sim.good,sim.last)

engine = create.jheem.engine('ehe','FL',end.year = 2030)

params.last = sim.last$params
params.last["black.active.idu.initial.prevalence.ratio"] = 1.7

sim.manual = engine$run(parameters = params.last)

simplot(sim.last,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 1980:2030))

simplot(sim.last,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030))

trans.lik$compare.sims(sim.last,sim.manual)


## NEW total 
simplot(sim.good,
        sim.last,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

# PREV by strata
simplot(sim.good,
        sim.last,
        simset.trans.4,
        facet.by = "risk",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        facet.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#sim.good,
        sim.last,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 1980:2030))

## NEW by strata
simplot(sim.good,
        simset.trans.4,
        sim.last,
        facet.by = "risk",
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        simset.trans.4,
        sim.last,
        facet.by = "race",
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#sim.good,
        sim.last,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 1980:2030))

simplot(#sim.good,
    sim.last,
   # facet.by = "risk", #split.by = "race",
    outcomes = c("aids.diagnoses"),
    dimension.values = list(year = 1980:2030))
