
# 4/8 runs 
load("~/jheem/code/jheem_analyses/prelim_results/trans.ehe.state_simset_2025-04-08_CA.Rdata")
simset.trans.4.8 = simset # 500 sims
# 92 sims
simset.full.4.8 = assemble.simulations.from.calibration(version = 'ehe',
                                                        location = 'CA',
                                                        calibration.code = 'full.ehe.state',
                                                        allow.incomplete = T) 

# the one we reran with 18x weight for AIDS diagnoses - 450 sims
simset.trans.4.9 = assemble.simulations.from.calibration(version = 'ehe',
                                                        location = 'CA',
                                                        calibration.code = 'trans.ehe.state',
                                                        allow.incomplete = T)

# the one todd is running until 1997 - 350 sims
simset.trans.1997 = assemble.simulations.from.calibration(version = 'ehe',
                                                         location = 'CA',
                                                         calibration.code = 'trans.ehe.state.97',
                                                         allow.incomplete = T)

# the one todd is running until 2007 - 185 sims
simset.trans.2007 = assemble.simulations.from.calibration(version = 'ehe',
                                                          location = 'CA',
                                                          calibration.code = 'trans.ehe.state.07',
                                                          allow.incomplete = T)

# one without hiv mort - 120 sims 
simset.no.hiv.mort = assemble.simulations.from.calibration(version = 'ehe',
                                               location = 'CA',
                                               calibration.code = 'trans.no.hiv.mort',
                                               allow.incomplete = T)

# aids diagnoses weight back to 1 and hiv.mort weight to 1/16 - 95 sims 
simset.test.2 = assemble.simulations.from.calibration(version = 'ehe',
                                                           location = 'CA',
                                                           calibration.code = 'trans.test.2',
                                                           allow.incomplete = T)


# 18x weight AIDS diagnoses has one sim that looks good (7th sim)
# what made it get worse?
sim.good = simset.trans.4.9$subset(7) 
sim.last = simset.trans.4.9$last.sim()

trans.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions.state$instantiate.likelihood('ehe','CA')
# says last sim is better for everything except total new, total prev (and pop)
trans.lik$compare.sims(sim.good,sim.last)


# same thing as above - calibration without HIV mortality has one sim that looks good (also the 7th sim...)
# what made it get worse?
sim.good.2 = simset.no.hiv.mort$subset(7)
sim.last.2 = simset.no.hiv.mort$last.sim()

trans.lik.no.hiv.mort = transmission.pop.idu.aware.aids.testing.likelihood.instructions.state.4.9$instantiate.likelihood('ehe','CA')
# says last sim is better for everything except total prevalence 
trans.lik$compare.sims(sim.good.2,sim.last.2)

## now trying the aids diagnoses weighted at 1 and hiv mortality weighted at 1/16
trans.lik.test.2 = transmission.pop.idu.aware.aids.testing.likelihood.instructions.state.4.9.2$instantiate.likelihood('ehe','CA')
# last sim is better for everything except hiv mortality; makes sense 
trans.lik.test.2$compare.sims(simset.test.2$first.sim(),simset.test.2$last.sim())


simplot(sim.good,
        sim.last,
        outcomes = c("aids.diagnoses","new","diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good.2,
        sim.last.2,
        simset.no.hiv.mort,
        outcomes = c("aids.diagnoses","new","diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.test.2$first.sim(),
        simset.test.2$last.sim(),
        simset.test.2,
        outcomes = c("aids.diagnoses","new","diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.test.2$first.sim(),
        simset.test.2$last.sim(),
        simset.test.2,
        outcomes = c("hiv.mortality"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        outcomes = c("hiv.mortality"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        facet.by = "risk", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.good,
        sim.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

# new ones 
simplot(#simset.no.hiv.mort$subset(7),
        simset.no.hiv.mort$last.sim(),
        simset.no.hiv.mort,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.test.2$last.sim(),
        simset.test.2,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 


## other comparisons 
simplot(simset.trans.4.8$last.sim(),
        simset.trans.4.8,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.full.4.8$last.sim(),
        simset.full.4.8,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans.4.9$last.sim(),
        simset.trans.4.9,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans.1997$last.sim(),
        simset.trans.1997,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans.2007$last.sim(),
        simset.trans.2007,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 
