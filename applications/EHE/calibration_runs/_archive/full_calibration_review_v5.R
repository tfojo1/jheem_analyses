source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')
trans.lik = two.way.transmission.pop.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

load("../jheem_analyses/prelim_results/init.pop.ehe_simset_2024-05-10_C.12580.Rdata")
simset.pop = simset
sim.pop.last = simset.pop[simset.pop$n.sim]
sim.pop = simset.pop$burn(keep = 1)

load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-05-14_C.12580.Rdata")
simset.trans = simset
sim.trans.last = simset.trans[simset.trans$n.sim]
sim.trans = simset.trans$burn(keep = 1)

load("../jheem_analyses/prelim_results/full.with.aids_simset_2024-05-16_C.12580.Rdata")
simset.full = simset
sim.full.last = simset.full[simset.full$n.sim]
sim.full = simset.full$burn(keep = 1)


# TRANS versus POP
exp(full.lik$compute.piecewise(sim.trans) - full.lik$compute.piecewise(sim.pop)) 
# TRANS better than POP for everything except pop, test positivity, prep (x2), idu (x2)
exp(full.lik$compute(sim.trans) - full.lik$compute(sim.pop))
# TRANS inf better than POP as a whole

# FULL versus POP
exp(full.lik$compute.piecewise(sim.full) - full.lik$compute.piecewise(sim.pop)) 
    # FULL better than POP for everything except pop, imm, aids diag, & prop tested
exp(full.lik$compute(sim.full) - full.lik$compute(sim.pop))
    # FULL inf better than POP as a whole

# FULL versus TRANS
exp(full.lik$compute.piecewise(sim.full) - full.lik$compute.piecewise(sim.trans)) 
    # FULL better than TRANS for everything except pop, imm, em, aids diag, & prop tested
exp(full.lik$compute(sim.full) - full.lik$compute(sim.trans))
    # FULL inf better than TRANS as a whole

# re: population, it goes pop calibration (best by FAR) >>>>> trans calibration > full calibration 

params.pop.last = sim.pop.last$params
params.trans.last = sim.trans.last$params
params.full.last = sim.full.last$params

params.pop.first = sim.pop$params
params.trans.first = sim.trans$params
params.full.first = sim.full$params

round(cbind(params.pop[par.names.transmission],
            params.trans.trans.rolled.back[par.names.transmission]
            #params.full[par.names.transmission]
            ),3)

round(cbind(params.pop[par.names.pop],
            params.trans[par.names.pop],
            params.full[par.names.pop]),3)

round(cbind(params.pop.last[par.names.transmission],
            params.trans.last[par.names.transmission]),3)

params.trans.pop.rolled.back = params.trans
params.trans.pop.rolled.back[par.names.pop] = params.pop[par.names.pop]

params.full.pop.rolled.back = params.full
params.full.pop.rolled.back[par.names.pop] = params.pop[par.names.pop]

params.full.pop.rolled.back.to.trans = params.full
params.full.pop.rolled.back.to.trans[par.names.pop] = params.trans[par.names.pop]

exp(trans.lik$compute.piecewise(sim.trans) - trans.lik$compute.piecewise(sim.pop)) 
exp(trans.lik$compute.piecewise(sim.pop) - trans.lik$compute.piecewise(sim.trans)) 
exp(trans.lik$compute.piecewise(sim.trans.with.orig.pop) - trans.lik$compute.piecewise(sim.trans)) 

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
sim.trans.with.orig.pop = engine$run(parameters = params.trans.pop.rolled.back)
sim.full.with.orig.pop = engine$run(parameters = params.full.pop.rolled.back)
sim.full.with.trans.pop = engine$run(parameters = params.full.pop.rolled.back.to.trans)

params.trans.trans.rolled.back = params.trans
incident.idu.params = par.names.transmission[grepl("incident",par.names.transmission)]
params.trans.trans.rolled.back[incident.idu.params] = params.pop[incident.idu.params]
params.trans.trans.rolled.back[par.names.pop] = params.pop[par.names.pop]

sim.trans.with.orig.trans = engine$run(parameters = params.trans.trans.rolled.back)

params.test = params.pop.last
params.test["global.trate"] = 0.05

sim.test = engine$run(parameters = params.test)

simplot(#simset.pop,
        #simset.trans,
        #simset.full,
        sim.pop.last,
        sim.test,
        # sim.trans.last,
        # sim.full.last,
        #sim.full,
        facet.by = "age", 
        split.by = "race", 
        outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.pop,
        sim.trans,
        sim.trans.with.orig.pop,
        #sim.full,
        #sim.full.with.orig.pop,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.pop,
  #sim.pop.last,
  sim.trans.last,
  sim.test,
  #sim.full.last,
        #sim.full.with.trans.pop,
        #sim.full,
        #sim.full.with.orig.pop,
        # facet.by = "risk",
        # split.by = "race", 
        outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        #facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        outcomes = c("cdc.hiv.test.positivity"),  # totals only 
        dimension.values = list(year = 2000:2030)) 

# Other plots to check: 
simplot(sim,
        simset,
        #facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        outcomes = c("total.mortality"), # totals only 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2001)) 

simplot(sim,
        simset,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030))

simplot(sim,
        simset,
        outcomes = c("awareness"), # totals only 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        simset,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        dimension.values = list(year = 2000:2030)) 
