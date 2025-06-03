source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

supp.trans.lik = race.risk.suppression.basic.likelihood.instructions$instantiate.likelihood('ehe','TX')
supp.full.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe','TX')

trans.lik = trans.state.likelihood.instructions$instantiate.likelihood('ehe','TX')
full.lik = full.state.likelihood.instructions$instantiate.likelihood('ehe','TX')


load("~/jheem/code/jheem_analyses/prelim_results/full.ehe.state_simset_2025-05-08_TX.Rdata")
simset.tx.full = simset

load("~/jheem/code/jheem_analyses/prelim_results/trans.ehe.state_simset_2025-05-07_TX.Rdata")
simset.tx.trans = simset

simplot(simset.tx.trans$first.sim(),
        simset.tx.trans$last.sim(),
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.trans$first.sim(),
        simset.tx.trans$last.sim(),
        facet.by = "race", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.trans$first.sim(),
        simset.tx.trans$last.sim(),
        facet.by = "risk", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# knows that last is worse
supp.trans.lik$compare.sims(simset.tx.trans$first.sim(),simset.tx.trans$last.sim())
# but everything else got better
trans.lik$compare.sims(simset.tx.trans$first.sim(),simset.tx.trans$last.sim())

simplot(simset.tx.full$first.sim(),
        simset.tx.full$last.sim(),
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.full$first.sim(),
        simset.tx.full$last.sim(),
        facet.by = "race", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.full$first.sim(),
        simset.tx.full$last.sim(),
        facet.by = "risk", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.full$first.sim(),
        simset.tx.full$last.sim(),
        facet.by = "age", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.tx.full$first.sim(),
        simset.tx.full$last.sim(),
        facet.by = "sex", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# says full is way worse; but they're basically the same?
supp.full.lik$compare.sims(simset.tx.full$first.sim(),simset.tx.full$last.sim())
# almost everything else got better
full.lik$compare.sims(simset.tx.full$first.sim(),simset.tx.full$last.sim())


sim.last = simset.tx.full$last.sim()
sim.trans.first = simset.tx.trans$first.sim()
params.last = sim.last$params
params.trans.first = sim.trans.first$params
params.manual = params.last

#engine = create.jheem.engine('ehe','TX',end.year = 2030)
#sim.manual = engine$run(parameters = params.manual)

round(cbind(sim.trans.first$get.params("suppress"),
            sim.last$get.params("suppress"),
            sim.manual$get.params("suppress")),3)

params.manual["black.suppressed.slope.or"] = 0.95 # 1.040
params.manual["hispanic.suppressed.slope.or"] = 0.95 # 1.064

sim.manual = engine$run(parameters = params.manual)

#supp.full.lik$compare.sims(sim.last,sim.manual)

# suppression better but everything else worse 
# new looks worse and scores worse; prev looks better but scores worse!
full.lik$compare.sims(sim.last,sim.manual,piecewise = T) 

simplot(sim.last,
        sim.manual,
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        facet.by = "race", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        facet.by = "age", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        facet.by = "sex", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        outcomes = c("new"), 
        #facet.by = "risk", split.by = "race",  # risk/race, risk/sex, age/sex
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        #facet.by = "risk", split.by = "race", # risk/race, risk/sex, age/sex
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 
