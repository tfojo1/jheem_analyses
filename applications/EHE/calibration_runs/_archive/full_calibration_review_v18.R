source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

# load("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-09-10_C.12580.Rdata")
# simset.old.2 = simset

load("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-09-23_C.12580.Rdata")
simset.old = simset
#simset.old = copy.simulation.set(simset.old)

load("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-09-25_C.12580.Rdata") 
simset.new = simset
#simset.new = copy.simulation.set(simset.new)

balt.full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
balt.full.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(balt.full.lik$compute(simset.new$last.sim()) - balt.full.lik$compute(simset.old$last.sim()))

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.mcmc,
        #sim.manual,
        facet.by = "age", 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.mcmc,
        #sim.manual,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.mcmc,
        #sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        style.manager = location.style.manager,
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.mcmc,
        #sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #sim.mcmc,
        #sim.manual,
        style.manager = location.style.manager,
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "race", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.new$last.sim(),
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 



# cbind(simset.new$get.params("hiv.general.mortality"))
# cbind(sim.mcmc$get.params("tested"))

engine = create.jheem.engine('ehe','C.12580',end.year = 2025)
params.mcmc = simset.new$last.sim()$params
sim.mcmc = engine$run(params.mcmc)

params.manual = params.mcmc
#sim.manual = engine$run(params.manual)

params.manual["age5.proportion.tested.or"] = 1 # 0.1277324
# params.manual["age1.hiv.general.mortality.multiplier"] = 10 # 1.102325
# params.manual["age2.hiv.general.mortality.multiplier"] = 10 # 1.982174
# params.manual["age3.hiv.general.mortality.multiplier"] = 10 # 1.555549
# params.manual["age4.hiv.general.mortality.multiplier"] = 10 # 1.443718

sim.manual = engine$run(params.manual)
params.manual.2 = params.manual
params.manual.2["age5.hiv.general.mortality.multiplier"] = 10 # 1.673035 
sim.manual.2 = engine$run(params.manual.2)


