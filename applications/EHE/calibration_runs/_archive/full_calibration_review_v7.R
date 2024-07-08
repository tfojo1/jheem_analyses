source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.35620')
engine = create.jheem.engine('ehe','C.35620',end.year = 2030)

load("../jheem_analyses/prelim_results/full.with.aids_simset_2024-06-02_C.35620.Rdata")
simset.full = simset
sim.last = simset.full[simset.full$n.sim]
params.last = sim.last$params
sim.last = engine$run(parameters = params.last)

sim.manual = sim.last 
params.manual = params.last 

params.manual['hispanic.age2.aging.multiplier.1'] = 1.5 # 2.75665
params.manual['other.age1.aging.multiplier.1'] = 1.5 # 1.899276
sim.manual = engine$run(parameters = params.manual)

exp(full.lik$compute.piecewise(sim.manual) - full.lik$compute.piecewise(sim.last))

# better 
simplot(sim.last,
        sim.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

# worse 
simplot(sim.last,
        sim.manual,
        #facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        sim.manual,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        sim.manual,
        #facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 





