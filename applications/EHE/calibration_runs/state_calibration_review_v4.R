source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

sim.mcmc = extract.last.simulation.from.calibration('ehe','NY','trans.ehe.state',allow.incomplete = T)
#sim.mcmc = extract.last.simulation.from.calibration('ehe','NY','full.ehe.state',allow.incomplete = T) # this has gotten worse

engine = create.jheem.engine('ehe','NY',end.year = 2030)
params.mcmc = sim.mcmc$params

sim.mcmc = engine$run(parameters = params.mcmc) # rerun with mcmc parameters - basically the same 

params.manual = params.mcmc
params.manual["heterosexual.male.idu.susceptibility.rr"] = 5 # 1.024416
params.manual["female.idu.susceptibility.rr"] = 0.01 # 1.012324 

sim.manual = engine$run(parameters = params.manual)

#full.lik.fl$compare.sims(sim.full,sim.manual,piecewise = F)

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 



