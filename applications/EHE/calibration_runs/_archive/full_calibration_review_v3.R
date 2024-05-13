source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

load("../jheem_analyses/prelim_results/base.plus.prep_2024-04-23_C.12580.Rdata")
sim.prep = sim

load("../jheem_analyses/prelim_results/full.with.aids.minus.prep_2024-04-20_C.12580.Rdata")
sim.full.minus.prep = sim

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

round(exp(full.lik$compute.piecewise(sim.prep) -
       full.lik$compute.piecewise(sim.full.minus.prep)))

params.original = sim.full.minus.prep$params
params.prep = sim.prep$params
params.prep.manual = params.prep
sim.prep.manual = engine$run(parameters = params.prep.manual)

# migration.params = names(params.prep.manual[grepl("migration",names(params.prep.manual))])
# indications.params = names(params.prep.manual[grepl("indications",names(params.prep.manual))])

# these fix pop (somewhat) but mess up prep and new diagnoses 
params.prep.manual["other.migration.multiplier.time.1"] = params.original["other.migration.multiplier.time.1"]
params.prep.manual["black.migration.multiplier.time.1"] = params.original["black.migration.multiplier.time.1"]
params.prep.manual["other.migration.multiplier.time.2"] = params.original["other.migration.multiplier.time.2"]
params.prep.manual["black.migration.multiplier.time.2"] = params.original["black.migration.multiplier.time.2"]
params.prep.manual["age3.migration.multiplier.time.1"] = params.original["age3.migration.multiplier.time.1"]
params.prep.manual["age2.migration.multiplier.time.1"] = params.original["age2.migration.multiplier.time.1"]

# these fix prep 
params.prep.manual["prep.indications.slope.or"] = .005  # 0.7914658 
params.prep.manual["msm.prep.indications.or"] = params.prep.manual["non.msm.prep.indications.or"] = .01
# params.prep.manual[indications.params]

sim.prep.manual = engine$run(parameters = params.prep.manual)

cbind(round(params.original,3), 
      round(params.prep.manual,3))

simplot(sim.full.minus.prep,
        sim.prep,
        sim.prep.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
  sim.full.minus.prep,
        # sim.prep,
        # sim.prep.manual,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("prep.uptake","prep.indications"),
        dimension.values = list(year = 2000:2030)) 

# really messes up new though 
simplot(sim.prep,
        sim.prep.manual,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

round(exp(full.lik$compute.piecewise(sim.prep.manual) -
            full.lik$compute.piecewise(sim.prep)))


## Old code
if(1==2){
  load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-18_C.12580.Rdata")
  sim.pop.trans.mort = sim
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "age", #split.by = "race", 
          outcomes = c("immigration","emigration"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          facet.by = "race", #split.by = "race", 
          outcomes = c("new"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          facet.by = "race", #split.by = "race", 
          outcomes = c("diagnosed.prevalence"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("hiv.mortality"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("total.mortality"),
          dimension.values = list(year = 2000:2030)) 
  
  # this isn't plotting? probably an issue because the data are cumulative estimates; can take sums from sim
  # undershooting so maybe we need to add aids.diagnoses back in 
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("aids.deaths"), 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "risk", #split.by = "race", 
          outcomes = c("aids.diagnoses"), 
          dimension.values = list(year = 1980:2030)) 
  
  simplot(sim,
          sim.pop.trans.mort,
          sim.full.minus.prep,
          facet.by = "risk", #split.by = "race", 
          outcomes = c("proportion.general.population.tested"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("hiv.test.positivity"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("awareness"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("suppression"), # we have age/sex/race/risk all one way
          dimension.values = list(year = 2000:2030)) 
  
  simplot(sim.pop.trans.mort,
          sim.full.minus.prep,
          #facet.by = "sex", #split.by = "race", 
          outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
          dimension.values = list(year = 2000:2030)) 
}

