source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-18_C.12580.Rdata")
sim.pop.trans.mort = sim

load("../jheem_analyses/prelim_results/full_minus_prep_supp_idu_2024-04-18_C.12580.Rdata")
sim.full.minus.prep.supp.idu = sim

load("../jheem_analyses/prelim_results/full_minus_prep_supp_2024-04-18_C.12580.Rdata")
sim.full.minus.prep.supp = sim

load("../jheem_analyses/prelim_results/full_minus_prep_2024-04-18_C.12580.Rdata")
sim.full.minus.prep = sim

# params.manual = base.params
# sim.manual = engine$run(parameters = params.manual) 

#full.lik = FULL.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

(exp(full.lik$compute.piecewise(sim.full.minus.prep) - 
            full.lik$compute.piecewise(sim.pop.trans.mort)))

# likelihood says worse; is worse - working correctly
simplot(sim.pop.trans.mort,
        #sim.full.minus.prep.supp.idu,
        #sim.full.minus.prep.supp,
        sim.full.minus.prep,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says better; is better I think - working correctly 
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "age", #split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says worse; could be worse but hard to tell (both pretty good) - working correctly 
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says worse; a bit worse - working correctly 
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

# this isn't plotting sex correctly; and can't do any other dimension?
simplot(sim.full.minus.prep,
        facet.by = "sex", #split.by = "race", 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says worse; SEEMS BETTER? WORKING INCORRECTLY? something off with 2010 and 2020 (or those might be right)
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

# likelihood says worse; is worse - working correctly; pretty bad though
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says better; is better - working correctly; not great 
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("hiv.test.positivity"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says worse; is worse (but both pretty good) - working correctly
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("awareness"),
        dimension.values = list(year = 2000:2030)) 

# likelihood says better; is better - working correctly
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("suppression"), # we have age/sex/race/risk all one way
        dimension.values = list(year = 2000:2030)) 

# likelihood says better; is better - working correctly
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 

# only one extreme parameter value (msm.incident.idu.multiplier.2, 447.9) - figure out what likelihood is driving this 
# by setting value back to 1 and seeing which likelihood is worse 
cbind(round(sim.pop.trans.mort$parameters,3),
      round(sim.full.minus.prep$parameters,3))
