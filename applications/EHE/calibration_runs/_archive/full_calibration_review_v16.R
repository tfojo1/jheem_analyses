source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/init.pop.ehe_simset_2024-08-23_C.26420.Rdata')
simset.houston.pop = simset
sim.houston.pop = simset.houston.pop$last.sim()

# load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-08-24_C.26420.Rdata')
# simset.houston.trans = simset
# sim.houston.trans = simset.houston.trans$last.sim()
# 
# load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-26_C.26420.Rdata')
# simset.houston.full = simset
# sim.houston.full = simset.houston.full$last.sim()

full.with.covid.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.26420')

engine = create.jheem.engine('ehe','C.26420',end.year = 2025)
params.mcmc = sim.houston.pop$params
params.manual = params.mcmc

cbind(params.manual[grepl("other",names(params.manual))])

params.manual["other.birth.rate.multiplier"] = 0.8 # 0.7228271
params.manual["other.age1.aging.multiplier.1"] = 1.3 # 1.2053845
params.manual["other.age1.aging.multiplier.2"] = 1 # 0.9382194
params.manual["other.age2.aging.multiplier.1"] = 1 # 0.9979644
params.manual["other.age2.aging.multiplier.2"] = 1 # 0.8132484
params.manual["other.age3.aging.multiplier.1"] = .9 # 0.8665266
params.manual["other.age3.aging.multiplier.2"] = .9 # 0.8001982
params.manual["other.age4.aging.multiplier.1"] = .8 # 0.7992431
params.manual["other.age4.aging.multiplier.2"] = .9 # 0.8081439

params.manual["other.non.idu.general.mortality.rate.multiplier"] = .1 # 0.75276305
# params.manual["other.age1.migration.multiplier.time.1"] = .8 # 1.4648300
# params.manual["other.age1.migration.multiplier.time.2"] = .8 # 1.038866

#sim.houston.pop = engine$run(params.mcmc)
sim.manual = engine$run(params.manual)

# manual pop is worse?? overall race looks better but not age*race
full.with.covid.lik$compare.sims(sim.houston.pop,sim.manual) 

# with female lines
simplot(sim.houston.pop,
        #sim.manual,
        facet.by = "sex", split.by = "race", # how is this sex*race plot possible? 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) #+ 
  geom_hline(yintercept = 2395617.9) + # this is what the sim total says it is - looks correct on the plot
  geom_hline(yintercept = 2197600) # this is what the surveillance manager says - incorrect on the plot??

# with male lines
simplot(sim.houston.pop,
        sim.manual,
        facet.by = "sex", #split.by = "race", # how is this sex*race plot possible? 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) + 
  geom_hline(yintercept = (2224922.0 + 119302.5)) +
  geom_hline(yintercept = 2150174)

# female/male other sums 
apply(SURVEILLANCE.MANAGER$data$adult.population$estimate$census.aggregated.adult.population$census.grouped.age$year__location__race__ethnicity__sex["2010","C.26420",-3,"not hispanic",],"sex",sum)

x = (SURVEILLANCE.MANAGER$data$adult.population$estimate$census.aggregated.adult.population$census.grouped.age$year__location__race__ethnicity["2019","C.26420",,])

# total female/male sums 
apply(SURVEILLANCE.MANAGER$data$adult.population$estimate$census.aggregated.adult.population$census.grouped.age$year__location__race__ethnicity__sex["2010","C.26420",,,],"sex",sum)

# total female/male sums for sim 
apply(sim.houston.pop$population["2010",,,,,,],"sex",sum)
apply(sim.houston.pop$population["2010",,,,,,],c("sex","race"),sum)



simplot(sim.houston.pop,
        sim.manual,
        #facet.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.houston.pop,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.houston.pop,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age",
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("cdc.hiv.test.positivity"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("aids.deaths"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
        #sim.balt.last,
         # simset.houston,
         # sim.houston.last,
        simset.nyc,
        sim.nyc.last,
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 


simplot(simset.houston.trans,sim.houston.trans,
  outcomes = c("sexual.transmission.rates"), 
  style.manager = location.style.manager,
  plot.year.lag.ratio = T,
  dimension.values = list(year = 2000:2030)) 
