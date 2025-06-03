source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

simset = assemble.simulations.from.calibration(version = 'ehe',
                                               location = 'AL',
                                               calibration.code = CALIBRATION.CODE.TRANS.STATE,
                                               allow.incomplete = T)

trans.lik = trans.state.halfx.likelihood.instructions$instantiate.likelihood('ehe','AL')
# individual likelihoods (at the end of this code)
race.lik = race.one.way$instantiate.likelihood('ehe','AL')
risk.lik = risk.one.way$instantiate.likelihood('ehe','AL')
two.way.lik = two.way$instantiate.likelihood('ehe','AL')


sim.first = simset$first.sim()
sim.last = simset$last.sim()

trans.lik$compare.sims(sim.first,sim.last)
# says all of these are infinitely better for sim.last: 
race.lik$compare.sims(sim.first,sim.last) 
        # sim first way overshoots hispanic (z score -65 in first vs 12 in last)
risk.lik$compare.sims(sim.first,sim.last) 
        # sim first way overshoots IDU (z score -47 in first vs 12 in last) 
        # sim first also overshoots MSM-IDU to a degree (z score -20 in first vs 12 in last)
two.way.lik$compare.sims(sim.first,sim.last) # sim first way overshoots hispanic IDU; other MSM-IDU 

race.lik$compute(sim.first,debug = T)
risk.lik$compute(sim.last,debug = T)

params.last = sim.last$params
params.first = sim.first$params
#params.manual = params.last
params.manual = params.first

# engine = create.jheem.engine('ehe','AL',end.year = 2030)
# sim.manual = engine$run(parameters = params.manual)

cbind(params.manual[grepl("idu",names(params.manual))]) # need to bring down early hispanic IDU, other MSM

cbind(params.first[grepl("hispanic",names(params.manual))], 
      params.manual[grepl("hispanic",names(params.manual))])

# using PARAMS.LAST: 
# params.manual["other.msm.trate.peak"] = 3 # 0.535817485
# params.manual["other.msm.trate.0"] = 4 # 1.571965311
# params.manual["other.msm.trate.1"] = 15 # 37.408041798
# params.manual["other.msm.trate.2"] = 15 # 52.992069044
# params.manual["hispanic.idu.trate.peak"] = .00001 # 0.068470788
# params.manual["hispanic.idu.trate.0"] = .000001 # 0.460761799
# params.manual["hispanic.idu.trate.1"] = .000001 # 0.498705623
# params.manual["hispanic.idu.trate.2"] = .000001 # 0.921397717
# 
# params.manual["other.heterosexual.trate.peak"] = 1 # 2.844281860
# params.manual["other.heterosexual.trate.0"] = 1.5 # 1.289950544
# params.manual["other.heterosexual.trate.1"] = 1.5 # 0.893097940
# params.manual["other.heterosexual.trate.2"] = 1.5 # 1.658529007

# using PARAMS.FIRST: 
params.manual["hispanic.idu.trate.peak"] = 0 # 1.545396 # NOT DOING MUCH
params.manual["hispanic.idu.trate.0"] = 0 # 1.55556 
params.manual["hispanic.idu.trate.1"] = 0 # 2.280923 
params.manual["hispanic.idu.trate.2"] = 0 # 2.704871 

params.manual["hispanic.female.idu.susceptibility.rr"] = 0 # 0.5402180  
params.manual["hispanic.heterosexual.male.idu.susceptibility.rr"] = 0 # 0.5491589  

if(1==2){
    params.manual["other.msm.trate.peak"] = 2.5 # 1.647622 
    # params.manual["other.msm.trate.0"] =  # 2.21725 
    # params.manual["other.msm.trate.1"] =  # 2.313908 
    params.manual["other.msm.trate.2"] = 3 # 4.018139 
    params.manual["black.msm.trate.peak"] = 3 # 1.719302  
    params.manual["black.msm.trate.0"] = 3 # 1.922096 
    # params.manual["black.msm.trate.1"] =  # 3.063101  
    # params.manual["black.msm.trate.2"] = # 3.5364  
    
    params.manual["hispanic.heterosexual.trate.peak"] = 0.01 # 0.77951999
    params.manual["hispanic.heterosexual.trate.0"] = 0.01 # 1.47816661
    params.manual["hispanic.heterosexual.trate.1"] = 1 # 2.30544899 
    params.manual["hispanic.heterosexual.trate.2"] = 1 # 2.73395610
    
    params.manual["black.heterosexual.trate.peak"] = 0.01 # 0.66071641
    params.manual["black.heterosexual.trate.0"] = 0.01 # 1.06363507
    params.manual["black.heterosexual.trate.1"] = 1 # 2.30544899
    params.manual["black.heterosexual.trate.2"] = 1 # 2.73395610 
}

sim.manual = engine$run(parameters = params.manual)

simplot(sim.first,
        #sim.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030))

simplot(sim.first,
        #sim.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030))

simplot(sim.first,
        #sim.last,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#sim.first,
        sim.last,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#sim.first,
        sim.last,
        sim.manual,
        facet.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 



race.one.way = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                         outcome.for.sim = "diagnosed.prevalence",
                                         dimensions = c("race"),
                                         levels.of.stratification = c(1), 
                                         from.year = 2008, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = DIAGNOSES.CV.STATE,
                                         error.variance.type = 'cv',
                                         minimum.error.sd = 1,
                                         weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                         equalize.weight.by.year = T
    )

risk.one.way = 
    create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                           outcome.for.sim = "diagnosed.prevalence",
                                           dimensions = c("risk"),
                                           levels.of.stratification = c(1), 
                                           from.year = 2008, 
                                           observation.correlation.form = 'compound.symmetry', 
                                           error.variance.term = DIAGNOSES.CV.STATE,
                                           error.variance.type = 'cv',
                                           minimum.error.sd = 1,
                                           weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                           equalize.weight.by.year = T
)

two.way = 
create.basic.likelihood.instructions(outcome.for.data = "diagnosed.prevalence",
                                     outcome.for.sim = "diagnosed.prevalence",
                                     dimensions = c("race","risk"),
                                     levels.of.stratification = c(2), 
                                     from.year = 2008, 
                                     observation.correlation.form = 'compound.symmetry', 
                                     error.variance.term = DIAGNOSES.CV.STATE,
                                     error.variance.type = 'cv',
                                     minimum.error.sd = 1,
                                     weights = (0.5), #list(0.3), # see prev_new_aware_weighting.R 
                                     equalize.weight.by.year = T
)
