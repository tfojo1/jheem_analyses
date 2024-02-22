#Run a single simulation in Baltimore
#source("../jheem2/R/tests/ENGINE_test.R")
#source("test/set_up_dummy_ehe_sims.R")
source("../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R")
load('../../files/simulations/ehe/manual-3/C.12580/ehe_manual-3_C.12580_baseline.Rdata')

#Plot model estimates and calibration data by race [outcome: new diagnoses]
simplot(simset, "new", split.by="race")

#Model natural course by race
noint=get.null.intervention()
sim.noint=noint$run(simset, start.year=2025, end.year=2035)
simplot(sim.noint, "new", split.by="race")

#Model full intervention by race
sim.int=test.intervention$run(simset, start.year=2025, end.year=2035, verbose=TRUE)
simplot(sim.noint, "new", split.by="race")

#Look at outcomes other than new diagnoses
simplot(sim, "aids.diagnoses") #used to calibrate model prior to ~2004
simplot(sim, "suppression") #calibration data are by county

#Model test intervention (using pre-specified parameter distribution)
sim.int=test.intervention$run(sim, start.year=2025, end.year=2035, verbose=TRUE)
simplot(sim.noint, sim.int, "incidence", split.by="race") +
  xlim(2010, 2035)

#Get numeric estimates
sim.noint$get(outcomes="incidence") #counts of new diagnoses
sim.noint$get(outcomes="incidence", year=2035, keep.dimensions="race") #by race, in year 2035

#Calculate incidence rates
ir=sim.noint$get(outcomes="incidence", year=2035, keep.dimensions="race") / sim.noint$get(outcomes="population", year=2035, keep.dimensions="race")*100000
#Note: could subtract out pop with prevalent hiv to get pop at risk of hiv

#Calculate incidence rate ratios
irr_black_oth = ir[c("black"),] / ir[c("other"),] #take mean and quantiles across sims for CI
irr_black_oth

irr_hisp_oth = ir["hispanic",] / ir["other",]
irr_hisp_oth
