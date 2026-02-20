

#Alabama


sim = load.simulation.set("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/test.AL.sim.Rdata")

source('applications/cdc_prep/cdc_prep_specification.R')
source('applications/cdc_prep/Surveillance_Manager_Updates.R')
source('applications/cdc_prep/likelihood.R')


transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2010, to.year = 2030)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

library(dplyr)
# setdiff <- base::setdiff
# find("setdiff")

sim.cdcp = transmuter$transmute(1, parameters = params)
range(sim.cdcp$cdc.nonfunded.general.population.testing)

lik = cdc.prep.joint.likelihood.instructions$instantiate.likelihood('cdcp', sim$location)
lik$compute(sim.cdcp)

print("Sourcing initial code")
source('applications/cdc_prep/register_cdc_prep_calibration.R')

print("Doing setup calibration")

results = set.up.transmute.calibration('cdcp', 
                                       from.calibration.code = 'final.ehe.state', 
                                       location = 'AL', 
                                       n.chunks = 1,
                                       n.sim = 10,
                                       allow.overwrite.cache = TRUE,
                                       return.simulations = T)

results = results$burn(500)

simplot(results$last.sim(),results,"testing",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cdc.funded.tests",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cumulative.cdc.prep.eligible",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cdc.fraction.prep.referred.of.eligible",dimension.values = list(year = 2014:2035))

#Louisiana 

load("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/test.LA.sim.Rdata")

source('applications/cdc_prep/cdc_prep_specification.R')
source('applications/cdc_prep/Surveillance_Manager_Updates.R')
source('applications/cdc_prep/likelihood.R')


transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2010, to.year = 2035)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

sim.cdcp = transmuter$transmute(1, parameters = params)


lik = cdc.prep.joint.likelihood.instructions$instantiate.likelihood('cdcp', sim$location)
lik$compute(sim.cdcp)

print("Sourcing initial code")
source('applications/cdc_prep/register_cdc_prep_calibration.R')

print("Doing setup calibration")

results = set.up.transmute.calibration('cdcp', 
                                       from.calibration.code = 'final.ehe.state', 
                                       location = 'LA', 
                                       n.chunks = 1,
                                       n.sim = 10,
                                       allow.overwrite.cache = TRUE,
                                       return.simulations = T)

results.LA <- results

#results.by.state[[results$location]] = results

#simplot(results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
#simplot(results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))

results = results$burn(500)

simplot(results$last.sim(),results,"testing",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cdc.funded.tests",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cumulative.cdc.prep.eligible",dimension.values = list(year = 2014:2035))
simplot(results$last.sim(),results,"cdc.fraction.prep.referred.of.eligible",dimension.values = list(year = 2014:2035))

