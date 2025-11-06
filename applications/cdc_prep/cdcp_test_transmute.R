

#load("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/test.AL.sim.Rdata")
load('test.AL.sim.Rdata')


source('applications/cdc_prep/cdc_prep_specification.R')
source('applications/cdc_prep/Surveillance_Manager_Updates.R')

transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2025, to.year = 2035)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

sim.cdcp = transmuter$transmute(1, parameters = params)

source('applications/cdc_prep/likelihood.R')
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

results.by.state[[results$location]] = results

#simplot(results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
#simplot(results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))

simplot(results$last.sim(),results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
simplot(results$last.sim(),results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))
simplot(simset$last.sim(),simset,"cumulative.cdc.prep.eligible",dimension.values = list(year = 2010:2030))
simplot(simset$last.sim(),simset,"cdc.funded.tests.nonhealthcare",dimension.values = list(year = 2010:2030))
simplot(simset$last.sim(),simset,"cumulative.fraction.referred",dimension.values = list(year = 2010:2030))
