
print("Sourcing initial code")

 

results.by.state = list()
#load up the test_sim.Rdata I gave you
# just need to do this once
if (1==2)
{
    load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/CA.Rdata')
    simset$save()
}
    
# The code below gives converts this simulation to a cdc-testing simulation
# Can use this if we need to test out our likelihood
if (1==2)
{
    transmuter = create.jheem.transmuter(simulation.set = simset,
                                         to.version = 'cdct')

    cdct.sim = transmuter$transmute(1)
    
    lik = test.cdc.test.positivity.likelihood.instr$instantiate.likelihood('ehe', simset$location)
    lik$compute(cdct.sim)
}

print("Registering calibrations")
source('applications/cdc_testing/register_cdc_testing_calibration.R')


print("Doing setup calibration")
# Which runs the mcmc on the first sim


results = set.up.transmute.calibration('cdct', 
                                       from.calibration.code = 'final.ehe.state', 
                                       location = 'CA', 
                                       n.chunks = 1,
                                       n.sim = 10,
                                       allow.overwrite.cache = TRUE,
                                       return.simulations = T)

results.by.state[[results$location]] = results

#simplot(results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
#simplot(results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))

simplot(results$last.sim(),results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
simplot(results$last.sim(),results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))
simplot(simset$last.sim(),simset,"new",dimension.values = list(year = 2010:2030))

lik = cdc.joint.likelihood.instructions$instantiate.likelihood("cdct",results$location)
sim1 = results$first.sim()
sim2 = results$last.sim()
sim1 = results[100]

simplot(sim1,sim2,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
simplot(sim1,sim2,"cdc.funded.tests",dimension.values = list(year = 2010:2030))

lik$compare.sims(sim1,sim2)

lik$sub.likelihoods$cdc.funded.tests$compute(sim1, debug = TRUE)

engine = create.jheem.engine("cdct","CA",2030)
params0 = sim2$params
sim0 = engine$run(params0)
params1 = params0
params1["cdc.funded.diagnoses.or1"] = 0.1
params1["cdc.funded.diagnoses.or0"] = 10
sim1 = engine$run(params1)


simplot(sim0,sim1,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
simplot(sim0,sim1,"new",dimension.values = list(year = 2010:2030))

lik$compare.sims(sim0,sim1)

positivity.lik = cdc.test.positivity.likelihood.instructions$instantiate.likelihood("cdct","MS")
positivity.lik$compare.sims(sim0,sim1)
positivity.lik$compute(sim0,debug = TRUE)


calculate.density(CDC.TESTING.PARAMETERS.PRIOR,params1)/calculate.density(CDC.TESTING.PARAMETERS.PRIOR,params0)

#Alabama 
