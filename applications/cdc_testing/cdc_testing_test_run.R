
print("Sourcing initial code")
source('applications/cdc_testing/cdc_testing_main.R')
source('applications/cdc_testing/cdc_testing_likelihoods.R')

#load up the test_sim.Rdata I gave you
# just need to do this once
if (1==2)
{
    load('test_sim.Rdata')
    sim$save()
}
    
# The code below gives converts this simulation to a cdc-testing simulation
# Can use this if we need to test out our likelihood
if (1==2)
{
    transmuter = create.jheem.transmuter(simulation.set = sim,
                                         to.version = 'cdct')
    cdct.sim = transmuter$transmute(1)
    
    lik = test.cdc.test.positivity.likelihood.instr$instantiate.likelihood('ehe', sim$location)
    lik$compute(cdct.sim)
}

print("Registering calibrations")
source('applications/cdc_testing/register_cdc_testing_calibration.R')


print("Doing setup calibration")
# Which runs the mcmc on the first sim

results = set.up.transmute.calibration('cdct.tx', 
                                       from.calibration.code = 'final.ehe.state', 
                                       location = 'TX', 
                                       n.chunks = 1,
                                       n.sim = 1,
                                       return.simulations = T)
