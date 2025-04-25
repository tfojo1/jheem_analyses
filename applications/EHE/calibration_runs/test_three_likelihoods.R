source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = "CA"

pop.lik = joint.pop.migration.total.trans.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
print("done instantiating pop")
trans.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions.state$instantiate.likelihood('ehe',LOCATION)
print("done instantiating trans")
full.lik = FULL.likelihood.instructions.32x.new.prev.state$instantiate.likelihood('ehe',LOCATION)
print("done instantiating full")
