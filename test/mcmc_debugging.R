source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
set.jheem.root.directory('Q:test')


library(distributions)
files = file.path('../../../commoncode/bayesian.simulations_package/bayesian.simulations/R/',
                  c(    'parallelization.R',
                        'mcmc_class.R',
                        'transformations.R',
                        'simset_class.R',
                        'sampling.R',
                        'helpers.R',
                        'merge.R',
                        'cache.R',
                        'run_interface.R',
                        'adaptive_blockwise_metropolis.R',
                        'postprocessing.R',
                        'output.R'))

for (file in files)
{
  print(file)
  source(file)
}

LOCATION = 'C.12580' #BALTIMORE.MSA 
CALIBRATION.CODE.TO.RUN = CALIBRATION.CODE.FULL.PLUS.AIDS   # CALIBRATION.CODE.POPULATION
# CALIBRATION.CODE.TRANSMISSION
# CALIBRATION.CODE.FULL.PLUS.AIDS
# CALIBRATION.CODE.FULL.MINUS.TESTING
# CALIBRATION.CODE.FULL.MINUS.IDU

set.seed(12345)

print(paste0("Setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))

# clear.calibration.cache(version='ehe',
#                         location=LOCATION,
#                         calibration.code = CALIBRATION.CODE.TO.RUN,
#                         allow.remove.incomplete = T)
# 
# set.up.calibration(version='ehe',
#                    location=LOCATION,
#                    calibration.code = CALIBRATION.CODE.TO.RUN,
#                    cache.frequency = 250)  

print(paste0("DONE setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
start.time = Sys.time()

print(ggplot2::qplot(1,1) + 
        ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                locations::get.location.name(LOCATION), " - ",
                                CALIBRATION.CODE.TO.RUN)))

print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
mcmc = run.calibration(version = 'ehe',
                       location = LOCATION,
                       calibration.code = CALIBRATION.CODE.TO.RUN,
                       chains = 1,
                       update.frequency = 100,
                       update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

mcmc = assemble.mcmc.from.cache('Q:test/mcmc_runs/ehe/C.12580/full.with.aids', allow.incomplete = T)
