source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATION = MIAMI.MSA 
CALIBRATION.CODE.TO.RUN = CALIBRATION.CODE.TRANSMISSION # or CALIBRATION.CODE.POPULATION

clear.calibration.cache(version='ehe',
                        location=LOCATION,
                        calibration.code = CALIBRATION.CODE.TO.RUN,
                        root.dir = '../test_runs')

set.up.calibration(version='ehe',
                   location=LOCATION,
                   calibration.code = CALIBRATION.CODE.TO.RUN,
                   root.dir = '../test_runs',
                   cache.frequency = 250)

set.seed(12345)
start.time = Sys.time()
print(paste0("STARTING MCMC RUN AT ",Sys.time()))
mcmc = run.calibration(version = 'ehe',
                location = LOCATION,
                calibration.code = CALIBRATION.CODE.TO.RUN,
                root.dir = '../test_runs',
                chains = 1,
                update.frequency = 100,
                update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run ",
             format(N.ITER, big.mark = ","),
             " simulations (",
             round(run.time / N.ITER, 1), " seconds per simulation on average)"))

sim = mcmc@simulations[[length(mcmc@simulations)]]

save(sim,file=paste0("prelim_results/init.transmission.sim_",Sys.Date(),"_",LOCATION,".Rdata"))
