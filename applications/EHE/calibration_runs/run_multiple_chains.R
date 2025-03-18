source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = CINCINATTI.MSA
CALIBRATION.CODE.TO.RUN = CALIBRATION.CODE.EHE.FINAL 
CHAIN = 4

set.seed(12345)
start.time = Sys.time()

print(ggplot2::qplot(1,1) + 
        ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                locations::get.location.name(LOCATION), " - ",
                                CALIBRATION.CODE.TO.RUN)))

print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))

mcmc = run.calibration(version = 'ehe',
                       location = LOCATION,
                       calibration.code = CALIBRATION.CODE.TO.RUN,
                       chains = CHAIN,
                       update.frequency = 100,
                       update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

# Save simset
simset = assemble.simulations.from.calibration(version = 'ehe',
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.CODE.TO.RUN)

save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",Sys.Date(),"_",LOCATION,"_",CHAIN,".Rdata"))

