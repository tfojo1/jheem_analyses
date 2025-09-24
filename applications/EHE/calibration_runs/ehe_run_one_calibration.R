
CHAIN = 1
LOCATION = 'VA'



print(paste0("run for location = ", LOCATION, ", chain = ", CHAIN))

# print('sleeping')
# Sys.sleep(10*60)
# print('waking up')


source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')


CALIBRATION.CODES.TO.RUN = 'final.ehe.state'

print(paste0("Running chain ", CHAIN, " for ", LOCATION, " - ", locations::get.location.name(LOCATION)))



for (CALIBRATION.CODE.TO.RUN in CALIBRATION.CODES.TO.RUN)
{   
   set.seed(12345)
   
  start.time = Sys.time()
    
    print(ggplot2::qplot(1,1) + 
            ggplot2::ggtitle(paste0(LOCATION, " (chain ", CHAIN, ") - ", 
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

    mcmc = NULL
}
