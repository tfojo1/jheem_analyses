LOCATION = "LA" # LOCATION.LIST[LOCATION.INDEX]
print(LOCATION)

source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION.LIST = setdiff(MSAS.OF.INTEREST, c('C.17140','C.44180','C.12580'))


print(paste0("Running for ", LOCATION, " - ", locations::get.location.name(LOCATION)))
CALIBRATION.CODES.TO.RUN = c(CALIBRATION.CODE.POPULATION, # 1
                             CALIBRATION.CODE.TRANSMISSION, # 2
                             CALIBRATION.CODE.FULL.PLUS.COVID, # 3
                             CALIBRATION.CODE.EHE.FINAL, # 4
                             # STATE-SPECIFIC trans, full, and final 
                             CALIBRATION.CODE.POP.STATE, # 5
                             CALIBRATION.CODE.TRANS.STATE, # 6
                             CALIBRATION.CODE.FULL.STATE, # 7
                             CALIBRATION.CODE.EHE.FINAL.STATE # 8
                             )[c(8)]

RESUME.FIRST = F

#CALIBRATION.CODE.TO.RUN = CALIBRATION.CODES.TO.RUN

for (CALIBRATION.CODE.TO.RUN in CALIBRATION.CODES.TO.RUN)
{   
    if (!RESUME.FIRST || CALIBRATION.CODES.TO.RUN[1] != CALIBRATION.CODE.TO.RUN)
    {
        simset = NULL
    
        gc()
        
        set.seed(12345)
        
        print(paste0("Setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
        
        clear.calibration.cache(version='ehe',
                                location=LOCATION,
                                calibration.code = CALIBRATION.CODE.TO.RUN,
                                allow.remove.incomplete = T)
        
        set.up.calibration(version='ehe',
                           location=LOCATION,
                           calibration.code = CALIBRATION.CODE.TO.RUN,
                           cache.frequency = 250)  
        
        print(paste0("DONE setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
    }
  
    start.time = Sys.time()
    
    print(ggplot2::qplot(1,1) + 
            ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                    locations::get.location.name(LOCATION), " - ",
                                    CALIBRATION.CODE.TO.RUN)))
    
    print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    #Rprof()
    mcmc = run.calibration(version = 'ehe',
                           location = LOCATION,
                           calibration.code = CALIBRATION.CODE.TO.RUN,
                           chains = 4,
                           update.frequency = 100,
                           update.detail = 'med')
    end.time = Sys.time()
    run.time = as.numeric(end.time) - as.numeric(start.time)

    mcmc = NULL
    
    # Save simset
    simset = assemble.simulations.from.calibration(version = 'ehe',
                                                   location = LOCATION,
                                                   calibration.code = CALIBRATION.CODE.TO.RUN)
    # 
    # simset = simset$burn(keep = 0.5)
    # simset = simset$thin(keep = 50)
    # 
    save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))
    

}
