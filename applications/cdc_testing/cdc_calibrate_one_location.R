
LOCATIONS = c("OH")
print(paste0("Running locations: ", paste0(LOCATIONS, collapse=', ')))


source("applications/cdc_testing/cdc_testing_main.R")


for (LOCATION in LOCATIONS)
{
    print(paste0("STARTING TRANSMUTE CALIBRATION OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    
    print(ggplot2::qplot(1,1) + 
              ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                      locations::get.location.name(LOCATION), " - ",
                                      'cdct')))
    
    clear.transmute.calibration.cache(transmute.code = 'cdct',
                                      location = LOCATION, 
                                      n.sim = N.SIMS,
                                      allow.remove.incomplete = T)
    set.up.transmute.calibration('cdct', 
                                 from.calibration.code = 'final.ehe.state', 
                                 location = LOCATION, 
                                 n.chunks = N.CHUNKS,
                                 n.sim = N.SIMS,
                                 allow.overwrite.cache = TRUE,
                                 return.simulations = F)
    
    print(paste0("RUNNNING"))
    run.transmute.calibration("cdct",location = LOCATION, n.sim = N.SIMS, chunks = 1:N.CHUNKS,verbose =  T, ignore.errors = F)
    
    simset = assemble.transmuted.simulations('cdct', LOCATION, n.sim = N.SIMS)
 
    simset$save()   
}