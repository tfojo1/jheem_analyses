
source('applications/cdc_testing/cdc_testing_main.R')

LOCATIONS = CDC.TESTING.LOCATIONS[4*4 + 1:4]
print(paste0("Re-running interventions for locations: ", paste0(LOCATIONS, collapse=', ')))


source("applications/cdc_testing/cdc_testing_main.R")

for (loc in LOCATIONS)
{
    print(paste0("RE-RUNNING ", loc))
    
    orig.simset = retrieve.simulation.set('cdct', loc, 'final.ehe.state', N.SIMS)
    simset = rerun.simulations(orig.simset)
    
    simset$save()
    
    print(paste0("DONE re-running ", loc))
    
    for (int.code in CDC.TESTING.INTERVENTION.CODES)
    {
        print(paste0("RE-RUNNING INTERVENTION '", int.code, "' for ", loc))
        int = get.intervention(int.code)
        simset.int = int$run(simset, start.year=2025, end.year=2035)
        
        simset.int$save()
        print(paste0("DONE re-running intervention '", int.code, "' for ", loc))
    }
}
