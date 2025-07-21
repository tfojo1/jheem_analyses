
LOCATION.INDICES = 5*3 + 1:3
#LOCATION.INDICES = LOCATION.INDICES[-1]
print(paste0("LOCATION INDICES = ", paste0(LOCATION.INDICES, collapse=', ')))

source('../jheem_analyses/applications/cdc_testing/cdc_testing_main.R')


LOCATIONS.TO.PREP = CDC.TESTING.LOCATIONS[LOCATION.INDICES]
INTERVENTION.CODES.TO.PREP = CDC.TESTING.INTERVENTION.CODES

print(paste0("LOCATION = ", paste0(LOCATIONS.TO.PREP, collapse=', ')))


start.time = as.numeric(Sys.time())
print(paste0("Making web simsets for **", length(LOCATIONS.TO.PREP), "** locations, ", length(INTERVENTION.CODES.TO.PREP), " interventions per location"))
print(paste0("   (", CDCT.N.SIM.FOR.WEB, " simulations per simset, running from ", CDCT.WEB.FROM.YEAR, " to ", CDCT.WEB.TO.YEAR, ")"))

for (loc in LOCATIONS.TO.PREP)
{
    print(paste0("Prepping seed simset for ", loc, " (", get.location.name(loc), ")"))
    full.simset = retrieve.simulation.set('cdct', location=loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIMS, intervention.code = NULL)
    thinned.simset = full.simset$thin(keep = CDCT.N.SIM.FOR.WEB)
    web.simset = rerun.simulations(thinned.simset,
                                   sub.version = 'ws',
                                   from.year = CDCT.WEB.SEED.FROM.YEAR,
                                   to.year = CDCT.WEB.SEED.TO.YEAR, 
                                   verbose = F)
    web.simset$save()
    
    print(paste0("Prepping ", length(INTERVENTION.CODES.TO.PREP), " simsets for ", loc, " (", get.location.name(loc), ")"))
    for (int.code in INTERVENTION.CODES.TO.PREP)
    {
        print(paste0("  - ", int.code))
        full.simset = retrieve.simulation.set('cdct', location=loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIMS, intervention.code = int.code)
        thinned.simset = full.simset$thin(keep = CDCT.N.SIM.FOR.WEB)
        
        web.simset = rerun.simulations(thinned.simset,
                                       sub.version = 'w',
                                       from.year = CDCT.WEB.FROM.YEAR,
                                       to.year = CDCT.WEB.TO.YEAR, 
                                       verbose = F)
        
        web.simset$save()
    }
    
    run.time.seconds = as.numeric(Sys.time()) - start.time
    run.time.minutes = run.time.seconds / 60
    run.time.hours = run.time.minutes / 60
    
    if (run.time.hours > 1)
        print(paste0("Done with ", loc, ". ",
                     floor(run.time.hours), " hours, ",
                     floor(run.time.minutes %% 60), " minutes elapsed"))
    else
        print(paste0("Done with ", loc, ". ",
                     floor(run.time.minutes), " minutes, ",
                     floor(run.time.seconds %% 60), " seconds elapsed"))
}