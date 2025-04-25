
#LOCATION.INDICES = 24 + 1:8
#LOCATION.INDICES = LOCATION.INDICES[-1]
print(paste0("LOCATION INDICES = ", paste0(LOCATION.INDICES, collapse=', ')))

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')
#source('../jheem_analyses/applications/ryan_white/ryan_white_interventions.R')


LOCATIONS.TO.PREP = RW.LOCATIONS[LOCATION.INDICES[LOCATION.INDICES<=length(RW.LOCATIONS)]]
INTERVENTION.CODES.TO.PREP = RW.INTERVENTION.CODES

start.time = as.numeric(Sys.time())
print(paste0("Making web simsets for **", length(LOCATIONS.TO.PREP), "** locations, ", length(INTERVENTION.CODES.TO.PREP), " interventions per location"))
print(paste0("   (", RW.N.SIM.FOR.WEB, " simulations per simset, running from ", RW.WEB.FROM.YEAR, " to ", RW.WEB.TO.YEAR, ")"))

for (loc in LOCATIONS.TO.PREP)
{
    print(paste0("Prepping seed simset for ", loc, " (", get.location.name(loc), ")"))
    full.simset = retrieve.simulation.set('rw', location=loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIM, intervention.code = NULL)
    if (any(is.na(full.simset$simulation.chain)))
        full.simset$fix.chains()
    thinned.simset = full.simset$thin(keep = RW.N.SIM.FOR.WEB)
    web.simset = rerun.simulations(thinned.simset,
                                   sub.version = 'ws',
                                   from.year = RW.WEB.SEED.FROM.YEAR,
                                   to.year = RW.WEB.SEED.TO.YEAR, 
                                   verbose = F)
    web.simset$save()
    
    print(paste0("Prepping ", length(INTERVENTION.CODES.TO.PREP), " simsets for ", loc, " (", get.location.name(loc), ")"))
    for (int.code in INTERVENTION.CODES.TO.PREP)
    {
        print(paste0("  - ", int.code))
        full.simset = retrieve.simulation.set('rw', location=loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIM, intervention.code = int.code)
        if (any(is.na(full.simset$simulation.chain)))
            full.simset$fix.chains()
        thinned.simset = full.simset$thin(keep = RW.N.SIM.FOR.WEB)
        
        web.simset = rerun.simulations(thinned.simset,
                                       sub.version = 'w',
                                       from.year = RW.WEB.FROM.YEAR,
                                       to.year = RW.WEB.TO.YEAR, 
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