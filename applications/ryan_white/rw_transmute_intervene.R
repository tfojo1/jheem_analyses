# ATI.INDICES = 19

LOCATIONS.TO.ATI = 'IN'

RESUME.FIRST = F
RERUN = F

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')


for (loc in LOCATIONS.TO.ATI)
{
    print(ggplot2::qplot(1,1) + 
              ggplot2::ggtitle(paste0(loc, " - ", 
                                      locations::get.location.name(loc), " - Ryan White")))
    
    if ((!RESUME.FIRST || loc != LOCATIONS.TO.ATI[1]) && !RERUN)
    {
        ehe.simset = retrieve.simulation.set('ehe', location=loc, calibration.code = CALIBRATION.CODE, n.sim = N.SIM)
        
        print(paste0("RW: TRANSMUTING FOR '", loc, "'..."))
        rw.simset = fit.rw.simset(ehe.simset, verbose=VERBOSE, track.mcmc = F)
        rw.simset$save()
        print(paste0("RW: ...DONE TRANSMUTING"))
    }
    else
        rw.simset = retrieve.simulation.set('rw', location=loc, calibration.code = CALIBRATION.CODE, n.sim=N.SIM)
    
    if (RERUN)
    {
        print(paste0("RW: RERUNNING SIMSET FOR '", loc, "'..."))
        rw.simset = rerun.simulations(rw.simset)
        rw.simset$save()
    }
    
    print(paste0("RW: RUNNING ", length(RW.INTERVENTION.CODES), " INTERVENTIONS for '", loc, "'..."))
    
    for (i in 1:length(RW.INTERVENTION.CODES))
    {
          code = RW.INTERVENTION.CODES[i]
          print(paste0("RW: Running Intervention '", code, "' - ", i, " of ", length(RW.INTERVENTION.CODES), "..."))
          
          int = get.intervention.from.code(code)
          simset.int = int$run(rw.simset, 
                               start.year = RW.INT.RUN.FROM.YEAR, 
                               end.year = RW.INT.RUN.TO.YEAR,
                               keep.from.year = ifelse(code=='noint', RW.NOINT.KEEP.FROM.YEAR, RW.INT.KEEP.FROM.YEAR),
                               keep.to.year = RW.INT.KEEP.TO.YEAR,
                               verbose = VERBOSE)
          
          simset.int$save()
          
          print(paste0("RW: ...Done running Intervention '", code, "' - ", i, " of ", length(RW.INTERVENTION.CODES), ""))
    }
    
    print(paste0("RW: ...DONE RUNNING ", length(RW.INTERVENTION.CODES), " INTERVENTIONS FOR '", loc, "'"))
}