
# Assume LOCATION is already set
# LOCATIONS.TO.ATI = list(
#     c('C.35620', 'C.12060', 'C.26420'),
#     c('C.19100', 'C.16980', 'C.47900'),
#     c('C.37980', 'C.36740', 'C.41860'),
#     c('C.38060', 'C.45300', 'C.12580'),
#     c('C.27260', 'C.35380', 'C.32820'),
#     c('C.42660', 'C.12420', 'C.26900'),
#     c('C.18140', 'C.12940'),
#     c('C.40900', 'C.17460')
# )[[7]]

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATIONS.TO.ATI = RW.LOCATIONS # SACRAMENTO.MSA
RESUME.FIRST = F
RERUN = T


for (loc in LOCATIONS.TO.ATI)
{
    if ((!RESUME.FIRST || loc != LOCATIONS.TO.ATI[1]) && !RERUN)
    {
        print(paste0("RW: ASSEMBLING AND BURNING SIMSET FOR '", loc, "'..."))
        ehe.simset = assemble.simulations.from.calibration('ehe', location=loc, calibration.code = CALIBRATION.CODE)
        ehe.simset = ehe.simset$burn(keep=N.SIM)
        ehe.simset$save()
        print(paste0("RW: ...DONE ASSEMBLING"))
        
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