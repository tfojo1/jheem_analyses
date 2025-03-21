

# Assume LOCATION is already set

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

print("RW: ASSEMBLING AND BURNING SIMSET...")
ehe.simset = assemble.simulations.from.calibration('ehe', location=LOCATION, calibration.code = CALIBRATION.CODE)
ehe.simset = ehe.simset$burn(keep=N.SIM)
ehe.simset$save()
print("RW: ...DONE ASSEMBLING")

print("RW: TRANSMUTING...")
rw.simset = fit.rw.simset(ehe.simset, verbose=VERBOSE, track.mcmc = F)
rw.simset$save()
print("RW: ...DONE TRANSMUTING")


print(paste0("RW: RUNNING ", length(RW.INTERVENTION.CODES), " INTERVENTIONS..."))

for (i in 1:length(RW.INTERVENTION.CODES))
{
    code = RW.INTERVENTION.CODES[i]
    print("RW: Running Intervention '", code, "' - ", i, " of ", length(RW.INTERVENTION.CODES), "...")
    
    int = get.intervention.from.code(code)
    simset.int = int$run(rw.simset, 
                         start.year = RW.INT.RUN.FROM.YEAR, 
                         end.year = RW.INT.RUN.TO.YEAR,
                         keep.from.year = ifelse(code=='noint', RW.NOINT.KEEP.FROM.YEAR, RW.INT.KEEP.FROM.YEAR),
                         keep.to.year = RW.INT.KEEP.TO.YEAR,
                         verbose = VERBOSE)

    simset.int$save(T)
        
    print("RW: ...Done running Intervention '", code, "' - ", i, " of ", length(RW.INTERVENTION.CODES), "")
}

print(paste0("RW: ...DONE RUNNING ", length(RW.INTERVENTION.CODES), " INTERVENTIONS"))