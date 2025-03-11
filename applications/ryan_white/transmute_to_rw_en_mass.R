
print("Sourcing code prior to transmuting...")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATION.INDICES = (4-1)*8 + 1:8
LOCATIONS.TO.TRANSMUTE = COLUMBUS.MSA#RW.LOCATIONS[ LOCATION.INDICES[LOCATION.INDICES<=length(RW.LOCATIONS)] ] [4]
#LOCATIONS.TO.TRANSMUTE = rev(LOCATIONS.TO.TRANSMUTE)

print("------------------------------------------------------------------------------")
print(paste0("PREPARING TO TRANSMUTE SIMULATIONS FOR ", length(LOCATIONS.TO.TRANSMUTE), " LOCATIONS TO RYAN-WHITE"))
print("------------------------------------------------------------------------------")

N.SUCCESS = 0

for (i in 1:length(LOCATIONS.TO.TRANSMUTE))
{
    loc = LOCATIONS.TO.TRANSMUTE[i]
  
    ehe.filename = get.simset.filename(version = 'ehe',
                                       location = loc,
                                       calibration.code = CALIBRATION.CODE,
                                       n.sim = N.SIM,
                                       include.path = T)
    
    if (!file.exists(ehe.filename))
        print(paste0("Skipping ", i, " of ", length(LOCATIONS.TO.TRANSMUTE), " - ", loc, " - no EHE simulation set has been saved"))
    else
    {
        rw.filename = get.simset.filename(version = 'rw',
                                          location = loc,
                                          calibration.code = CALIBRATION.CODE,
                                          n.sim = N.SIM,
                                          include.path = T)
        
        if (!FORCE.REDO && file.exists(rw.filename))
        {
            ehe.mtime = file.info(ehe.filename)$mtime
            rw.mtime = file.info(rw.filename)$mtime 
            
            if (ehe.mtime >= rw.mtime)
            {
                print(paste0("Redoing transmutation ", i, " of ", length(LOCATIONS.TO.TRANSMUTE), " - ", loc, "..."))
                redo = T
            }
            else
            {
                print(paste0("Skipping ", i, " of ", length(LOCATIONS.TO.TRANSMUTE), " - ", loc, " - we have already done the transmutation"))
                redo = F
            }
              
        }
        else
        {
            redo = T
            print(paste0("Transmuting ", i, " of ", length(LOCATIONS.TO.TRANSMUTE), " - ", loc, "..."))
        } 
      
        if (redo)
        {
            ehe.simset = load.simulation.set(ehe.filename)
            tryCatch({
                rw.simset = fit.rw.simset(ehe.simset, verbose=VERBOSE, track.mcmc = F)
                
                print(paste0("  ..DONE transmuting ", loc, " - saving..."))
                
                rw.simset$save()
              
            },
            error = function(e){
                print(paste0("  ..ERROR transmuting ", loc, ":"))
                print(e$message)
                print(" MOVING ON...")
            })
        
            print(paste0("  ..DONE with ", loc))
        }
        
        print("")
        
        N.SUCCESS = N.SUCCESS + 1
    }
}

print("------------------------------------------------------------------------------")
print(paste0("DONE TRANSMUTING. SUCCESSFULLY TRANSMUTED ", N.SUCCESS, " LOCATION(S)"))
print("------------------------------------------------------------------------------")