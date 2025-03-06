
print("Sourcing code prior to transmuting...")

source('applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('applications/ryan_white/ryan_white_mcmc.R')
source('applications/ryan_white/ryan_white_likelihoods.R')
source('commoncode/locations_of_interest.R')

LOCATIONS.TO.TRANSMUTE = BALTIMORE.MSA#MSAS.OF.INTEREST
VERBOSE = T
CALIBRATION.CODE = 'full.with.covid2'
N.SIM = 100

print("------------------------------------------------------------------------------")
print(paste0("PREPARING TO TRANSMUTE SIMULATIONS FOR ", length(LOCATIONS.TO.TRANSMUTE), " LOCATIONS TO RYAN-WHITE"))
print("------------------------------------------------------------------------------")

N.SUCCESS = 0
FORCE.REDO = F

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
            rw.simset = fit.rw.simset(ehe.simset, verbose=VERBOSE, track.mcmc = F)
        
            print(paste0("  ..DONE transmuting ", loc, " - saving..."))
        
            rw.simset$save()
        
            print(paste0("  ..DONE with ", loc))
        }
        
        print("")
        
        N.SUCCESS = N.SUCCESS + 1
    }
}

print("------------------------------------------------------------------------------")
print(paste0("DONE TRANSMUTING. SUCCESSFULLY TRANSMUTED ", N.SUCCESS, " LOCATION(S)"))
print("------------------------------------------------------------------------------")