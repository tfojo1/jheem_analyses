ource("applications/SHIELD/shield_specification.R")
source("applications/SHIELD/analysis/calibration/calibration_helper_functions.R")

CALIBRATION.CODE <- "calib.10.20.stage3"   # your stage 3 code
LOCATIONS        <- SHIELD.TEN.MSAS      # names = cities, values = codes
KEEP             <- 0.5                  # keep the second half of each chain

# 1. number of sims saved per city: 4 chains x 20000 iterations / thin 50
N.SIM <- 1600

# 2. take from calib.simsets in memory; load from disk only the missing cities
calib.simsets <- load.calib.simsets(locations         = LOCATIONS,
                                    calibration.codes = CALIBRATION.CODE,
                                    n.sim             = N.SIM)

# 3. per city: burn half of each chain and save as a new file
for (loc.name in names(LOCATIONS)) {
    key <- paste0(loc.name, " \u2013 ", CALIBRATION.CODE)
    if (is.null(calib.simsets[[key]])) {
        message("Skipping ", loc.name, ": no stage 3 simset found")
        next
    }
    
    full   <- calib.simsets[[key]]$full_simset
    burned <- full$burn(keep = KEEP)
    
    message(loc.name, ": ", full$n.sim, " -> ", burned$n.sim, " sims; per chain: ",
            paste(table(burned$simulation.chain), collapse = ", "))
    
    save.simulation.set(burned)   # n.sim is in the file name, so the full file is kept
}
