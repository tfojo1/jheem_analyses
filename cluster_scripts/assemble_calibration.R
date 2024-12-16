#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<5) {
    stop("Five arguments must be supplied", call.=FALSE)
}

#-- SET THE WD --#
print(getwd())
# setwd('~scr4_pkasaie1/azalesak/jheem/code/jheem_analyses')

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source('applications/EHE/ehe_specification.R')
source('applications/EHE/calibration_runs/ehe_register_calibrations.R')

version = args[1]
location = args[2]
calibration.code = args[3]
burn.keep = as.numeric(args[4])
thin.keep = as.numeric(args[5])


# Save simset
print(paste0("Saving mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
simset = assemble.simulations.from.calibration(version = version,
                                               location = location,
                                               calibration.code = calibration.code)

print(paste0("Burning/thinning..."))
if (burn.keep!=0)
    simset = simset$burn(keep = burn.keep) #0.5
if (thin.keep!=0)
    simset = simset$thin(keep = thin.keep)

print(paste0("Saving..."))
save(simset,file=paste0("../../files/run_results/",calibration.code,"_simset_",Sys.Date(),"_",location,".Rdata"))