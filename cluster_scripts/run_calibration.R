#

#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<4) {
    stop("Four arguments must be supplied", call.=FALSE)
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
chain = args[4]

# print(typeof(version))
# print(typeof(location))
# print(typeof(calibration.code))

print(paste0("Running mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
mcmc = run.calibration(version = version,
                       location = location,
                       calibration.code = calibration.code,
                       chains = chain,
                       update.frequency = 100,
                       update.detail = 'med')