#

#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<3) {
    stop("Three arguments must be supplied", call.=FALSE)
}

#-- SET THE WD --#
print(getwd())
setwd('~scr4_pkasaie1/azalesak/jheem/code/jheem_analyses')

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source('applications/EHE/ehe_specification.R')

version = args[1]
location = args[2]
calibration.code = args[3]

# print(typeof(version))
# print(typeof(location))
# print(typeof(calibration.code))

clear.calibration.cache(version=version,
                        location=location,
                        calibration.code=calibration.code,
                        allow.remove.incomplete = T)
set.up.calibration(version=version,
                   location=location,
                   calibration.code=calibration.code,
                   cache.frequency = 250)