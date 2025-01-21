#

#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<5) {
    stop("Five arguments must be supplied", call.=FALSE)
}

#-- SET THE WD --#
# print(getwd())
# setwd('~scr4_pkasaie1/azalesak/jheem/code/jheem_analyses')

version = args[1]
location = args[2]
calibration.code = args[3]
specification.path = args[4]
register.calibration.path = args[5]

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source(specification.path)
source(register.calibration.path)

clear.calibration.cache(version=version,
                        location=location,
                        calibration.code=calibration.code,
                        allow.remove.incomplete = T)
set.up.calibration(version=version,
                   location=location,
                   calibration.code=calibration.code,
                   cache.frequency = 250)