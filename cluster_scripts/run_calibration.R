#

#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<6) {
    stop("Six arguments must be supplied", call.=FALSE)
}

#-- SET THE WD --#
# print(getwd())
# setwd('~scr4_pkasaie1/azalesak/jheem/code/jheem_analyses')

version = args[1]
location = args[2]
calibration.code = args[3]
chain = as.numeric(args[4])
specification.path = args[5]
register.calibration.path = args[6]

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source(specification.path)
source(register.calibration.path)

print(paste0("Running mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
mcmc = run.calibration(version = version,
                       location = location,
                       calibration.code = calibration.code,
                       chains = chain,
                       update.frequency = 100,
                       update.detail = 'med')