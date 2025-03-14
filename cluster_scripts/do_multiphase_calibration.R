# SETUP, THEN RUN, MULTIPLE STAGES WITH ONE CHAIN

#-- CHECK ARGUMENTS --#
args = commandArgs(trailingOnly=TRUE)
if (length(args)<6) {
    stop("Six arguments must be supplied", call.=FALSE)
}

version = args[1]
location = args[2]
calibration.codes = args[3]
chain = as.numeric(args[4])
specification.path = args[4]
register.calibration.path = args[5]

#-- SOURCE THE RELEVANT FILES --#
print("Loading Source Files...")
source(specification.path)
source(register.calibration.path)

for (calibration.code in calibration.codes) {
    
    print(paste0("On code ", calibration.code, " at ", Sys.time()))
    
    clear.calibration.cache(version=version,
                            location=location,
                            calibration.code=calibration.code,
                            allow.remove.incomplete = T)
    set.up.calibration(version=version,
                       location=location,
                       calibration.code=calibration.code,
                       cache.frequency = 250)
    
    print(paste0("Running mcmc for '", version, "', '", location, "', '", calibration.code, "'"))
    mcmc = run.calibration(version = version,
                           location = location,
                           calibration.code = calibration.code,
                           chains = chain,
                           update.frequency = 100,
                           update.detail = 'med')
    
}

