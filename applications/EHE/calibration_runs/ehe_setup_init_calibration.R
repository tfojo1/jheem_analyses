source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c(BALTIMORE.MSA,MIAMI.MSA,HOUSTON.MSA,CHICAGO.MSA)
CALIBRATION.CODE.TO.RUN = CALIBRATION.CODE.TRANSMISSION # or CALIBRATION.CODE.POPULATION

for(location in LOCATIONS){
  clear.calibration.cache(version='ehe',
                          location=location,
                          calibration.code = CALIBRATION.CODE.TO.RUN)
  
  set.up.calibration(version='ehe',
                     location=location,
                     calibration.code = CALIBRATION.CODE.TO.RUN,
                     cache.frequency = 250)  
}
