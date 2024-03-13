source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c(# NYC.MSA, #'C.35620' # non.idu.general.mortality error
              #MIAMI.MSA, #'C.33100'
              LA.MSA, #'C.31080'
              #ATLANTA.MSA, #'C.12060'
              #HOUSTON.MSA, #'C.26420'
              #DALLAS.MSA, #'C.19100'
              #CHICAGO.MSA, #'C.16980' # non.idu.general.mortality error
              DC.MSA, #'C.47900'
              #PHILADELPHIA.MSA, #'C.37980'
              #ORLANDO.MSA, #'C.36740'
              SF.MSA, #'C.41860'
              #PHOENIX.MSA, #'C.38060' # proportion.msm.of.male error
              TAMPA.MSA, #'C.45300'
              #RIVERSIDE.MSA, #'C.40140'
              #DETROIT.MSA, #'C.19820' # "control file for the 1st chain is missing"
              BALTIMORE.MSA #'C.12580'
              #VEGAS.MSA, #'C.29820' # proportion.msm.of.male error
              #BOSTON.MSA, #'C.14460' # non.idu.general.mortality error
              #SAN.DIEGO.MSA, #'C.41740' # proportion.msm.of.male error
              #CHARLOTTE.MSA, #'C.16740' # non.idu.general.mortality error
              #SAN.ANTONIO.MSA, #'C.41700'
              #JACKSONVILLE.MSA, #'C.27260'
              #NEW.ORLEANS.MSA, #'C.35380'
              #MEMPHIS.MSA, #'C.32820' # non.idu.general.mortality error
              #SEATTLE.MSA, #'C.42660'
              #AUSTIN.MSA, #'C.12420'
              #INDIANAPOLIS.MSA, #'C.26900'
              #CINCINATTI.MSA, #'C.17140' # non.idu.general.mortality error
              #COLUMBUS.MSA, #'C.18140'
              #BATON.ROUGE.MSA, #'C.12940'
              #SACRAMENTO.MSA, #'C.40900'
              #CLEVELAND.MSA #'C.17460'
)

CALIBRATION.CODE.TO.RUN = CALIBRATION.CODE.POPULATION # or CALIBRATION.CODE.TRANSMISSION

for(location in LOCATIONS){
  clear.calibration.cache(version='ehe',
                          location=location,
                          calibration.code = CALIBRATION.CODE.TO.RUN)
  
  set.up.calibration(version='ehe',
                     location=location,
                     calibration.code = CALIBRATION.CODE.TO.RUN,
                     cache.frequency = 250)  
}
