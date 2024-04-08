source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c(#NYC.MSA, #'C.35620' # non.idu.general.mortality error
              #MIAMI.MSA, #'C.33100'
              #LA.MSA, #'C.31080'
              #ATLANTA.MSA, #'C.12060'
              #HOUSTON.MSA, #'C.26420'
              #DALLAS.MSA, #'C.19100'
              #CHICAGO.MSA, #'C.16980' # non.idu.general.mortality error
              #DC.MSA, #'C.47900'
              #PHILADELPHIA.MSA, #'C.37980'
              #ORLANDO.MSA, #'C.36740'
              #SF.MSA, #'C.41860'
              #PHOENIX.MSA, #'C.38060' # proportion.msm.of.male error
              #TAMPA.MSA, #'C.45300'
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
              #SACRAMENTO.MSA #'C.40900'
              #CLEVELAND.MSA #'C.17460'
)

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.FULL # CALIBRATION.CODE.POPULATION, 
                                                    # CALIBRATION.CODE.TRANSMISSION
                                                    # CALIBRATION.CODE.FULL
                                                    # CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION

for(location in LOCATIONS){
  mcmc = assemble.mcmc.from.calibration(version = 'ehe',
                                        location = location,
                                        calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                        allow.incomplete=T)
  
  source('../jheem_analyses/applications/EHE/ehe_specification.R')
  
  # Pull just the last simulation
  sim = mcmc@simulations[[length(mcmc@simulations)]]
  
  if(CALIBRATION.CODE.TO.RUN==CALIBRATION.CODE.TRANSMISSION){
    save(sim,file=paste0("prelim_results/init.transmission.sim_",Sys.Date(),"_",LOCATION,".Rdata"))
  } else if(CALIBRATION.CODE.TO.RUN==CALIBRATION.CODE.POPULATION){
    save(sim,file=paste0("prelim_results/init.pop.migration.sim_",Sys.Date(),"_",LOCATION,".Rdata"))
  } else if(CALIBRATION.CODE.TO.RUN==CALIBRATION.CODE.FULL){
    save(sim,file=paste0("prelim_results/init.full.sim_",Sys.Date(),"_",LOCATION,".Rdata"))
  } else if(CALIBRATION.CODE.TO.RUN==CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION){
    save(sim,file=paste0("prelim_results/init.full.minus.two.sim_",Sys.Date(),"_",LOCATION,".Rdata"))
  } else stop("invalid calibration code")

}
