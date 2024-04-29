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
              'C.12580' #'C.12580'
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

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION # CALIBRATION.CODE.POPULATION, 
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
  
  
  save(sim,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_",Sys.Date(),"_",location,".Rdata"))


}


# looking at full mcmc results
if(1==2){
  library(bayesian.simulations)
  
  
  # not sure why there are NA's in the # accepted
  table(is.na(mcmc@n.accepted))

  
  sim.last = mcmc@simulations[[length(mcmc@simulations)]]
  sim.first = mcmc@simulations[[1]]
  load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.sim_2024-03-29_C.12580.Rdata")
  sim.trans = sim
  
  simplot(sim.trans,sim.first,sim.last,outcomes = "population")
  
  cbind(round(sim.trans$parameters,3),
        round(sim.first$parameters,3),
        round(sim.last$parameters,3))
  round(exp(full.lik.without.supp$compute.piecewise(sim.last) - full.lik.without.supp$compute.piecewise(sim.first)),4)

  round(exp(full.lik.without.supp$compute.piecewise(sim.first) - full.lik.without.supp$compute.piecewise(sim.trans)),4)
  # all of these say sim.first (dashed) is better than sim.trans (solid) in the likelihood
  simplot(sim.trans,sim.first,outcomes="proportion.general.population.tested") # not much diff
  simplot(sim.trans,sim.first,outcomes="hiv.test.positivity",dimension.values = list(year=2000:2030))
  simplot(sim.trans,sim.first,outcomes="awareness") # no data?
  simplot(sim.trans,sim.first,outcomes="prep.uptake") # this could be dragging it?
  simplot(sim.trans,sim.first,outcomes="prep.indications") # this could be dragging it?
  simplot(sim.trans,sim.first,outcomes="proportion.using.heroin")
  simplot(sim.trans,sim.first,outcomes="proportion.using.cocaine")
  
  
  
  round(exp(full.lik.without.supp$compute.piecewise(sim.last) - full.lik.without.supp$compute.piecewise(sim.first)),4)
  # all of these say sim.last (dashed) is better than sim.first (solid) in the likelihood
  simplot(sim.first,sim.last,outcomes="emigration")
  simplot(sim.first,sim.last,outcomes="hiv.mortality") # no data?
  simplot(sim.first,sim.last,outcomes="total.mortality",dimension.values = list(year=2008:2030))
  simplot(sim.first,sim.last,outcomes="aids.deaths")
  simplot(sim.first,sim.last,outcomes="proportion.general.population.tested")
  simplot(sim.first,sim.last,outcomes="awareness") # no data?
  simplot(sim.first,sim.last,outcomes="prep.uptake") 
  simplot(sim.first,sim.last,outcomes="prep.indications")
  simplot(sim.first,sim.last,outcomes="proportion.using.heroin")
  simplot(sim.first,sim.last,outcomes="proportion.using.cocaine")
  
}
