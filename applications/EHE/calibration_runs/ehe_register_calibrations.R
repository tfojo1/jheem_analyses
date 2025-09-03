print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'trans.ehe'
CALIBRATION.CODE.FULL.PLUS.COVID = 'full.ehe'
CALIBRATION.CODE.EHE.FINAL = 'final.ehe'

CALIBRATION.CODE.POP.STATE = 'pop.ehe.state'
CALIBRATION.CODE.TRANS.STATE = 'trans.ehe.state'
CALIBRATION.CODE.FULL.STATE = 'full.ehe.state'
CALIBRATION.CODE.EHE.FINAL.STATE = 'final.ehe.state'
N.ITER.TEST = 10000
N.ITER.POP = 20000
N.ITER.TRANS = 20000
N.ITER.FULL = 50000
N.ITER.FINAL = 250000

CALIBRATION.END.YEAR = 2035

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
    POPULATION.PARAMETERS.PRIOR@var.names[!grepl('hiv\\..*migration', POPULATION.PARAMETERS.PRIOR@var.names)],
    "global.trate",
    EHE.PARAMETERS.PRIOR@var.names[grepl('proportion\\.msm\\.of\\.male', EHE.PARAMETERS.PRIOR@var.names)]
)

par.aliases.population = list(
    
    trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('trate\\.2', EHE.PARAMETERS.PRIOR@var.names)])
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions, # added race/risk transmission targets 10/21
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = par.names.pop,
                          parameter.aliases = par.aliases.population, # added for state-level 4/3
                          n.iter = N.ITER.POP,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.09), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)

#-- REGISTER TRANSMISSION CALIBRATION  --#
par.names.transmission = EHE.PARAMETERS.PRIOR@var.names[(grepl('trate', EHE.PARAMETERS.PRIOR@var.names) & !grepl('after', EHE.PARAMETERS.PRIOR@var.names)) | 
                                                            grepl('msm\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility', 
                                                                  EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('sexual\\.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('needle\\.sharing.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('incident\\.idu', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('idu\\.initial\\.prevalence', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('remission', EHE.PARAMETERS.PRIOR@var.names) |
                                                            grepl('relapse', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('idu.mortality', EHE.PARAMETERS.PRIOR@var.names) |
                                                            grepl('fraction\\.heterosexual', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('oe', EHE.PARAMETERS.PRIOR@var.names) | 
                                                            grepl('female\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names)  |
                                                            grepl('male\\.vs\\.female\\.heterosexual\\.rr', EHE.PARAMETERS.PRIOR@var.names)|
                                                            (grepl('idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names) & !grepl("age", EHE.PARAMETERS.PRIOR@var.names)) |
                                                            # ((grepl('suppressed\\.or', EHE.PARAMETERS.PRIOR@var.names) | grepl('suppressed\\.slope\\.or', EHE.PARAMETERS.PRIOR@var.names)) & 
                                                            #      !grepl('age', EHE.PARAMETERS.PRIOR@var.names)) |
                                                            grepl('proportion\\.msm\\.of\\.male', EHE.PARAMETERS.PRIOR@var.names)
                                                        # grepl('susceptibility.rr', EHE.PARAMETERS.PRIOR@var.names) |   
                                                        # grepl('hiv.aging', EHE.PARAMETERS.PRIOR@var.names)
]

par.names.transmission = c(par.names.transmission,
                           "unsuppressed.peak.hiv.mortality",
                           "unsuppressed.hiv.mortality.0",
                           "unsuppressed.hiv.mortality.1",
                           "hiv.general.mortality.multiplier",
                           'aids.to.new.diagnoses.ratio.peak',
                           'aids.to.new.diagnoses.ratio.0',
                           'aids.to.new.diagnoses.ratio.1',
                           'idu.sex.assortativity.multiplier'
)

par.aliases.transmission = list(
    
    msm.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
    msm.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    msm.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    msm.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    msm.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    male.heterosexual.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.male\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
    male.heterosexual.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.male\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    male.heterosexual.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.male\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    male.heterosexual.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.male\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    male.heterosexual.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.male\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    female.heterosexual.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.female\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
    female.heterosexual.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.female\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    female.heterosexual.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.female\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    female.heterosexual.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.female\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    female.heterosexual.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.female\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    idu.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('idu\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
    idu.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('idu\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    idu.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('idu\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    idu.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('idu\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    idu.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('idu\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    black.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('black.*trate', EHE.PARAMETERS.PRIOR@var.names)]),
    black.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('black.*trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    black.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('black.*trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    black.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('black.*trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    black.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('black.*trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    hispanic.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('hispanic.*trate', EHE.PARAMETERS.PRIOR@var.names)]),
    hispanic.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('hispanic.*trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    hispanic.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('hispanic.*trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    hispanic.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('hispanic.*trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    hispanic.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('hispanic.*trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
    
    other.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('other.*trate', EHE.PARAMETERS.PRIOR@var.names)]),
    other.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('other.*trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
    other.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('other.*trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
    other.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('other.*trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
    other.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('other.*trate\\.2', EHE.PARAMETERS.PRIOR@var.names)])
    
)
par.aliases.transmission = par.aliases.transmission[sapply(par.aliases.transmission, length)>0]

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          likelihood.instructions = transmission.pop.idu.aware.aids.testing.likelihood.instructions,
                          special.case.likelihood.instructions = list(
                              C.31080=transmission.pop.idu.aware.aids.testing.likelihood.instructions.4x.aids),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = c(par.names.transmission), 
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER.TRANS,
                          thin = 50, 
                          #fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION
)


#-- REGISTER FULL (WITHOUT COVID) CALIBRATION  --#
# register.calibration.info(CALIBRATION.CODE.FULL.PLUS.AIDS,
#                           likelihood.instructions = FULL.likelihood.instructions.with.aids,
#                           data.manager = SURVEILLANCE.MANAGER,
#                           end.year = CALIBRATION.END.YEAR, 
#                           parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
#                           n.iter = N.ITER.FULL, 
#                           thin = 200, 
#                           is.preliminary = T,
#                           max.run.time.seconds = 10,
#                           preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
#                           description = "Full with aids diagnoses"
# )


#-- REGISTER FULL CALIBRATION WITH COVID-RELATED --#
register.calibration.info(CALIBRATION.CODE.FULL.PLUS.COVID,
                          likelihood.instructions = FULL.likelihood.instructions.32x.new.prev,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full with covid likelihoods"
)

#-- REGISTER FINAL CALIBRATION --#
register.calibration.info(CALIBRATION.CODE.EHE.FINAL,
                          likelihood.instructions = FULL.likelihood.instructions.8x.new.prev,
                          special.case.likelihood.instructions = 
                              list(C.33100=FULL.likelihood.instructions.32x.new.prev),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FINAL, 
                          thin = 500, 
                          n.chains = 4,
                          n.burn = ifelse(RUNNING.ON.DESKTOP, 0, floor(N.ITER.FINAL/2)),
                          is.preliminary = F,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.FULL.PLUS.COVID),
                          description = "FULL RUN"
)

##---------------------------------------------##
##-- STATE-LEVEL POP, TRANS, FULL, AND FINAL --##
##---------------------------------------------##

register.calibration.info(CALIBRATION.CODE.POP.STATE,
                          likelihood.instructions = pop.state.likelihood.instructions, 
                          special.case.likelihood.instructions = list(
                              MS = pop.state.upweighted.new.prev.likelihood.instructions,
                              TN = pop.state.upweighted.new.prev.likelihood.instructions
                          ),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = par.names.pop,
                          parameter.aliases = par.aliases.population,
                          n.iter = N.ITER.POP,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.09), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)

register.calibration.info(CALIBRATION.CODE.TRANS.STATE,
                          likelihood.instructions = trans.state.likelihood.instructions,
                          special.case.likelihood.instructions = list(
                              SC = trans.state.overweighted.prevalence.likelihood.instructions
                          ),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = c(par.names.transmission), 
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER.TRANS,
                          thin = 50, 
                          #fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POP.STATE
)

# state-level full calibration - removed AIDS deaths 
register.calibration.info(CALIBRATION.CODE.FULL.STATE,
                          likelihood.instructions = full.state.likelihood.instructions,
                          special.case.likelihood.instructions = list(
                              SC = full.state.overweighted.prev.vs.new.likelihood.instructions,
                              NJ = full.state.overweighted.prev.vs.new.likelihood.instructions
                          ),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANS.STATE),
                          description = "Full with covid likelihoods"
)

# state-level final calibration 
register.calibration.info(CALIBRATION.CODE.EHE.FINAL.STATE,
                          likelihood.instructions = full.state.likelihood.instructions.half.weight,
                          special.case.likelihood.instructions = list(
                              SC = full.state.overweighted.prev.vs.new.likelihood.instructions.half.weight,
                              NJ = full.state.overweighted.prev.vs.new.likelihood.instructions.half.weight
                          ),
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FINAL, 
                          thin = 500, 
                          n.chains = 4,
                          n.burn = ifelse(RUNNING.ON.DESKTOP, 0, floor(N.ITER.FINAL/2)),
                          is.preliminary = F,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.FULL.STATE),
                          description = "FULL RUN"
)


