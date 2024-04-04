print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.FULL = 'init.full.ehe'
CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION = 'init.full.minus.supp.ehe'
CALIBRATION.CODE.TEST = 'test'
N.ITER = 20000
N.ITER.FULL = 40000

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c("black.birth.rate.multiplier",
                  "hispanic.birth.rate.multiplier",
                  "other.birth.rate.multiplier",
                  "age1.non.idu.general.mortality.rate.multiplier",
                  "age2.non.idu.general.mortality.rate.multiplier",
                  "age3.non.idu.general.mortality.rate.multiplier",
                  "age4.non.idu.general.mortality.rate.multiplier",
                  "age5.non.idu.general.mortality.rate.multiplier",
                  # "black.age1.aging.multiplier",
                  # "hispanic.age1.aging.multiplier",
                  # "other.age1.aging.multiplier",
                  # "black.age2.aging.multiplier",
                  # "hispanic.age2.aging.multiplier",
                  # "other.age2.aging.multiplier",
                  "black.age1.aging.multiplier.1",
                  "hispanic.age1.aging.multiplier.1",
                  "other.age1.aging.multiplier.1",
                  "black.age2.aging.multiplier.1",
                  "hispanic.age2.aging.multiplier.1",
                  "other.age2.aging.multiplier.1",
                  "black.age1.aging.multiplier.2",
                  "hispanic.age1.aging.multiplier.2",
                  "other.age1.aging.multiplier.2",
                  "black.age2.aging.multiplier.2",
                  "hispanic.age2.aging.multiplier.2",
                  "other.age2.aging.multiplier.2",
                  
                  "black.age3.aging.multiplier",
                  "hispanic.age3.aging.multiplier",
                  "other.age3.aging.multiplier",
                  "age4.aging.multiplier",
                  # "black.domino.aging.multiplier",
                  # "hispanic.domino.aging.multiplier",
                  # "other.domino.aging.multiplier",
                  "immigration.multiplier.time.1",
                  "immigration.multiplier.time.2",
                  "emigration.multiplier.time.1",
                  "emigration.multiplier.time.2",
                  "black.migration.multiplier.time.1",
                  "black.migration.multiplier.time.2",
                  "hispanic.migration.multiplier.time.1",
                  "hispanic.migration.multiplier.time.2",
                  "other.migration.multiplier.time.1",
                  "other.migration.multiplier.time.2",
                  "age1.migration.multiplier.time.1",
                  "age1.migration.multiplier.time.2",
                  "age2.migration.multiplier.time.1",
                  "age2.migration.multiplier.time.2",
                  "age3.migration.multiplier.time.1",
                  "age3.migration.multiplier.time.2",
                  "age4.migration.multiplier.time.1",
                  "age4.migration.multiplier.time.2",
                  "age5.migration.multiplier.time.1",
                  "age5.migration.multiplier.time.2"
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)

#-- REGISTER TRANSMISSION CALIBRATION  --#
par.names.transmission = EHE.PARAMETERS.PRIOR@var.names[grepl('trate', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('msm.vs.heterosexual.male.idu.susceptibility', 
                                                                EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('sexual.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('needle.sharing.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('incident.idu', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('fraction.heterosexual', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('female.vs.heterosexual.male.idu.susceptibility.rr', EHE.PARAMETERS.PRIOR@var.names) ]

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          #  added aids diagnoses, added population back in 
                          likelihood.instructions = two.way.transmission.pop.likelihood.instructions, # no aids at all; added sex 2/29
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = c(par.names.transmission),
                          n.iter = N.ITER,
                          thin = 50, 
                          # fixed.initial.parameter.values = params.manual[par.names.transmission], 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          pull.parameters.and.values.from.preceding = F,
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION
)


#-- REGISTER FULL CALIBRATION  --#
register.calibration.info(CALIBRATION.CODE.FULL,
                          likelihood.instructions = FULL.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, # 40,000
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "A first test of the full likelihood"
)


#-- REGISTER FULL CALIBRATION, WITHOUT PROPORTION TESTED AND TEST POSITIVITY  --#
register.calibration.info(CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION,
                          likelihood.instructions = FULL.likelihood.instructions.minus.supp,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, # 40,000
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "A first test of the full likelihood without suppression"
)


register.calibration.info(CALIBRATION.CODE.TEST,
                          likelihood.instructions = heroin.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, # 40,000
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "DEBUGGING TEST"
)