print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.FULL = 'init.full.ehe'
CALIBRATION.CODE.FULL.WITHOUT.SUPPRESSION = 'init.full.minus.supp.ehe'
CALIBRATION.CODE.POP.TRANS.MORT = 'pop.trans.mort'
CALIBRATION.CODE.POP.TRANS.MORT.IDU = 'pop.trans.mort.idu'
CALIBRATION.CODE.POP.TRANS.MORT.NON.IDU = 'pop.trans.mort.non.idu'
CALIBRATION.CODE.FULL.WITHOUT.PREP = 'full.minus.prep'
CALIBRATION.CODE.FULL.WITHOUT.PREP.SUPP = 'full.minus.prep.supp'
CALIBRATION.CODE.FULL.WITHOUT.PREP.SUPP.IDU = 'full.minus.prep.supp.idu'

N.ITER.TEST = 10000
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

#-- REGISTER FULL CALIBRATION, MINUS SUPPRESSION  --#
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



#-- REGISTER ITERATIVE CALIBRATIONS  --#

# pop, trans, mort - 40k runs
register.calibration.info(CALIBRATION.CODE.POP.TRANS.MORT,
                          likelihood.instructions = pop.trans.mortality.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER, 
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Adding in likelihoods iteratively, population + transmission + mortality"
)

# pop, trans, mort, idu-related (heroin and cocaine)
register.calibration.info(CALIBRATION.CODE.POP.TRANS.MORT.IDU,
                          likelihood.instructions = pop.trans.mortality.idu.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER, 
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Adding in likelihoods iteratively, population + transmission + mortality + idu"
)

# pop, trans, mort, non-idu-related (prep and continuum)
register.calibration.info(CALIBRATION.CODE.POP.TRANS.MORT.NON.IDU,
                          likelihood.instructions = pop.trans.mortality.non.idu.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Adding in likelihoods iteratively, population + transmission + mortality + non-idu"
)

# pop, trans, mort, non-idu-related (prep and continuum)
register.calibration.info(CALIBRATION.CODE.POP.TRANS.MORT.NON.IDU,
                          likelihood.instructions = pop.trans.mortality.non.idu.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Adding in likelihoods iteratively, population + transmission + mortality + non-idu"
)

# full without prep 
register.calibration.info(CALIBRATION.CODE.FULL.WITHOUT.PREP,
                          likelihood.instructions = FULL.likelihood.instructions.minus.prep,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER, 
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full except prep"
)

# full without prep/supp
register.calibration.info(CALIBRATION.CODE.FULL.WITHOUT.PREP.SUPP,
                          likelihood.instructions = FULL.likelihood.instructions.minus.prep.supp,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER, 
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full except prep/supp"
)

# full without prep/supp/idu
register.calibration.info(CALIBRATION.CODE.FULL.WITHOUT.PREP.SUPP.IDU,
                          likelihood.instructions = FULL.likelihood.instructions.minus.prep.supp.idu,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER, 
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.1), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full except prep/supp/idu"
)
