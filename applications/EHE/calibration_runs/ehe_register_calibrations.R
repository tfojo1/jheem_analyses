print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.POP.TRANS.MORT = 'pop.trans.mort'

CALIBRATION.CODE.FULL.PLUS.AIDS = 'full.with.aids'
CALIBRATION.CODE.FULL.PLUS.COVID = 'full.with.covid'

N.ITER.TEST = 10000
N.ITER = 15000
N.ITER.FULL = 35000

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c("black.birth.rate.multiplier",
                  "hispanic.birth.rate.multiplier",
                  "other.birth.rate.multiplier",
                  "black.non.idu.general.mortality.rate.multiplier", # accidentally left out before
                  "hispanic.non.idu.general.mortality.rate.multiplier", # accidentally left out before
                  "other.non.idu.general.mortality.rate.multiplier", # accidentally left out before
                  "age1.non.idu.general.mortality.rate.multiplier",
                  "age2.non.idu.general.mortality.rate.multiplier",
                  "age3.non.idu.general.mortality.rate.multiplier",
                  "age4.non.idu.general.mortality.rate.multiplier",
                  "age5.non.idu.general.mortality.rate.multiplier",
                  "male.non.idu.general.mortality.rate.multiplier", # accidentally left out before
                  "female.non.idu.general.mortality.rate.multiplier", # accidentally left out before
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

par.names.basic.trans = c(
  "global.trate",
  "peak.hiv.mortality",
  "hiv.mortality.0",
  "hiv.mortality.1",
  'testing.ramp.1.or',
  'testing.ramp.2.or'
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.aids.likelihood.instructions, # added total prev/new 5/20
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = c(par.names.pop, par.names.basic.trans), # adding this in 5/20
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.03), 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)

#-- REGISTER TRANSMISSION CALIBRATION  --#
par.names.transmission = EHE.PARAMETERS.PRIOR@var.names[grepl('trate', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('msm\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility', 
                                                                EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('sexual\\.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('needle\\.sharing.assortativity', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('incident\\.idu', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('idu\\.initial\\.prevalence', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('fraction\\.heterosexual', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('oe', EHE.PARAMETERS.PRIOR@var.names) | 
                                                          grepl('female\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names) ]

par.names.transmission = c(par.names.transmission,
                           "peak.hiv.mortality",
                           "hiv.mortality.0",
                           "hiv.mortality.1",
                           'testing.ramp.1.or',
                           'testing.ramp.2.or',
                           'msm.testing.ramp.or',
                           'heterosexual.testing.ramp.or',
                           'idu.testing.ramp.or',
                           'idu.remission.multiplier',
                           'idu.relapse.multiplier',
                           'msm.proportion.tested.or',
                           'msm.proportion.tested.slope.or',
                           'heterosexual.proportion.tested.or',
                           'heterosexual.proportion.tested.slope.or',
                           'idu.proportion.tested.or',
                           'idu.proportion.tested.slope.or',
                           'msm.idu.proportion.tested.or',
                           'msm.idu.proportion.tested.slope.or',
                           'black.proportion.tested.or',
                           'hispanic.proportion.tested.or')

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          # added proportion tested 4/23
                          likelihood.instructions = two.way.transmission.pop.idu.aware.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = c(par.names.transmission), 
                          n.iter = N.ITER,
                          thin = 50, 
                          #fixed.initial.parameter.values = c(global.trate=0.1), 
                          pull.parameters.and.values.from.preceding = F,
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION
)


#-- REGISTER FULL CALIBRATION  --#
register.calibration.info(CALIBRATION.CODE.FULL.PLUS.AIDS,
                          likelihood.instructions = FULL.likelihood.instructions.with.aids,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full with aids diagnoses"
)

#-- REGISTER FULL CALIBRATION WITH COVID-RELATED --#
register.calibration.info(CALIBRATION.CODE.FULL.PLUS.COVID,
                          likelihood.instructions = FULL.likelihood.instructions.with.covid,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION),
                          description = "Full with covid likelihoods"
)

