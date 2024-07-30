print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.POP.TRANS.MORT = 'pop.trans.mort'

CALIBRATION.CODE.FULL.PLUS.AIDS = 'full.with.aids'
CALIBRATION.CODE.FULL.PLUS.COVID = 'full.with.covid'

CALIBRATION.CODE.FULL = 'full'

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
                  "black.age1.aging.multiplier.2",
                  "hispanic.age1.aging.multiplier.2",
                  "other.age1.aging.multiplier.2",
                  
                  "black.age2.aging.multiplier.1",
                  "hispanic.age2.aging.multiplier.1",
                  "other.age2.aging.multiplier.1",
                  "black.age2.aging.multiplier.2",
                  "hispanic.age2.aging.multiplier.2",
                  "other.age2.aging.multiplier.2",
                  
                  "black.age3.aging.multiplier.1",
                  "hispanic.age3.aging.multiplier.1",
                  "other.age3.aging.multiplier.1",
                  "black.age3.aging.multiplier.2",
                  "hispanic.age3.aging.multiplier.2",
                  "other.age3.aging.multiplier.2",
                  
                  "black.age4.aging.multiplier.1",
                  "hispanic.age4.aging.multiplier.1",
                  "other.age4.aging.multiplier.1",
                  "black.age4.aging.multiplier.2",
                  "hispanic.age4.aging.multiplier.2",
                  "other.age4.aging.multiplier.2",
                  
                  "black.immigration.multiplier.time.1",
                  "hispanic.immigration.multiplier.time.1",
                  "other.immigration.multiplier.time.1",
                  "black.immigration.multiplier.time.2",
                  "hispanic.immigration.multiplier.time.2",
                  "other.immigration.multiplier.time.2",
                  
                  "black.emigration.multiplier.time.1",
                  "hispanic.emigration.multiplier.time.1",
                  "other.emigration.multiplier.time.1",
                  "black.emigration.multiplier.time.2",
                  "hispanic.emigration.multiplier.time.2",
                  "other.emigration.multiplier.time.2",
                  
                  "age1.migration.multiplier.time.1",
                  "age1.migration.multiplier.time.2",
                  "age2.migration.multiplier.time.1",
                  
                  # "age2.migration.multiplier.time.2",
                  "black.age2.migration.multiplier.time.2",
                  "hispanic.age2.migration.multiplier.time.2",
                  "other.age2.migration.multiplier.time.2",
                  "age3.migration.multiplier.time.1",
                  # "age3.migration.multiplier.time.2",
                  "black.age3.migration.multiplier.time.2",
                  "hispanic.age3.migration.multiplier.time.2",
                  "other.age3.migration.multiplier.time.2",
                  
                  "age4.migration.multiplier.time.1",
                  "age4.migration.multiplier.time.2",
                  "age5.migration.multiplier.time.1",
                  "age5.migration.multiplier.time.2"
)

par.names.basic.trans = c(
  "global.trate"#,
  # "peak.hiv.mortality",
  # "hiv.mortality.0",
  # "hiv.mortality.1",
  # 'msm.testing.ramp.1.or',
  # 'msm.testing.ramp.2.or',
  # 'heterosexual.testing.ramp.1.or',
  # 'heterosexual.testing.ramp.2.or',
  # 'idu.testing.ramp.1.or',
  # 'idu.testing.ramp.2.or'
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions, # added total prev/new 5/20
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
                                                          grepl('female\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names) |
                                            grepl('susceptibility.rr', EHE.PARAMETERS.PRIOR@var.names) |   
                                            grepl('hiv.aging', EHE.PARAMETERS.PRIOR@var.names)
                                                          ]



par.names.transmission = c(par.names.transmission,
                           "peak.hiv.mortality",
                           "hiv.mortality.0",
                           "hiv.mortality.1",
                           # 'msm.testing.ramp.1.or',
                           # 'msm.testing.ramp.2.or',
                           # 'heterosexual.testing.ramp.1.or',
                           # 'heterosexual.testing.ramp.2.or',
                           # 'idu.testing.ramp.1.or',
                           # 'idu.testing.ramp.2.or',
                           'idu.remission.multiplier',
                           'idu.relapse.multiplier',
                           'aids.to.new.diagnoses.ratio.peak',
                           'aids.to.new.diagnoses.ratio.0',
                           'aids.to.new.diagnoses.ratio.1'
                           )

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          # added proportion tested 4/23
                          #likelihood.instructions = two.way.transmission.pop.idu.aware.aids.testing.likelihood.instructions,
                          likelihood.instructions = transmission.pop.idu.aware.aids.testing.likelihood.instructions,
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


#-- REGISTER FULL (WITHOUT COVID) CALIBRATION  --#
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

#-- REGISTER FULL CALIBRATION WITH EVERYTHING - straight from pop calibration --#
register.calibration.info(CALIBRATION.CODE.FULL,
                          likelihood.instructions = FULL.likelihood.instructions.with.covid,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.POPULATION),
                          description = "Full with covid likelihoods"
)

