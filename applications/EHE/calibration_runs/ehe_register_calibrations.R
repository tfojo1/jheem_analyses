print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.FULL.PLUS.AIDS = 'full.with.aids'
CALIBRATION.CODE.FULL.PLUS.COVID = 'full.with.covid2'

N.ITER.TEST = 10000
N.ITER = 20000
N.ITER.FULL = 50000

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
    POPULATION.PARAMETERS.PRIOR@var.names,
    "global.trate"#,
)

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions, # added race/risk transmission targets 10/21
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.13), 
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
                                                          grepl('female\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names) 
                                            # grepl('susceptibility.rr', EHE.PARAMETERS.PRIOR@var.names) |   
                                            # grepl('hiv.aging', EHE.PARAMETERS.PRIOR@var.names)
                                                          ]

par.names.transmission = c(par.names.transmission,
                           "unsuppressed.peak.hiv.mortality",
                           "unsuppressed.hiv.mortality.0",
                           "unsuppressed.hiv.mortality.1",
                           #"hiv.general.mortality.multiplier", # added 11/5, took out 11/6 but may want to put back 
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

par.aliases.transmission = list(
  
  msm.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
  msm.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
  msm.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
  msm.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
  msm.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),
  
  heterosexual.trates = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.trate', EHE.PARAMETERS.PRIOR@var.names)]),
  heterosexual.trates.peak = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.trate\\.peak', EHE.PARAMETERS.PRIOR@var.names)]),
  heterosexual.trates.0 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.trate\\.0', EHE.PARAMETERS.PRIOR@var.names)]),
  heterosexual.trates.1 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.trate\\.1', EHE.PARAMETERS.PRIOR@var.names)]),
  heterosexual.trates.2 = (EHE.PARAMETERS.PRIOR@var.names[grepl('heterosexual\\.trate\\.2', EHE.PARAMETERS.PRIOR@var.names)]),

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

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          likelihood.instructions = transmission.pop.idu.aware.aids.testing.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = c(par.names.transmission), 
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER,
                          thin = 50, 
                          #fixed.initial.parameter.values = c(global.trate=0.1), 
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
