print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
#source('../jheem_analyses/commoncode/locations_of_interest.R')

CALIBRATION.CODE.POPULATION = 'init.pop.ehe'
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
CALIBRATION.CODE.FULL.PLUS.AIDS = 'full.with.aids'
CALIBRATION.CODE.FULL.PLUS.COVID = 'full.with.covid2'

N.ITER.TEST = 10000
N.ITER = 15000
N.ITER.FULL = 50000

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
    POPULATION.PARAMETERS.PRIOR@var.names,
    "global.trate"#,
)

par.names.pop = c(par.names.pop,
                  EHE.PARAMETERS.PRIOR@var.names[grepl('msm\\.trate', EHE.PARAMETERS.PRIOR@var.names) | 
                                                   grepl('heterosexual\\.trate', EHE.PARAMETERS.PRIOR@var.names) |
                                                   grepl('idu\\.trate', EHE.PARAMETERS.PRIOR@var.names) |
                                                   grepl('incident\\.idu', EHE.PARAMETERS.PRIOR@var.names) |
                                                   grepl('black\\.active\\.idu\\.initial', EHE.PARAMETERS.PRIOR@var.names) |
                                                   grepl('hispanic\\.active\\.idu\\.initial', EHE.PARAMETERS.PRIOR@var.names) |
                                                   grepl('other\\.active\\.idu\\.initial', EHE.PARAMETERS.PRIOR@var.names) | 
                                                   grepl('fraction\\.heterosexual', EHE.PARAMETERS.PRIOR@var.names) | 
                                                   grepl('sexual\\.assortativity', EHE.PARAMETERS.PRIOR@var.names)]
                    )
 

register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions, # added race/risk transmission targets 10/21
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
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
                                                          grepl('female\\.vs\\.heterosexual\\.male\\.idu\\.susceptibility\\.rr', EHE.PARAMETERS.PRIOR@var.names) 
                                            # grepl('susceptibility.rr', EHE.PARAMETERS.PRIOR@var.names) |   
                                            # grepl('hiv.aging', EHE.PARAMETERS.PRIOR@var.names)
                                                          ]



par.names.transmission = c(par.names.transmission,
                           "unsuppressed.peak.hiv.mortality",
                           "unsuppressed.hiv.mortality.0",
                           "unsuppressed.hiv.mortality.1",
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
