# =============================================================================
# OAKLAND TGA CALIBRATION REGISTRATION
# Registers Oakland-specific calibrations (codes suffixed '.oakland') using the
# likelihoods in ehe_likelihoods_oakland.R. Adapted from
# ehe_register_calibrations.R; add codes here as we go.
# =============================================================================

print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods_oakland.R')
source('../jheem_analyses/applications/EHE/calibration_runs/oakland_add_missing_data.R') # in-memory data patches for TGA.OAKLAND

CALIBRATION.CODE.POPULATION.OAKLAND = 'pop.ehe.oakland'
CALIBRATION.CODE.TRANSMISSION.OAKLAND = 'trans.ehe.oakland'
CALIBRATION.CODE.FULL.PLUS.COVID.OAKLAND = 'full.ehe.oakland'

N.ITER.POP = 20000
N.ITER.TRANS = 20000
N.ITER.FULL = 50000

CALIBRATION.END.YEAR = 2035

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata") 

print("REGISTERING OAKLAND CALIBRATIONS")
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

register.calibration.info(CALIBRATION.CODE.POPULATION.OAKLAND,
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
                          description = "Oakland TGA: a quick run to get population parameters in the general vicinity"
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

register.calibration.info(CALIBRATION.CODE.TRANSMISSION.OAKLAND,
                          likelihood.instructions = transmission.oakland.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = c(par.names.transmission), 
                          parameter.aliases = par.aliases.transmission,
                          n.iter = N.ITER.TRANS,
                          thin = 50, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "Oakland TGA: a quick run to get transmission parameters in the general vicinity",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION.OAKLAND
)

#-- REGISTER FULL (WITH COVID) CALIBRATION --#
# Same settings as full.ehe in ehe_register_calibrations.R.
# Likelihood: FULL.oakland.likelihood.instructions (ehe_likelihoods_oakland.R) = the main
# FULL.likelihood.instructions.32x.new.prev minus the targets with no Oakland data
register.calibration.info(CALIBRATION.CODE.FULL.PLUS.COVID.OAKLAND,
                          likelihood.instructions = FULL.oakland.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = CALIBRATION.END.YEAR, 
                          parameter.names = EHE.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = c(CALIBRATION.CODE.TRANSMISSION.OAKLAND),
                          description = "Oakland TGA: full with covid likelihoods"
)
