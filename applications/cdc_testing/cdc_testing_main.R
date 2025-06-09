
source('applications/cdc_testing/cdc_testing_specification.R')
SURVEILLANCE.MANAGER$put(data = as.numeric(NA),allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2011"))
SURVEILLANCE.MANAGER$put(data = as.numeric(NA), allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2013"))
source("applications/cdc_testing/cdc_testing_parameters.R")
source("applications/cdc_testing/likelihood.R")

N.SIMS = 100
N.CHUNKS = 4

register.transmute.calibration.info(transmute.code = 'cdct',
                                    from.version = 'ehe',
                                    to.version = 'cdct',
                                    n.iter.first.sim = 1000,
                                    n.iter.subsequent.sims = 100,
                                    likelihood.instructions = cdc.joint.likelihood.instructions,
                                    prior.distribution = CDC.TESTING.PARAMETERS.PRIOR,
                                    sampling.blocks = CDC.TESTING.PARAMETER.SAMPLING.BLOCKS)
                         
