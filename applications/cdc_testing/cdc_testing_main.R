
source('applications/cdc_testing/cdc_testing_specification.R')
SURVEILLANCE.MANAGER$put(data = as.numeric(NA),allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2011"))
SURVEILLANCE.MANAGER$put(data = as.numeric(NA), allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2013"))
source("applications/cdc_testing/cdc_testing_parameters.R")
source("applications/cdc_testing/likelihood.R")
source('applications/cdc_testing/cdc_testing_interventions.R')

N.SIMS = 1000
N.CHUNKS = 1

CDC.TESTING.LOCATIONS = c('AL','CA','FL','GA','IL','LA','MO','MS','NY','TX','WI',
                          'AZ','MD','OH','WA')
CDC.TESTING.EXTRA.LOCATIONS = c('AR','AZ','CO','KY','MD','OK','OH','PA','SC','TN','WA')
CDC.TESTING.INTERVENTION.CODES = c('noint', 'cdct.end', 'cdct.pintr', 'cdct.bintr')
CALIBRATION.CODE = 'final.ehe.state'
VERBOSE = T

register.transmute.calibration.info(transmute.code = 'cdct',
                                    from.version = 'ehe',
                                    to.version = 'cdct',
                                    n.iter.first.sim = 1000,
                                    n.iter.subsequent.sims = 100,
                                    likelihood.instructions = cdc.joint.likelihood.instructions,
                                    prior.distribution = CDC.TESTING.PARAMETERS.PRIOR,
                                    sampling.blocks = CDC.TESTING.PARAMETER.SAMPLING.BLOCKS)
                         
