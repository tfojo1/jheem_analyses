
source('applications/cdc_testing/cdc_testing_specification.R')
SURVEILLANCE.MANAGER$put(data = as.numeric(NA),allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2011"))
SURVEILLANCE.MANAGER$put(data = as.numeric(NA), allow.na.to.overwrite = TRUE, outcome = "cdc.hiv.test.positivity",ontology.name = "cdc", source= "cdc.testing", url = "https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.", details = "x", dimension.values = list(location = "MS", year = "2013"))
source("applications/cdc_testing/cdc_testing_parameters.R")
source("applications/cdc_testing/likelihood.R")

N.SIMS = 1000
N.CHUNKS = 1

CDC.TESTING.LOCATIONS = c('AL','CA','FL','GA','IL','LA','MO','MS','NY','TX','WI',
                          'AZ','KY','MD','OH','SC','TN','WA')
# CDC.TESTING.INTERVENTION.CODES = c('noint', 'cdct.end', 'cdct.pintr', 'cdct.bintr')#,'cdc.intr')

#CDC.TESTING.LOCATIONS = c('AL', 'AZ', 'CA', 'FL', 'GA', 'IL', 'IN', 'KY', 'LA', 'MA', 'MD', 'MI', 'MO', 'MS', 'NV', 'NY', 'OH', 'PA', 'SC', 'TN', 'TX', 'VA', 'WA', 'WI')

CDC.TESTING.ANCHOR.YEAR = 2025
if (CDC.TESTING.ANCHOR.YEAR==2025)
{
    CDC.TESTING.INTERVENTION.SUFFIX = ''
}
if (CDC.TESTING.ANCHOR.YEAR==2026)
{
    CDC.TESTING.INTERVENTION.SUFFIX = ".26"
}

CDC.TESTING.INTERVENTION.CODES = c('noint', 
                                   paste0(c('cdct.end', 
                                            'cdct.end.25','cdct.end.50','cdct.end.75',
                                            'cdct.pintr', 'cdct.bintr'), 
                                          CDC.TESTING.INTERVENTION.SUFFIX))

CALIBRATION.CODE = 'final.ehe.state'
VERBOSE = T

source('applications/cdc_testing/cdc_testing_interventions.R')

register.transmute.calibration.info(transmute.code = 'cdct',
                                    from.version = 'ehe',
                                    to.version = 'cdct',
                                    n.iter.first.sim = 1000,
                                    n.iter.subsequent.sims = 100,
                                    likelihood.instructions = cdc.joint.likelihood.instructions,
                                    prior.distribution = CDC.TESTING.PARAMETERS.PRIOR,
                                    sampling.blocks = CDC.TESTING.PARAMETER.SAMPLING.BLOCKS)
                         

# Web tool stuff
CDCT.N.SIM.FOR.WEB = 80
CDCT.WEB.FROM.YEAR = 2015
CDCT.WEB.TO.YEAR = 2030
CDCT.WEB.SEED.FROM.YEAR = CDCT.WEB.FROM.YEAR
CDCT.WEB.SEED.TO.YEAR = 2025
