

load("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/test.AL.sim.Rdata")

source('applications/cdc_prep/cdc_prep_specification.R')

transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2025, to.year = 2036)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

sim.cdcp = transmuter$transmute(1, parameters = params)

