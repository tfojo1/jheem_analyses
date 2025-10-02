

# simset = retrieve.simulation.set('ehe','AL','final.ehe.state',1000)
# sim = simset$last.sim()

source('applications/cdc_prep/cdc_prep_specification.R')

transmuter = create.jheem.transmuter(sim, to.version = 'cdcp', from.year = 2025, to.year = 2036)

params = get.medians(CDC.PREP.PARAMETERS.PRIOR)

sim.cdcp = transmuter$transmute(1, parameters = params)
