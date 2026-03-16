# simset = retrieve.simulation.set('ehe', 'FL', 'final.ehe.state', 1000)
# sim = simset$last.sim()


source("applications/adap_cuts/adap_cuts_specification.R")


transmuter = create.jheem.transmuter(sim, 'adapcuts', from.year=2025, to.year=2035)
sim.ac = transmuter$transmute(1)
