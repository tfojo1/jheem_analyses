

source('../jheem_analyses/applications/EHE/ehe_specification.R')

orig.root.dir = ROOT.DIR
set.jheem.root.directory(LAPTOP.ROOT.DIR)


# set up baltimore
engine = create.jheem.engine('ehe', 'C.12580', 2025)
params1 = params2 = params3 = params
params1['global.trate'] = .06
sim1 = engine$run(params1)
params2['global.trate'] = .055
sim2 = engine$run(params2)
params3['global.trate'] = .057
sim3 = engine$run(params3)
simplot(sim1, sim2, sim3, 'new')

simset = join.simulation.sets(sim1, sim2, sim3)
simset$save()

# set up miami
engine = create.jheem.engine('ehe', 'C.33100', 2025)
params1 = params2 = params3 = params
params1['global.trate'] = .054
sim1 = engine$run(params1)
params2['global.trate'] = .053
sim2 = engine$run(params2)
params3['global.trate'] = .052
sim3 = engine$run(params3)
simplot(sim1, sim2, sim3, 'new')

simset = join.simulation.sets(sim1, sim2, sim3)
simset$save()


set.jheem.root.directory(orig.root.dir)