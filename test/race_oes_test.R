

source('R/tests/ENGINE_test.R')


engine2 = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10)
params2 = params
params2['other.other.sexual.oe'] = 50
sim2 = engine2$run(params2)

simplot(sim, sim2, outcome='new', facet.by='race')
simplot(sim, sim2, outcome='new', facet.by='risk', split.by='race')
