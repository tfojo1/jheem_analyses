
engine = create.jheem.engine("cdcp", "AL", end.year = 2035)

last.sim = results$last.sim()

params = last.sim$params

sim = engine$run(params)

simplot(last.sim,sim,"new")

params
