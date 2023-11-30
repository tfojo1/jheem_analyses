params.2 = params

params.2['other.birth.rate.multiplier'] = 0.01
params.2['hispanic.birth.rate.multiplier'] = 0.01
params.2['black.birth.rate.multiplier'] = 0.01

sim2 = engine$run(parameters = params.2)

simplot(sim2,"population",split.by = "race")
simplot(sim2,"population",split.by = "age")
