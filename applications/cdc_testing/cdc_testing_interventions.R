

CDC.TESTING.START.YEAR = 2025.75
CDC.TESTING.LOSS.LAG = 0.25


cdc.testing.cessation.effect = create.intervention.effect(quantity.name = "cdc.funded.testing.of.undiagnosed",
                                                          start.time = CDC.TESTING.START.YEAR,
                                                          effect.values = 0.5,
                                                          times = CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG,
                                                          scale = "rate",
                                                          apply.effects.as = "multiplier",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.testing.cessation = create.intervention(WHOLE.POPULATION,cdc.testing.cessation.effect,code = "cdct.end")

sim.cessation = cdc.testing.cessation$run(sim2, start.year = 2025, end.year = 2035)
