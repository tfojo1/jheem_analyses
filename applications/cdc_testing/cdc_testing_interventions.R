

CDC.TESTING.START.YEAR = 2025.75
CDC.TESTING.LOSS.LAG = 0.25
CDC.TESTING.EARLY.END.YEAR = 2027 
CDC.TESTING.RETURN.LAG = 1 
CDC.TESTING.LATE.END.YEAR = 2029 

set.seed(1234)
proportion.tested.regardless.values = rbeta(1000, shape1 = 4.84, shape2 = 4.84)
dim(proportion.tested.regardless.values) = c(1,1000)
dimnames(proportion.tested.regardless.values)[[1]] = "proportion.tested.regardless"


cdc.testing.cessation.effect = create.intervention.effect(quantity.name = "cdc.funded.testing.of.undiagnosed",
                                                          start.time = CDC.TESTING.START.YEAR,
                                                          effect.values = expression(proportion.tested.regardless),
                                                          times = CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG,
                                                          scale = "rate",
                                                          apply.effects.as = "multiplier",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.testing.cessation = create.intervention(WHOLE.POPULATION,cdc.testing.cessation.effect,code = "cdct.end",parameters = proportion.tested.regardless.values)

#Return 2027

cdc.testing.brief.interruption.effect = create.intervention.effect(quantity.name = "cdc.funded.testing.of.undiagnosed",
                                                          start.time = CDC.TESTING.START.YEAR,
                                                          effect.values = expression(c(proportion.tested.regardless,proportion.tested.regardless)),
                                                          times = c(CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG, CDC.TESTING.EARLY.END.YEAR),
                                                          end.time = CDC.TESTING.EARLY.END.YEAR + CDC.TESTING.RETURN.LAG,
                                                          scale = "rate",
                                                          apply.effects.as = "multiplier",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.testing.cessation.brief.interruption = create.intervention(WHOLE.POPULATION,cdc.testing.brief.interruption.effect,code = "cdct.bintr", parameters = proportion.tested.regardless.values)


#Return 2029 

cdc.testing.prolonged.interruption.effect = create.intervention.effect(quantity.name = "cdc.funded.testing.of.undiagnosed",
                                                                   start.time = CDC.TESTING.START.YEAR,
                                                                   effect.values = expression(c(proportion.tested.regardless,proportion.tested.regardless)),
                                                                   times = c(CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG, CDC.TESTING.LATE.END.YEAR),
                                                                   end.time = CDC.TESTING.LATE.END.YEAR + CDC.TESTING.RETURN.LAG,
                                                                   scale = "rate",
                                                                   apply.effects.as = "multiplier",
                                                                   allow.values.less.than.otherwise = T,
                                                                   allow.values.greater.than.otherwise = F)

cdc.testing.cessation.prolonged.interruption = create.intervention(WHOLE.POPULATION,cdc.testing.prolonged.interruption.effect,code = "cdct.pintr",parameters = proportion.tested.regardless.values)

qbeta(0.975, shape1 = 4.84, shape2 = 4.84)
qbeta(0.025, shape1 = 4.84, shape2 = 4.84)


1/(exp(-qnorm(c(0.025,0.975,0.5), mean = 0, sd = 0.7)) + 1)  
x = seq(0,1, length = 1000)
qplot(c(x,x,x),c(dbeta(x,shape1 = 4.84, shape2 = 4.84),dnorm(log(x)- log(1-x), 0, 0.7)*1/(x*(1-x)), dnorm(x, mean = 0.5, sd = 0.1530612)), color = rep(c("beta","logit norm", "normal"), each = 1000))



simset = rerun.simulations(simset)

#sim.cessation = cdc.testing.cessation$run(sim2, start.year = 2025, end.year = 2035)
