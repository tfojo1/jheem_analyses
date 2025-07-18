

CDC.TESTING.START.YEAR = 2025.75
CDC.TESTING.LOSS.LAG = 0.25
CDC.TESTING.EARLY.END.YEAR = 2027 
CDC.TESTING.RETURN.LAG = 1 
CDC.TESTING.LATE.END.YEAR = 2029 

set.seed(1234)
proportion.tested.regardless.values = rbeta(1000, shape1 = 4.84, shape2 = 4.84)
dim(proportion.tested.regardless.values) = c(1,1000)
dimnames(proportion.tested.regardless.values)[[1]] = "proportion.tested.regardless"


proportion.tested.regardless.effect = create.intervention.effect(quantity.name = "proportion.cdc.tests.done.regardless",
                                                                 start.time = -Inf,
                                                                 effect.values = 'proportion.tested.regardless',
                                                                 times = 0,
                                                                 scale = "proportion",
                                                                 apply.effects.as = "value",
                                                                 allow.values.less.than.otherwise = T,
                                                                 allow.values.greater.than.otherwise = T)

cdc.testing.cessation.effect = create.intervention.effect(quantity.name = "cdc.effect",
                                                          start.time = CDC.TESTING.START.YEAR,
                                                          effect.values = 0,
                                                          times = CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG,
                                                          scale = "proportion",
                                                          apply.effects.as = "value",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.testing.cessation = create.intervention(WHOLE.POPULATION,
                                            cdc.testing.cessation.effect,
   #                                         proportion.tested.regardless.effect,
                                            code = "cdct.end",
                                            parameters = proportion.tested.regardless.values)

#Return 2027

cdc.testing.brief.interruption.effect = create.intervention.effect(quantity.name = "cdc.effect",
                                                          start.time = CDC.TESTING.START.YEAR,
                                                          effect.values = c(0,0),
                                                          times = c(CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG, CDC.TESTING.EARLY.END.YEAR),
                                                          end.time = CDC.TESTING.EARLY.END.YEAR + CDC.TESTING.RETURN.LAG,
                                                          scale = "proportion",
                                                          apply.effects.as = "value",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.testing.cessation.brief.interruption = create.intervention(WHOLE.POPULATION,
                                                               cdc.testing.brief.interruption.effect,
                                                       #        proportion.tested.regardless.effect,
                                                               code = "cdct.bintr",
                                                               parameters = proportion.tested.regardless.values)


#Return 2029 

cdc.testing.prolonged.interruption.effect = create.intervention.effect(quantity.name = "cdc.effect",
                                                                   start.time = CDC.TESTING.START.YEAR,
                                                                   effect.values = c(0,0),
                                                                   times = c(CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG, CDC.TESTING.LATE.END.YEAR),
                                                                   end.time = CDC.TESTING.LATE.END.YEAR + CDC.TESTING.RETURN.LAG,
                                                                   scale = "proportion",
                                                                   apply.effects.as = "value",
                                                                   allow.values.less.than.otherwise = T,
                                                                   allow.values.greater.than.otherwise = F)

cdc.testing.cessation.prolonged.interruption = create.intervention(WHOLE.POPULATION,
                                                                   cdc.testing.prolonged.interruption.effect,
                                                              #     proportion.tested.regardless.effect,
                                                                   code = "cdct.pintr",
                                                                   parameters = proportion.tested.regardless.values)

