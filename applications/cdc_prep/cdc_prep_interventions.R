
CDC.PREP.INTERVENTION.SUFFIX = ".26"


CDC.PREP.START.YEAR = 2026 + 9/12 #October 2026 start 

CDC.PREP.LOSS.LAG = 0.25
CDC.PREP.RETURN.LAG = 1 
CDC.PREP.LATE.END.YEAR = 2029  #reconsider for prolonged 
CDC.PREP.END.YEAR = 2027.75

set.seed(1234)
proportion.tested.regardless.values = rbeta(1000, shape1 = 4.84, shape2 = 4.84)
dim(proportion.tested.regardless.values) = c(1,1000)
dimnames(proportion.tested.regardless.values)[[1]] = "proportion.tested.regardless"


proportion.tested.regardless.effect = create.intervention.effect(quantity.name = "proportion.tested.regardless",
                                                                 start.time = -Inf,
                                                                 effect.values = 'proportion.tested.regardless',
                                                                 times = 0,
                                                                 scale = "proportion",
                                                                 apply.effects.as = "value",
                                                                 allow.values.less.than.otherwise = T,
                                                                 allow.values.greater.than.otherwise = T)

cdc.testing.cessation.effect = create.intervention.effect(quantity.name = "cdc.testing.effect",
                                                          start.time = CDC.PREP.START.YEAR,
                                                          effect.values = 0,
                                                          times = CDC.PREP.START.YEAR + CDC.PREP.LOSS.LAG,
                                                          scale = "proportion",
                                                          apply.effects.as = "value",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.prep.cessation.effect = create.intervention.effect(quantity.name = "cdc.prep.effect",
                                                          start.time = CDC.PREP.START.YEAR,
                                                          effect.values = 0,
                                                          times = CDC.PREP.START.YEAR + CDC.PREP.LOSS.LAG,
                                                          scale = "proportion",
                                                          apply.effects.as = "value",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)


cdc.contact.tracing.cesation.effect = create.intervention.effect(quantity.name = "cdc.contact.tracing.effect",
                                                          start.time = CDC.PREP.START.YEAR,
                                                          effect.values = 0,
                                                          times = CDC.PREP.START.YEAR + CDC.PREP.LOSS.LAG,
                                                          scale = "proportion",
                                                          apply.effects.as = "value",
                                                          allow.values.less.than.otherwise = T,
                                                          allow.values.greater.than.otherwise = F)

cdc.cessation = create.intervention(WHOLE.POPULATION,
                                            cdc.testing.cessation.effect,
                                            proportion.tested.regardless.effect,
                                            cdc.contact.tracing.cesation.effect,
                                            cdc.prep.cessation.effect,
                                            code = paste0("cdcp.end", CDC.PREP.INTERVENTION.SUFFIX),
                                            parameters = proportion.tested.regardless.values)

