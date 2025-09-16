
CDC.TESTING.INTERVENTION.SUFFIX

if (CDC.TESTING.ANCHOR.YEAR == 2025)
    CDC.TESTING.START.YEAR = 2025.75
if (CDC.TESTING.ANCHOR.YEAR == 2026)
    CDC.TESTING.START.YEAR = 2026 + 1/12

CDC.TESTING.LOSS.LAG = 0.25
CDC.TESTING.EARLY.END.YEAR = 2027 
CDC.TESTING.RETURN.LAG = 1 
CDC.TESTING.LATE.END.YEAR = 2029 
CDC.TESTING.END.YEAR = 2027.75

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
                                            code = paste0("cdct.end", CDC.TESTING.INTERVENTION.SUFFIX),
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
                                                               code = paste0("cdct.bintr", CDC.TESTING.INTERVENTION.SUFFIX),
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
                                                                   code = paste0("cdct.pintr", CDC.TESTING.INTERVENTION.SUFFIX),
                                                                   parameters = proportion.tested.regardless.values)

# Return Fiscal Year 2028 (ie, Oct 2027)
cdc.testing.interruption.effect = create.intervention.effect(quantity.name = "cdc.effect",
                                                             start.time = CDC.TESTING.START.YEAR,
                                                             effect.values = c(0,0),
                                                             times = c(CDC.TESTING.START.YEAR + CDC.TESTING.LOSS.LAG, CDC.TESTING.END.YEAR),
                                                             end.time = CDC.TESTING.END.YEAR + CDC.TESTING.RETURN.LAG,
                                                             scale = "proportion",
                                                             apply.effects.as = "value",
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F)

cdc.testing.interruption = create.intervention(WHOLE.POPULATION,
                                               cdc.testing.prolonged.interruption.effect,
                                               code = paste0("cdct.intr", CDC.TESTING.INTERVENTION.SUFFIX),
                                               parameters = proportion.tested.regardless.values)
