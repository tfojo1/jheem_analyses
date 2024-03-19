
# Sensitivity Analyses

# lowering PrEP efficacy to 60% 
prep_eff_60 <- create.intervention.effect(
  quantity.name = "oral.prep.msm.rr",
  start.time = start.year,
  effect.values = 0.6, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)



#oral.prep.heterosexual.rr
#oral.prep.idu.rr

x <- simset$parameters
rowMeans(x)
# 
# make a scatterplot
# x = parameter
# y = percent.reduction


dimnames(simset$parameters)

x <- dimnames(simset$parameters)[[1]]
