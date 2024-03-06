
# File to set PrEP Upscale Interventions

source("../jheem_analyses/applications/EHE/ehe_specification.R") # re-source to wipe out codes

# BLACK
# HISPANIC
# NON.BLACK.NON.HISPANIC

BLACK.MSM <- create.target.population(race = "black", sex = "msm", name = "Black MSM")
HISPANIC.MSM <- create.target.population(race = "hispanic", sex = "msm", name = "Hispanic MSM")
NON.BLACK.NON.HISPANIC.MSM <- create.target.population(race = "other", sex = "msm",
                                                       name = "Non-Black non-Hispanic MSM")


start.year <- 2025
end.year <- 2035

prepuse_10 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.1, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prepuse_20 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.20, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)


prepuse_25 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.25,
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prepuse_30 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.30, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)


prepuse_40 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.40, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prepuse_50 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.50, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

# prepuse_50 <- create.intervention.effect(
#   quantity.name = "oral.prep.uptake",
#   start.time = start.year,
#   effect.values = 0.50, 
#   times = end.year,
#   scale = "proportion",
#   apply.effects.as = "addend",
#   allow.values.less.than.otherwise = FALSE,
#   allow.values.greater.than.otherwise = TRUE
# )

# prep.use10 <- create.intervention(c(BLACK, HISPANIC), 
#                                     prepuse_10, 
#                                     code = "prepuse10") # can pass more than 1
# 
# prep.use25 <- create.intervention(c(BLACK, HISPANIC), 
#                                          prepuse_25, 
#                                          code = "prepuse25") # can pass more than 1
# 
# prep.use10.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_10, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse10") # can pass more than 1
# 
# prep.use20.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_20, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse20") # can pass more than 1
# 
# prep.use25.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_25, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse25") # can pass more than 1
# 
# prep.use35.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_35, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse35") # can pass more than 1
# 
# prep.use40.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_40, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse40") # can pass more than 1
# # 
# prep.use50.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                       prepuse_50, 
#                                       parameter.distribution=extra.params.dist,
#                                       code = "msmprepuse50") # can pass more than 1

prep_baseline <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.00, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prep_persistence_30 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.30, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

# 
# prep.pers30 <- create.intervention(c(BLACK, HISPANIC), 
#                                          prep_persistence_30, 
#                                    parameter.distribution=extra.params.dist,
#                                          code = "preppers30") # can pass more than 1

baseline.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                      prep_baseline,
                                       parameter.distribution=extra.params.dist,
                                       code = "prepblmsm") # can pass more than 1

prep.pers30.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                       prep_persistence_30,
                                       parameter.distribution=extra.params.dist,
                                       code = "preppers30msm") # can pass more than 1

prepu10_p30 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_10, prep_persistence_30),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu10p30msm")

prepu20_p30 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_20, prep_persistence_30),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu20p30msm")
# 
# prepu25_p30 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
#                                    c(prepuse_25, prep_persistence_30),
#                                    parameter.distribution=extra.params.dist,
#                                    code = "prepu25p30msm")

prepu30_p30 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_30, prep_persistence_30),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu30p30msm")

prepu40_p30 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_40, prep_persistence_30),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu40p30msm")

prep_persistence_50 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.50, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)
# 
# prep.pers50 <- create.intervention(c(BLACK, HISPANIC), 
#                                           prep_persistence_50, 
#                                    parameter.distribution=extra.params.dist,
#                                          code = "preppers50") # can pass more than 1
# 
# prep.pers50.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
#                                        prep_persistence_50, 
#                                        parameter.distribution=extra.params.dist,
#                                        code = "preppers50msm") # can pass more than 1

prepu10_p50 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_10, prep_persistence_50),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu10p50msm")

prepu20_p50 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_20, prep_persistence_50),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu20p50msm")

prepu30_p50 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_30, prep_persistence_50),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu30p50msm")

prepu40_p50 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_40, prep_persistence_50),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu40p50msm")

prepu50_p50 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_50, prep_persistence_50),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu50p50msm")


prep_persistence_80 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.80, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
) 
# 
# prep.pers80 <- create.intervention(c(BLACK, HISPANIC),
#                                           prep_persistence_80,
#                                    parameter.distribution=extra.params.dist,
#                                           code = "preppers80")
# 
# prep.pers80.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
#                                        prep_persistence_80,
#                                        parameter.distribution=extra.params.dist,
#                                        code = "preppers80msm")

prepu10_p80 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_10, prep_persistence_80),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu10p80msm")

prepu20_p80 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_20, prep_persistence_80),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu20p80msm")

prepu30_p80 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_30, prep_persistence_80),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu30p80msm")

prepu40_p80 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_40, prep_persistence_80),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu40p80msm")

prepuse_60 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.60, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prepu60_p80 <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                   c(prepuse_60, prep_persistence_80),
                                   parameter.distribution=extra.params.dist,
                                   code = "prepu60p80msm")

# proportion of use instead of additional use

prepuse_10p <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.1, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prepuse_20p <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.20, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)


prepuse_30p <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.30, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)


prepuse_40p <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.40, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)