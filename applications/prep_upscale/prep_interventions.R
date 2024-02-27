
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

# 
# prepuse_25 <- create.intervention.effect(
#   quantity.name = "oral.prep.uptake",
#   start.time = start.year,
#   effect.values = 0.25, 
#   times = end.year,
#   scale = "proportion",
#   apply.effects.as = "addend",
#   allow.values.less.than.otherwise = FALSE,
#   allow.values.greater.than.otherwise = TRUE
# )

prepuse_35 <- create.intervention.effect(
  quantity.name = "oral.prep.uptake",
  start.time = start.year,
  effect.values = 0.35, 
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

# prep.use10 <- create.intervention(c(BLACK, HISPANIC), 
#                                     prepuse_10, 
#                                     code = "prepuse10") # can pass more than 1
# 
# prep.use25 <- create.intervention(c(BLACK, HISPANIC), 
#                                          prepuse_25, 
#                                          code = "prepuse25") # can pass more than 1

prep.use10.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                         prepuse_10, 
                                         code = "msmprepuse10") # can pass more than 1

prep.use20.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                      prepuse_20, 
                                      code = "msmprepuse20") # can pass more than 1

prep.use25.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                         prepuse_25, 
                                         code = "msmprepuse25") # can pass more than 1

prep.use35.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                      prepuse_35, 
                                      code = "msmprepuse35") # can pass more than 1

prep.use50.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                      prepuse_50, 
                                      code = "msmprepuse50") # can pass more than 1

prep_persistence_30 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.3, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prep.pers30 <- create.intervention(c(BLACK, HISPANIC), 
                                         prep_persistence_30, 
                                         code = "preppers30") # can pass more than 1

prep.pers30.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                   prep_persistence_30, 
                                   code = "preppers30msm") # can pass more than 1

prep_persistence_50 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.55, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

prep.pers50 <- create.intervention(c(BLACK, HISPANIC), 
                                          prep_persistence_50, 
                                         code = "preppers50") # can pass more than 1

prep.pers50.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM), 
                                   prep_persistence_50, 
                                   code = "preppers50msm") # can pass more than 1

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

prep.pers80 <- create.intervention(c(BLACK, HISPANIC),
                                          prep_persistence_80,
                                          code = "preppers80")

prep.pers80.msm <- create.intervention(c(BLACK.MSM, HISPANIC.MSM),
                                          prep_persistence_80,
                                          code = "preppers80msm")

