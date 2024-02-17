# source("applications/ehe/ehe_specification.R")

BLACK
HISPANIC
NON.BLACK.NON.HISPANIC

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

# intervention <- create.intervention(c(BLACK, HISPANIC, NON.BLACK.NON.HISPANIC), 
#                                     prepuse_10, 
#                                        code = "prepup10") # can pass more than 1

prepuse_upscale10 <- create.intervention(c(BLACK, HISPANIC, NON.BLACK.NON.HISPANIC), 
                                    prepuse_10, 
                                    code = "prepuse10") # can pass more than 1

prepuse_upscale25 <- create.intervention(c(BLACK, HISPANIC, NON.BLACK.NON.HISPANIC), 
                                         prepuse_25, 
                                         code = "prepu25") # can pass more than 1

prep_persistence_30 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.3, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

preppers_upscale30 <- create.intervention(c(BLACK, HISPANIC, NON.BLACK.NON.HISPANIC), 
                                         prep_persistence_30, 
                                         code = "preppers30") # can pass more than 1

prep_persistence_55 <- create.intervention.effect(
  quantity.name = "oral.prep.persistence",
  start.time = start.year,
  effect.values = 0.55, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

preppers_upscale55 <- create.intervention(c(BLACK, HISPANIC, NON.BLACK.NON.HISPANIC), 
                                          prep_persistence_30, 
                                         code = "prepuse55") # can pass more than 1
# 80
