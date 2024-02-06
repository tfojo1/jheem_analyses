source("applications/ehe/ehe_specification.R")

BLACK
HISPANIC
NON.BLACK.NON.HISPANIC

start.year <- 2025
end.year <- 2035

increaseprep_10 <- create.intervention.effect(
  quantity.name = "prep.uptake",
  start.time = start.year,
  effect.values = 0.1, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
    )

intervention <- create.intervention(BLACK, increaseprep_10, 
                                    code = "prepup10") # can pass more than 1

prep_persistence_30 <- create.intervention.effect(
  quantity.name = "prep.persistence",
  start.time = start.year,
  effect.values = 0.3, 
  times = end.year,
  scale = "proportion",
  apply.effects.as = "addend",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

intervention