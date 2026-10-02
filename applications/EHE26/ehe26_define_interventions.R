# EHE26: intervention definitions
# Sourced by ehe26_main.R (which sets BASELINE.YEAR and IMPLEMENTED.BY.YEAR)
#
# How every effect works:
#   1. It starts at BASELINE.YEAR (2026) at the current (no-intervention) level.
#   2. It rises linearly to the TARGET by IMPLEMENTED.BY.YEAR (2030).
#   3. It stays at the target after 2030.
#   4. allow.values.less.than.otherwise = F means we never push a value DOWN.
#      Example: if suppression in 2030 would already be 92% without intervention,
#      a 90% target leaves it at 92%.

# 1. TARGETS ----
# These are placeholders. Change them as we decide.
PREP.TARGET = 0.50         # 50% of MSM eligible for PrEP start oral PrEP
TESTING.TARGET = 1         # 1 HIV test per person per year
SUPPRESSION.TARGET = 0.90  # 90% of diagnosed MSM are virally suppressed

# 2. TARGET POPULATION ----
ALL.MSM = create.target.population(sex = 'msm', name = 'All MSM')

# 3. INTERVENTION EFFECTS ----

# 3a. PrEP uptake (proportion)
prep.effect = create.intervention.effect(quantity.name = 'oral.prep.uptake',
                                         start.time = BASELINE.YEAR,
                                         effect.values = PREP.TARGET,
                                         times = IMPLEMENTED.BY.YEAR,
                                         scale = 'proportion',
                                         apply.effects.as = 'value',
                                         allow.values.less.than.otherwise = F,
                                         allow.values.greater.than.otherwise = T)

# 3b. HIV testing (rate = tests per person per year)
testing.effect = create.intervention.effect(quantity.name = 'general.population.testing',
                                            start.time = BASELINE.YEAR,
                                            effect.values = TESTING.TARGET,
                                            times = IMPLEMENTED.BY.YEAR,
                                            scale = 'rate',
                                            apply.effects.as = 'value',
                                            allow.values.less.than.otherwise = F,
                                            allow.values.greater.than.otherwise = T)

# 3c. Viral suppression among diagnosed (proportion)
suppression.effect = create.intervention.effect(quantity.name = 'suppression.of.diagnosed',
                                                start.time = BASELINE.YEAR,
                                                effect.values = SUPPRESSION.TARGET,
                                                times = IMPLEMENTED.BY.YEAR,
                                                scale = 'proportion',
                                                apply.effects.as = 'value',
                                                allow.values.less.than.otherwise = F,
                                                allow.values.greater.than.otherwise = T)

# 4. INTERVENTIONS ----
# Each one = target population + one or more effects + a unique code.
# overwrite.existing.intervention = T lets us re-source this file after edits.

# 4a. PrEP only
msm.prep = create.intervention(ALL.MSM,
                               prep.effect,
                               code = 'ehe26.msm.prep',
                               overwrite.existing.intervention = T)

# 4b. Testing only
msm.test = create.intervention(ALL.MSM,
                               testing.effect,
                               code = 'ehe26.msm.test',
                               overwrite.existing.intervention = T)

# 4c. Suppression only
msm.supp = create.intervention(ALL.MSM,
                               suppression.effect,
                               code = 'ehe26.msm.supp',
                               overwrite.existing.intervention = T)

# 4d. All three together
msm.all = create.intervention(ALL.MSM,
                              prep.effect,
                              testing.effect,
                              suppression.effect,
                              code = 'ehe26.msm.all',
                              overwrite.existing.intervention = T)

# 4e. No intervention (built into jheem2, code = 'noint')
noint = get.null.intervention()

# 5. LIST OF ALL INTERVENTIONS ----
# When you add a new intervention above, also add it here.
# main.R reads the codes from this list, so nothing else needs to change.
INTERVENTION.LIST = list(noint,
                         msm.prep,
                         msm.test,
                         msm.supp,
                         msm.all)

# Codes, e.g. c('noint', 'ehe26.msm.prep', ...)
INTERVENTION.CODES = sapply(INTERVENTION.LIST, function(int) int$code)
