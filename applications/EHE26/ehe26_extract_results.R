# EHE26: pull results from the saved simsets and compute impact
# Run after ehe26_run_interventions.R has finished.
# Based on applications/cdc_testing/cdc_testing_extract_results.R

source('../jheem_analyses/applications/EHE26/ehe26_main.R')

collection = create.simset.collection(version = VERSION,
                                      calibration.code = CALIBRATION.CODE,
                                      locations = LOCATIONS,
                                      interventions = INTERVENTION.CODES,
                                      n.sim = N.SIM)

# 1. PULL COUNTS BY YEAR AND SEX ----
# Counts can be added up across years and locations.
# Result dimensions: year x sex x sim x outcome x location x intervention
print("PULLING COUNTS...")
counts = collection$get(outcomes = c('incidence', 'new', 'population', 'prep.uptake'),
                        output = 'numerator',
                        keep.dimensions = c('year', 'sex'),
                        dimension.values = list(year = KEEP.FROM.YEAR:END.YEAR),
                        verbose = VERBOSE)

# 2. PULL COVERAGE BY YEAR AND SEX ----
# Proportions: use these to check the targets were reached in 2030.
# Do NOT add these up across locations.
print("PULLING COVERAGE...")
coverage = collection$get(outcomes = c('testing', 'awareness', 'suppression', 'prep.uptake.proportion'),
                          output = 'value',
                          keep.dimensions = c('year', 'sex'),
                          dimension.values = list(year = KEEP.FROM.YEAR:END.YEAR),
                          verbose = VERBOSE)

# 3. SAVE THE RAW RESULTS ----
if (!dir.exists(RESULTS.DIR))
    dir.create(RESULTS.DIR, recursive = T)

results.file = file.path(RESULTS.DIR, paste0('ehe26_results_', Sys.Date(), '.Rdata'))
save(counts, coverage, file = results.file)
print(paste0("Saved results to ", results.file))

# 4. IMPACT: INFECTIONS AVERTED AMONG MSM, 2026-2035 ----
# Example: noint = 1,000 infections, intervention = 700 infections
#          averted = 300, percent reduction = 300/1000 = 30%

# 4a. Put the dimensions in a known order so we can index by name
counts.ordered = apply(counts, c('year', 'sex', 'outcome', 'sim', 'location', 'intervention'), function(v) v)

# 4b. Cumulative MSM incidence -> sim x location x intervention
years = as.character(BASELINE.YEAR:END.YEAR)
msm.incidence = counts.ordered[years, 'msm', 'incidence', , , , drop = F]
cum.incidence = apply(msm.incidence, c('sim', 'location', 'intervention'), sum)

# 4c. Compare each intervention with noint, simulation by simulation
#     averted = noint - intervention
cum.noint = cum.incidence[, , 'noint']
averted = -sweep(cum.incidence, c(1, 2), cum.noint, '-')
pct.reduction = 100 * sweep(averted, c(1, 2), cum.noint, '/')

# 4d. Summarize across simulations: mean and 95% interval
summarize = function(x) c(mean = mean(x, na.rm = T),
                          lower = unname(quantile(x, 0.025, na.rm = T)),
                          upper = unname(quantile(x, 0.975, na.rm = T)))

averted.summary = apply(averted, c('location', 'intervention'), summarize)
pct.reduction.summary = apply(pct.reduction, c('location', 'intervention'), summarize)

print("Infections averted among MSM, 2026-2035:")
print(round(averted.summary))
print("Percent reduction among MSM, 2026-2035:")
print(round(pct.reduction.summary, 1))

save(averted.summary, pct.reduction.summary,
     file = file.path(RESULTS.DIR, paste0('ehe26_impact_', Sys.Date(), '.Rdata')))
