# =============================================================================
# OAKLAND TGA: REVIEW full.ehe.oakland
# Run from code/jheem_analyses (same working directory as the calibration).
# Plot order follows test/plotting_test_code.R
#
# Plots every target in FULL.oakland.likelihood.instructions. "DATA:" comments say
# whether Oakland has data for that breakdown (checked 2026-11-02, surveillance
# manager v2026.10.30 + in-session Oakland patches; see Oakland_TGA_calibration_targets.docx)
#
# In the full likelihood but NO Oakland data (skipped by the likelihood):
#   - new diagnoses / diagnosed prevalence, all 2-way breakdowns (county data are 1-way only)
#   - heroin, by age (Oakland heroin is total only)
# Dropped from the Oakland full likelihood (no data): AIDS diagnoses, AIDS deaths, HIV mortality
#   -> still plotted below to check the model's shape
# =============================================================================

# Registers the Oakland calibrations AND applies the in-session data patches
# (deaths, migration, heroin/cocaine, STI ratios), so the data show up on the plots
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations_oakland.R')

# alpha.line = NULL: line transparency follows line width (the pre-June-2026 jheem2 default),
# so a simset draws as many thin, faint lines and a single sim stands out on top of it
location.style.manager = create.style.manager(color.data.by = "location.type", alpha.line = NULL) 
source.style.manager = create.style.manager(color.data.by = "source", alpha.line = NULL) 
stratum.style.manager = create.style.manager(color.data.by = "stratum", alpha.line = NULL) # this is the default right now 

LOCATION = 'TGA.OAKLAND'

# Most recent full.ehe.oakland simset in prelim_results/
full.file = tail(sort(Sys.glob("prelim_results/full.ehe.oakland_simset_*_TGA.OAKLAND.Rdata")), 1)
if (length(full.file) == 0)
    stop("No full.ehe.oakland simset in prelim_results/ - copy it over from the remote desktop")
print(paste0("Loading ", full.file))
load(full.file)
sim.first = simset$first.sim()
sim.last = simset$last.sim()

## --- LIKELIHOOD ---- ## 
full.lik = FULL.oakland.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
full.lik$compare.sims(sim.first, sim.last)   # >1 = last fits that component better
full.lik$compute.piecewise(sim.first)
full.lik$compute.piecewise(sim.last)

# Which breakdowns of Oakland's data exist? (prep/prep.indications not yet checked)
oak.data.strata = function(outcome, location = LOCATION)
{
    d = SURVEILLANCE.MANAGER$data[[outcome]]$estimate
    res = character()
    for (src in names(d)) for (ont in names(d[[src]])) for (st in names(d[[src]][[ont]]))
    {
        arr = d[[src]][[ont]][[st]]
        if (location %in% dimnames(arr)$location &&
            any(!is.na(array.access(arr, location = location))))
            res = c(res, paste0(sub('year__location__?', '', st), ' [', src, ']'))
    }
    if (length(res) == 0) 'NO DATA' else sort(unique(sub('^ \\[', 'total [', res)))
}
for (o in c('prep', 'prep.indications', 'suppression', 'gonorrhea.ratio', 'ps.syphilis.ratio'))
{
    cat('\n==', o, '==\n')
    print(oak.data.strata(o))
}

## --- STANDARD PLOTS ---- ## 

# POPULATION -- DATA: total; age, sex, race; all 2-way - yes (race from race x ethnicity)
simplot(sim.last,
        simset,
        facet.by = "age", split.by = "race", # DATA: age x race - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", split.by = "sex", # DATA: age x sex - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", split.by = "sex", # DATA: sex x race - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("population"), 
        style.manager = source.style.manager, # DATA: total - yes
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("immigration","emigration"), # DATA: totals only (patched: x5, ages 13+); no age/race
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# NEW DIAGNOSES -- DATA: total; age, sex, race, risk (1-way) - yes; 2-way - NO (skipped)
simplot(sim.last,
        simset,
        outcomes = c("new"), 
        style.manager = source.style.manager, # DATA: total - yes
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", # DATA: race - yes
        outcomes = c("new"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "risk", # DATA: risk - yes
        outcomes = c("new"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "sex", # DATA: sex - yes
        outcomes = c("new"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - yes
        outcomes = c("new"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

# DIAGNOSED PREVALENCE -- DATA: total; age, sex, race, risk (1-way) - yes; 2-way - NO (skipped)
simplot(sim.last,
        simset,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager, # DATA: total - yes
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", # DATA: race - yes
        outcomes = c("diagnosed.prevalence"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "risk", # DATA: risk - yes
        outcomes = c("diagnosed.prevalence"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "sex", # DATA: sex - yes
        outcomes = c("diagnosed.prevalence"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - yes
        outcomes = c("diagnosed.prevalence"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 1980:2030)) 

# MORTALITY
simplot(sim.last,
        simset,
        facet.by = "sex", # DATA: NO hiv.deaths for Oakland (likelihood dropped) - check the shape
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("total.mortality"), # DATA: total - yes (patched: county -> TGA, partial years removed)
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# AIDS DIAGNOSES -- DATA: NO aids.diagnoses for Oakland (likelihood dropped) - check the shape
simplot(sim.last,
        simset,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", # DATA: NO - check the shape
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

# TESTING -- nested likelihood (no TGA-level data); any data shown are for other locations
simplot(sim.last,
        simset,
        facet.by = "age", # DATA: no TGA-level data (nested)
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("testing"), # DATA: no TGA-level data (nested)
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("cdc.hiv.test.positivity"), # DATA: likelihood instantiates for Oakland; totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("awareness"), # DATA: no TGA-level data (nested); totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# SUPPRESSION -- DATA: total, age, sex, risk 2019-2023; race 2022-2023 only (basic likelihood)
simplot(sim.last,
        simset,
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = source.style.manager, # DATA: total - yes
        dimension.values = list(year = 2000:2040)) 

simplot(sim.last,
        simset,
        facet.by = "race", # DATA: race - yes (2022-2023 only)
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim.last,
        simset,
        facet.by = "risk", # DATA: risk - yes
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - yes
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim.last,
        simset,
        facet.by = "sex", # DATA: sex - yes
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

# DRUG USE -- Oakland = adult-pop-weighted average of NSDUH Regions 7R + 8R
simplot(sim.last,
        simset,
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager, # DATA: totals - yes
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - cocaine yes; heroin NO (skipped)
        outcomes = c("proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# PREP -- DATA: yes; breakdowns: see oak.data.strata('prep') / ('prep.indications') above
simplot(sim.last,
        simset,
        facet.by = "age", # likelihood: total; age, sex, race; 2-way
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "sex", 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # likelihood: total; age, sex (2017-2018)
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# COVID-PERIOD YEAR-ON-YEAR CHANGES
simplot(sim.last,
        simset,
        outcomes = c("sexual.transmission.rates"), # DATA: gonorrhea/P&S syphilis ratios - total, sex, race, age (patched)
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        outcomes = c("total.hiv.tests.per.population"), # DATA: likelihood instantiates for Oakland
        style.manager = location.style.manager, plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

# FUTURE INCIDENCE PENALTY -- no data; check projected new diagnoses don't swing wildly
simplot(sim.last,
        simset,
        outcomes = c("new", "incidence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2010:2040)) 
