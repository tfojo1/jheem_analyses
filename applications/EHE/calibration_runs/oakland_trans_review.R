# =============================================================================
# OAKLAND TGA: REVIEW trans.ehe.oakland
# Run from code/jheem_analyses (same working directory as the calibration).
#
# Plots every target in transmission.oakland.likelihood.instructions that has
# Oakland data. Data availability checked 2026-11-02 (surveillance manager
# v2026.10.29 + in-session Oakland patches); see Oakland_TGA_calibration_targets.docx
#
# In the trans likelihood but NO Oakland data (skipped by the likelihood, not plotted):
#   - new diagnoses, race x risk      (county data are 1-way only)
#   - diagnosed prevalence, race x risk (county data are 1-way only)
#   - heroin, by age                   (Oakland heroin is total only)
# Dropped from the Oakland trans likelihood (no data): AIDS diagnoses, AIDS deaths, HIV mortality
# =============================================================================

# Registers the Oakland calibrations AND applies the in-session data patches
# (deaths, migration, heroin/cocaine), so the data show up on the plots
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations_oakland.R')

# alpha.line = NULL: line transparency follows line width (the pre-June-2026 jheem2 default),
# so a simset draws as many thin, faint lines and a single sim stands out on top of it
location.style.manager = create.style.manager(color.data.by = "location.type", alpha.line = NULL) 
source.style.manager = create.style.manager(color.data.by = "source", alpha.line = NULL) 
stratum.style.manager = create.style.manager(color.data.by = "stratum", alpha.line = NULL) # this is the default right now 

LOCATION = 'TGA.OAKLAND'

load("prelim_results/trans.ehe.oakland_simset_2026-10-30_TGA.OAKLAND.Rdata")
sim.first = simset$first.sim()
sim.last = simset$last.sim()

## --- LIKELIHOOD ---- ## 
trans.lik = transmission.oakland.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
trans.lik$compare.sims(sim.first, sim.last)   # >1 = last fits that component better
trans.lik$compute.piecewise(sim.first)
trans.lik$compute.piecewise(sim.last)

## --- POPULATION ---- ## 
# Trans likelihood: total; age, sex, race; all 2-way. Oakland data: all available
# (race from race x ethnicity)
simplot(sim.last,
        simset,
        outcomes = c("population"), 
        style.manager = source.style.manager, # DATA: total - yes
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "sex", # DATA: sex - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", # DATA: race - yes
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
        facet.by = "age", split.by = "race", # DATA: age x race - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "race", split.by = "sex", # DATA: sex x race - yes
        outcomes = c("population"), 
        style.manager = stratum.style.manager,
        dimension.values = list(year = 2000:2030)) 

## --- NEW DIAGNOSES ---- ## 
# Trans likelihood: total; race, risk; race x risk. Oakland data: 1-way only (no race x risk)
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

# race x risk - NO Oakland data (in the likelihood, but skipped)

## --- DIAGNOSED PREVALENCE ---- ## 
# Trans likelihood: total; race, risk; race x risk. Oakland data: 1-way only (no race x risk)
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

# race x risk - NO Oakland data (in the likelihood, but skipped)

## --- DRUG USE ---- ## 
# Trans likelihood: total; age. Oakland = adult-pop-weighted average of NSDUH Regions 7R + 8R
simplot(sim.last,
        simset,
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager, # DATA: total - yes (heroin and cocaine)
        dimension.values = list(year = 2000:2030)) 

simplot(sim.last,
        simset,
        facet.by = "age", # DATA: age - cocaine yes; heroin NO (in the likelihood, but skipped)
        outcomes = c("proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

## --- IDU ACTIVE/PRIOR RATIO ---- ## 
# In the trans likelihood as a prior on the model's own ratio - no data, nothing to plot
