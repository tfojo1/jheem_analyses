# EHE26 STEP 2: observed trends vs the JHEEM baseline projection, from saved model numbers
#
# Purpose
#   Figure 1 (ehe26_benchmark_trends.R) shows observed surveillance data up to 2022-2024.
#   JHEEM is calibrated to these data and projects its own trend to the end of each simset.
#   This script compares the two and shows what the model assumes after the data end.
#
# This step does NOT load simulations. It reads the raw model numbers saved by step 1
# (ehe26_jheem_extract.R), so you can change measures, groups and figures and re-run in seconds.
#
# Before running (from the jheem_analyses/ folder)
#   1. ehe26_benchmark_trends.R  -> observed_headline_trends.csv, observed_prep_to_need.csv
#   2. ehe26_jheem_extract.R     -> raw model numbers, one file per state (slow; run once)
#   3. this file                 -> source it in RStudio, or Rscript applications/EHE26/ehe26_jheem_baseline_trends.R
#   Shared settings (simset file, folders, years): ehe26_jheem_settings.R
#
# What it makes (in OUT.DIR)
#   1. jheem_baseline_by_state.csv            figure 11 measures: model median and 95% interval, per state and year
#   2. jheem_outcomes_by_state.csv            raw JHEEM outcomes (dictionary below), everyone and MSM
#   3. jheem_prep_by_group.csv                PrEP by group (PREP.GROUPS) and measure (PREP.MEASURES)
#   4. fig11_observed_vs_jheem_baseline.png   observed vs JHEEM baseline, by Medicaid expansion and all states
#   5. fig12_jheem_outcome_trends.png         how each raw JHEEM outcome evolves
#   6. fig13_prep_to_need_observed_vs_jheem.png  PrEP-to-need where the data allow a direct comparison:
#                                             total, male, female, age (observed vs JHEEM, both denominators)
#   7. fig14_prep_to_need_jheem_only.png      PrEP-to-need for MSM, race, MSM x race (JHEEM only: no observed data)
#   8. fig15_prep_need_vs_2017_2018.png       the model's PrEP need each year / its 2017-2018 level, by group
#
# =====================================================================================
# MODEL OUTCOME DICTIONARY (from applications/EHE/ehe_specification.R)
# =====================================================================================
# Model quantities behind the outcomes (what drives them, and what is assumed after the data end)
#   A. prep.indication = share of uninfected people with an indication for PrEP.
#      Trend: logistic-tail curve, anchored 2010, ceiling 85%, calibrated level by group
#      (msm / non-msm). So in the model, PrEP need CHANGES over time.
#      (input_managers/prep_input_manager.R, get.prep.indication.functional.form)
#   B. oral.prep.uptake = share of people WITH an indication who receive PrEP.
#      Trend: logistic-tail curve, anchored 2020, ceiling 50%, calibrated intercepts and
#      slopes by group (msm, heterosexual, idu). After the data end it keeps rising but
#      bends toward the 50% ceiling. Starts in 2014 (zero in 2011).
#      (get.prep.use.functional.form)
#   C. oral.prep.persistence = share still on PrEP after a year. Constant over time (calibrated).
#      LAI PrEP = 0 in the baseline.
#   D. proportion.receiving.prep = prep.indication x oral.prep.uptake  (share of ALL uninfected on PrEP)
#   E. general.population.testing = HIV tests per person per year (uninfected / diagnosed population).
#      Trend: logistic-linear from 2010, ceiling 90% (on the proportion scale), calibrated
#      intercepts and slopes by group. After the data end it keeps its post-2010 slope.
#      Before 2010: ramp-up from 1982 (testing.ramp.up, testing.ramp.rr).
#      (applications/EHE/ehe_specification_helpers.R, get.testing.functional.form)
#   F. testing.of.undiagnosed = general.population.testing x (1 + undiagnosed.testing.increase)
#      People with undiagnosed HIV test more often (constant multiplier, from BRFSS high-risk ratios).
#   G. suppression.of.diagnosed = share of diagnosed people who are virally suppressed.
#      Trend: logistic-linear from 2008, ceiling 90%, calibrated intercepts and slopes by group.
#      After the data end it keeps rising toward 90%. Zero before 1996.
#      (input_managers/continuum_input_manager.R, get.suppression.functional.form)
#   H. COVID: PrEP uptake, testing and suppression are multiplied down in 2020-2021
#      (covid.on), then return to their trend.
#
# How the model was calibrated (state level, calibration code 'final.ehe.state')
#   1. Registered in applications/EHE/calibration_runs/ehe_register_calibrations.R
#   2. Uses full.state.likelihood.instructions.half.weight
#      = full.state.likelihood.instructions (applications/EHE/ehe_likelihoods.R, ~line 2616)
#        with every weight x 1/2. FULL.WEIGHT = 1.
#   3. "Breakdowns used" = levels.of.stratification: 0 = state total, 1 = one dimension at a time
#      (e.g. by age, by sex, ...), 2 = two dimensions together (e.g. age x sex).
#   4. A likelihood without to.year uses every data year from from.year onward.
#   5. Weights below are before the x 1/2. A larger weight = the fit pays more attention to that data.
#
# Tracked outcomes (what simset$get() returns). Breakdowns kept = dimensions you can split by.
#   1. prep.indications  - COUNT. Number of people with an indication for PrEP in the year.
#                          = uninfected x prep.indication. Kept: location, age, race, sex.
#        Calibration: prep.indications.likelihood.instructions
#          data = 'prep.indications', CDC estimates of PrEP need (source cdc.prep.indications)
#          years = 2017-2018 only (the only years CDC published; it carried 2018 forward after that)
#          breakdowns used = state total, by age, by sex
#          error = CV 0.5: very loose, the model may land anywhere from ~0 to 2x the CDC number
#          weight = 1
#        So after 2018 nothing in the data pins down PrEP need: its trend is the model's assumption (A).
#   2. prep.uptake       - COUNT. Number of people who received a PrEP prescription in the year.
#                          = uninfected x proportion.receiving.prep
#                          = uninfected x prep.indication x oral.prep.uptake,
#                          so everyone counted here has an indication.
#                          Kept: location, age, race, sex.
#        Calibration: prep.uptake.likelihood.instructions
#          data = 'prep' (PrEP users). Sources in the data manager: AIDSVu (2012-2024) and
#                 CDC AtlasPlus (cdc.prep, 2017-2023). The likelihood does not restrict the source.
#          years = from 2007 onward (in practice 2012-2024, as the data allow)
#          breakdowns used = state total, by age / sex / race, and two-way (e.g. age x sex)
#          error = CV 0.012 (tight), weight = 0.3
#   3. prep.uptake.proportion - PROPORTION. prep.uptake / prep.indications
#                          = share of people with an indication who received PrEP (~ oral.prep.uptake).
#                          The denominator changes each year (unlike our observed PrEP-to-need,
#                          which divides by fixed 2017-2018 indications). Kept: location, age, race, sex.
#        Calibration: no likelihood of its own. It follows from outcomes 1 and 2.
#   4. general.population.testing - RATE. Average HIV tests per person per year (quantity E),
#                          weighted by population. Kept: location, age, race, sex, risk.
#        Calibration: no likelihood of its own. It is informed through outcomes 5, 6 and 7.
#   5. testing           - PROPORTION. Share of adults (18+) without HIV tested in the past year,
#                          converted from the testing rate. Kept: location, age, race, sex, risk.
#        Calibration: proportion.tested.basic.likelihood.instructions
#          data = 'proportion.tested', BRFSS (source brfss, states 2013-2024)
#          years = from 2010 onward (in practice 2013-2024)
#          breakdowns used = state total, by age / sex / race / risk (risk = MSM vs not, 2014-2024)
#          error = the survey's own variance, weight = 1
#   6. total.hiv.tests   - COUNT. Number of HIV tests in the year
#                          = tests among uninfected + new diagnoses. Kept: location, age, race, sex, risk.
#        Calibration: not directly. It enters through test positivity:
#          hiv.test.positivity.basic.likelihood.instructions
#          model = cdc.hiv.test.positivity = 2.81 x new diagnoses / total.hiv.tests
#                  (2.81 = bias of CDC-funded tests vs all tests, from cdc_positivity_bias.R)
#          data = 'cdc.hiv.test.positivity', positivity of CDC-funded tests (source cdc.testing, 2011-2021)
#          years = 2014-2020, breakdowns used = state total only
#          error = CV 0.5, weight = 18
#   7. total.hiv.tests.per.population - PROPORTION. total.hiv.tests / population
#                          (tests per person; x100 = tests per 100 people). Kept: location only.
#        Calibration: number.of.tests.year.on.year.change.likelihood.instructions (a "COVID likelihood")
#          data = 'hiv.tests.per.population', CDC HIV tests per population (source cdc.testing, 2011-2021)
#          only the CHANGE from one year to the next is fitted (ratio of year t to year t-1),
#          not the level. This is mainly what captures the testing drop in 2020-2021.
#          years = from 2008 onward (in practice 2011-2021), breakdowns used = state total only
#          error = CV 0.03 on each year, ratio CV 1.2, weight = 18
#   8. suppression       - PROPORTION. Share of people with diagnosed HIV who are suppressed
#                          (quantity G). Kept: location, age, race, sex, risk.
#        Calibration: suppression.basic.likelihood.instructions.state
#          data = 'suppression', CDC surveillance (source cdc.hiv, states 2010-2023;
#                 MSM breakdown from 2017)
#          years = from 2008 onward (in practice 2010-2023)
#          breakdowns used = state total, by age / sex / race / risk
#          error = SD 0.01 (1 percentage point), weight = 1
#   9. awareness         - PROPORTION. Diagnosed / all people with HIV. Not set directly:
#                          results from testing and new infections. Kept: location, age.
#        Calibration: awareness.basic.likelihood.instructions
#          data = 'awareness', CDC estimates of knowledge of status (source cdc.hiv, states 2010-2022).
#                 CDC's own numbers are model-based (back-calculated from diagnoses).
#          years = from 2008 onward (in practice 2010-2022), breakdowns used = state total only
#          error = the data's own CV, weight = 18
#
# What this means for the projection
#   All these likelihoods stop at the last data year (2021-2024). After that, the trends come
#   only from the assumptions A-H above (shape, slope and ceiling of each curve).
#   The only likelihood that looks past the data is the future incidence penalty
#   (future.new.incidence.change.likelihood.instructions, ehe_likelihoods.R ~line 2192-2217).
#   It penalizes large changes in new diagnoses ('new') and 'incidence', by age, race, sex and risk:
#     a. 5-year changes pivoting around 2026-2030 (shouldn't swing more than ~2-fold)
#     b. 10-year changes ending 2026-2035 (shouldn't change more than ~2.5-fold)
#   It does not target PrEP, testing or suppression directly, but it indirectly rules out
#   future PrEP / testing / suppression paths that would make incidence jump or collapse.
#   Sex groups in the model: 'msm', 'heterosexual_male', 'female'.
#   Outcomes without sex (7, 9) are shown for everyone only.
# =====================================================================================
#
# How the model values are built (same definitions as the observed data)
#   Groups are made by adding up the saved cells (year x age x race x sex x sim):
#   counts are summed; proportions = summed numerators / summed denominators.
#   1. PrEP-to-need, computed two ways for the model (both from the model's own output):
#      a. fixed need     = model PrEP users each year / the model's mean PrEP indications in 2017-2018.
#                          Same definition as the observed PrEP-to-need (which uses CDC 2017-2018 need).
#      b. need same year = model PrEP users each year / the model's PrEP indications that same year
#                          (= the model's prep.uptake.proportion). Uses the model's changing need.
#      The 2017-2018 values are MODEL values from the saved simset numbers, not surveillance data.
#      Version (a) needs model years 2017-2018 in the simset file (a 'noint' file may start later).
#      Male = model msm + heterosexual_male (the data's "male"). MSM is its own group (model only).
#   2. Viral suppression = model "suppression" (among diagnosed); MSM = model sex 'msm'.
#   3. Awareness = model "awareness" (total only in the model).
#   4. Tested in past year = model "testing" (proportion tested in the past year); MSM = sex 'msm'.
#   For each state: the value is computed in each simulation, then we keep the median and the
#   2.5th / 97.5th percentiles across simulations, for each year.
#
# "Baseline" = no added interventions (SIMSET.FILE in ehe26_jheem_settings.R: 'noint' or 'baseline').

source('../jheem_analyses/applications/EHE26/ehe26_jheem_settings.R')
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. SETTINGS ----
LAST.DATA.YEAR = 2024                   # shaded projection period starts after this
STATES.TO.USE = NULL                    # NULL = every state saved by step 1; e.g. c('AL', 'MD')

# Raw JHEEM outcomes shown in figure 12 (see the dictionary above).
#   type: 'count' (shown as an index, 2019 = 1), 'proportion' (shown in %), 'rate' (as is)
#   has.sex: T = also shown for MSM
#   prep.uptake.proportion is computed here as prep.uptake / prep.indications
FIGURE12.OUTCOMES = data.frame(
    outcome = c('prep.indications', 'prep.uptake', 'prep.uptake.proportion',
                'general.population.testing', 'testing', 'total.hiv.tests',
                'total.hiv.tests.per.population', 'suppression', 'awareness'),
    type = c('count', 'count', 'proportion',
             'rate', 'proportion', 'count',
             'proportion', 'proportion', 'proportion'),
    has.sex = c(T, T, T,
                T, T, T,
                F, T, F),
    label = c('PrEP indications (prep.indications)\nindex, 2019 = 1',
              'PrEP users (prep.uptake)\nindex, 2019 = 1',
              'PrEP users / indicated (prep.uptake.proportion)\n%',
              'Testing rate (general.population.testing)\ntests per person per year',
              'Tested in past year (testing)\n% of adults',
              'Total HIV tests (total.hiv.tests)\nindex, 2019 = 1',
              'HIV tests per population (total.hiv.tests.per.population)\ntests per 100 people',
              'Viral suppression (suppression)\n% of diagnosed',
              'Aware of status (awareness)\n% of people with HIV'))

# PrEP groups. Each group = the model cells it adds up (NULL = all values of that dimension).
#   Model sex: 'msm', 'heterosexual_male', 'female'.  Male = MSM + heterosexual men.
#   Model race: 'black', 'hispanic', 'other'. The model has no separate White group:
#   'other' is mostly White, so it is labeled "White/other".
#   Model age: '13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'.
PREP.GROUPS = list(
    `All`             = list(),
    `Male`            = list(sex = c('msm', 'heterosexual_male')),
    `Female`          = list(sex = 'female'),
    `13-24 years`     = list(age = '13-24 years'),
    `25-34 years`     = list(age = '25-34 years'),
    `35-44 years`     = list(age = '35-44 years'),
    `45-54 years`     = list(age = '45-54 years'),
    `55+ years`       = list(age = '55+ years'),
    `MSM`             = list(sex = 'msm'),
    `Black`           = list(race = 'black'),
    `Hispanic`        = list(race = 'hispanic'),
    `White/other`     = list(race = 'other'),
    `Black MSM`       = list(sex = 'msm', race = 'black'),
    `Hispanic MSM`    = list(sex = 'msm', race = 'hispanic'),
    `White/other MSM` = list(sex = 'msm', race = 'other'))

# Groups with observed PrEP-to-need (numerator AND denominator in the data) -> figure 13.
# Value = how the group is named in observed_prep_to_need.csv (dimension / group).
PREP.COMPARE = c(`All` = 'Total / All', `Male` = 'Sex / Male', `Female` = 'Sex / Female',
                 `13-24 years` = 'Age / 13-24 years', `25-34 years` = 'Age / 25-34 years',
                 `35-44 years` = 'Age / 35-44 years', `45-54 years` = 'Age / 45-54 years')
# Groups without observed PrEP-to-need -> JHEEM trend only (figure 14)
PREP.MODEL.ONLY = c('MSM', 'Black', 'Hispanic', 'White/other',
                    'Black MSM', 'Hispanic MSM', 'White/other MSM')

# PrEP measures for each group (all computed within each simulation, from MODEL output)
#   1. PrEP-to-need, fixed 2017-2018 need = users that year / mean of the group's 2017 and 2018 indications
#   2. PrEP-to-need, need of same year    = users that year / indications that year
#   3. PrEP indications                    = indications that year (count)
#   4. PrEP indications, 2017-2018 mean    = the fixed denominator of measure 1 (count, same every year)
#   5. PrEP indications / 2017-2018 mean   = measure 3 / measure 4. Example: 1.3 = need 30% above 2017-2018.
#   Measures 1, 4 and 5 need model years 2017-2018 in the saved numbers; otherwise they are NA.
PREP.MEASURES = c('PrEP-to-need, fixed 2017-2018 need', 'PrEP-to-need, need of same year',
                  'PrEP indications', 'PrEP indications, 2017-2018 mean',
                  'PrEP indications / 2017-2018 mean')

# Palette (validated reference slots) and the "how to read" note, as in ehe26_benchmark_trends.R
COLS.EXPANSION = c(Expansion = '#2a78d6', `Non-expansion` = '#eb6834')
COLS.GROUP = c(All = '#2a78d6', MSM = '#eb6834')
how.to.read = function(x.axis, y.axis, read)
    paste0('X axis: ', x.axis, '\nY axis: ', y.axis, '\nHow to read:\n',
           paste0('  ', seq_along(read), '. ', read, collapse = '\n'))
READ.STYLE = theme(plot.subtitle = element_text(size = 9.5, colour = 'grey25', lineheight = 1.15,
                                                margin = margin(t = 2, b = 10)))

# 2. OBSERVED DATA (from ehe26_benchmark_trends.R) ----
read.observed = function(name)
{
    f = file.path(OUT.DIR, name)
    if (!file.exists(f))
        stop("Run ehe26_benchmark_trends.R first: ", f, " is missing")
    read.csv(f)
}
observed = read.observed('observed_headline_trends.csv')
observed.prep = read.observed('observed_prep_to_need.csv')
HEADLINE = unique(observed$headline)    # figure 11 panel names and order, as in figure 1

# 3. HELPERS: ADD UP CELLS AND SUMMARISE SIMULATIONS ----
# sum.cells(): add up the cells of one saved array for a group -> year x sim
#   sel = list(sex = ..., race = ..., age = ...); a dimension not named in sel is summed over all values.
#   Example: sel = list(sex = 'msm', race = 'black') adds up all ages for Black MSM.
sum.cells = function(arr, sel = list())
{
    dn = dimnames(arr)
    missing.dims = setdiff(names(sel), names(dn))
    if (length(missing.dims) > 0)
        stop("This outcome has no '", paste(missing.dims, collapse = "', '"), "' breakdown")
    idx = lapply(names(dn), function(d) if (is.null(sel[[d]])) dn[[d]] else sel[[d]])
    x = do.call(`[`, c(list(arr), idx, list(drop = F)))
    apply(x, c('year', 'sim'), sum)
}

# value.of(): one outcome for a group -> year x sim
#   counts: summed values. Proportions and rates: summed numerators / summed denominators.
value.of = function(outcome, sel = list())
{
    if (!is.null(outcome$value))
        sum.cells(outcome$value, sel)
    else
        sum.cells(outcome$numerator, sel) / sum.cells(outcome$denominator, sel)
}

# summarise.sims(): year x sim -> median and 95% interval across simulations, per year
summarise.sims = function(x)
    data.frame(year = as.numeric(rownames(x)),
               median = apply(x, 1, median, na.rm = T),
               lower = apply(x, 1, quantile, 0.025, na.rm = T),
               upper = apply(x, 1, quantile, 0.975, na.rm = T),
               row.names = NULL)

# 4. BUILD ALL MEASURES, ONE STATE AT A TIME (from the saved numbers) ----
build.state = function(file)
{
    raw = readRDS(file)
    state = raw$info$state
    o = raw$outcomes
    if (length(raw$problems) > 0)
        warning(state, ": step 1 could not read ", paste(names(raw$problems), collapse = ', '))

    users = o$prep.uptake
    need = o$prep.indications
    years = dimnames(users$value)$year
    has.2017.2018 = all(c('2017', '2018') %in% years)

    # PrEP-to-need for a group, both denominators (a: fixed 2017-2018, b: same year)
    prep.for = function(sel)
    {
        u = value.of(users, sel)
        n = value.of(need, sel)
        if (has.2017.2018)
        {
            n.fixed = colMeans(n[c('2017', '2018'), , drop = F])     # one value per simulation
            fixed = sweep(u, 2, n.fixed, '/')
            n.fixed.every.year = n * 0 + rep(n.fixed, each = nrow(n))
            n.vs.fixed = sweep(n, 2, n.fixed, '/')
        }
        else
            fixed = n.fixed.every.year = n.vs.fixed = n * NA
        list(fixed = fixed, same.year = u / n, need = n,
             need.fixed = n.fixed.every.year, need.vs.fixed = n.vs.fixed)
    }

    # 4a. Figure 11 measures (headline names must match figure 1)
    FIXED = 'JHEEM baseline'
    SAME.YEAR = 'JHEEM baseline, need of same year'
    prep.all = prep.for(list())
    prep.male = prep.for(PREP.GROUPS$Male)
    msm = list(sex = 'msm')
    m = function(headline, version, x) cbind(data.frame(state = state, headline = headline, version = version),
                                             summarise.sims(x))
    figure = bind_rows(
        m('PrEP-to-need, total', FIXED, prep.all$fixed),
        m('PrEP-to-need, total', SAME.YEAR, prep.all$same.year),
        m('PrEP-to-need, males', FIXED, prep.male$fixed),
        m('PrEP-to-need, males', SAME.YEAR, prep.male$same.year),
        m('Viral suppression, diagnosed total', FIXED, value.of(o$suppression)),
        m('Viral suppression, diagnosed MSM', FIXED, value.of(o$suppression, msm)),
        m('Awareness of status, all', FIXED, value.of(o$awareness)),
        m('Tested in past year, total(BRFSS)', FIXED, value.of(o$testing)),
        m('Tested in past year, MSM (BRFSS)', FIXED, value.of(o$testing, msm)))

    # 4b. Figure 12: raw outcomes, everyone and MSM
    outcome.value = function(name, sel)
        if (name == 'prep.uptake.proportion') value.of(users, sel) / value.of(need, sel) else value.of(o[[name]], sel)
    outcomes = bind_rows(lapply(seq_len(nrow(FIGURE12.OUTCOMES)), function(i) {
        name = FIGURE12.OUTCOMES$outcome[i]
        groups = if (FIGURE12.OUTCOMES$has.sex[i]) list(All = list(), MSM = msm) else list(All = list())
        bind_rows(lapply(names(groups), function(g)
            cbind(data.frame(state = state, outcome = name, group = g), summarise.sims(outcome.value(name, groups[[g]])))))
    }))

    # 4c. PrEP by group, 5 measures
    prep.groups = bind_rows(lapply(names(PREP.GROUPS), function(g) {
        x = prep.for(PREP.GROUPS[[g]])
        bind_rows(lapply(seq_along(PREP.MEASURES), function(i)
            cbind(data.frame(state = state, group = g, measure = PREP.MEASURES[i]), summarise.sims(x[[i]]))))
    }))

    list(figure = figure, outcomes = outcomes, prep.groups = prep.groups,
         info = data.frame(state = state, n.sim = raw$info$n.sim,
                           years = paste(raw$info$years.saved, collapse = '-'),
                           has.2017.2018 = has.2017.2018))
}

raw.files = list.files(RAW.DIR, pattern = '\\.rds$', full.names = T)
if (!is.null(STATES.TO.USE))
    raw.files = raw.files[sub('\\.rds$', '', basename(raw.files)) %in% STATES.TO.USE]
if (length(raw.files) == 0)
    stop("No saved model numbers in ", RAW.DIR, ". Run ehe26_jheem_extract.R first.")

t0 = Sys.time()
built = lapply(raw.files, build.state)
model = bind_rows(lapply(built, `[[`, 'figure'))
model.outcomes = bind_rows(lapply(built, `[[`, 'outcomes'))
model.prep = bind_rows(lapply(built, `[[`, 'prep.groups'))
model.info = bind_rows(lapply(built, `[[`, 'info'))
rm(built)
print(model.info)
print(paste0("Built measures for ", nrow(model.info), " states in ",
             round(as.numeric(Sys.time() - t0, units = 'secs')), " seconds"))

# 5. SAVE TABLES ----
write.csv(model, file.path(OUT.DIR, 'jheem_baseline_by_state.csv'), row.names = F)
write.csv(model.outcomes, file.path(OUT.DIR, 'jheem_outcomes_by_state.csv'), row.names = F)
write.csv(model.prep, file.path(OUT.DIR, 'jheem_prep_by_group.csv'), row.names = F)

missing.names = setdiff(unique(model$headline), HEADLINE)
if (length(missing.names) > 0)
    warning("Model measures with no matching figure 1 panel: ", paste(missing.names, collapse = '; '))

# Compare the same states: keep only states with saved model numbers
states.used = intersect(unique(model$state), unique(observed$state))
expansion.by.state = observed %>% distinct(state, expansion)
n.expansion = table(expansion.by.state$expansion[expansion.by.state$state %in% states.used])

# Last model year (end of the projection); used for the figures
PROJECTION.END.YEAR = max(model$year)
print(paste0("Model years: ", min(model$year), "-", PROJECTION.END.YEAR))

# Shared figure pieces: COVID band, projection band, 2019 line
time.bands = function()
    list(annotate('rect', xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'grey92'),
         annotate('rect', xmin = LAST.DATA.YEAR, xmax = PROJECTION.END.YEAR, ymin = -Inf, ymax = Inf,
                  fill = 'grey96'),
         geom_vline(xintercept = 2019, linetype = 'dashed', colour = 'grey40', linewidth = 0.4))

# 6. FIGURE 11: OBSERVED VS JHEEM BASELINE, BY MEDICAID EXPANSION AND ALL STATES ----
# Observed: median of the states' observed values each year.
# JHEEM: median of the states' model medians each year.
# PrEP panels have two model lines (version): fixed 2017-2018 need, need of same year.
observed.used = observed %>% filter(state %in% states.used)
model.used = model %>%
    filter(state %in% states.used, headline %in% HEADLINE) %>%
    left_join(expansion.by.state, by = 'state')

SOURCES = c('Observed', 'JHEEM baseline', 'JHEEM baseline, need of same year')
GROUPS = c('Expansion', 'Non-expansion', 'All states')    # 'All states' last, so it is drawn on top
# median.by.group(): median state in each Medicaid group, and across all states
#   (the rows are copied once with expansion = 'All states')
median.by.group = function(d, value.col)
    bind_rows(d, d %>% mutate(expansion = 'All states')) %>%
        group_by(headline, source, year, expansion) %>%
        summarise(value = median(.data[[value.col]], na.rm = T), n = n(), .groups = 'drop') %>%
        filter(n >= 3, !is.na(value))

lines = bind_rows(median.by.group(observed.used %>% mutate(source = 'Observed'), 'value'),
                  median.by.group(model.used %>% rename(source = version), 'median')) %>%
    mutate(headline = factor(headline, levels = HEADLINE),
           source = factor(source, levels = SOURCES),
           expansion = factor(expansion, levels = GROUPS))
observed.used$headline = factor(observed.used$headline, levels = HEADLINE)

fig11 = ggplot() +
    time.bands() +
    geom_line(data = observed.used, aes(year, 100 * value, group = state, colour = expansion),
              linewidth = 0.3, alpha = 0.3) +
    geom_line(data = lines, aes(year, 100 * value, colour = expansion, linetype = source),
              linewidth = 1.1) +
    facet_wrap(~ headline, scales = 'free_y', ncol = 3) +
    scale_colour_manual(values = c(COLS.EXPANSION, `All states` = 'grey10'), name = 'Group:',
                        labels = c(Expansion = paste0('Medicaid expansion (', n.expansion['Expansion'], ' states)'),
                                   `Non-expansion` = paste0('Non-expansion (', n.expansion['Non-expansion'], ' states)'),
                                   `All states` = paste0('All states (', length(states.used), ' states)'))) +
    scale_linetype_manual(values = c(Observed = 'solid', `JHEEM baseline` = 'dashed',
                                     `JHEEM baseline, need of same year` = 'dotted'),
                          labels = c(Observed = 'Observed',
                                     `JHEEM baseline` = 'JHEEM (PrEP-to-need: fixed 2017-2018 need)',
                                     `JHEEM baseline, need of same year` = 'JHEEM, PrEP-to-need with need of same year'),
                          name = 'Median state:') +
    scale_x_continuous(breaks = seq(2013, PROJECTION.END.YEAR, 2)) +
    guides(colour = guide_legend(order = 1, override.aes = list(alpha = 1, linewidth = 1.1)),
           linetype = guide_legend(order = 2, override.aes = list(linewidth = 0.9, colour = 'grey20'))) +
    labs(x = NULL, y = 'Percent',
         title = paste0('Observed trends and the JHEEM baseline projection to ', PROJECTION.END.YEAR),
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = 'the measure named in the panel title, in percent. Each panel has its own scale.',
             read = c(paste0('Thin lines = observed data for one state (only the ', length(states.used),
                             ' states with a calibrated JHEEM model).'),
                      paste0('Solid thick line = observed median state. Dashed thick line = JHEEM median state. ',
                             'Blue = expansion states, orange = non-expansion states, black = all states.'),
                      paste0('PrEP panels have a second JHEEM line (dotted): PrEP users / the model\'s PrEP need in the same year. ',
                             'Dashed = divided by the model\'s 2017-2018 need, like the observed line.'),
                      paste0('Where solid and dashed overlap, the model matches the data. After ', LAST.DATA.YEAR,
                             ' (light shading) the dashed line is the model\'s projection with no new interventions.'),
                      'Dashed vertical line = 2019 (EHE launch). Darker shading = 2020-2021 (COVID).')),
         caption = paste0('JHEEM baseline = "', SIMSET.FILE, '" simsets (', CALIBRATION.CODE,
                          '), no added interventions. Each state\'s value is its median across simulations.\n',
                          'PrEP-to-need (solid, dashed) = PrEP users / mean PrEP indications in 2017-2018 ',
                          '(CDC for the data, the model\'s own for JHEEM). Dotted = PrEP users / the model\'s indications in the same year.')) +
    theme_minimal(base_size = 11) +
    theme(legend.position = 'top', legend.justification = 'left', legend.box = 'vertical',
          legend.box.just = 'left', legend.key.width = unit(2.2, 'lines'),
          plot.caption = element_text(hjust = 0, colour = 'grey30', size = 9),
          panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0)) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig11_observed_vs_jheem_baseline.png'), fig11, width = 12, height = 12.4,
       dpi = 200, bg = 'white')

# 7. FIGURE 12: HOW EACH RAW JHEEM OUTCOME EVOLVES ----
# Counts are shown as an index: each state's value / its own value in INDEX.YEAR (2 = twice INDEX.YEAR),
# so large and small states can share one panel. Proportions in percent; the rate as is.
# Thin lines = one state (its median across simulations). Thick lines = median state.
#
# INDEX.YEAR
#   1. 2019 (EHE launch) when the saved model years include it ('baseline' simsets).
#   2. Otherwise the first saved year. Example: 'noint' simsets start in 2025, so the index is 2025 = 1.
#   Without this fallback the count panels are empty (value / missing 2019 value = NA).
INDEX.YEAR = if (2019 %in% model.outcomes$year) 2019 else min(model.outcomes$year)
if (INDEX.YEAR != 2019)
    print(paste0("Figure 12: no model year 2019 in the saved numbers; counts are indexed to ", INDEX.YEAR))
fig12.labels = sub('2019 = 1', paste0(INDEX.YEAR, ' = 1'), FIGURE12.OUTCOMES$label)

fig12.data = model.outcomes %>%
    left_join(FIGURE12.OUTCOMES %>% select(outcome, type, label), by = 'outcome') %>%
    group_by(state, outcome, group) %>%
    mutate(value = case_when(type == 'count' ~ median / median[year == INDEX.YEAR][1],
                             type == 'proportion' ~ 100 * median,
                             TRUE ~ median)) %>%
    ungroup() %>%
    mutate(label = factor(sub('2019 = 1', paste0(INDEX.YEAR, ' = 1'), label), levels = fig12.labels),
           group = factor(group, levels = names(COLS.GROUP)))

fig12.medians = fig12.data %>%
    group_by(label, group, year) %>%
    summarise(value = median(value, na.rm = T), .groups = 'drop')

fig12 = ggplot() +
    time.bands() +
    geom_line(data = fig12.data, aes(year, value, group = interaction(state, group), colour = group),
              linewidth = 0.3, alpha = 0.25) +
    geom_line(data = fig12.medians, aes(year, value, colour = group), linewidth = 1.1) +
    facet_wrap(~ label, scales = 'free_y', ncol = 3) +
    scale_colour_manual(values = COLS.GROUP, name = NULL, labels = c(All = 'Everyone', MSM = 'MSM')) +
    scale_x_continuous(breaks = seq(2013, PROJECTION.END.YEAR, 2)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, linewidth = 1.1))) +
    labs(x = NULL, y = NULL,
         title = 'What JHEEM projects for PrEP, testing and suppression with no new interventions',
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = paste0('the unit in each panel title: index (', INDEX.YEAR,
                             ' = 1) for counts, % for proportions, tests per person per year for the rate.'),
             read = c(paste0('Thin lines = one state (', length(unique(fig12.data$state)),
                             ' states, median across simulations). Thick lines = median state. Blue = everyone, orange = MSM.'),
                      paste0('After ', LAST.DATA.YEAR, ' (light shading) the lines are the model\'s own projection: ',
                             'the trend assumptions in the dictionary at the top of the code.'),
                      paste0('Index panels: 2 = twice the ', INDEX.YEAR, ' value. A flat line = no change.'),
                      'Dashed vertical line = 2019 (EHE launch). Darker shading = 2020-2021 (COVID).')),
         caption = paste0('JHEEM baseline = "', SIMSET.FILE, '" simsets (', CALIBRATION.CODE, '), no added interventions. ',
                          'Awareness and tests per population are kept for everyone only (no sex breakdown in the model).')) +
    theme_minimal(base_size = 11) +
    theme(legend.position = 'top', legend.justification = 'left', legend.key.width = unit(2.2, 'lines'),
          plot.caption = element_text(hjust = 0, colour = 'grey30', size = 9),
          panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0, lineheight = 1.05)) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig12_jheem_outcome_trends.png'), fig12, width = 12, height = 12.5,
       dpi = 200, bg = 'white')

# 8. FIGURES 13-15: PrEP BY GROUP ----
# Median state each year (line) and the middle half of states (band, 25th-75th percentile).
prep.summary = model.prep %>%
    filter(!is.na(median), state %in% states.used) %>%
    group_by(group, measure, year) %>%
    summarise(mid = median(median), p25 = quantile(median, 0.25), p75 = quantile(median, 0.75),
              n = n(), .groups = 'drop') %>%
    filter(n >= 3)

prep.theme = function()
    list(scale_x_continuous(breaks = seq(2015, PROJECTION.END.YEAR, 5)),
         theme_minimal(base_size = 11),
         theme(legend.position = 'top', legend.justification = 'left', legend.key.width = unit(2.2, 'lines'),
               plot.caption = element_text(hjust = 0, colour = 'grey30', size = 9),
               axis.title.y = element_text(size = 10),
               panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0)),
         READ.STYLE)

# 8a. Figure 13: PrEP-to-need, observed vs JHEEM, where the data allow it (total, male, female, age)
#     Observed group names: dimension / group in observed_prep_to_need.csv
observed.prep.lines = observed.prep %>%
    filter(state %in% states.used) %>%
    mutate(key = paste0(dimension, ' / ', group),
           group = names(PREP.COMPARE)[match(key, PREP.COMPARE)]) %>%
    filter(!is.na(group)) %>%
    group_by(group, year) %>%
    summarise(value = median(value, na.rm = T), n = n(), .groups = 'drop') %>%
    filter(n >= 3) %>%
    mutate(source = 'Observed (CDC 2017-2018 need)')

model.prep.lines = prep.summary %>%
    filter(group %in% names(PREP.COMPARE),
           measure %in% c('PrEP-to-need, fixed 2017-2018 need', 'PrEP-to-need, need of same year')) %>%
    transmute(group, year, value = mid,
              source = ifelse(measure == 'PrEP-to-need, fixed 2017-2018 need',
                              'JHEEM, fixed 2017-2018 need', 'JHEEM, need of same year'))

COLS.SOURCE = c(`Observed (CDC 2017-2018 need)` = 'grey20',
                `JHEEM, fixed 2017-2018 need` = '#2a78d6',
                `JHEEM, need of same year` = '#eb6834')
fig13.data = bind_rows(observed.prep.lines, model.prep.lines) %>%
    mutate(group = factor(group, levels = names(PREP.COMPARE)),
           source = factor(source, levels = names(COLS.SOURCE)))

fig13 = ggplot(fig13.data, aes(year, 100 * value, colour = source, linetype = source)) +
    time.bands() +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ group, ncol = 4) +
    scale_colour_manual(values = COLS.SOURCE, name = 'Median state:') +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted'), name = 'Median state:') +
    labs(x = NULL, y = 'PrEP users / people with a PrEP indication (%)',
         title = 'PrEP-to-need where the data allow a direct comparison: observed vs JHEEM',
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = 'PrEP users as a percent of people with an indication for PrEP.',
             read = c('Black solid = observed: PrEP users (AIDSVu) / CDC PrEP need in 2017-2018 (the data have need only for 2017-2018).',
                      'Blue dashed = JHEEM with the same definition: model PrEP users / the model\'s 2017-2018 need.',
                      'Orange dotted = JHEEM with the need of the same year. If orange falls below blue, the model\'s need grew.',
                      paste0('Lines = median state (', length(states.used), ' states with a JHEEM model). After ',
                             LAST.DATA.YEAR, ' (light shading) = model projection.'))),
         caption = 'Only groups with observed PrEP users AND PrEP need are shown. Observed need for age 55+ does not exist. Male = MSM + heterosexual men.') +
    prep.theme()

ggsave(file.path(OUT.DIR, 'fig13_prep_to_need_observed_vs_jheem.png'), fig13, width = 13, height = 8.5,
       dpi = 200, bg = 'white')

# 8b. Figure 14: PrEP-to-need for MSM, race, MSM x race: JHEEM only (no observed data)
COLS.DENOMINATOR = c(`PrEP-to-need, fixed 2017-2018 need` = '#2a78d6',
                     `PrEP-to-need, need of same year` = '#eb6834')
fig14.data = prep.summary %>%
    filter(group %in% PREP.MODEL.ONLY, measure %in% names(COLS.DENOMINATOR)) %>%
    mutate(group = factor(group, levels = PREP.MODEL.ONLY))

fig14 = ggplot(fig14.data) +
    time.bands() +
    geom_ribbon(aes(year, ymin = 100 * p25, ymax = 100 * p75, fill = measure), alpha = 0.12) +
    geom_line(aes(year, 100 * mid, colour = measure), linewidth = 1.1) +
    facet_wrap(~ group, ncol = 4) +
    scale_colour_manual(values = COLS.DENOMINATOR, name = NULL,
                        labels = c('Divided by the 2017-2018 need (fixed)', 'Divided by the need of the same year')) +
    scale_fill_manual(values = COLS.DENOMINATOR, guide = 'none') +
    labs(x = NULL, y = 'PrEP users / people with a PrEP indication (%)',
         title = 'JHEEM baseline only: PrEP-to-need for MSM, by race, and MSM by race',
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = 'PrEP users as a percent of people with an indication for PrEP (model output).',
             read = c('There are no observed data to compare: the observed PrEP need has no race or MSM breakdown.',
                      'Blue = divided by the group\'s 2017-2018 need (fixed). Orange = divided by the need of the same year.',
                      'If the lines split apart, the model\'s need has changed since 2017-2018: orange below blue = need grew.',
                      paste0('Line = median state; band = middle half of states. After ', LAST.DATA.YEAR,
                             ' (light shading) = model projection.'))),
         caption = 'White/other = the model\'s "other" race group (mostly White). Blue is missing if the saved numbers have no model years 2017-2018.') +
    prep.theme()

ggsave(file.path(OUT.DIR, 'fig14_prep_to_need_jheem_only.png'), fig14, width = 13, height = 8.5,
       dpi = 200, bg = 'white')

# 8c. Figure 15: the model's PrEP need each year compared with its 2017-2018 level, every group
fig15.data = prep.summary %>%
    filter(measure == 'PrEP indications / 2017-2018 mean') %>%
    mutate(group = factor(group, levels = names(PREP.GROUPS)))

fig15 = ggplot(fig15.data) +
    time.bands() +
    geom_hline(yintercept = 1, colour = 'grey30', linewidth = 0.5) +
    geom_ribbon(aes(year, ymin = p25, ymax = p75), fill = '#2a78d6', alpha = 0.15) +
    geom_line(aes(year, mid), colour = '#2a78d6', linewidth = 1.1) +
    facet_wrap(~ group, ncol = 5) +
    labs(x = NULL, y = 'PrEP indications / 2017-2018 mean',
         title = 'JHEEM baseline: how the model\'s PrEP need changes compared with 2017-2018',
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = 'the group\'s PrEP indications that year / its average in 2017-2018 (model output).',
             read = c('1 (gray line) = same need as in 2017-2018. 1.3 = 30% more people with an indication; 0.8 = 20% fewer.',
                      'The observed PrEP-to-need assumes this line stays at 1 (CDC only estimated need for 2017-2018).',
                      paste0('Line = median state; band = middle half of states. After ', LAST.DATA.YEAR,
                             ' (light shading) = model projection.'))),
         caption = paste0('Model PrEP need is calibrated to CDC estimates for 2017-2018 only (loosely, CV 0.5); ',
                          'other years follow the model\'s prep.indication trend (see the dictionary at the top).')) +
    prep.theme()

ggsave(file.path(OUT.DIR, 'fig15_prep_need_vs_2017_2018.png'), fig15, width = 13, height = 9.5,
       dpi = 200, bg = 'white')

print(paste0("Saved tables and figures 11-15 to ", OUT.DIR))
