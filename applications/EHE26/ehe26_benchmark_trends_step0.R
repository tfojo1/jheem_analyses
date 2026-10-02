# EHE26: how much did PrEP, testing and viral suppression improve across states since EHE began?
#
# Purpose
#   Characterize state-level changes from EHE launch (2019) to the latest data year,
#   overall and by sex, race and age. These changes become the benchmarks for the
#   2026-2030 scale-up scenarios. The by-group results tell us whether the benchmarks
#   should differ by sex, race or age.
#
# What it makes (in OUT.DIR)
#   Tables
#     1. benchmark_state_changes.csv   one row per measure x dimension x group x state
#     2. benchmark_summary.csv         median / 75th / 90th / max across states
#     3. group_differences.csv         do groups differ? (paired, within state)
#   Figures: headline measures (overall and MSM)
#     4. fig1_trends.png               each state over time, by Medicaid expansion status,
#                                      with group medians and the all-state median and 90th percentile
#     5. fig2_change_by_state.png      states ranked by how much of the gap they closed
#     6. fig3_start_vs_change.png      starting level vs gap closed (is there a ceiling effect?),
#                                      with the Pearson correlation in each panel
#        fig3_correlations.csv         Pearson r (95% CI, p) and Spearman rho for each panel
#   Figures: by group
#     7. fig4_trends_by_sex.png, fig5_trends_by_race.png, fig6_trends_by_age.png
#        median state per group over time (band = 25th-75th percentile of states)
#     8. fig7_change_by_sex.png, fig8_change_by_race.png, fig9_change_by_age.png
#        change since 2019 in each state, per group. The panel title shows the
#        p-value for "do the groups differ?" (Friedman test, states as blocks).
#   2030 targets
#     9. targets_2030_by_state.csv     projected 2026 level and 2030 targets for each state (proportions only)
#    10. fig10_targets_2030.png        projected 2026 level vs 2030 targets, headline measures
#
# Which data exist by group (state level)
#                              Total  Sex  Race  Age
#   PrEP-to-need                 x     x    -     x    (no PrEP indications by race, or for age 55+)
#   PrEP users, rel. change      x     x    x     x
#   Viral suppression            x     x    x     x
#   Viral suppression, MSM       x     -    x     -
#   Awareness of status          x     -    -     -
#   Tested in past year (BRFSS)  x     x    x     -    (all adults; MSM only as a total)
#
# How we measure change (from 2019 to the latest year)
#   Every change column says what it is and its unit:
#     .points = percentage points, .pct = percent. All are stored in percent.
#   proj. = a projected value, i.e. for a year after the last data year
#     (proj.gap.2026, proj.level.2026.pct, proj.2030.pct, proj.target.2030.*).
#     Columns without proj. come from observed data.
#
#   A. Proportions (PrEP-to-need, suppression, awareness, testing)
#      Example: suppression goes from 70% to 76%.
#      1. absolute.change.points = end - start                = 76 - 70       = +6 points
#      2. relative.change.pct    = (end - start) / start      = 6 / 70        = +8.6%
#      3. gap.closed.pct         = relative fall in the gap (gap = 1 - p)
#                                = 1 - (1 - end) / (1 - start) = 1 - 24 / 30  = 20%
#         Gap closed is the relative change of the gap, not of the level.
#      4. annual.gap.closed.pct  = 1 - ((1 - end) / (1 - start))^(1 / years)
#      5. implied.gap.closed.2026.2030.pct = 1 - (1 - annual)^4
#         The same pace, over the 4 years from 2026 to 2030.
#         Percentiles of this across states are the scenario benchmarks.
#      6. proj.level.2026.pct = projected level in 2026 (see PROJECT.TO.2026 in settings)
#      7. proj.2030.pct = projected level in 2030 if the state kept its own pace:
#         1 - gap 2026 x (1 - implied.gap.closed.2026.2030)
#         Example: 70% in 2026 (gap 30%), own implied gap closed 20%
#                  -> gap 2030 = 30% x 0.8 = 24% -> projected 2030 = 76%.
#
#   B. PrEP users (counts; shown as an index with 2019 = 1)
#      Example: 1,000 users in 2019 and 2,500 in 2024 -> index 2.5.
#      1. absolute.change.points: not used (counts, not proportions)
#      2. relative.change.pct        = end / start - 1          = +150%
#      3. annual.relative.change.pct = (end / start)^(1 / years) - 1
#
# Race groups: Black, Hispanic, White in every source, so they can be compared.
#   (The model's third group is "other", which is mostly White.)
#
# Working directory is assumed to be jheem_analyses/
# Data only. Nothing here runs or changes simulations.

source('../jheem_analyses/source_code.R')
get.jheem.root.directory()
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)

# 1. SETTINGS ----
START.YEAR = 2019                       # EHE launch
PLOT.YEARS = 2013:2024
STATES = c(state.abb, 'DC')             # 50 states + DC (no PR, no US total)

# Projection to 2026 and 2030
BASELINE.YEAR = 2026                    # interventions start
TARGET.YEAR = 2030                      # targets reached
# Level in 2026 (data end in 2022-2024):
#   'flat'      = latest observed level carried forward unchanged to 2026
#   'own.trend' = the state keeps closing its gap at its own 2019-latest pace until 2026
PROJECT.TO.2026 = 'flat'

if (!exists('OUT.DIR'))
    OUT.DIR = file.path(get.jheem.root.directory(), 'results', 'ehe26', 'benchmarks')
if (!dir.exists(OUT.DIR))
    dir.create(OUT.DIR, recursive = T)

# Reference palette (validated slots, in fixed order)
COL.MEDIAN = '#2a78d6'   # slot 1, blue
COL.P90 = '#eb6834'      # slot 2, orange
COL.STATE = 'grey75'
SLOTS = c('#2a78d6', '#eb6834', '#1baf7a', '#eda100', '#e87ba4', '#008300')
COLS.SEX = c(Female = SLOTS[1], Male = SLOTS[2])
COLS.RACE = c(Black = SLOTS[1], Hispanic = SLOTS[2], White = SLOTS[3])
AGES = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years')
COLS.AGE = setNames(SLOTS, AGES)

# "How to read" note shown under each figure title:
#   X axis, Y axis, then numbered reading tips.
how.to.read = function(x.axis, y.axis, read)
    paste0('X axis: ', x.axis, '\nY axis: ', y.axis, '\nHow to read:\n',
           paste0('  ', seq_along(read), '. ', read, collapse = '\n'))
READ.STYLE = theme(plot.subtitle = element_text(size = 9.5, colour = 'grey25', lineheight = 1.15,
                                                margin = margin(t = 2, b = 10)))

# Reference group for the paired comparisons
REFERENCE = c(Sex = 'Male', Race = 'White', Age = '25-34 years')

# Medicaid expansion (ACA): year expansion took effect. NA = not expanded (as of 2025).
#   Source: KFF, Status of State Medicaid Expansion Decisions. Check against KFF before publishing.
#   7 states expanded during our 2019-2024 window: ID, UT (2020), NE (2020), OK, MO (2021),
#   SD, NC (2023). Maine and Virginia took effect in early 2019.
MEDICAID.EXPANSION.YEAR = c(
    AK = 2015, AZ = 2014, AR = 2014, CA = 2014, CO = 2014, CT = 2014, DE = 2014, DC = 2014,
    HI = 2014, ID = 2020, IL = 2014, IN = 2015, IA = 2014, KY = 2014, LA = 2016, ME = 2019,
    MD = 2014, MA = 2014, MI = 2014, MN = 2014, MO = 2021, MT = 2016, NE = 2020, NV = 2014,
    NH = 2014, NJ = 2014, NM = 2014, NY = 2014, NC = 2023, ND = 2014, OH = 2014, OK = 2021,
    OR = 2014, PA = 2015, RI = 2014, SD = 2023, UT = 2020, VT = 2014, VA = 2019, WA = 2014,
    WV = 2014,
    AL = NA, FL = NA, GA = NA, KS = NA, MS = NA, SC = NA, TN = NA, TX = NA, WI = NA, WY = NA)

# A state counts as "Expansion" if it had expanded by this year.
#   2024 = current status (41 expansion incl. DC, 10 non-expansion).
#   2019 = status at EHE launch (the 7 late expanders count as non-expansion).
EXPANSION.AS.OF = 2024
expansion.status = function(state)
{
    year = MEDICAID.EXPANSION.YEAR[state]
    ifelse(!is.na(year) & year <= EXPANSION.AS.OF, 'Expansion', 'Non-expansion')
}
COLS.EXPANSION = c(Expansion = SLOTS[1], `Non-expansion` = SLOTS[2])

# 2. PULL THE DATA ----
# We read the stored arrays directly so we know exactly which source is used.
# Dimensions are always year x location (x one group dimension).
SM = SURVEILLANCE.MANAGER
get.data = function(outcome, source, ontology, stratification)
    SM$data[[outcome]]$estimate[[source]][[ontology]][[stratification]]

# Turn an array into a long data frame
#   arr:     year x location, or year x location x group
#   labels:  named vector, raw group name -> display name. Groups not listed are dropped.
#   type:    'proportion' or 'index'
to.long = function(arr, measure, dimension, type = 'proportion', labels = NULL)
{
    states = intersect(STATES, dimnames(arr)$location)
    if (length(dim(arr)) == 2)
    {
        arr = arr[, states, drop = F]
        years = rownames(arr)
        dim(arr) = c(dim(arr), 1)
        dimnames(arr) = list(year = years, location = states, group = 'All')
        labels = c(All = 'All')
    }
    else
        arr = arr[, states, names(labels), drop = F]

    data.frame(measure = measure,
               dimension = dimension,
               type = type,
               group = rep(labels[dimnames(arr)[[3]]], each = dim(arr)[1] * dim(arr)[2]),
               state = rep(rep(states, each = dim(arr)[1]), dim(arr)[3]),
               year = rep(as.numeric(dimnames(arr)[[1]]), dim(arr)[2] * dim(arr)[3]),
               value = as.numeric(arr),
               row.names = NULL)
}

# Divide each state (and group) by its own 2019 value -> index with 2019 = 1
index.to.2019 = function(arr)
{
    base = arr[as.character(START.YEAR), , , drop = F]
    arr / base[rep(1, dim(arr)[1]), , , drop = F]
}

SEX.LABELS = c(female = 'Female', male = 'Male')
AGE.LABELS = setNames(AGES, AGES)

# 2a. PrEP users (AIDSVu) ----
#     AIDSVu, not CDC:
#       1. CDC and AIDSVu agree within ~3% for 2017-2022 (sum over states).
#       2. CDC 2023 drops to 379k from 436k in 2022; AIDSVu keeps rising (505k in 2023).
#          The CDC 2023 value looks like an artifact.
#       3. AIDSVu runs to 2024 and has race.
prep.total = get.data('prep', 'aidsvu', 'aidsvu', 'year__location')
prep.sex = get.data('prep', 'aidsvu', 'aidsvu', 'year__location__sex')
prep.race = get.data('prep', 'aidsvu', 'aidsvu', 'year__location__race')
prep.age = get.data('prep', 'aidsvu', 'aidsvu', 'year__location__age')

# 2b. PrEP indications (CDC; only 2017-2018, no race)
#     need = mean of 2017 and 2018, fixed for all years
ind.total = get.data('prep.indications', 'cdc.prep.indications', 'cdc', 'year__location')
ind.sex = get.data('prep.indications', 'cdc.prep.indications', 'cdc', 'year__location__sex')
ind.age = get.data('prep.indications', 'cdc.prep.indications', 'cdc', 'year__location__age')

# PrEP-to-need = users / fixed need. Works for 2-D (total) and 3-D (by group) arrays.
prep.to.need = function(users, indications)
{
    st = intersect(dimnames(users)$location, dimnames(indications)$location)
    if (length(dim(users)) == 2)
    {
        need = colMeans(indications[c('2017', '2018'), st, drop = F])
        return(sweep(users[, st, drop = F], 2, need, '/'))
    }
    grp = intersect(dimnames(users)[[3]], dimnames(indications)[[3]])
    need = apply(indications[c('2017', '2018'), st, grp, drop = F], c(2, 3), mean)
    sweep(users[, st, grp, drop = F], c(2, 3), need, '/')
}

# Total PrEP users as a 3-D array (one group) so index.to.2019 works on it
as.one.group = function(mat)
{
    arr = mat
    dim(arr) = c(dim(mat), 1)
    dimnames(arr) = c(dimnames(mat), list(group = 'All'))
    arr
}

# 2c. Viral suppression among diagnosed (CDC) ----
supp.total = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location')
supp.sex = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location__sex')
supp.race = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location__race')
supp.age = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location__age')
supp.sex.risk = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location__sex__risk')
supp.race.risk = get.data('suppression', 'cdc.hiv', 'cdc', 'year__location__race__risk')
SUPP.RACE.LABELS = c(`black/african american` = 'Black', `hispanic/latino` = 'Hispanic', white = 'White')

# 2d. Awareness of HIV status (total only) ----
awareness = get.data('awareness', 'cdc.hiv', 'cdc', 'year__location')

# 2e. Tested for HIV in the past year (BRFSS; small samples) ----
test.total = get.data('proportion.tested', 'brfss', 'brfss', 'year__location')
test.sex = get.data('proportion.tested', 'brfss', 'brfss', 'year__location__sex')
test.race = get.data('proportion.tested', 'brfss', 'brfss', 'year__location__race')
test.risk = get.data('proportion.tested', 'brfss', 'brfss', 'year__location__risk')
BRFSS.RACE.LABELS = c(black = 'Black', hispanic = 'Hispanic', white = 'White')

# 2f. Stack everything into one long table ----
M.PREP = 'PrEP-to-need'
M.PREP.USERS = 'PrEP users (2019 = 1)'
M.SUPP = 'Viral suppression, diagnosed'
M.SUPP.MSM = 'Viral suppression, diagnosed MSM'
M.AWARE = 'Awareness of status'
M.TEST = 'Tested in past year, adults (BRFSS)'
M.TEST.MSM = 'Tested in past year, MSM (BRFSS)'

df = bind_rows(
    # PrEP-to-need: total, sex, age 
    to.long(prep.to.need(prep.total, ind.total), M.PREP, 'Total'),
    to.long(prep.to.need(prep.sex, ind.sex), M.PREP, 'Sex', labels = SEX.LABELS),
    to.long(prep.to.need(prep.age, ind.age), M.PREP, 'Age', labels = AGE.LABELS),
    #
    # PrEP users, indexed to 2019: total, sex, race, age
    to.long(index.to.2019(as.one.group(prep.total))[, , 1], M.PREP.USERS, 'Total', type = 'index'),
    to.long(index.to.2019(prep.sex), M.PREP.USERS, 'Sex', type = 'index', labels = SEX.LABELS),
    to.long(index.to.2019(prep.race), M.PREP.USERS, 'Race', type = 'index',
            labels = c(black = 'Black', hispanic = 'Hispanic', white = 'White')),
    to.long(index.to.2019(prep.age), M.PREP.USERS, 'Age', type = 'index', labels = AGE.LABELS),
    #
    # Viral suppression: total, sex, race, age
    to.long(supp.total, M.SUPP, 'Total'),
    to.long(supp.sex, M.SUPP, 'Sex', labels = SEX.LABELS),
    to.long(supp.race, M.SUPP, 'Race', labels = SUPP.RACE.LABELS),
    to.long(supp.age, M.SUPP, 'Age', labels = AGE.LABELS),
    # Viral suppression among MSM: total (sex = male, risk = msm), race
    to.long(supp.sex.risk[, , 'male', 'msm'], M.SUPP.MSM, 'Total'),
    to.long(supp.race.risk[, , , 'msm'], M.SUPP.MSM, 'Race', labels = SUPP.RACE.LABELS),
    #
    # Awareness: total
    to.long(awareness, M.AWARE, 'Total'),
    # Testing, adults: total, sex, race. Testing, MSM: total
    to.long(test.total, M.TEST, 'Total'),
    to.long(test.sex, M.TEST, 'Sex', labels = SEX.LABELS),
    to.long(test.race, M.TEST, 'Race', labels = BRFSS.RACE.LABELS),
    to.long(test.risk[, , 'msm'], M.TEST.MSM, 'Total')) %>%
    filter(!is.na(value), is.finite(value), year %in% PLOT.YEARS) %>%
    mutate(measure = factor(measure, levels = c(M.PREP, M.PREP.USERS, M.SUPP, M.SUPP.MSM,
                                                M.AWARE, M.TEST, M.TEST.MSM)),
           dimension = factor(dimension, levels = c('Total', 'Sex', 'Race', 'Age')))

# 3. CHANGE FROM 2019 TO THE LATEST YEAR, BY STATE AND GROUP ----
# The end year is the latest year with data for that measure and dimension
END.YEARS = df %>% group_by(measure, dimension) %>% summarise(end.year = max(year), .groups = 'drop')
print(as.data.frame(END.YEARS))

changes = df %>%
    left_join(END.YEARS, by = c('measure', 'dimension')) %>%
    group_by(measure, dimension, group, type, state) %>%
    summarise(start = value[year == START.YEAR][1],
              end = value[year == end.year[1]][1],
              end.year = end.year[1],
              .groups = 'drop') %>%
    filter(!is.na(start), !is.na(end)) %>%
    mutate(years = end.year - START.YEAR,
           # PrEP-to-need can pass 100%: users grow, but need is fixed at 2017-2018.
           # Then there is no gap left, so gap closed is not defined. We flag these
           # states and leave their gap-closed values empty (NA).
           above.100 = type == 'proportion' & (start >= 1 | end >= 1),
           has.gap = type == 'proportion' & !above.100,
           # Absolute change, in percentage points (proportions only)
           absolute.change.points = ifelse(type == 'proportion', 100 * (end - start), NA),
           # Relative change of the level, in percent (proportions and PrEP users)
           relative.change.pct = 100 * (end / start - 1),
           annual.relative.change.pct = 100 * ((end / start)^(1 / years) - 1),
           # Relative change of the gap = gap closed, in percent (proportions only)
           gap.closed.pct = ifelse(has.gap, 100 * (1 - (1 - end) / (1 - start)), NA),
           annual.gap.closed.pct = ifelse(has.gap, 100 * (1 - ((1 - end) / (1 - start))^(1 / years)), NA),
           implied.gap.closed.2026.2030.pct = ifelse(has.gap, 100 * (1 - (1 - annual.gap.closed.pct / 100)^4), NA),
           # Projected level in 2026 (proportions only)
           # (plain if/else: ifelse() with a single TRUE/FALSE would return only one value)
           years.to.2026 = BASELINE.YEAR - end.year,
           proj.gap.2026 = if (PROJECT.TO.2026 == 'own.trend')
                          (1 - end) * (1 - annual.gap.closed.pct / 100)^years.to.2026
                      else
                          1 - end,
           proj.gap.2026 = ifelse(has.gap & proj.gap.2026 > 0, proj.gap.2026, NA),
           proj.level.2026.pct = 100 * (1 - proj.gap.2026),
           # Projected 2030: the state keeps its own pace from 2026 to 2030
           proj.2030.pct = 100 * (1 - proj.gap.2026 * (1 - implied.gap.closed.2026.2030.pct / 100)))

write.csv(changes, file.path(OUT.DIR, 'benchmark_state_changes.csv'), row.names = F)

# 4. SUMMARY ACROSS STATES ----
# For each column: median, 75th and 90th percentile across states.
# Column names say which column, e.g. absolute.change.points.median = median absolute change.
# States flagged above.100 are counted but left out of the gap-closed percentiles.
# best.state.* = the state with the largest value (NA when the column does not apply).
state.with.max = function(state, x) if (all(is.na(x))) NA else state[which.max(x)]

summary.table = changes %>%
    group_by(measure, dimension, group, type) %>%
    summarise(n.states = n(),
              n.states.above.100 = sum(above.100),
              start.pct.median = ifelse(type[1] == 'proportion', 100 * median(start), NA),
              across(c(absolute.change.points, relative.change.pct, gap.closed.pct,
                       implied.gap.closed.2026.2030.pct, proj.level.2026.pct, proj.2030.pct),
                     list(median = ~ median(.x, na.rm = T),
                          p75 = ~ unname(quantile(.x, 0.75, na.rm = T)),
                          p90 = ~ unname(quantile(.x, 0.90, na.rm = T))),
                     .names = '{.col}.{.fn}'),
              best.state.gap.closed = state.with.max(state, gap.closed.pct),
              best.state.relative.change = state.with.max(state, relative.change.pct),
              .groups = 'drop')

write.csv(summary.table, file.path(OUT.DIR, 'benchmark_summary.csv'), row.names = F)

# 5. DO GROUPS DIFFER? (PAIRED WITHIN STATE) ----
# Each state is compared with itself, so differences between states do not get in the way.
#   1. Friedman test: do the groups differ at all? (one p-value per measure x dimension)
#      Uses states that have every group.
#   2. Each group vs the reference group (Male, White, 25-34 years):
#      median within-state difference, share of states where the group is higher,
#      and a Wilcoxon signed-rank p-value.
#   What is compared (column compared.metric says which):
#      gap.closed.pct for proportions,
#      relative.change.pct for PrEP users (counts have no gap).
#   Example: suppression gap closed, Black vs White. A state where Black closed 12%
#   of the gap and White closed 5% gives a difference of +7 percentage points.
#   We take the median over states.
# The value compared in sections 5 and 8
add.compared = function(d)
    d %>% mutate(compared.metric = ifelse(type == 'proportion', 'gap.closed.pct', 'relative.change.pct'),
                 compared.pct = ifelse(type == 'proportion', gap.closed.pct, relative.change.pct))

friedman = changes %>%
    add.compared() %>%
    filter(dimension != 'Total', !is.na(compared.pct)) %>%
    group_by(measure, dimension) %>%
    group_modify(function(d, key) {
        wide = d %>% select(state, group, compared.pct) %>%
            pivot_wider(names_from = group, values_from = compared.pct) %>% na.omit()
        p = if (nrow(wide) >= 5 && ncol(wide) >= 3)
                friedman.test(as.matrix(wide[, -1]))$p.value else NA
        data.frame(n.states.complete = nrow(wide), friedman.p = p)
    }) %>%
    ungroup()

paired = changes %>%
    add.compared() %>%
    filter(dimension != 'Total', !is.na(compared.pct)) %>%
    mutate(reference = REFERENCE[as.character(dimension)]) %>%
    group_by(measure, dimension) %>%
    group_modify(function(d, key) {
        ref = d$reference[1]
        ref.values = d %>% filter(group == ref) %>% select(state, ref.compared.pct = compared.pct)
        d %>% filter(group != ref) %>%
            inner_join(ref.values, by = 'state') %>%
            group_by(group) %>%
            summarise(reference = ref,
                      n.states = n(),
                      compared.metric = compared.metric[1],
                      median.group.pct = median(compared.pct),
                      median.reference.pct = median(ref.compared.pct),
                      median.difference.points = median(compared.pct - ref.compared.pct),
                      share.states.higher = mean(compared.pct > ref.compared.pct),
                      wilcoxon.p = if (n() >= 5) suppressWarnings(
                          wilcox.test(compared.pct, ref.compared.pct, paired = T)$p.value) else NA,
                      .groups = 'drop')
    }) %>%
    ungroup() %>%
    left_join(friedman, by = c('measure', 'dimension'))

write.csv(paired, file.path(OUT.DIR, 'group_differences.csv'), row.names = F)
print(as.data.frame(paired %>% mutate(across(where(is.numeric), ~ signif(., 3)))))

# 6. HEADLINE FIGURES (OVERALL AND MSM) ----
# Same five measures as before: total PrEP, male PrEP (data have no MSM breakdown for PrEP), MSM suppression,
# awareness, MSM testing.
HEADLINE = c('PrEP-to-need, total', 'PrEP-to-need, males',
             'Viral suppression, diagnosed total','Viral suppression, diagnosed MSM', 
             'Awareness of status, all',
             'Tested in past year, total(BRFSS)','Tested in past year, MSM (BRFSS)')

# M.PREP = 'PrEP-to-need'
# M.PREP.USERS = 'PrEP users (2019 = 1)'
# M.SUPP = 'Viral suppression, diagnosed'
# M.SUPP.MSM = 'Viral suppression, diagnosed MSM'
# M.AWARE = 'Awareness of status'
# M.TEST = 'Tested in past year, adults (BRFSS)'
# M.TEST.MSM = 'Tested in past year, MSM (BRFSS)'

pick.headline = function(d)
{
    d %>%
        mutate(headline = case_when(
            measure == M.PREP & dimension == 'Total' ~ HEADLINE[1],
            measure == M.PREP & dimension == 'Sex' & group == 'Male' ~ HEADLINE[2],
            measure == M.SUPP & dimension == 'Total' ~ HEADLINE[3],
            measure == M.SUPP.MSM & dimension == 'Total' ~ HEADLINE[4],
            measure == M.AWARE & dimension == 'Total' ~ HEADLINE[5],
            measure == M.TEST & dimension == 'Total' ~ HEADLINE[6],
            measure == M.TEST.MSM & dimension == 'Total' ~ HEADLINE[7]
            )) %>%
        filter(!is.na(headline)) %>%
        mutate(headline = factor(headline, levels = HEADLINE))
}
df.head = pick.headline(df)
changes.head = pick.headline(changes) %>% filter(!is.na(gap.closed.pct))

# 6a. Figure 1: trends over time, by Medicaid expansion status
#     One panel per measure.
#     Thin lines = one state: light blue = expansion, light orange = non-expansion.
#     Thick colored lines = median state each year, within each group.
#     Dark gray lines = all states: median (solid) and 90th percentile (dashed).
#     Expansion status as of EXPANSION.AS.OF (see settings).
df.head = df.head %>% mutate(expansion = expansion.status(state))

# Saved for ehe26_jheem_baseline_trends.R
#   observed_headline_trends.csv: figure 11 (observed vs JHEEM baseline)
#   observed_prep_to_need.csv:    PrEP-to-need by total, sex and age (figure 13: observed vs JHEEM)
write.csv(df.head, file.path(OUT.DIR, 'observed_headline_trends.csv'), row.names = F)
write.csv(df %>% filter(measure == M.PREP) %>% mutate(expansion = expansion.status(state)),
          file.path(OUT.DIR, 'observed_prep_to_need.csv'), row.names = F)

bands = df.head %>%
    group_by(headline, year) %>%
    summarise(Median = median(value), `90th percentile` = quantile(value, 0.9), .groups = 'drop') %>%
    pivot_longer(c(Median, `90th percentile`), names_to = 'line', values_to = 'value') %>%
    mutate(line = factor(line, levels = c('Median', '90th percentile')))

# Median by expansion group (only years with at least 3 states in the group)
bands.expansion = df.head %>%
    group_by(headline, year, expansion) %>%
    summarise(value = median(value), n = n(), .groups = 'drop') %>%
    filter(n >= 3)

n.expansion = table(expansion.status(STATES))

fig1 = ggplot() +
    annotate('rect', xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'grey92') +
    geom_vline(xintercept = START.YEAR, linetype = 'dashed', colour = 'grey40', linewidth = 0.4) +
    geom_line(data = df.head, aes(year, 100 * value, group = state, colour = expansion),
              linewidth = 0.3, alpha = 0.3) +
    geom_line(data = bands.expansion, aes(year, 100 * value, colour = expansion), linewidth = 1.1) +
    geom_line(data = bands, aes(year, 100 * value, linetype = line), colour = 'grey20', linewidth = 0.8) +
    facet_wrap(~ headline, scales = 'free_y', ncol = 3) +
    scale_colour_manual(values = COLS.EXPANSION, name = paste0('Medicaid (as of ', EXPANSION.AS.OF, '):'),
                        labels = c(Expansion = paste0('Expansion (', n.expansion['Expansion'], ' states)'),
                                   `Non-expansion` = paste0('Non-expansion (', n.expansion['Non-expansion'], ' states)'))) +
    scale_linetype_manual(values = c(Median = 'solid', `90th percentile` = 'dashed'), name = 'All states:') +
    scale_x_continuous(breaks = seq(2013, 2024, 2)) +
    guides(colour = guide_legend(order = 1, override.aes = list(alpha = 1, linewidth = 1.1)),
           linetype = guide_legend(order = 2, override.aes = list(linewidth = 0.8))) +
    labs(x = NULL, y = 'Percent',
         title = 'State trends since EHE launch, by Medicaid expansion status',
         subtitle = how.to.read(
             x.axis = 'calendar year.',
             y.axis = 'the measure named in the panel title, in percent. Each panel has its own scale.',
             read = c('Each thin line is one state: light blue = Medicaid expansion, light orange = non-expansion.',
                      'Thick blue / orange line = the median state in each group that year. Dark gray = all states: median (solid), 90th percentile (dashed).',
                      'A line going up = improvement. Compare the blue and orange lines to see whether expansion states differ.',
                      'Dashed vertical line = 2019 (EHE launch). Shaded band = 2020-2021 (COVID).')),
         caption = paste0('PrEP-to-need = PrEP users each year (AIDSVu) / PrEP indications (CDC). ',
                          'Indications exist only for 2017-2018, so the same denominator\n',
                          '(mean of 2017 and 2018) is used for every year. Before 2017 and after 2018, ',
                          'the trend reflects change in PrEP users only, not change in need.')) +
    theme_minimal(base_size = 11) +
    theme(legend.position = 'top', legend.justification = 'left', legend.box = 'vertical',
          legend.box.just = 'left', legend.key.width = unit(2, 'lines'),
          plot.caption = element_text(hjust = 0, colour = 'grey30', size = 9),
          panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0)) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig1_trends.png'), fig1, width = 12, height = 11.8, dpi = 200, bg = 'white')

# 6b. Figure 2: gap closed, states ranked
#     Vertical lines = median, 75th and 90th percentile across states.
#     Sort states within each panel (same idea as tidytext::reorder_within)
tidytext.reorder = function(x, by, within)
    reorder(paste(x, within, sep = '___'), by)

ref.lines = changes.head %>%
    group_by(headline) %>%
    summarise(Median = median(gap.closed.pct), `75th` = quantile(gap.closed.pct, 0.75),
              `90th` = quantile(gap.closed.pct, 0.9), .groups = 'drop') %>%
    pivot_longer(-headline, names_to = 'label', values_to = 'x') %>%
    mutate(label = factor(label, levels = c('Median', '75th', '90th')))

fig2 = ggplot(changes.head, aes(x = gap.closed.pct, y = tidytext.reorder(state, gap.closed.pct, headline))) +
    geom_vline(xintercept = 0, colour = 'grey60', linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = gap.closed.pct, yend = tidytext.reorder(state, gap.closed.pct, headline)),
                 colour = COL.STATE, linewidth = 0.4) +
    geom_point(colour = COL.MEDIAN, size = 1.6) +
    geom_vline(data = ref.lines, aes(xintercept = x, linetype = label),
               colour = COL.P90, linewidth = 0.5) +
    facet_wrap(~ headline, scales = 'free', ncol = 3) +
    scale_y_discrete(labels = function(x) sub('___.*$', '', x)) +
    scale_linetype_manual(values = c(Median = 'solid', `75th` = 'dashed', `90th` = 'dotted'),
                          name = 'Across states:') +
    labs(x = 'Gap closed from 2019 to latest year (%) = relative fall in the gap', y = NULL,
         title = 'How much of the gap each state closed since 2019',
         subtitle = how.to.read(
             x.axis = 'gap closed from 2019 to the latest year (%). Gap = share not on PrEP / not suppressed / not aware / not tested.',
             y.axis = 'states, sorted from most to least gap closed.',
             read = c('20% means the state cut its gap by one fifth. Example: suppression 70% -> 76% = gap 30% -> 24% = 20% closed.',
                      '0 = no change. Negative = the state got worse.',
                      'Orange lines = median (solid), 75th (dashed) and 90th percentile (dotted) across states. These are the benchmark levels.'))) +
    theme_minimal(base_size = 10) +
    theme(legend.position = 'top', legend.justification = 'left', legend.key.width = unit(2, 'lines'),
          panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 5.5), strip.text = element_text(face = 'bold', hjust = 0),
          panel.spacing.y = unit(1.2, 'lines')) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig2_change_by_state.png'), fig2, width = 12, height = 12, dpi = 200, bg = 'white')

# 6c. Figure 3: starting level vs gap closed
#     Checks for a ceiling effect: do states that started high close less of their gap?
#     The 5 states that closed the most of their gap are labeled in each panel.
#     Each panel title shows the Pearson correlation between the 2019 level and gap closed.
#       r < 0: states that started higher closed less of their gap (ceiling effect)
#       r > 0: states that started higher closed more
#     Gray line = straight-line (least squares) fit, to show the direction of r.
top5 = changes.head %>% group_by(headline) %>% slice_max(gap.closed.pct, n = 5)

# Pearson correlation per panel: r, 95% CI, p-value, number of states.
# Spearman (rank) correlation is saved too: it is less affected by single outlying states.
correlations = changes.head %>%
    group_by(headline) %>%
    group_modify(function(d, key) {
        x = 100 * d$start
        y = d$gap.closed.pct
        pearson = cor.test(x, y, method = 'pearson')
        spearman = suppressWarnings(cor.test(x, y, method = 'spearman', exact = F))
        data.frame(n.states = length(x),
                   pearson.r = unname(pearson$estimate),
                   pearson.ci.lower = pearson$conf.int[1],
                   pearson.ci.upper = pearson$conf.int[2],
                   pearson.p = pearson$p.value,
                   spearman.rho = unname(spearman$estimate),
                   spearman.p = spearman$p.value)
    }) %>%
    ungroup() %>%
    mutate(label = paste0('Pearson r = ', sprintf('%.2f', pearson.r),
                          ', p ', ifelse(pearson.p < 0.001, '< 0.001', paste0('= ', sprintf('%.3f', pearson.p))),
                          ', n = ', n.states))

write.csv(correlations %>% select(-label), file.path(OUT.DIR, 'fig3_correlations.csv'), row.names = F)
print(as.data.frame(correlations %>% select(-label) %>% mutate(across(where(is.numeric), ~ signif(., 3)))))

# Panel title = measure + correlation (kept out of the plot area so it never covers points)
fig3.data = changes.head %>%
    left_join(correlations %>% select(headline, label), by = 'headline') %>%
    mutate(panel = paste0(headline, '\n', label))
fig3.data$panel = factor(fig3.data$panel, levels = unique(fig3.data$panel[order(fig3.data$headline)]))
top5 = fig3.data %>% group_by(headline) %>% slice_max(gap.closed.pct, n = 5)

fig3 = ggplot(fig3.data, aes(100 * start, gap.closed.pct)) +
    geom_hline(yintercept = 0, colour = 'grey60', linewidth = 0.4) +
    geom_smooth(method = 'lm', formula = y ~ x, se = F, colour = 'grey45', linewidth = 0.6) +
    geom_point(colour = COL.MEDIAN, size = 1.8, alpha = 0.8) +
    geom_text_repel(data = top5, aes(label = state), size = 3, colour = 'grey20',
                    min.segment.length = 0, seed = 1) +
    facet_wrap(~ panel, scales = 'free', ncol = 3) +
    labs(x = 'Level in 2019 (%)', y = 'Gap closed, 2019 to latest year (%)\n(relative fall in the gap)',
         title = 'Starting level vs progress',
         subtitle = how.to.read(
             x.axis = 'the state\'s level in 2019 (%), before EHE.',
             y.axis = 'gap closed from 2019 to the latest year (%). Higher = more progress; negative = got worse.',
             read = c('One dot per state. Labeled = the 5 states that closed the most of their gap.',
                      'Gray line = straight-line fit. Pearson r in each panel title: -1 to +1, 0 = no relationship.',
                      'r < 0: states that started higher closed less of their gap (ceiling or catch-up). r > 0: states that started higher closed more.'))) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0, lineheight = 1.1)) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig3_start_vs_change.png'), fig3, width = 12, height = 11, dpi = 200, bg = 'white')

# 7. FIGURES BY GROUP: TRENDS OVER TIME ----
# One panel per measure. One line per group = median state that year.
# Band = 25th to 75th percentile of states (not drawn for age: too many overlapping bands).
# Proportions are shown in percent; PrEP users as an index (2019 = 1).
plot.trends.by = function(dim.name, colours, file, show.band = T)
{
    d = df %>%
        filter(dimension == dim.name) %>%
        mutate(y = ifelse(type == 'proportion', 100 * value, value),
               group = factor(group, levels = names(colours)))
    bands = d %>%
        group_by(measure, group, year) %>%
        summarise(median = median(y), p25 = quantile(y, 0.25), p75 = quantile(y, 0.75),
                  n = n(), .groups = 'drop') %>%
        filter(n >= 5)
    ends = bands %>% group_by(measure, group) %>% slice_max(year, n = 1)

    p = ggplot(bands, aes(year, median, colour = group, fill = group)) +
        annotate('rect', xmin = 2020, xmax = 2021, ymin = -Inf, ymax = Inf, fill = 'grey92') +
        geom_vline(xintercept = START.YEAR, linetype = 'dashed', colour = 'grey40', linewidth = 0.4)
    if (show.band)
        p = p + geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.12, colour = NA)
    p = p +
        geom_line(linewidth = 0.9) +
        geom_text_repel(data = ends, aes(label = group), size = 2.8, direction = 'y', hjust = 0,
                        nudge_x = 0.4, segment.colour = 'grey70', show.legend = F, seed = 1) +
        facet_wrap(~ measure, scales = 'free_y', ncol = 3) +
        scale_colour_manual(values = colours, name = NULL, drop = F) +
        scale_fill_manual(values = colours, name = NULL, drop = F) +
        scale_x_continuous(breaks = seq(2013, 2024, 2), expand = expansion(mult = c(0.02, 0.15))) +
        labs(x = NULL, y = 'Percent (PrEP users: index, 2019 = 1)',
             title = paste0('Trends by ', tolower(dim.name), ': median state'),
             subtitle = how.to.read(
                 x.axis = 'calendar year.',
                 y.axis = 'percent for proportions. For "PrEP users (2019 = 1)": users relative to 2019 (2 = twice as many as in 2019).',
                 read = c(paste0('Each line = the median state for that ', tolower(dim.name), ' group, each year.',
                                 if (show.band) ' Band = middle half of states (25th-75th percentile).' else ''),
                          'Distance between lines = the gap between groups. Lines moving apart = the gap is growing; together = narrowing.',
                          'Dashed vertical line = 2019 (EHE launch). Shaded band = 2020-2021 (COVID).'))) +
        theme_minimal(base_size = 11) +
        theme(legend.position = 'top', legend.justification = 'left',
              panel.grid.minor = element_blank(), strip.text = element_text(face = 'bold', hjust = 0)) +
        READ.STYLE

    n.panels = length(unique(d$measure))
    ggsave(file.path(OUT.DIR, file), p, width = 12, height = 3.6 * ceiling(n.panels / 3) + 2.2,
           dpi = 200, bg = 'white')
}

plot.trends.by('Sex', COLS.SEX, 'fig4_trends_by_sex.png')
plot.trends.by('Race', COLS.RACE, 'fig5_trends_by_race.png')
plot.trends.by('Age', COLS.AGE, 'fig6_trends_by_age.png', show.band = F)

# 8. FIGURES BY GROUP: CHANGE SINCE 2019 ----
# One dot per state. Black bar = median across states.
# y, in percent (panel title says which):
#     gap closed for proportions (relative fall in the gap),
#     relative change for PrEP users (growth in the number of users).
# Panel title shows the Friedman p-value: small p = groups changed differently.
plot.change.by = function(dim.name, colours, file)
{
    p.values = friedman %>% filter(dimension == dim.name)
    d = changes %>%
        add.compared() %>%
        filter(dimension == dim.name, !is.na(compared.pct)) %>%
        left_join(p.values, by = c('measure', 'dimension')) %>%
        mutate(panel = paste0(measure, '\n',
                              ifelse(type == 'index', 'relative change in users', 'gap closed'),
                              ', p = ', ifelse(is.na(friedman.p), 'NA', signif(friedman.p, 2))),
               group = factor(group, levels = names(colours)))
    # Keep panels in measure order (not alphabetical)
    d$panel = factor(d$panel, levels = unique(d$panel[order(d$measure)]))
    meds = d %>% group_by(panel, group) %>% summarise(median = median(compared.pct), .groups = 'drop')

    p = ggplot(d, aes(group, compared.pct, colour = group)) +
        geom_hline(yintercept = 0, colour = 'grey60', linewidth = 0.4) +
        geom_line(aes(group = state), colour = 'grey88', linewidth = 0.3) +
        geom_point(size = 1.5, alpha = 0.6) +
        geom_crossbar(data = meds, aes(y = median, ymin = median, ymax = median),
                      colour = 'grey15', width = 0.5, linewidth = 0.4) +
        facet_wrap(~ panel, scales = 'free_y', ncol = 3) +
        scale_colour_manual(values = colours, guide = 'none') +
        labs(x = NULL, y = 'Gap closed or relative change since 2019 (%)',
             title = paste0('Change since 2019 by ', tolower(dim.name)),
             subtitle = how.to.read(
                 x.axis = paste0(tolower(dim.name), ' group.'),
                 y.axis = 'change from 2019 to the latest year (%): gap closed for proportions, relative change in users for PrEP users (panel title says which).',
                 read = c('One dot per state. Gray lines join the same state across groups. Black bar = median state.',
                          'Higher = more improvement. 0 = no change. Negative = got worse.',
                          'p in the panel title (Friedman test, each state compared with itself): p < 0.05 = the groups changed differently.'))) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
              strip.text = element_text(face = 'bold', hjust = 0)) +
        READ.STYLE

    n.panels = length(unique(d$panel))
    ggsave(file.path(OUT.DIR, file), p, width = 12, height = 3.8 * ceiling(n.panels / 3) + 2.2,
           dpi = 200, bg = 'white')
}

plot.change.by('Sex', COLS.SEX, 'fig7_change_by_sex.png')
plot.change.by('Race', COLS.RACE, 'fig8_change_by_race.png')
plot.change.by('Age', COLS.AGE, 'fig9_change_by_age.png')

# 9. 2030 TARGETS FOR EACH STATE ----
# Apply the benchmark (implied gap closed over 2026-2030) to each state's projected 2026 level.
#
# Steps for one state:
#   1. Latest observed level (e.g. suppression in 2023).
#   2. Projected level in 2026 (proj.level.2026.pct, from section 3; see PROJECT.TO.2026 in settings)
#   3. Gap in 2026 = 1 - level in 2026
#   4. Gap in 2030 = gap in 2026 x (1 - benchmark)
#   5. Target 2030 = 1 - gap in 2030
#
# Worked example (suppression, 'flat'):
#   latest = 70% -> 2026 = 70% -> gap 30%.
#   90th percentile benchmark = 21.8% of the gap closed over 2026-2030.
#   Gap 2030 = 30% x (1 - 0.218) = 23.5% -> target 2030 = 76.5%.
#
# Benchmarks are taken from the same measure and group (e.g. Black suppression
# uses the Black benchmark). For comparison we also keep proj.2030.pct
# from section 3: where the state gets at its own pace.
#
# Notes
#   1. Proportions only. PrEP users (index) have no level to target.
#   2. If a benchmark is negative (testing), the target is below the projected 2026 level.
#      In the model we will not let an intervention push a value down.
#   3. States with no gap left (PrEP-to-need >= 100%) get no target (NA).
#   4. In the model, the 2026 level will be the model's own projection, and the
#      benchmark is applied the same way: as a multiplier on the gap.
benchmarks = summary.table %>%
    filter(type == 'proportion') %>%
    select(measure, dimension, group,
           benchmark.median.pct = implied.gap.closed.2026.2030.pct.median,
           benchmark.p75.pct = implied.gap.closed.2026.2030.pct.p75,
           benchmark.p90.pct = implied.gap.closed.2026.2030.pct.p90)

# gap 2030 = gap 2026 x (1 - benchmark); returns the 2030 level in percent
target.from.gap = function(proj.gap.2026, benchmark.pct)
    100 * (1 - proj.gap.2026 * (1 - benchmark.pct / 100))

targets = changes %>%
    filter(type == 'proportion') %>%
    select(measure, dimension, group, state, latest.year = end.year, end,
           proj.level.2026.pct, proj.gap.2026, implied.gap.closed.2026.2030.pct, proj.2030.pct) %>%
    left_join(benchmarks, by = c('measure', 'dimension', 'group')) %>%
    mutate(level.latest.pct = 100 * end,
           # Steps 4-5: 2030 targets
           proj.target.2030.median.pct = target.from.gap(proj.gap.2026, benchmark.median.pct),
           proj.target.2030.p75.pct = target.from.gap(proj.gap.2026, benchmark.p75.pct),
           proj.target.2030.p90.pct = target.from.gap(proj.gap.2026, benchmark.p90.pct)) %>%
    select(measure, dimension, group, state, latest.year, level.latest.pct, proj.level.2026.pct,
           implied.gap.closed.2026.2030.pct, proj.2030.pct,
           benchmark.median.pct, benchmark.p75.pct, benchmark.p90.pct,
           proj.target.2030.median.pct, proj.target.2030.p75.pct, proj.target.2030.p90.pct)

write.csv(targets, file.path(OUT.DIR, 'targets_2030_by_state.csv'), row.names = F)

# Quick look: median across states of the projected 2026 level, own-pace 2030 and 2030 targets
print(as.data.frame(targets %>%
    group_by(measure, dimension, group) %>%
    summarise(across(c(proj.level.2026.pct, proj.2030.pct, proj.target.2030.median.pct,
                       proj.target.2030.p75.pct, proj.target.2030.p90.pct),
                     ~ round(median(.x, na.rm = T), 1)), .groups = 'drop')))

# 10. FIGURE 10: 2026 LEVEL VS 2030 TARGETS (HEADLINE MEASURES) ----
# One row per state, sorted by the projected 2026 level.
# Gray = projected 2026 level. Blue = target at the median benchmark. Orange = target at the 90th percentile.
targets.head = pick.headline(targets) %>% filter(!is.na(proj.level.2026.pct)) %>%
    mutate(sort.level = proj.level.2026.pct)     # sort key, kept for every layer
targets.long = targets.head %>%
    select(headline, state, sort.level, proj.level.2026.pct, proj.target.2030.median.pct, proj.target.2030.p90.pct) %>%
    pivot_longer(-c(headline, state, sort.level), names_to = 'point', values_to = 'value') %>%
    mutate(point = factor(point, levels = c('proj.level.2026.pct', 'proj.target.2030.median.pct', 'proj.target.2030.p90.pct'),
                          labels = c('2026 level (projected)', '2030 target, median benchmark',
                                     '2030 target, 90th percentile benchmark')))

fig10 = ggplot(targets.head, aes(y = tidytext.reorder(state, sort.level, headline))) +
    geom_segment(aes(x = proj.level.2026.pct, xend = proj.target.2030.p90.pct,
                     yend = tidytext.reorder(state, sort.level, headline)),
                 colour = COL.STATE, linewidth = 0.4) +
    geom_point(data = targets.long, aes(x = value, colour = point), size = 1.6) +
    facet_wrap(~ headline, scales = 'free', ncol = 3) +
    scale_y_discrete(labels = function(x) sub('___.*$', '', x)) +
    scale_colour_manual(values = c('grey55', COL.MEDIAN, COL.P90), name = NULL) +
    labs(x = 'Percent', y = NULL,
         title = paste0('2030 targets if each state matches the benchmark pace, 2026-2030'),
         subtitle = how.to.read(
             x.axis = 'level of the measure (%).',
             y.axis = 'states, sorted by their projected 2026 level.',
             read = c(paste0('Gray = projected 2026 level (latest observed value ',
                             ifelse(PROJECT.TO.2026 == 'flat', 'carried forward', 'continued at the state\'s own pace'), ').'),
                      'Blue = 2030 target if the state closes its gap at the median state\'s pace. Orange = at the 90th percentile pace.',
                      'Target 2030 = 1 - gap 2026 x (1 - benchmark gap closed). Gray to orange = improvement needed under the strongest benchmark.',
                      'If a target is left of the gray dot, the benchmark is negative (testing): the median state got worse.'))) +
    theme_minimal(base_size = 10) +
    theme(legend.position = 'top', legend.justification = 'left',
          panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 5.5), strip.text = element_text(face = 'bold', hjust = 0)) +
    READ.STYLE

ggsave(file.path(OUT.DIR, 'fig10_targets_2030.png'), fig10, width = 12, height = 12.2, dpi = 200, bg = 'white')

print(paste0("Saved tables and figures to ", OUT.DIR))
