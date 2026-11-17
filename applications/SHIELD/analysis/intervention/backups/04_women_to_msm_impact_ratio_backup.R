# ****************************************************************************************************
# 04 WOMEN-TO-MSM RELATIVE IMPACT RATIO ----
# ****************************************************************************************************
# Run it after 01a_generate_raw_results_arrays.R: it reads only the raw arrays. It does not need
# 03_manuscript_main.R, but uses its intervention.results if that is already in memory.
#
# WHAT IT BUILDS (<cov> = the coverage, e.g. 20pct)
#   1. tables/4-impact_ratio_values_at_<cov>_doxy.csv         both ratios and the baseline characteristics, per MSA
#   2. tables/4-impact_ratio_associations_at_<cov>_doxy.csv   Spearman correlation of each ratio with each characteristic
#      tables/4-reduction_women_associations_at_<cov>_doxy.csv   the same for the % reduction in incidence among women
#   3. figures/4-impact_ratio_vs_drivers_at_<cov>_doxy.png    the women-to-MSM ratio only, against each characteristic
#                                                             with |rho| > RHO.CUTOFF, one panel each
#
# DEFINITIONS
#   1. Relative reduction in incidence, 2030 = 100 x (without Doxy-PEP - with) / without
#   2. Women-to-MSM Relative Impact Ratio = relative reduction among women / among MSM
#        e.g. women 10%, MSM 30%: 10 / 30 = 0.33. Higher = greater impact among women.
#        NA when MSM incidence did not fall.
#   3. Heterosexual men-to-MSM Relative Impact Ratio: the same, with heterosexual men in place of
#      women. In the tables only, not the figure.
#   4. Baseline characteristics, 2022, no Doxy-PEP:
#        a. incidence rate among women (per 100,000 women)
#        b. incidence rate among MSM (per 100,000 MSM)
#        c. population size: the whole MSA, and MSM
#        d. total syphilis diagnosis rate (all stages, per 100,000): everyone, women, MSM
#        e. P&S syphilis diagnosis rate (per 100,000): everyone, women, MSM
#        f. past incidence change = log(fold change among women / fold change among MSM),
#           where fold change = incident infections in 2022 / in PAST.YEAR (2018)
#        g. past diagnosis change: the same as f., with total syphilis diagnoses (all stages) in
#           place of incident infections. Unlike incidence, diagnoses are observed in surveillance.
#      For the heterosexual men ratio, everything about women uses heterosexual men instead
#      (HETMALE.DRIVERS); the rest is the same.
#      To add or drop one: compute it in section 3, add it to `sims` and COLUMNS (section 4), and
#      to DRIVERS or HETMALE.DRIVERS.
#
# HOW
#   1. Everything is computed within each simulation.
#   2. Each MSA is summarised as median [2.5th to 97.5th percentile] across simulations.
#   3. Correlations are Spearman, across the ten MSAs. No p-values.
# ****************************************************************************************************


# ---- SETUP ----
# source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')

FIG.DIR   <- shield.fig.path(CALIB.NAME,   create = TRUE)
TABLE.DIR <- shield.table.path(CALIB.NAME, create = TRUE)

COVERAGE  <- "doxy.cov.20"                                            # Doxy-PEP coverage
COV.LABEL <- paste0(sub("doxy.cov.", "", COVERAGE, fixed = TRUE), "pct")   # "20pct", for the file names
PAST.YEAR <- "2018"                                                   # start of the past change (to YEAR0)

# The result arrays, in intervention.results as in 03_manuscript_main.R. Only the raw arrays are
# needed. If intervention.results is already in memory for this calibration and holds them, it is
# used as it is; otherwise the two raw arrays are read from disk.
if (!exists("intervention.results") ||
    !identical(attr(intervention.results, "calibration.code"), CALIB.NAME) ||
    !all(c("total_raw", "sex_raw") %in% names(intervention.results)))
  intervention.results <- load.shield.results(CALIB.NAME, which = c("total_raw", "sex_raw"))

total.raw <- intervention.results$total_raw
sex.raw   <- intervention.results$sex_raw


# ---- 1. HELPER ----
# Picks one outcome, one intervention, one year, and optionally one sex group.
# Returns a matrix: one row per MSA, one column per simulation.
sim_values <- function(arr, outcome, intervention, year, sex = NULL) {
  keep <- list(outcome = outcome, intervention = intervention, year = year)
  if (!is.null(sex)) keep$sex <- sex
  x <- apply(subset_array(arr, keep), c("location", "sim"), sum)
  rownames(x) <- names(LOCATIONS)[match(rownames(x), LOCATIONS)]   # location codes -> city names
  x[MSAS, ]
}


# ---- 2. THE COUNTS (each is MSA x simulation) ----
# 2a. Incident infections in 2030, without and with Doxy-PEP
msm.inc.2030.without   <- sim_values(sex.raw, "incidence", "noint",  EVAL.YEAR, "msm")
msm.inc.2030.with      <- sim_values(sex.raw, "incidence", COVERAGE, EVAL.YEAR, "msm")
women.inc.2030.without <- sim_values(sex.raw, "incidence", "noint",  EVAL.YEAR, "female")
women.inc.2030.with    <- sim_values(sex.raw, "incidence", COVERAGE, EVAL.YEAR, "female")
hetmale.inc.2030.without   <- sim_values(sex.raw, "incidence", "noint",  EVAL.YEAR, "heterosexual_male")
hetmale.inc.2030.with      <- sim_values(sex.raw, "incidence", COVERAGE, EVAL.YEAR, "heterosexual_male")

# 2b. Incident infections, diagnoses (all stages and P&S) and population in 2022 (no Doxy-PEP yet)
msm.inc.2022     <- sim_values(sex.raw,   "incidence",       "noint", YEAR0, "msm")
women.inc.2022   <- sim_values(sex.raw,   "incidence",       "noint", YEAR0, "female")
hetmale.inc.2022 <- sim_values(sex.raw,   "incidence",       "noint", YEAR0, "heterosexual_male")
total.dx.2022    <- sim_values(total.raw, "diagnosis.total", "noint", YEAR0)
msm.dx.2022      <- sim_values(sex.raw,   "diagnosis.total", "noint", YEAR0, "msm")
women.dx.2022    <- sim_values(sex.raw,   "diagnosis.total", "noint", YEAR0, "female")
hetmale.dx.2022  <- sim_values(sex.raw,   "diagnosis.total", "noint", YEAR0, "heterosexual_male")
total.ps.2022    <- sim_values(total.raw, "diagnosis.ps",    "noint", YEAR0)
msm.ps.2022      <- sim_values(sex.raw,   "diagnosis.ps",    "noint", YEAR0, "msm")
women.ps.2022    <- sim_values(sex.raw,   "diagnosis.ps",    "noint", YEAR0, "female")
hetmale.ps.2022  <- sim_values(sex.raw,   "diagnosis.ps",    "noint", YEAR0, "heterosexual_male")
total.pop.2022   <- sim_values(total.raw, "population",      "noint", YEAR0)
msm.pop.2022     <- sim_values(sex.raw,   "population",      "noint", YEAR0, "msm")
women.pop.2022   <- sim_values(sex.raw,   "population",      "noint", YEAR0, "female")
hetmale.pop.2022 <- sim_values(sex.raw,   "population",      "noint", YEAR0, "heterosexual_male")

# 2c. Incident infections and total syphilis diagnoses in PAST.YEAR (2018), for the past change
msm.inc.2018     <- sim_values(sex.raw, "incidence",       "noint", PAST.YEAR, "msm")
women.inc.2018   <- sim_values(sex.raw, "incidence",       "noint", PAST.YEAR, "female")
hetmale.inc.2018 <- sim_values(sex.raw, "incidence",       "noint", PAST.YEAR, "heterosexual_male")
msm.dx.2018      <- sim_values(sex.raw, "diagnosis.total", "noint", PAST.YEAR, "msm")
women.dx.2018    <- sim_values(sex.raw, "diagnosis.total", "noint", PAST.YEAR, "female")
hetmale.dx.2018  <- sim_values(sex.raw, "diagnosis.total", "noint", PAST.YEAR, "heterosexual_male")


# ---- 3. THE QUANTITIES, WITHIN EACH SIMULATION (each is MSA x simulation) ----
# 3a. The ratios
reduction.msm.inc   <- 100 * (msm.inc.2030.without   - msm.inc.2030.with)   / msm.inc.2030.without
reduction.women.inc <- 100 * (women.inc.2030.without - women.inc.2030.with) / women.inc.2030.without
reduction.hetmale.inc   <- 100 * (hetmale.inc.2030.without   - hetmale.inc.2030.with)   / hetmale.inc.2030.without

ratio.inc <- reduction.women.inc / reduction.msm.inc        # women to MSM
ratio.inc[reduction.msm.inc <= 0] <- NA

ratio.hetmale.inc <- reduction.hetmale.inc / reduction.msm.inc      # heterosexual men to MSM
ratio.hetmale.inc[reduction.msm.inc <= 0] <- NA

# 3b. The baseline characteristics: rates per 100,000 of each group, and past change
per_100k <- function(count, pop) 100000 * count / pop

women.inc.rate.2022   <- per_100k(women.inc.2022,   women.pop.2022)     # incidence rate
msm.inc.rate.2022     <- per_100k(msm.inc.2022,     msm.pop.2022)
hetmale.inc.rate.2022 <- per_100k(hetmale.inc.2022, hetmale.pop.2022)

total.dx.rate.2022    <- per_100k(total.dx.2022,    total.pop.2022)     # total syphilis diagnosis rate
women.dx.rate.2022    <- per_100k(women.dx.2022,    women.pop.2022)
msm.dx.rate.2022      <- per_100k(msm.dx.2022,      msm.pop.2022)
hetmale.dx.rate.2022  <- per_100k(hetmale.dx.2022,  hetmale.pop.2022)

total.ps.rate.2022    <- per_100k(total.ps.2022,    total.pop.2022)     # P&S diagnosis rate
women.ps.rate.2022    <- per_100k(women.ps.2022,    women.pop.2022)
msm.ps.rate.2022      <- per_100k(msm.ps.2022,      msm.pop.2022)
hetmale.ps.rate.2022  <- per_100k(hetmale.ps.2022,  hetmale.pop.2022)

past.change            <- log((women.inc.2022   / women.inc.2018)   / (msm.inc.2022 / msm.inc.2018))   # log ratio of incidence change 2018-2022, women to MSM
past.change.hetmale    <- log((hetmale.inc.2022 / hetmale.inc.2018) / (msm.inc.2022 / msm.inc.2018))   # the same, heterosexual men to MSM
past.dx.change         <- log((women.dx.2022    / women.dx.2018)    / (msm.dx.2022  / msm.dx.2018))    # log ratio of diagnosis change 2018-2022, women to MSM
past.dx.change.hetmale <- log((hetmale.dx.2022  / hetmale.dx.2018)  / (msm.dx.2022  / msm.dx.2018))    # the same, heterosexual men to MSM


# ---- 4. ONE DATA FRAME: one row per MSA and simulation ----
sims <- data.frame(location               = rep(MSAS, times = ncol(ratio.inc)),
                   sim                    = rep(colnames(ratio.inc), each = length(MSAS)),
                   reduction_msm          = as.vector(reduction.msm.inc),
                   reduction_women        = as.vector(reduction.women.inc),
                   reduction_hetmale      = as.vector(reduction.hetmale.inc),
                   ratio                  = as.vector(ratio.inc),
                   ratio_hetmale          = as.vector(ratio.hetmale.inc),
                   rate_women             = as.vector(women.inc.rate.2022),
                   rate_hetmale           = as.vector(hetmale.inc.rate.2022),
                   rate_msm               = as.vector(msm.inc.rate.2022),
                   population             = as.vector(total.pop.2022),
                   population_msm         = as.vector(msm.pop.2022),
                   dx_rate_total          = as.vector(total.dx.rate.2022),
                   dx_rate_women          = as.vector(women.dx.rate.2022),
                   dx_rate_hetmale        = as.vector(hetmale.dx.rate.2022),
                   dx_rate_msm            = as.vector(msm.dx.rate.2022),
                   ps_rate_total          = as.vector(total.ps.rate.2022),
                   ps_rate_women          = as.vector(women.ps.rate.2022),
                   ps_rate_hetmale        = as.vector(hetmale.ps.rate.2022),
                   ps_rate_msm            = as.vector(msm.ps.rate.2022),
                   past_change            = as.vector(past.change),
                   past_change_hetmale    = as.vector(past.change.hetmale),
                   past_dx_change         = as.vector(past.dx.change),
                   past_dx_change_hetmale = as.vector(past.dx.change.hetmale))

# Every column of `sims`: its label in the tables and figure, and the decimals shown in the
# values table. This is the only place the labels are written. The row order here is the column
# order of the values table.
column <- function(name, label, digits) data.frame(name = name, label = label, digits = digits)
COLUMNS <- rbind(
  column("reduction_msm",          "Reduction in incidence among MSM (%), 2030",                                 1),
  column("reduction_women",        "Reduction in incidence among women (%), 2030",                               1),
  column("reduction_hetmale",      "Reduction in incidence among heterosexual men (%), 2030",                    1),
  column("ratio",                  "Women-to-MSM Relative Impact Ratio, 2030",                                   2),
  column("ratio_hetmale",          "Heterosexual men-to-MSM Relative Impact Ratio, 2030",                        2),
  column("rate_women",             "Incidence rate among women (per 100,000), 2022",                             1),
  column("rate_hetmale",           "Incidence rate among heterosexual men (per 100,000), 2022",                  1),
  column("rate_msm",               "Incidence rate among MSM (per 100,000), 2022",                               0),
  column("population",             "Population size, 2022",                                                      0),
  column("population_msm",         "MSM population size, 2022",                                                  0),
  column("dx_rate_total",          "Total syphilis diagnosis rate (per 100,000), 2022",                          1),
  column("dx_rate_women",          "Total syphilis diagnosis rate among women (per 100,000), 2022",              1),
  column("dx_rate_hetmale",        "Total syphilis diagnosis rate among heterosexual men (per 100,000), 2022",   1),
  column("dx_rate_msm",            "Total syphilis diagnosis rate among MSM (per 100,000), 2022",                0),
  column("ps_rate_total",          "P&S syphilis diagnosis rate (per 100,000), 2022",                            1),
  column("ps_rate_women",          "P&S syphilis diagnosis rate among women (per 100,000), 2022",                1),
  column("ps_rate_hetmale",        "P&S syphilis diagnosis rate among heterosexual men (per 100,000), 2022",     1),
  column("ps_rate_msm",            "P&S syphilis diagnosis rate among MSM (per 100,000), 2022",                  0),
  column("past_change",            "Past relative incidence change, women to MSM, 2018-2022",                    2),
  column("past_change_hetmale",    "Past relative incidence change, heterosexual men to MSM, 2018-2022",         2),
  column("past_dx_change",         "Past relative diagnosis change, women to MSM, 2018-2022",                    2),
  column("past_dx_change_hetmale", "Past relative diagnosis change, heterosexual men to MSM, 2018-2022",         2))
LABELS <- setNames(COLUMNS$label, COLUMNS$name)

# The characteristics we test against the women's outcomes. The order here is the order of the
# rows in the associations tables and of the figure panels.
DRIVERS <- LABELS[c("rate_women", "rate_msm", "population", "population_msm",
                    "dx_rate_total", "dx_rate_women", "dx_rate_msm",
                    "ps_rate_total", "ps_rate_women", "ps_rate_msm",
                    "past_change", "past_dx_change")]

# The same for the heterosexual men-to-MSM ratio: heterosexual men in place of women; the other
# characteristics are the same as above.
HETMALE.DRIVERS <- LABELS[c("rate_hetmale", "rate_msm", "population", "population_msm",
                            "dx_rate_total", "dx_rate_hetmale", "dx_rate_msm",
                            "ps_rate_total", "ps_rate_hetmale", "ps_rate_msm",
                            "past_change_hetmale", "past_dx_change_hetmale")]



# ---- 5. TABLE: MEDIAN [95% INTERVAL] PER MSA ----
# "median [2.5th to 97.5th percentile]", rounded to d decimals
show <- function(x, d) {
  q <- round(quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE), d)
  paste0(q[1], " [", q[2], " to ", q[3], "]")
}

# 5a. Every column in COLUMNS, as median [95% interval], with its own number of decimals
values.table <- sims %>%
  group_by(location) %>%
  summarise(across(all_of(COLUMNS$name), ~ show(.x, COLUMNS$digits[COLUMNS$name == cur_column()])),
            # checks the "bounded below by 0" statement: a ratio < 0 means that group's incidence rose
            below0_women   = round(100 * mean(ratio         < 0, na.rm = TRUE), 1),
            below0_hetmale = round(100 * mean(ratio_hetmale < 0, na.rm = TRUE), 1))

# 5b. Column names -> labels
names(values.table) <- c("MSA", LABELS,
                         "Simulations with women-to-MSM ratio below 0 (%)",
                         "Simulations with heterosexual men-to-MSM ratio below 0 (%)")

write.csv(values.table, file.path(TABLE.DIR, paste0("4-impact_ratio_values_at_", COV.LABEL, "_doxy.csv")), row.names = FALSE)
values.table


# ---- 6. TABLE: CORRELATIONS ----
spearman <- function(x, y) cor(x, y, method = "spearman", use = "complete.obs")

# 6a. The median of every column for each MSA (the points in the figure): one row per MSA
medians <- sims %>%
  group_by(location) %>%
  summarise(across(-sim, ~ median(.x, na.rm = TRUE)))

# 6b. The rows of `sims` split by simulation: one data frame of ten MSAs per simulation
by.sim <- split(sims, sims$sim)

# 6c. One row per characteristic, for one outcome (outcome.col is its column in `sims`):
#       1. rho between the MSA medians
#       2. rho within each simulation, across the ten MSAs, reported as median [95% interval]
#          to show the model uncertainty
association_table <- function(outcome.label, outcome.col, drivers) {
  rho.medians <- sapply(names(drivers), function(v) round(spearman(medians[[v]], medians[[outcome.col]]), 2))
  rho.sims    <- sapply(names(drivers), function(v)
                   show(sapply(by.sim, function(d) spearman(d[[v]], d[[outcome.col]])), 2))
  data.frame("Outcome"                               = outcome.label,
             "Baseline characteristic"               = drivers,
             "Spearman rho, MSA medians"             = rho.medians,
             "Spearman rho within simulations [95%]" = rho.sims,
             check.names = FALSE, row.names = NULL)
}

# 6d. The two ratios, in one file, women first
associations         <- association_table("Women-to-MSM Relative Impact Ratio",            "ratio",         DRIVERS)          # used by the figure
hetmale.associations <- association_table("Heterosexual men-to-MSM Relative Impact Ratio", "ratio_hetmale", HETMALE.DRIVERS)  # table only

write.csv(rbind(associations, hetmale.associations),
          file.path(TABLE.DIR, paste0("4-impact_ratio_associations_at_", COV.LABEL, "_doxy.csv")), row.names = FALSE)
rbind(associations, hetmale.associations)

# 6e. Separately: the % reduction in incidence among women itself (the numerator of the women-to-MSM
#     ratio), against the same characteristics. Its own file; not in the figure.
women.associations <- association_table("Reduction in incidence among women (%), 2030", "reduction_women", DRIVERS)

write.csv(women.associations,
          file.path(TABLE.DIR, paste0("4-reduction_women_associations_at_", COV.LABEL, "_doxy.csv")), row.names = FALSE)
women.associations


# ---- 7. FIGURE ----
# The women-to-MSM ratio only. One panel per characteristic, in the order of DRIVERS. One point per MSA, at the medians. The
# dashed line is a least-squares fit, only to guide the eye. The subtitle is rho between the
# medians (6a).
#
# Only the characteristics with the strongest associations are drawn: |rho| between the MSA
# medians above RHO.CUTOFF. This is a cutoff on the size of rho, not a test of significance.
RHO.CUTOFF <- 0.5

strong.associations <- associations[abs(associations$`Spearman rho, MSA medians`) > RHO.CUTOFF, ]
strong.DRIVERS      <- DRIVERS[DRIVERS %in% strong.associations$`Baseline characteristic`]
# remove incidences
strong.DRIVERS      <- strong.DRIVERS[-grep("incidence",strong.DRIVERS)]
strong.DRIVERS      <- strong.DRIVERS[-grep("Incidence",strong.DRIVERS)]

if (length(strong.DRIVERS) == 0)
  stop("No characteristic has |rho| > ", RHO.CUTOFF, ", so there is nothing to plot. Lower RHO.CUTOFF.")

# Drawn on a log scale: sizes and counts, which differ a lot between MSAs
LOG.SCALE <- c("population", "population_msm")

panel <- function(x.col, letter) {
  rho <- spearman(medians[[x.col]], medians$ratio)
  p <- ggplot(medians, aes(x = .data[[x.col]], y = ratio)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", colour = PAL.FIT) +
    geom_point(size = 3, colour = PAL.POINT) +
    ggrepel::geom_text_repel(aes(label = location), size = 3) +
    labs(x = strong.DRIVERS[[x.col]], y = "Women-to-MSM Relative Impact Ratio", title = letter,
         subtitle = paste0("Spearman rho = ", round(rho, 2))) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
  if (x.col %in% LOG.SCALE)
    p <- p + scale_x_log10(labels = scales::comma) + labs(x = paste0(strong.DRIVERS[[x.col]], " (log scale)"))
  p
}

N.COL <- 2   # panels per row

panels <- lapply(seq_along(strong.DRIVERS), function(i) panel(names(strong.DRIVERS)[i], LETTERS[i]))

# y-axis title only on the left column (panels 1, 4, 7, ...); the others share it
for (i in seq_along(panels))
  if ((i - 1) %% N.COL != 0)
    panels[[i]] <- panels[[i]] + theme(axis.title.y = element_blank())

impact.fig <- wrap_plots(panels, ncol = N.COL)
impact.fig

# 5 inches of height per row of panels
ggsave(file.path(FIG.DIR, paste0("Fig5-impact_ratio_vs_drivers_at_", COV.LABEL, "_doxy.png")), impact.fig,
       width = 10, height = 4 * ceiling(length(panels) / N.COL), dpi = 300, bg = "white")
