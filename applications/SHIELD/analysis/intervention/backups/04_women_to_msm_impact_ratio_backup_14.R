# ****************************************************************************************************
# 04 WOMEN-TO-MSM RELATIVE IMPACT RATIO ----
# ****************************************************************************************************
# Run it after 01a_generate_raw_results_arrays.R: it reads only the raw arrays. It does not need
# 03_manuscript_main.R, but uses its intervention.results if that is already in memory.
#
# WHAT IT BUILDS (<cov> = the coverage, e.g. 20pct)
#   1. tables/4-RIR_and_characteristics_by_MSA_at_<cov>_doxy.csv         both ratios and the baseline characteristics, per MSA
#   2. tables/4-impact_ratio_associations_at_<cov>_doxy.csv   Spearman correlation of the women-to-MSM RIR with
#                                                             each characteristic (selects Figure 5 panels D onward)
#      tables/4-unused_associations_at_<cov>_doxy.csv         NOT USED by any figure or table: the same for the
#                                                             heterosexual men-to-MSM RIR and for the % reduction
#                                                             in incidence among women
#   3. figures/Fig5-impact_ratio_vs_drivers_at_<cov>_doxy.png   Figure 5, assembled in section 10:
#                                                             A. women RIR in each MSA, median and 95% interval
#                                                             B. % reduction in total-population incidence vs women RIR
#                                                             C. heterosexual men RIR vs women RIR
#                                                             D, E, F, ... women RIR vs each characteristic
#                                                                with |rho| > RHO.CUTOFF, one panel each
#   4. tables/4-population_reduction_vs_RIR_at_<cov>_doxy.csv   Spearman rho between the % reduction in
#                                                             total-population incidence and each RIR (Fig 5B)
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
#
# FIGURE RULE
#   The figure shows only the characteristics with |rho| between the MSA medians > RHO.CUTOFF,
#   and never the incidence-based ones (incidence rates and the past incidence change): incidence
#   is not observed, so it is not used in the figure. They stay in the tables.
# ****************************************************************************************************


# ---- SETUP ----
# message("04 needs the settings from 00_settings.R (CALIB.NAME, LOCATIONS, MSAS, YEAR0, EVAL.YEAR). ",
        # "Run 03_manuscript_main.R first in this session, or source 00_settings.R yourself.")
# source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
# source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')

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


# Printed wherever a 95% interval or a within-simulation rho is reported
PAIRING.NOTE <- paste0(
  "NOTE on simulation pairing: simulations are paired within each MSA across scenarios ",
  "(sim i with and without Doxy-PEP share the same parameters), but NOT across MSAs ",
  "(sim 1 in Baltimore is unrelated to sim 1 in Atlanta). Quantities that combine MSAs within a ",
  "simulation (Total-row sums, within-simulation rho across MSAs) therefore pair independent ",
  "draws arbitrarily; their 95% intervals assume independence across MSAs.")


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

rir.women <- reduction.women.inc / reduction.msm.inc        # women to MSM
rir.women[reduction.msm.inc <= 0] <- NA

rir.hetmale <- reduction.hetmale.inc / reduction.msm.inc      # heterosexual men to MSM
rir.hetmale[reduction.msm.inc <= 0] <- NA

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
sims <- data.frame(location               = rep(MSAS, times = ncol(rir.women)),
                   sim                    = rep(colnames(rir.women), each = length(MSAS)),
                   reduction_msm          = as.vector(reduction.msm.inc),
                   reduction_women        = as.vector(reduction.women.inc),
                   reduction_hetmale      = as.vector(reduction.hetmale.inc),
                   rir_women              = as.vector(rir.women),
                   rir_hetmale            = as.vector(rir.hetmale),
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
  column("rir_women",              "Women-to-MSM Relative Impact Ratio, 2030",                                   2),
  column("rir_hetmale",            "Heterosexual men-to-MSM Relative Impact Ratio, 2030",                        2),
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
            below0_women   = round(100 * mean(rir_women   < 0, na.rm = TRUE), 1),
            below0_hetmale = round(100 * mean(rir_hetmale < 0, na.rm = TRUE), 1))

# 5b. Column names -> labels
names(values.table) <- c("MSA", LABELS,
                         "Simulations with women-to-MSM ratio below 0 (%)",
                         "Simulations with heterosexual men-to-MSM ratio below 0 (%)")

write.csv(values.table, file.path(TABLE.DIR, paste0("4-RIR_and_characteristics_by_MSA_at_", COV.LABEL, "_doxy.csv")), row.names = FALSE)
values.table
message(PAIRING.NOTE)


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

# 6d. The women-to-MSM RIR: used by Figure 5 (its |rho| picks panels D onward)
associations <- association_table("Women-to-MSM Relative Impact Ratio", "rir_women", DRIVERS)

write.csv(associations,
          file.path(TABLE.DIR, paste0("4-impact_ratio_associations_at_", COV.LABEL, "_doxy.csv")), row.names = FALSE)
associations
message(PAIRING.NOTE)

# 6e. NOT USED for anything at the moment: no figure or table reads these. Kept in one file for
#     reference only:
#       1. the heterosexual men-to-MSM RIR against its characteristics (HETMALE.DRIVERS)
#       2. the % reduction in incidence among women itself (the numerator of the women-to-MSM RIR)
hetmale.associations <- association_table("Heterosexual men-to-MSM Relative Impact Ratio", "rir_hetmale", HETMALE.DRIVERS)
women.associations   <- association_table("Reduction in incidence among women (%), 2030",  "reduction_women", DRIVERS)
unused.associations  <- rbind(hetmale.associations, women.associations)

con <- file(file.path(TABLE.DIR, paste0("4-unused_associations_at_", COV.LABEL, "_doxy.csv")), "w")
writeLines(c(
  '"# NOT USED by any figure or table. Spearman rho of (1) the heterosexual men-to-MSM RIR and (2) the % reduction in incidence among women (2030, with vs without Doxy-PEP) with each baseline characteristic."',
  '"# MSA medians: rho across the 10 MSA medians. Within simulations: rho across the 10 MSAs in each simulation, median [2.5th to 97.5th percentile]; simulations are not paired across MSAs."'),
  con)
write.csv(unused.associations, con, row.names = FALSE)
close(con)
unused.associations
message("6e: the heterosexual men RIR and women-reduction association tables are NOT USED for anything ",
        "at the moment (no figure or table reads them). Saved for reference only.")
message(PAIRING.NOTE)


# ---- 7. FIGURE ----
# The women-to-MSM ratio only.
# Panel A: the projected ratio in 2030 in each MSA. Point = median, line = 2.5th to 97.5th
# percentile across simulations (simulations where MSM incidence did not fall are NA and left
# out, as in the tables). MSAs ordered from lowest to highest median. Dotted line at 1 = the same
# relative reduction among women as among MSM.
# Panels D onward (B and C are built in sections 8 and 9): one panel per characteristic, in the order of DRIVERS. One point per MSA, at the medians. The
# dashed line is a least-squares fit, only to guide the eye. The subtitle is rho between the
# medians (6a).
#
# Only the characteristics with the strongest associations are drawn: |rho| between the MSA
# medians above RHO.CUTOFF. This is a cutoff on the size of rho, not a test of significance.
RHO.CUTOFF <- 0.5

strong.associations <- associations[abs(associations$`Spearman rho, MSA medians`) > RHO.CUTOFF, ]
strong.DRIVERS      <- DRIVERS[DRIVERS %in% strong.associations$`Baseline characteristic`]
# remove incidences (see FIGURE RULE in the header). grepl(), not -grep(): -grep() with no match
# returns an empty vector and would drop every characteristic
strong.DRIVERS      <- strong.DRIVERS[!grepl("incidence", strong.DRIVERS, ignore.case = TRUE)]

if (length(strong.DRIVERS) == 0)
  stop("No characteristic has |rho| > ", RHO.CUTOFF, ", so there is nothing to plot. Lower RHO.CUTOFF.")

# Drawn on a log scale: sizes and counts, which differ a lot between MSAs
LOG.SCALE <- c("population", "population_msm")

panel <- function(x.col, letter) {
  rho <- spearman(medians[[x.col]], medians$rir_women)
  p <- ggplot(medians, aes(x = .data[[x.col]], y = rir_women)) +
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

N.COL <- 3   # panels per row in panel B of Figure 5

# Panel A: the projected ratio in each MSA, median and 95% interval
rir.by.msa <- sims %>%
  group_by(location) %>%
  summarise(median = median(rir_women, na.rm = TRUE),
            lo95   = quantile(rir_women, 0.025, na.rm = TRUE),
            hi95   = quantile(rir_women, 0.975, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(median) %>%
  mutate(location = factor(location, levels = location))

rir.panel <- ggplot(rir.by.msa, aes(x = location, y = median)) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = PAL.FIT) +
  geom_errorbar(aes(ymin = lo95, ymax = hi95), width = 0.25, colour = PAL.POINT) +
  geom_point(size = 3, colour = PAL.POINT) +
  labs(x = NULL,
       y = "Women-to-MSM Relative Impact Ratio",   # the definition is in Figure 5's row title
       title = "A",
       subtitle = paste0("Projected in ", EVAL.YEAR, ", median and 95% interval")) +
  theme_minimal(base_size = 10) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Figure 5, panel A
fig5.pA <- rir.panel

# Figure 5, panels D, E, F, ...: one panel per characteristic (A-C are the top row)
driver.panels <- lapply(seq_along(strong.DRIVERS), function(i)
  panel(names(strong.DRIVERS)[i], LETTERS[3 + i]))

# y-axis title only on the left column (panels 1, 4, 7, ...); the others share it
for (i in seq_along(driver.panels))
  if ((i - 1) %% N.COL != 0)
    driver.panels[[i]] <- driver.panels[[i]] + theme(axis.title.y = element_blank())

# Figure 5 is put together and saved in section 10, after panels B and C (sections 8 and 9)


# ---- 8. POPULATION-LEVEL IMPACT AGAINST THE RIRs ----
# How much the reduction in the total population depends on the impact among women relative to
# MSM. Builds Figure 5 panel B (% reduction in incidence, total population, 2030, against the
# women-to-MSM RIR; one point per MSA) and a rho table for both RIRs.
# Point = MSA median of each axis; lines = 2.5th to 97.5th percentile across simulations on each
# axis. Dashed line = least-squares fit of the medians, to guide the eye only. Subtitle = Spearman
# rho between the MSA medians. The table also gives rho within each simulation across the ten MSAs.

# 8a. % reduction in incidence in the total population, within each simulation (MSA x simulation)
total.inc.2030.without <- sim_values(total.raw, "incidence", "noint",  EVAL.YEAR)
total.inc.2030.with    <- sim_values(total.raw, "incidence", COVERAGE, EVAL.YEAR)
reduction.total.inc    <- 100 * (total.inc.2030.without - total.inc.2030.with) / total.inc.2030.without

# same row order as `sims` (MSA fastest, then simulation)
pop.sims <- sims %>% mutate(reduction_total = as.vector(reduction.total.inc))

# 8b. Median and 95% interval of each axis, per MSA
pop.by.msa <- pop.sims %>%
  group_by(location) %>%
  summarise(across(c(reduction_total, rir_women, rir_hetmale),
                   list(median = ~ median(.x, na.rm = TRUE),
                        lo95   = ~ quantile(.x, 0.025, na.rm = TRUE),
                        hi95   = ~ quantile(.x, 0.975, na.rm = TRUE))),
            .groups = "drop")

# 8c. Spearman rho: between the MSA medians, and within each simulation across the ten MSAs
pop.by.sim <- split(pop.sims, pop.sims$sim)
pop_rho_row <- function(rir.col, rir.label) {
  data.frame("Outcome"                               = "Reduction in incidence, total population (%), 2030",
             "RIR"                                   = rir.label,
             "Spearman rho, MSA medians"             = round(spearman(pop.by.msa[[paste0(rir.col, "_median")]],
                                                                      pop.by.msa$reduction_total_median), 2),
             "Spearman rho within simulations [95%]" = show(sapply(pop.by.sim, function(d)
                                                          spearman(d[[rir.col]], d$reduction_total)), 2),
             check.names = FALSE)
}
pop.rho <- rbind(pop_rho_row("rir_women",   "Women-to-MSM RIR"),
                 pop_rho_row("rir_hetmale", "Heterosexual men-to-MSM RIR"))

# The table starts with two description rows, then the results
pop.rho.file <- file.path(TABLE.DIR, paste0("4-population_reduction_vs_RIR_at_", COV.LABEL, "_doxy.csv"))
con <- file(pop.rho.file, "w")
writeLines(c(
  paste0('"# Spearman rho between the % reduction in incidence in the total population (2030, ',
         COVERAGE, ' vs no Doxy-PEP) and each RIR (% reduction in that group / % reduction among MSM)."'),
  paste0('"# MSA medians: rho across the 10 MSA medians. Within simulations: rho across the 10 MSAs in each ',
         'simulation, shown as median [2.5th to 97.5th percentile]; simulations are not paired across MSAs."')),
  con)
write.csv(pop.rho, con, row.names = FALSE)
close(con)
pop.rho
message(PAIRING.NOTE)

# 8d. The figure
pop_panel <- function(rir.col, x.lab, letter) {
  d <- data.frame(location = pop.by.msa$location,
                  x    = pop.by.msa[[paste0(rir.col, "_median")]],
                  x.lo = pop.by.msa[[paste0(rir.col, "_lo95")]],
                  x.hi = pop.by.msa[[paste0(rir.col, "_hi95")]],
                  y    = pop.by.msa$reduction_total_median,
                  y.lo = pop.by.msa$reduction_total_lo95,
                  y.hi = pop.by.msa$reduction_total_hi95)
  rho <- spearman(d$x, d$y)
  ggplot(d, aes(x = x, y = y)) +
    geom_linerange(aes(ymin = y.lo, ymax = y.hi), colour = PAL.POINT, alpha = 0.5) +
    geom_linerange(aes(xmin = x.lo, xmax = x.hi), colour = PAL.POINT, alpha = 0.5) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", colour = PAL.FIT) +
    geom_point(size = 3, colour = PAL.POINT) +
    ggrepel::geom_text_repel(aes(label = location), size = 3) +
    labs(x = x.lab, y = paste0("Reduction in incidence, total population (%), ", EVAL.YEAR),
         title = letter, subtitle = paste0("Spearman rho = ", round(rho, 2))) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"))
}

# Figure 5, panel B
fig5.pB <- pop_panel("rir_women", "Women-to-MSM Relative Impact Ratio, 2030", "B")


# ---- 9. HETEROSEXUAL MEN-TO-MSM RIR AGAINST WOMEN-TO-MSM RIR ----
# Are the two RIRs separate signals? Heterosexual men are infected mostly through women, and both
# RIRs share the same denominator (the % reduction among MSM), so they may move together.
# One point per MSA: x = women-to-MSM RIR, y = heterosexual men-to-MSM RIR, at the MSA medians;
# lines = 2.5th to 97.5th percentile across simulations on each axis. Dashed line = least-squares
# fit of the medians (guide only). Dotted line = y = x (the same RIR in both groups).
# Subtitle: Spearman rho between the MSA medians, and within each simulation across the ten MSAs
# as median [95% interval].

# 9a. Median and 95% interval of each RIR, per MSA
rir.both.by.msa <- sims %>%
  group_by(location) %>%
  summarise(across(c(rir_women, rir_hetmale),
                   list(median = ~ median(.x, na.rm = TRUE),
                        lo95   = ~ quantile(.x, 0.025, na.rm = TRUE),
                        hi95   = ~ quantile(.x, 0.975, na.rm = TRUE))),
            .groups = "drop")

# 9b. Spearman rho: between the MSA medians, and within each simulation
rho.rir.medians <- spearman(rir.both.by.msa$rir_women_median, rir.both.by.msa$rir_hetmale_median)
rho.rir.sims    <- show(sapply(split(sims, sims$sim), function(d) spearman(d$rir_women, d$rir_hetmale)), 2)
message("Spearman rho, RIR heterosexual men vs RIR women: MSA medians = ", round(rho.rir.medians, 2),
        "; within simulations = ", rho.rir.sims)
message(PAIRING.NOTE)

# 9c. The figure
rir.scatter <- ggplot(rir.both.by.msa, aes(x = rir_women_median, y = rir_hetmale_median)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = PAL.FIT) +
  geom_linerange(aes(ymin = rir_hetmale_lo95, ymax = rir_hetmale_hi95), colour = PAL.POINT, alpha = 0.5) +
  geom_linerange(aes(xmin = rir_women_lo95,   xmax = rir_women_hi95),   colour = PAL.POINT, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", colour = PAL.FIT) +
  geom_point(size = 3, colour = PAL.POINT) +
  ggrepel::geom_text_repel(aes(label = location), size = 3) +
  labs(x = paste0("Women-to-MSM Relative Impact Ratio, ", EVAL.YEAR),
       y = paste0("Heterosexual men-to-MSM Relative Impact Ratio, ", EVAL.YEAR),
       subtitle = paste0("Spearman rho = ", round(rho.rir.medians, 2), " (MSA medians); ",
                         rho.rir.sims, " (within simulations)")) +
  theme_minimal(base_size = 10)

# Figure 5, panel C
fig5.pC <- rir.scatter + labs(title = "C") + theme(plot.title = element_text(face = "bold"))


# ---- 10. FIGURE 5 ----
# Top row, left to right:
#   A. women RIR in each MSA, median and 95% interval                 (section 7)
#   B. % reduction in total-population incidence vs women RIR         (section 8)
#   C. heterosexual men RIR vs women RIR                               (section 9)
# Following rows: women RIR vs each baseline characteristic with |rho| > RHO.CUTOFF, D, E, F, ...
#   (section 7). The cutoff is on the size of rho, not a significance test.
# Each row has a title; the first one defines the RIR
row_title <- function(text)
  wrap_elements(full = grid::textGrob(text, x = 0, hjust = 0,
                                      gp = grid::gpar(fontsize = 11, fontface = "bold", col = "black")))

n.rows.drivers <- ceiling(length(driver.panels) / N.COL)

fig5 <- row_title(paste0("Population-level impact and the women-to-MSM Relative Impact Ratio (RIR)\n",
                         "RIR = % reduction in incidence among women \u00F7 % reduction among MSM (",
                         EVAL.YEAR, ", with vs. without Doxy-PEP)")) /
        (fig5.pA | fig5.pB | fig5.pC) /
        row_title(paste0("Baseline characteristics associated with the women-to-MSM RIR (|Spearman rho| > ",
                         RHO.CUTOFF, ")")) /
        wrap_plots(driver.panels, ncol = N.COL) +
        plot_layout(heights = c(0.12, 1.2, 0.08, n.rows.drivers))
fig5

# 1 inch for the row titles, 5 for the top row, 4 per row of characteristics
ggsave(file.path(FIG.DIR, paste0("Fig5-impact_ratio_vs_drivers_at_", COV.LABEL, "_doxy.png")), fig5,
       width = 15, height = 1 + 5 + 4 * n.rows.drivers, dpi = 300, bg = "white")
