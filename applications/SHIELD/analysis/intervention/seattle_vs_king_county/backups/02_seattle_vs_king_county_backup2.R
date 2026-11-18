# ****************************************************************************************************
# 02 SEATTLE (SHIELD, KING COUNTY-ALIGNED SCENARIOS) VS THE KING COUNTY DOXY-PEP ITS ----
# ****************************************************************************************************
# Compares SHIELD's Seattle MSA (C.42660) with the population-level effect of doxy-PEP reported for
# King County, WA, by Menza et al. (Clin Infect Dis 2026;83:267-73, PMID 41885610,
# doi:10.1093/cid/ciag209). Reads ONLY this analysis's own results file from step 01
# (seattle.kc.results); never the main arrays or `intervention.results`.
#
# OUTCOME
#   Total syphilis diagnoses, ALL stages (`diagnosis.total`), as in the paper.
#   Groups: total population | all men (MSM + heterosexual men) | women.
#   Paper strata: total (incl. TNB) | cisgender men | cisgender women. SHIELD has no TNB stratum.
#
# TWO ESTIMANDS (each computed within every simulation, then median [2.5th to 97.5th percentile])
#   A. Counterfactual (= the paper's ITS estimand)
#        cumulative diagnoses in the post window, scenario vs no doxy-PEP:
#        rel.diff = 100 * (D_scenario - D_noint) / D_noint ; abs.diff = D_scenario - D_noint
#   C. The paper's ITS method applied to SHIELD (section 3b)
#        counterfactual = SHIELD's 2021-2022 linear trend in diagnoses, extended forward;
#        rel.diff = 100 * (D_scenario - D_trend) / D_trend. Comparable to the paper's estimate.
#        artifact = 100 * (D_noint - D_trend) / D_trend: how much the ITS method alone adds.
#   B. Pre-post (within each scenario)
#        % change in mean MONTHLY diagnoses, post window vs pre window.
#        Paper side: crude observed change, from the reported pre and post counts.
#
# TIMING
#   Paper : doxy-PEP from March 2023 (SHC, primary) or June 2023 (guidelines, secondary).
#   SHIELD: doxy-PEP from January 2023; KC scenarios reach 15% at mid-2023 and 30% or 45% by May 2024.
#   Annual model outputs are turned into the paper's monthly windows with year weights (fraction of
#   each calendar year inside the window). Assumes the value for year y = diagnoses during calendar
#   year y (check 2b below), spread evenly within the year.
#
# GEOGRAPHY
#   Seattle MSA = King + Pierce + Snohomish counties; the paper is King County only. Compare RELATIVE
#   differences. Absolute differences are printed for completeness only.
#
# PAIRING
#   One MSA only: simulation i with and without doxy-PEP share the same parameters; no 95% interval
#   here combines MSAs.
#
# OUTPUT (in <root>/shield/outputs/<CALIB.NAME>/seattle_vs_king_county/)
#   tables/kc-1_counterfactual.csv        estimand A, model vs paper
#   tables/kc-2_pre_post.csv              estimand B, model vs paper (crude)
#   tables/kc-4_its_on_model.csv          estimand C (paper's ITS method on SHIELD output) vs A vs paper
#   figures/kc-2_its_on_model.png         estimands A and C by scenario, with the paper's estimate as a band
#   figures/kc-3_diagnoses_over_time.png  annual diagnoses by group: no doxy-PEP, extended 2021-22 trend, scenarios
#   tables/kc-3_doxy_coverage.csv         share of MSM aged 15-64 on doxy-PEP the model reached, by year and window
#   figures/kc-1_counterfactual.png       estimand A by scenario, with the paper's estimate as a band
# ****************************************************************************************************


# ---- SETUP ----
source('../jheem_analyses/applications/SHIELD/analysis/intervention/seattle_vs_king_county/00_settings_seattle_kc.R')

if (!file.exists(KC.RESULTS.FILE))
  stop("No results file:\n  ", KC.RESULTS.FILE, "\nRun 01_extract_seattle_kc.R first.")
if (!exists("seattle.kc.results") ||
    !identical(attr(seattle.kc.results, "calibration.code"), CALIB.NAME))
  load(KC.RESULTS.FILE)   # creates seattle.kc.results

KC.TOTAL <- seattle.kc.results$total   # year x sim x outcome x intervention
KC.SEX   <- seattle.kc.results$sex     # year x sex x sim x outcome x intervention

missing.int <- setdiff(INTERVENTION.CODES, dimnames(KC.TOTAL)$intervention)
if (length(missing.int) > 0)
  stop("Not in the results file: ", paste(missing.int, collapse = ", "), ". Re-run 01_extract_seattle_kc.R.")

OUTCOME <- "diagnosis.total"   # all stages, as in the paper

# Model groups: name -> sex levels summed
GROUPS <- list(
  total   = c("msm", "heterosexual_male", "female"),
  men     = c("msm", "heterosexual_male"),
  women   = "female",
  msm     = "msm",                 # extra: no paper comparator (paper could not isolate MSM)
  hetmale = "heterosexual_male"    # extra: no paper comparator
)
GROUP.LABELS <- c(total = "Total population", men = "All men (MSM + heterosexual men)",
                  women = "Women", msm = "MSM", hetmale = "Heterosexual men")
PAPER.GROUPS <- c("total", "men", "women")


# ---- WINDOWS ----
# Year weights = fraction of each calendar year inside the window. months = 12 * sum(weights).
# Pre windows start in February 2018, the first month with model-based estimates in the paper
# (the reported pre-period counts are consistent with Feb 2018 starts; the text says Jan 2018/2017).
WINDOWS <- list(
  primary = list(
    label = "Primary: post Mar 2023-Jun 2025 (28 mo), pre Feb 2018-Feb 2023 (61 mo)",
    post  = c("2023" = 10/12, "2024" = 1, "2025" = 6/12),
    pre   = c("2018" = 11/12, "2019" = 1, "2020" = 1, "2021" = 1, "2022" = 1, "2023" = 2/12)),
  secondary = list(
    label = "Secondary: post Jun 2023-Jun 2025 (25 mo), pre Feb 2018-May 2023 (64 mo)",
    post  = c("2023" = 7/12, "2024" = 1, "2025" = 6/12),
    pre   = c("2018" = 11/12, "2019" = 1, "2020" = 1, "2021" = 1, "2022" = 1, "2023" = 5/12)),
  model.start = list(
    label = "Model start: post Jan 2023-Jun 2025 (30 mo), pre Feb 2018-Dec 2022 (59 mo)",
    post  = c("2023" = 1, "2024" = 1, "2025" = 6/12),
    pre   = c("2018" = 11/12, "2019" = 1, "2020" = 1, "2021" = 1, "2022" = 1))
)

PAIRING.NOTE <- paste0(
  "NOTE on simulation pairing: simulations are paired within the Seattle MSA across scenarios ",
  "(sim i with and without Doxy-PEP share the same parameters). Only one MSA is used here, so ",
  "no 95% interval combines MSAs.")


# ---- PAPER ESTIMATES (Menza et al. 2026, PMID 41885610) ----
# Counts as reported. rel.diff.recalc = 100 * (observed - counterfactual) / counterfactual.
# Two reported values do not reconcile with the reported counts:
#   primary total  : -3031 / 5952 = -50.9%; reported -52.3% (its CI implies a denominator of 5795)
#   secondary women: -728 / 1468  = -49.6%; reported -46.9% (its CI is centred on -49.6%)
PAPER <- tibble::tribble(
  ~window,     ~group,  ~pre.obs, ~post.obs, ~post.cf, ~abs.diff, ~abs.lo, ~abs.hi, ~rel.diff, ~rel.lo, ~rel.hi,
  "primary",   "total",     6135,      2921,     5952,     -3031,   -4295,   -1768,     -52.3,   -74.1,   -30.5,
  "primary",   "men",       5083,      1985,     4233,     -2248,   -3321,   -1175,     -53.1,   -78.5,   -27.8,
  "primary",   "women",      932,       855,     1610,      -755,    -939,    -572,     -46.9,   -58.3,   -35.5,
  "primary",   "tnb",        160,       113,      169,       -56,     -90,     -23,     -33.1,   -53.3,   -13.6,
  "secondary", "total",     6511,      2501,     4371,     -1870,   -3260,    -479,     -42.8,   -74.6,   -11.0,
  "secondary", "men",       5337,      1694,     2927,     -1233,   -2305,    -162,     -42.1,   -78.7,    -5.5,
  "secondary", "women",     1043,       740,     1468,      -728,    -876,    -580,     -46.9,   -59.7,   -39.5,
  "secondary", "tnb",        171,       100,      147,       -47,     -79,     -14,     -32.0,   -53.7,    -9.5
) %>%
  mutate(
    months.post     = sapply(window, function(w) 12 * sum(WINDOWS[[w]]$post)),
    months.pre      = sapply(window, function(w) 12 * sum(WINDOWS[[w]]$pre)),
    rel.diff.recalc = 100 * (post.obs - post.cf) / post.cf,
    pre.post.pct    = 100 * (post.obs / months.post - pre.obs / months.pre) / (pre.obs / months.pre)
  )

# Arithmetic checks on the transcription (stop if a number was mistyped)
stopifnot(with(PAPER, all(post.obs - post.cf == abs.diff)))
stopifnot(all(abs(PAPER$months.post - c(rep(28, 4), rep(25, 4))) < 1e-9))


# ---- 1. HELPERS ----
# year x sim matrix of one outcome, one intervention, summed over the requested sex levels
year_by_sim <- function(intervention, years, sexes = NULL, outcome = OUTCOME) {
  if (is.null(sexes)) {
    x <- KC.TOTAL[years, , outcome, intervention, drop = FALSE]
  } else {
    x <- KC.SEX[years, sexes, , outcome, intervention, drop = FALSE]
  }
  apply(x, c("year", "sim"), sum)
}

# Weighted sum over a window (weights named by year): one value per simulation
window_sum <- function(intervention, weights, sexes = NULL, outcome = OUTCOME) {
  m <- year_by_sim(intervention, names(weights), sexes, outcome)
  colSums(m[names(weights), , drop = FALSE] * weights)
}

# "median [2.5th to 97.5th percentile]"
show <- function(x, d = 1) {
  q <- round(quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE), d)
  paste0(q[1], " [", q[2], " to ", q[3], "]")
}

# Writes a table with description rows on top
write_described_csv <- function(df, file, description) {
  con <- file(file, "w")
  writeLines(paste0('"# ', description, '"'), con)
  write.csv(df, con, row.names = FALSE)
  close(con)
}

scenario_label <- function(code) unname(SCENARIO.LABELS[code])


# ---- 2. SANITY CHECKS ----
# 2a. The sex strata add up to the total-population diagnoses
chk.total <- window_sum(NOINT, WINDOWS$primary$post)
chk.sex   <- window_sum(NOINT, WINDOWS$primary$post, GROUPS$total)
max.rel   <- max(abs(chk.sex - chk.total) / chk.total, na.rm = TRUE)
if (max.rel > 1e-6)
  warning("Sex strata do not add up to total diagnoses (largest relative difference ", signif(max.rel, 3), ").")

# 2b. Year convention: every scenario must equal noint before 2023
pre.years <- as.character(2018:2022)
for (sc in SCENARIOS) {
  gap <- year_by_sim(sc, pre.years) - year_by_sim(NOINT, pre.years)
  if (max(abs(gap), na.rm = TRUE) > 1e-6)
    warning(sc, ": diagnoses differ from no doxy-PEP before 2023 -- check the year convention.")
}

# 2c. Diagnoses per year, no doxy-PEP, for reference (median across sims)
cat("\nSeattle MSA, total diagnoses (all stages), no doxy-PEP, median across simulations:\n")
print(round(apply(year_by_sim(NOINT, as.character(2017:2026)), 1, median)))


# ---- 3. ESTIMAND A: COUNTERFACTUAL (SCENARIO VS NO DOXY-PEP, POST WINDOW) ----
cf.long <- list()
for (w in names(WINDOWS)) {
  for (g in names(GROUPS)) {
    d0 <- window_sum(NOINT, WINDOWS[[w]]$post, GROUPS[[g]])
    for (sc in SCENARIOS) {
      d1 <- window_sum(sc, WINDOWS[[w]]$post, GROUPS[[g]])
      cf.long[[length(cf.long) + 1]] <- tibble(
        window = w, group = g, scenario = sc,
        sim = seq_along(d0), d.noint = d0, d.doxy = d1,
        abs.diff = d1 - d0, rel.diff = 100 * (d1 - d0) / d0)
    }
  }
}
cf.long <- bind_rows(cf.long)

cf.summary <- cf.long %>%
  group_by(window, group, scenario) %>%
  summarise(rel.med = median(rel.diff, na.rm = TRUE),
            rel.lo  = quantile(rel.diff, 0.025, na.rm = TRUE),
            rel.hi  = quantile(rel.diff, 0.975, na.rm = TRUE),
            model.rel.diff = show(rel.diff, 1),
            model.abs.diff = show(abs.diff, 0),
            model.noint.dx = show(d.noint, 0),
            .groups = "drop")

cf.table <- cf.summary %>%
  left_join(PAPER %>% select(window, group, p.rel = rel.diff, p.lo = rel.lo, p.hi = rel.hi,
                             p.recalc = rel.diff.recalc, p.abs = abs.diff),
            by = c("window", "group")) %>%
  mutate(paper.in.model.95 = ifelse(is.na(p.rel), NA, p.rel >= rel.lo & p.rel <= rel.hi),
         window = sapply(window, function(w) WINDOWS[[w]]$label),
         group  = GROUP.LABELS[group],
         scenario.label = scenario_label(scenario),
         paper.rel.diff = ifelse(is.na(p.rel), NA, paste0(p.rel, " [", p.lo, " to ", p.hi, "]")),
         paper.rel.diff.recalc = round(p.recalc, 1),
         paper.abs.diff.king.county = p.abs) %>%
  arrange(window, group, match(scenario, SCENARIOS)) %>%
  select(window, group, scenario, scenario.label, model.rel.diff, paper.rel.diff,
         paper.rel.diff.recalc, paper.in.model.95, model.abs.diff, paper.abs.diff.king.county,
         model.noint.dx)

write_described_csv(
  cf.table,
  file.path(KC.TABLE.DIR, "kc-1_counterfactual.csv"),
  c(paste0("Seattle MSA (SHIELD, ", CALIB.NAME, ", ", N.SIM, " paired sims) vs King County ITS (Menza et al. 2026, PMID 41885610):",
           " relative difference (%) in cumulative all-stage syphilis diagnoses in the post window, scenario vs no doxy-PEP. Negative = fewer diagnoses."),
    paste0("Columns: model.rel.diff / model.abs.diff / model.noint.dx = SHIELD median [2.5th to 97.5th percentile];",
           " paper.rel.diff = reported estimate [95% CI]; paper.rel.diff.recalc = (observed - counterfactual)/counterfactual from reported counts;",
           " paper.in.model.95 = paper point estimate inside the model 95% interval. Paper 'All men' = cisgender men; paper total includes TNB.",
           " Absolute differences are not comparable (MSA vs county).")))

cat("\nPrimary window: SHIELD vs King County, relative difference in diagnoses (%):\n")
print(cf.table %>%
        filter(grepl("^Primary", window), group %in% GROUP.LABELS[PAPER.GROUPS]) %>%
        select(group, scenario.label, model.rel.diff, paper.rel.diff, paper.in.model.95),
      n = Inf)


# ---- 3b. ESTIMAND C: THE PAPER'S ITS METHOD APPLIED TO SHIELD ----
# Counterfactual = linear trend through SHIELD's 2021 and 2022 annual diagnoses, extended forward.
# Mirrors the paper's "pre-implementation trend persists" counterfactual (paper: linear trend over
# monthly diagnoses Jan 2021-Feb 2023). Pre-2023 values are identical in every scenario (check 2b),
# so the trend is the same whichever simset it is read from. Annual values are placed at mid-year:
#   trend(y) = Dx2022 + (y - 2022) * (Dx2022 - Dx2021)
# then summed over the post window with the same year weights as estimand A.
# Within each simulation:
#   C.its    = 100 * (D_doxy  - D_trend) / D_trend   what the paper's method would report on SHIELD output
#   A.model  = 100 * (D_doxy  - D_noint) / D_noint   true model effect (estimand A)
#   artifact = 100 * (D_noint - D_trend) / D_trend   no-doxy epidemic vs the extended trend
#                                                     (negative = growth slows below the 2021-22 line)
# Identity: (1 + C/100) = (1 + A/100) * (1 + artifact/100)
# Limitation: the slope rests on 2 annual points (the paper used 26 monthly points); the 95%
# intervals carry that noise because everything is computed within each simulation.
TREND.YEARS <- c("2021", "2022")

# Paper pre-implementation trends, Jan 2021 to the interruption (cases/month), for reference
PAPER.SLOPES <- tibble::tribble(
  ~window,     ~group,  ~paper.slope,
  "primary",   "total", "3.1 [2.7 to 3.5]",
  "primary",   "men",   "2.0 [1.6 to 2.3]",
  "primary",   "women", "1.0 [0.9 to 1.1]",
  "secondary", "total", "2.9 [2.5 to 3.3]",
  "secondary", "men",   "1.8 [1.4 to 2.1]",
  "secondary", "women", "1.0 [0.9 to 1.1]"
)

# Weighted sum of the extended trend over a window: one value per simulation
trend_window_sum <- function(weights, sexes) {
  m     <- year_by_sim(NOINT, TREND.YEARS, sexes)
  slope <- m["2022", ] - m["2021", ]
  yrs   <- as.numeric(names(weights))
  trend <- matrix(sapply(yrs, function(y) m["2022", ] + (y - 2022) * slope), ncol = length(yrs))
  as.numeric(trend %*% weights)
}

its.long <- list()
for (w in names(WINDOWS)) {
  wts <- WINDOWS[[w]]$post
  for (g in names(GROUPS)) {
    m      <- year_by_sim(NOINT, TREND.YEARS, GROUPS[[g]])
    slope  <- (m["2022", ] - m["2021", ]) / 12     # cases per month, as in the paper
    growth <- 100 * (m["2022", ] - m["2021", ]) / m["2021", ]
    d.tr   <- trend_window_sum(wts, GROUPS[[g]])
    d0     <- window_sum(NOINT, wts, GROUPS[[g]])
    if (any(d.tr <= 0, na.rm = TRUE))
      warning(w, " / ", g, ": extended trend is <= 0 in ", sum(d.tr <= 0, na.rm = TRUE), " simulations")
    for (sc in SCENARIOS) {
      d1 <- window_sum(sc, wts, GROUPS[[g]])
      its.long[[length(its.long) + 1]] <- tibble(
        window = w, group = g, scenario = sc, sim = seq_along(d0),
        C.its    = 100 * (d1 - d.tr) / d.tr,
        A.model  = 100 * (d1 - d0)   / d0,
        artifact = 100 * (d0 - d.tr) / d.tr,
        slope = slope, growth = growth)
    }
  }
}
its.long <- bind_rows(its.long)

# The identity must hold in every simulation
stopifnot(max(abs((1 + its.long$C.its / 100) -
                  (1 + its.long$A.model / 100) * (1 + its.long$artifact / 100)), na.rm = TRUE) < 1e-8)

its.summary <- its.long %>%
  group_by(window, group, scenario) %>%
  summarise(C.med = median(C.its, na.rm = TRUE),
            C.lo  = quantile(C.its, 0.025, na.rm = TRUE),
            C.hi  = quantile(C.its, 0.975, na.rm = TRUE),
            A.med = median(A.model, na.rm = TRUE),
            A.lo  = quantile(A.model, 0.025, na.rm = TRUE),
            A.hi  = quantile(A.model, 0.975, na.rm = TRUE),
            model.its.rel.diff   = show(C.its, 1),
            model.rel.diff       = show(A.model, 1),
            trend.artifact       = show(artifact, 1),
            model.pre.slope      = show(slope, 1),
            model.growth.2021.22 = show(growth, 1),
            .groups = "drop")

its.table <- its.summary %>%
  left_join(PAPER %>% select(window, group, p.rel = rel.diff, p.lo = rel.lo, p.hi = rel.hi),
            by = c("window", "group")) %>%
  left_join(PAPER.SLOPES, by = c("window", "group")) %>%
  mutate(its.overlaps.paper   = ifelse(is.na(p.rel), NA, pmax(C.lo, p.lo) <= pmin(C.hi, p.hi)),
         model.overlaps.paper = ifelse(is.na(p.rel), NA, pmax(A.lo, p.lo) <= pmin(A.hi, p.hi)),
         paper.rel.diff = ifelse(is.na(p.rel), NA, paste0(p.rel, " [", p.lo, " to ", p.hi, "]")),
         window = sapply(window, function(w) WINDOWS[[w]]$label),
         group  = GROUP.LABELS[group],
         scenario.label = scenario_label(scenario)) %>%
  arrange(window, group, match(scenario, SCENARIOS)) %>%
  select(window, group, scenario, scenario.label,
         model.its.rel.diff, model.rel.diff, trend.artifact, paper.rel.diff,
         its.overlaps.paper, model.overlaps.paper,
         model.pre.slope, paper.pre.slope = paper.slope, model.growth.2021.22)

write_described_csv(
  its.table,
  file.path(KC.TABLE.DIR, "kc-4_its_on_model.csv"),
  c(paste0("The paper's ITS counterfactual applied to SHIELD Seattle: relative difference (%) in cumulative all-stage diagnoses in the post window.",
           " model.its.rel.diff = scenario vs SHIELD's own 2021-2022 linear trend extended forward (estimand C, comparable to the paper);",
           " model.rel.diff = scenario vs SHIELD no doxy-PEP (estimand A, the true model effect);",
           " trend.artifact = SHIELD no doxy-PEP vs the extended trend (negative = the no-doxy epidemic grows more slowly than the 2021-22 line, which the ITS method counts as doxy-PEP effect)."),
    paste0("All model values: median [2.5th to 97.5th percentile] across ", N.SIM, " paired simulations; (1+C) = (1+A)(1+artifact) within each simulation.",
           " *.overlaps.paper = the model 95% interval overlaps the paper 95% CI. model.pre.slope = SHIELD 2021-22 change in diagnoses, cases/month (Seattle MSA);",
           " paper.pre.slope = King County Jan 2021 to interruption, cases/month (county; compare direction, not size); model.growth.2021.22 = % change 2021 to 2022.")))

cat("\nPrimary window: paper's ITS method applied to SHIELD (C) vs true model effect (A) vs King County (%):\n")
print(its.table %>%
        filter(grepl("^Primary", window), group %in% GROUP.LABELS[PAPER.GROUPS]) %>%
        select(group, scenario.label, model.its.rel.diff, model.rel.diff, trend.artifact, paper.rel.diff),
      n = Inf, width = Inf)

# Figure: A and C side by side for each scenario, with the paper's estimate as a band (primary window)
fig.its <- its.summary %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  select(group, scenario, C.med, C.lo, C.hi, A.med, A.lo, A.hi) %>%
  pivot_longer(-c(group, scenario), names_to = c("estimand", ".value"), names_sep = "\\.") %>%
  mutate(estimand = factor(estimand, levels = c("A", "C"),
                           labels = c("vs SHIELD no doxy-PEP (model effect)",
                                      "vs SHIELD 2021-22 trend (paper's ITS method)")),
         group    = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]),
         scenario = factor(scenario_label(scenario), levels = scenario_label(SCENARIOS)))
fig.its.paper <- PAPER %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  mutate(group = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]))

fig.its.plot <- ggplot(fig.its, aes(x = scenario, y = med, colour = estimand)) +
  geom_rect(data = fig.its.paper, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = rel.lo, ymax = rel.hi), fill = "grey70", alpha = 0.35) +
  geom_hline(data = fig.its.paper, aes(yintercept = rel.diff), linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.35, position = position_dodge(width = 0.5)) +
  facet_wrap(~ group, nrow = 1) +
  scale_colour_manual(values = c("grey20", "#1f78b4"), name = NULL) +
  labs(x = NULL,
       y = "Relative difference in all-stage diagnoses,\nMar 2023-Jun 2025 (%)",
       title = "SHIELD Seattle: true model effect vs the paper's ITS method applied to model output",
       caption = paste0("Points and bars: SHIELD median and 95% interval (", N.SIM, " paired simulations). ",
                        "Dashed line and band: King County ITS, Menza et al. 2026 (PMID 41885610), primary analysis.\n",
                        "ITS counterfactual on SHIELD: linear trend through 2021 and 2022 annual diagnoses, extended forward.")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        axis.text.x  = element_text(angle = 35, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 8))

ggsave(file.path(KC.FIG.DIR, "kc-2_its_on_model.png"), fig.its.plot, width = 11, height = 5.5, dpi = 300)
fig.its.plot


# ---- 3c. FIGURE: DIAGNOSES OVER TIME, NO DOXY-PEP VS EXTENDED 2021-22 TREND VS SCENARIOS ----
# Annual all-stage diagnoses in the Seattle MSA by group, 2017 to TRAJ.LAST.YEAR:
#   no doxy-PEP (median, 95% band), the ITS-style counterfactual (SHIELD's 2021-2022 linear trend
#   extended forward, median, 95% band), and each doxy-PEP scenario (median line).
# Each annual value is drawn at mid-year (year + 0.5), the same placement the trend uses.
# The gap between the no doxy-PEP band and the trend band after 2022 is the trend artifact (3b).
# Shaded: the paper's primary post window (Mar 2023-Jun 2025). Points: the two years the trend uses.
TRAJ.YEARS     <- as.character(2017:2030)
TRAJ.GROUPS    <- c("total", "men", "women", "msm", "hetmale")
TRAJ.SCENARIOS <- SCENARIOS

band <- function(mat, years, series, g) {          # mat: year x sim
  tibble(group = g, series = series, year = as.numeric(years) + 0.5,
         med = apply(mat, 1, median, na.rm = TRUE),
         lo  = apply(mat, 1, quantile, 0.025, na.rm = TRUE),
         hi  = apply(mat, 1, quantile, 0.975, na.rm = TRUE))
}

traj <- list()
for (g in TRAJ.GROUPS) {
  m0 <- year_by_sim(NOINT, TRAJ.YEARS, GROUPS[[g]])
  traj[[length(traj) + 1]] <- band(m0, TRAJ.YEARS, "No doxy-PEP", g)

  # Extended trend from 2021 onward, within each simulation
  tr.years <- TRAJ.YEARS[as.numeric(TRAJ.YEARS) >= 2021]
  slope    <- m0["2022", ] - m0["2021", ]
  tr       <- t(sapply(as.numeric(tr.years), function(y) m0["2022", ] + (y - 2022) * slope))
  traj[[length(traj) + 1]] <- band(tr, tr.years, "2021-22 trend extended (ITS-style)", g)

  for (sc in TRAJ.SCENARIOS)
    traj[[length(traj) + 1]] <- band(year_by_sim(sc, TRAJ.YEARS, GROUPS[[g]]), TRAJ.YEARS,
                                     scenario_label(sc), g)
}
traj <- bind_rows(traj) %>%
  mutate(group  = factor(GROUP.LABELS[group], levels = GROUP.LABELS[TRAJ.GROUPS]),
         series = factor(series, levels = c("No doxy-PEP", "2021-22 trend extended (ITS-style)",
                                            scenario_label(TRAJ.SCENARIOS))))

banded    <- traj %>% filter(series %in% c("No doxy-PEP", "2021-22 trend extended (ITS-style)"))
trend.pts <- traj %>% filter(series == "No doxy-PEP", year %in% (as.numeric(TREND.YEARS) + 0.5))

SERIES.COLOURS <- c("No doxy-PEP" = "grey20", "2021-22 trend extended (ITS-style)" = "#e31a1c",
                    setNames(c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00",
                               "#cab2d6", "#6a3d9a", "#fb9a99", "#b15928", "#ffff99", "#8dd3c7",
                               "#bebada", "#80b1d3")[seq_along(TRAJ.SCENARIOS)],
                             scenario_label(TRAJ.SCENARIOS)))
SERIES.LINETYPES <- c("No doxy-PEP" = "solid", "2021-22 trend extended (ITS-style)" = "dashed",
                      setNames(rep("solid", length(TRAJ.SCENARIOS)), scenario_label(TRAJ.SCENARIOS)))

traj.plot <- ggplot(traj, aes(x = year, y = med, colour = series, linetype = series)) +
  annotate("rect", xmin = 2023 + 2/12, xmax = 2025.5, ymin = -Inf, ymax = Inf,
           fill = "grey85", alpha = 0.5) +
  geom_vline(xintercept = 2023, colour = "grey40", linewidth = 0.3) +
  geom_ribbon(data = banded, aes(ymin = lo, ymax = hi, fill = series),
              colour = NA, alpha = 0.15, show.legend = FALSE) +
  geom_line(linewidth = 0.7) +
  geom_point(data = trend.pts, colour = "#e31a1c", size = 2, show.legend = FALSE) +
  facet_wrap(~ group, scales = "free_y", nrow = 2) +
  scale_colour_manual(values = SERIES.COLOURS, name = NULL) +
  scale_fill_manual(values = SERIES.COLOURS) +
  scale_linetype_manual(values = SERIES.LINETYPES, name = NULL) +
  scale_x_continuous(breaks = seq(2017, 2031, 2)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "All-stage syphilis diagnoses per year (Seattle MSA)",
       title = "SHIELD Seattle: projected diagnoses with and without doxy-PEP, and the ITS-style counterfactual",
       caption = paste0("Lines: median across ", N.SIM, " simulations; bands: 95% interval (no doxy-PEP and extended trend only). ",
                        "Annual values drawn at mid-year. Red points: 2021 and 2022, the years the trend is fitted to.\n",
                        "Vertical line: doxy-PEP starts in SHIELD (Jan 2023). Shaded: King County primary post window (Mar 2023-Jun 2025).")) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8)) +
  guides(colour = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2))

ggsave(file.path(KC.FIG.DIR, "kc-3_diagnoses_over_time.png"), traj.plot, width = 13, height = 8, dpi = 300)
traj.plot


# ---- 4. DOXY-PEP COVERAGE THE MODEL REACHED ----
# doxy.coverage = person-years of MSM aged 15-64 on doxy-PEP (eligibility = 1);
# population.msm.15.64 = person-years of MSM aged 15-64 (SEXUAL.ACTIVITY.AGES).
# Ratio = share of MSM aged 15-64 on doxy-PEP (annual average), i.e. the coverage the model applies.
# NOT population.msm: that counts MSM of all ages (incl. 0-14 and 65+) and understates coverage by ~30%.
# Compare with King County: 12% -> 45% (SHC MSM, May 2023 -> Jun 2024) and 15% -> 31% (Pride surveys, 2023 -> 2024).
COV.DENOM <- "population.msm.15.64"
if (!COV.DENOM %in% dimnames(KC.TOTAL)$outcome)
  stop("'", COV.DENOM, "' is not in the results file. Re-run 01_extract_seattle_kc.R (it re-extracts automatically).")
# Check: MSM aged 15-64 must be fewer than all MSM
stopifnot(all(year_by_sim(NOINT, "2024", outcome = COV.DENOM) <= year_by_sim(NOINT, "2024", outcome = "population.msm")))
cat("\nShare of MSM aged 15-64 among all MSM, Seattle 2024 (median):",
    round(100 * median(year_by_sim(NOINT, "2024", outcome = COV.DENOM) /
                       year_by_sim(NOINT, "2024", outcome = "population.msm")), 1), "%\n")

cov.table <- list()
for (sc in SCENARIOS) {
  row <- tibble(scenario = sc, scenario.label = scenario_label(sc))
  for (y in c("2023", "2024", "2025", "2026")) {
    share <- year_by_sim(sc, y, outcome = "doxy.coverage") / year_by_sim(sc, y, outcome = COV.DENOM)
    row[[paste0("pct.msm.on.doxy.", y)]] <- show(100 * as.numeric(share), 1)
  }
  for (w in names(WINDOWS)) {
    wts <- WINDOWS[[w]]$post
    row[[paste0("pct.msm.on.doxy.window.", w)]] <-
      show(100 * window_sum(sc, wts, outcome = "doxy.coverage") / window_sum(sc, wts, outcome = COV.DENOM), 1)
  }
  cov.table[[length(cov.table) + 1]] <- row
}
cov.table <- bind_rows(cov.table)

write_described_csv(
  cov.table,
  file.path(KC.TABLE.DIR, "kc-3_doxy_coverage.csv"),
  c("Share of MSM aged 15-64 on doxy-PEP in SHIELD's Seattle MSA (doxy.coverage / population.msm.15.64, person-years), median [2.5th to 97.5th percentile]. This is the coverage the model applies to transmission.",
    paste0("Columns pct.msm.on.doxy.<year> = annual average in that year; pct.msm.on.doxy.window.<window> = average over that post window.",
           " King County reference: 12% -> 45% of MSM at the SHC (May 2023 -> Jun 2024); 15% -> 31% of MSM in Pride surveys (2023 -> 2024).")))
print(cov.table, width = Inf)


# ---- 5. ESTIMAND B: PRE-POST CHANGE IN MEAN MONTHLY DIAGNOSES ----
pp.long <- list()
for (w in names(WINDOWS)) {
  m.pre  <- 12 * sum(WINDOWS[[w]]$pre)
  m.post <- 12 * sum(WINDOWS[[w]]$post)
  for (g in names(GROUPS)) {
    for (int in INTERVENTION.CODES) {
      pre  <- window_sum(int, WINDOWS[[w]]$pre,  GROUPS[[g]]) / m.pre
      post <- window_sum(int, WINDOWS[[w]]$post, GROUPS[[g]]) / m.post
      pp.long[[length(pp.long) + 1]] <- tibble(window = w, group = g, scenario = int,
                                               pct.change = 100 * (post - pre) / pre)
    }
  }
}
pp.long <- bind_rows(pp.long)

pp.table <- pp.long %>%
  group_by(window, group, scenario) %>%
  summarise(model.pre.post.pct = show(pct.change, 1), .groups = "drop") %>%
  left_join(PAPER %>% select(window, group, pre.post.pct), by = c("window", "group")) %>%
  mutate(paper.crude.pre.post.pct = round(pre.post.pct, 1),
         scenario.label = scenario_label(scenario),
         group  = GROUP.LABELS[group],
         window = sapply(window, function(w) WINDOWS[[w]]$label)) %>%
  arrange(window, group, match(scenario, INTERVENTION.CODES)) %>%
  select(window, group, scenario, scenario.label, model.pre.post.pct, paper.crude.pre.post.pct)

write_described_csv(
  pp.table,
  file.path(KC.TABLE.DIR, "kc-2_pre_post.csv"),
  c("Pre-post % change in mean monthly all-stage syphilis diagnoses (post window vs pre window), within each scenario.",
    paste0("Columns: model.pre.post.pct = SHIELD median [2.5th to 97.5th percentile]; paper.crude.pre.post.pct = crude observed change",
           " from the paper's reported counts (NOT the ITS estimate). The pre window spans the flat 2018-2020 and rising 2021-2023 periods.")))


# ---- 6. FIGURE: EACH SCENARIO VS THE KING COUNTY ESTIMATE (PRIMARY WINDOW) ----
fig.model <- cf.summary %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  mutate(group    = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]),
         scenario = factor(scenario_label(scenario), levels = scenario_label(SCENARIOS)))
fig.paper <- PAPER %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  mutate(group = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]))

fig <- ggplot(fig.model, aes(x = scenario, y = rel.med)) +
  geom_rect(data = fig.paper, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = rel.lo, ymax = rel.hi),
            fill = "grey70", alpha = 0.35) +
  geom_hline(data = fig.paper, aes(yintercept = rel.diff), linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_pointrange(aes(ymin = rel.lo, ymax = rel.hi), size = 0.4) +
  facet_wrap(~ group, nrow = 1) +
  labs(x = NULL,
       y = "Relative difference in all-stage diagnoses,\nMar 2023-Jun 2025, vs no doxy-PEP (%)",
       title = "Seattle MSA (SHIELD, King County-aligned uptake) vs King County interrupted time series",
       caption = paste0("Points and bars: SHIELD median and 95% interval (", N.SIM, " paired simulations). ",
                        "Dashed line and band: Menza et al. 2026 (PMID 41885610), primary analysis.\n",
                        "Paper strata: total incl. TNB; cisgender men; cisgender women. ",
                        "Scenarios: doxy-PEP from Jan 2023, 15% at mid-2023.")) +
  theme_bw(base_size = 11) +
  theme(axis.text.x  = element_text(angle = 35, hjust = 1),
        plot.caption = element_text(hjust = 0, size = 8))

ggsave(file.path(KC.FIG.DIR, "kc-1_counterfactual.png"), fig, width = 11, height = 5, dpi = 300)
fig

message(PAIRING.NOTE)
