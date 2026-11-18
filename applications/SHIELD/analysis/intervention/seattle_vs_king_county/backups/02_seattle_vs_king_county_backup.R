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
