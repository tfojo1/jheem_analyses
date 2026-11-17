# ****************************************************************************************************
# 06 SEATTLE: SHIELD PROJECTIONS VS THE KING COUNTY DOXY-PEP INTERRUPTED TIME SERIES ----
# ****************************************************************************************************
# Compares SHIELD's Seattle MSA (C.42660) projections with the population-level effect of doxy-PEP
# reported for King County, WA, by Menza et al. (Clin Infect Dis 2026;83:267-73, PMID 41885610,
# doi:10.1093/cid/ciag209). Reads the raw arrays only (total and sex), like 04 and 05.
#
# OUTCOME
#   Total syphilis diagnoses, ALL stages (`diagnosis.total`), as in the paper.
#   Groups: total population | all men (MSM + heterosexual men) | women.
#   Paper strata: total (incl. TNB) | cisgender men | cisgender women. SHIELD has no TNB stratum.
#
# TWO ESTIMANDS (each computed within every simulation, then median [2.5th to 97.5th percentile])
#   A. Counterfactual (= the paper's ITS estimand)
#        cumulative diagnoses in the post window, doxy-PEP vs no doxy-PEP:
#        rel.diff = 100 * (D_doxy - D_noint) / D_noint ; abs.diff = D_doxy - D_noint
#   B. Pre-post (within each scenario)
#        % change in mean MONTHLY diagnoses, post window vs pre window:
#        100 * (D_post / months_post - D_pre / months_pre) / (D_pre / months_pre)
#        Paper side: crude observed change, from the reported pre and post counts.
#
# TIMING
#   Paper : doxy-PEP from March 2023 (SHC, primary) or June 2023 (guidelines, secondary).
#   SHIELD: scale-up starts January 2023 and rises LINEARLY to the target coverage in 2030
#           (intervention/intervention_definitions.R). So "doxy.cov.50" means 50% in 2030, i.e. only
#           ~9.5% on average over Mar 2023-Jun 2025 and ~18% by June 2025. King County reported
#           31-45% prior-month use among MSM by mid-2024. Section 4 reports the coverage the model
#           actually reached inside each window.
#   Annual model outputs are turned into the paper's monthly windows with year weights
#   (fraction of each calendar year inside the window). This assumes:
#     (1) the value for year y = diagnoses during calendar year y  -> CHECK against jheem2
#     (2) diagnoses are spread evenly within a year
#
# GEOGRAPHY
#   Seattle MSA = King + Pierce + Snohomish counties; the paper is King County only. Compare
#   RELATIVE differences. Absolute differences are printed for completeness only.
#
# PAIRING
#   Only one MSA is used, so simulation i with and without doxy-PEP are paired (same parameters);
#   no cross-MSA pairing issue arises here.
#
# OUTPUT (in shield.table.path / shield.fig.path for CALIB.NAME)
#   tables/6-seattle_vs_king_county_counterfactual.csv
#   tables/6-seattle_vs_king_county_pre_post.csv
#   tables/6-seattle_doxy_coverage_in_window.csv
#   figures/6-seattle_vs_king_county_counterfactual.png
# ****************************************************************************************************


# ---- SETUP ----
# source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
# source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')

FIG.DIR   <- shield.fig.path(CALIB.NAME,   create = TRUE)
TABLE.DIR <- shield.table.path(CALIB.NAME, create = TRUE)

CITY     <- "Seattle"
LOC.CODE <- if (CITY %in% names(LOCATIONS)) LOCATIONS[[CITY]] else "C.42660"
stopifnot(LOC.CODE == "C.42660")

OUTCOME <- "diagnosis.total"   # all stages, as in the paper

# Model groups: name -> sex levels summed (sex_raw levels: msm, heterosexual_male, female)
GROUPS <- list(
  total = c("msm", "heterosexual_male", "female"),
  men   = c("msm", "heterosexual_male"),
  women = "female",
  msm   = "msm",                 # extra: no paper comparator (paper could not isolate MSM)
  hetmale = "heterosexual_male"  # extra: no paper comparator
)
GROUP.LABELS <- c(total = "Total population", men = "All men (MSM + heterosexual men)",
                  women = "Women", msm = "MSM", hetmale = "Heterosexual men")
PAPER.GROUPS <- c("total", "men", "women")   # groups with a paper comparator

# ---- WINDOWS ----
# Year weights = fraction of each calendar year inside the window. months = sum(weights) * 12.
# primary   : the paper's primary windows (SHC implementation, March 2023)
# secondary : the paper's secondary windows (guideline release, June 2023)
# model.start: post window aligned to SHIELD's own start (January 2023), paper end (June 2025)
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
    months.post    = sapply(window, function(w) 12 * sum(WINDOWS[[w]]$post)),
    months.pre     = sapply(window, function(w) 12 * sum(WINDOWS[[w]]$pre)),
    rel.diff.recalc = 100 * (post.obs - post.cf) / post.cf,
    # crude observed pre-post change in mean monthly diagnoses
    pre.post.pct   = 100 * (post.obs / months.post - pre.obs / months.pre) / (pre.obs / months.pre)
  )

# Arithmetic checks on the transcription (stop if a number was mistyped)
stopifnot(with(PAPER, all(post.obs - post.cf == abs.diff)))
stopifnot(all(abs(PAPER$months.post - c(rep(28, 4), rep(25, 4))) < 1e-9))


# ---- LOAD ARRAYS ----
# Memory: loads the full total_raw and sex_raw arrays (all MSAs). Reused if 03-05 already loaded them.
if (!exists("intervention.results") ||
    !identical(attr(intervention.results, "calibration.code"), CALIB.NAME) ||
    !all(c("total_raw", "sex_raw") %in% names(intervention.results)))
  intervention.results <- load.shield.results(CALIB.NAME, which = c("total_raw", "sex_raw"))

total.raw <- intervention.results$total_raw
sex.raw   <- intervention.results$sex_raw

# Keep only Seattle, so later steps work on a small array
total.sea <- subset_array(total.raw, list(location = LOC.CODE))
sex.sea   <- subset_array(sex.raw,   list(location = LOC.CODE))
stopifnot(!all(is.na(sex.sea)))   # Seattle simsets are in the arrays


# ---- 1. HELPERS ----
# year x sim matrix of one outcome, one intervention, summed over the requested sex levels
year_by_sim <- function(intervention, years, sexes = NULL, outcome = OUTCOME) {
  if (is.null(sexes)) {
    x <- subset_array(total.sea, list(outcome = outcome, intervention = intervention, year = years))
  } else {
    x <- subset_array(sex.sea, list(outcome = outcome, intervention = intervention,
                                    year = years, sex = sexes))
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

# Writes a table with description rows on top (same as 05)
write_described_csv <- function(df, file, description) {
  con <- file(file, "w")
  writeLines(paste0('"# ', description, '"'), con)
  write.csv(df, con, row.names = FALSE)
  close(con)
}


# ---- 2. SANITY CHECKS ----
# 2a. The sex strata add up to the total-population diagnoses
chk.total <- window_sum(NOINT, WINDOWS$primary$post)
chk.sex   <- window_sum(NOINT, WINDOWS$primary$post, GROUPS$total)
max.rel   <- max(abs(chk.sex - chk.total) / chk.total, na.rm = TRUE)
if (max.rel > 1e-6)
  warning("Sex strata do not add up to total diagnoses (largest relative difference ", signif(max.rel, 3), ").")

# 2b. Year convention: doxy-PEP and no doxy-PEP must be identical before 2023
pre.gap <- year_by_sim(COVERAGE.LEVELS[length(COVERAGE.LEVELS)], as.character(2018:2022)) -
           year_by_sim(NOINT, as.character(2018:2022))
if (max(abs(pre.gap), na.rm = TRUE) > 1e-6)
  warning("Doxy-PEP and no-intervention diagnoses differ before 2023: check the year convention.")

# 2c. Diagnoses per year, no doxy-PEP, for reference (median across sims)
cat("\nSeattle MSA, total diagnoses (all stages), no doxy-PEP, median across simulations:\n")
print(round(apply(year_by_sim(NOINT, as.character(2017:2026)), 1, median)))


# ---- 3. ESTIMAND A: COUNTERFACTUAL (DOXY-PEP VS NO DOXY-PEP, POST WINDOW) ----
cf.long <- list()
for (w in names(WINDOWS)) {
  for (g in names(GROUPS)) {
    d0 <- window_sum(NOINT, WINDOWS[[w]]$post, GROUPS[[g]])
    for (cov in COVERAGE.LEVELS) {
      d1 <- window_sum(cov, WINDOWS[[w]]$post, GROUPS[[g]])
      cf.long[[length(cf.long) + 1]] <- tibble(
        window = w, group = g, coverage = cov,
        target.pct = as.numeric(sub("doxy.cov.", "", cov, fixed = TRUE)),
        sim = seq_along(d0), d.noint = d0, d.doxy = d1,
        abs.diff = d1 - d0, rel.diff = 100 * (d1 - d0) / d0)
    }
  }
}
cf.long <- bind_rows(cf.long)

cf.summary <- cf.long %>%
  group_by(window, group, coverage, target.pct) %>%
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
  mutate(group = GROUP.LABELS[group],
         window = sapply(window, function(w) WINDOWS[[w]]$label),
         paper.rel.diff = ifelse(is.na(p.rel), NA, paste0(p.rel, " [", p.lo, " to ", p.hi, "]")),
         paper.rel.diff.recalc = round(p.recalc, 1),
         paper.abs.diff.king.county = p.abs) %>%
  select(window, group, coverage.2030.target = target.pct, model.rel.diff, paper.rel.diff,
         paper.rel.diff.recalc, model.abs.diff, paper.abs.diff.king.county, model.noint.dx)

write_described_csv(
  cf.table,
  file.path(TABLE.DIR, "6-seattle_vs_king_county_counterfactual.csv"),
  c(paste0("Seattle MSA (SHIELD, ", CALIB.NAME, ") vs King County ITS (Menza et al. 2026, PMID 41885610).",
           " Relative difference (%) in cumulative all-stage syphilis diagnoses in the post window, doxy-PEP vs none."),
    paste0("Model: median [2.5th to 97.5th percentile] across ", N.SIM, " paired simulations. Coverage = 2030 target;",
           " SHIELD scales up linearly from Jan 2023. Paper 'All men' = cisgender men; paper total includes TNB.",
           " Absolute differences are not comparable (MSA vs county).")))

# Lowest target coverage whose median reduction reaches the paper's point estimate (primary window)
match.cov <- cf.summary %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  left_join(PAPER %>% filter(window == "primary") %>% select(group, paper = rel.diff), by = "group") %>%
  group_by(group) %>%
  summarise(lowest.target.matching.paper = {
              hit <- target.pct[rel.med <= paper]
              if (length(hit) == 0) "none up to 50%" else paste0(min(hit), "%")
            },
            largest.model.reduction = round(min(rel.med), 1),
            paper = first(paper), .groups = "drop")
cat("\nCoverage (2030 target) at which SHIELD's median reduction reaches the King County estimate:\n")
print(match.cov)


# ---- 4. DOXY-PEP COVERAGE THE MODEL REACHED INSIDE EACH POST WINDOW ----
# doxy.coverage = person-years of eligible MSM on doxy-PEP; population.msm = MSM person-years.
# Their ratio is the share of MSM on doxy-PEP, averaged over the window. Compare with King County:
# 12% -> 45% (SHC MSM, May 2023 -> Jun 2024) and 15% -> 31% (Pride surveys, 2023 -> 2024).
cov.table <- list()
for (w in names(WINDOWS)) {
  wts <- WINDOWS[[w]]$post
  for (cov in COVERAGE.LEVELS) {
    on.doxy <- window_sum(cov, wts, outcome = "doxy.coverage")
    msm.pop <- window_sum(cov, wts, outcome = "population.msm")
    end.yr  <- year_by_sim(cov, tail(names(wts), 1), outcome = "doxy.coverage") /
               year_by_sim(cov, tail(names(wts), 1), outcome = "population.msm")
    cov.table[[length(cov.table) + 1]] <- tibble(
      window = WINDOWS[[w]]$label,
      coverage.2030.target = as.numeric(sub("doxy.cov.", "", cov, fixed = TRUE)),
      pct.msm.on.doxy.window.mean = show(100 * on.doxy / msm.pop, 1),
      pct.msm.on.doxy.last.year   = show(100 * as.numeric(end.yr), 1))
  }
}
cov.table <- bind_rows(cov.table)

write_described_csv(
  cov.table,
  file.path(TABLE.DIR, "6-seattle_doxy_coverage_in_window.csv"),
  c("Share of MSM on doxy-PEP in SHIELD's Seattle MSA (doxy.coverage / population.msm, person-years), median [95% interval].",
    "King County reference: 12% -> 45% of MSM at the SHC (May 2023 -> Jun 2024); 15% -> 31% of MSM in Pride surveys (2023 -> 2024)."))
print(cov.table, n = Inf)


# ---- 5. ESTIMAND B: PRE-POST CHANGE IN MEAN MONTHLY DIAGNOSES ----
pp.long <- list()
for (w in names(WINDOWS)) {
  m.pre  <- 12 * sum(WINDOWS[[w]]$pre)
  m.post <- 12 * sum(WINDOWS[[w]]$post)
  for (g in names(GROUPS)) {
    for (int in c(NOINT, COVERAGE.LEVELS)) {
      pre  <- window_sum(int, WINDOWS[[w]]$pre,  GROUPS[[g]]) / m.pre
      post <- window_sum(int, WINDOWS[[w]]$post, GROUPS[[g]]) / m.post
      pp.long[[length(pp.long) + 1]] <- tibble(window = w, group = g, intervention = int,
                                               pct.change = 100 * (post - pre) / pre)
    }
  }
}
pp.long <- bind_rows(pp.long)

pp.table <- pp.long %>%
  group_by(window, group, intervention) %>%
  summarise(model.pre.post.pct = show(pct.change, 1), .groups = "drop") %>%
  left_join(PAPER %>% select(window, group, pre.post.pct), by = c("window", "group")) %>%
  mutate(paper.crude.pre.post.pct = round(pre.post.pct, 1),
         group  = GROUP.LABELS[group],
         window = sapply(window, function(w) WINDOWS[[w]]$label)) %>%
  select(window, group, intervention, model.pre.post.pct, paper.crude.pre.post.pct)

write_described_csv(
  pp.table,
  file.path(TABLE.DIR, "6-seattle_vs_king_county_pre_post.csv"),
  c("Pre-post % change in mean monthly all-stage syphilis diagnoses (post window vs pre window), within each scenario.",
    paste0("Model: median [95% interval] across simulations. Paper: crude observed change from reported counts",
           " (not the ITS estimate). Pre window spans the flat 2018-2020 and rising 2021-2023 periods.")))


# ---- 6. FIGURE: DOSE-RESPONSE VS THE KING COUNTY ESTIMATE (PRIMARY WINDOW) ----
fig.model <- cf.summary %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  mutate(group = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]))
fig.paper <- PAPER %>%
  filter(window == "primary", group %in% PAPER.GROUPS) %>%
  mutate(group = factor(GROUP.LABELS[group], levels = GROUP.LABELS[PAPER.GROUPS]))

fig <- ggplot(fig.model, aes(x = target.pct)) +
  geom_rect(data = fig.paper, inherit.aes = FALSE,
            aes(xmin = -Inf, xmax = Inf, ymin = rel.lo, ymax = rel.hi),
            fill = "grey70", alpha = 0.35) +
  geom_hline(data = fig.paper, aes(yintercept = rel.diff), linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_errorbar(aes(ymin = rel.lo, ymax = rel.hi), width = 1.5) +
  geom_point(size = 2) +
  facet_wrap(~ group, nrow = 1) +
  scale_x_continuous(breaks = seq(5, 50, 5)) +
  labs(x = "Doxy-PEP coverage among MSM targeted for 2030 (%; linear scale-up from Jan 2023)",
       y = "Relative difference in all-stage diagnoses,\nMar 2023-Jun 2025, vs no doxy-PEP (%)",
       title = "Seattle MSA (SHIELD) vs King County interrupted time series",
       caption = paste0("Points and bars: SHIELD median and 95% interval (", N.SIM, " paired simulations). ",
                        "Dashed line and band: Menza et al. 2026 (PMID 41885610), primary analysis.\n",
                        "Paper strata: total incl. TNB; cisgender men; cisgender women.")) +
  theme_bw(base_size = 11) +
  theme(plot.caption = element_text(hjust = 0, size = 8))

ggsave(file.path(FIG.DIR, "6-seattle_vs_king_county_counterfactual.png"),
       fig, width = 11, height = 4.5, dpi = 300)
fig

message(PAIRING.NOTE)
