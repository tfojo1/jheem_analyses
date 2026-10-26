
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')
# ****************************************************************************************************
# Set the calibration once. The folders and the four result arrays follow from it.
CALIB.NAME <- "calib.8.21.stage3.az"
print(shield.output.path(CALIB.NAME))
if(1==2){
  # total_raw, total_calc, sex_raw, sex_calc -- in that order, as before
  results   <- load.shield.results(CALIB.NAME)
  FIG.DIR   <- shield.fig.path(CALIB.NAME,   create = TRUE)
  TABLE.DIR <- shield.table.path(CALIB.NAME, create = TRUE)
  
  # The same four arrays under their own names, for the scripts that use them that way
  # (generate_correlation_plots.R, and the example blocks in doxy_figures.R).
  total_raw_results  <- results$total_raw
  total_calc_results <- results$total_calc
  sex_raw_results    <- results$sex_raw
  sex_calc_results   <- results$sex_calc
}
# ----------------------------------------------------------------------------
# ONE city order for every figure in this script.
#
# Each figure has its own value-based ranking (coverage needed, terminal
# impact, ...) that it uses when you do NOT tell it an order -- which is why
# panels used to move between figures. Passing `locations = MSAS` pins the
# order everywhere. Add order.by = "value" to any single call when you want
# that figure ranked instead.
# ----------------------------------------------------------------------------
MSAS <- names(SHIELD.TEN.MSAS)
COVERAGE.LEVELS <- paste0("doxy.cov.", seq(10, 50, 10))
EVAL.YEAR=2030


# dimnames(sex_calc_results)
# $outcome
#[1] "num_incidence_averted"                          "pct_incidence_averted"                         
# [3] "rate_incidence_averted_per_pop"                 "pct_incidence_reduction_vs_baseline"           
# [5] "rate_incidence_per_pop"                         "ratio_incidence_vs_baseline"                   
# [7] "ratio_rate_incidence_per_pop_vs_baseline"       "num_diagnosis_total_averted"                   
# [9] "pct_diagnosis_total_averted"                    "rate_diagnosis_total_averted_per_pop"          
# [11] "pct_diagnosis_total_reduction_vs_baseline"      "rate_diagnosis_total_per_pop"                  
# [13] "ratio_diagnosis_total_vs_baseline"              "ratio_rate_diagnosis_total_per_pop_vs_baseline"
# [15] "num_diagnosis_ps_averted"                       "pct_diagnosis_ps_averted"                      
# [17] "rate_diagnosis_ps_averted_per_pop"              "pct_diagnosis_ps_reduction_vs_baseline"        
# [19] "rate_diagnosis_ps_per_pop"                      "ratio_diagnosis_ps_vs_baseline"                
# [21] "ratio_rate_diagnosis_ps_per_pop_vs_baseline"    "ratio_ps_diagnosis_to_incidence"               
# [23] "num_cum_incidence"                              "num_cum_incidence_averted"                     
# [25] "pct_cum_incidence_averted"                      "num_cum_diagnosis_total"                       
# [27] "num_cum_diagnosis_total_averted"                "pct_cum_diagnosis_total_averted"               
# [29] "num_cum_diagnosis_ps"                           "num_cum_diagnosis_ps_averted"                  
# [31] "pct_cum_diagnosis_ps_averted"                   "pct_incidence_share"                           
# [33] "pct_male_incidence_share"                       "ratio_el_diagnosis_to_incidence"               


# FIGURE1-----
fig1.tbl <- make_multi_location_table(
  data            = results,
  locations       = MSAS,
  outcomes        = c("incidence",
                      "rate_incidence_per_pop",
                      "pct_incidence_share",
                      "ratio_ps_diagnosis_to_incidence",
                      "rate_diagnosis_ps_per_pop"),
  interventions   = "noint",
  filter.by.strat = c("Total", "msm"),
  years           = as.character(2022:2030),
  digits = c(.default                                 = 2),
  stat.type       = "median")

pA <- plot_trend_by_location(fig1.tbl, outcome = "incidence",
                             subgroup = "Total", 
                             # dashed = HL,
                             log.y = TRUE,
                             y.breaks = c(1000, 5000, 10000,50000,100000),
                             y.limits = c(1000,100000),
                             y.lab = "Incident cases")
pB <- plot_trend_by_location(fig1.tbl, outcome = "rate_incidence_per_pop",
                             subgroup = "Total", 
                             # dashed = HL,
                             y.lab = "Incidence rate (per 100,000 person-years)")


pC <- plot_trend_by_location(fig1.tbl, outcome = "pct_incidence_share",
                             subgroup = "msm", 
                             # dashed = HL,
                             y.lab = "MSM share of incident infections (%)")

pD <- plot_trend_by_location(fig1.tbl, outcome = "rate_diagnosis_ps_per_pop",
                             subgroup = "Total", 
                             # dashed = HL,
                             y.lab = "PS Diagnosis rate (per 100,000 person-years)")

fig1 <- (pA | pB)/( pD | pC) + plot_annotation(tag_levels = "A");fig1

.save_fig(fig1, .default_fig_dir(), "fig1_noint", width = 8, height = 8, dpi = 300)


# FIGURE 2: heatmaps ----
### Relative to 2022 
tbl.rel.2022 = make_multi_location_table(
  data          = results,
  locations     = MSAS,
  outcomes      = c("pct_incidence_reduction_vs_baseline"),
  interventions = COVERAGE.LEVELS,
  years         = EVAL.YEAR,
  stat.type     = "median",
  save          = F
);
### Relative to 2030 with no intervention  
tbl.rel.2030 = make_multi_location_table(
  data          = results,
  locations     = MSAS,
  outcomes      = c("pct_incidence_averted"),
  interventions = COVERAGE.LEVELS,
  years         = EVAL.YEAR,
  stat.type     = "median",
  save          = F
);

## mk ----
# helper code to build the heatmap from each table
mk <- function(tbl, strat, ttl){
  plot_coverage_heatmap(
    tbl, 
    locations = MSAS,
    subgroup = strat, midpoint = 50,
    fill.style   = "banded",
    limits       = c(-100, 100),
    order.rows   = "alpha",
    label.colour = "black",
    squish.marks = "always",        # <- identical labels on every panel
    title        = ttl,
    fill.lab     = "%Reduction In Incidence",
    legend.dir   = "horizontal",
    fixed.aspect = FALSE)
}

pA <- mk(tbl.rel.2022, "msm",   "A: MSM (vs. 2022 baseline)")
pB <- mk(tbl.rel.2022, "Total", "B: Total Population (vs. 2022 baseline)")
pC <- mk(tbl.rel.2030,          "msm",   "C: MSM (vs. 2030 No Intervention)")
pD <- mk(tbl.rel.2030,          "Total", "B: Total Population (vs. 2030 No Intervention)")

no.y <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
no.x <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())

fig2 <- (pA + no.x | pB + no.x + no.y) /
  (pC        | pD + no.y) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom");fig2

ggsave(file.path(FIG.DIR, "fig2_heatmaps.png"), fig2,
       width = 9, height = 9, dpi = 300, bg = "white")

# FIGURE 3: critical coverage ----
# Same 2x2 layout and the same reading order as Figure 2, so the two can sit
# side by side:
#
#   A: MSM   vs the 2022 baseline          B: Total vs the 2022 baseline
#   C: MSM   vs no intervention in 2030    D: Total vs no intervention in 2030
#
# Figure 2 asks how much reduction each coverage level buys. This figure reads
# the same surface the other way: how much coverage is needed to cross 50%.
#
# TWO DELIBERATE DIFFERENCES FROM FIGURE 2:
#
#  1. The coverage ladder runs to 100%, not 50%. Figure 2 displays a
#     dose-response across the policy-relevant range; this figure SEARCHES for
#     a threshold, and a ladder stopping at 50% cannot tell "needs 70%" apart
#     from "never gets there" -- both would print as "not reached", which
#     would understate what the model can actually answer.
#
#  2. Rows are in the common city order, not ranked by coverage needed.
#     Ranking is right for a standalone single panel, where the ranking IS the
#     message; in a 2x2 it lets cities move between panels and destroys the
#     comparison the figure exists to make. For a ranked single panel, call
#     plot_coverage_needed() directly with order.by = "value" and a filename.
#
# EXPECT MANY "not reached" BARS IN A AND B. That is the finding, not a
# failure. Because the untreated epidemic grows, a given coverage can avert
# most of the infections projected for 2030 (C and D) while incidence still
# sits above its 2022 level (A and B). The gap between the top and bottom rows
# is the whole point of showing both comparators.
#
# NOTE the top row inherits the count-vs-rate caveat: 
# pct_incidence_reduction_vs_baseline is computed on incident COUNTS, so "50%
# reduction vs 2022" here means half the infections, not half the incidence
# RATE. With a growing population those differ, and the count target is the
# harder of the two.

# the threshold search needs the full ladder, not Figure 2's 10-50% range
COVERAGE.LEVELS.F3 <- paste0("doxy.cov.", seq(10, 100, 10))

### Relative to 2022
f3.rel.2022 <- make_multi_location_table(
  data          = results,
  locations     = MSAS,
  outcomes      = c("pct_incidence_reduction_vs_baseline"),
  interventions = COVERAGE.LEVELS.F3,
  years         = EVAL.YEAR,
  stat.type     = "median",
  save          = F
)
### Relative to no intervention in the same year
f3.rel.noint <- make_multi_location_table(
  data          = results,
  locations     = MSAS,
  outcomes      = c("pct_incidence_averted"),
  interventions = COVERAGE.LEVELS.F3,
  years         = EVAL.YEAR,
  stat.type     = "median",
  save          = F
)

## mk3 ----
# helper, mirroring `mk` in the Figure 2 block above.
# filename = NULL so nothing is written per-panel; the composed figure is
# saved once at the end.
mk3 <- function(tbl, strat, ttl)
  plot_coverage_needed(
    tbl,
    locations = MSAS,
    subgroup  = strat,
    target    = 50,
    order.by  = "alpha",      # common row order across all four panels
    title     = ttl,
    filename  = NULL)

p3A <- mk3(f3.rel.2022,  "msm",   "A: MSM (vs. 2022 baseline)")
p3B <- mk3(f3.rel.2022,  "Total", "B: Total Population (vs. 2022 baseline)")
p3C <- mk3(f3.rel.noint, "msm",   paste0("C: MSM (vs. ", EVAL.YEAR, " No Intervention)"))
p3D <- mk3(f3.rel.noint, "Total", paste0("D: Total Population (vs. ", EVAL.YEAR, " No Intervention)"))

# same two theme strippers as Figure 2; redefined here so this block runs
# standalone rather than depending on the Figure 2 block having been sourced
no.y3 <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
no.x3 <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())

fig3 <- (p3A + no.x3 | p3B + no.x3 + no.y3) /
        (p3C         | p3D + no.y3);fig3

ggsave(file.path(FIG.DIR, "fig3_crit_coverage.png"), fig3,
       width = 12, height = 9, dpi = 300, bg = "white")

# DOXY IMPACT- TABLE2 -----
# Doxy-PEP impact across the ten MSAs at a single coverage level, for the total
# population and for MSM, in two clearly separated column blocks.
#
#   BLOCK A -- SINGLE-YEAR (YEAR.T2 only). What the epidemic looks like in that
#              one year relative to the no-intervention counterfactual, in
#              relative (%) and absolute (rate difference) terms. There is
#              deliberately NO efficiency column here: see below.
#   BLOCK B -- CUMULATIVE (baseline.year through YEAR.T2). The whole burden
#              prevented over the scale-up window, and the antibiotic it cost.
#              The percentages are LOWER than Block A by construction, because
#              coverage ramps from zero in 2023 and the early years contribute
#              little.
#
# WHY EFFICIENCY APPEARS ONLY IN BLOCK B. A single-year "infections averted per
# doxy person-year" outcome existed until 2026-09-08 and was removed: drug
# given in one year keeps preventing infections in later years, so a same-year
# ratio credits the wrong year and is biased in opposite directions at the two
# ends of a scale-up. See "WHY THERE IS NO SINGLE-YEAR *_averted_ppy_doxy
# OUTCOME" at the foot of results/generate_custom_outcomes.R. The cumulative
# version spans the same window on both sides and is the one to report.
#
# READ THE TWO BLOCKS SEPARATELY. Reporting "78% averted" from Block A beside
# "24,492 infections averted" from Block B in the same sentence is the single
# easiest way to make this table say something untrue.
#
# TWO COLUMNS CHANGE MEANING BETWEEN THE ROW TYPES:
#
#   rate_incidence_averted_per_pop -- the denominator is the `population`
#     outcome AT THAT ROW'S STRATIFICATION. On a Total row it is the whole MSA;
#     on an MSM row it is the MSM population. So the Total row reads "per
#     100,000 residents" and the MSM row reads "per 100,000 MSM". They are not
#     on the same scale and must not be compared down the column.
#
#   the *_ppy_doxy column -- doxy.coverage is not sex-stratified, so it exists
#     only at the total level and comes back NA on every MSM row. That is
#     expected, not a bug; the check below confirms it rather than leaving you
#     to wonder. If you want an MSM-specific efficiency number, it has to be
#     built by hand: MSM cumulative infections averted (sex level) over TOTAL
#     cumulative doxy person-years, because the drug goes only to MSM and so
#     the denominator is the same for every stratum.
# ------------------------------------------------------------------------------

CCRIT.T2 <- "doxy.cov.30"   # coverage level to summarise. Your earlier draft
                            # here used doxy.cov.20; 30% is what you chose as
                            # the median critical coverage. One edit to switch.
YEAR.T2  <- "2030"          # Block A year, and the horizon for Block B

t2.outcomes <- c(
#   # ---- BLOCK A: single year. RELATIVE and ABSOLUTE effect, no efficiency ----
#   "pct_incidence_averted",
#   "rate_incidence_averted_per_pop",
#   # ---- BLOCK B: cumulative from baseline.year. Burden prevented, and the
#   #      antibiotic it cost. Efficiency lives HERE and only here ----
#   "num_cum_incidence_averted",
#   "pct_cum_incidence_averted",
#   "rate_cum_incidence_averted_ppy_doxy")
# 
# tbl2.impact <- make_multi_location_table(
#   data            = results,
#   locations       = MSAS,
#   outcomes        = t2.outcomes,
#   interventions   = CCRIT.T2,
#   filter.by.strat = c("Total", "msm"),
#   years           = YEAR.T2,
#   stat.type       = "median.ci",
#   digits = c(.default                       = 0,
#              pct_incidence_averted          = 1,
#              pct_cum_incidence_averted      = 1,
#              rate_incidence_averted_per_pop = 1)
# )
# 
# # column names come back as <outcome>_<intervention>_<year>
# .t2sfx <- paste0("_", CCRIT.T2, "_", YEAR.T2)
# .t2cov <- sub("doxy\\.cov\\.", "", CCRIT.T2)
# 
# col.labels.t2 <- stats::setNames(
#   c("MSA", "Population",
#     paste0("A. Incidence averted (%), ", YEAR.T2),
#     paste0("A. Incidence averted per 100,000 population, ", YEAR.T2),
#     paste0("B. Cumulative infections averted, 2022-", YEAR.T2),
#     paste0("B. Cumulative incidence averted (%), 2022-", YEAR.T2),
#     paste0("B. Cumulative infections averted per 100,000 doxy person-years, 2022-", YEAR.T2)),
#   c("location", "subgroup", paste0(t2.outcomes, .t2sfx)))
# 
# hit2 <- names(tbl2.impact) %in% names(col.labels.t2)
# final2 <- tbl2.impact[names(tbl2.impact)[hit2]]
# names(final2) <- col.labels.t2[names(final2)]
# # keep the block order regardless of the order the builder returned them in
# final2 <- final2[col.labels.t2[c("location", "subgroup", paste0(t2.outcomes, .t2sfx))]]
# final2
# 
# write.csv(final2,
#           file = paste0(TABLE.DIR, "2-impact-cov", .t2cov, "-", YEAR.T2, ".csv"),
#           row.names = FALSE)

# ---- checks ------------------------------------------------------------------
cat("\n=== TABLE 2 CHECK 1: is the doxy-denominated column NA for MSM? ===\n")
cat("   Expected: doxy.coverage is total-level only, so the *_ppy_doxy column\n")
cat("   should be NA on every MSM row and populated on every Total row.\n")
.ppy <- col.labels.t2[paste0("rate_cum_incidence_averted_ppy_doxy", .t2sfx)]
for (cc in .ppy) {
  msm.na <- all(is.na(final2[[cc]][final2$Population == "msm"]) |
                final2[[cc]][final2$Population == "msm"] == "")
  tot.ok <- !all(is.na(final2[[cc]][final2$Population == "Total"]))
  cat("   ", if (msm.na && tot.ok) "OK  " else "CHECK", cc, "\n")
}

cat("\n=== TABLE 2 CHECK 2: is Block B a smaller percentage than Block A? ===\n")
cat("   Cumulative % averted must be BELOW single-year % averted, because\n")
cat("   coverage ramps from zero in 2023. If it is not, the two blocks have\n")
cat("   been mixed up somewhere.\n")
.pa <- col.labels.t2[paste0("pct_incidence_averted",     .t2sfx)]
.pb <- col.labels.t2[paste0("pct_cum_incidence_averted", .t2sfx)]
.est <- !grepl("^\\[", as.character(final2[[.pa]]))       # drop the CI rows
.a <- suppressWarnings(as.numeric(as.character(final2[[.pa]])[.est]))
.b <- suppressWarnings(as.numeric(as.character(final2[[.pb]])[.est]))
.bad <- which(!is.na(.a) & !is.na(.b) & .b > .a)
if (!length(.bad)) cat("   OK - cumulative below single-year in every row.\n") else {
  cat("   UNEXPECTED in", length(.bad), "row(s):\n")
  print(data.frame(MSA = final2$MSA[.est][.bad],
                   Population = final2$Population[.est][.bad],
                   single_year = .a[.bad], cumulative = .b[.bad]), row.names = FALSE)
}


 