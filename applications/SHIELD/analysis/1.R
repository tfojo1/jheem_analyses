# ****************************************************************************************************
# FIGURE 2 -- NO INTERVENTION TRENDS -----
# ****************************************************************************************************
baseline.tbl <- make_multi_location_table(
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

p.base.A <- plot_trend_by_location(baseline.tbl, outcome = "incidence",
                                   subgroup = "Total", 
                                   # dashed = HL,
                                   log.y = TRUE,
                                   y.breaks = c(1000, 5000, 10000,50000,100000),
                                   y.limits = c(1000,100000),
                                   y.lab = "Incident cases")
p.base.B <- plot_trend_by_location(baseline.tbl, outcome = "rate_incidence_per_pop",
                                   subgroup = "Total", 
                                   # dashed = HL,
                                   y.lab = "Incidence rate (per 100,000 person-years)")


p.base.C <- plot_trend_by_location(baseline.tbl, outcome = "pct_incidence_share",
                                   subgroup = "msm", 
                                   # dashed = HL,
                                   y.lab = "MSM share of incident infections (%)")

p.base.D <- plot_trend_by_location(baseline.tbl, outcome = "rate_diagnosis_ps_per_pop",
                                   subgroup = "Total", 
                                   # dashed = HL,
                                   y.lab = "PS Diagnosis rate (per 100,000 person-years)")

fig.baseline <- (p.base.A | p.base.B)/( p.base.D | p.base.C) + plot_annotation(tag_levels = "A");fig.baseline

.save_fig(fig.baseline, .default_fig_dir(), "fig2_noint", width = 8, height = 8, dpi = 300)


# TABLE S2 -- DOXY-PEP EFFICIENCY ----
# ****************************************************************************************************
#1	Antibiotic cost per infection averted
# At the total-population level, Doxy-PEP was projected to be most efficient at low coverage and to become progressively less efficient as coverage increased. At 10% coverage, a median of 3.2 person-years of Doxy-PEP (range across MSAs 1.7–14.4) was required per infection averted, rising to 5.3 person-years (2.2–21.0) at 50% coverage (Table 3). The marginal measure deteriorated considerably faster: the first ten percentage points of coverage averted one infection for every 3.2 person-years of drug, whereas the increment from 40% to 50% coverage required 10.1 person-years (3.3–32.7) per additional infection averted. Across the modelled range, average efficiency fell by a factor of 1.7 while marginal efficiency fell by a factor of 3.2, indicating that the aggregate figure understates the antibiotic cost of expanding an existing programme. Pooled across all ten MSAs, 30% coverage corresponded to approximately 1.94 million person-years of doxycycline and 493,000 infections averted, or 3.9 person-years per infection averted.
# Efficiency differed substantially between populations, but in a stable manner across coverage levels. At 30% coverage, a median of 8.8 person-years of Doxy-PEP (3.8–42.0) was required per infection averted among MSM, compared with 17.5 person-years (6.1–108.7) per infection averted among women. Preventing an infection in a woman therefore required approximately twice as much antibiotic as preventing one among MSM, consistent with the indirect nature of that benefit. This ratio was essentially invariant to coverage.
# Between-MSA variation in efficiency was considerably larger than variation across coverage levels. At 30% coverage, the total-population cost ranged approximately nine-fold, from 1.9 person-years per infection averted in Houston to 17.7 in Chicago. The same ordering was broadly preserved within each population, and the MSAs in which Doxy-PEP was least efficient were those in which the heterosexual epidemic had grown furthest beyond the MSM epidemic by 2030.
# 
# Discussion point: accounting for spillover roughly halves the estimated antibiotic cost per infection averted, This is a structural feature of the modelled transmission networks rather than something that changes with coverage scale up

# Doxy-PEP is delivered only to MSM, so cumulative Doxy-PEP person-years are
# the SAME denominator for all three populations. That is deliberate and it is
# what makes the columns comparable:
#
#   Total  -- every infection prevented anywhere in the population, per
#             person-year of drug. The number the AMR trade-off turns on.
#   MSM    -- the direct effect only.
#   female -- infections prevented in women per person-year of drug given to
#             men. The cost, in antibiotic, of the spillover.
#
#   py_per_infection(Total) / py_per_infection(MSM) is exactly the inverse of
#   the spillover multiplier, so Tables 3 and 4 have to agree; CHECK 3b tests
#   that they do.
#
# TWO RATIOS PER POPULATION:
#   ..._cum   all drug given 2022->horizon over all infections prevented over
#             the same window. Programme-to-date efficiency.
#   ..._incr  what the LAST 10 points of coverage bought, per additional
#             person-year they cost. The decision-relevant number, and the one
#             that falls as the MSM epidemic saturates.
#
# NOTE there is deliberately no single-year version of either. See "WHY THERE
# IS NO SINGLE-YEAR *_averted_ppy_doxy OUTCOME" at the foot of
# results/generate_custom_outcomes.R.
# ****************************************************************************************************
{
    t3.long <- table_to_long(make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("num_cum_doxy_coverage", "num_cum_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = EVAL.YEAR,
        stat.type     = "median"))
    
    t3.py <- t3.long %>%
        filter(outcome == "num_cum_doxy_coverage", subgroup == "Total") %>%
        select(location, year, coverage, cum_py = value)
    
    t3.av <- t3.long %>%
        filter(outcome == "num_cum_incidence_averted",
               subgroup %in% c("Total", "msm", "female")) %>%
        select(location, year, coverage, subgroup, averted = value)
    
    t3.step <- t3.av %>%
        left_join(t3.py, by = c("location", "year", "coverage")) %>%
        arrange(location, year, subgroup, coverage) %>%
        group_by(location, year, subgroup) %>%
        mutate(d_py      = cum_py  - dplyr::lag(cum_py,  default = 0),
               d_averted = averted - dplyr::lag(averted, default = 0)) %>%
        ungroup() %>%
        mutate(py_per_infection_cum  = cum_py / .pos(averted),
               py_per_infection_incr = d_py   / .pos(d_averted))
    
    # Tidy (long) form for the checks below; `final3` is the wide, labelled
    # version written to CSV. Both are kept on purpose -- re-deriving one from
    # the other is how the two drift apart.
    table3 <- t3.step %>%
        select(location, year, coverage, subgroup, cum_py, averted,
               d_py, d_averted, py_per_infection_cum, py_per_infection_incr) %>%
        arrange(location, year, subgroup, coverage)
    
    #reordering the columns to have MSA and Subgroup-similar to other tables
    t3.step <- t3.step[, c("location", "subgroup",
                           setdiff(names(t3.step), c("location", "subgroup")))]
    # names(t3.step)
    
    tbl3<-t3.step
    col.labels.t3 <- stats::setNames(
        # ORDER MATTERS: these map POSITIONALLY onto names(tbl3)[-3], and the
        # pipeline creates cum_py, then d_py, then d_averted. Labels 6 and 7
        # were transposed here, which reported person-years as infections
        # averted and vice versa.
        c("MSA", "Population", "Coverage",
          paste0("Cumulative Incidence Averted, ", BASE.YEAR, "-", EVAL.YEAR),
          paste0("Cumulative DoxyPEP Person-Years, ", BASE.YEAR, "-", EVAL.YEAR),
          "Marginal DoxyPEP Person-Years",
          "Marginal Incidence Averted",
          "Person-Years of DoxyPEP per Infection Averted (Cumulative)",
          "Person-Years of DoxyPEP per Infection Averted (Marginal)"
        ),
        #
        names(tbl3)[-3]
    )
    
    hit3 <- names(tbl3) %in% names(col.labels.t3)
    final3 <- tbl3[names(tbl3)[hit3]]
    names(final3) <- col.labels.t3[names(final3)]
    
    write.csv(final3, file.path(TABLE.DIR, paste0("3-efficiency.csv")),
              row.names = FALSE)
}
cat("\n=== TABLE 3: person-years of doxycycline per infection averted ===\n")
cat("    Rows       one per MSA x population x coverage level; all of\n")
cat("              ", paste(sub("doxy\\.cov\\.", "", COVERAGE.LEVELS), collapse = ", "),
    "% are shown, cumulative", BASE.YEAR, "-", EVAL.YEAR, "\n")
cat("    Denominator Doxy-PEP person-years are delivered to MSM only, so the\n")
cat("               SAME denominator applies to all three populations --\n")
cat("               women included. That is what makes the columns\n")
cat("               comparable, and it is why the female row reads as the\n")
cat("               antibiotic cost of the spillover.\n")
cat("    Cumulative all drug given and all infections prevented over the\n")
cat("               window; programme-to-date efficiency.\n")
cat("    Marginal   each coverage level against the one below it (the lowest\n")
cat("               rung against no intervention); what the last 10 points\n")
cat("               bought per additional person-year they cost.\n")
cat("    NOT shown  any single-year efficiency. Drug given in one year keeps\n")
cat("               preventing infections in later years, so a same-year\n")
cat("               ratio credits the wrong year -- see the note at the foot\n")
cat("               of results/generate_custom_outcomes.R.\n\n")
print(as.data.frame(table3), digits = 4, row.names = FALSE)


# ****************************************************************************************************