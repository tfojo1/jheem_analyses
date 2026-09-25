

# TREND BY LOCATIONS SIDE BY SIDE ----
## No intervention: how does incidence change to 2030 and 2035?---
#how is incidence changing in each city, for het vs MSM? 
# Heterosexual incidence rises in nine of ten cities. Atlanta is not an exception — it rises, just slowly (~×1.3 over 2022–2030) while MSM rises faster (~×1.5). Baltimore is the only city where heterosexual incidence falls, and MSM falls there too.
{
    noint_tbl = make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("incidence","rate_incidence_per_pop"),
        interventions = "noint",
        years         = as.character(c(2022:2035)),
        stat.type     = "median.ci",
        # filter.by.strat = "msm",
        save          = F
    ); 
    
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("Total"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "incidence",
                       y.lab = "Incident Cases",
                       filename = "noint_inc_total.png"
    )
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("female","heterosexual_male","msm"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "incidence",
                       y.lab = "Incident Cases",
                       filename = "noint_inc_subgroup.png"
    )
    
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("female","heterosexual_male","msm"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "rate_incidence_per_pop",
                       y.lab = "Incidence Rate (per 100,000 py)",
                       filename = "noint_inc_rate.png"
    )
}


## %incidence averted at diff doxy-PEP: Locations side by side ----
{
    traj.tbl = make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = as.character(c(2022:2035)),
        stat.type     = "median",
        save          = F
        # filename = "t"
    )
    # incidence averted accross doxy sccenarios among total population
    plot_impact_over_time(traj.tbl,
                          locations = MSAS,
                          coverages = seq(10,50,10),
                          subgroup = "Total", target = 50, facet.ncol = 5,
                          # subgroup = "msm", target = 50, facet.ncol = 5,
                          # subgroup = "female", target = 50, facet.ncol = 5,
                          y.lab = "%Incidence averted",
                          title = "%Incidence averted among total population over time (at various Doxy-PEP coverages)",
                          # title = "%Incidence averted among MSM over time (at various Doxy-PEP coverages)",
                          # title = "%Incidence averted among women over time (at various Doxy-PEP coverages)",
                          year.range = c(2022, 2035),
                          filename = "inc_averted_doxy_total.png"
    )
}


## minimum C_crit of 40%, how does the impact builds up over time? ----
{
    c_crit = 40
    traj.tbl.ccrit = make_multi_location_table(
        data          = results,          # was list(total_results, sex_results) -- neither object exists here
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", c_crit),
        years         = as.character(c(2022:2035)),
        stat.type     = "median",
        save          = F
    )
    plot_impact_over_time(traj.tbl.ccrit,
                          locations = MSAS,
                          coverages = c_crit,
                          subgroup = c("Total","msm","heterosexual_male","female"),
                          target = 90, facet.ncol = 5,
                          y.lab = "%Incidence averted",
                          title = "%Incidence averted over time by subgroup (Doxy-PEP critical coverage of 40%)",
                          year.range = c(2022, 2035))
    
}


# WHAT EXPLAINS THE BETWEEN-CITY HETEROGENEITY? ----
# Reading the figures above:
#   * the MSM effect is nearly flat across the ten MSAs;
#   * total-population impact spans a five-fold range;
#   * so the spread must come from how much the intervention suppresses the
#     HETEROSEXUAL epidemic, not from how well it works in MSM.
#
# This block puts numbers on that. It produces:
#   1. one row per city: % averted in each subgroup, the MSM share of incident
#      cases at the start and end of the projection, and a "spillover ratio"
#      (% averted in women / % averted in MSM);
#   2. an arithmetic CHECK -- the share-weighted sum of the subgroup effects
#      should reproduce the model's own total-population effect;
#   3. two scatter plots.
CCRIT     <- "doxy.cov.50"     # coverage scenario to evaluate at
EVAL.YEAR <- "2030"            # year to evaluate at
BASE.YEAR <- "2022"

##1: Put data together >>>> ----
# ---- % incidence averted, by subgroup, at one coverage and one year  
impact.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = EVAL.YEAR,
    stat.type     = "median")

# ---- each subgroup's share of incident cases under no intervention 
share.eval.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_share",
    interventions = "noint",
    years         = c(BASE.YEAR, EVAL.YEAR),
    stat.type     = "median")

## reformat % incidence averted: rows location; columns subgroup: 
av <- table_to_long(impact.tbl) %>%
    select(location, subgroup, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value,
                       names_prefix = "av_")

# reformat MSM share: rows locations, columns %share in 2022 and 2030
sh.msm <- table_to_long(share.eval.tbl) %>%
    filter(subgroup == "msm") %>%
    select(location, year, value) %>%
    tidyr::pivot_wider(names_from = year, values_from = value,
                       names_prefix = "msm_share_")
# join:
spillover.tbl <- left_join(av, sh.msm, by = "location") %>%
    mutate(spillover = av_female / av_msm) %>%
    arrange(desc(spillover))

cat("\n=== % incidence averted at", CCRIT, "in", EVAL.YEAR,
    ", by city and subgroup ===\n")
print(as.data.frame(spillover.tbl), digits = 3, row.names = FALSE)

##2: Test association: >>>>>
# ---- does the MSM share predict the spillover? -----------------------------
# If the correlation is strong at BASE.YEAR 2022 but weak at EVAL.YEAR 2030, the message
# is that what matters is how much of the epidemic MSM drove at the START --
# by the end, the heterosexual epidemic they seeded has grown past them.
cat("\n=== Spearman correlation: MSM share vs spillover ratio ===\n")
print(round(c(
    `share at start` = cor(spillover.tbl[[paste0("msm_share_", BASE.YEAR)]],
                           spillover.tbl$spillover, method = "spearman"),
    `share at end`   = cor(spillover.tbl[[paste0("msm_share_", EVAL.YEAR)]],
                           spillover.tbl$spillover, method = "spearman")), 3))
# the correlation is strong at both years. It matters how much are their academic is driven by MSM both at the store and at the end

##3: Check aritmatic >>>> This does check out as it should
# ---- arithmetic check - 
# total % averted should equal the share-weighted sum of the subgroup effects,
# with the shares taken from the SAME (no-intervention) year. If these do not
# agree, the decomposition argument does not hold and something is off.
# sh.all.2030 <- table_to_long(share.eval.tbl) %>%
#     filter(year == as.integer(EVAL.YEAR),
#            subgroup %in% c("msm", "heterosexual_male", "female")) %>%
#     select(location, subgroup, share = value)
# 
# check <- table_to_long(impact.tbl) %>%
#     filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
#     select(location, subgroup, averted = value) %>%
#     left_join(sh.all.2030, by = c("location", "subgroup")) %>%
#     group_by(location) %>%
#     summarise(predicted.total = sum(share * averted) / 100, .groups = "drop") %>%
#     left_join(av %>% select(location, model.total = av_Total), by = "location") %>%
#     mutate(difference = predicted.total - model.total)
# 
# cat("\n=== check: share-weighted subgroup effects vs the model's own total ===\n")
# print(as.data.frame(check), digits = 4, row.names = FALSE)
# cat("   largest absolute discrepancy:",
#     signif(max(abs(check$difference), na.rm = TRUE), 3), "percentage points\n")
# cat("   NOTE: get_stats() rounds to whole numbers, so a discrepancy of one or\n",
#     "  two percentage points is rounding. Several points would mean the\n",
#     "  decomposition does not hold and something needs looking at.\n")


## Scatterplot Figures ----

## .label_layer ----
# ggrepel keeps the city labels from overlapping; fall back to plain text if
# it is not installed rather than failing at the end of a long script.
.label_layer <- function() {
    if (requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(aes(label = location), size = 3.2)
    else
        geom_text(aes(label = location), size = 3.2, hjust = -0.15, vjust = -0.4)
}

## Fig- %incidence averted total vs %incident averted women ----
p.tot.vs.fem <- ggplot(spillover.tbl, aes(x = av_female, y = av_Total)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.18))) +
    labs(x = "% incidence averted among women",
         y = "% incidence averted, total population",
         title = paste0("Total-population impact tracks the effect in women (",
                        CCRIT, ", ", EVAL.YEAR, ")"),
         subtitle = "dashed line is 1:1") +
    theme_minimal(base_size = 11); p.tot.vs.fem
# .save_fig(p.tot.vs.fem, .default_fig_dir(), "explain_total_vs_female.png", 6, 5, 300)

## Fig- spillover against the MSM share ----
p.spill <- spillover.tbl %>%
    tidyr::pivot_longer(dplyr::starts_with("msm_share_"),
                        names_to = "when", values_to = "msm.share") %>%
    mutate(when = sub("msm_share_", "MSM share in ", when)) %>%
    ggplot(aes(x = msm.share, y = spillover)) +
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    facet_wrap(~ when, scales = "free_x") +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.22))) +
    labs(x = "MSM share of incident cases under no intervention (%)",
         y = "spillover ratio  (% averted in women / % averted in MSM)",
         title = "How much of the MSM-side benefit reaches women") +
    theme_minimal(base_size = 11);p.spill
# .save_fig(p.spill, .default_fig_dir(), "explain_spillover_vs_share.png", 9, 4.5, 300)



# ---- is it the MSM TREND rather than the MSM SHARE? ------------------------
# A city where the MSM epidemic keeps pace with the heterosexual one leaves
# Doxy-PEP something to work on; a city where MSM incidence is already falling
# under no intervention while heterosexual incidence takes off leaves very
# little population-level curve to bend.
#
# The discriminating quantity is not the MSM trend on its own -- Baltimore's
# MSM incidence falls too, and it has one of the highest spillovers, because
# its heterosexual epidemic is falling as well. It is the DIVERGENCE between
# the two trends:
#
#     divergence = log( heterosexual growth / MSM growth )
#
# negative when MSM keep pace or better, strongly positive where the
# heterosexual epidemic is running away from them.
growth.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "rate_incidence_per_pop",
    interventions = "noint",
    years         = c(BASE.YEAR, EVAL.YEAR),
    stat.type     = "median")

g <- table_to_long(growth.tbl) %>%
    filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
    mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
    group_by(location, grp, year) %>%
    summarise(inc = sum(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
    mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
               .data[[paste0("inc_", BASE.YEAR)]]) %>%
    select(location, grp, growth) %>%
    tidyr::pivot_wider(names_from = grp, values_from = growth, names_prefix = "growth_") %>%
    mutate(divergence = log(growth_het / growth_msm))

tbl2 <- left_join(spillover.tbl, g, by = "location") %>% arrange(desc(spillover))

cat("\n=== MSM vs heterosexual growth,", BASE.YEAR, "->", EVAL.YEAR, "(no intervention) ===\n")
print(as.data.frame(tbl2[, c("location", "av_msm", "av_female", "spillover",
                                       "growth_msm", "growth_het", "divergence")]),
      digits = 3, row.names = FALSE)

cat("\n=== which candidate explains the spillover best? (Spearman) ===\n")
print(round(c(
    `MSM share at start` = cor(tbl2[[paste0("msm_share_", BASE.YEAR)]],
                               tbl2$spillover, method = "spearman"),
    `MSM share at end`   = cor(tbl2[[paste0("msm_share_", EVAL.YEAR)]],
                               tbl2$spillover, method = "spearman"),
    `MSM growth alone`   = cor(tbl2$growth_msm, tbl2$spillover, method = "spearman"),
    `divergence`         = cor(tbl2$divergence, tbl2$spillover, method = "spearman")), 3))
cat("  (divergence should be NEGATIVE: the further the heterosexual epidemic\n",
    "  runs ahead of the MSM one, the less of the MSM benefit reaches women)\n")

# Figure ----
m<- lm(tbl2$spillover~tbl2$divergence )
p.diverge <- ggplot(tbl2, aes(x = divergence, y = spillover)) +
    # geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_abline(intercept = m$coefficients[1],slope = m$coefficients[2],linetype = "dashed", colour = "grey50")+
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.20))) +
    labs(x = paste0("log(heterosexual growth / MSM growth), ",
                    BASE.YEAR, "-", EVAL.YEAR, ", no intervention"),
         y = "spillover ratio  (% averted in women / % averted in MSM)",
         title = "Where the heterosexual epidemic outruns the MSM epidemic, Doxy-PEP has less to work on",
         subtitle = "left of the dashed line: MSM keeping pace or better") +
    theme_minimal(base_size = 11);p.diverge
.save_fig(p.diverge, .default_fig_dir(), "spillover_vs_divergence.png", 8, 5.5, 300)
tbl2

# TRANSMISSION COUPLING: HOW FAR THE MSM EFFECT CARRIES INTO WOMEN
# ****************************************************************************
#
# The spillover ratio uses a single coverage level. This uses the whole
# coverage gradient: within each city, regress the effect in women on the
# effect in MSM across all coverage scenarios, through the origin (zero
# coverage gives zero effect in both, by construction). The slope is a
# per-city coupling coefficient estimated from ten points rather than one.
#
#   slope near 1   the MSM effect carries fully into women -- heterosexual
#                  transmission largely depends on the MSM network
#   slope near 0   it does not -- the heterosexual epidemic is self-sustaining
#
# `r2` is worth reading too: a low value means the relationship is curved
# rather than proportional, and the single slope is hiding that.
#
# `residual.het` is the complementary bound: the percentage of incidence in
# women still occurring at the HIGHEST coverage. Whatever survives near-total
# MSM coverage is, by construction, not sustained by the MSM network. It is an
# upper bound rather than a point estimate, because Doxy-PEP does not fully
# suppress MSM incidence even at 100% coverage.
# ****************************************************************************

coupling.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median")

cpl <- table_to_long(coupling.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, coverage, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value)

coupling <- cpl %>%
    group_by(location) %>%
    summarise(slope        = coef(lm(female ~ 0 + msm))[["msm"]],
              r2           = summary(lm(female ~ 0 + msm))$r.squared,
              residual.het = 100 - female[which.max(msm)],
              x            = max(msm),
              y            = female[which.max(msm)],
              .groups = "drop") %>%
    arrange(desc(slope))

cat("\n=== transmission coupling by city (", EVAL.YEAR, ") ===\n")
print(as.data.frame(coupling[, c("location", "slope", "r2", "residual.het")]),
      digits = 3, row.names = FALSE)
cat("  slope        rise in women's % averted per point of MSM % averted\n")
cat("  residual.het % of women's incidence remaining at the highest coverage\n")
cat("               (upper bound on the self-sustaining heterosexual fraction)\n")

# ---- figure ---------------------------------------------------------------
# order everything by slope so the palette runs with the ranking
cpl$location      <- factor(cpl$location,      levels = coupling$location)
coupling$location <- factor(coupling$location, levels = coupling$location)

# Spread the end-of-line labels so they never collide. Done arithmetically
# rather than with ggrepel so the figure looks the same on any machine.
coupling <- coupling[order(coupling$y), ]
min.gap  <- 0.05 * diff(range(cpl$female, na.rm = TRUE))
coupling$y.lab <- coupling$y
for (i in seq_len(nrow(coupling))[-1])
    coupling$y.lab[i] <- max(coupling$y[i], coupling$y.lab[i - 1] + min.gap)

x.pad <- 0.04 * diff(range(cpl$msm, na.rm = TRUE))

p.coupling <- ggplot(cpl, aes(msm, female, colour = location)) +
    geom_abline(slope = 1, linetype = "dashed", colour = "grey55") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    # a short leader from each line's end to its (possibly nudged) label
    geom_segment(data = coupling, colour = "grey70", linewidth = 0.3,
                 aes(x = x, xend = x + x.pad, y = y, yend = y.lab)) +
    geom_text(data = coupling, hjust = 0, size = 3.3, fontface = "bold",
              show.legend = FALSE,
              aes(x = x + x.pad * 1.2, y = y.lab,
                  label = sprintf("%s  %.2f", location, slope))) +
    # room on the right for the labels, but no tick marks above 100
    scale_x_continuous(breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0.03, 0.32))) +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, guide = "none") +
    labs(x = "% incidence averted among MSM",
         y = "% incidence averted among women",
         title = "Transmission coupling: how far the MSM effect carries into women",
         subtitle = paste0("one line per city across Doxy-PEP coverage levels, ",
                           EVAL.YEAR, "; the number is the through-origin slope. ",
                           "Dashed line is 1:1.")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
p.coupling
.save_fig(p.coupling, .default_fig_dir(), "explain_coupling.png", 10, 6.5, 300)


# ADDITION 1 -- HOW LONG THE INDIRECT EFFECT TAKES TO ARRIVE
# ****************************************************************************
# Everything above is evaluated at EVAL.YEAR (2030). Doxy-PEP is delivered to
# MSM only, so the effect in women is second-order: it has to travel through
# the transmission network before it appears. If it is still accruing in 2030
# then every spillover and coupling number above is a LOWER BOUND, and the
# size of that bias is what this block measures.
#
#   (a) the spillover ratio recomputed at 2030 and at 2035;
#   (b) the coupling slope refit at each horizon, using all ten coverages;
#   (c) t90 -- the year each subgroup reaches 90% of its own 2035 effect, and
#       the gap between MSM and women, which is the lag in years.
#
# PRECISION: get_stats() rounds to whole numbers, so a ratio of two small
# percentages is coarse. Where av_msm is below ~10%, read the slope in (b)
# rather than the ratio in (a) -- it is fitted from ten points and is far less
# sensitive to rounding.
# ****************************************************************************

LAG.YEARS  <- c("2030", "2035")
TRAJ.YEARS <- as.character(2023:2035)   # 2022 is the start year: % averted is 0

.y1 <- LAG.YEARS[1]
.y2 <- LAG.YEARS[2]

# ---- (a) spillover ratio at each horizon -----------------------------------
lag.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = LAG.YEARS,
    stat.type     = "median")

spill.by.year <- table_to_long(lag.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, year, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value) %>%
    mutate(spillover = female / msm) %>%
    rename(av_msm = msm, av_female = female)

spill.wide <- spill.by.year %>%
    select(location, year, spillover) %>%
    tidyr::pivot_wider(names_from = year, values_from = spillover,
                       names_prefix = "spillover_") %>%
    mutate(absolute.change = .data[[paste0("spillover_", .y2)]] -
               .data[[paste0("spillover_", .y1)]],
           pct.change      = 100 * (.data[[paste0("spillover_", .y2)]] /
                                        .data[[paste0("spillover_", .y1)]] - 1)) %>%
    arrange(desc(pct.change))

cat("\n=== spillover ratio at", CCRIT, ":", .y1, "vs", .y2, "===\n")
print(as.data.frame(spill.wide), digits = 3, row.names = FALSE)
cat("  pct.change > 0 means the", .y1,
    "figure UNDERSTATES how much of the MSM benefit reaches women.\n")

## .slope0 ----
# ---- (b) coupling slope refit at each horizon ------------------------------
# Through-origin slope of (% averted in women) on (% averted in MSM) across
# the coverage gradient. Same estimator as the coupling block above, refit
# once per year, so the two horizons are directly comparable.
.slope0 <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2 || all(x[ok] == 0))
        return(c(slope = NA_real_, r2 = NA_real_))
    m <- lm(y[ok] ~ 0 + x[ok])
    c(slope = unname(coef(m)[1]), r2 = summary(m)$r.squared)
}

coupling.yr.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = COVERAGE.LEVELS,
    years         = LAG.YEARS,
    stat.type     = "median")

cpl.yr <- table_to_long(coupling.yr.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, coverage, year, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value)

coupling.by.year <- cpl.yr %>%
    group_by(location, year) %>%
    group_modify(~ {
        f <- .slope0(.x$msm, .x$female)
        tibble::tibble(slope        = f[["slope"]],
                       r2           = f[["r2"]],
                       residual.het = 100 - .x$female[which.max(.x$msm)])
    }) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from  = year,
                       values_from = c(slope, r2, residual.het))

cat("\n=== transmission coupling refit at each horizon ===\n")
print(as.data.frame(coupling.by.year), digits = 3, row.names = FALSE)
cat("  a slope that rises from", .y1, "to", .y2,
    "means the coupling estimated at", .y1, "is a lower bound;\n")
cat("  residual.het falling over the same window means the self-sustaining\n",
    " heterosexual fraction is smaller than the", .y1, "figure suggests.\n")

## .time_to_frac ----
# ---- (c) t90: when has each subgroup realised 90% of its 2035 effect? ------
# Linear interpolation between the two bracketing years, so the answer is a
# fractional year rather than a step function of the annual output grid.
.time_to_frac <- function(year, value, frac = 0.90) {
    ok <- is.finite(year) & is.finite(value)
    if (sum(ok) < 2) return(NA_real_)
    year <- year[ok]; value <- value[ok]
    o <- order(year); year <- year[o]; value <- value[o]
    target <- frac * value[length(value)]
    if (!is.finite(target) || target <= 0) return(NA_real_)
    hit <- which(value >= target)[1]
    if (is.na(hit)) return(NA_real_)
    if (hit == 1L)  return(year[1])
    v0 <- value[hit - 1L]; v1 <- value[hit]
    if (v1 == v0) return(year[hit])
    year[hit - 1L] + (target - v0) / (v1 - v0) * (year[hit] - year[hit - 1L])
}

traj.lag.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = TRAJ.YEARS,
    stat.type     = "median")

t90.long <- table_to_long(traj.lag.tbl) %>%
    filter(subgroup %in% c("msm", "female", "Total")) %>%
    group_by(location, subgroup) %>%
    summarise(t90      = .time_to_frac(year, value, 0.90),
              terminal = value[which.max(year)],
              .groups  = "drop")

t90.wide <- t90.long %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = c(t90, terminal)) %>%
    mutate(lag.years = t90_female - t90_msm) %>%
    mutate(across(dplyr::starts_with("t90_"), ~ round(.x, 1)),
           lag.years = round(lag.years, 2)) %>%
    arrange(desc(lag.years))

# digits = 6, not 4: with 4 significant digits a fractional calendar year such
# as 2032.4 prints as 2032 and the interpolation is invisible.
cat("\n=== years to 90% of the", .y2, "effect (", CCRIT, ") ===\n")
print(as.data.frame(t90.wide), digits = 6, row.names = FALSE)
cat("  lag.years = how much later women reach 90% of THEIR OWN terminal\n",
    " effect than MSM reach 90% of theirs. A positive lag is the number to\n",
    " quote: the benefit to women is still arriving when the talk stops.\n")

# ---- figures ---------------------------------------------------------------
# NOTE: titles below are deliberately descriptive. Once you have seen the
# direction, rewrite them as the claim (that is what the slide should say).
.ord.spill <- spill.wide$location[order(spill.wide[[paste0("spillover_", .y2)]])]

p.spill.lag <- spill.by.year %>%
    mutate(location = factor(location, levels = .ord.spill),
           year     = factor(year, levels = LAG.YEARS)) %>%
    ggplot(aes(x = spillover, y = location)) +
    geom_line(aes(group = location), colour = "grey65", linewidth = 1) +
    geom_point(aes(colour = year), size = 3) +
    scale_colour_manual(values = setNames(c("#92C5DE", "#2166AC"), LAG.YEARS),
                        name = NULL) +
    labs(x = "spillover ratio  (% averted in women / % averted in MSM)",
         y = NULL,
         title = paste0("Spillover ratio at ", .y1, " and ", .y2),
         subtitle = paste0(CCRIT, "; one segment per city")) +
    theme_minimal(base_size = 11)
p.spill.lag
.save_fig(p.spill.lag, .default_fig_dir(),
          "lag_spillover_2030_vs_2035.png", 7.5, 5, 300)

p.t90 <- t90.long %>%
    filter(subgroup %in% c("msm", "female")) %>%
    mutate(location = factor(location, levels = rev(t90.wide$location))) %>%
    ggplot(aes(x = t90, y = location)) +
    geom_line(aes(group = location), colour = "grey65", linewidth = 1) +
    geom_point(aes(colour = subgroup), size = 3) +
    scale_colour_manual(values = c(msm = "#2166AC", female = "#B2182B"),
                        name = NULL) +
    labs(x = paste0("year at which 90% of the ", .y2, " effect is reached"),
         y = NULL,
         title = "When each subgroup realises its benefit",
         subtitle = paste0(CCRIT,
                           "; interpolated between annual model output")) +
    theme_minimal(base_size = 11)
p.t90
.save_fig(p.t90, .default_fig_dir(), "lag_t90_msm_vs_women.png", 7.5, 5, 300)


# ****************************************************************************
# ADDITION 2 -- EFFICIENCY: HOW MUCH DOXYCYCLINE PER CASE AVERTED
# ****************************************************************************
# generate_custom_outcomes.R already builds the pieces, at the TOTAL level
# only (they sit behind `if (is.null(STRATIFICATION_DIMENSIONS))`):
#
#   num_cum_doxy_coverage                cumulative Doxy-PEP person-years,
#                                        accumulated from the 2022 baseline
#   num_cum_incidence_averted            cumulative cases averted vs noint
#   rate_cum_incidence_averted_ppy_doxy  1e5 * averted / person-years
#
# Missing are (i) the inverse -- person-years of Doxy-PEP per case averted,
# which is the number people actually quote -- and (ii) the same quantity with
# the numerator restricted to WOMEN. That second one has an MSM-only
# denominator and a female-only numerator BY DESIGN: it is the cost, in drug
# delivered to men, of protecting people who never receive it. It is where the
# AMR question lands.
#
# CAVEAT, and it belongs on the slide: these are ratios of medians, not
# medians of ratios, because make_multi_location_table() summarises across
# sims before they are divided. There is no credible interval here. The check
# below prints the model's own within-sim rate beside the ratio of medians so
# you can see the gap; if it is material, recompute at the array level before
# the number goes into a manuscript.
# ****************************************************************************

EFF.YEARS  <- LAG.YEARS
.ccrit.cov <- as.integer(sub("doxy\\.cov\\.", "", CCRIT))

eff.raw <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("num_cum_doxy_coverage",
                      "num_cum_incidence_averted",
                      "rate_cum_incidence_averted_ppy_doxy"),
    interventions = COVERAGE.LEVELS,
    years         = EFF.YEARS,
    stat.type     = "median")
# The "Dropped N cell(s)" message is expected: num_cum_doxy_coverage and
# rate_cum_incidence_averted_ppy_doxy exist only at the total level, so their
# sex rows are empty by construction.

eff.long <- table_to_long(eff.raw)

doxy.py <- eff.long %>%
    filter(outcome == "num_cum_doxy_coverage", subgroup == "Total") %>%
    select(location, coverage, year, doxy.py = value)

averted.cum <- eff.long %>%
    filter(outcome == "num_cum_incidence_averted",
           subgroup %in% c("Total", "msm", "female")) %>%
    select(location, coverage, year, subgroup, averted = value)

efficiency <- averted.cum %>%
    left_join(doxy.py, by = c("location", "coverage", "year")) %>%
    mutate(py.per.case.averted = doxy.py / averted,
           averted.per.1e5.py  = 1e5 * averted / doxy.py)

# ---- check: ratio of medians vs the model's own within-sim rate ------------
eff.check <- efficiency %>%
    filter(subgroup == "Total") %>%
    select(location, coverage, year, ratio.of.medians = averted.per.1e5.py) %>%
    left_join(eff.long %>%
                  filter(outcome == "rate_cum_incidence_averted_ppy_doxy",
                         subgroup == "Total") %>%
                  select(location, coverage, year, within.sim = value),
              by = c("location", "coverage", "year")) %>%
    mutate(pct.difference = 100 * (ratio.of.medians / within.sim - 1))

cat("\n=== check: ratio of medians vs within-sim rate (cases per 1e5 py) ===\n")
cat("   largest absolute difference across all cities/coverages/years:",
    signif(max(abs(eff.check$pct.difference), na.rm = TRUE), 3), "%\n")
print(as.data.frame(eff.check %>%
                        filter(year == as.integer(.y1),
                               coverage == .ccrit.cov)),
      digits = 4, row.names = FALSE)

# ---- headline table --------------------------------------------------------
eff.headline <- efficiency %>%
    filter(year == as.integer(.y1), coverage == .ccrit.cov) %>%
    select(location, subgroup, doxy.py, averted, py.per.case.averted) %>%
    tidyr::pivot_wider(names_from  = subgroup,
                       values_from = c(averted, py.per.case.averted)) %>%
    arrange(py.per.case.averted_Total)

cat("\n=== Doxy-PEP person-years per case averted,", CCRIT,
    ", cumulative 2022-", .y1, "===\n")
print(as.data.frame(eff.headline), digits = 4, row.names = FALSE)
cat("  doxy.py = MSM person-years on Doxy-PEP; it is the denominator of ALL\n",
    " three columns, including the one for women.\n")

# ---- marginal efficiency: what does the NEXT 10 points of coverage buy? ----
# The average is what a programme reports; the margin is what a programme
# decides on. Diminishing returns show up here first.
marginal <- efficiency %>%
    filter(year == as.integer(.y1), subgroup %in% c("Total", "female")) %>%
    arrange(location, subgroup, coverage) %>%
    group_by(location, subgroup) %>%
    mutate(d.py                 = doxy.py - dplyr::lag(doxy.py),
           d.averted            = averted - dplyr::lag(averted),
           marginal.py.per.case = d.py / d.averted,
           step                 = paste0(dplyr::lag(coverage), "->", coverage)) %>%
    ungroup() %>%
    filter(is.finite(marginal.py.per.case))

cat("\n=== marginal person-years per additional case averted (total pop,",
    .y1, ") ===\n")
print(as.data.frame(marginal %>%
                        filter(subgroup == "Total") %>%
                        select(location, step, marginal.py.per.case) %>%
                        tidyr::pivot_wider(names_from  = step,
                                           values_from = marginal.py.per.case)),
      digits = 4, row.names = FALSE)

# ---- figures ---------------------------------------------------------------
.eff_labels <- c(Total = "all cases averted", female = "cases averted in women")

p.eff <- efficiency %>%
    filter(year == as.integer(.y1), subgroup %in% c("Total", "female")) %>%
    mutate(subgroup = factor(.eff_labels[subgroup],
                             levels = unname(.eff_labels))) %>%
    ggplot(aes(x = coverage, y = py.per.case.averted, colour = location)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_wrap(~ subgroup, scales = "free_y") +
    scale_y_log10() +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, name = NULL) +
    labs(x = "Doxy-PEP coverage among MSM (%)",
         y = "person-years of Doxy-PEP per case averted (log scale)",
         title = "Drug delivered per case averted",
         subtitle = paste0("cumulative 2022-", .y1,
                           "; the denominator is MSM person-years in BOTH panels")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
p.eff
.save_fig(p.eff, .default_fig_dir(),
          "efficiency_py_per_case_averted.png", 10, 5.5, 300)

p.eff.marginal <- marginal %>%
    mutate(subgroup = factor(.eff_labels[subgroup],
                             levels = unname(.eff_labels)),
           step     = factor(step, levels = unique(step[order(coverage)]))) %>%
    ggplot(aes(x = step, y = marginal.py.per.case, colour = location,
               group = location)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_wrap(~ subgroup, scales = "free_y") +
    scale_y_log10() +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, name = NULL) +
    labs(x = "coverage step (%)",
         y = "additional person-years per additional case averted (log scale)",
         title = "Marginal cost, in drug delivered, of each further 10 points of coverage",
         subtitle = paste0("cumulative 2022-", .y1)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
p.eff.marginal
.save_fig(p.eff.marginal, .default_fig_dir(),
          "efficiency_marginal_py_per_case.png", 10, 5.5, 300)


library(tidyverse)

# ****************************************************************************
# HELPERS ----
# ****************************************************************************

## parse_coverage_table ----
#' Convert a wide locations x (outcome_coverage_year) table to long format
#'
#' Works with or without stratification columns. A table with no `subgroup`
#' column behaves exactly as before. A table carrying one can be filtered to a
#' single stratum, or plotted with location x subgroup as the series label.
#'
#' @param tbl Wide data frame: identifier column(s) plus value columns.
#' @param location.col Name of the location column.
#' @param id.cols Additional non-value identifier columns. Any absent from
#'   `tbl` are silently ignored, so the default is safe for totals-only tables.
#' @param subgroup Optional vector of stratum values to keep, e.g. "Total" or
#'   c("Total", "msm"). NULL keeps every row present.
#' @param row.sep Separator used when >1 stratum is retained and the series
#'   label becomes "Atlanta - msm".
#' @return Tibble: location, outcome, coverage (int), year (int), value (num).
parse_coverage_table <- function(tbl,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 subgroup     = NULL,
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ") {
    
    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'.")
    
    # A `stat` column marks estimate vs credible-interval rows. val.cols below
    # is computed as a LEFTOVER, so an unrecognised id column would be treated
    # as a value column and trip the col.pattern check. These figures plot
    # point estimates, so keep the estimates and drop the marker. (This also
    # removes the CI rows, which previously became NA values downstream.)
    if ("stat" %in% names(tbl)) {
        tbl <- tbl[as.character(tbl$stat) == "estimate", , drop = FALSE]
        tbl$stat <- NULL
    }
    
    # identifier columns actually present; anything matching col.pattern is a
    # value column and can never be an identifier
    id.cols <- intersect(id.cols, names(tbl))
    if (length(id.cols) > 0)
        id.cols <- id.cols[is.na(stringr::str_match(id.cols, col.pattern)[, 1])]
    
    # ---- optional stratum filter ------------------------------------------
    if (!is.null(subgroup)) {
        if (length(id.cols) == 0)
            stop("'subgroup' supplied but 'tbl' has no stratification column.")
        keep <- Reduce(`|`, lapply(tbl[id.cols],
                                   function(x) as.character(x) %in% subgroup))
        if (!any(keep))
            stop("No rows match subgroup = ", paste(subgroup, collapse = ", "),
                 ".\nAvailable: ",
                 paste(sort(unique(as.character(unlist(tbl[id.cols])))),
                       collapse = ", "))
        tbl <- tbl[keep, , drop = FALSE]
    }
    
    # ---- parse value columns ----------------------------------------------
    val.cols <- setdiff(names(tbl), c(location.col, id.cols))
    if (length(val.cols) == 0)
        stop("No value columns left after removing identifiers.")
    
    parts <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "),
             "\nIf these are identifier columns, add them to 'id.cols'.")
    
    long <- tbl %>%
        select(all_of(c(location.col, id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = as.integer(parts[, 4])),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # ---- series label ------------------------------------------------------
    # append the stratum only when more than one is shown, so a totals-only or
    # single-stratum table keeps clean city names
    strat.col <- if (length(id.cols) > 0) id.cols[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) > 1)
        long <- long %>%
        mutate(location = paste0(location, row.sep, .data[[strat.col]]))
    
    long %>% select(location, outcome, coverage, year, value)
}


## .prep_long ----
#' Internal: accept either a wide table or an already-long one
.prep_long <- function(tbl, location.col, id.cols, subgroup, col.pattern, row.sep) {
    if (all(c("coverage", "value") %in% names(tbl))) {
        # already long -- apply the same stratum filter if a strat column exists
        sc <- intersect(id.cols, names(tbl))
        if (!is.null(subgroup)) {
            if (length(sc) == 0)
                stop("'subgroup' supplied but the long table has no stratification column.")
            tbl <- tbl %>% filter(if_any(all_of(sc), ~ as.character(.x) %in% subgroup))
            if (nrow(tbl) == 0) stop("No rows match subgroup = ",
                                     paste(subgroup, collapse = ", "))
        }
        if (length(sc) > 0 && dplyr::n_distinct(tbl[[sc[1]]]) > 1)
            tbl <- tbl %>% mutate(location = paste0(location, row.sep,
                                                    .data[[sc[1]]]))
        return(tbl)
    }
    parse_coverage_table(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
}

## .filter_locations ----
#' Internal: filter locations by full label, base city name, or glob pattern
.filter_locations <- function(long, locations, row.sep = " \u2014 ") {
    if (is.null(locations)) return(long)
    
    base.loc <- sub(paste0(row.sep, ".*$"), "", long$location)
    keep <- long$location %in% locations | base.loc %in% locations
    
    globs <- locations[grepl("[*?]", locations)]
    if (length(globs) > 0)
        keep <- keep | Reduce(`|`, lapply(globs, function(g)
            grepl(utils::glob2rx(g), long$location)))
    
    if (!any(keep))
        stop("No rows match locations = ", paste(locations, collapse = ", "),
             ".\nAvailable cities: ", paste(sort(unique(base.loc)), collapse = ", "),
             "\nAvailable series: ", paste(sort(unique(long$location)), collapse = ", "))
    
    long[keep, , drop = FALSE]
}

## .strat_suffix ----
#' Internal: append the stratum to a title when exactly one was selected
.strat_suffix <- function(subgroup)
    if (!is.null(subgroup) && length(subgroup) == 1) paste0(" (", subgroup, ")") else ""


## .save_fig ----
#' Internal: save a figure if a path was supplied
.save_fig <- function(p, save.path, width, height, dpi) {
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    invisible(p)
}


# ****************************************************************************
# FIGURE 1: Ranked bar -- coverage needed to reach the target ----
# ****************************************************************************

## plot_coverage_needed ----
#' Minimum coverage required to reach a target impact, ranked by city
#'
#' @param tbl Wide table, or the long output of parse_coverage_table().
#' @param target Impact threshold, e.g. 50 for a 50% reduction.
#' @param year Which year to evaluate. Defaults to the latest in the data.
#' @param subgroup Optional stratum filter, e.g. "Total" or "msm".
#' @param higher.is.better TRUE if larger values are the goal (e.g. % averted).
#' @param locations Optional subset of locations to show.
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 subgroup     = NULL,
                                 higher.is.better = TRUE,
                                 locations    = NULL,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = "#2166AC",
                                 unreached.lab = "not reached",
                                 save.path = NULL, width = 7, height = 4.5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    hit <- function(v) if (higher.is.better) v >= target else v <= target
    
    summ <- long %>%
        group_by(location) %>%
        summarise(cov.needed = suppressWarnings(min(coverage[hit(value)])),
                  best       = if (higher.is.better) max(value, na.rm = TRUE)
                  else min(value, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(reached  = is.finite(cov.needed),
               bar.len  = ifelse(reached, cov.needed, max.cov),
               lab      = ifelse(reached, paste0(cov.needed, "%"),
                                 paste0(unreached.lab, " (max ",
                                        round(best), "%)"))) %>%
        arrange(desc(reached), cov.needed, desc(best)) %>%
        mutate(location = factor(location, levels = rev(location)))
    
    if (is.null(title))
        title <- paste0("Doxy-PEP coverage needed to reach ", target,
                        "% reduction by ", year, .strat_suffix(subgroup))
    
    p <- ggplot(summ, aes(x = bar.len, y = location)) +
        geom_col(aes(fill = reached, color = reached),
                 linewidth = 0.6, width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = lab, hjust = ifelse(reached, -0.15, 1.05),
                      color = reached),
                  size = 3.3, fontface = "bold", show.legend = FALSE) +
        scale_fill_manual(values  = c(`TRUE` = bar.fill, `FALSE` = "grey95")) +
        scale_color_manual(values = c(`TRUE` = bar.fill, `FALSE` = "grey45")) +
        scale_x_continuous(limits = c(0, max.cov * 1.25),
                           breaks = seq(0, max.cov, by = 20), expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              axis.ticks         = element_blank(),
              plot.title         = element_text(face = "bold", size = 12))
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# ****************************************************************************
# FIGURE 2: Dose-response -- impact vs coverage ----
# ****************************************************************************

## plot_dose_response ----
#' Impact as a function of coverage, at a fixed year
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               subgroup     = NULL,
                               locations    = NULL,
                               location.col = "location",
                               id.cols      = c("subgroup", "outcome.group"),
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               row.sep      = " \u2014 ",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.path = NULL, width = 7.5, height = 5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    # order labels by terminal impact so the key reads as a ranking
    ord <- long %>%
        group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location)
    long <- long %>% mutate(location = factor(location, levels = ord))
    
    ends <- long %>% group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>% ungroup()
    
    if (is.null(y.lab)) y.lab <- paste0(unique(long$outcome), collapse = " / ")
    if (is.null(title))
        title <- paste0(y.lab, " by coverage level, ", year, .strat_suffix(subgroup))
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    
    p <- ggplot(long, aes(x = coverage, y = value,
                          color = location, group = location)) +
        geom_hline(yintercept = target, linetype = "dashed",
                   color = "grey35", linewidth = 0.5) +
        annotate("text", x = 0, y = target, label = paste0(target, "% target"),
                 hjust = -0.05, vjust = -0.6, size = 3, color = "grey35") +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.6) +
        scale_x_continuous(breaks = sort(unique(long$coverage)),
                           limits = c(0, max.cov * ifelse(direct.label, 1.28, 1.02))) +
        labs(x = x.lab, y = y.lab, title = title, color = NULL) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (direct.label) "none" else "right")
    
    if (direct.label)
        p <- p + geom_text(data = ends, aes(label = location),
                           hjust = -0.15, size = 3.1, fontface = "bold",
                           show.legend = FALSE)
    
    if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# ****************************************************************************
# FIGURE 3: Impact over time -- x axis is year ----
#   1. one city, one coverage      -> single trajectory
#   2. one city, many coverages    -> fan of curves (color.by = "coverage")
#   3. many cities, many coverages -> small multiples
# ****************************************************************************
# .strat_labeller ----
#' Build a labelling function for stratum display names
#'
#' Returns identity when `map` is NULL or empty, so callers that pass nothing
#' keep the raw factor levels. Levels absent from `map` are passed through
#' unchanged rather than becoming NA.
#' @param map Named character vector: names are stratum values, values are labels.
#' @noRd
.strat_labeller <- function(map) {
    if (is.null(map) || length(map) == 0) return(function(x) x)
    function(x) {
        x   <- as.character(x)
        out <- unname(map[x])
        ifelse(is.na(out), x, out)
    }
}
# .make_labeller ----
#' Build a labelling function from a flexible spec
#'
#' Accepts NULL (identity), a function, a named character vector (value -> label
#' lookup, unmapped values pass through), or a single unnamed string used as a
#' template in which `{x}` is replaced by the value.
#' @noRd
.make_labeller <- function(spec) {
    if (is.null(spec)) return(function(x) as.character(x))
    if (is.function(spec)) return(function(x) as.character(spec(x)))
    if (!is.null(names(spec)) && any(nzchar(names(spec))))
        return(.strat_labeller(spec))
    if (length(spec) == 1)
        return(function(x) vapply(as.character(x),
                                  function(v) gsub("{x}", v, spec, fixed = TRUE),
                                  character(1), USE.NAMES = FALSE))
    stop("Label spec must be NULL, a function, a named vector, or a single template string.")
}
# plot_impact_over_time ----
#' Trajectory plot of impact over time
#'
#' Draws one line per series, where a series is a location x coverage x stratum
#' combination. Colour is assigned to the stratum when more than one stratum is
#' present, otherwise to `color.by`; the remaining dimensions become facets.
#'
#' Ordering: when `locations` and/or `subgroup` are supplied, their order is
#' respected in facets, legends and line stacking. Otherwise locations are
#' ranked by their endpoint value at the highest coverage.
#'
#' @param tbl Wide table as produced upstream.
#' @param color.by Dimension mapped to colour when only one stratum is present.
#' @param locations,coverages,subgroup Optional subsets. Supplied order is kept.
#' @param strat.palette ColorBrewer palette used when the stratum takes the
#'   colour channel.
#' @param strat.labels Named character vector mapping stratum values to display
#'   labels for the legend. Unmapped levels pass through unchanged; NULL keeps
#'   the raw values.
#' @param year.range Two-element numeric range, inclusive.
#' @param target Horizontal reference line; NULL to omit.
#' @param outcome Required when the table holds more than one outcome.
#' @param x.lab,y.lab Axis labels. `y.lab` defaults to the outcome name.
#' @param title Overall plot title; NULL auto-generates one, NA suppresses it.
#' @param loc.labels Panel titles for location facets. NULL keeps the raw
#'   location names; supply a named character vector to rename them, or a
#'   function.
#' @param cov.label Panel titles for coverage facets. A template string in which
#'   `{x}` is replaced by the coverage value, a named vector, or a function.
#' @param show.strip FALSE hides all panel titles.
#' @param free.y Free y scales across facets.
#' @param direct.label End-of-line labels when colouring by location.
#' @param annotate.ends Endpoint value labels; single-series plots only.
#' @return A ggplot object.
plot_impact_over_time <- function(tbl,
                                  color.by      = c("coverage", "location"),
                                  locations     = NULL,
                                  coverages     = NULL,
                                  year.range    = NULL,
                                  target        = 50,
                                  outcome       = NULL,
                                  subgroup      = NULL,
                                  location.col  = "location",
                                  id.cols       = c("subgroup", "outcome.group"),
                                  col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  row.sep       = " \u2014 ",
                                  facet.ncol    = NULL,
                                  loc.labels    = NULL,
                                  cov.label     = "{x}% coverage",
                                  show.strip    = TRUE,
                                  free.y        = FALSE,
                                  direct.label  = TRUE,
                                  annotate.ends = TRUE,
                                  strat.palette = "Set1",
                                  strat.labels  = c(Total             = "Total population",
                                                    msm               = "MSM",
                                                    heterosexual_male = "Heterosexual men",
                                                    female            = "Women"),
                                  x.lab         = "Year",
                                  y.lab         = NULL,
                                  title         = NULL,
                                  save.path = NULL, width = 10, height = 6, dpi = 300) {
    
    color.by <- match.arg(color.by)
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")
    
    # ---- subsets ----------------------------------------------------------
    long <- .filter_locations(long, locations, row.sep)
    if (!is.null(coverages)) {
        miss <- setdiff(coverages, unique(long$coverage))
        if (length(miss)) stop("Coverage level(s) not in table: ", paste(miss, collapse = ", "))
        long <- long %>% filter(coverage %in% coverages)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))
    
    if (nrow(long) == 0) stop("No rows left after filtering.")
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years. Build the table with years = as.character(2026:2035).")
    
    # ---- stratification column -------------------------------------------
    # resolved before any of the dimension counts are used
    strat.col <- intersect(id.cols, names(long))
    strat.col <- if (length(strat.col)) strat.col[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) < 2)
        strat.col <- NULL
    
    # ---- ordering ---------------------------------------------------------
    # user-supplied order wins; anything unmatched is appended rather than dropped
    u.loc <- as.character(unique(long$location))
    ord.loc <- if (!is.null(locations)) {
        c(intersect(locations, u.loc), setdiff(u.loc, locations))
    } else {
        long %>%
            filter(coverage == max(coverage)) %>%
            group_by(location) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
            arrange(desc(value)) %>% pull(location) %>% as.character()
    }
    long <- long %>% mutate(location = factor(as.character(location), levels = ord.loc))
    
    if (!is.null(strat.col)) {
        u.str <- as.character(unique(long[[strat.col]]))
        ord.str <- if (!is.null(subgroup))
            c(intersect(subgroup, u.str), setdiff(u.str, subgroup)) else u.str
        long[[strat.col]] <- factor(as.character(long[[strat.col]]), levels = ord.str)
    }
    
    long <- long %>%
        mutate(coverage = as.numeric(coverage), year = as.numeric(year))
    
    # ---- series identifier, ordered location -> subgroup -> coverage ------
    if (!is.null(strat.col)) {
        long <- long %>%
            arrange(location, .data[[strat.col]], coverage) %>%
            mutate(series = paste(location, .data[[strat.col]], coverage, sep = "|"))
    } else {
        long <- long %>%
            arrange(location, coverage) %>%
            mutate(series = paste(location, coverage, sep = "|"))
    }
    long <- long %>% mutate(series = factor(series, levels = unique(series)))
    
    # ---- dimension counts -------------------------------------------------
    n.loc   <- dplyr::n_distinct(long$location)
    n.cov   <- dplyr::n_distinct(long$coverage)
    n.strat <- if (!is.null(strat.col)) dplyr::n_distinct(long[[strat.col]]) else 1L
    single.line <- (n.loc == 1 && n.cov == 1 && n.strat == 1)
    
    # ---- colour and facet assignment --------------------------------------
    # stratum takes the colour channel whenever it varies; location and
    # coverage then compete for the facet channel
    strat.colour <- n.strat > 1
    if (strat.colour) {
        facet.dims <- c(if (n.loc > 1) "location", if (n.cov > 1) "coverage")
    } else {
        fb <- setdiff(c("coverage", "location"), color.by)
        facet.dims <- if ((fb == "location" && n.loc > 1) ||
                          (fb == "coverage" && n.cov > 1)) fb else character(0)
    }
    
    # ---- labels -----------------------------------------------------------
    if (is.null(y.lab)) y.lab <- unique(long$outcome)
    if (length(title) == 1 && is.na(title)) {
        title <- NULL
    } else if (is.null(title)) {
        title <- if (single.line)
            paste0(y.lab, ": ", levels(droplevels(long$location))[1],
                   " at ", unique(long$coverage), "% coverage")
        else if (strat.colour)
            paste0(y.lab, " over time, by subgroup")
        else if (color.by == "coverage")
            paste0(y.lab, " over time, by coverage level", .strat_suffix(subgroup))
        else
            paste0(y.lab, " over time, by city", .strat_suffix(subgroup))
    }
    
    use.direct.label <- direct.label && !strat.colour &&
        color.by == "location" && !single.line
    max.yr <- max(long$year, na.rm = TRUE)
    pad    <- if (use.direct.label) 4 else 0
    
    # ---- base plot --------------------------------------------------------
    p <- ggplot(long, aes(x = year, y = value))
    
    if (!is.null(target))
        p <- p + geom_hline(yintercept = target, linetype = "dashed",
                            color = "grey35", linewidth = 0.4)
    
    if (single.line) {
        p <- p + geom_line(linewidth = 1, color = "#2166AC") +
            geom_point(size = 1.8, color = "#2166AC")
        if (annotate.ends) {
            ends <- long %>% filter(year %in% range(year))
            p <- p + geom_text(data = ends,
                               aes(label = paste0(round(value, 1), "%")),
                               vjust = -1, size = 3.2, fontface = "bold",
                               color = "#2166AC")
        }
        
    } else if (strat.colour) {
        p <- p +
            geom_line(aes(color = .data[[strat.col]], group = series),
                      linewidth = 0.9) +
            geom_point(aes(color = .data[[strat.col]]), size = 1.5) +
            scale_color_brewer(palette = strat.palette, name = NULL, drop = FALSE,
                               labels = .strat_labeller(strat.labels))
        
    } else if (color.by == "coverage") {
        p <- p +
            geom_line(aes(color = coverage, group = series), linewidth = 0.9) +
            scale_color_viridis_c(option = "C", end = 0.92,
                                  name = "Doxy-PEP\ncoverage (%)",
                                  breaks = sort(unique(long$coverage)))
        
    } else {
        p <- p + geom_line(aes(color = location, group = series), linewidth = 0.85)
        if (use.direct.label) {
            ends <- long %>% group_by(series) %>%
                slice_max(year, n = 1, with_ties = FALSE) %>% ungroup()
            p <- p + geom_text(data = ends, aes(label = location, color = location),
                               hjust = -0.1, size = 2.9, fontface = "bold",
                               show.legend = FALSE)
        }
    }
    
    # ---- faceting ---------------------------------------------------------
    loc.fn <- .make_labeller(loc.labels)
    cov.fn <- .make_labeller(cov.label)
    if (length(facet.dims) == 2) {
        p <- p + facet_grid(location ~ coverage,
                            labeller = labeller(location = loc.fn, coverage = cov.fn),
                            scales = if (free.y) "free_y" else "fixed")
    } else if (length(facet.dims) == 1) {
        lab.fn <- if (facet.dims == "coverage") as_labeller(cov.fn) else as_labeller(loc.fn)
        p <- p + facet_wrap(vars(.data[[facet.dims]]), ncol = facet.ncol,
                            labeller = lab.fn,
                            scales = if (free.y) "free_y" else "fixed")
    }
    
    p <- p +
        scale_x_continuous(limits = c(min(long$year), max.yr + pad)) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              strip.text       = if (show.strip) element_text(face = "bold")
              else element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (single.line || use.direct.label) "none" else "right")
    
    .save_fig(p, save.path, width, height, dpi)
    p
}

# examples ----
if (1==2){
    # These examples write to the driver script's FIG.DIR. Running them on their own,
    # set the calibration name first and the folder follows from it.
    if (!exists("CALIB.NAME")) CALIB.NAME <- "calib.8.21.stage3.az"
    FIG.DIR <- shield.fig.path(CALIB.NAME, create = TRUE)
    
    # --- Figure 1: headline ------------------------------------------------------
    # Needs a table using a single year (e.g., 2035) and spanning multiple locations. Build it with the multi-location
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    f1 <- plot_coverage_needed(pct.inc.ave.tbl,
                               target    = 50,
                               title = "Doxy coverage needed to reach 50% incidence reduction among MSM by 2030",
                               save.path = paste0(FIG.DIR, "fig1_coverage_needed.png"))
    f1
    # --- Figure 2: dose-response -------------------------------------------------
    f2 <- plot_dose_response(pct.inc.ave.tbl,
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             save.path = paste0(FIG.DIR, "fig2_dose_response_msm.png"))
    
    f2
    # comparing the 3 sexes in a single city
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        save          = F
    )
    # all groups in Atlanta
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("Atlanta *" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    # all MSM in different cities
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("* — msm" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    
    # --- Figure 3: trajectories --------------------------------------------------
    
    traj.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = as.character(c(2022:2040)),
        stat.type     = "median",
        save          = F
    )
    
    # ---- View 1: one city, one scenario ----------------------------------------
    plot_impact_over_time(traj.tbl,
                          locations = c("Baltimore — *"    ),
                          coverages = 10,
                          year.range = c(2022, 2040),
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 2: one city, all coverage levels ---------------------------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("Baltimore — *"    ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          y.lab      = "Diagnoses averted in Baltimore (%)",
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 3: all cities, all coverage levels (small multiples) -------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("* — female" ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          facet.ncol = 5,
                          y.lab      = "Diagnoses averted (%)"
                          # save.path = paste0(FIG.DIR, "fig3_multi_by_coverage.png"
    ) 
    
    # ---- View 3b: flipped -- panels are coverage levels, lines are cities ------
    plot_impact_over_time(traj.tbl,
                          color.by   = "location",
                          coverages  = c(10, 30, 60, 90),
                          year.range = c(2026, 2035),
                          facet.ncol = 4,
                          save.path = paste0(FIG.DIR, "fig3_multi_by_location.png") )
    
}

# Critical coverage Plot (C_crit) ----
# order.by = "value" here if you want the cities RANKED by coverage needed
# rather than kept in the common order.
# these two are RANKED by coverage needed rather than kept in the common
# city order -- here the ranking is the message. order.by makes that
# explicit instead of leaving it implied by a commented-out argument.
{
    tbl.rel.2030 = make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = as.character(year),
        stat.type     = "median",
        save          = F
    );
    plot_coverage_needed(tbl.rel.2030,
                         locations = MSAS, order.by = "value",
                         target    = 50,
                         subgroup = "msm",
                         title = paste0("Doxy-PEP coverage needed for a 50% incidence reduction among MSM by ", year),
                         filename=paste0("crit_cov_msm_",EVAL.YEAR,".png"))
    
    plot_coverage_needed(tbl.rel.2030,
                         locations = MSAS, order.by = "value",
                         target    = 50,
                         subgroup = "Total",
                         title = paste0("Doxy-PEP coverage needed for a 50% incidence reduction, total population, by ", year),
                         filename=paste0("crit_cov_total_",EVAL.YEAR,".png"))
    
    
}

# TREND BY LOCATIONS SIDE BY SIDE ----
## No intervention: how does incidence change to 2030 and 2035?---
#how is incidence changing in each city, for het vs MSM? 
# Heterosexual incidence rises in nine of ten cities. Atlanta is not an exception — it rises, just slowly (~×1.3 over 2022–2030) while MSM rises faster (~×1.5). Baltimore is the only city where heterosexual incidence falls, and MSM falls there too.
{
    noint_tbl = make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("incidence","rate_incidence_per_pop"),
        interventions = "noint",
        years         = as.character(c(2022:2035)),
        stat.type     = "median.ci",
        # filter.by.strat = "msm",
        save          = F
    ); 
    
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("Total"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "incidence",
                       y.lab = "Incident Cases",
                       filename = "noint_inc_total.png"
    )
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("female","heterosexual_male","msm"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "incidence",
                       y.lab = "Incident Cases",
                       filename = "noint_inc_subgroup.png"
    )
    
    plot_trend_with_ci(tbl = noint_tbl,
                       locations = MSAS,
                       interventions = "noint",
                       subgroup = c("female","heterosexual_male","msm"),
                       free.y = T,
                       facet.ncol = 5,
                       outcome = "rate_incidence_per_pop",
                       y.lab = "Incidence Rate (per 100,000 py)",
                       filename = "noint_inc_rate.png"
    )
}


## %incidence averted at diff doxy-PEP: Locations side by side ----
{
    traj.tbl = make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = as.character(c(2022:2035)),
        stat.type     = "median",
        save          = F
        # filename = "t"
    )
    # incidence averted accross doxy sccenarios among total population
    plot_impact_over_time(traj.tbl,
                          locations = MSAS,
                          coverages = seq(10,50,10),
                          subgroup = "Total", target = 50, facet.ncol = 5,
                          # subgroup = "msm", target = 50, facet.ncol = 5,
                          # subgroup = "female", target = 50, facet.ncol = 5,
                          y.lab = "%Incidence averted",
                          title = "%Incidence averted among total population over time (at various Doxy-PEP coverages)",
                          # title = "%Incidence averted among MSM over time (at various Doxy-PEP coverages)",
                          # title = "%Incidence averted among women over time (at various Doxy-PEP coverages)",
                          year.range = c(2022, 2035),
                          filename = "inc_averted_doxy_total.png"
    )
}


## minimum C_crit of 40%, how does the impact builds up over time? ----
{
    c_crit = 40
    traj.tbl.ccrit = make_multi_location_table(
        data          = results,          # was list(total_results, sex_results) -- neither object exists here
        locations     = MSAS,
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.", c_crit),
        years         = as.character(c(2022:2035)),
        stat.type     = "median",
        save          = F
    )
    plot_impact_over_time(traj.tbl.ccrit,
                          locations = MSAS,
                          coverages = c_crit,
                          subgroup = c("Total","msm","heterosexual_male","female"),
                          target = 90, facet.ncol = 5,
                          y.lab = "%Incidence averted",
                          title = "%Incidence averted over time by subgroup (Doxy-PEP critical coverage of 40%)",
                          year.range = c(2022, 2035))
    
}


# WHAT EXPLAINS THE BETWEEN-CITY HETEROGENEITY? ----
#
# Reading the figures above:
#   * the MSM effect is nearly flat across the ten MSAs;
#   * total-population impact spans a five-fold range;
#   * so the spread must come from how much the intervention suppresses the
#     HETEROSEXUAL epidemic, not from how well it works in MSM.
#
# This block puts numbers on that. It produces:
#   1. one row per city: % averted in each subgroup, the MSM share of incident
#      cases at the start and end of the projection, and a "spillover ratio"
#      (% averted in women / % averted in MSM);
#   2. an arithmetic CHECK -- the share-weighted sum of the subgroup effects
#      should reproduce the model's own total-population effect;
#   3. two scatter plots.
CCRIT     <- "doxy.cov.50"     # coverage scenario to evaluate at
EVAL.YEAR <- "2030"            # year to evaluate at
BASE.YEAR <- "2022"
# ---- SPILLOVER ----
##1: Put data together >>>>
# ---- % incidence averted, by subgroup, at one coverage and one year --------
impact.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = EVAL.YEAR,
    stat.type     = "median")

# ---- each subgroup's share of incident cases under no intervention --------
share.eval.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_share",
    interventions = "noint",
    years         = c(BASE.YEAR, EVAL.YEAR),
    stat.type     = "median")

## reformat % incidence averted: rows location; columns subgroup: 
av <- table_to_long(impact.tbl) %>%
    select(location, subgroup, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value,
                       names_prefix = "av_")

# reformat MSM share: rows locations, columns %share in 2022 and 2030
sh.msm <- table_to_long(share.eval.tbl) %>%
    filter(subgroup == "msm") %>%
    select(location, year, value) %>%
    tidyr::pivot_wider(names_from = year, values_from = value,
                       names_prefix = "msm_share_")
# join:
spillover.tbl <- left_join(av, sh.msm, by = "location") %>%
    mutate(spillover = av_female / av_msm) %>%
    arrange(desc(spillover))

cat("\n=== % incidence averted at", CCRIT, "in", EVAL.YEAR,
    ", by city and subgroup ===\n")
print(as.data.frame(spillover.tbl), digits = 3, row.names = FALSE)

##2: Test association: >>>>>
# ---- does the MSM share predict the spillover? -----------------------------
# If the correlation is strong at BASE.YEAR 2022 but weak at EVAL.YEAR 2030, the message
# is that what matters is how much of the epidemic MSM drove at the START --
# by the end, the heterosexual epidemic they seeded has grown past them.
cat("\n=== Spearman correlation: MSM share vs spillover ratio ===\n")
print(round(c(
    `share at start` = cor(spillover.tbl[[paste0("msm_share_", BASE.YEAR)]],
                           spillover.tbl$spillover, method = "spearman"),
    `share at end`   = cor(spillover.tbl[[paste0("msm_share_", EVAL.YEAR)]],
                           spillover.tbl$spillover, method = "spearman")), 3))
# the correlation is strong at both years. It matters how much are their academic is driven by MSM both at the store and at the end

##3: Check aritmatic >>>> This does check out as it should
# ---- arithmetic check ------------------------------------------------------
# total % averted should equal the share-weighted sum of the subgroup effects,
# with the shares taken from the SAME (no-intervention) year. If these do not
# agree, the decomposition argument does not hold and something is off.
# sh.all.2030 <- table_to_long(share.eval.tbl) %>%
#     filter(year == as.integer(EVAL.YEAR),
#            subgroup %in% c("msm", "heterosexual_male", "female")) %>%
#     select(location, subgroup, share = value)
# 
# check <- table_to_long(impact.tbl) %>%
#     filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
#     select(location, subgroup, averted = value) %>%
#     left_join(sh.all.2030, by = c("location", "subgroup")) %>%
#     group_by(location) %>%
#     summarise(predicted.total = sum(share * averted) / 100, .groups = "drop") %>%
#     left_join(av %>% select(location, model.total = av_Total), by = "location") %>%
#     mutate(difference = predicted.total - model.total)
# 
# cat("\n=== check: share-weighted subgroup effects vs the model's own total ===\n")
# print(as.data.frame(check), digits = 4, row.names = FALSE)
# cat("   largest absolute discrepancy:",
#     signif(max(abs(check$difference), na.rm = TRUE), 3), "percentage points\n")
# cat("   NOTE: get_stats() rounds to whole numbers, so a discrepancy of one or\n",
#     "  two percentage points is rounding. Several points would mean the\n",
#     "  decomposition does not hold and something needs looking at.\n")


## .label_layer ----
# ---- figures ---------------------------------------------------------------
# ggrepel keeps the city labels from overlapping; fall back to plain text if
# it is not installed rather than failing at the end of a long script.
.label_layer <- function() {
    if (requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(aes(label = location), size = 3.2)
    else
        geom_text(aes(label = location), size = 3.2, hjust = -0.15, vjust = -0.4)
}

# Scatterplot: ----
## 1- %incidence averted total vs %incident averted women ----
p.tot.vs.fem <- ggplot(spillover.tbl, aes(x = av_female, y = av_Total)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    scale_x_continuous(expand = expansion(mult = c(0.08, 0.18))) +
    labs(x = "% incidence averted among women",
         y = "% incidence averted, total population",
         title = paste0("Total-population impact tracks the effect in women (",
                        CCRIT, ", ", EVAL.YEAR, ")"),
         subtitle = "dashed line is 1:1") +
    theme_minimal(base_size = 11); p.tot.vs.fem
# .save_fig(p.tot.vs.fem, .default_fig_dir(), "explain_total_vs_female.png", 6, 5, 300)

## 2- spillover against the MSM share ----
p.spill <- spillover.tbl %>%
    tidyr::pivot_longer(dplyr::starts_with("msm_share_"),
                        names_to = "when", values_to = "msm.share") %>%
    mutate(when = sub("msm_share_", "MSM share in ", when)) %>%
    ggplot(aes(x = msm.share, y = spillover)) +
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    facet_wrap(~ when, scales = "free_x") +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.22))) +
    labs(x = "MSM share of incident cases under no intervention (%)",
         y = "spillover ratio  (% averted in women / % averted in MSM)",
         title = "How much of the MSM-side benefit reaches women") +
    theme_minimal(base_size = 11);p.spill
# .save_fig(p.spill, .default_fig_dir(), "explain_spillover_vs_share.png", 9, 4.5, 300)


## ----
# ---- is it the MSM TREND rather than the MSM SHARE? ------------------------
# A city where the MSM epidemic keeps pace with the heterosexual one leaves
# Doxy-PEP something to work on; a city where MSM incidence is already falling
# under no intervention while heterosexual incidence takes off leaves very
# little population-level curve to bend.
#
# The discriminating quantity is not the MSM trend on its own -- Baltimore's
# MSM incidence falls too, and it has one of the highest spillovers, because
# its heterosexual epidemic is falling as well. It is the DIVERGENCE between
# the two trends:
#
#     divergence = log( heterosexual growth / MSM growth )
#
# negative when MSM keep pace or better, strongly positive where the
# heterosexual epidemic is running away from them.
growth.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "incidence",
    interventions = "noint",
    years         = c(BASE.YEAR, EVAL.YEAR),
    stat.type     = "median")

g <- table_to_long(growth.tbl) %>%
    filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
    mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
    group_by(location, grp, year) %>%
    summarise(inc = sum(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
    mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
               .data[[paste0("inc_", BASE.YEAR)]]) %>%
    select(location, grp, growth) %>%
    tidyr::pivot_wider(names_from = grp, values_from = growth, names_prefix = "growth_") %>%
    mutate(divergence = log(growth_het / growth_msm))

tbl2 <- left_join(spillover.tbl, g, by = "location") %>% arrange(desc(spillover))

cat("\n=== MSM vs heterosexual growth,", BASE.YEAR, "->", EVAL.YEAR, "(no intervention) ===\n")
print(as.data.frame(tbl2[, c("location", "av_msm", "av_female", "spillover",
                                       "growth_msm", "growth_het", "divergence")]),
      digits = 3, row.names = FALSE)

cat("\n=== which candidate explains the spillover best? (Spearman) ===\n")
print(round(c(
    `MSM share at start` = cor(tbl2[[paste0("msm_share_", BASE.YEAR)]],
                               tbl2$spillover, method = "spearman"),
    `MSM share at end`   = cor(tbl2[[paste0("msm_share_", EVAL.YEAR)]],
                               tbl2$spillover, method = "spearman"),
    `MSM growth alone`   = cor(tbl2$growth_msm, tbl2$spillover, method = "spearman"),
    `divergence`         = cor(tbl2$divergence, tbl2$spillover, method = "spearman")), 3))
cat("  (divergence should be NEGATIVE: the further the heterosexual epidemic\n",
    "  runs ahead of the MSM one, the less of the MSM benefit reaches women)\n")

p.diverge <- ggplot(tbl2, aes(x = divergence, y = spillover)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(size = 3, colour = "#2166AC") +
    .label_layer() +
    scale_x_continuous(expand = expansion(mult = c(0.10, 0.20))) +
    labs(x = paste0("log(heterosexual growth / MSM growth), ",
                    BASE.YEAR, "-", EVAL.YEAR, ", no intervention"),
         y = "spillover ratio  (% averted in women / % averted in MSM)",
         title = "Where the heterosexual epidemic outruns the MSM epidemic, Doxy-PEP has less to work on",
         subtitle = "left of the dashed line: MSM keeping pace or better") +
    theme_minimal(base_size = 11)
p.diverge
.save_fig(p.diverge, .default_fig_dir(), "explain_spillover_vs_divergence.png", 8, 5.5, 300)


# ****************************************************************************
# TRANSMISSION COUPLING: HOW FAR THE MSM EFFECT CARRIES INTO WOMEN
# ****************************************************************************
#
# The spillover ratio uses a single coverage level. This uses the whole
# coverage gradient: within each city, regress the effect in women on the
# effect in MSM across all coverage scenarios, through the origin (zero
# coverage gives zero effect in both, by construction). The slope is a
# per-city coupling coefficient estimated from ten points rather than one.
#
#   slope near 1   the MSM effect carries fully into women -- heterosexual
#                  transmission largely depends on the MSM network
#   slope near 0   it does not -- the heterosexual epidemic is self-sustaining
#
# `r2` is worth reading too: a low value means the relationship is curved
# rather than proportional, and the single slope is hiding that.
#
# `residual.het` is the complementary bound: the percentage of incidence in
# women still occurring at the HIGHEST coverage. Whatever survives near-total
# MSM coverage is, by construction, not sustained by the MSM network. It is an
# upper bound rather than a point estimate, because Doxy-PEP does not fully
# suppress MSM incidence even at 100% coverage.
# ****************************************************************************

coupling.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median")

cpl <- table_to_long(coupling.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, coverage, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value)

coupling <- cpl %>%
    group_by(location) %>%
    summarise(slope        = coef(lm(female ~ 0 + msm))[["msm"]],
              r2           = summary(lm(female ~ 0 + msm))$r.squared,
              residual.het = 100 - female[which.max(msm)],
              x            = max(msm),
              y            = female[which.max(msm)],
              .groups = "drop") %>%
    arrange(desc(slope))

cat("\n=== transmission coupling by city (", EVAL.YEAR, ") ===\n")
print(as.data.frame(coupling[, c("location", "slope", "r2", "residual.het")]),
      digits = 3, row.names = FALSE)
cat("  slope        rise in women's % averted per point of MSM % averted\n")
cat("  residual.het % of women's incidence remaining at the highest coverage\n")
cat("               (upper bound on the self-sustaining heterosexual fraction)\n")

# ---- figure ---------------------------------------------------------------
# order everything by slope so the palette runs with the ranking
cpl$location      <- factor(cpl$location,      levels = coupling$location)
coupling$location <- factor(coupling$location, levels = coupling$location)

# Spread the end-of-line labels so they never collide. Done arithmetically
# rather than with ggrepel so the figure looks the same on any machine.
coupling <- coupling[order(coupling$y), ]
min.gap  <- 0.05 * diff(range(cpl$female, na.rm = TRUE))
coupling$y.lab <- coupling$y
for (i in seq_len(nrow(coupling))[-1])
    coupling$y.lab[i] <- max(coupling$y[i], coupling$y.lab[i - 1] + min.gap)

x.pad <- 0.04 * diff(range(cpl$msm, na.rm = TRUE))

p.coupling <- ggplot(cpl, aes(msm, female, colour = location)) +
    geom_abline(slope = 1, linetype = "dashed", colour = "grey55") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    # a short leader from each line's end to its (possibly nudged) label
    geom_segment(data = coupling, colour = "grey70", linewidth = 0.3,
                 aes(x = x, xend = x + x.pad, y = y, yend = y.lab)) +
    geom_text(data = coupling, hjust = 0, size = 3.3, fontface = "bold",
              show.legend = FALSE,
              aes(x = x + x.pad * 1.2, y = y.lab,
                  label = sprintf("%s  %.2f", location, slope))) +
    # room on the right for the labels, but no tick marks above 100
    scale_x_continuous(breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0.03, 0.32))) +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, guide = "none") +
    labs(x = "% incidence averted among MSM",
         y = "% incidence averted among women",
         title = "Transmission coupling: how far the MSM effect carries into women",
         subtitle = paste0("one line per city across Doxy-PEP coverage levels, ",
                           EVAL.YEAR, "; the number is the through-origin slope. ",
                           "Dashed line is 1:1.")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
p.coupling
.save_fig(p.coupling, .default_fig_dir(), "explain_coupling.png", 10, 6.5, 300)


# ADDITION 1 -- HOW LONG THE INDIRECT EFFECT TAKES TO ARRIVE
# Everything above is evaluated at EVAL.YEAR (2030). Doxy-PEP is delivered to
# MSM only, so the effect in women is second-order: it has to travel through
# the transmission network before it appears. If it is still accruing in 2030
# then every spillover and coupling number above is a LOWER BOUND, and the
# size of that bias is what this block measures.
#
#   (a) the spillover ratio recomputed at 2030 and at 2035;
#   (b) the coupling slope refit at each horizon, using all ten coverages;
#   (c) t90 -- the year each subgroup reaches 90% of its own 2035 effect, and
#       the gap between MSM and women, which is the lag in years.
#
# PRECISION: get_stats() rounds to whole numbers, so a ratio of two small
# percentages is coarse. Where av_msm is below ~10%, read the slope in (b)
# rather than the ratio in (a) -- it is fitted from ten points and is far less
# sensitive to rounding.
# ****************************************************************************

LAG.YEARS  <- c("2030", "2035")
TRAJ.YEARS <- as.character(2023:2035)   # 2022 is the start year: % averted is 0

.y1 <- LAG.YEARS[1]
.y2 <- LAG.YEARS[2]

# ---- (a) spillover ratio at each horizon -----------------------------------
lag.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = LAG.YEARS,
    stat.type     = "median")

spill.by.year <- table_to_long(lag.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, year, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value) %>%
    mutate(spillover = female / msm) %>%
    rename(av_msm = msm, av_female = female)

spill.wide <- spill.by.year %>%
    select(location, year, spillover) %>%
    tidyr::pivot_wider(names_from = year, values_from = spillover,
                       names_prefix = "spillover_") %>%
    mutate(absolute.change = .data[[paste0("spillover_", .y2)]] -
               .data[[paste0("spillover_", .y1)]],
           pct.change      = 100 * (.data[[paste0("spillover_", .y2)]] /
                                        .data[[paste0("spillover_", .y1)]] - 1)) %>%
    arrange(desc(pct.change))

cat("\n=== spillover ratio at", CCRIT, ":", .y1, "vs", .y2, "===\n")
print(as.data.frame(spill.wide), digits = 3, row.names = FALSE)
cat("  pct.change > 0 means the", .y1,
    "figure UNDERSTATES how much of the MSM benefit reaches women.\n")

## .slope0 ----
# ---- (b) coupling slope refit at each horizon ------------------------------
# Through-origin slope of (% averted in women) on (% averted in MSM) across
# the coverage gradient. Same estimator as the coupling block above, refit
# once per year, so the two horizons are directly comparable.
.slope0 <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2 || all(x[ok] == 0))
        return(c(slope = NA_real_, r2 = NA_real_))
    m <- lm(y[ok] ~ 0 + x[ok])
    c(slope = unname(coef(m)[1]), r2 = summary(m)$r.squared)
}

coupling.yr.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = COVERAGE.LEVELS,
    years         = LAG.YEARS,
    stat.type     = "median")

cpl.yr <- table_to_long(coupling.yr.tbl) %>%
    filter(subgroup %in% c("msm", "female")) %>%
    select(location, subgroup, coverage, year, value) %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = value)

coupling.by.year <- cpl.yr %>%
    group_by(location, year) %>%
    group_modify(~ {
        f <- .slope0(.x$msm, .x$female)
        tibble::tibble(slope        = f[["slope"]],
                       r2           = f[["r2"]],
                       residual.het = 100 - .x$female[which.max(.x$msm)])
    }) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from  = year,
                       values_from = c(slope, r2, residual.het))

cat("\n=== transmission coupling refit at each horizon ===\n")
print(as.data.frame(coupling.by.year), digits = 3, row.names = FALSE)
cat("  a slope that rises from", .y1, "to", .y2,
    "means the coupling estimated at", .y1, "is a lower bound;\n")
cat("  residual.het falling over the same window means the self-sustaining\n",
    " heterosexual fraction is smaller than the", .y1, "figure suggests.\n")

## .time_to_frac ----
# ---- (c) t90: when has each subgroup realised 90% of its 2035 effect? ------
# Linear interpolation between the two bracketing years, so the answer is a
# fractional year rather than a step function of the annual output grid.
.time_to_frac <- function(year, value, frac = 0.90) {
    ok <- is.finite(year) & is.finite(value)
    if (sum(ok) < 2) return(NA_real_)
    year <- year[ok]; value <- value[ok]
    o <- order(year); year <- year[o]; value <- value[o]
    target <- frac * value[length(value)]
    if (!is.finite(target) || target <= 0) return(NA_real_)
    hit <- which(value >= target)[1]
    if (is.na(hit)) return(NA_real_)
    if (hit == 1L)  return(year[1])
    v0 <- value[hit - 1L]; v1 <- value[hit]
    if (v1 == v0) return(year[hit])
    year[hit - 1L] + (target - v0) / (v1 - v0) * (year[hit] - year[hit - 1L])
}

traj.lag.tbl <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = "pct_incidence_averted",
    interventions = CCRIT,
    years         = TRAJ.YEARS,
    stat.type     = "median")

t90.long <- table_to_long(traj.lag.tbl) %>%
    filter(subgroup %in% c("msm", "female", "Total")) %>%
    group_by(location, subgroup) %>%
    summarise(t90      = .time_to_frac(year, value, 0.90),
              terminal = value[which.max(year)],
              .groups  = "drop")

t90.wide <- t90.long %>%
    tidyr::pivot_wider(names_from = subgroup, values_from = c(t90, terminal)) %>%
    mutate(lag.years = t90_female - t90_msm) %>%
    mutate(across(dplyr::starts_with("t90_"), ~ round(.x, 1)),
           lag.years = round(lag.years, 2)) %>%
    arrange(desc(lag.years))

# digits = 6, not 4: with 4 significant digits a fractional calendar year such
# as 2032.4 prints as 2032 and the interpolation is invisible.
cat("\n=== years to 90% of the", .y2, "effect (", CCRIT, ") ===\n")
print(as.data.frame(t90.wide), digits = 6, row.names = FALSE)
cat("  lag.years = how much later women reach 90% of THEIR OWN terminal\n",
    " effect than MSM reach 90% of theirs. A positive lag is the number to\n",
    " quote: the benefit to women is still arriving when the talk stops.\n")

# ---- figures ---------------------------------------------------------------
# NOTE: titles below are deliberately descriptive. Once you have seen the
# direction, rewrite them as the claim (that is what the slide should say).
.ord.spill <- spill.wide$location[order(spill.wide[[paste0("spillover_", .y2)]])]

p.spill.lag <- spill.by.year %>%
    mutate(location = factor(location, levels = .ord.spill),
           year     = factor(year, levels = LAG.YEARS)) %>%
    ggplot(aes(x = spillover, y = location)) +
    geom_line(aes(group = location), colour = "grey65", linewidth = 1) +
    geom_point(aes(colour = year), size = 3) +
    scale_colour_manual(values = setNames(c("#92C5DE", "#2166AC"), LAG.YEARS),
                        name = NULL) +
    labs(x = "spillover ratio  (% averted in women / % averted in MSM)",
         y = NULL,
         title = paste0("Spillover ratio at ", .y1, " and ", .y2),
         subtitle = paste0(CCRIT, "; one segment per city")) +
    theme_minimal(base_size = 11)
p.spill.lag
.save_fig(p.spill.lag, .default_fig_dir(),
          "lag_spillover_2030_vs_2035.png", 7.5, 5, 300)

p.t90 <- t90.long %>%
    filter(subgroup %in% c("msm", "female")) %>%
    mutate(location = factor(location, levels = rev(t90.wide$location))) %>%
    ggplot(aes(x = t90, y = location)) +
    geom_line(aes(group = location), colour = "grey65", linewidth = 1) +
    geom_point(aes(colour = subgroup), size = 3) +
    scale_colour_manual(values = c(msm = "#2166AC", female = "#B2182B"),
                        name = NULL) +
    labs(x = paste0("year at which 90% of the ", .y2, " effect is reached"),
         y = NULL,
         title = "When each subgroup realises its benefit",
         subtitle = paste0(CCRIT,
                           "; interpolated between annual model output")) +
    theme_minimal(base_size = 11)
p.t90
.save_fig(p.t90, .default_fig_dir(), "lag_t90_msm_vs_women.png", 7.5, 5, 300)


# ****************************************************************************
# ADDITION 2 -- EFFICIENCY: HOW MUCH DOXYCYCLINE PER CASE AVERTED
# ****************************************************************************
# generate_custom_outcomes.R already builds the pieces, at the TOTAL level
# only (they sit behind `if (is.null(STRATIFICATION_DIMENSIONS))`):
#
#   num_cum_doxy_coverage                cumulative Doxy-PEP person-years,
#                                        accumulated from the 2022 baseline
#   num_cum_incidence_averted            cumulative cases averted vs noint
#   rate_cum_incidence_averted_ppy_doxy  1e5 * averted / person-years
#
# Missing are (i) the inverse -- person-years of Doxy-PEP per case averted,
# which is the number people actually quote -- and (ii) the same quantity with
# the numerator restricted to WOMEN. That second one has an MSM-only
# denominator and a female-only numerator BY DESIGN: it is the cost, in drug
# delivered to men, of protecting people who never receive it. It is where the
# AMR question lands.
#
# CAVEAT, and it belongs on the slide: these are ratios of medians, not
# medians of ratios, because make_multi_location_table() summarises across
# sims before they are divided. There is no credible interval here. The check
# below prints the model's own within-sim rate beside the ratio of medians so
# you can see the gap; if it is material, recompute at the array level before
# the number goes into a manuscript.
# ****************************************************************************

EFF.YEARS  <- LAG.YEARS
.ccrit.cov <- as.integer(sub("doxy\\.cov\\.", "", CCRIT))

eff.raw <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("num_cum_doxy_coverage",
                      "num_cum_incidence_averted",
                      "rate_cum_incidence_averted_ppy_doxy"),
    interventions = COVERAGE.LEVELS,
    years         = EFF.YEARS,
    stat.type     = "median")
# The "Dropped N cell(s)" message is expected: num_cum_doxy_coverage and
# rate_cum_incidence_averted_ppy_doxy exist only at the total level, so their
# sex rows are empty by construction.

eff.long <- table_to_long(eff.raw)

doxy.py <- eff.long %>%
    filter(outcome == "num_cum_doxy_coverage", subgroup == "Total") %>%
    select(location, coverage, year, doxy.py = value)

averted.cum <- eff.long %>%
    filter(outcome == "num_cum_incidence_averted",
           subgroup %in% c("Total", "msm", "female")) %>%
    select(location, coverage, year, subgroup, averted = value)

efficiency <- averted.cum %>%
    left_join(doxy.py, by = c("location", "coverage", "year")) %>%
    mutate(py.per.case.averted = doxy.py / averted,
           averted.per.1e5.py  = 1e5 * averted / doxy.py)

# ---- check: ratio of medians vs the model's own within-sim rate ------------
eff.check <- efficiency %>%
    filter(subgroup == "Total") %>%
    select(location, coverage, year, ratio.of.medians = averted.per.1e5.py) %>%
    left_join(eff.long %>%
                  filter(outcome == "rate_cum_incidence_averted_ppy_doxy",
                         subgroup == "Total") %>%
                  select(location, coverage, year, within.sim = value),
              by = c("location", "coverage", "year")) %>%
    mutate(pct.difference = 100 * (ratio.of.medians / within.sim - 1))

cat("\n=== check: ratio of medians vs within-sim rate (cases per 1e5 py) ===\n")
cat("   largest absolute difference across all cities/coverages/years:",
    signif(max(abs(eff.check$pct.difference), na.rm = TRUE), 3), "%\n")
print(as.data.frame(eff.check %>%
                        filter(year == as.integer(.y1),
                               coverage == .ccrit.cov)),
      digits = 4, row.names = FALSE)

# ---- headline table --------------------------------------------------------
eff.headline <- efficiency %>%
    filter(year == as.integer(.y1), coverage == .ccrit.cov) %>%
    select(location, subgroup, doxy.py, averted, py.per.case.averted) %>%
    tidyr::pivot_wider(names_from  = subgroup,
                       values_from = c(averted, py.per.case.averted)) %>%
    arrange(py.per.case.averted_Total)

cat("\n=== Doxy-PEP person-years per case averted,", CCRIT,
    ", cumulative 2022-", .y1, "===\n")
print(as.data.frame(eff.headline), digits = 4, row.names = FALSE)
cat("  doxy.py = MSM person-years on Doxy-PEP; it is the denominator of ALL\n",
    " three columns, including the one for women.\n")

# ---- marginal efficiency: what does the NEXT 10 points of coverage buy? ----
# The average is what a programme reports; the margin is what a programme
# decides on. Diminishing returns show up here first.
marginal <- efficiency %>%
    filter(year == as.integer(.y1), subgroup %in% c("Total", "female")) %>%
    arrange(location, subgroup, coverage) %>%
    group_by(location, subgroup) %>%
    mutate(d.py                 = doxy.py - dplyr::lag(doxy.py),
           d.averted            = averted - dplyr::lag(averted),
           marginal.py.per.case = d.py / d.averted,
           step                 = paste0(dplyr::lag(coverage), "->", coverage)) %>%
    ungroup() %>%
    filter(is.finite(marginal.py.per.case))

cat("\n=== marginal person-years per additional case averted (total pop,",
    .y1, ") ===\n")
print(as.data.frame(marginal %>%
                        filter(subgroup == "Total") %>%
                        select(location, step, marginal.py.per.case) %>%
                        tidyr::pivot_wider(names_from  = step,
                                           values_from = marginal.py.per.case)),
      digits = 4, row.names = FALSE)

# ---- figures ---------------------------------------------------------------
.eff_labels <- c(Total = "all cases averted", female = "cases averted in women")

p.eff <- efficiency %>%
    filter(year == as.integer(.y1), subgroup %in% c("Total", "female")) %>%
    mutate(subgroup = factor(.eff_labels[subgroup],
                             levels = unname(.eff_labels))) %>%
    ggplot(aes(x = coverage, y = py.per.case.averted, colour = location)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_wrap(~ subgroup, scales = "free_y") +
    scale_y_log10() +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, name = NULL) +
    labs(x = "Doxy-PEP coverage among MSM (%)",
         y = "person-years of Doxy-PEP per case averted (log scale)",
         title = "Drug delivered per case averted",
         subtitle = paste0("cumulative 2022-", .y1,
                           "; the denominator is MSM person-years in BOTH panels")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
p.eff
.save_fig(p.eff, .default_fig_dir(),
          "efficiency_py_per_case_averted.png", 10, 5.5, 300)

p.eff.marginal <- marginal %>%
    mutate(subgroup = factor(.eff_labels[subgroup],
                             levels = unname(.eff_labels)),
           step     = factor(step, levels = unique(step[order(coverage)]))) %>%
    ggplot(aes(x = step, y = marginal.py.per.case, colour = location,
               group = location)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_wrap(~ subgroup, scales = "free_y") +
    scale_y_log10() +
    scale_colour_viridis_d(option = "D", begin = 0.02, end = 0.88,
                           direction = -1, name = NULL) +
    labs(x = "coverage step (%)",
         y = "additional person-years per additional case averted (log scale)",
         title = "Marginal cost, in drug delivered, of each further 10 points of coverage",
         subtitle = paste0("cumulative 2022-", .y1)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
p.eff.marginal
.save_fig(p.eff.marginal, .default_fig_dir(),
          "efficiency_marginal_py_per_case.png", 10, 5.5, 300)


library(tidyverse)

# ****************************************************************************
# HELPERS ----
# ****************************************************************************

## parse_coverage_table ----
#' Convert a wide locations x (outcome_coverage_year) table to long format
#'
#' Works with or without stratification columns. A table with no `subgroup`
#' column behaves exactly as before. A table carrying one can be filtered to a
#' single stratum, or plotted with location x subgroup as the series label.
#'
#' @param tbl Wide data frame: identifier column(s) plus value columns.
#' @param location.col Name of the location column.
#' @param id.cols Additional non-value identifier columns. Any absent from
#'   `tbl` are silently ignored, so the default is safe for totals-only tables.
#' @param subgroup Optional vector of stratum values to keep, e.g. "Total" or
#'   c("Total", "msm"). NULL keeps every row present.
#' @param row.sep Separator used when >1 stratum is retained and the series
#'   label becomes "Atlanta - msm".
#' @return Tibble: location, outcome, coverage (int), year (int), value (num).
parse_coverage_table <- function(tbl,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 subgroup     = NULL,
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ") {
    
    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'.")
    
    # A `stat` column marks estimate vs credible-interval rows. val.cols below
    # is computed as a LEFTOVER, so an unrecognised id column would be treated
    # as a value column and trip the col.pattern check. These figures plot
    # point estimates, so keep the estimates and drop the marker. (This also
    # removes the CI rows, which previously became NA values downstream.)
    if ("stat" %in% names(tbl)) {
        tbl <- tbl[as.character(tbl$stat) == "estimate", , drop = FALSE]
        tbl$stat <- NULL
    }
    
    # identifier columns actually present; anything matching col.pattern is a
    # value column and can never be an identifier
    id.cols <- intersect(id.cols, names(tbl))
    if (length(id.cols) > 0)
        id.cols <- id.cols[is.na(stringr::str_match(id.cols, col.pattern)[, 1])]
    
    # ---- optional stratum filter ------------------------------------------
    if (!is.null(subgroup)) {
        if (length(id.cols) == 0)
            stop("'subgroup' supplied but 'tbl' has no stratification column.")
        keep <- Reduce(`|`, lapply(tbl[id.cols],
                                   function(x) as.character(x) %in% subgroup))
        if (!any(keep))
            stop("No rows match subgroup = ", paste(subgroup, collapse = ", "),
                 ".\nAvailable: ",
                 paste(sort(unique(as.character(unlist(tbl[id.cols])))),
                       collapse = ", "))
        tbl <- tbl[keep, , drop = FALSE]
    }
    
    # ---- parse value columns ----------------------------------------------
    val.cols <- setdiff(names(tbl), c(location.col, id.cols))
    if (length(val.cols) == 0)
        stop("No value columns left after removing identifiers.")
    
    parts <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "),
             "\nIf these are identifier columns, add them to 'id.cols'.")
    
    long <- tbl %>%
        select(all_of(c(location.col, id.cols, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(all_of(val.cols), names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = as.integer(parts[, 4])),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # ---- series label ------------------------------------------------------
    # append the stratum only when more than one is shown, so a totals-only or
    # single-stratum table keeps clean city names
    strat.col <- if (length(id.cols) > 0) id.cols[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) > 1)
        long <- long %>%
        mutate(location = paste0(location, row.sep, .data[[strat.col]]))
    
    long %>% select(location, outcome, coverage, year, value)
}


## .prep_long ----
#' Internal: accept either a wide table or an already-long one
.prep_long <- function(tbl, location.col, id.cols, subgroup, col.pattern, row.sep) {
    if (all(c("coverage", "value") %in% names(tbl))) {
        # already long -- apply the same stratum filter if a strat column exists
        sc <- intersect(id.cols, names(tbl))
        if (!is.null(subgroup)) {
            if (length(sc) == 0)
                stop("'subgroup' supplied but the long table has no stratification column.")
            tbl <- tbl %>% filter(if_any(all_of(sc), ~ as.character(.x) %in% subgroup))
            if (nrow(tbl) == 0) stop("No rows match subgroup = ",
                                     paste(subgroup, collapse = ", "))
        }
        if (length(sc) > 0 && dplyr::n_distinct(tbl[[sc[1]]]) > 1)
            tbl <- tbl %>% mutate(location = paste0(location, row.sep,
                                                    .data[[sc[1]]]))
        return(tbl)
    }
    parse_coverage_table(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
}

## .filter_locations ----
#' Internal: filter locations by full label, base city name, or glob pattern
.filter_locations <- function(long, locations, row.sep = " \u2014 ") {
    if (is.null(locations)) return(long)
    
    base.loc <- sub(paste0(row.sep, ".*$"), "", long$location)
    keep <- long$location %in% locations | base.loc %in% locations
    
    globs <- locations[grepl("[*?]", locations)]
    if (length(globs) > 0)
        keep <- keep | Reduce(`|`, lapply(globs, function(g)
            grepl(utils::glob2rx(g), long$location)))
    
    if (!any(keep))
        stop("No rows match locations = ", paste(locations, collapse = ", "),
             ".\nAvailable cities: ", paste(sort(unique(base.loc)), collapse = ", "),
             "\nAvailable series: ", paste(sort(unique(long$location)), collapse = ", "))
    
    long[keep, , drop = FALSE]
}

## .strat_suffix ----
#' Internal: append the stratum to a title when exactly one was selected
.strat_suffix <- function(subgroup)
    if (!is.null(subgroup) && length(subgroup) == 1) paste0(" (", subgroup, ")") else ""


## .save_fig ----
#' Internal: save a figure if a path was supplied
.save_fig <- function(p, save.path, width, height, dpi) {
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    invisible(p)
}


# ****************************************************************************
# FIGURE 1: Ranked bar -- coverage needed to reach the target ----
# ****************************************************************************

## plot_coverage_needed ----
#' Minimum coverage required to reach a target impact, ranked by city
#'
#' @param tbl Wide table, or the long output of parse_coverage_table().
#' @param target Impact threshold, e.g. 50 for a 50% reduction.
#' @param year Which year to evaluate. Defaults to the latest in the data.
#' @param subgroup Optional stratum filter, e.g. "Total" or "msm".
#' @param higher.is.better TRUE if larger values are the goal (e.g. % averted).
#' @param locations Optional subset of locations to show.
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 subgroup     = NULL,
                                 higher.is.better = TRUE,
                                 locations    = NULL,
                                 location.col = "location",
                                 id.cols      = c("subgroup", "outcome.group"),
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 row.sep      = " \u2014 ",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = "#2166AC",
                                 unreached.lab = "not reached",
                                 save.path = NULL, width = 7, height = 4.5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    hit <- function(v) if (higher.is.better) v >= target else v <= target
    
    summ <- long %>%
        group_by(location) %>%
        summarise(cov.needed = suppressWarnings(min(coverage[hit(value)])),
                  best       = if (higher.is.better) max(value, na.rm = TRUE)
                  else min(value, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(reached  = is.finite(cov.needed),
               bar.len  = ifelse(reached, cov.needed, max.cov),
               lab      = ifelse(reached, paste0(cov.needed, "%"),
                                 paste0(unreached.lab, " (max ",
                                        round(best), "%)"))) %>%
        arrange(desc(reached), cov.needed, desc(best)) %>%
        mutate(location = factor(location, levels = rev(location)))
    
    if (is.null(title))
        title <- paste0("Doxy-PEP coverage needed to reach ", target,
                        "% reduction by ", year, .strat_suffix(subgroup))
    
    p <- ggplot(summ, aes(x = bar.len, y = location)) +
        geom_col(aes(fill = reached, color = reached),
                 linewidth = 0.6, width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = lab, hjust = ifelse(reached, -0.15, 1.05),
                      color = reached),
                  size = 3.3, fontface = "bold", show.legend = FALSE) +
        scale_fill_manual(values  = c(`TRUE` = bar.fill, `FALSE` = "grey95")) +
        scale_color_manual(values = c(`TRUE` = bar.fill, `FALSE` = "grey45")) +
        scale_x_continuous(limits = c(0, max.cov * 1.25),
                           breaks = seq(0, max.cov, by = 20), expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor   = element_blank(),
              axis.ticks         = element_blank(),
              plot.title         = element_text(face = "bold", size = 12))
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# ****************************************************************************
# FIGURE 2: Dose-response -- impact vs coverage ----
# ****************************************************************************

## plot_dose_response ----
#' Impact as a function of coverage, at a fixed year
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               subgroup     = NULL,
                               locations    = NULL,
                               location.col = "location",
                               id.cols      = c("subgroup", "outcome.group"),
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               row.sep      = " \u2014 ",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.path = NULL, width = 7.5, height = 5, dpi = 300) {
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    long <- .filter_locations(long, locations, row.sep)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    # order labels by terminal impact so the key reads as a ranking
    ord <- long %>%
        group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location)
    long <- long %>% mutate(location = factor(location, levels = ord))
    
    ends <- long %>% group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>% ungroup()
    
    if (is.null(y.lab)) y.lab <- paste0(unique(long$outcome), collapse = " / ")
    if (is.null(title))
        title <- paste0(y.lab, " by coverage level, ", year, .strat_suffix(subgroup))
    
    max.cov <- max(long$coverage, na.rm = TRUE)
    
    p <- ggplot(long, aes(x = coverage, y = value,
                          color = location, group = location)) +
        geom_hline(yintercept = target, linetype = "dashed",
                   color = "grey35", linewidth = 0.5) +
        annotate("text", x = 0, y = target, label = paste0(target, "% target"),
                 hjust = -0.05, vjust = -0.6, size = 3, color = "grey35") +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.6) +
        scale_x_continuous(breaks = sort(unique(long$coverage)),
                           limits = c(0, max.cov * ifelse(direct.label, 1.28, 1.02))) +
        labs(x = x.lab, y = y.lab, title = title, color = NULL) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (direct.label) "none" else "right")
    
    if (direct.label)
        p <- p + geom_text(data = ends, aes(label = location),
                           hjust = -0.15, size = 3.1, fontface = "bold",
                           show.legend = FALSE)
    
    if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# ****************************************************************************
# FIGURE 3: Impact over time -- x axis is year ----
#   1. one city, one coverage      -> single trajectory
#   2. one city, many coverages    -> fan of curves (color.by = "coverage")
#   3. many cities, many coverages -> small multiples
# ****************************************************************************
# .strat_labeller ----
#' Build a labelling function for stratum display names
#'
#' Returns identity when `map` is NULL or empty, so callers that pass nothing
#' keep the raw factor levels. Levels absent from `map` are passed through
#' unchanged rather than becoming NA.
#' @param map Named character vector: names are stratum values, values are labels.
#' @noRd
.strat_labeller <- function(map) {
    if (is.null(map) || length(map) == 0) return(function(x) x)
    function(x) {
        x   <- as.character(x)
        out <- unname(map[x])
        ifelse(is.na(out), x, out)
    }
}
# .make_labeller ----
#' Build a labelling function from a flexible spec
#'
#' Accepts NULL (identity), a function, a named character vector (value -> label
#' lookup, unmapped values pass through), or a single unnamed string used as a
#' template in which `{x}` is replaced by the value.
#' @noRd
.make_labeller <- function(spec) {
    if (is.null(spec)) return(function(x) as.character(x))
    if (is.function(spec)) return(function(x) as.character(spec(x)))
    if (!is.null(names(spec)) && any(nzchar(names(spec))))
        return(.strat_labeller(spec))
    if (length(spec) == 1)
        return(function(x) vapply(as.character(x),
                                  function(v) gsub("{x}", v, spec, fixed = TRUE),
                                  character(1), USE.NAMES = FALSE))
    stop("Label spec must be NULL, a function, a named vector, or a single template string.")
}
# plot_impact_over_time ----
#' Trajectory plot of impact over time
#'
#' Draws one line per series, where a series is a location x coverage x stratum
#' combination. Colour is assigned to the stratum when more than one stratum is
#' present, otherwise to `color.by`; the remaining dimensions become facets.
#'
#' Ordering: when `locations` and/or `subgroup` are supplied, their order is
#' respected in facets, legends and line stacking. Otherwise locations are
#' ranked by their endpoint value at the highest coverage.
#'
#' @param tbl Wide table as produced upstream.
#' @param color.by Dimension mapped to colour when only one stratum is present.
#' @param locations,coverages,subgroup Optional subsets. Supplied order is kept.
#' @param strat.palette ColorBrewer palette used when the stratum takes the
#'   colour channel.
#' @param strat.labels Named character vector mapping stratum values to display
#'   labels for the legend. Unmapped levels pass through unchanged; NULL keeps
#'   the raw values.
#' @param year.range Two-element numeric range, inclusive.
#' @param target Horizontal reference line; NULL to omit.
#' @param outcome Required when the table holds more than one outcome.
#' @param x.lab,y.lab Axis labels. `y.lab` defaults to the outcome name.
#' @param title Overall plot title; NULL auto-generates one, NA suppresses it.
#' @param loc.labels Panel titles for location facets. NULL keeps the raw
#'   location names; supply a named character vector to rename them, or a
#'   function.
#' @param cov.label Panel titles for coverage facets. A template string in which
#'   `{x}` is replaced by the coverage value, a named vector, or a function.
#' @param show.strip FALSE hides all panel titles.
#' @param free.y Free y scales across facets.
#' @param direct.label End-of-line labels when colouring by location.
#' @param annotate.ends Endpoint value labels; single-series plots only.
#' @return A ggplot object.
plot_impact_over_time <- function(tbl,
                                  color.by      = c("coverage", "location"),
                                  locations     = NULL,
                                  coverages     = NULL,
                                  year.range    = NULL,
                                  target        = 50,
                                  outcome       = NULL,
                                  subgroup      = NULL,
                                  location.col  = "location",
                                  id.cols       = c("subgroup", "outcome.group"),
                                  col.pattern   = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  row.sep       = " \u2014 ",
                                  facet.ncol    = NULL,
                                  loc.labels    = NULL,
                                  cov.label     = "{x}% coverage",
                                  show.strip    = TRUE,
                                  free.y        = FALSE,
                                  direct.label  = TRUE,
                                  annotate.ends = TRUE,
                                  strat.palette = "Set1",
                                  strat.labels  = c(Total             = "Total population",
                                                    msm               = "MSM",
                                                    heterosexual_male = "Heterosexual men",
                                                    female            = "Women"),
                                  x.lab         = "Year",
                                  y.lab         = NULL,
                                  title         = NULL,
                                  save.path = NULL, width = 10, height = 6, dpi = 300) {
    
    color.by <- match.arg(color.by)
    
    long <- .prep_long(tbl, location.col, id.cols, subgroup, col.pattern, row.sep)
    
    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) {
        .outcome <- outcome
        long <- long %>% filter(outcome == .outcome)
        if (nrow(long) == 0) stop("Outcome '", .outcome, "' not present in table.")
    }
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")
    
    # ---- subsets ----------------------------------------------------------
    long <- .filter_locations(long, locations, row.sep)
    if (!is.null(coverages)) {
        miss <- setdiff(coverages, unique(long$coverage))
        if (length(miss)) stop("Coverage level(s) not in table: ", paste(miss, collapse = ", "))
        long <- long %>% filter(coverage %in% coverages)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))
    
    if (nrow(long) == 0) stop("No rows left after filtering.")
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years. Build the table with years = as.character(2026:2035).")
    
    # ---- stratification column -------------------------------------------
    # resolved before any of the dimension counts are used
    strat.col <- intersect(id.cols, names(long))
    strat.col <- if (length(strat.col)) strat.col[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) < 2)
        strat.col <- NULL
    
    # ---- ordering ---------------------------------------------------------
    # user-supplied order wins; anything unmatched is appended rather than dropped
    u.loc <- as.character(unique(long$location))
    ord.loc <- if (!is.null(locations)) {
        c(intersect(locations, u.loc), setdiff(u.loc, locations))
    } else {
        long %>%
            filter(coverage == max(coverage)) %>%
            group_by(location) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
            arrange(desc(value)) %>% pull(location) %>% as.character()
    }
    long <- long %>% mutate(location = factor(as.character(location), levels = ord.loc))
    
    if (!is.null(strat.col)) {
        u.str <- as.character(unique(long[[strat.col]]))
        ord.str <- if (!is.null(subgroup))
            c(intersect(subgroup, u.str), setdiff(u.str, subgroup)) else u.str
        long[[strat.col]] <- factor(as.character(long[[strat.col]]), levels = ord.str)
    }
    
    long <- long %>%
        mutate(coverage = as.numeric(coverage), year = as.numeric(year))
    
    # ---- series identifier, ordered location -> subgroup -> coverage ------
    if (!is.null(strat.col)) {
        long <- long %>%
            arrange(location, .data[[strat.col]], coverage) %>%
            mutate(series = paste(location, .data[[strat.col]], coverage, sep = "|"))
    } else {
        long <- long %>%
            arrange(location, coverage) %>%
            mutate(series = paste(location, coverage, sep = "|"))
    }
    long <- long %>% mutate(series = factor(series, levels = unique(series)))
    
    # ---- dimension counts -------------------------------------------------
    n.loc   <- dplyr::n_distinct(long$location)
    n.cov   <- dplyr::n_distinct(long$coverage)
    n.strat <- if (!is.null(strat.col)) dplyr::n_distinct(long[[strat.col]]) else 1L
    single.line <- (n.loc == 1 && n.cov == 1 && n.strat == 1)
    
    # ---- colour and facet assignment --------------------------------------
    # stratum takes the colour channel whenever it varies; location and
    # coverage then compete for the facet channel
    strat.colour <- n.strat > 1
    if (strat.colour) {
        facet.dims <- c(if (n.loc > 1) "location", if (n.cov > 1) "coverage")
    } else {
        fb <- setdiff(c("coverage", "location"), color.by)
        facet.dims <- if ((fb == "location" && n.loc > 1) ||
                          (fb == "coverage" && n.cov > 1)) fb else character(0)
    }
    
    # ---- labels -----------------------------------------------------------
    if (is.null(y.lab)) y.lab <- unique(long$outcome)
    if (length(title) == 1 && is.na(title)) {
        title <- NULL
    } else if (is.null(title)) {
        title <- if (single.line)
            paste0(y.lab, ": ", levels(droplevels(long$location))[1],
                   " at ", unique(long$coverage), "% coverage")
        else if (strat.colour)
            paste0(y.lab, " over time, by subgroup")
        else if (color.by == "coverage")
            paste0(y.lab, " over time, by coverage level", .strat_suffix(subgroup))
        else
            paste0(y.lab, " over time, by city", .strat_suffix(subgroup))
    }
    
    use.direct.label <- direct.label && !strat.colour &&
        color.by == "location" && !single.line
    max.yr <- max(long$year, na.rm = TRUE)
    pad    <- if (use.direct.label) 4 else 0
    
    # ---- base plot --------------------------------------------------------
    p <- ggplot(long, aes(x = year, y = value))
    
    if (!is.null(target))
        p <- p + geom_hline(yintercept = target, linetype = "dashed",
                            color = "grey35", linewidth = 0.4)
    
    if (single.line) {
        p <- p + geom_line(linewidth = 1, color = "#2166AC") +
            geom_point(size = 1.8, color = "#2166AC")
        if (annotate.ends) {
            ends <- long %>% filter(year %in% range(year))
            p <- p + geom_text(data = ends,
                               aes(label = paste0(round(value, 1), "%")),
                               vjust = -1, size = 3.2, fontface = "bold",
                               color = "#2166AC")
        }
        
    } else if (strat.colour) {
        p <- p +
            geom_line(aes(color = .data[[strat.col]], group = series),
                      linewidth = 0.9) +
            geom_point(aes(color = .data[[strat.col]]), size = 1.5) +
            scale_color_brewer(palette = strat.palette, name = NULL, drop = FALSE,
                               labels = .strat_labeller(strat.labels))
        
    } else if (color.by == "coverage") {
        p <- p +
            geom_line(aes(color = coverage, group = series), linewidth = 0.9) +
            scale_color_viridis_c(option = "C", end = 0.92,
                                  name = "Doxy-PEP\ncoverage (%)",
                                  breaks = sort(unique(long$coverage)))
        
    } else {
        p <- p + geom_line(aes(color = location, group = series), linewidth = 0.85)
        if (use.direct.label) {
            ends <- long %>% group_by(series) %>%
                slice_max(year, n = 1, with_ties = FALSE) %>% ungroup()
            p <- p + geom_text(data = ends, aes(label = location, color = location),
                               hjust = -0.1, size = 2.9, fontface = "bold",
                               show.legend = FALSE)
        }
    }
    
    # ---- faceting ---------------------------------------------------------
    loc.fn <- .make_labeller(loc.labels)
    cov.fn <- .make_labeller(cov.label)
    if (length(facet.dims) == 2) {
        p <- p + facet_grid(location ~ coverage,
                            labeller = labeller(location = loc.fn, coverage = cov.fn),
                            scales = if (free.y) "free_y" else "fixed")
    } else if (length(facet.dims) == 1) {
        lab.fn <- if (facet.dims == "coverage") as_labeller(cov.fn) else as_labeller(loc.fn)
        p <- p + facet_wrap(vars(.data[[facet.dims]]), ncol = facet.ncol,
                            labeller = lab.fn,
                            scales = if (free.y) "free_y" else "fixed")
    }
    
    p <- p +
        scale_x_continuous(limits = c(min(long$year), max.yr + pad)) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              strip.text       = if (show.strip) element_text(face = "bold")
              else element_blank(),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (single.line || use.direct.label) "none" else "right")
    
    .save_fig(p, save.path, width, height, dpi)
    p
}

# examples ----
if (1==2){
    # These examples write to the driver script's FIG.DIR. Running them on their own,
    # set the calibration name first and the folder follows from it.
    if (!exists("CALIB.NAME")) CALIB.NAME <- "calib.8.21.stage3.az"
    FIG.DIR <- shield.fig.path(CALIB.NAME, create = TRUE)
    
    # --- Figure 1: headline ------------------------------------------------------
    # Needs a table using a single year (e.g., 2035) and spanning multiple locations. Build it with the multi-location
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    f1 <- plot_coverage_needed(pct.inc.ave.tbl,
                               target    = 50,
                               title = "Doxy coverage needed to reach 50% incidence reduction among MSM by 2030",
                               save.path = paste0(FIG.DIR, "fig1_coverage_needed.png"))
    f1
    # --- Figure 2: dose-response -------------------------------------------------
    f2 <- plot_dose_response(pct.inc.ave.tbl,
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             save.path = paste0(FIG.DIR, "fig2_dose_response_msm.png"))
    
    f2
    # comparing the 3 sexes in a single city
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        save          = F
    )
    # all groups in Atlanta
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("Atlanta *" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    # all MSM in different cities
    plot_dose_response(pct.inc.ave.tbl,
                       locations = c("* — msm" ),
                       target    = 50,
                       y.lab     = "Diagnoses averted (%)")
    
    # --- Figure 3: trajectories --------------------------------------------------
    
    traj.tbl = make_multi_location_table(
        data          = list(total_raw_results,sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = as.character(c(2022:2040)),
        stat.type     = "median",
        save          = F
    )
    
    # ---- View 1: one city, one scenario ----------------------------------------
    plot_impact_over_time(traj.tbl,
                          locations = c("Baltimore — *"    ),
                          coverages = 10,
                          year.range = c(2022, 2040),
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 2: one city, all coverage levels ---------------------------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("Baltimore — *"    ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          y.lab      = "Diagnoses averted in Baltimore (%)",
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 3: all cities, all coverage levels (small multiples) -------------
    plot_impact_over_time(traj.tbl,
                          locations  = c("* — female" ),
                          color.by   = "coverage",
                          year.range = c(2026, 2040),
                          facet.ncol = 5,
                          y.lab      = "Diagnoses averted (%)"
                          # save.path = paste0(FIG.DIR, "fig3_multi_by_coverage.png"
    ) 
    
    # ---- View 3b: flipped -- panels are coverage levels, lines are cities ------
    plot_impact_over_time(traj.tbl,
                          color.by   = "location",
                          coverages  = c(10, 30, 60, 90),
                          year.range = c(2026, 2035),
                          facet.ncol = 4,
                          save.path = paste0(FIG.DIR, "fig3_multi_by_location.png") )
    
}