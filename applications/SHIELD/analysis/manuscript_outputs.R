# ****************************************************************************************************
# SHIELD / Doxy-PEP -- MANUSCRIPT OUTPUTS
# ****************************************************************************************************
#
# Every manuscript table and figure is built HERE, in this file, in the order
# below. Run CONFIGURATION first, then SHARED INPUTS, then any section on its
# own -- the sections are independent except for the dependency noted below.
#
#   BUILD ORDER          R OBJECT       WRITES
#   Table 1              final          tables/1-notInt.csv
#   Figure 3             fig.heatmap    figures/fig3_heatmaps.png
#   Figure 3a (suppl.)   fig.critcov    figures/fig3a_crit_coverage.png
#   Figure 4             fig.impact     figures/fig5_impact_boxplots.png    <- see NAMING
#   Table S1             final2         tables/S1-impact-cov<c>-<yr>.csv
#   Table S2             final4         tables/S2-spillover-cov<c>.csv      [builds `table4`]
#   Table S2a            final5         tables/5-spillover-associations-cov<c>.csv
#   Figure 5             fig.spillover  figures/fig4_spillover_drivers.png  <- see NAMING
#   Check 1              --             console only
#
# NOT BUILT HERE
#   Figure 1   model schematic, drawn separately
#   Figure 2   baseline trends -- see the calibration / pretty-plot script
#   Table 3    doxycycline efficiency (person-years per infection averted), and
#              the two reconciliation checks that go with it -- see
#              table_efficiency.R. `table3` does NOT exist in this file; do not
#              add checks here that assume it does.
#
# NAMING. Object and file names describe CONTENT, not manuscript number, because
# the manuscript numbering drifts and the code should not have to follow it. Two
# names are currently one ahead of their section: the FIGURE 4 section writes
# fig5_impact_boxplots.png using p5.* panel objects, and the FIGURE 5 section
# writes fig4_spillover_drivers.png. The SECTION HEADINGS are the manuscript
# truth; the filenames are historical. Same for Table S2a, which still writes
# 5-spillover-associations-*.csv.
#
# DEPENDENCY. Table S2 builds `table4` (and `t4.share`, `t4.impact`, `av`).
# Table S2a, Figure 5 and Check 1 all read those, so Table S2 must run first.
# Figures 3 and 3a both read the SHARED INPUTS block. Nothing else is coupled.
#
# CONSOLE vs FILE. Each section writes its table to TABLE.DIR and prints a
# definition block to the console. The console text is the audit trail -- what
# each column means, which comparator it uses, what is deliberately absent. It
# is not duplicated in the CSV, so read it when regenerating.
#
# PREREQUISITES. `results` (the four arrays), SHIELD.TEN.MSAS, and the helper
# functions sourced in the guarded block below. TABLE.DIR / FIG.DIR are set
# there. Colours come from SHIELD FIGURE PALETTE in
# intervention_helper_functions.R -- not from this file.
#
# ----------------------------------------------------------------------------
# TWO METHOD CHOICES THAT DIFFER FROM THE EXPLORATORY CODE IN doxy_figures.R
#
#   1. HETEROSEXUAL GROWTH IS COMPUTED FROM COUNTS, NOT FROM A SUM OF RATES.
#      rate_incidence_per_pop for heterosexual_male and for female have
#      DIFFERENT denominators, so their sum is not the heterosexual incidence
#      rate, and its year-on-year ratio is not the heterosexual growth rate. On
#      a worked example the error was ~6%, and it fed straight into `divergence`
#      and therefore into the driver correlation. Summing incident COUNTS is
#      well defined. Table S2a prints the old and new correlations side by side.
#
#   2. PERCENTAGES ARE PULLED AT 3 DECIMALS, not rounded to whole numbers. The
#      spillover ratio divides two percentages and get_stats() rounds before
#      dividing; at digits = 0 a ratio like 8/29 carried avoidable rounding error.
# ****************************************************************************************************
# CONFIGURATION ------------------------------------------------------
if(1==2){
  source('../jheem_analyses/commoncode/locations_of_interest.R')
  source('../jheem_analyses/applications/SHIELD/shield_specification.R')
  source('../jheem_analyses/applications/SHIELD/analysis/intervention_helper_functions.R')
  
  print(paste("Root directory is set to: ",ROOT.DIR))
  BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
  FIG.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/figures/") else "figures/"
  TABLE.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/tables/") else "tables/"
  for (d in c(TABLE.DIR, FIG.DIR)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  
  # 
  total_raw_results=get(load(file = paste0(BASE.PATH,"/total_raw_results.Rdata")))
  sex_raw_results=get(load(file = paste0(BASE.PATH,"/sex_raw_results.Rdata")))
  # calculated results
  total_calc_results=get(load(file = paste0(BASE.PATH,"/total_calc_results.Rdata")))
  sex_calc_results=get(load(file = paste0(BASE.PATH,"/sex_calc_results.Rdata")))
  results=list(
    total_raw_results,
    total_calc_results,
    sex_raw_results,
    sex_calc_results
  )
  
}
{
  CCRIT.MS   <- "doxy.cov.30"                        # policy coverage level
  EVAL.YEAR  <- "2030"                               # policy evaluation horizon
  BASE.YEAR  <- "2022"
  COVERAGE.LEVELS <- paste0("doxy.cov.", seq(10, 50, 10))
  .ccrit     <- as.integer(sub("doxy\\.cov\\.", "", CCRIT.MS))
  
  .pos <- function(x) ifelse(!is.na(x) & x > 0, x, NA_real_)
  
  .label_layer <- function() {
    if (requireNamespace("ggrepel", quietly = TRUE))
      ggrepel::geom_text_repel(aes(label = location), size = 3.2)
    else geom_text(aes(label = location), size = 3.2, hjust = -0.15, vjust = -0.4)
  }

  # ---- FIGURE PALETTE ------------------------------------------------------
  # Defined once for the whole project in intervention_helper_functions.R, under
  # "SHIELD FIGURE PALETTE": SHIELD.PAL plus the PAL.* aliases (WOMEN / MSM /
  # TOTAL / TINT / BAR / POINT / FIT / OFF), and SHIELD.HEAT.COLS +
  # SHIELD.HEAT.SHADE for the ordered 3-band heat scale. The plotting helpers
  # already default to those, so the figures below need no colour arguments.
  #
  # To try a different heat palette for THIS run only, uncomment one line -- the
  # defaults are resolved at call time, so this wins over the helper:
  # SHIELD.HEAT.COLS <- SHIELD.HEAT.PALETTES[["rdylbu"]]   # or "teal" / "legacy"

  HEAT.MIDPOINT <- 49      # policy target; also the upper break between bands
  HEAT.LABEL    <- NULL    # NULL = per-cell contrast, which shaded bands need

  MSAS <- names(SHIELD.TEN.MSAS)
  PRINT.VERSION = T #
}

# ****************************************************************************************************
# SHARED INPUTS -- the coverage x location surfaces used by Figures 3 and 3a ----
# ****************************************************************************************************
# WHAT   Median % incidence reduction for every MSA x coverage level in
#        EVAL.YEAR, under the two comparators the paper uses throughout:
#          tbl.cov.vs2022     vs the 2022 baseline          (pct_incidence_reduction_vs_baseline)
#          tbl.cov.vs.noint   vs no intervention, same year (pct_incidence_averted)
# WHY    Figures 3 and 3a plot the SAME surface two ways -- Figure 3 reads it as
#        a dose-response, Figure 3a searches it for a threshold -- so it is
#        built once here rather than twice under two sets of object names.
# SCOPE  COVERAGE.LEVELS only, currently 10-50%. See the note in FIGURE 3a about
#        what that does and does not let the threshold search conclude.
{
  tbl.cov.vs2022 <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("pct_incidence_reduction_vs_baseline"),
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median",
    save          = F
  )
  tbl.cov.vs.noint <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("pct_incidence_averted"),
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median",
    save          = F
  )
}


# ****************************************************************************************************
# TABLE 1 -- BASELINE EPIDEMIOLOGY, NO INTERVENTION ----
# ****************************************************************************************************
# WHAT   One row per MSA describing the epidemic BEFORE Doxy-PEP: incidence rate,
#        fold-change 2022 -> 2030, total diagnosis rate, P&S diagnoses per
#        incident infection, and the MSM share of incidence.
# INPUT  results, MSAS; `noint` only; 2022 and 2030; median with 95% CrI.
# OUTPUT tables/1-notInt.csv  (object `final`)
# NOTE   tbl1 is the Total stratum; tbl2 supplies the MSM share and is joined on
#        location + stat, so both must be pulled at the SAME stat.type or the
#        join silently drops rows.

{
  tbl1 = make_multi_location_table(
    data = results, locations = MSAS,
    outcomes = c("rate_incidence_per_pop",
                 "ratio_rate_incidence_per_pop_vs_baseline",
                 "rate_diagnosis_total_per_pop",
                 "ratio_ps_diagnosis_to_incidence"),
    interventions = "noint", filter.by.strat = "Total",
    years = as.character(c(2022, 2030)), stat.type = "median.ci",
    digits = c(.default                                 = 0,
               ratio_rate_incidence_per_pop_vs_baseline = 2,
               ratio_ps_diagnosis_to_incidence          = 2)
  )
  
  tbl2 = make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("pct_incidence_share" ),
    interventions = "noint",
    filter.by.strat = c("msm"),
    years         = as.character(c(2022,2030)),
    stat.type     = "median.ci"
  );
  
  final <- left_join(tbl1,
                     select(tbl2, location, stat, starts_with("pct_incidence_share")),
                     by = c("location", "stat"))
  col.labels <- c(
    location                                            = "MSA",
    rate_incidence_per_pop_noint_2022                   = "Incidence rate (per 100,000 persons), 2022",
    rate_incidence_per_pop_noint_2030                   = "Incidence rate (per 100,000 persons), 2030",
    ratio_rate_incidence_per_pop_vs_baseline_noint_2030 = "Fold-change in incidence rate, 2030 vs 2022",
    rate_diagnosis_total_per_pop_noint_2022                = "Total diagnosis rate (per 100,000 persons), 2022",
    rate_diagnosis_total_per_pop_noint_2030                = "Total diagnosis rate (per 100,000 persons), 2030",
    ratio_ps_diagnosis_to_incidence_noint_2022          = "P&S diagnoses per incident infection, 2022",
    ratio_ps_diagnosis_to_incidence_noint_2030          = "P&S diagnoses per incident infection, 2030",
    pct_incidence_share_noint_2022                      = "MSM share of incidence (%), 2022",
    pct_incidence_share_noint_2030                      = "MSM share of incidence (%), 2030"
  )
  hit <- names(final) %in% names(col.labels)
  names(final)[hit] <- col.labels[names(final)[hit]]
  final<-final[names(final)[hit]];final
  
  if(PRINT.VERSION) final<-.remove.duplicate.rows(final)
  
  write.csv(final,file = paste0(TABLE.DIR,"1-notInt.csv"))
}

# ****************************************************************************************************
# FIGURE 2 <NOT BUILT HERE -- see the calibration / pretty-plot script> ----
# FIGURE 3 -- IMPACT HEATMAPS ----
# ****************************************************************************************************
# WHAT   MSA x coverage heatmaps of the median % incidence reduction in
#        EVAL.YEAR, 2 x 2:
#          A  MSM   vs the 2022 baseline        B  Total vs the 2022 baseline
#          C  MSM   vs no intervention, 2030    D  Total vs no intervention, 2030
# INPUT  tbl.cov.vs2022 and tbl.cov.vs.noint from SHARED INPUTS above.
# OUTPUT figures/fig3_heatmaps.png  (object `fig.heatmap`)
# COLOUR SHIELD.HEAT.COLS / SHIELD.HEAT.SHADE, from the helper's SHIELD
#        FIGURE PALETTE; HEAT.MIDPOINT / HEAT.LABEL from CONFIGURATION here.
# NOTE   fixed limits c(-100, 100) plus squish.marks = "always" keep the legend
#        byte-identical on all four panels, which is what lets patchwork's
#        guides = "collect" merge them into one bar instead of four.

{

  # helper code to build the heatmap from each table
  .mk.heat <- function(tbl, strat, ttl){
    plot_coverage_heatmap(
      tbl, 
      locations = MSAS,
      subgroup     = strat,
      midpoint     = HEAT.MIDPOINT,
      fill.style   = "banded",
      limits       = c(-100, 100),
      order.rows   = "alpha",
      label.colour = HEAT.LABEL,
      squish.marks = "always",        # <- identical labels on every panel
      title        = ttl,
      fill.lab     = "%Reduction In Incidence",
      legend.dir   = "horizontal",
      fixed.aspect = FALSE)
  }
  
  p.heat.A <- .mk.heat(tbl.cov.vs2022, "msm",   "A: Projected Reductions among MSM \n(Incidence in 2030 vs. 2022)")
  p.heat.B <- .mk.heat(tbl.cov.vs2022, "Total", "B: Projected Reductions in Total Population \n(Incidence in 2030 vs. 2022)")
  p.heat.C <- .mk.heat(tbl.cov.vs.noint,          "msm",   "C: Projected Reductions among MSM \n(Incidence in 2030, with DoxyPEP vs. No Intervention)")
  p.heat.D <- .mk.heat(tbl.cov.vs.noint,          "Total", "D: Projected Reductions in Total Population \n(Incidence 2030, with DoxyPEP vs. No Intervention)")
  
  .no.y.heat <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  .no.x.heat <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  fig.heatmap <- (p.heat.A + .no.x.heat | p.heat.B + .no.x.heat + .no.y.heat) /
    (p.heat.C        | p.heat.D + .no.y.heat) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom");fig.heatmap
  
  ggsave(file.path(FIG.DIR, "fig3_heatmaps.png"), fig.heatmap,
         width = 10, height = 10, dpi = 300, bg = "white")
}

# ****************************************************************************************************
# FIGURE 3a -- CRITICAL COVERAGE (SUPPLEMENTARY) ----
# ****************************************************************************************************
# WHAT   The coverage each MSA needs to reach a 50% reduction. Same 2 x 2 layout
#        and reading order as Figure 3, so the two can sit side by side:
#          A  MSM   vs the 2022 baseline        B  Total vs the 2022 baseline
#          C  MSM   vs no intervention, 2030    D  Total vs no intervention, 2030
# INPUT  tbl.cov.vs2022 and tbl.cov.vs.noint from SHARED INPUTS above.
# OUTPUT figures/fig3a_crit_coverage.png  (object `fig.critcov`)
#        Supplementary -- related to Figure 3, but not a numbered figure.
#
# Figure 3 asks how much reduction each coverage level buys. This figure reads
# the same surface the other way round: how much coverage is needed to cross 50%.
#
# THE LADDER STOPS AT 50%. "not reached" therefore means "not reached at or
# below 50% coverage", and CANNOT distinguish "needs 70%" from "never gets
# there". To answer anything about coverage above 50%, widen COVERAGE.LEVELS to
# paste0("doxy.cov.", seq(10, 100, 10)) and re-run SHARED INPUTS -- both figures
# then use the wider ladder.
#
# ROWS ARE IN THE COMMON CITY ORDER, not ranked by coverage needed. Ranking is
# right for a standalone single panel, where the ranking IS the message; inside
# a 2 x 2 it lets cities move between panels and destroys the comparison the
# figure exists to make. For a ranked single panel, call plot_coverage_needed()
# directly with order.by = "value" and a filename.
#
# EXPECT MANY "not reached" BARS IN A AND B. That is the finding, not a failure.
# Because the untreated epidemic grows, a given coverage can avert most of the
# infections projected for 2030 (C and D) while incidence still sits above its
# 2022 level (A and B). The gap between the top and bottom rows is the whole
# point of showing both comparators.
#
# COUNT-VS-RATE CAVEAT, top row: pct_incidence_reduction_vs_baseline is computed
# on incident COUNTS, so "50% reduction vs 2022" means half the INFECTIONS, not
# half the incidence RATE. With a growing population those differ, and the count
# target is the harder of the two.

{

  # helper, mirroring .mk.heat in the FIGURE 3 block above.
  # filename = NULL so nothing is written per-panel; the composed figure is
  # saved once at the end.
  .mk.crit <- function(tbl, strat, ttl)
    plot_coverage_needed(
      tbl,
      locations = MSAS,
      subgroup  = strat,
      target    = 50,
      order.by  = "alpha",      # common row order across all four panels
      title     = ttl,
      filename  = NULL)
  
  p.crit.A <- .mk.crit(tbl.cov.vs2022,  "msm",   "A: MSM (vs. 2022 baseline)")
  p.crit.B <- .mk.crit(tbl.cov.vs2022,  "Total", "B: Total Population (vs. 2022 baseline)")
  p.crit.C <- .mk.crit(tbl.cov.vs.noint, "msm",   paste0("C: MSM (vs. ", EVAL.YEAR, " No Intervention)"))
  p.crit.D <- .mk.crit(tbl.cov.vs.noint, "Total", paste0("D: Total Population (vs. ", EVAL.YEAR, " No Intervention)"))
  
  # same two theme strippers as FIGURE 3; redefined here so this block runs
  # standalone rather than depending on the FIGURE 3 block having been sourced
  .no.y.crit <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  .no.x.crit <- theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  fig.critcov <- (p.crit.A + .no.x.crit | p.crit.B + .no.x.crit + .no.y.crit) /
    (p.crit.C         | p.crit.D + .no.y.crit);fig.critcov
  
  ggsave(file.path(FIG.DIR, "fig3a_crit_coverage.png"), fig.critcov,
         width = 12, height = 9, dpi = 300, bg = "white")
}

# ****************************************************************************************************
# FIGURE 4 -- DOXY-PEP IMPACT AT ONE COVERAGE LEVEL ----
# ****************************************************************************************************
# WHAT   Nine box plots in a 3 x 3 grid, all at CCRIT.MS in EVAL.YEAR. Rows are
#        the three impact metrics, columns are the three populations:
#
#                          Total   MSM   Women
#          relative          A      B      C     % incidence averted
#          absolute          D      E      F     averted per 100,000 (own denominator)
#          cumulative        G      H      I     infections averted, 2022-EVAL.YEAR
#
# INPUT  the CALCULATED arrays inside `results`, read at simulation level (see
#        WHY SIM-LEVEL below). Independent of every other section.
# OUTPUT figures/fig5_impact_boxplots.png  (object `fig.impact`; the filename
#        keeps the old numbering -- see NAMING in the file header).
# COLOUR PAL.TOTAL / PAL.MSM / PAL.WOMEN, from the SHIELD FIGURE PALETTE.

{
  # WHY THIS NEEDS SIM-LEVEL VALUES. A box plot is a summary of a DISTRIBUTION.
  # make_multi_location_table() returns a median and a 2.5/97.5 pair -- three
  # numbers, which cannot make a box. So this block reads the posterior values
  # straight out of the calculated arrays. Note that it only EXTRACTS: no
  # quantity is computed here that the arrays do not already carry, so this is
  # not the sim-level arithmetic that belongs in generate_custom_outcomes.R.
  #
  # WHAT THE BOX MEANS -- stated because the default is misleading for a
  # posterior. Box = interquartile range, midline = median, whiskers = the 2.5th
  # and 97.5th percentiles, i.e. the 95% credible interval. Whiskers are NOT the
  # usual 1.5 x IQR rule and no outliers are drawn: every simulation is a valid
  # draw, so flagging the tails as outliers would be wrong.
  #
  # DENOMINATORS IN ROW 2 ARE STRATUM-SPECIFIC and that is deliberate. The MSM
  # box is infections averted per 100,000 MSM and the female box per 100,000
  # women, so each reads as the absolute benefit to a member of that group. They
  # are directly comparable as risk differences even though the denominators
  # differ.
  #
  # COLUMN 1 IS NOT THE SUM OF COLUMNS 2 AND 3: heterosexual men have no panel
  # of their own, so MSM + women falls short of the total in every row.
  # ****************************************************************************************************
  {
    # ---- 0. locate the calculated arrays inside `results` -----------------------
    .has.oc <- function(a, oc) oc %in% dimnames(a)$outcome
    .arr.tot <- Filter(function(a) !("sex" %in% names(dimnames(a))) &&
                         .has.oc(a, "pct_incidence_averted"), results)
    .arr.sex <- Filter(function(a)  ("sex" %in% names(dimnames(a))) &&
                         .has.oc(a, "pct_incidence_averted"), results)
    if (!length(.arr.tot) || !length(.arr.sex))
      stop("Figure 4 (impact) needs both the total-level and sex-level CALCULATED arrays ",
           "in `results`. Found total: ", length(.arr.tot),
           ", sex: ", length(.arr.sex), ".")
    .arr.tot <- .arr.tot[[1]]; .arr.sex <- .arr.sex[[1]]
    
    # The location dimension is keyed by MSA CODE (C.12060, ...), with the city
    # name carried as the dimnames' names. make_multi_location_table() relabels via
    # resolve_locations(); reading the array directly does not, so do it here or
    # the x axis shows codes. This also pins the subset to the ten MSAs in MSAS.
    .f5.loc <- resolve_locations(.arr.tot, MSAS)
    .f5.lab <- stats::setNames(.f5.loc$label, .f5.loc$code)
    
    # ---- 1. pull posterior values, one row per location x sim -------------------
    .sim.long <- function(arr, outcome, strat = NULL) {
      idx <- list(outcome = outcome, intervention = CCRIT.MS, year = EVAL.YEAR,
                  location = .f5.loc$code)
      if (!is.null(strat)) idx$sex <- strat
      x <- subset_array(arr, idx, drop = FALSE)
      m <- apply(x, c("location", "sim"), sum)      # identity over singleton dims
      df <- as.data.frame.table(m, responseName = "value", stringsAsFactors = FALSE)
      names(df)[1:2] <- c("location", "sim")
      df$location <- unname(.f5.lab[df$location])   # code -> city name
      if (anyNA(df$location))
        stop("Figure 4 (impact): an MSA code in the array had no matching city label. ",
             "Check that MSAS and the array's location dimnames agree.")
      df
    }
    
    F5.METRICS <- c(rel = "pct_incidence_averted",
                    abs = "rate_incidence_averted_per_pop",
                    cum = "num_cum_incidence_averted")
    
    f5.raw <- bind_rows(lapply(names(F5.METRICS), function(k) bind_rows(
      transform(.sim.long(.arr.tot, F5.METRICS[[k]]),          population = "Total",  metric = k),
      transform(.sim.long(.arr.sex, F5.METRICS[[k]], "msm"),    population = "MSM",    metric = k),
      transform(.sim.long(.arr.sex, F5.METRICS[[k]], "female"), population = "Women",  metric = k))))
    
    # ---- 2. one city order for ALL six panels ----------------------------------
    # Ranked by the total-population relative reduction, so the strongest responder
    # is leftmost everywhere. Set F5.ORDER <- MSAS to use the common city order.
    F5.ORDER <- f5.raw %>%
      filter(metric == "rel", population == "Total") %>%
      group_by(location) %>% summarise(m = stats::median(value), .groups = "drop") %>%
      arrange(desc(m)) %>% pull(location)
    
    # ---- 3. box statistics, computed explicitly rather than left to the default --
    f5.box <- f5.raw %>%
      group_by(metric, population, location) %>%
      summarise(ymin   = stats::quantile(value, 0.025, na.rm = TRUE),
                lower  = stats::quantile(value, 0.25,  na.rm = TRUE),
                middle = stats::median(value, na.rm = TRUE),
                upper  = stats::quantile(value, 0.75,  na.rm = TRUE),
                ymax   = stats::quantile(value, 0.975, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(location   = factor(location, levels = F5.ORDER),
             population = factor(population, levels = c("Total", "MSM", "Women")))
    
    # ---- 4. panels --------------------------------------------------------------
    # ONE POPULATION PER PANEL, in a 3 x 3 grid: rows are the three metrics,
    # columns are total population / MSM / women.
    #
    # MSM and women are NOT plotted on a shared axis. Their incidence rates differ
    # by one to two orders of magnitude -- MSM run in the thousands per 100,000
    # MSM, women in the tens per 100,000 women -- so a shared axis compresses the
    # female boxes to a line and hides all the between-city variation that the
    # panel exists to show. Each panel therefore carries its own y scale, EXCEPT
    # row 1, where the metric is a percentage and a common 0-100 axis is what makes
    # the three columns comparable.
    #
    # One colour per population, applied consistently down each column. No legend:
    # every panel holds a single series and its title names it.
    F5.COLS <- c(Total = PAL.TOTAL, MSM = PAL.MSM, Women = PAL.WOMEN)  # SHIELD FIGURE PALETTE
    
    .f5.panel <- function(met, pop, ttl, ylab, log.y = FALSE, fix.pct = FALSE) {
      d <- f5.box %>% filter(metric == met, population == pop)
      p <- ggplot(d, aes(x = location, ymin = ymin, lower = lower, middle = middle,
                         upper = upper, ymax = ymax)) +
        geom_boxplot(stat = "identity", width = 0.6, linewidth = 0.35,
                     colour = PAL.LINE, fill = unname(F5.COLS[pop])) +
        labs(title = ttl, x = NULL, y = ylab) +
        theme_minimal(base_size = 9) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor   = element_blank(),
              axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
              plot.title         = element_text(face = "bold", size = 9),
              legend.position    = "none")
      if (met == "rel")
        p <- p + geom_hline(yintercept = 50, linetype = "dashed",
                            colour = PAL.FIT, linewidth = 0.3)
      if (fix.pct)     p <- p + scale_y_continuous(limits = c(0, 100),
                                                   breaks = seq(0, 100, 25))
      else if (log.y)  p <- p + scale_y_log10(labels = scales::comma)
      else             p <- p + scale_y_continuous(labels = scales::comma)
      p
    }
    
    .yr <- EVAL.YEAR
    # row 1 -- relative reduction, shared 0-100 axis so the columns are comparable
    p5.A <- .f5.panel("rel","Total","A. Relative reduction, total population",
                      paste0("Incidence averted by ", .yr, " (%)"), fix.pct = TRUE)
    p5.B <- .f5.panel("rel","MSM",  "B. Relative reduction, MSM",
                      paste0("Incidence averted by ", .yr, " (%)"), fix.pct = TRUE)
    p5.C <- .f5.panel("rel","Women","C. Relative reduction, women",
                      paste0("Incidence averted by ", .yr, " (%)"), fix.pct = TRUE)
    # row 2 -- absolute reduction, INDEPENDENT y scales (see note above)
    p5.D <- .f5.panel("abs","Total","D. Absolute reduction, total population",
                      "Averted per 100,000 population")
    p5.E <- .f5.panel("abs","MSM",  "E. Absolute reduction, MSM",
                      "Averted per 100,000 MSM")
    p5.F <- .f5.panel("abs","Women","F. Absolute reduction, women",
                      "Averted per 100,000 women")
    # row 3 -- cumulative counts on a LINEAR scale (log.y = FALSE below). Switch
    #          log.y = TRUE if the largest MSAs compress the smaller ones flat.
    p5.G <- .f5.panel("cum","Total","G. Cumulative infections averted, total population",
                      paste0("Infections averted, 2022-", .yr), log.y = F)
    p5.H <- .f5.panel("cum","MSM",  "H. Cumulative infections averted, MSM",
                      paste0("Infections averted, 2022-", .yr), log.y = F)
    p5.I <- .f5.panel("cum","Women","I. Cumulative infections averted, women",
                      paste0("Infections averted, 2022-", .yr), log.y = F)
    
    fig.impact <- (p5.A | p5.B | p5.C) /
      (p5.D | p5.E | p5.F) /
      (p5.G | p5.H | p5.I); fig.impact
    
    ggsave(file.path(FIG.DIR, "fig5_impact_boxplots.png"), fig.impact,
           width = 14, height = 12, dpi = 300, bg = "white")
    
    cat("\n=== FIGURE 4: impact at", CCRIT.MS, "in", EVAL.YEAR, "===\n")
    cat("    Box = IQR, midline = median, whiskers = 2.5th-97.5th percentile\n")
    cat("    (the 95% credible interval), across", length(unique(f5.raw$sim)),
        "posterior simulations.\n")
    cat("    No outliers drawn: every simulation is a valid draw.\n")
    cat("    City order (all nine panels), by total-population relative reduction:\n      ",
        paste(F5.ORDER, collapse = " > "), "\n")
  }
}

# ****************************************************************************************************
# TABLE S1 -- DOXY-PEP IMPACT AT A SINGLE COVERAGE LEVEL, BY POPULATION ----
# ****************************************************************************************************
# WHAT   One row per MSA x population (Total / MSM / women / heterosexual men):
#        incident counts and incidence rates in BASE.YEAR and EVAL.YEAR under
#        `noint` and under CCRIT.MS, then the three impact metrics at EVAL.YEAR.
#        The numeric backing for Figure 4.
# INPUT  results, MSAS; medians only -- this is a supporting table, no CrI.
# OUTPUT tables/S1-impact-cov<c>-<yr>.csv  (object `final2`)
# WARNING col.labels.t2 is assigned POSITIONALLY, over names(tbl2)[-3]. Add or
#        reorder anything in t2.outcomes1 / t2.outcomes2 and every label shifts
#        silently -- check the CSV header row after any change here.

{
  # Doxy-PEP impact across the ten MSAs at a single coverage level, for the total 
  
  t2.outcomes1 <- c(
    # ---- BLOCK 0: KEY outcomes ----
    "incidence",
    "rate_incidence_per_pop"
  )
  
  tbl2.impact1 <- make_multi_location_table(
    data            = results,
    locations       = MSAS,
    outcomes        = t2.outcomes1,
    interventions   = c("noint",CCRIT.MS),
    filter.by.strat = c("Total", "msm","female","heterosexual_male"),
    years           = c(BASE.YEAR, EVAL.YEAR),
    stat.type       = "median",
    digits = c(.default                       = 0,
               pct_incidence_averted          = 1,
               pct_cum_incidence_averted      = 1,
               rate_incidence_averted_per_pop = 1)
  )
  
  t2.outcomes2 <- c(
    # ---- BLOCK A: single year. RELATIVE and ABSOLUTE effect, no efficiency ----
    "pct_incidence_averted",
    "rate_incidence_averted_per_pop",
    # ---- BLOCK B: cumulative from baseline.year. Burden prevented, and the
    #      antibiotic it cost. Efficiency lives HERE and only here ----
    "num_cum_incidence_averted"
    # "pct_cum_incidence_averted",
    # "rate_cum_incidence_averted_ppy_doxy"
  )
  
  tbl2.impact2 <- make_multi_location_table(
    data            = results,
    locations       = MSAS,
    outcomes        = t2.outcomes2,
    interventions   = CCRIT.MS,
    filter.by.strat = c("Total", "msm","female","heterosexual_male"),
    years           = EVAL.YEAR,
    stat.type       = "median",
    digits = c(.default                       = 0,
               pct_incidence_averted          = 1,
               pct_cum_incidence_averted      = 1,
               rate_incidence_averted_per_pop = 1)
  )
  
  # column names come back as <outcome>_<intervention>_<year>
  # .t2sfx is used only by the commented-out column reorder further down; it is
  # kept so that line still works if you re-enable it.
  .t2sfx <- paste0("_", CCRIT.MS, "_", EVAL.YEAR)
  .t2cov <- sub("doxy\\.cov\\.", "", CCRIT.MS)
  
  tbl2<-left_join(tbl2.impact1,tbl2.impact2,by = c("location","subgroup","stat"))
  t2.outcomes<-c(t2.outcomes1,t2.outcomes2)
  
  
  col.labels.t2 <- stats::setNames(
    c("MSA", "Population",
      paste0("Incidence ", BASE.YEAR, ", No Intervention"),
      paste0("Incidence ", EVAL.YEAR, ", No Intervention"),
      paste0("Incidence ", BASE.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence Rate (per 100,000 population) ", BASE.YEAR, ", No Intervention"),
      paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", No Intervention"),
      paste0("Incidence Rate (per 100,000 population) ", BASE.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      
      
      paste0("Incidence averted (%), ", EVAL.YEAR),
      paste0("Incidence averted per 100,000 population, ", EVAL.YEAR),
      paste0("Cumulative infections averted, 2022-", EVAL.YEAR)
      # paste0("Cumulative incidence averted (%), 2022-", EVAL.YEAR),
      # paste0("Cumulative infections averted per 1000 doxy person-years, 2022-", EVAL.YEAR)),
    ),
    #
    names(tbl2)[-3]
  )
  
  hit2 <- names(tbl2) %in% names(col.labels.t2)
  final2 <- tbl2[names(tbl2)[hit2]]
  names(final2) <- col.labels.t2[names(final2)]
  
  # keep the block order regardless of the order the builder returned them in
  # final2 <- final2[col.labels.t2[c("location", "subgroup", paste0(t2.outcomes, .t2sfx))]]
  final2$Population <- factor(final2$Population, levels = c("Total", "msm","female","heterosexual_male"))
  final2 <- final2[order(final2$Population,final2$MSA),]
  final2
  
  if(PRINT.VERSION) final2<-.remove.duplicate.rows(final2)
  
  write.csv(final2,
            file = paste0(TABLE.DIR, "S1-impact-cov", .t2cov, "-", EVAL.YEAR, ".csv"),
            row.names = FALSE)
}

# ****************************************************************************************************
# TABLE S2 -- SPILLOVER AND ITS CANDIDATE DRIVERS ----
# ****************************************************************************************************
# WHAT   One row per MSA: % incidence averted in each population at CCRIT.MS in
#        EVAL.YEAR, the spillover ratio (women / MSM), and seven candidate
#        city-level drivers, all measured at BASE.YEAR under NO intervention so
#        they describe the epidemic Doxy-PEP is dropped into, not its effects.
# INPUT  results, MSAS. Percentages pulled at digits = 3 (header, item 2).
# OUTPUT tables/S2-spillover-cov<c>.csv  (object `final4`)
# BUILDS `table4`, plus t4.share / t4.impact / av. Table S2a, Figure 5 and
#        Check 1 all read those, so THIS SECTION MUST RUN BEFORE ALL THREE.

{
  t4.impact <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "pct_incidence_averted",
  interventions = CCRIT.MS, years = EVAL.YEAR,
  stat.type = "median", digits = 3))

av <- t4.impact %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value, names_prefix = "av_")

t4.share <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "pct_incidence_share",
  interventions = "noint", years = c(BASE.YEAR, EVAL.YEAR),
  stat.type = "median", digits = 3))

sh.msm <- t4.share %>%
  filter(subgroup == "msm") %>%
  select(location, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "msm_share_")

# ---- growth, from COUNTS (see item 1 in the header) *********************
# incidence is a count at every stratification level, so summing
# heterosexual_male + female is well defined. Summing rate_incidence_per_pop
# would not be: those two rates have different denominators.
# growth= 2030 levels -2022 levels
#divergence=log(growth_het / growth_msm)

t4.growth <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "incidence",
  interventions = "noint", years = c(BASE.YEAR, EVAL.YEAR),
  stat.type = "median"))

g <- t4.growth %>%
  filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
  group_by(location, grp, year) %>%
  summarise(inc = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
  mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
           .data[[paste0("inc_", BASE.YEAR)]]) %>%
  select(location, grp, growth) %>%
  pivot_wider(names_from = grp, values_from = growth, names_prefix = "growth_") %>%
  mutate(divergence = log(growth_het / growth_msm))

# ---- ADDITIONAL CANDIDATE DRIVERS *********************
# All measured at BASE.YEAR under no intervention, so they are properties of
# the epidemic Doxy-PEP is dropped into rather than consequences of it.
#
# Everything below is scale-free ON PURPOSE. Absolute counts (MSM incident
# cases, MSM population size) are largely city size in disguise: they would
# correlate with almost anything and tell you nothing mechanistic.

# (a) baseline incidence RATE in each stratum 2022 -- how intense each epidemic is
#     per capita, in its own denominator
inc.rate <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "rate_incidence_per_pop",
  interventions = "noint", years = BASE.YEAR,
  stat.type = "median", digits = 3)) %>%
  filter(subgroup %in% c("msm", "female")) %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value,
              names_prefix = "inc_rate_") %>%
  # the ratio is the scale-free summary of WHERE the force of infection
  # sits; prefer it to either rate alone when picking one predictor
  mutate(msm_to_female_rate = inc_rate_msm / .pos(inc_rate_female))

# (b) MSM as a share of the MALE population -- the structural quantity that
#     sets how much of the sexual network the intervention can reach.
#     Share, not headcount, for the reason above.
msm.pop <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "population",
  interventions = "noint", years = BASE.YEAR,
  stat.type = "median")) %>%
  filter(subgroup %in% c("msm", "heterosexual_male")) %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value) %>%
  mutate(msm_pop_share_male = 100 * msm / .pos(msm + heterosexual_male)) %>%
  select(location, msm_pop_share_male)

# (c) P&S detection fraction among MSM -- a proxy for mean infectious
#     duration in the SOURCE population. Faster detection means shorter
#     infectiousness, so less onward transmission per infection and a weaker
#     bridge into the heterosexual network. This is the only candidate here
#     that is not another rearrangement of incidence composition, which makes
#     it the most informative one to add.
detect <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS,
  outcomes = "ratio_ps_diagnosis_to_incidence",
  interventions = "noint", years = BASE.YEAR,
  stat.type = "median", digits = 3)) %>%
  filter(subgroup == "msm") %>%
  select(location, ps_detect_msm = value)

table4 <- av %>%
  left_join(sh.msm,   by = "location") %>%
  left_join(g,        by = "location") %>%
  left_join(inc.rate, by = "location") %>%
  left_join(msm.pop,  by = "location") %>%
  left_join(detect,   by = "location") %>%
  mutate(spillover = av_female / .pos(av_msm)) %>%
  # select(location, av_msm, av_heterosexual_male, av_female, av_Total,
  #        spillover, dplyr::starts_with("msm_share_"),
  #        growth_msm, growth_het, divergence) %>%
  arrange(desc(spillover))

# ---- format like Tables 1-3: MSA first, labelled columns *********************
T4.COLS <- c("location",
             # outcome
             "av_msm", "av_heterosexual_male", "av_female", "av_Total",
             "spillover",
             # candidate drivers, all at BASE.YEAR under no intervention
             paste0("msm_share_", BASE.YEAR), paste0("msm_share_", EVAL.YEAR),
             "inc_rate_msm", "inc_rate_female", "msm_to_female_rate",
             "msm_pop_share_male", "ps_detect_msm",
             "growth_msm", "growth_het", "divergence")

col.labels.t4 <- stats::setNames(
  c("MSA",
    "Incidence averted, MSM (%)",
    "Incidence averted, heterosexual men (%)",
    "Incidence averted, women (%)",
    "Incidence averted, total population (%)",
    "Spillover ratio (women / MSM)",
    paste0("MSM share of incidence, ", BASE.YEAR, " (%)"),
    paste0("MSM share of incidence, ", EVAL.YEAR, " (%)"),
    paste0("MSM incidence rate, ", BASE.YEAR, " (per 100,000 MSM)"),
    paste0("Female incidence rate, ", BASE.YEAR, " (per 100,000 women)"),
    "MSM : female incidence rate ratio",
    paste0("MSM share of the male population, ", BASE.YEAR, " (%)"),
    paste0("P&S diagnoses per incident infection, MSM, ", BASE.YEAR),
    paste0("MSM incidence fold-change, ", BASE.YEAR, "-", EVAL.YEAR),
    paste0("Heterosexual incidence fold-change, ", BASE.YEAR, "-", EVAL.YEAR),
    "Divergence: log(het growth / MSM growth)"),
  T4.COLS)

.missing4 <- setdiff(T4.COLS, names(table4))
if (length(.missing4))
  stop("Table 4 is missing column(s): ", paste(.missing4, collapse = ", "),
       ". The labels and the data have drifted apart.")

final4 <- table4[, T4.COLS]
names(final4) <- col.labels.t4[names(final4)]

write.csv(final4, file.path(TABLE.DIR, paste0("S2-spillover-cov", .ccrit, ".csv")),
          row.names = FALSE)

cat("\n\n=== TABLE 4: spillover and its drivers,", CCRIT.MS, "in", EVAL.YEAR, "===\n")
cat("    Outcome    spillover = % averted in women / % averted in MSM.\n")
cat("    Drivers    all measured at", BASE.YEAR, "under NO INTERVENTION, so they\n")
cat("               describe the epidemic Doxy-PEP is dropped into rather than\n")
cat("               anything it caused. All scale-free: an absolute MSM\n")
cat("               population or case count is mostly city size in disguise.\n\n")
print(as.data.frame(table4[, T4.COLS]), digits = 3, row.names = FALSE)
}

# ****************************************************************************************************
# TABLE S2a -- ASSOCIATIONS BETWEEN SPILLOVER AND THE CANDIDATE DRIVERS ----
# ****************************************************************************************************
# WHAT   Spearman rho between the spillover ratio and each candidate driver,
#        with the mechanism that would produce that sign. Then three
#        console-only diagnostics: collinearity among the candidates, whether
#        the city ranking survives to 2035, and what the counts-vs-sum-of-rates
#        fix changed (header, item 1).
# INPUT  `table4` from TABLE S2 above -- this block fails without it.
# OUTPUT tables/5-spillover-associations-cov<c>.csv  (object `final5`; filename
#        keeps the old numbering -- see NAMING in the file header).

{
# One row per MSA, so ten points per association. This is CANDIDATE
# IDENTIFICATION, not inference: it asks which city-level features move
# together with the spillover the model produces. Uncertainty is deferred to
# the posterior sensitivity analysis, where the correlation can be recomputed
# within each of the 400 draws and reported with a credible interval. Nothing
# here carries a p-value, because the ten MSAs are the whole set of interest
# rather than a sample from a larger population of cities, so a null
# distribution over hypothetical MSAs is not the relevant uncertainty.
#
# The `mechanism` column says what pathway would produce each direction. It is
# there for INTERPRETATION and as a code check -- a sign that contradicts the
# implemented mechanism is a prompt to inspect the code, which is how the
# heterosexual-growth bug was found -- not as a standard the model is graded
# against. The model is the mechanism; where it disagrees with intuition, the
# intuition is what needs revisiting.

.assoc <- function(nm, col, mech) {
  x  <- table4[[col]]
  ok <- !is.na(x) & !is.na(table4$spillover)
  data.frame(candidate = nm,
             rho       = round(cor(x[ok], table4$spillover[ok],
                                   method = "spearman"), 3),
             n         = sum(ok),
             mechanism = mech,
             stringsAsFactors = FALSE)
}

assoc <- rbind(
  .assoc("MSM share of incidence, baseline",  paste0("msm_share_", BASE.YEAR),
         "+ more of the epidemic sits in the treated group"),
  .assoc("MSM incidence rate, baseline",      "inc_rate_msm",
         "+ a more intense MSM epidemic is a larger source term"),
  .assoc("Female incidence rate, baseline",   "inc_rate_female",
         "- an established female epidemic is less MSM-derived"),
  .assoc("MSM : female incidence rate ratio", "msm_to_female_rate",
         "+ scale-free summary of where the force of infection sits"),
  .assoc("MSM share of the male population",  "msm_pop_share_male",
         "ambiguous: more network reached (+) vs lower per-capita rate (-)"),
  .assoc("P&S detection among MSM, baseline", "ps_detect_msm",
         "- faster detection = shorter infectious period = weaker bridge"),
  .assoc("Growth divergence (het vs MSM)",    "divergence",
         "- the further het runs ahead, the less of the effect reaches women"))
assoc
# ---- the TABLE goes to a file, the narrative goes to the console ----------
table5 <- assoc
col.labels.t5 <- stats::setNames(
  c("Candidate driver",
    "Spearman rho with spillover ratio",
    "MSAs (n)",
    "Mechanism that would produce this direction"),
  c("candidate", "rho", "n", "mechanism"))

.missing5 <- setdiff(names(col.labels.t5), names(table5))
if (length(.missing5))
  stop("Table 5 is missing column(s): ", paste(.missing5, collapse = ", "),
       ". Labels and data have drifted apart.")

final5 <- table5[, names(col.labels.t5)]
names(final5) <- col.labels.t5[names(final5)]

write.csv(final5,
          file = paste0(TABLE.DIR, "5-spillover-associations-cov", .ccrit, ".csv"),
          row.names = FALSE)

cat("\n\n=== TABLE 5: candidate drivers of the spillover ratio ===\n")
cat("    Outcome    spillover = % incidence averted in women / % averted in MSM,\n")
cat("               at", CCRIT.MS, "in", EVAL.YEAR, "\n")
cat("    Drivers    measured at", BASE.YEAR, "under no intervention\n")
cat("    Statistic  Spearman rank correlation across the", nrow(table4),
    "MSAs, one row each\n")
cat("    NOT reported: p-values or confidence intervals. The ten MSAs are the\n")
cat("               whole set of interest, not a sample from a population of\n")
cat("               cities, so a null distribution over hypothetical MSAs is\n")
cat("               not the relevant uncertainty. Parameter uncertainty is\n")
cat("               addressed in the posterior sensitivity analysis, where rho\n")
cat("               is recomputed within each of the 400 draws.\n\n")
print(final5, row.names = FALSE, right = FALSE)

# ---- collinearity: how much independent information is actually here? ------
cat("\n=== collinearity among the candidates (Spearman) ===\n")
cat("    These candidates are not merely correlated, they are algebraically\n")
cat("    linked: cases = population x rate, so the MSM SHARE of incidence is\n")
cat("    determined by the population shares and the rates. Two candidates\n")
cat("    that move together here are one fact told twice, and ten cities\n")
cat("    cannot separate them. Read the set as a whole, not column by column.\n\n")
.cand <- c(paste0("msm_share_", BASE.YEAR), "inc_rate_msm", "inc_rate_female",
           "msm_to_female_rate", "msm_pop_share_male", "ps_detect_msm", "divergence")
print(round(cor(table4[, .cand], method = "spearman", use = "pairwise.complete.obs"), 2))

# ---- is the 2030 spillover measuring coupling, or just who is furthest behind?
# Doxy-PEP reaches women only through the network, so their effect lags. If
# the cities REORDER between horizons, the 2030 correlations are partly
# measuring lag rather than coupling strength, and the drivers above are
# confounded by it.
.sp2035 <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "pct_incidence_averted",
  interventions = CCRIT.MS, years = "2035",
  stat.type = "median", digits = 3)) %>%
  filter(subgroup %in% c("msm", "female")) %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value) %>%
  transmute(location, spillover.2035 = female / .pos(msm)) %>%
  left_join(table4[, c("location", "spillover")], by = "location")

cat("\n=== does the spillover ranking hold at 2035? ===\n")
cat("    Spearman(2030 ranking, 2035 ranking):",
    round(cor(.sp2035$spillover, .sp2035$spillover.2035,
              method = "spearman", use = "complete.obs"), 3), "\n")
cat("    median ratio 2035/2030:",
    round(stats::median(.sp2035$spillover.2035 / .sp2035$spillover, na.rm = TRUE), 3),
    "-- above 1 means the indirect effect was still accruing in 2030,\n")
cat("    so the 2030 spillover is a LOWER bound.\n")
print(as.data.frame(.sp2035), digits = 3, row.names = FALSE)


# ---- what did fixing the rate-summing bug change? --------------------------
# Rebuilds `divergence` the OLD way -- summing rate_incidence_per_pop across
# the two heterosexual strata -- so the two correlations can be compared. Kept
# in the script rather than in a commit message: a reviewer may ask.
.old <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "rate_incidence_per_pop",
  interventions = "noint", years = c(BASE.YEAR, EVAL.YEAR),
  stat.type = "median")) %>%
  filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
  group_by(location, grp, year) %>%
  summarise(inc = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
  mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
           .data[[paste0("inc_", BASE.YEAR)]]) %>%
  select(location, grp, growth) %>%
  pivot_wider(names_from = grp, values_from = growth, names_prefix = "growth_") %>%
  mutate(divergence.old = log(growth_het / growth_msm)) %>%
  select(location, divergence.old) %>%
  left_join(table4 %>% select(location, spillover, divergence), by = "location")

cat("\n=== effect of the counts-vs-sum-of-rates fix on the divergence result ===\n")
cat("   Spearman, spillover vs divergence\n")
cat("     old (sum of rates):",
    round(cor(.old$divergence.old, .old$spillover, method = "spearman"), 3), "\n")
cat("     new (counts)      :",
    round(cor(.old$divergence,     .old$spillover, method = "spearman"), 3), "\n")
cat("   largest per-MSA change in divergence:",
    signif(max(abs(.old$divergence - .old$divergence.old), na.rm = TRUE), 3), "\n")

}

# ****************************************************************************************************
# FIGURE 5 -- SPILLOVER AGAINST FOUR CANDIDATE DRIVERS ----
# ****************************************************************************************************
# WHAT   Four scatterplots, one point per MSA, spillover ratio on y, with a
#        Spearman rho in each subtitle and a dashed OLS line for the eye only.
# INPUT  `table4` from TABLE S2 above -- THIS BLOCK FAILS IF TABLE S2 HAS NOT
#        RUN, which is why it sits below the tables rather than with Figure 4.
# OUTPUT figures/fig4_spillover_drivers.png  (object `fig.spillover`; the
#        filename keeps the old numbering -- see NAMING in the file header).
# COLOUR PAL.POINT / PAL.FIT, from the SHIELD FIGURE PALETTE.
# OPTION SPILL.SIZE.BY encodes a third variable as point AREA -- see the block
#        immediately below. NULL (the default) reproduces the figure as published.

{
  # ---- OPTION: scale the points by a third variable --------------------------
  # SPILL.SIZE.BY
  #   NULL      every point the same size. The default, and what the paper used.
  #   "<col>"   any numeric column of table4. "av_msm" is the % incidence
  #             averted among MSM at CCRIT.MS in EVAL.YEAR, i.e. the projected
  #             reduction among MSM by 2030.
  #
  # AREA, NOT RADIUS. scale_size_area() maps the value to point AREA and pins
  # zero to zero area. ggplot's default scale_size() maps to RADIUS, which
  # squares the visual weight of large values and reliably misleads. Limits are
  # pinned to the full column range so all four panels share one legend.
  #
  # READ THIS BEFORE SIZING BY av_msm. The y axis is
  #     spillover = av_female / av_msm
  # so av_msm is the DENOMINATOR of the quantity already being plotted. Point
  # size is then a component of y, NOT an independent covariate: a small point
  # sits high partly for an arithmetic reason. That is precisely why it is worth
  # showing -- it separates cities that reach a high spillover through a strong
  # female effect from those that reach it through a weak MSM effect -- but it
  # must be described that way, never as a third variable that "explains" the
  # pattern. Sizing by a driver that is NOT inside the ratio (inc_rate_female,
  # ps_detect_msm, msm_pop_share_male) carries no such caveat.
  SPILL.SIZE.BY   <- NULL                  # e.g. "av_msm"
  SPILL.SIZE.LAB  <- paste0("Incidence averted,\nMSM (%), ", EVAL.YEAR)
  SPILL.SIZE.MAX  <- 9                     # area of the largest point

  .panel <- function(xvar, xlab, ttl, size.by = SPILL.SIZE.BY) {
    d <- table4 %>% select(location, spillover, x = all_of(xvar)) %>%
      filter(!is.na(x), !is.na(spillover))
    rho <- cor(d$x, d$spillover, method = "spearman")

    if (!is.null(size.by)) {
      if (!size.by %in% names(table4))
        stop("SPILL.SIZE.BY = '", size.by, "' is not a column of table4. ",
             "Available: ", paste(names(table4), collapse = ", "), ".")
      d$size.var <- table4[[size.by]][match(d$location, table4$location)]
    }

    p <- ggplot(d, aes(x = x, y = spillover)) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                  linetype = "dashed", colour = PAL.FIT, linewidth = 0.6)

    # the size aesthetic is confined to geom_point, so the MSA labels added by
    # .label_layer() do not inherit it and stay a constant size
    p <- p + if (is.null(size.by))
               geom_point(size = 3, colour = PAL.POINT)
             else
               geom_point(aes(size = size.var), colour = PAL.POINT, alpha = 0.85)

    if (!is.null(size.by))
      p <- p + scale_size_area(
                 name     = SPILL.SIZE.LAB,
                 max_size = SPILL.SIZE.MAX,
                 # identical limits on every panel, or patchwork cannot merge
                 # the four legends into one
                 limits   = c(0, max(table4[[size.by]], na.rm = TRUE)))

    p +
      .label_layer() +
      scale_x_continuous(expand = expansion(mult = c(0.10, 0.20))) +
      labs(x = xlab, y = "Spillover Ratio", #(% averted in women / % averted in MSM)
           title = ttl,
           subtitle = paste0("Spearman rho = ", round(rho, 2) )) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(face = "bold", size = 11))
  }
  
  # Panels are grouped by WHICH SIDE OF THE EPIDEMIC they describe:
  #   top row    - the female side (A) and the MSM side (B)
  #   bottom row - the two composites: the rate ratio (C) and the growth
  #                divergence (D), each combining both sides into one number
  #
  # Note when reading C: msm_to_female_rate is MSM rate / FEMALE rate, and it is
  # dominated by its DENOMINATOR -- which is exactly what panel A plots. The MSM
  # rate on its own is essentially unassociated with spillover, so C is close to
  # a rescaling of A rather than independent evidence. Read A and C as ONE
  # finding, not two.
  p.spill.A <- .panel("inc_rate_female",
                      paste0("Female incidence rate (per 100,000 women), ", BASE.YEAR),
                      "A: How established the female epidemic already was")
  p.spill.B <- .panel(paste0("msm_share_", BASE.YEAR),
                      paste0("MSM share of incident infections, ", BASE.YEAR, " (%)"),
                      "B: How much of the epidemic MSM drove at the start")
  p.spill.C <- .panel("msm_to_female_rate",
                      paste0("MSM : female incidence rate ratio, ", BASE.YEAR),
                      "C: Where the force of infection sat")
  p.spill.D <- .panel("divergence",
                      paste0("Het-MSM divergence factor, ",
                             BASE.YEAR, "-", EVAL.YEAR, ", no intervention"),
                      "D: How far the heterosexual epidemic ran ahead")
  
  .no.y.spill <- theme(axis.title.y = element_blank())
  
  fig.spillover <- (p.spill.A | p.spill.B + .no.y.spill) /
    (p.spill.C | p.spill.D + .no.y.spill)

  # with SPILL.SIZE.BY on, all four panels carry the same size legend; collect
  # them into one strip along the bottom instead of repeating it four times
  if (!is.null(SPILL.SIZE.BY))
    fig.spillover <- fig.spillover + plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  fig.spillover
  # plot_annotation(
  #     caption = paste0("Spillover = % incidence averted in women / % averted ",
  #                      "in MSM, at ", CCRIT.MS, " in ", EVAL.YEAR,
  #                      ". Drivers measured at ", BASE.YEAR,
  #                      " under no intervention; growth is the fold-change in ",
  #                      "incident counts. Ten MSAs; descriptive, no inference."));
  fig.spillover
  
  ggsave(file.path(FIG.DIR, "fig4_spillover_drivers.png"), fig.spillover,
         width = 11, height = 10, dpi = 300, bg = "white")
  
  cat("\nWrote: ", file.path(FIG.DIR, "fig4_spillover_drivers.png"), "\n")
}

# ****************************************************************************************************
# CHECK 1 -- DOES THE DECOMPOSITION HOLD? ----
# ****************************************************************************************************
# WHAT   The share-weighted sum of the subgroup effects must reproduce the
#        model's own total-population effect, with shares taken from the same
#        no-intervention year. This is what licenses the entire "spillover
#        explains the spread" argument, so it RUNS EVERY TIME rather than being
#        trusted -- a check that does not run is not a check, and it is free.
# INPUT  t4.share, t4.impact and av, all from TABLE S2. Console output only.
# PASS   a few tenths of a percentage point is rounding. Several points means
#        the decomposition does not hold and the Table S2 argument needs
#        re-examining before anything is written up.
#
# REMOVED: the two efficiency checks that used to live here (Table 3 against the
# model's own rate_cum_incidence_averted_ppy_doxy, and the Table 3 / Table S2
# spillover multiplier). Both read `table3`, which is built in
# table_efficiency.R and not in this file, so both were dead code that would
# error on a clean run. They belong with the table they check.

cat("\n\n=== CHECK 1: decomposition -- do the strata reproduce the total? ===\n")
cat("   The share-weighted sum of the subgroup effects must equal the model's\n")
cat("   own total-population effect, with shares from the same no-int year.\n")
cat("   This is what licenses the whole 'spillover explains the spread'\n")
cat("   argument, so it runs every time rather than being trusted.\n")
sh.all <- t4.share %>%
  filter(year == as.integer(EVAL.YEAR),
         subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  select(location, subgroup, share = value)
chk1 <- t4.impact %>%
  filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  select(location, subgroup, averted = value) %>%
  left_join(sh.all, by = c("location", "subgroup")) %>%
  group_by(location) %>%
  summarise(predicted.total = sum(share * averted) / 100, .groups = "drop") %>%
  left_join(av %>% select(location, model.total = av_Total), by = "location") %>%
  mutate(difference = predicted.total - model.total)
print(as.data.frame(chk1), digits = 4, row.names = FALSE)
cat("   largest absolute discrepancy:",
    signif(max(abs(chk1$difference), na.rm = TRUE), 3), "percentage points\n")
cat("   (a few tenths is rounding; several points means the decomposition\n")
cat("    does not hold and the Table S2 argument needs re-examining)\n")

# ****************************************************************************************************
# END OF MANUSCRIPT OUTPUTS
# ****************************************************************************************************
