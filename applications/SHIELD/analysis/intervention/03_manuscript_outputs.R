# ****************************************************************************************************
# SHIELD / Doxy-PEP -- MANUSCRIPT OUTPUTS
# ****************************************************************************************************
#
# Every manuscript table and figure is built HERE, in this file, in the order
# below. Run CONFIGURATION first, then SHARED INPUTS, then any section on its
# own -- the sections are independent except for the dependency noted below.
#
# Step 3 of the workflow listed in 00_settings.R. Run it after 02_generate_custom_outcomes.R.
#
#   BUILD ORDER          R OBJECT       WRITES
#   Table 1              final          tables/1-notInt.csv
#   Figure 2             fig.baseline   figures/fig2_noint.png
#   Figure 3             fig.heatmap    figures/fig3_heatmaps.png
#   Figure 3a (suppl.)   fig.critcov    figures/fig3a_crit_coverage.png
#   Figure 4             fig.impact     figures/fig5_impact_boxplots.png    <- see NAMING
#   Table 3              final3         tables/3-efficiency.csv              [builds `table3`]
#   Table S1             final2         tables/S1-impact-cov<c>-<yr>.csv
#   Table S2             final4         tables/S2-spillover-cov<c>.csv      [builds `table4`]
#   Table S2a            final5         tables/5-spillover-associations-cov<c>.csv
#   Figure 5             fig.spillover  figures/fig4_spillover_drivers.png  <- see NAMING
#   Check 1              --             console only
#
# NOT BUILT HERE
#   Figure 1   model schematic, drawn separately
#   The pooled and by-MSA efficiency tables -- see 04_table_efficiency.R. That is a
#              second version of Table 3; keep one of the two.
#
# NAMING. Object and file names describe CONTENT, not manuscript number, because
# the manuscript numbering drifts and the code should not have to follow it. Two
# names are currently one ahead of their section: the FIGURE 4 section writes
# fig5_impact_boxplots.png using p5.* panel objects, and the FIGURE 5 section
# writes fig4_spillover_drivers.png. The SECTION HEADINGS are the manuscript
# truth; the filenames are historical. Same for Table S2a, which still writes
# 5-spillover-associations-*.csv.
#
# DEPENDENCY. Table S2 builds `table4` (and `t4.share`, `t4.impact`, `chg`).
# Table S2a, Figure 5 and Check 1 all read those, so Table S2 must run first.
# Figures 3 and 3a both read the SHARED INPUTS block. Nothing else is coupled.
#
# CONSOLE vs FILE. Each section writes its table to TABLE.DIR and prints a
# definition block to the console. The console text is the audit trail -- what
# each column means, which comparator it uses, what is deliberately absent. It
# is not duplicated in the CSV, so read it when regenerating.
#
# PREREQUISITES. The CONFIGURATION block below sources 00_settings.R (calibration,
# MSAS, COVERAGE.LEVELS, YEAR0, EVAL.YEAR) and the helper functions, sets
# TABLE.DIR / FIG.DIR, and loads `results` (the four arrays). Set it to
# if(1==2) to skip the reload when re-running single sections. Colours come from SHIELD FIGURE PALETTE in
# intervention_helper_functions.R -- not from this file.
#
# ----------------------------------------------------------------------------
# TWO METHOD CHOICES THAT DIFFER FROM THE EXPLORATORY CODE IN archive/doxy_figures.R
#
#   1. HETEROSEXUAL GROWTH IS COMPUTED FROM COUNTS, NOT FROM A SUM OF RATES.
#      incidence_rate for heterosexual_male and for female have
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
if(1==1){
  # the calibration, MSAS, COVERAGE.LEVELS, YEAR0 and EVAL.YEAR come from 00_settings.R
  # source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
  source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')
   
  print(paste("Root directory is set to: ",ROOT.DIR))
  
  FIG.DIR    <- shield.fig.path(CALIB.NAME,   create = TRUE)
  TABLE.DIR  <- shield.table.path(CALIB.NAME, create = TRUE)
  print(paste("Figures/Tables will be written to: ",FIG.DIR," **/tables"))
  
  # Loading raw and calculated results:
  results <- load.shield.results(CALIB.NAME)
  #
  total_raw_results  <- results$total_raw
  total_calc_results <- results$total_calc
  sex_raw_results    <- results$sex_raw
  sex_calc_results   <- results$sex_calc
 
   print(paste("Results are loaded"))
}
{
  CCRIT.MS   <- "doxy.cov.30"                        # policy coverage level
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

  PRINT.VERSION = T #
}

# ****************************************************************************************************
# SHARED INPUTS -- the coverage x location surfaces used by Figures 3 and 3a ----
# ****************************************************************************************************
# WHAT   Median % change in incidence for every MSA x coverage level in
#        EVAL.YEAR, under the two references the paper uses throughout:
#          tbl.cov.vs2022     vs YEAR0, same intervention   (pct_change_incidence_vs_year0)
#          tbl.cov.vs.noint   vs no intervention, same year (pct_change_incidence_vs_noint)
#        Negative = fewer infections than the reference.
# WHY    Figures 3 and 3a plot the SAME surface two ways -- Figure 3 reads it as
#        a dose-response, Figure 3a searches it for a threshold -- so it is
#        built once here rather than twice under two sets of object names.
# SCOPE  COVERAGE.LEVELS only, currently 10-50%. See the note in FIGURE 3a about
#        what that does and does not let the threshold search conclude.
{
  tbl.cov.vs2022 <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("pct_change_incidence_vs_year0"),
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median",
    save          = F
  )
  tbl.cov.vs.noint <- make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("pct_change_incidence_vs_noint"),
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
#        % change in the incidence rate 2022 -> 2030, total diagnosis rate, P&S
#        diagnoses per incident infection, and the MSM share of incidence.
# INPUT  results, MSAS; `noint` only; 2022 and 2030; median with 95% CrI.
# OUTPUT tables/1-notInt.csv  (object `final`)
# NOTE   tbl1 is the Total stratum; tbl2 supplies the MSM share and is joined on
#        location + stat, so both must be pulled at the SAME stat.type or the
#        join silently drops rows.

{
  tbl1 = make_multi_location_table(
    data = results, locations = MSAS,
    outcomes = c("incidence_rate",
                 "pct_change_incidence_rate_vs_year0",
                 "diagnosis_total_rate",
                 "diagnosis_ps_per_incidence"),
    interventions = "noint", filter.by.strat = "Total",
    years = as.character(c(2022, 2030)), stat.type = "median.ci",
    digits = c(.default                   = 0,
               diagnosis_ps_per_incidence = 2)
  )
  
  tbl2 = make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("share_incidence" ),
    interventions = "noint",
    filter.by.strat = c("msm"),
    years         = as.character(c(2022,2030)),
    stat.type     = "median.ci"
  );
  
  final <- left_join(tbl1,
                     select(tbl2, location, stat, starts_with("share_incidence")),
                     by = c("location", "stat"))
  col.labels <- c(
    location                                            = "MSA",
    incidence_rate_noint_2022                           = "Incidence rate (per 100,000 persons), 2022",
    incidence_rate_noint_2030                           = "Incidence rate (per 100,000 persons), 2030",
    pct_change_incidence_rate_vs_year0_noint_2030       = "Change in incidence rate, 2022-2030 (%)",
    diagnosis_total_rate_noint_2022                     = "Total diagnosis rate (per 100,000 persons), 2022",
    diagnosis_total_rate_noint_2030                     = "Total diagnosis rate (per 100,000 persons), 2030",
    diagnosis_ps_per_incidence_noint_2022               = "P&S diagnoses per incident infection, 2022",
    diagnosis_ps_per_incidence_noint_2030               = "P&S diagnoses per incident infection, 2030",
    share_incidence_noint_2022                          = "MSM share of incidence (%), 2022",
    share_incidence_noint_2030                          = "MSM share of incidence (%), 2030"
  )
  hit <- names(final) %in% names(col.labels)
  names(final)[hit] <- col.labels[names(final)[hit]]
  final<-final[names(final)[hit]];final
  
  if(PRINT.VERSION) final<-.remove.duplicate.rows(final)
  
  write.csv(final,file = file.path(TABLE.DIR, "1-notInt.csv"))
}

# ****************************************************************************************************
# FIGURE 2 -- NO INTERVENTION TRENDS ----
# ****************************************************************************************************
# WHAT   Four panels, 2022-2030, no intervention: incident cases (log scale), incidence rate,
#        P&S diagnosis rate, and the MSM share of incident infections.
# INPUT  results, MSAS; `noint` only; medians.
# OUTPUT figures/fig2_noint.png  (object `fig.baseline`)
{
baseline.tbl <- make_multi_location_table(
    data            = results,
    locations       = MSAS,
    outcomes        = c("incidence",
                        "incidence_rate",
                        "share_incidence",
                        "diagnosis_ps_per_incidence",
                        "diagnosis_ps_rate"),
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
p.base.B <- plot_trend_by_location(baseline.tbl, outcome = "incidence_rate",
                                   subgroup = "Total", 
                                   # dashed = HL,
                                   y.lab = "Incidence rate (per 100,000 person-years)")


p.base.C <- plot_trend_by_location(baseline.tbl, outcome = "share_incidence",
                                   subgroup = "msm", 
                                   # dashed = HL,
                                   y.lab = "MSM share of incident infections (%)")

p.base.D <- plot_trend_by_location(baseline.tbl, outcome = "diagnosis_ps_rate",
                                   subgroup = "Total", 
                                   # dashed = HL,
                                   y.lab = "PS Diagnosis rate (per 100,000 person-years)")

fig.baseline <- (p.base.A | p.base.B)/( p.base.D | p.base.C) + plot_annotation(tag_levels = "A");fig.baseline

.save_fig(fig.baseline, .default_fig_dir(), "fig2_noint", width = 8, height = 8, dpi = 300)
}

# ****************************************************************************************************
# FIGURE 3 -- IMPACT HEATMAPS ----
# ****************************************************************************************************
# WHAT   MSA x coverage heatmaps in EVAL.YEAR, 2 x 2:
#          A  MSM   vs the 2022 baseline        B  Total vs the 2022 baseline
#          C  MSM   vs no intervention, 2030    D  Total vs no intervention, 2030
# INPUT  tbl.cov.vs2022 and tbl.cov.vs.noint from SHARED INPUTS above.
# OPTION HEAT.FRAME picks what a cell MEANS -- % change (as computed) or % reduction
#        (sign flipped). See the block below; it must agree with the panel titles.
# OUTPUT figures/fig3_heatmaps.png  (object `fig.heatmap`)
# COLOUR SHIELD.HEAT.COLS / SHIELD.HEAT.SHADE, from the helper's SHIELD
#        FIGURE PALETTE; HEAT.MIDPOINT / HEAT.LABEL from CONFIGURATION here.
# NOTE   fixed limits c(-100, 100) plus squish.marks = "always" keep the legend
#        byte-identical on all four panels, which is what lets patchwork's
#        guides = "collect" merge them into one bar instead of four.

{
  # ---- OPTION: what does a cell MEAN? ----------------------------------------
  # HEAT.FRAME
  #   "change"     cell = % CHANGE in incidence. Positive = incidence ROSE.
  #                This is the scale the outcomes are computed on
  #                (pct_change_*), so nothing is transformed.
  #   "reduction"  cell = % REDUCTION in incidence. Positive = incidence FELL.
  #                Every value is negated, and the band breaks and their colours
  #                are flipped with it, so blue still means "target met" and red
  #                still means "worse off". Only the sign convention changes --
  #                no cell changes which band it falls in.
  #
  # THE PANEL TITLES ARE NOT DERIVED FROM THIS. They are written out below, so
  # if you change HEAT.FRAME you must change them to match, or the figure will
  # say one thing and mean the other.
  #
  # SCOPE: this flips the FIGURE only. tbl.cov.vs2022 / tbl.cov.vs.noint stay
  # on the change scale, because FIGURE 3a searches those same tables for
  # "value <= -50".
  #
  # EDGE CASE, only if you ever set SHIELD.HEAT.SHADE = FALSE: the flat-band
  # path assigns bands with cut(right = FALSE), so a value sitting EXACTLY on a
  # break lands on the upper side either way, which is the opposite band once
  # the sign is flipped. Shaded mode never calls cut() -- the colour comes from
  # a gradient whose hue steps at the break -- so it is unaffected.
  HEAT.FRAME <- "change"          # "change" or "reduction"

  # Negate every value column, leaving the identifier columns alone. Value
  # columns are the ones matching the builder's <outcome>_<intervention>_<year>
  # pattern -- the same pattern plot_coverage_heatmap() uses to find them.
  .flip.values <- function(tbl, pattern = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$") {
    hit <- grepl(pattern, names(tbl))
    if (!any(hit))
      stop("HEAT.FRAME = 'reduction': no column matched '", pattern, "', so ",
           "nothing would be flipped and the figure would silently stay on the ",
           "change scale. Has col.pattern changed?")
    tbl[hit] <- lapply(tbl[hit], function(v) {
      num <- suppressWarnings(as.numeric(v))
      if (all(is.na(num)) && !all(is.na(v)))
        stop("HEAT.FRAME = 'reduction': a value column is not numeric and cannot ",
             "be sign-flipped. A median.ci table carries '[lower-upper]' ",
             "strings -- use stat.type = 'median' for this figure.")
      -num
    })
    tbl
  }

  # Everything that has to move together when the sign convention flips. Under
  # "change" the breaks become c(-49, 0) and the colours reverse, so the band
  # MEANINGS are unchanged: blue = target met, tint = benefit short of target,
  # red = worse off. band.reverse = c(1, 2) because BOTH lower bands now sit
  # below the neutral value and must darken towards their extreme.
  .hf <- if (HEAT.FRAME == "change")
    list(breaks   = c(-HEAT.MIDPOINT, 0),
         colours  = rev(SHIELD.HEAT.COLS),
         reverse  = c(1L, 2L),
         midpoint = -HEAT.MIDPOINT,
         better   = FALSE,
         lab      = "% Change in Incidence")
  else if (HEAT.FRAME == "reduction")
    list(breaks   = c(0, HEAT.MIDPOINT),
         colours  = SHIELD.HEAT.COLS,
         reverse  = 1L,
         midpoint = HEAT.MIDPOINT,
         better   = TRUE,
         lab      = "% Reduction in Incidence")
  else stop("HEAT.FRAME must be 'change' or 'reduction', not '", HEAT.FRAME, "'.")

  # helper code to build the heatmap from each table
  .mk.heat <- function(tbl, strat, ttl){
    plot_coverage_heatmap(
      if (HEAT.FRAME == "reduction") .flip.values(tbl) else tbl,
      locations    = MSAS,
      subgroup     = strat,
      midpoint     = .hf$midpoint,
      band.breaks  = .hf$breaks,
      band.colours = .hf$colours,
      band.reverse = .hf$reverse,
      higher.is.better = .hf$better,
      fill.style   = "banded",
      limits       = c(-100, 100),
      order.rows   = "alpha",
      label.colour = HEAT.LABEL,
      squish.marks = "always",        # <- identical labels on every panel
      title        = ttl,
      fill.lab     = .hf$lab,
      legend.dir   = "horizontal",
      fixed.aspect = FALSE)
  }
  
  p.heat.A <- .mk.heat(tbl.cov.vs2022, "msm",   "A: Projected Incidence Change among MSM \n2022–2030 (%)")
  p.heat.B <- .mk.heat(tbl.cov.vs2022, "Total", "B: Projected Incidence Change among Total Population \n2022–2030 (%))")
  p.heat.C <- .mk.heat(tbl.cov.vs.noint,          "msm",   "C: Projected Incidence Change among MSM \n2030 with- vs. without- Doxy-PEP (%)")
  p.heat.D <- .mk.heat(tbl.cov.vs.noint,          "Total", "D: Projected Incidence Change among Total Population \n2030 with- vs. without- Doxy-PEP (%)")
  
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
# WHAT   The coverage each MSA needs to reach a change of -50% or lower (incidence at
#        least halved). Same 2 x 2 layout
#        and reading order as Figure 3, so the two can sit side by side:
#          A  MSM   vs the 2022 baseline        B  Total vs the 2022 baseline
#          C  MSM   vs no intervention, 2030    D  Total vs no intervention, 2030
# INPUT  tbl.cov.vs2022 and tbl.cov.vs.noint from SHARED INPUTS above.
# OUTPUT figures/fig3a_crit_coverage.png  (object `fig.critcov`)
#        Supplementary -- related to Figure 3, but not a numbered figure.
#
# Figure 3 asks how much reduction each coverage level buys. This figure reads
# the same surface the other way round: how much coverage is needed to cross -50%.
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
# COUNT-VS-RATE CAVEAT, top row: pct_change_incidence_vs_year0 is computed
# on incident COUNTS, so "-50% vs 2022" means half the INFECTIONS, not
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
      target    = -50,          # values are % change: -50 = incidence halved
      higher.is.better = FALSE, # so the target is reached at -50 or lower
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
#          relative          A      B      C     % change in incidence vs no intervention
#          absolute          D      E      F     change per 100,000 (own denominator)
#          cumulative        G      H      I     change in infections, 2022-EVAL.YEAR
#
#        All three are changes against no intervention, so they are NEGATIVE when
#        Doxy-PEP prevents infections.
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
  # box is the change per 100,000 MSM and the female box per 100,000
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
                         .has.oc(a, "pct_change_incidence_vs_noint"), results)
    .arr.sex <- Filter(function(a)  ("sex" %in% names(dimnames(a))) &&
                         .has.oc(a, "pct_change_incidence_vs_noint"), results)
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
    
    F5.METRICS <- c(rel = "pct_change_incidence_vs_noint",
                    abs = "abs_change_incidence_rate_vs_noint",
                    cum = "abs_change_cum_incidence_vs_noint")
    
    f5.raw <- bind_rows(lapply(names(F5.METRICS), function(k) bind_rows(
      transform(.sim.long(.arr.tot, F5.METRICS[[k]]),          population = "Total",  metric = k),
      transform(.sim.long(.arr.sex, F5.METRICS[[k]], "msm"),    population = "MSM",    metric = k),
      transform(.sim.long(.arr.sex, F5.METRICS[[k]], "female"), population = "Women",  metric = k))))
    
    # ---- 2. one city order for ALL six panels ----------------------------------
    # Ranked by the total-population relative change, most negative first, so the
    # strongest responder is leftmost everywhere. Set F5.ORDER <- MSAS to use the
    # common city order.
    F5.ORDER <- f5.raw %>%
      filter(metric == "rel", population == "Total") %>%
      group_by(location) %>% summarise(m = stats::median(value), .groups = "drop") %>%
      arrange(m) %>% pull(location)
    
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
    # row 1, where the metric is a percentage and a common -100 to 0 axis is what
    # makes the three columns comparable.
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
        p <- p + geom_hline(yintercept = -50, linetype = "dashed",
                            colour = PAL.FIT, linewidth = 0.3)
      if (fix.pct)     p <- p + scale_y_continuous(limits = c(-100, 0),
                                                   breaks = seq(-100, 0, 25))
      else if (log.y)  p <- p + scale_y_log10(labels = scales::comma)
      else             p <- p + scale_y_continuous(labels = scales::comma)
      p
    }
    
    .yr <- EVAL.YEAR
    # row 1 -- relative change, shared -100 to 0 axis so the columns are comparable
    p5.A <- .f5.panel("rel","Total","A. Relative change, total population",
                      paste0("Change in incidence, ", .yr, " (%)"), fix.pct = TRUE)
    p5.B <- .f5.panel("rel","MSM",  "B. Relative change, MSM",
                      paste0("Change in incidence, ", .yr, " (%)"), fix.pct = TRUE)
    p5.C <- .f5.panel("rel","Women","C. Relative change, women",
                      paste0("Change in incidence, ", .yr, " (%)"), fix.pct = TRUE)
    # row 2 -- absolute change, INDEPENDENT y scales (see note above)
    p5.D <- .f5.panel("abs","Total","D. Absolute change, total population",
                      "Change per 100,000 population")
    p5.E <- .f5.panel("abs","MSM",  "E. Absolute change, MSM",
                      "Change per 100,000 MSM")
    p5.F <- .f5.panel("abs","Women","F. Absolute change, women",
                      "Change per 100,000 women")
    # row 3 -- cumulative change on a LINEAR scale. A log scale cannot show
    #          negative values, so keep log.y = FALSE here.
    p5.G <- .f5.panel("cum","Total","G. Change in cumulative infections, total population",
                      paste0("Change in infections, 2022-", .yr), log.y = F)
    p5.H <- .f5.panel("cum","MSM",  "H. Change in cumulative infections, MSM",
                      paste0("Change in infections, 2022-", .yr), log.y = F)
    p5.I <- .f5.panel("cum","Women","I. Change in cumulative infections, women",
                      paste0("Change in infections, 2022-", .yr), log.y = F)
    
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
    cat("    City order (all nine panels), by total-population relative change:\n      ",
        paste(F5.ORDER, collapse = " > "), "\n")
  }
}

# ****************************************************************************************************
# TABLE 3 -- DOXY-PEP EFFICIENCY ----
# ****************************************************************************************************
# OUTPUT tables/3-efficiency.csv  (objects `final3`, and `table3` in long form)
# NOTE   04_table_efficiency.R builds a second version of this table (pooled across MSAs, MSM and
#        Total only, 2030 and 2035). Keep one of the two.
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
# NOTE there is deliberately no single-year version of either: drug given in
# one year keeps preventing infections in later years.
#
# INFECTIONS AVERTED = minus abs_change_cum_incidence_vs_noint. The change is
# negative when infections are prevented; this table needs the positive count.
# ****************************************************************************************************
{
    t3.long <- table_to_long(make_multi_location_table(
        data          = results,
        locations     = MSAS,
        outcomes      = c("cum_doxy_coverage", "abs_change_cum_incidence_vs_noint"),
        interventions = COVERAGE.LEVELS,
        years         = EVAL.YEAR,
        stat.type     = "median"))
    
    t3.py <- t3.long %>%
        filter(outcome == "cum_doxy_coverage", subgroup == "Total") %>%
        select(location, year, coverage, cum_py = value)
    
    t3.av <- t3.long %>%
        filter(outcome == "abs_change_cum_incidence_vs_noint",
               subgroup %in% c("Total", "msm", "female")) %>%
        transmute(location, year, coverage, subgroup, averted = -value)
    
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
          paste0("Cumulative Incidence Averted, ", YEAR0, "-", EVAL.YEAR),
          paste0("Cumulative DoxyPEP Person-Years, ", YEAR0, "-", EVAL.YEAR),
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
    "% are shown, cumulative", YEAR0, "-", EVAL.YEAR, "\n")
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
cat("               ratio credits the wrong year.\n\n")
print(as.data.frame(table3), digits = 4, row.names = FALSE)

# ****************************************************************************************************
# TABLE S1 -- DOXY-PEP IMPACT AT A SINGLE COVERAGE LEVEL, BY POPULATION ----
# ****************************************************************************************************
# WHAT   One row per MSA x population (Total / MSM / women / heterosexual men):
#        incident counts and incidence rates in YEAR0 and EVAL.YEAR under
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
    "incidence_rate"
  )
  
  tbl2.impact1 <- make_multi_location_table(
    data            = results,
    locations       = MSAS,
    outcomes        = t2.outcomes1,
    interventions   = c("noint",CCRIT.MS),
    filter.by.strat = c("Total", "msm","female","heterosexual_male"),
    years           = c(YEAR0, EVAL.YEAR),
    stat.type       = "median",
    digits = c(.default                           = 0,
               pct_change_incidence_vs_noint      = 1,
               abs_change_incidence_rate_vs_noint = 1)
  )
  
  t2.outcomes2 <- c(
    # ---- BLOCK A: single year, change vs no intervention: relative and absolute ----
    "pct_change_incidence_vs_noint",
    "abs_change_incidence_rate_vs_noint",
    # ---- BLOCK B: cumulative from YEAR0, change vs no intervention ----
    "abs_change_cum_incidence_vs_noint"
    # "pct_change_cum_incidence_vs_noint"
  )
  
  tbl2.impact2 <- make_multi_location_table(
    data            = results,
    locations       = MSAS,
    outcomes        = t2.outcomes2,
    interventions   = CCRIT.MS,
    filter.by.strat = c("Total", "msm","female","heterosexual_male"),
    years           = EVAL.YEAR,
    stat.type       = "median",
    digits = c(.default                           = 0,
               pct_change_incidence_vs_noint      = 1,
               abs_change_incidence_rate_vs_noint = 1)
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
      paste0("Incidence ", YEAR0, ", No Intervention"),
      paste0("Incidence ", EVAL.YEAR, ", No Intervention"),
      paste0("Incidence ", YEAR0, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence Rate (per 100,000 population) ", YEAR0, ", No Intervention"),
      paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", No Intervention"),
      paste0("Incidence Rate (per 100,000 population) ", YEAR0, ", Doxy Coverage ",.t2cov,"%"),
      paste0("Incidence Rate (per 100,000 population) ", EVAL.YEAR, ", Doxy Coverage ",.t2cov,"%"),
      
      
      paste0("Change in incidence vs no intervention (%), ", EVAL.YEAR),
      paste0("Change in incidence rate vs no intervention (per 100,000 population), ", EVAL.YEAR),
      paste0("Change in cumulative infections vs no intervention, ", YEAR0, "-", EVAL.YEAR)
      # paste0("Change in cumulative infections vs no intervention (%), ", YEAR0, "-", EVAL.YEAR)
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
            file = file.path(TABLE.DIR, paste0("S1-impact-cov", .t2cov, "-", EVAL.YEAR, ".csv")),
            row.names = FALSE)
}

# ****************************************************************************************************
# TABLE S2 -- SPILLOVER AND ITS CANDIDATE DRIVERS ----
# ****************************************************************************************************
# WHAT   One row per MSA: % change in incidence vs no intervention in each population
#        at CCRIT.MS in EVAL.YEAR, the spillover ratio (women / MSM), and seven candidate
#        city-level drivers, all measured at YEAR0 under NO intervention so
#        they describe the epidemic Doxy-PEP is dropped into, not its effects.
# INPUT  results, MSAS. Percentages pulled at digits = 3 (header, item 2).
# OUTPUT tables/S2-spillover-cov<c>.csv  (object `final4`)
# BUILDS `table4`, plus t4.share / t4.impact / chg. Table S2a, Figure 5 and
#        Check 1 all read those, so THIS SECTION MUST RUN BEFORE ALL THREE.

{
  t4.impact <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "pct_change_incidence_vs_noint",
  interventions = CCRIT.MS, years = EVAL.YEAR,
  stat.type = "median", digits = 3))

# chg_<group> = % change in incidence vs no intervention (negative = fewer infections)
chg <- t4.impact %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value, names_prefix = "chg_")

t4.share <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "share_incidence",
  interventions = "noint", years = c(YEAR0, EVAL.YEAR),
  stat.type = "median", digits = 3))

sh.msm <- t4.share %>%
  filter(subgroup == "msm") %>%
  select(location, year, value) %>%
  pivot_wider(names_from = year, values_from = value, names_prefix = "msm_share_")

# ---- growth, from COUNTS (see item 1 in the header) *********************
# incidence is a count at every stratification level, so summing
# heterosexual_male + female is well defined. Summing incidence_rate
# would not be: those two rates have different denominators.
# growth= 2030 levels -2022 levels
#divergence=log(growth_het / growth_msm)

t4.growth <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "incidence",
  interventions = "noint", years = c(YEAR0, EVAL.YEAR),
  stat.type = "median"))

g <- t4.growth %>%
  filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
  group_by(location, grp, year) %>%
  summarise(inc = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
  mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
           .data[[paste0("inc_", YEAR0)]]) %>%
  select(location, grp, growth) %>%
  pivot_wider(names_from = grp, values_from = growth, names_prefix = "growth_") %>%
  mutate(divergence = log(growth_het / growth_msm))

# ---- ADDITIONAL CANDIDATE DRIVERS *********************
# All measured at YEAR0 under no intervention, so they are properties of
# the epidemic Doxy-PEP is dropped into rather than consequences of it.
#
# Everything below is scale-free ON PURPOSE. Absolute counts (MSM incident
# cases, MSM population size) are largely city size in disguise: they would
# correlate with almost anything and tell you nothing mechanistic.

# (a) baseline incidence RATE in each stratum 2022 -- how intense each epidemic is
#     per capita, in its own denominator
inc.rate <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "incidence_rate",
  interventions = "noint", years = YEAR0,
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
  interventions = "noint", years = YEAR0,
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
  outcomes = "diagnosis_ps_per_incidence",
  interventions = "noint", years = YEAR0,
  stat.type = "median", digits = 3)) %>%
  filter(subgroup == "msm") %>%
  select(location, ps_detect_msm = value)

table4 <- chg %>%
  left_join(sh.msm,   by = "location") %>%
  left_join(g,        by = "location") %>%
  left_join(inc.rate, by = "location") %>%
  left_join(msm.pop,  by = "location") %>%
  left_join(detect,   by = "location") %>%
  # both changes are negative, so the ratio is positive. NA when MSM incidence did not fall.
  mutate(spillover = ifelse(chg_msm < 0, chg_female / chg_msm, NA_real_)) %>%
  # select(location, chg_msm, chg_heterosexual_male, chg_female, chg_Total,
  #        spillover, dplyr::starts_with("msm_share_"),
  #        growth_msm, growth_het, divergence) %>%
  arrange(desc(spillover))

# ---- format like Tables 1-3: MSA first, labelled columns *********************
T4.COLS <- c("location",
             # outcome
             "chg_msm", "chg_heterosexual_male", "chg_female", "chg_Total",
             "spillover",
             # candidate drivers, all at YEAR0 under no intervention
             paste0("msm_share_", YEAR0), paste0("msm_share_", EVAL.YEAR),
             "inc_rate_msm", "inc_rate_female", "msm_to_female_rate",
             "msm_pop_share_male", "ps_detect_msm",
             "growth_msm", "growth_het", "divergence")

col.labels.t4 <- stats::setNames(
  c("MSA",
    "Change in incidence vs no intervention, MSM (%)",
    "Change in incidence vs no intervention, heterosexual men (%)",
    "Change in incidence vs no intervention, women (%)",
    "Change in incidence vs no intervention, total population (%)",
    "Spillover ratio (women / MSM)",
    paste0("MSM share of incidence, ", YEAR0, " (%)"),
    paste0("MSM share of incidence, ", EVAL.YEAR, " (%)"),
    paste0("MSM incidence rate, ", YEAR0, " (per 100,000 MSM)"),
    paste0("Female incidence rate, ", YEAR0, " (per 100,000 women)"),
    "MSM : female incidence rate ratio",
    paste0("MSM share of the male population, ", YEAR0, " (%)"),
    paste0("P&S diagnoses per incident infection, MSM, ", YEAR0),
    paste0("MSM incidence fold-change, ", YEAR0, "-", EVAL.YEAR),
    paste0("Heterosexual incidence fold-change, ", YEAR0, "-", EVAL.YEAR),
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
cat("    Outcome    spillover = % change in women / % change in MSM (both vs no intervention).\n")
cat("    Drivers    all measured at", YEAR0, "under NO INTERVENTION, so they\n")
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

## .assoc ----
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
  .assoc("MSM share of incidence, baseline",  paste0("msm_share_", YEAR0),
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
          file = file.path(TABLE.DIR, paste0("5-spillover-associations-cov", .ccrit, ".csv")),
          row.names = FALSE)

cat("\n\n=== TABLE 5: candidate drivers of the spillover ratio ===\n")
cat("    Outcome    spillover = % change in incidence in women / % change in MSM,\n")
cat("               at", CCRIT.MS, "in", EVAL.YEAR, "\n")
cat("    Drivers    measured at", YEAR0, "under no intervention\n")
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
.cand <- c(paste0("msm_share_", YEAR0), "inc_rate_msm", "inc_rate_female",
           "msm_to_female_rate", "msm_pop_share_male", "ps_detect_msm", "divergence")
print(round(cor(table4[, .cand], method = "spearman", use = "pairwise.complete.obs"), 2))

# ---- is the 2030 spillover measuring coupling, or just who is furthest behind?
# Doxy-PEP reaches women only through the network, so their effect lags. If
# the cities REORDER between horizons, the 2030 correlations are partly
# measuring lag rather than coupling strength, and the drivers above are
# confounded by it.
.sp2035 <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "pct_change_incidence_vs_noint",
  interventions = CCRIT.MS, years = "2035",
  stat.type = "median", digits = 3)) %>%
  filter(subgroup %in% c("msm", "female")) %>%
  select(location, subgroup, value) %>%
  pivot_wider(names_from = subgroup, values_from = value) %>%
  transmute(location, spillover.2035 = ifelse(msm < 0, female / msm, NA_real_)) %>%
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
# Rebuilds `divergence` the OLD way -- summing incidence_rate across
# the two heterosexual strata -- so the two correlations can be compared. Kept
# in the script rather than in a commit message: a reviewer may ask.
.old <- table_to_long(make_multi_location_table(
  data = results, locations = MSAS, outcomes = "incidence_rate",
  interventions = "noint", years = c(YEAR0, EVAL.YEAR),
  stat.type = "median")) %>%
  filter(subgroup %in% c("msm", "heterosexual_male", "female")) %>%
  mutate(grp = ifelse(subgroup == "msm", "msm", "het")) %>%
  group_by(location, grp, year) %>%
  summarise(inc = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = inc, names_prefix = "inc_") %>%
  mutate(growth = .data[[paste0("inc_", EVAL.YEAR)]] /
           .data[[paste0("inc_", YEAR0)]]) %>%
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
  #   "<col>"   any numeric column of table4. "chg_msm" is the % change in
  #             incidence among MSM vs no intervention at CCRIT.MS in EVAL.YEAR.
  #             Point AREA uses the absolute value, so a change of -60% draws a
  #             bigger point than -30%.
  #
  # AREA, NOT RADIUS. scale_size_area() maps the value to point AREA and pins
  # zero to zero area. ggplot's default scale_size() maps to RADIUS, which
  # squares the visual weight of large values and reliably misleads. Limits are
  # pinned to the full column range so all four panels share one legend.
  #
  # READ THIS BEFORE SIZING BY chg_msm. The y axis is
  #     spillover = chg_female / chg_msm
  # so chg_msm is the DENOMINATOR of the quantity already being plotted. Point
  # size is then a component of y, NOT an independent covariate: a small point
  # sits high partly for an arithmetic reason. That is precisely why it is worth
  # showing -- it separates cities that reach a high spillover through a strong
  # female effect from those that reach it through a weak MSM effect -- but it
  # must be described that way, never as a third variable that "explains" the
  # pattern. Sizing by a driver that is NOT inside the ratio (inc_rate_female,
  # ps_detect_msm, msm_pop_share_male) carries no such caveat.
  SPILL.SIZE.BY   <- NULL                  # e.g. "chg_msm"
  SPILL.SIZE.LAB  <- paste0("Change in incidence,\nMSM (%, size), ", EVAL.YEAR)
  SPILL.SIZE.MAX  <- 9                     # area of the largest point

  .panel <- function(xvar, xlab, ttl, size.by = SPILL.SIZE.BY) {
    d <- table4 %>% select(location, spillover, x = all_of(xvar)) %>%
      filter(!is.na(x), !is.na(spillover))
    rho <- cor(d$x, d$spillover, method = "spearman")

    if (!is.null(size.by)) {
      if (!size.by %in% names(table4))
        stop("SPILL.SIZE.BY = '", size.by, "' is not a column of table4. ",
             "Available: ", paste(names(table4), collapse = ", "), ".")
      d$size.var <- abs(table4[[size.by]][match(d$location, table4$location)])
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
                 limits   = c(0, max(abs(table4[[size.by]]), na.rm = TRUE)))

    p +
      .label_layer() +
      scale_x_continuous(expand = expansion(mult = c(0.10, 0.20))) +
      labs(x = xlab, y = "Spillover Ratio", #(% change in women / % change in MSM)
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
                      paste0("Female incidence rate (per 100,000 women), ", YEAR0),
                      "A: How established the female epidemic already was")
  p.spill.B <- .panel(paste0("msm_share_", YEAR0),
                      paste0("MSM share of incident infections, ", YEAR0, " (%)"),
                      "B: How much of the epidemic MSM drove at the start")
  p.spill.C <- .panel("msm_to_female_rate",
                      paste0("MSM : female incidence rate ratio, ", YEAR0),
                      "C: Where the force of infection sat")
  p.spill.D <- .panel("divergence",
                      paste0("Het-MSM divergence factor, ",
                             YEAR0, "-", EVAL.YEAR, ", no intervention"),
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
  #     caption = paste0("Spillover = % change in incidence in women / % change ",
  #                      "in MSM, at ", CCRIT.MS, " in ", EVAL.YEAR,
  #                      ". Drivers measured at ", YEAR0,
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
# INPUT  t4.share, t4.impact and chg, all from TABLE S2. Console output only.
# PASS   a few tenths of a percentage point is rounding. Several points means
#        the decomposition does not hold and the Table S2 argument needs
#        re-examining before anything is written up.
#
# REMOVED: the two efficiency checks that used to live here (Table 3 against the
# model's own efficiency outcome, since removed, and the Table 3 / Table S2
# spillover multiplier). Both read `table3`, which did not exist in this file at
# the time, so both were dead code that would error on a clean run. `table3` is
# now built in the TABLE 3 section above, but these two checks have not been
# brought back.

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
  select(location, subgroup, change = value) %>%
  left_join(sh.all, by = c("location", "subgroup")) %>%
  group_by(location) %>%
  summarise(predicted.total = sum(share * change) / 100, .groups = "drop") %>%
  left_join(chg %>% select(location, model.total = chg_Total), by = "location") %>%
  mutate(difference = predicted.total - model.total)
print(as.data.frame(chk1), digits = 4, row.names = FALSE)
cat("   largest absolute discrepancy:",
    signif(max(abs(chk1$difference), na.rm = TRUE), 3), "percentage points\n")
cat("   (a few tenths is rounding; several points means the decomposition\n")
cat("    does not hold and the Table S2 argument needs re-examining)\n")

# ****************************************************************************************************
# END OF MANUSCRIPT OUTPUTS
# ****************************************************************************************************
