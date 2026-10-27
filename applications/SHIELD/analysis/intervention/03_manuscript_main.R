# ****************************************************************************************************
# 03 MANUSCRIPT: MAIN TABLES AND FIGURES ----
# ****************************************************************************************************
# Builds the main-text outputs. Run it after 01b_generate_custom_outcomes.R.
#
#   1. Table 1    the epidemic without Doxy-PEP, 2022 and 2030          tables/table1_no_intervention.csv
#                 (the same, median and interval in one cell)           tables/table1_no_intervention_one_cell.csv
#   2. Figure 3   % change in incidence with vs without Doxy-PEP        figures/fig3_heatmap_vs_noint.png
#   3. Figure 4   the effect of Doxy-PEP at one coverage level          figures/fig4_impact_boxplots.png
#   4. Table 2    the values in Figure 4, with a Total row              tables/table2_fig4_values_<CCRIT>.csv
#
# Run SETUP first. After that, each section runs on its own, except Table 2, which needs Figure 4.
#
# SIGNS: every change is "with Doxy-PEP minus without", so a negative value means fewer infections.
# A change of -50% means incidence was halved.
# ****************************************************************************************************


# ****************************************************************************************************
# SETUP ----
# ****************************************************************************************************
# CALIB.NAME, MSAS, LOCATIONS, COVERAGE.LEVELS, YEAR0 and EVAL.YEAR come from 00_settings.R
source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')

FIG.DIR   <- shield.fig.path(CALIB.NAME,   create = TRUE)
TABLE.DIR <- shield.table.path(CALIB.NAME, create = TRUE)

# The result arrays: raw (step 01a) and calculated (step 01b) outcomes, by total, sex and race.
# Reused if already in memory; force.reload = TRUE reads them from disk again.
# The race arrays need step 01a run with "race" in RUN.STRATA, then step 01b with "race" in
# CALC.STRATA. Until both exist, loading stops and names the missing files.
RESULT.ARRAYS <- c("total_raw", "total_calc",
                   "sex_raw",   "sex_calc",
                   "race_raw",  "race_calc")
intervention.results <- load.shield.results(CALIB.NAME, which = RESULT.ARRAYS)

# MSA names: full ("Atlanta-Sandy Springs-Alpharetta, GA", from the location registry) or short
# ("Atlanta"). Set each one here:
FULL.NAMES.IN.TABLE   <- TRUE    # Table 1
FULL.NAMES.IN.FIGURES <- FALSE   # Figures 3 and 4 (TRUE also widens them to fit the names)

MSA.FULL.NAMES  <- setNames(unname(sapply(LOCATIONS, get.location.name)), names(LOCATIONS))
MSA.SHORT.NAMES <- setNames(names(LOCATIONS), names(LOCATIONS))
TABLE.CITY.NAMES  <- if (FULL.NAMES.IN.TABLE)   MSA.FULL.NAMES else MSA.SHORT.NAMES
FIGURE.CITY.NAMES <- if (FULL.NAMES.IN.FIGURES) MSA.FULL.NAMES else MSA.SHORT.NAMES


#to see outcome names:
# dimnames(intervention.results$total_calc)
dimnames(intervention.results$sex_calc)
# ****************************************************************************************************
# TABLE 1 -- THE EPIDEMIC WITHOUT DOXY-PEP ----
# ****************************************************************************************************
# One row per MSA, with a row of 95% credible intervals under it, and a Total row for the ten MSAs
# combined. No intervention. Columns are for the total population unless they name a group.
#
# HOW EACH CELL IS COMPUTED
#   1. Take the counts (infections, diagnoses, population) in each simulation from the raw arrays.
#   2. For the Total row, sum the counts over the ten MSAs, still within each simulation.
#   3. Compute the rates, ratios and shares from those counts, within each simulation.
#   4. Report the median and 95% interval across simulations.
#   The MSA rows and the Total row go through the same function, so they are always computed the
#   same way. Never sum or average rates across MSAs: a rate is rebuilt from the summed counts.
{
  total.raw <- intervention.results$total_raw
  sex.raw   <- intervention.results$sex_raw
  # race.raw  <- intervention.results$race_raw

  # 1. Helpers
  # One outcome, no intervention, one year, summed over the given MSAs, optionally for one group,
  # e.g. list(sex = "msm"): one value per simulation
  noint_count <- function(arr, outcome, year, locs, group = list()) {
    keep <- c(list(outcome = outcome, intervention = "noint", year = year, location = locs), group)
    apply(subset_array(arr, keep), "sim", sum)
  }

  per_100k <- function(count, pop) 100000 * count / pop

  # The median, then "[2.5th-97.5th]" underneath: the same format as make_multi_location_table()
  #   d = decimals: 2 = 0.01, 0 = whole numbers, -1 = nearest 10
  round_to <- function(x, d) format(round(x, d), nsmall = max(d, 0), trim = TRUE)
  median_and_interval <- function(x, d)
    c(round_to(median(x), d),
      paste0("[", round_to(quantile(x, 0.025), d), "-", round_to(quantile(x, 0.975), d), "]"))

  # 2. The two rows (median, interval) for one MSA, or for several MSAs combined.
  #    The column names below are the column headers of the table, in order.
  table1_rows <- function(locs) {

    # 2a. Counts, total population
    inc.2022 <- noint_count(total.raw, "incidence",       YEAR0,     locs)
    inc.2030 <- noint_count(total.raw, "incidence",       EVAL.YEAR, locs)
    pop.2022 <- noint_count(total.raw, "population",      YEAR0,     locs)
    pop.2030 <- noint_count(total.raw, "population",      EVAL.YEAR, locs)
    dx.2030  <- noint_count(total.raw, "diagnosis.total", EVAL.YEAR, locs)
    ps.2030  <- noint_count(total.raw, "diagnosis.ps",    EVAL.YEAR, locs)

    # 2b. Incidence among MSM, for the MSM share of incidence
    inc.msm.2022 <- noint_count(sex.raw, "incidence", YEAR0,     locs, list(sex = "msm"))
    inc.msm.2030 <- noint_count(sex.raw, "incidence", EVAL.YEAR, locs, list(sex = "msm"))

    # 2c. Counts by race (needs race_raw in RESULT.ARRAYS)
    # ps.black    <- noint_count(race.raw, "diagnosis.ps", EVAL.YEAR, locs, list(race = "black"))
    # ps.hispanic <- noint_count(race.raw, "diagnosis.ps", EVAL.YEAR, locs, list(race = "hispanic"))
    # ps.other    <- noint_count(race.raw, "diagnosis.ps", EVAL.YEAR, locs, list(race = "other"))

    # 2d. The columns: median and interval of each quantity, with the number of decimals
    #     (counts are rounded to the nearest 10: d = -1)
    data.frame(
      "Incident infections, 2030"                         = median_and_interval(inc.2030, -1),
      "Incidence rate (per 100,000 persons), 2022"         = median_and_interval(per_100k(inc.2022, pop.2022), 0),
      "Incidence rate (per 100,000 persons), 2030"         = median_and_interval(per_100k(inc.2030, pop.2030), 0),
      "Fold change in incidence rate, 2022-2030"           = median_and_interval(per_100k(inc.2030, pop.2030) / per_100k(inc.2022, pop.2022), 2),
      "Total syphilis diagnoses, 2030"                     = median_and_interval(dx.2030, -1),
      "P&S diagnoses, 2030"                                = median_and_interval(ps.2030, -1),
      "P&S diagnosis rate (per 100,000), 2030"             = median_and_interval(per_100k(ps.2030, pop.2030), 0),
      "P&S share of total diagnoses (%), 2030"             = median_and_interval(100 * ps.2030 / dx.2030, 0),
      # "P&S diagnoses, Black, 2030"                       = median_and_interval(ps.black, 0),
      # "P&S diagnoses, Hispanic, 2030"                    = median_and_interval(ps.hispanic, 0),
      # "P&S diagnoses, other race/ethnicity, 2030"        = median_and_interval(ps.other, 0),
      "MSM share of incidence (%), 2022"                   = median_and_interval(100 * inc.msm.2022 / inc.2022, 0),
      "MSM share of incidence (%), 2030"                   = median_and_interval(100 * inc.msm.2030 / inc.2030, 0),
      check.names = FALSE)
  }

  # 3. One pair of rows per MSA, then the Total row (all ten MSAs combined)
  #    MSA names are full or short (TABLE.CITY.NAMES, set in SETUP), shown once, blank on the
  #    interval row.
  table1 <- NULL
  for (city in MSAS)
    table1 <- rbind(table1, cbind(MSA = c(TABLE.CITY.NAMES[[city]], ""), table1_rows(LOCATIONS[[city]])))
  table1 <- rbind(table1, cbind(MSA = c("Total", ""), table1_rows(LOCATIONS)))

  write.csv(table1, file.path(TABLE.DIR, "table1_no_intervention.csv"), row.names = FALSE)

  # 4. The same table with the interval in the same cell as the median: "median [lo-hi]".
  #    The median rows are the odd rows of table1, the interval rows the even rows.
  median.rows   <- seq(1, nrow(table1), 2)
  interval.rows <- seq(2, nrow(table1), 2)
  table1.one.cell <- table1[median.rows, ]
  for (col in names(table1)[-1])
    table1.one.cell[[col]] <- paste(table1[[col]][median.rows], table1[[col]][interval.rows])
  rownames(table1.one.cell) <- NULL

  write.csv(table1.one.cell, file.path(TABLE.DIR, "table1_no_intervention_one_cell.csv"), row.names = FALSE)
  table1.one.cell
}



# ****************************************************************************************************
# FIGURE 3 -- % CHANGE IN INCIDENCE WITH VS WITHOUT DOXY-PEP ----
# ****************************************************************************************************
# Two heatmaps: A = MSM, B = total population. Rows are MSAs, columns are coverage levels.
# Each cell is the median % change in incidence in EVAL.YEAR compared with no intervention.
#
# Colours: which value gets which colour. Colours in between are blended.
#   1. 0 to -49%      near white to light blue: fewer infections, short of the target
#   2. -50% or lower  light teal to teal: target met, darker = larger reduction
#   The switch sits at -49.5 because the cell numbers are rounded: a cell showing "-50" is teal.
# Cells at TARGET (-50%) or lower also get a black outline, so the target still shows in
# black-and-white print. All text is black.
{
  FIG3.COLOURS <- c("0"     = "#F7F7F7",   # no change
                    "-49.4" = "#92C5DE",   # just short of the target
                    "-49.5" = "#6BAED6",   # target met
                    "-100"  = "#4393C3")   # 100% reduction
  TARGET <- -50             # % change that counts as meeting the target (Figure 3 outline, Figure 4 line)
  
  # 1. The values
  fig3.tbl <- make_multi_location_table(
    data          = intervention.results,
    locations     = MSAS,
    outcomes      = "pct_change_incidence_vs_noint",
    interventions = COVERAGE.LEVELS,
    years         = EVAL.YEAR,
    stat.type     = "median")

  # 2. One heatmap for one population
  heatmap_panel <- function(subgroup, title) {
    plot_coverage_heatmap(
      fig3.tbl,
      locations     = MSAS,
      subgroup      = subgroup,
      colours       = FIG3.COLOURS,
      legend.breaks = seq(-100, 0, 25),
      highlight.at  = TARGET,
      row.labels    = FIGURE.CITY.NAMES,
      title         = title,
      fill.lab      = "% Incidence Change",
      fixed.aspect  = FALSE)
  }

  # 3. The two panels, side by side with one legend and one title
  fig3.A <- heatmap_panel("msm",   "A: MSM")
  fig3.B <- heatmap_panel("Total", "B: Total population")
  
  no.y.axis <- theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  fig3 <- (fig3.A | fig3.B + no.y.axis) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Projected change in syphilis incidence in 2030, with vs without Doxy-PEP (%)",
      theme = theme(plot.title = element_text(face = "bold", size = 14, colour = "black", hjust = 0.5))) &
    theme(legend.position = "bottom");fig3

  ggsave(file.path(FIG.DIR, "fig3_heatmap_vs_noint.png"), fig3,
         width = if (FULL.NAMES.IN.FIGURES) 12 else 10, height = 5.5, dpi = 300, bg = "white")
  fig3
}


# ****************************************************************************************************
# FIGURE 4 -- THE EFFECT OF DOXY-PEP AT ONE COVERAGE LEVEL ----
# ****************************************************************************************************
# Nine panels at CCRIT in EVAL.YEAR:
#
#                          Total   MSM   Women
#   relative change          A      B      C     box plots: % change in incidence vs no Doxy-PEP
#   infections averted       D      E      F     bars: infections averted, 2022 to 2030
#   incidence rate           G      H      I     per 100,000 of that population in 2030:
#                                                open circle = without, filled circle = with Doxy-PEP
#
# 1. Box = middle 50% of simulations, line = median, whiskers = 2.5th to 97.5th percentile.
#    Bar = median, line across it = 2.5th to 97.5th percentile.
#    Circles = medians.
# 2. A box needs every simulation, so this section reads the calc arrays directly.
# 3. Cities run down the side, in the same order in every panel: smallest projected reduction
#    (total) at the top, largest at the bottom. Each city sits on the same row in all three
#    columns, and grey stripes on every other city help follow a row across the figure.
# 5. Colour shows the population (column): cyan = total, blue = MSM, red = women.
# 4. Heterosexual men have no column, so MSM + women do not add up to the total.
{
  CCRIT  <- "doxy.cov.20"   # doxy coverage providing over 50% deduction among MSM across all cities
  total.calc <- intervention.results$total_calc
  sex.calc   <- intervention.results$sex_calc

  # 1. Every simulation's value for one outcome, one population and one intervention (CCRIT
  #    unless given), in EVAL.YEAR. One row per city x simulation.
  sim_values <- function(arr, outcome, sex = NULL, intervention = CCRIT) {
    keep <- list(outcome = outcome, intervention = intervention, year = EVAL.YEAR)
    if (!is.null(sex)) keep$sex <- sex
    x <- subset_array(arr, keep)
    x <- apply(x, c("location", "sim"), sum)    # every other dimension has a single value
    data.frame(location = rownames(x)[row(x)],
               sim      = colnames(x)[col(x)],
               value    = as.vector(x))
  }

  # 2. All nine panels' values in one table
  f3 <- bind_rows(
    mutate(sim_values(total.calc, "pct_change_incidence_vs_noint"),                measure = "relative",   population = "Total"),
    mutate(sim_values(sex.calc,   "pct_change_incidence_vs_noint",      "msm"),    measure = "relative",   population = "MSM"),
    mutate(sim_values(sex.calc,   "pct_change_incidence_vs_noint",      "female"), measure = "relative",   population = "Women"),
    # infections averted = minus the change in cumulative infections, so the bars are positive
    mutate(sim_values(total.calc, "abs_change_cum_incidence_vs_noint"),            value = -value, measure = "averted", population = "Total"),
    mutate(sim_values(sex.calc,   "abs_change_cum_incidence_vs_noint",  "msm"),    value = -value, measure = "averted", population = "MSM"),
    mutate(sim_values(sex.calc,   "abs_change_cum_incidence_vs_noint",  "female"), value = -value, measure = "averted", population = "Women"),
    # incidence rate in EVAL.YEAR, with Doxy-PEP (CCRIT) and without (noint)
    mutate(sim_values(total.calc, "incidence_rate"),                               measure = "rate_with",    population = "Total"),
    mutate(sim_values(sex.calc,   "incidence_rate",                     "msm"),    measure = "rate_with",    population = "MSM"),
    mutate(sim_values(sex.calc,   "incidence_rate",                     "female"), measure = "rate_with",    population = "Women"),
    mutate(sim_values(total.calc, "incidence_rate",                     intervention = "noint"),           measure = "rate_without", population = "Total"),
    mutate(sim_values(sex.calc,   "incidence_rate", "msm",              intervention = "noint"),           measure = "rate_without", population = "MSM"),
    mutate(sim_values(sex.calc,   "incidence_rate", "female",           intervention = "noint"),           measure = "rate_without", population = "Women"))

  # 3. Location codes (C.12060) -> city names (Atlanta)
  f3$location <- names(LOCATIONS)[match(f3$location, LOCATIONS)]

  # 4. City order: by the median relative change in the total population. The smallest reduction
  #    (the change closest to 0) comes first, the largest reduction (most negative) last.
  city.order <- f3 %>%
    filter(measure == "relative", population == "Total") %>%
    group_by(location) %>%
    summarise(median = median(value)) %>%
    arrange(desc(median)) %>%
    pull(location)

  # 5. The summary for each panel and city: 2.5th, 25th, 50th, 75th and 97.5th percentiles
  f3.box <- f3 %>%
    group_by(measure, population, location) %>%
    summarise(lo95   = quantile(value, 0.025),
              q25    = quantile(value, 0.25),
              median = median(value),
              q75    = quantile(value, 0.75),
              hi95   = quantile(value, 0.975),
              .groups = "drop")

  # 6. One panel. Cities down the side (first city at the top), values across.
  #    Each population has a light fill, and its full colour for the outline and whiskers.
  #    The median is black so it shows on every box.
  #    box_panel() draws row 1, bar_panel() row 2 and rate_panel() row 3.
  F4.FILL <- c(Total = "#80CDC1", MSM = "#92C5DE", Women = "#F4A582")   # light cyan, light blue, light coral
  F4.LINE <- c(Total = "#01665E", MSM = "#2166AC", Women = "#B2182B")   # dark cyan, blue, red

  stripes <- data.frame(ymin = seq(1, length(city.order), 2) - 0.5,     # every other city
                        ymax = seq(1, length(city.order), 2) + 0.5)

  box_panel <- function(which.measure, which.population, title, x.lab, show.cities) {
    d <- filter(f3.box, measure == which.measure, population == which.population)

    p <- ggplot(d, aes(y = location)) +
      geom_rect(data = stripes, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = "grey95") +
      geom_boxplot(aes(xmin = lo95, xlower = q25, xmiddle = median, xupper = q75, xmax = hi95),
                   stat = "identity", orientation = "y", width = 0.6, linewidth = 0.45,
                   fill = F4.FILL[[which.population]], colour = F4.LINE[[which.population]],
                   median.colour = "black", median.linewidth = 0.7) +
      scale_y_discrete(limits = rev(city.order), labels = FIGURE.CITY.NAMES) +
      labs(title = title, x = x.lab, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(text               = element_text(colour = "black"),
            axis.text          = element_text(colour = "black"),
            plot.title         = element_text(face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())

    # city names only in the left column; the other two columns line up with them
    if (!show.cities)
      p <- p + theme(axis.text.y = element_blank())

    # the % row shares one axis, with the target marked
    if (which.measure == "relative")
      p <- p +
        geom_vline(xintercept = TARGET, linetype = "dashed", colour = "grey40", linewidth = 0.3) +
        scale_x_continuous(limits = c(-100, 0), breaks = seq(-100, 0, 25))
    else
      p <- p + scale_x_continuous(labels = scales::comma)

    p
  }

  bar_panel <- function(which.population, title, x.lab, show.cities) {
    d <- filter(f3.box, measure == "averted", population == which.population)

    p <- ggplot(d, aes(y = location)) +
      geom_rect(data = stripes, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = "grey95") +
      geom_col(aes(x = median), orientation = "y", width = 0.6, linewidth = 0.45,
               fill = F4.FILL[[which.population]], colour = F4.LINE[[which.population]]) +
      geom_errorbar(aes(xmin = lo95, xmax = hi95), orientation = "y", width = 0.25,
                    linewidth = 0.45, colour = "black") +
      scale_y_discrete(limits = rev(city.order), labels = FIGURE.CITY.NAMES) +
      scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +   # bars start at 0
      labs(title = title, x = x.lab, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(text               = element_text(colour = "black"),
            axis.text          = element_text(colour = "black"),
            plot.title         = element_text(face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())

    # city names only in the left column; the other two columns line up with them
    if (!show.cities)
      p <- p + theme(axis.text.y = element_blank())

    p
  }

  rate_panel <- function(which.population, title, x.lab, show.cities) {
    without <- filter(f3.box, measure == "rate_without", population == which.population)
    with    <- filter(f3.box, measure == "rate_with",    population == which.population)
    d <- data.frame(location = without$location,
                    without  = without$median,
                    with     = with$median[match(without$location, with$location)])
    colour <- F4.LINE[[which.population]]

    p <- ggplot(d, aes(y = location)) +
      geom_rect(data = stripes, aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE, fill = "grey95") +
      geom_segment(aes(x = without, xend = with, yend = location), colour = colour, linewidth = 0.8) +
      geom_point(aes(x = without), shape = 21, size = 2.6, stroke = 0.9, colour = colour, fill = "white") +
      geom_point(aes(x = with),    shape = 21, size = 2.6, stroke = 0.9, colour = colour, fill = colour) +
      scale_y_discrete(limits = rev(city.order), labels = FIGURE.CITY.NAMES) +
      scale_x_continuous(labels = scales::comma) +
      expand_limits(x = 0) +                                      # the axis starts at 0
      labs(title = title, x = x.lab, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(text               = element_text(colour = "black"),
            axis.text          = element_text(colour = "black"),
            plot.title         = element_text(face = "bold"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())

    # city names only in the left column; the other two columns line up with them
    if (!show.cities)
      p <- p + theme(axis.text.y = element_blank())

    p
  }

  # 7. The nine panels
  # Each row has one title (row_title below); each panel's title names only its population
  pA <- box_panel("relative",   "Total", "A. Total population", "Change in incidence, 2030 (%)",   show.cities = TRUE)
  pB <- box_panel("relative",   "MSM",   "B. MSM",              "Change in incidence, 2030 (%)",   show.cities = FALSE)
  pC <- box_panel("relative",   "Women", "C. Women",            "Change in incidence, 2030 (%)",   show.cities = FALSE)
  pD <- bar_panel(              "Total", "D. Total population", "Infections averted, 2022-2030",   show.cities = TRUE)
  pE <- bar_panel(              "MSM",   "E. MSM",              "Infections averted, 2022-2030",   show.cities = FALSE)
  pF <- bar_panel(              "Women", "F. Women",            "Infections averted, 2022-2030",   show.cities = FALSE)
  pG <- rate_panel(             "Total", "G. Total population",        "Per 100,000 population, 2030\n\u25CB without   \u25CF with Doxy-PEP", show.cities = TRUE)
  pH <- rate_panel(             "MSM",   "H. MSM",              "Per 100,000 MSM, 2030\n\u25CB without   \u25CF with Doxy-PEP",        show.cities = FALSE)
  pI <- rate_panel(             "Women", "I. Women",            "Per 100,000 women, 2030\n\u25CB without   \u25CF with Doxy-PEP",      show.cities = FALSE)

  # 8. Put the rows together, each under its own title
  row_title <- function(text)
    wrap_elements(full = grid::textGrob(text, x = 0, hjust = 0,
                                        gp = grid::gpar(fontsize = 11, fontface = "bold", col = "black")))

  fig4 <- row_title("Relative change in incidence with vs without Doxy-PEP, 2030") /
          (pA | pB | pC) /
          row_title("Infections averted by Doxy-PEP, 2022-2030") /
          (pD | pE | pF) /
          row_title("Incidence rate in 2030, with and without Doxy-PEP") /
          (pG | pH | pI) +
          plot_layout(heights = c(0.07, 1, 0.07, 1, 0.07, 1));fig4

  ggsave(file.path(FIG.DIR, "fig4_impact_boxplots.png"), fig4,
         width = if (FULL.NAMES.IN.FIGURES) 15 else 13, height = 11, dpi = 300, bg = "white")
  fig4
}


# ****************************************************************************************************
# TABLE 2 -- THE VALUES IN FIGURE 4 ----
# ****************************************************************************************************
# Every value plotted in Figure 4, as median with the 95% interval underneath, for each MSA and a
# Total row for the ten MSAs combined. Same coverage (CCRIT) and years as the figure.
#
# COLUMNS, for the total population, MSM and women (the figure's three columns):
#   1. % change in incidence, 2030          row A-C of the figure
#   2. Infections averted, 2022-2030        row D-F
#   3. Incidence rate without Doxy-PEP and with Doxy-PEP, 2030 (per 100,000 of that population)
#                                           row G-I
#
# HOW EACH CELL IS COMPUTED (the same way as Table 1)
#   1. Take the counts (infections, population) in each simulation from the raw arrays.
#   2. For the Total row, sum the counts over the ten MSAs, still within each simulation.
#   3. Compute the % change, infections averted and rates from those counts, within each simulation.
#   4. Report the median and 95% interval across simulations.
#   For each MSA this gives the same numbers as the figure. The Total row is not in the figure.
#
# Run the Figure 4 section first: it sets CCRIT and city.order (the MSAs in the figure's order,
# top to bottom).
{
  total.raw <- intervention.results$total_raw
  sex.raw   <- intervention.results$sex_raw

  # 1. Helpers
  # One outcome, one intervention, the given years and MSAs (all summed), optionally for one group,
  # e.g. list(sex = "msm"): one value per simulation
  sim_count <- function(arr, outcome, intervention, years, locs, group = list()) {
    keep <- c(list(outcome = outcome, intervention = intervention, year = years, location = locs), group)
    apply(subset_array(arr, keep), "sim", sum)
  }

  per_100k <- function(count, pop) 100000 * count / pop

  # The median, then "[2.5th-97.5th]" underneath, as in Table 1
  round_to <- function(x, d) format(round(x, d), nsmall = d, trim = TRUE)
  median_and_interval <- function(x, d)
    c(round_to(median(x), d),
      paste0("[", round_to(quantile(x, 0.025), d), "-", round_to(quantile(x, 0.975), d), "]"))

  # 2. The four columns for one population. arr and group pick the population:
  #      total population   total.raw, list()
  #      MSM                sex.raw,   list(sex = "msm")
  #      women              sex.raw,   list(sex = "female")
  population_columns <- function(label, arr, group, locs) {
    all.years <- as.character(as.integer(YEAR0):as.integer(EVAL.YEAR))   # 2022 to 2030, both included

    # 2a. Counts, without (noint) and with Doxy-PEP (CCRIT)
    inc.without <- sim_count(arr, "incidence",  "noint", EVAL.YEAR, locs, group)
    inc.with    <- sim_count(arr, "incidence",  CCRIT,   EVAL.YEAR, locs, group)
    pop.without <- sim_count(arr, "population", "noint", EVAL.YEAR, locs, group)
    pop.with    <- sim_count(arr, "population", CCRIT,   EVAL.YEAR, locs, group)
    cum.without <- sim_count(arr, "incidence",  "noint", all.years, locs, group)
    cum.with    <- sim_count(arr, "incidence",  CCRIT,   all.years, locs, group)

    # 2b. The columns
    out <- data.frame(
      "% change in incidence, 2030"                      = median_and_interval(100 * (inc.with - inc.without) / inc.without, 0),
      "Infections averted, 2022-2030"                    = median_and_interval(cum.without - cum.with, 0),
      "Incidence rate without Doxy-PEP (per 100,000), 2030" = median_and_interval(per_100k(inc.without, pop.without), 0),
      "Incidence rate with Doxy-PEP (per 100,000), 2030"    = median_and_interval(per_100k(inc.with,    pop.with),    0),
      check.names = FALSE)
    names(out) <- paste0(label, ": ", names(out))
    out
  }

  # 3. The two rows (median, interval) for one MSA, or for several MSAs combined
  table2_rows <- function(locs)
    cbind(population_columns("Total population", total.raw, list(),               locs),
          population_columns("MSM",              sex.raw,   list(sex = "msm"),    locs),
          population_columns("Women",            sex.raw,   list(sex = "female"), locs))

  # 4. One pair of rows per MSA, in the figure's order, then the Total row
  table2 <- NULL
  for (city in city.order)
    table2 <- rbind(table2, cbind(MSA = c(TABLE.CITY.NAMES[[city]], ""), table2_rows(LOCATIONS[[city]])))
  table2 <- rbind(table2, cbind(MSA = c("Total", ""), table2_rows(LOCATIONS)))

  write.csv(table2, file.path(TABLE.DIR, paste0("table2_fig4_values_", CCRIT, ".csv")), row.names = FALSE)
  table2
}
