# ****************************************************************************************************
# 03b MANUSCRIPT: MAIN TABLE AND FIGURES ----
# ****************************************************************************************************
# Builds the three main-text outputs. Run it after 02_generate_custom_outcomes.R.
#
#   1. Table 1    the epidemic without Doxy-PEP, 2022 and 2030          tables/table1_no_intervention.csv
#   2. Figure 3   % change in incidence with vs without Doxy-PEP        figures/fig3_heatmap_vs_noint.png
#   3. Figure 4   the effect of Doxy-PEP at one coverage level          figures/fig4_impact_boxplots.png
#
# Run SETUP first. After that, each section runs on its own.
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

# The result arrays. Reused if already in memory; force.reload = TRUE reads them from disk again.
intervention.results <- load.shield.results(CALIB.NAME)


#to see outcome names:
# dimnames(intervention.results$total_calc)
dimnames(intervention.results$sex_calc)
# ****************************************************************************************************
# TABLE 1 -- THE EPIDEMIC WITHOUT DOXY-PEP ----
# ****************************************************************************************************
# One row per MSA, with a row of 95% credible intervals under it. No intervention.
# Every column is for the total population, except the MSM share of incidence.
{
  # 1. Total population
  t1.total <- make_multi_location_table(
    data            = intervention.results,
    locations       = MSAS,
    outcomes        = c("incidence",
                        "incidence_rate",
                        "pct_change_incidence_rate_vs_year0",
                        "diagnosis.ps",
                        "diagnosis_ps_per_incidence"),
    interventions   = "noint",
    filter.by.strat = "Total",
    years           = c(YEAR0, EVAL.YEAR),
    stat.type       = "median.ci",
    digits          = c(.default = 0, diagnosis_ps_per_incidence = 2))

  # 2. MSM share of incidence
  t1.msm <- make_multi_location_table(
    data            = intervention.results,
    locations       = MSAS,
    outcomes        = "share_incidence",
    interventions   = "noint",
    filter.by.strat = "msm",
    years           = c(YEAR0, EVAL.YEAR),
    stat.type       = "median.ci")

  # 3. Put the two side by side
  table1 <- left_join(t1.total,
                      select(t1.msm, location, stat, starts_with("share_incidence")),
                      by = c("location", "stat"))

  # 4. Pick the columns, in this order, and label them.
  #    Columns come back named <outcome>_<intervention>_<year>. The names below are written for
  #    2022 and 2030: if you change YEAR0 or EVAL.YEAR, change them too.
  col.labels <- c(
    location                                      = "MSA",
    incidence_noint_2022                          = "Incident infections, 2022",
    incidence_noint_2030                          = "Incident infections, 2030",
    incidence_rate_noint_2022                     = "Incidence rate (per 100,000 persons), 2022",
    incidence_rate_noint_2030                     = "Incidence rate (per 100,000 persons), 2030",
    pct_change_incidence_rate_vs_year0_noint_2030 = "Change in incidence rate, 2022-2030 (%)",
    diagnosis.ps_noint_2022                       = "P&S diagnoses, 2022",
    diagnosis.ps_noint_2030                       = "P&S diagnoses, 2030",
    diagnosis_ps_per_incidence_noint_2022         = "P&S diagnoses per incident infection, 2022",
    diagnosis_ps_per_incidence_noint_2030         = "P&S diagnoses per incident infection, 2030",
    share_incidence_noint_2022                    = "MSM share of incidence (%), 2022",
    share_incidence_noint_2030                    = "MSM share of incidence (%), 2030")

  table1 <- table1[names(col.labels)]
  names(table1) <- col.labels

  # 5. Show each MSA name once (blank on the credible-interval row)
  table1$MSA[duplicated(table1$MSA)] <- ""

  write.csv(table1, file.path(TABLE.DIR, "table1_no_intervention.csv"), row.names = FALSE)
  table1
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
         width = 10, height = 5.5, dpi = 300, bg = "white")
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
      scale_y_discrete(limits = rev(city.order)) +
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
      scale_y_discrete(limits = rev(city.order)) +
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
      scale_y_discrete(limits = rev(city.order)) +
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
         width = 13, height = 11, dpi = 300, bg = "white")
  fig4
}
