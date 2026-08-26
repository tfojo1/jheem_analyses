library(tidyverse)

# ============================================================================
# HELPERS ----
# ============================================================================
#' Convert a wide locations x (outcome_coverage_year) table to long format
parse_coverage_table <- function(tbl,
                                 location.col = "location",
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$") {
    
    val.cols <- setdiff(names(tbl), location.col)
    parts    <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "))
    
    tbl %>%
        select(all_of(c(location.col, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(-location, names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = as.integer(parts[, 4])),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(location, outcome, coverage, year, value)
}


#' Internal: save a figure if a path was supplied
.save_fig <- function(p, save.path, width, height, dpi) {
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    invisible(p)
}


# FIGURE 1:Ranked bar: coverage needed to reach the target -----
#' Minimum coverage required to reach a target impact, ranked by city
#'
#' The headline figure: one number per location. Cities that never reach the
#' target are drawn as open bars spanning the full axis and labelled.
#'
#' @param tbl Wide table, or the long output of parse_coverage_table().
#' @param target Impact threshold, e.g. 50 for a 50% reduction.
#' @param year Which year to evaluate. Defaults to the latest in the data.
#' @param higher.is.better TRUE if larger values are the goal (e.g. % averted).
#' @param locations Optional subset / ordering of locations to show.
plot_coverage_needed <- function(tbl,
                                 target       = 50,
                                 year         = NULL,
                                 higher.is.better = TRUE,
                                 locations    = NULL,
                                 location.col = "location",
                                 col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                 title        = NULL,
                                 x.lab        = "Coverage required (%)",
                                 bar.fill     = "#2166AC",
                                 unreached.lab = "not reached",
                                 save.path = NULL, width = 7, height = 4.5, dpi = 300) {
    
    long <- if ("coverage" %in% names(tbl)) tbl else
        parse_coverage_table(tbl, location.col, col.pattern)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    if (!is.null(locations)) long <- long %>% filter(location %in% locations)
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
        title <- paste0("Doxy-PEP Coverage needed to reach ", target, "% reduction by ", year)
    
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



# FIGURE 2: Dose-response: impact vs coverage: X-axis coverage ----
#' Impact as a function of coverage, at a fixed year
#'
#' Lines are labelled directly at the right edge rather than by legend.
#' The horizontal reference line at `target` makes the crossing point --
#' i.e. the number in Figure 1 -- readable off this figure too.
plot_dose_response <- function(tbl,
                               target       = 50,
                               year         = NULL,
                               locations    = NULL,
                               location.col = "location",
                               col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                               title        = NULL,
                               x.lab        = "Doxy-PEP coverage (%)",
                               y.lab        = NULL,
                               direct.label = TRUE,
                               palette      = NULL,
                               save.path = NULL, width = 7.5, height = 5, dpi = 300) {
    
    long <- if ("coverage" %in% names(tbl)) tbl else
        parse_coverage_table(tbl, location.col, col.pattern)
    
    if (is.null(year)) year <- max(long$year, na.rm = TRUE)
    long <- long %>% filter(year == !!year)
    if (!is.null(locations)) long <- long %>% filter(location %in% locations)
    if (nrow(long) == 0) stop("No rows left after filtering on year / locations.")
    
    # order the legend / labels by terminal impact so the key reads as a ranking
    ord <- long %>%
        group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location)
    long <- long %>% mutate(location = factor(location, levels = ord))
    
    ends <- long %>% group_by(location) %>%
        slice_max(coverage, n = 1, with_ties = FALSE) %>% ungroup()
    
    if (is.null(y.lab)) y.lab <- paste0(unique(long$outcome), collapse = " / ")
    if (is.null(title)) title <- paste0(y.lab, " by coverage level, ", year)
    
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


# FIGURE 3: Impact over time: X-axis is year  ----
#   1. one city, one coverage      -> single trajectory
#   2. one city, many coverages    -> fan of curves (color.by = "coverage")
#   3. many cities, many coverages -> small multiples
#'
#' @param tbl Wide table (location x outcome_coverage_year) or the long output
#'   of parse_coverage_table(). Must span >= 2 years.
#' @param color.by Which dimension becomes line color: "coverage" (default;
#'   panels = cities) or "location" (panels = coverage levels).
#' @param locations,coverages Optional subsets. A single value of each collapses
#'   the figure to one trajectory.
#' @param year.range Optional c(min, max), e.g. c(2026, 2035).
#' @param target Optional horizontal reference line; NULL to omit.
#' @param outcome Required only if the table holds more than one outcome.
#' @param free.y Independent y scales per panel. Default FALSE -- shared scales
#'   are what make cross-city magnitudes comparable.
#' @param annotate.ends Label the first and last point when a single trajectory
#'   is plotted.
plot_impact_over_time <- function(tbl,
                                  color.by     = c("coverage", "location"),
                                  locations    = NULL,
                                  coverages    = NULL,
                                  year.range   = NULL,
                                  target       = 50,
                                  outcome      = NULL,
                                  location.col = "location",
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  facet.ncol   = NULL,
                                  free.y       = FALSE,
                                  direct.label = TRUE,
                                  annotate.ends = TRUE,
                                  x.lab        = "Year",
                                  y.lab        = NULL,
                                  title        = NULL,
                                  save.path = NULL, width = 10, height = 6, dpi = 300) {
    
    color.by <- match.arg(color.by)
    facet.by <- setdiff(c("coverage", "location"), color.by)
    
    long <- if ("coverage" %in% names(tbl)) tbl else
        parse_coverage_table(tbl, location.col, col.pattern)
    
    # ---- outcome ----------------------------------------------------------
    if (!is.null(outcome)) long <- long %>% filter(outcome == !!outcome)
    if (dplyr::n_distinct(long$outcome) > 1)
        stop("Table holds >1 outcome (", paste(unique(long$outcome), collapse = ", "),
             "). Supply 'outcome' to pick one.")
    
    # ---- subsets ----------------------------------------------------------
    if (!is.null(locations)) {
        miss <- setdiff(locations, unique(long$location))
        if (length(miss)) stop("Location(s) not in table: ", paste(miss, collapse = ", "))
        long <- long %>% filter(location %in% locations)
    }
    if (!is.null(coverages)) {
        miss <- setdiff(coverages, unique(long$coverage))
        if (length(miss)) stop("Coverage level(s) not in table: ", paste(miss, collapse = ", "))
        long <- long %>% filter(coverage %in% coverages)
    }
    if (!is.null(year.range))
        long <- long %>% filter(year >= min(year.range), year <= max(year.range))
    
    if (dplyr::n_distinct(long$year) < 2)
        stop("Need >= 2 years. Build the table with years = as.character(2026:2035).")
    if (nrow(long) == 0) stop("No rows left after filtering.")
    
    n.loc <- dplyr::n_distinct(long$location)
    n.cov <- dplyr::n_distinct(long$coverage)
    single.line <- (n.loc == 1 && n.cov == 1)
    
    # ---- ordering ---------------------------------------------------------
    # cities ranked by terminal impact at the highest coverage shown
    ord.loc <- long %>%
        filter(coverage == max(coverage)) %>%
        group_by(location) %>% slice_max(year, n = 1, with_ties = FALSE) %>%
        arrange(desc(value)) %>% pull(location)
    long <- long %>% mutate(location = factor(location, levels = ord.loc))
    
    if (is.null(y.lab)) y.lab <- unique(long$outcome)
    if (is.null(title)) {
        title <- if (single.line)
            paste0(y.lab, ": ", levels(droplevels(long$location))[1],
                   " at ", unique(long$coverage), "% coverage")
        else if (color.by == "coverage")
            paste0(y.lab, " over time, by coverage level")
        else paste0(y.lab, " over time, by city")
    }
    
    max.yr <- max(long$year, na.rm = TRUE)
    pad    <- if (direct.label && color.by == "location" && !single.line) 4 else 0
    
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
    } else if (color.by == "coverage") {
        p <- p +
            geom_line(aes(color = coverage, group = coverage), linewidth = 0.9) +
            scale_color_viridis_c(option = "C", end = 0.92,
                                  name = "Doxy-PEP\ncoverage (%)",
                                  breaks = sort(unique(long$coverage)))
    } else {
        ends <- long %>% group_by(location, coverage) %>%
            slice_max(year, n = 1, with_ties = FALSE) %>% ungroup()
        p <- p +
            geom_line(aes(color = location, group = location), linewidth = 0.85)
        if (direct.label)
            p <- p + geom_text(data = ends, aes(label = location, color = location),
                               hjust = -0.1, size = 2.9, fontface = "bold",
                               show.legend = FALSE)
    }
    
    # ---- faceting ---------------------------------------------------------
    n.panel <- if (facet.by == "location") n.loc else n.cov
    if (n.panel > 1) {
        lab.fn <- if (facet.by == "coverage")
            as_labeller(function(x) paste0(x, "% coverage")) else label_value
        p <- p + facet_wrap(vars(.data[[facet.by]]), ncol = facet.ncol,
                            labeller = lab.fn,
                            scales = if (free.y) "free_y" else "fixed")
    }
    
    p <- p +
        scale_x_continuous(limits = c(min(long$year), max.yr + pad)) +
        labs(x = x.lab, y = y.lab, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid.minor = element_blank(),
              strip.text       = element_text(face = "bold"),
              plot.title       = element_text(face = "bold", size = 12),
              legend.position  = if (color.by == "location" && direct.label)
                  "none" else "right")
    
    .save_fig(p, save.path, width, height, dpi)
    p
}


# examples ----
if (1==1){
    FIG.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/figures/") else "figures/"
    
    # --- Figure 1: headline ------------------------------------------------------
    # Needs a table using a single year (e.g., 2035) and spanning multiple locations. Build it with the multi-location
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2035"),
        stat.type     = "median",
        save          = F
    )
    f1 <- plot_coverage_needed(pct.inc.ave.tbl,
                               target    = 50,
                               
                               save.path = paste0(FIG.DIR, "fig1_coverage_needed.png"))
    f1
    # --- Figure 2: dose-response -------------------------------------------------
    f2 <- plot_dose_response(pct.inc.ave.tbl,
                             target    = 50,
                             y.lab     = "Diagnoses averted (%)",
                             save.path = paste0(FIG.DIR, "fig2_dose_response.png"))
    
    f2
    # --- Figure 3: trajectories --------------------------------------------------
    
    traj.tbl = make_multi_location_table(
        data          = list(total_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = as.character(c(2022:2035)),
        stat.type     = "median",
        save          = F
    )
    
    # ---- View 1: one city, one scenario ----------------------------------------
    plot_impact_over_time(traj.tbl,
                          locations = "Baltimore",
                          coverages = 10,
                          year.range = c(2026, 2035),
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 2: one city, all coverage levels ---------------------------------
    plot_impact_over_time(traj.tbl,
                          locations  = "Baltimore",
                          color.by   = "coverage",
                          year.range = c(2026, 2035),
                          y.lab      = "Diagnoses averted in Baltimore (%)",
                          save.path = paste0(FIG.DIR, "fig3_baltimore_2035.png") )
    
    # ---- View 3: all cities, all coverage levels (small multiples) -------------
    plot_impact_over_time(traj.tbl,
                          color.by   = "coverage",
                          year.range = c(2026, 2035),
                          facet.ncol = 5,
                          y.lab      = "Diagnoses averted (%)",
                          save.path = paste0(FIG.DIR, "fig3_multi_by_coverage.png") )
    
    # ---- View 3b: flipped -- panels are coverage levels, lines are cities ------
    plot_impact_over_time(traj.tbl,
                          color.by   = "location",
                          coverages  = c(10, 30, 60, 90),
                          year.range = c(2026, 2035),
                          facet.ncol = 4,
                          save.path = paste0(FIG.DIR, "fig3_multi_by_location.png") )
   
}