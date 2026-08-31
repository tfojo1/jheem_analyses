#' Heat-map a locations × coverage table with a diverging fill
#'
#' @param tbl Wide data frame: one row per location, one column per
#'   outcome × coverage × year combination.
#' @param location.col Name of the row-label column.
#' @param col.pattern Regex with three capture groups — outcome, coverage, year —
#'   matching the value columns.
#' @param midpoint Value placed at the neutral (white) color. Also the default
#'   threshold used for row ordering.
#' @param limits Fill scale bounds; NULL auto-computes a range symmetric about
#'   midpoint so the neutral color stays at midpoint.
#' @param higher.is.better If FALSE, the palette is flipped (high = blue).
#' @param order.rows "threshold" sorts by the lowest coverage reaching the
#'   threshold, producing a monotone staircase; also "max", "alpha", "none".
library(tidyverse)

#' Heat-map a locations x coverage table with a diverging fill
#'
#' Works with or without stratification columns. A table with no `subgroup`
#' column behaves exactly as before: one row per location. A table carrying a
#' `subgroup` column (from `data = list(total_raw_results, sex_results)`) can either
#' be filtered to one stratum, or plotted with a location x subgroup y axis.
#'
#' @param tbl Wide data frame: identifier column(s) plus one column per
#'   outcome x coverage x year combination.
#' @param location.col Name of the location column.
#' @param id.cols Additional non-value identifier columns. Any that are absent
#'   from `tbl` are silently ignored, so the default is safe for totals-only
#'   tables.
#' @param subgroup Optional character vector of stratum values to retain, e.g.
#'   "Total" or c("Total", "msm"). NULL keeps every row present.
#' @param col.pattern Regex with three capture groups: outcome, coverage, year.
#' @param midpoint Value placed at the neutral (white) colour.
#' @param threshold Value used for row ordering; defaults to `midpoint`.
#' @param limits Fill scale bounds. NULL auto-computes a range symmetric about
#'   `midpoint`, which is what keeps white pinned to the threshold.
#' @param higher.is.better FALSE flips the palette and the ordering test.
#' @param order.rows "threshold" (lowest coverage reaching `threshold`), "max",
#'   "alpha", or "none".
#' @param row.sep Separator used when both location and subgroup label a row.
#' @return A ggplot object, invisibly saved to `save.path` if supplied.
plot_coverage_heatmap <- function(tbl,
                                  location.col = "location",
                                  id.cols      = c("subgroup", "outcome.group"),
                                  subgroup     = NULL,
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  midpoint     = 50,
                                  threshold    = midpoint,
                                  limits       = NULL,
                                  higher.is.better = TRUE,
                                  order.rows   = c("threshold", "max", "alpha", "none"),
                                  label.digits = 0,
                                  show.labels  = TRUE,
                                  row.sep      = " \u2014 ",
                                  title        = NULL,
                                  x.lab        = "Doxy-PEP coverage (%)",
                                  fill.lab     = NULL,
                                  save.path    = NULL,
                                  width = 8, height = 5, dpi = 300) {
    
    order.rows <- match.arg(order.rows)
    
    if (!location.col %in% names(tbl))
        stop("Column '", location.col, "' not found in 'tbl'.")
    
    # ---- identifier columns actually present ------------------------------
    # anything matching col.pattern is a value column, never an identifier
    id.cols <- intersect(id.cols, names(tbl))
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
    
    # ---- parse the value columns ------------------------------------------
    val.cols <- setdiff(names(tbl), c(location.col, id.cols))
    if (length(val.cols) == 0) stop("No value columns left after removing identifiers.")
    
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
                         year     = parts[, 4]),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # ---- build the row label ----------------------------------------------
    # only append the stratum when more than one is being shown, so a
    # totals-only or single-stratum table keeps clean city names
    strat.col <- if (length(id.cols) > 0) id.cols[1] else NULL
    if (!is.null(strat.col) && dplyr::n_distinct(long[[strat.col]]) > 1)
        long <- long %>%
        mutate(row.id = paste0(location, row.sep, .data[[strat.col]]))
    else
        long <- long %>% mutate(row.id = location)
    
    # ---- row ordering ------------------------------------------------------
    crossed <- function(v) if (higher.is.better) v >= threshold else v <= threshold
    ord <- long %>%
        group_by(row.id) %>%
        summarise(cross = suppressWarnings(min(coverage[crossed(value)])),
                  best  = if (higher.is.better) max(value, na.rm = TRUE)
                  else min(value, na.rm = TRUE),
                  .groups = "drop")
    row.order <- switch(order.rows,
                        threshold = ord %>% arrange(cross, if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        max       = ord %>% arrange(if (higher.is.better) desc(best) else best) %>% pull(row.id),
                        alpha     = sort(unique(long$row.id)),
                        none      = unique(long$row.id))
    
    long <- long %>%
        mutate(row.id   = factor(row.id, levels = rev(row.order)),
               coverage = factor(coverage, levels = sort(unique(coverage))))
    
    # ---- fill scale --------------------------------------------------------
    if (is.null(limits)) limits <- c(0, 100)
    pal <- if (higher.is.better) c("#B2182B", "#1A9850") else c("#1A9850", "#B2182B")
    
    if (is.null(fill.lab)) fill.lab <- paste(unique(long$outcome), collapse = " / ")
    
    if (is.null(title) && dplyr::n_distinct(long$year) == 1) {
        title <- paste0(fill.lab, ", ", unique(long$year))
        if (!is.null(subgroup) && length(subgroup) == 1)
            title <- paste0(title, " (", subgroup, ")")
    }
    
    p <- ggplot(long, aes(x = coverage, y = row.id, fill = value)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradientn( colours = c(pal[1], "#F7F7F7", pal[2]),
                              values  = scales::rescale(c(limits[1], midpoint, limits[2]),
                                                        from = limits),
                              limits  = limits,
                              oob     = scales::squish,
                              name    = fill.lab) +
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        labs(x = x.lab, y = NULL, title = title) +
        theme_minimal(base_size = 11) +
        theme(panel.grid = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(face = "bold", size = 12),
              legend.key.height = unit(1.2, "cm"))
    
    if (show.labels)
        p <- p +
        geom_text(aes(label = format(round(value, label.digits),
                                     nsmall = label.digits),
                      color = ifelse(value >= midpoint,
                                     (value - midpoint) / (limits[2] - midpoint),
                                     (midpoint - value) / (midpoint - limits[1])) > 0.55),
                  size = 3.4, fontface = "bold", show.legend = FALSE) +
        scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "grey15"))
    
    n.facet <- dplyr::n_distinct(paste(long$outcome, long$year))
    if (n.facet > 1) p <- p + facet_wrap(~ outcome + year, scales = "free_x")
    else             p <- p + coord_fixed(ratio = 0.75)
    
    if (!is.null(save.path)) {
        dir.create(dirname(save.path), recursive = TRUE, showWarnings = FALSE)
        ggsave(save.path, p, width = width, height = height, dpi = dpi)
        message("Figure written to: ", normalizePath(save.path, winslash = "/"))
    }
    
    p
}


# ============================================================================
# EXAMPLES
# ============================================================================
if (1==2){
    # BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
    total_results=get(load(file = paste0(BASE.PATH,"/total_results.Rdata")))
    # total_raw_results=get(load(file = paste0(BASE.PATH,"/total_raw_results.Rdata")))
    # sex_raw_results=get(load(file = paste0(BASE.PATH,"/sex_raw_results.Rdata")))
    # # total_calc_results=get(load(file = paste0(BASE.PATH,"/total_calc_results")))
    # # sex_calc_results=get(load(file = paste0(BASE.PATH,"/sex_calc_results")))
    # ****************************************************************************************************
    
    # % incidence averted among MSM
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_raw_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    plot_coverage_heatmap(pct.inc.ave.tbl,
                          midpoint  = 50,
                          save.path = paste0(BASE.PATH, "/figures/pct_inc_averted.png"))
    
    # pct.diag.ave.tbl = make_multi_location_table(
    #     data          = list(total_raw_results),
    #     locations     = names(SHIELD.TEN.MSAS),
    #     outcomes      = c("pct_diagnosis_averted"),
    #     interventions = paste0("doxy.cov.",seq(10,100,10)),
    #     years         = c("2035"),
    #     stat.type     = "median",
    #     save          = F
    # )
    # plot_coverage_heatmap(pct.diag.ave.tbl,
    #                       midpoint  = 50,
    #                       save.path = paste0(BASE.PATH, "/figures/pct_diag_averted.png"))
    
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2035"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    
    plot_coverage_heatmap(pct.inc.ave.tbl,
                          midpoint  = 50,
                          title = "% incidence averted MSM, 2035",
                          save.path = paste0(BASE.PATH, "/figures/pct_inc_averted_msm.png"))
    
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "msm",
        save          = F
    )
    
    plot_coverage_heatmap(pct.inc.ave.tbl,
                          midpoint  = 50,
                          title = "% incidence averted MSM, 2030",
                          save.path = paste0(BASE.PATH, "/figures/pct_inc_averted_msm.png"))
    
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(sex_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2030"),
        stat.type     = "median",
        filter.by.strat = "female",
        save          = F
    )
    plot_coverage_heatmap(pct.inc.ave.tbl,
                          midpoint  = 50,
                          title = "% incidence averted Women, 2030",
                          save.path = paste0(BASE.PATH, "/figures/pct_inc_averted_female.png"))
}
