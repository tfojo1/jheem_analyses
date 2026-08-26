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
plot_coverage_heatmap <- function(tbl,
                                  location.col = "location",
                                  col.pattern  = "^(.*)_doxy\\.cov\\.(\\d+)_(\\d+)$",
                                  midpoint     = 50,
                                  threshold    = midpoint,
                                  limits       = NULL,
                                  higher.is.better = TRUE,
                                  order.rows   = c("threshold", "max", "alpha", "none"),
                                  label.digits = 0,
                                  show.labels  = TRUE,
                                  title        = NULL,
                                  x.lab        = "Doxy-PEP coverage (%)",
                                  fill.lab     = NULL,
                                  save.path    = NULL,
                                  width = 8, height = 5, dpi = 300) {
    
    order.rows <- match.arg(order.rows)
    
    val.cols <- setdiff(names(tbl), location.col)
    parts    <- stringr::str_match(val.cols, col.pattern)
    if (any(is.na(parts[, 1])))
        stop("Column(s) not matching 'col.pattern': ",
             paste(val.cols[is.na(parts[, 1])], collapse = ", "))
    
    long <- tbl %>%
        select(all_of(c(location.col, val.cols))) %>%
        rename(location = all_of(location.col)) %>%
        pivot_longer(-location, names_to = "colname", values_to = "value") %>%
        left_join(tibble(colname  = val.cols,
                         outcome  = parts[, 2],
                         coverage = as.integer(parts[, 3]),
                         year     = parts[, 4]),
                  by = "colname") %>%
        mutate(value = as.numeric(value)) %>%
        select(-colname)
    
    # --- row ordering ------------------------------------------------------
    crossed <- function(v) if (higher.is.better) v >= threshold else v <= threshold
    ord <- long %>%
        group_by(location) %>%
        summarise(cross = suppressWarnings(min(coverage[crossed(value)])),
                  best  = if (higher.is.better) max(value) else min(value),
                  .groups = "drop")
    loc.order <- switch(order.rows,
                        threshold = ord %>% arrange(cross, if (higher.is.better) desc(best) else best) %>% pull(location),
                        max       = ord %>% arrange(if (higher.is.better) desc(best) else best) %>% pull(location),
                        alpha     = sort(unique(long$location)),
                        none      = unique(long$location))
    
    long <- long %>%
        mutate(location = factor(location, levels = rev(loc.order)),
               coverage = factor(coverage, levels = sort(unique(coverage))))
    
    # --- scale -------------------------------------------------------------
    if (is.null(limits)) {
        half   <- max(abs(range(long$value, na.rm = TRUE) - midpoint))
        limits <- c(midpoint - half, midpoint + half)
    }
    half <- max(abs(limits - midpoint))
    pal  <- if (higher.is.better) c("#2166AC", "#B2182B") else c("#B2182B", "#2166AC")
    
    if (is.null(fill.lab)) fill.lab <- paste(unique(long$outcome), collapse = " / ")
    if (is.null(title) && dplyr::n_distinct(long$year) == 1)
        title <- paste0(fill.lab, ", ", unique(long$year))
    
    p <- ggplot(long, aes(x = coverage, y = location, fill = value)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradient2(low = pal[1], mid = "#F7F7F7", high = pal[2],
                             midpoint = midpoint, limits = limits,
                             name = fill.lab) +
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
        geom_text(aes(label = format(round(value, label.digits), nsmall = label.digits),
                      color = abs(value - midpoint) > 0.55 * half),
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

# *********
if (1==1){
    # ROOT.DIR # is set by the specification
    # BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
    # total_results=get(load(file = paste0(BASE.PATH,"/total_results.Rdata")))
    # ****************************************************************************************************
    
    
    pct.inc.ave.tbl = make_multi_location_table(
        data          = list(total_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_incidence_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2035"),
        stat.type     = "median",
        save          = F
    )
    plot_coverage_heatmap(pct.inc.ave.tbl,
                          midpoint  = 50,
                          save.path = paste0(BASE.PATH, "/figures/pct_inc_averted.png"))
    
    pct.diag.ave.tbl = make_multi_location_table(
        data          = list(total_results),
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_diagnosis_averted"),
        interventions = paste0("doxy.cov.",seq(10,100,10)),
        years         = c("2035"),
        stat.type     = "median",
        save          = F
    )
    plot_coverage_heatmap(pct.diag.ave.tbl,
                          midpoint  = 50,
                          save.path = paste0(BASE.PATH, "/figures/pct_diag_averted.png"))
    
 
    
}
