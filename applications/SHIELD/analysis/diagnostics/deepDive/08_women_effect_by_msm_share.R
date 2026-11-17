# ****************************************************************************************************
# DEEP DIVE 08: PRESENTING THE WOMEN EFFECT AS DEPENDING ON THE SHARE OF EXPOSURE FROM MSM ----
# ****************************************************************************************************
# Script 07 showed that the doxy-PEP effect among women follows the share of women's exposure that comes from
# MSM partners (share.from.msm), which the calibration data do not pin down. Instead of one pooled interval per
# city, this script shows the women effect conditional on that share, in two ways, so they can be compared:
#
#   OPTION 1, WITHIN CITIES: each city's simulations split into tertiles of share.from.msm; women's % reduction
#     (median and 95% interval) in each tertile, at every coverage level. Reads as: "if about X% of women's
#     exposure comes from MSM, the reduction among women is about Y%".
#   OPTION 2, ACROSS CITIES: one point per city, x = median share.from.msm, y = median women's reduction
#     (with 95% intervals), plus each chain's medians as small points. Reads as: "cities where more of women's
#     exposure comes from MSM get a larger spillover benefit".
#
# Both use the infectious version of share.from.msm by default (07: weighted by stage as the model weights
# transmission). share.from.msm is an approximation (no age/race mixing),
# and the tertiles cover only the range of bridging that the 4 chains reached.
#
# USAGE (from jheem_analyses)
#   1. run deepDive/01_doxy_impact_by_chain.R (effects) and deepDive/07_women_exposure_share_from_msm.R
#      (it writes women_share_from_msm_values_*.csv, one row per simulation)
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/08_women_effect_by_msm_share.R")
#   Reads only small CSVs: no simset or array is loaded.
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<code>/deepDive/
#   women_by_msm_share_tertiles_<code>_<date>.csv     city x coverage x population x tertile
#   women_by_msm_share_cities_<code>_<date>.csv       city x coverage: medians and intervals (option 2)
#   women_by_msm_share_option1_<code>_<date>.png      option 1 figure
#   women_by_msm_share_option2_<code>_<date>.png      option 2 figure
#   women_by_msm_share_compare_<code>_<date>.png      both side by side, at CCRIT (needs the patchwork package)
#   Each CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read a CSV back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
library(ggplot2)


## SETTINGS ----
CITIES        <- NULL                              # NULL = all cities in the 07 file
SHARE.VERSION <- "share.infectious"                # main; or "share.incidence" / "share.prevalence" (checks)
COVERAGES     <- paste0("doxy.cov.", c(10, 20, 30, 50))
CCRIT         <- "doxy.cov.20"                     # coverage for option 2 and the side-by-side figure
OUTCOME       <- "pct_change_incidence_vs_noint"
N.GROUPS      <- 3
GROUP.LABELS  <- c("low", "mid", "high")
SHOW.MSM      <- TRUE                              # option 1: MSM as a grey contrast
if (length(GROUP.LABELS) != N.GROUPS) stop("GROUP.LABELS needs one label per group")


## FOLDERS ----
DEEP.DIR <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIBRATION.CODE, "deepDive")
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## COLUMN DESCRIPTIONS ----
COLUMN.NOTES <- list(
    city       = c("MSA name", "One city per block of rows"),
    coverage   = c("Doxy-PEP coverage scenario", "doxy.cov.20 = 20% coverage"),
    population = c("Women, or MSM as a contrast", "-"),
    tertile    = c(paste0("Group of simulations by ", SHARE.VERSION, " within the city (", paste(GROUP.LABELS, collapse = " / "), ")"),
                   "high = most of women's exposure comes from MSM partners"),
    n.sims     = c("Simulations in the tertile", "-"),
    share.median = c(paste0("Median ", SHARE.VERSION, " in the tertile"), "Approximate share of women's exposure from MSM"),
    share.min  = c("Lowest share in the tertile", "-"),
    share.max  = c("Highest share in the tertile", "-"),
    effect.median = c(paste0("Median ", OUTCOME, " (2030) in the tertile"), "Negative = fewer infections with doxy-PEP"),
    effect.lower  = c("2.5th percentile of the effect in the tertile", "-"),
    effect.upper  = c("97.5th percentile of the effect in the tertile", "-"),
    share.lower   = c(paste0("2.5th percentile of ", SHARE.VERSION, " in the city"), "-"),
    share.upper   = c(paste0("97.5th percentile of ", SHARE.VERSION, " in the city"), "-"),
    women.median  = c("Median women's effect in the city (all simulations)", "The pooled value the manuscript reports today"),
    women.lower   = c("2.5th percentile of the women's effect in the city", "-"),
    women.upper   = c("97.5th percentile of the women's effect in the city", "-"),
    flagged       = c("TRUE if 01 flagged this city for women at this coverage (chain medians >= 10 points apart)", "-")
)
column.notes <- function(cols) {
    notes <- lapply(cols, function(col) if (!is.null(COLUMN.NOTES[[col]])) COLUMN.NOTES[[col]] else c("", ""))
    rows  <- as.data.frame(do.call(cbind, notes), stringsAsFactors = FALSE)
    names(rows) <- cols
    rows[[1]] <- paste0(c("WHAT IT IS: ", "HOW TO READ: "), rows[[1]])
    rows
}
write.annotated.csv <- function(df, file) {
    con <- file(file, open = "w")
    on.exit(close(con))
    write.table(column.notes(names(df)), con, sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double")
    write.table(df, con, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double")
}


## INPUTS ----
read.latest <- function(pattern) {
    f <- list.files(DEEP.DIR, pattern = pattern, full.names = TRUE)
    f <- f[!grepl("_backup\\.csv$", f)]
    if (length(f) == 0) stop("No file matching ", pattern, " in ", DEEP.DIR)
    f <- f[which.max(file.mtime(f))]
    skip <- if (grepl("^\"?WHAT IT IS", readLines(f, n = 1))) 2 else 0
    cat("Reading", basename(f), "\n")
    read.csv(f, skip = skip, stringsAsFactors = FALSE)
}
SH   <- read.latest("^women_share_from_msm_values_.*\\.csv$")
VALS <- read.latest("^doxy_chain_values_.*\\.csv$")
SUMM <- read.latest("^doxy_chain_summary_.*\\.csv$")
VALS <- VALS[VALS$outcome == OUTCOME & VALS$coverage %in% COVERAGES, ]
if (!SHARE.VERSION %in% names(SH)) stop(SHARE.VERSION, " is not a column of the 07 values file")
cities <- if (is.null(CITIES)) unique(SH$city) else CITIES
is.flagged <- function(city, cov) any(SUMM$chain.gap.flag[SUMM$city == city & SUMM$population == "Women" &
                                                          SUMM$coverage == cov & SUMM$outcome == OUTCOME] %in% TRUE)


## OPTION 1: TERTILES OF THE SHARE WITHIN EACH CITY ----
tert.rows <- list()
for (city in cities) {
    sh <- SH[SH$city == city, ]
    sh <- sh[order(sh$sim), ]
    x  <- sh[[SHARE.VERSION]]
    g  <- factor(GROUP.LABELS[ceiling(rank(x, ties.method = "first") * N.GROUPS / length(x))], levels = GROUP.LABELS)
    for (pop in c("Women", "MSM")) for (cov in COVERAGES) {
        d <- VALS[VALS$city == city & VALS$population == pop & VALS$coverage == cov, ]
        if (nrow(d) == 0) next
        if (nrow(d) != nrow(sh) || !identical(as.integer(d$chain), as.integer(sh$chain)))
            stop(city, " ", pop, " ", cov, ": the 01 effects and the 07 shares do not line up (number of sims or chain order)")
        for (lev in GROUP.LABELS) {
            i <- g == lev
            q <- quantile(d$value[i], c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)
            tert.rows[[length(tert.rows) + 1]] <- data.frame(
                city = city, coverage = cov, population = pop, tertile = lev, n.sims = sum(i),
                share.median = median(x[i]), share.min = min(x[i]), share.max = max(x[i]),
                effect.median = q[2], effect.lower = q[1], effect.upper = q[3], stringsAsFactors = FALSE)
        }
    }
}
tert <- do.call(rbind, tert.rows)


## OPTION 2: ACROSS CITIES ----
city.rows <- list(); chain.pts <- list()
for (city in cities) {
    sh <- SH[SH$city == city, ]; sh <- sh[order(sh$sim), ]
    x  <- sh[[SHARE.VERSION]]
    qs <- quantile(x, c(0.025, 0.5, 0.975), names = FALSE)
    for (cov in COVERAGES) {
        d <- VALS[VALS$city == city & VALS$population == "Women" & VALS$coverage == cov, ]
        if (nrow(d) == 0) next
        q <- quantile(d$value, c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)
        city.rows[[length(city.rows) + 1]] <- data.frame(
            city = city, coverage = cov, share.median = qs[2], share.lower = qs[1], share.upper = qs[3],
            women.median = q[2], women.lower = q[1], women.upper = q[3], flagged = is.flagged(city, cov),
            stringsAsFactors = FALSE)
        chain.pts[[length(chain.pts) + 1]] <- data.frame(
            city = city, coverage = cov, chain = factor(sort(unique(sh$chain))),
            share = as.numeric(tapply(x, sh$chain, median)), women = as.numeric(tapply(d$value, d$chain, median)))
    }
}
ctab <- do.call(rbind, city.rows)
cpts <- do.call(rbind, chain.pts)


## SAVE TABLES ----
f <- function(what, ext = "csv") file.path(DEEP.DIR, paste0("women_by_msm_share_", what, "_", CALIBRATION.CODE, "_", STAMP, ".", ext))
write.annotated.csv(tert, f("tertiles"))
write.annotated.csv(ctab, f("cities"))


## FIGURES ----
share.lab <- paste0(sub("share.", "", SHARE.VERSION), " version")
# order cities by median share (low to high), the same in both figures
city.order <- ctab[ctab$coverage == CCRIT, ]
city.order <- city.order$city[order(city.order$share.median)]

# Option 1
p1d <- tert[tert$population == "Women" | SHOW.MSM, ]
p1d$coverage <- factor(sub("doxy.cov.", "", p1d$coverage), levels = sub("doxy.cov.", "", COVERAGES))
p1d$city <- factor(p1d$city, levels = city.order)
p1d$tertile <- factor(p1d$tertile, levels = GROUP.LABELS)
xlabs <- unique(p1d[p1d$population == "Women", c("city", "tertile", "share.median")])
xlabs$label <- sprintf("%.0f%%", 100 * xlabs$share.median)
fl <- sapply(levels(p1d$city), function(cty) is.flagged(cty, CCRIT))
p1d$city.lab <- factor(paste0(p1d$city, ifelse(fl[as.character(p1d$city)], " *", "")),
                       levels = paste0(levels(p1d$city), ifelse(fl, " *", "")))
xlabs$city.lab <- factor(paste0(xlabs$city, ifelse(fl[as.character(xlabs$city)], " *", "")), levels = levels(p1d$city.lab))
# one fixed colour per coverage (orange to dark red), so a single coverage is never drawn in a pale colour
COVERAGE.COLOURS <- setNames(colorRampPalette(c("#f46d43", "#a50026"))(length(COVERAGES)), sub("doxy.cov.", "", COVERAGES))
option1 <- function(d) {
    ggplot(d[d$population == "Women", ], aes(x = tertile, y = effect.median, colour = coverage, group = coverage)) +
        { if (SHOW.MSM) geom_line(data = d[d$population == "MSM", ], colour = "grey70", linewidth = 0.4, linetype = "dashed") } +
        geom_line(linewidth = 0.4, position = position_dodge(width = 0.4)) +
        geom_pointrange(aes(ymin = effect.lower, ymax = effect.upper), size = 0.2, linewidth = 0.4,
                        position = position_dodge(width = 0.4)) +
        geom_text(data = xlabs[xlabs$city.lab %in% d$city.lab, ], aes(x = tertile, y = Inf, label = label),
                  inherit.aes = FALSE, vjust = 1.4, size = 2.2, colour = "grey30") +
        facet_wrap(~ city.lab, ncol = 5) +
        scale_colour_manual(values = COVERAGE.COLOURS, drop = TRUE) +
        labs(x = "Tertile of the share of women's exposure from MSM (median share shown at the top)",
             y = "% change in women's incidence, 2030", colour = "Coverage (%)") +
        theme_bw(base_size = 8) + theme(legend.position = "bottom")
}
p1 <- option1(p1d) +
    labs(title = paste0("Option 1: doxy-PEP effect among women by tertile of the share of their exposure from MSM (", CALIBRATION.CODE, ")"),
         subtitle = paste0("Points and bars = median and 95% interval of the simulations in each tertile; ",
                           if (SHOW.MSM) "grey dashed = MSM medians (contrast); " else "",
                           "* = chains >= 10 points apart for women at ", sub("doxy.cov.", "", CCRIT), "%. Share: ", share.lab, ", 2022."))
ggsave(f("option1", "png"), p1, width = 13, height = 6.5, dpi = 200)

# Option 2
p2d <- ctab[ctab$coverage == CCRIT, ]
p2c <- cpts[cpts$coverage == CCRIT, ]
rho <- cor(p2d$share.median, p2d$women.median, method = "spearman")
option2 <- function() {
    ggplot(p2d, aes(x = share.median, y = women.median)) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "grey60", linewidth = 0.4, linetype = "dashed") +
        geom_segment(data = merge(p2c, p2d[, c("city", "share.median", "women.median")], by = "city"),
                     aes(x = share.median, y = women.median, xend = share, yend = women), colour = "grey80", linewidth = 0.3) +
        geom_point(data = p2c, aes(x = share, y = women), colour = "grey55", size = 1) +
        geom_errorbar(aes(ymin = women.lower, ymax = women.upper), width = 0, colour = "grey40", linewidth = 0.3) +
        geom_segment(aes(x = share.lower, xend = share.upper, y = women.median, yend = women.median), colour = "grey40", linewidth = 0.3) +
        geom_point(aes(fill = flagged), shape = 21, size = 3) +
        geom_text(aes(label = city), vjust = -1, size = 2.6) +
        scale_fill_manual(values = c(`TRUE` = "#d95f02", `FALSE` = "#1b9e77"),
                          labels = c(`TRUE` = "chains disagree (>= 10 points)", `FALSE` = "chains agree"), name = NULL) +
        scale_x_continuous(labels = function(v) paste0(round(100 * v), "%")) +
        labs(x = paste0("Share of women's exposure from MSM partners (median, 95% interval; ", share.lab, ", 2022)"),
             y = paste0("% change in women's incidence, 2030, ", sub("doxy.cov.", "", CCRIT), "% coverage")) +
        theme_bw(base_size = 9) + theme(legend.position = "bottom")
}
p2 <- option2() +
    labs(title = paste0("Option 2: doxy-PEP effect among women vs share of their exposure from MSM, across cities (", CALIBRATION.CODE, ")"),
         subtitle = sprintf("Large points = city median with 95%% intervals; small grey points = each chain's medians. Spearman rho across cities = %.2f.", rho))
ggsave(f("option2", "png"), p2, width = 8, height = 6.5, dpi = 200)

# Side by side at CCRIT
if (requireNamespace("patchwork", quietly = TRUE)) {
    a <- option1(p1d[p1d$coverage == sub("doxy.cov.", "", CCRIT), ]) + theme(legend.position = "none") +
        labs(title = "Option 1: within each city, by tertile of the share")
    b <- option2() + labs(title = "Option 2: across cities")
    pc <- patchwork::wrap_plots(a, b, widths = c(1.4, 1)) +
        patchwork::plot_annotation(title = paste0("Women's doxy-PEP effect conditional on the share of their exposure from MSM, ",
                                                  sub("doxy.cov.", "", CCRIT), "% coverage (", CALIBRATION.CODE, ")"))
    ggsave(f("compare", "png"), pc, width = 16, height = 6.5, dpi = 200)
} else cat("Package patchwork not installed: the side-by-side figure was skipped (the two figures are saved separately)\n")


## PRINT ----
cat("\n== Option 1: women's effect by tertile at", CCRIT, "==\n")
t1 <- tert[tert$population == "Women" & tert$coverage == CCRIT, ]
w <- reshape(t1[, c("city", "tertile", "share.median", "effect.median")], idvar = "city", timevar = "tertile", direction = "wide")
print(w[match(city.order, w$city), ], row.names = FALSE, digits = 3)
cat("\n== Option 2: across cities at ", CCRIT, ", Spearman rho = ", round(rho, 2), " ==\n", sep = "")
print(p2d[order(p2d$share.median), c("city", "share.median", "women.median", "women.lower", "women.upper", "flagged")], row.names = FALSE, digits = 3)
cat("\nSaved in ", DEEP.DIR, "\n", sep = "")
