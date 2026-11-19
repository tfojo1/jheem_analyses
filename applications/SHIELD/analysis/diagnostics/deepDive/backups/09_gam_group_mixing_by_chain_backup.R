# ****************************************************************************************************
# DEEP DIVE 09: DO THE GAM PARAMETER GROUPS MIX BETTER THAN SINGLE PARAMETERS? ----
# ****************************************************************************************************
# Single parameters do not mix across the 4 MCMC chains (stage 4 mixing report). The GAM sensitivity
# analysis (analysis/sensitivity_analyses/gam/) scores groups of related parameters. This script asks:
# at the GROUP level, do the 4 chains cover the same range, so that every chain spans the whole
# x axis, even where the group's single parameters do not?
#
# WHAT IT DOES, for each of the 21 GAM groups and each city:
#   1. Summarizes the group's parameters as ONE value per simulation (x), in two ways (X.TYPES):
#        fitted : the group's combined effect on the outcome = E[outcome | group's parameters], the
#                 fitted value of the same kind of regression as the GAM analysis (GAM for 1-2
#                 parameters, MARS/earth for more). On the outcome scale; var(x) / var(outcome) is
#                 (approximately) the group's variance share.
#        pc1    : first principal component of the group's parameters (each standardized). Uses
#                 parameter values only, not the outcome. Sign is arbitrary. For a one-parameter
#                 group it is the standardized parameter itself.
#   2. Plots x against the outcome (2030 % reduction in TOTAL incidence, INTERVENTION vs no
#      doxy-PEP in the same simulation; positive = fewer cases), one point per simulation,
#      coloured by chain.
#   3. Measures how far apart the chains are along x:
#        between.chain.share : share of the variance of x that lies between the chain means
#                              (R^2 of x on chain). ~0 = chains overlap (mixed at the group level);
#                              ~1 = each chain sits in its own part of the axis.
#        chain.coverage.min  : smallest share of the pooled 2.5-97.5% range of x that one chain's
#                              own 2.5-97.5% range covers (1 = every chain spans the full range).
#      The same between-chain share is computed for each single parameter in the group (raw values)
#      and summarized as their median, to compare group vs single parameters directly.
#
# HOW TO READ THE FIGURES
#   One PDF per x type, one page per group, one panel per city (strip: between-chain share of x).
#   Mixed at the group level: the four colours spread over the whole x axis.
#   Not mixed: each colour occupies its own band of x.
#   For 'fitted', x is a prediction of the outcome, so x and y are related by construction:
#   look at how the colours spread along x, not at the slope.
#
# SOURCES
#   Groups, parameters and outcome settings: analysis/sensitivity_analyses/gam/00_gam_settings.R
#   (its TEST.MSA is ignored here; use CITIES below)
#   Parameters: extracts/params/<MSA>.Rdata (saved by intervention/01a, no-intervention simset order)
#   Outcome: total_calc_results.Rdata (the arrays the GAM analysis uses)
#   Chain of each simulation: CHAIN.SOURCE below (as in 01_doxy_impact_by_chain.R), cached per city
#
# MEMORY
#   total_calc_results.Rdata is loaded once. CHAIN.SOURCE = "simset" loads one no-intervention simset
#   per city, one at a time, ONLY the first time; the chain labels are then cached in chain_labels/.
#   No simulation is run.
#
# USAGE (from jheem_analyses)
#   1. check the SETTINGS below (try one city first: CITIES <- c("Atlanta"))
#   2. source("applications/SHIELD/analysis/diagnostics/deepDive/09_gam_group_mixing_by_chain.R")
#
# OUTPUTS: <jheem root>/shield/calibration_diagnostics/<CALIB.CODE>/deepDive/
#   gam_group_mixing_<x type>_<code>_<date>.pdf   one page per group, one panel per city
#   gam_group_mixing_<code>_<date>.csv            one row per city x group x x type
#   chain_labels/<MSA code>.Rdata                 cached chain of each simulation
#   The CSV starts with two description rows (WHAT IT IS, HOW TO READ), then the column names.
#   To read it back into R: read.csv(file, skip = 2)
# ****************************************************************************************************

## SOURCES ----
# GAM settings: checks and attaches the packages (tidyverse, voi, mgcv, earth, scales), loads the
# SHIELD base files if needed, and defines PARAM_GROUPS, assign_groups(), get_param_matrix(),
# CALIB.CODE, N.SIM, EVAL.YEAR, INTERVENTION, MAX.GAM.PARS and BASE.PATH
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/00_gam_settings.R')


## SETTINGS ----
CITIES       <- NULL                  # NULL = all 10; or e.g. c("Atlanta", "LA")
X.TYPES      <- c("fitted", "pc1")    # one PDF each (see WHAT IT DOES)
N.CHAINS     <- 4
CHAIN.SOURCE <- "simset"              # "simset": simset$simulation.chain, read once per city, then cached
                                      # "blocks": sims 1-250 = chain 1, 251-500 = chain 2, ... (only if the
                                      #           order is known, e.g. confirmed by "simset")
NOINT        <- "noint"
CHAIN.COLORS <- c("1" = "#0072B2", "2" = "#E69F00", "3" = "#009E73", "4" = "#CC79A7")   # Okabe-Ito (colour-blind safe)


## FOLDERS ----
LOCS <- SHIELD.TEN.MSAS                        # names = city, values = location code
if (!is.null(CITIES)) {
    if (!all(CITIES %in% names(LOCS))) stop("Unknown city: ", paste(setdiff(CITIES, names(LOCS)), collapse = ", "))
    LOCS <- LOCS[CITIES]
}
OUT.DIR   <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", CALIB.CODE, "deepDive")
CHAIN.DIR <- file.path(OUT.DIR, "chain_labels")
dir.create(CHAIN.DIR, recursive = TRUE, showWarnings = FALSE)
STAMP <- format(Sys.time(), "%Y%m%d_%H%M")


## HELPERS ----
# Chain of every simulation, in the order of the sim dimension (same logic as 01_doxy_impact_by_chain.R,
# plus a cache so each simset is read only once)
get.chains <- function(loc, n.sim) {
    if (CHAIN.SOURCE == "blocks") {
        if (n.sim %% N.CHAINS != 0) stop(n.sim, " simulations do not split into ", N.CHAINS, " equal blocks")
        return(rep(seq_len(N.CHAINS), each = n.sim / N.CHAINS))
    }
    cache.file <- file.path(CHAIN.DIR, paste0(loc, ".Rdata"))
    if (file.exists(cache.file)) return(get(load(cache.file)))

    # 1. the no-intervention simset the arrays and parameter files were built from
    sim.file <- file.path(get.jheem.root.directory(), "simulations", "shield", paste0(CALIB.CODE, "-", n.sim),
                          loc, paste0("shield_", CALIB.CODE, "-", n.sim, "_", loc, "_", NOINT, ".Rdata"))
    chains <- NULL
    if (file.exists(sim.file)) {
        cat("  reading chain labels from", basename(sim.file), "(first time only)\n")
        simset <- load.simulation.set(sim.file)
        chains <- simset$simulation.chain
        rm(simset); gc(verbose = FALSE)
    }
    # 2. the baseline calibration simset of the same size
    if (is.null(chains)) {
        cat("  no chain labels in the intervention simset; reading the calibration simset\n")
        simset <- retrieve.simulation.set(version = "shield", location = loc,
                                          calibration.code = CALIB.CODE, n.sim = n.sim)
        chains <- simset$simulation.chain
        rm(simset); gc(verbose = FALSE)
    }
    if (is.null(chains)) stop("No chain labels found for ", loc, ". Set CHAIN.SOURCE <- \"blocks\" only if the order is known.")
    if (length(chains) != n.sim) stop("The simset for ", loc, " has ", length(chains), " sims, the parameters ", n.sim)
    chains <- as.integer(factor(chains))   # chains numbered 1..k
    save(chains, file = cache.file)
    chains
}

# One value per simulation summarizing a group's parameters (see WHAT IT DOES, step 1)
group_x <- function(pars, inputs, y, type) {
    X <- inputs[, pars, drop = FALSE]
    if (type == "pc1") {
        if (ncol(X) == 1) return(as.numeric(scale(X[, 1])))
        return(as.numeric(prcomp(X, scale. = TRUE)$x[, 1]))
    }
    # fitted: E[y | group], same kinds of regression as the GAM analysis
    if (length(pars) == 1) {
        fit <- mgcv::gam(y ~ s(x1), data = data.frame(y = y, x1 = X[, 1]))
    } else if (length(pars) <= MAX.GAM.PARS) {
        fit <- mgcv::gam(y ~ te(x1, x2), data = data.frame(y = y, x1 = X[, 1], x2 = X[, 2]))
    } else {
        fit <- earth::earth(x = X, y = y)
    }
    as.numeric(fitted(fit))
}

# How far apart the chains are along x (see WHAT IT DOES, step 3)
between_chain_share <- function(x, chains) summary(lm(x ~ factor(chains)))$r.squared

chain_coverage_min <- function(x, chains) {
    pooled <- quantile(x, c(0.025, 0.975))
    if (pooled[2] <= pooled[1]) return(NA_real_)
    min(sapply(split(x, chains), function(v) {
        q <- quantile(v, c(0.025, 0.975))
        max(0, min(q[2], pooled[2]) - max(q[1], pooled[1])) / (pooled[2] - pooled[1])
    }))
}

# CSV with two description rows (WHAT IT IS, HOW TO READ), then the results
write.annotated.csv <- function(df, file, what, how) {
    writeLines(paste0('"', c(paste("WHAT IT IS:", what), paste("HOW TO READ:", how)), '"'), file)
    suppressWarnings(write.table(df, file, sep = ",", row.names = FALSE, append = TRUE))
}


## LOAD ----
total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))


## RUN ----
rows   <- list()
points <- list()

for (city in names(LOCS)) {
    loc <- unname(LOCS[city])
    cat("City:", city, "(", loc, ") at", format(Sys.time(), "%H:%M:%S"), "\n")

    inputs <- as.data.frame(get_param_matrix(loc))
    inputs <- inputs[, apply(inputs, 2, var) > 0, drop = FALSE]          # drop fixed parameters
    y      <- -1 * total_calc_results[EVAL.YEAR, , INTERVENTION, loc, "pct_change_incidence_vs_noint"]
    chains <- get.chains(loc, nrow(inputs))
    stopifnot(length(y) == nrow(inputs), length(chains) == nrow(inputs))   # sims must line up

    ok     <- !is.na(y)
    inputs <- inputs[ok, , drop = FALSE]
    y      <- y[ok]
    chains <- chains[ok]
    if (sum(!ok) > 0) cat("  dropped", sum(!ok), "sims with NA outcome\n")

    single_share <- sapply(inputs, between_chain_share, chains = chains)   # each parameter on its own
    y_share      <- between_chain_share(y, chains)
    groups       <- assign_groups(colnames(inputs))

    for (g in names(groups)) {
        for (type in X.TYPES) {
            x <- group_x(groups[[g]], inputs, y, type)
            rows[[length(rows) + 1]] <- tibble(
                city                       = city,
                msa                        = loc,
                group                      = g,
                n_params                   = length(groups[[g]]),
                x_type                     = type,
                between_chain_share_x      = between_chain_share(x, chains),
                chain_coverage_min_x       = chain_coverage_min(x, chains),
                median_single_param_share  = median(single_share[groups[[g]]]),
                max_single_param_share     = max(single_share[groups[[g]]]),
                between_chain_share_y      = y_share,
                fitted_var_share_pct       = if (type == "fitted") 100 * var(x) / var(y) else NA_real_
            )
            points[[length(points) + 1]] <- tibble(city = city, group = g, x_type = type,
                                                   x = x, y = y, chain = factor(chains))
        }
    }
}

summary_df <- bind_rows(rows) %>% mutate(across(where(is.numeric), ~ round(.x, 3)))
points_df  <- bind_rows(points)


## SAVE TABLE ----
f.csv <- file.path(OUT.DIR, paste0("gam_group_mixing_", CALIB.CODE, "_", STAMP, ".csv"))
write.annotated.csv(
    summary_df, f.csv,
    what = paste0("One row per city x GAM parameter group x summary type (x_type: fitted = group's combined effect on the 2030 % reduction in total incidence, ",
                  INTERVENTION, " vs no doxy-PEP; pc1 = first principal component of the group's standardized parameters). ",
                  "between_chain_share_x = R^2 of x on MCMC chain; chain_coverage_min_x = smallest share of the pooled 2.5-97.5% range of x covered by one chain; ",
                  "median/max_single_param_share = between-chain share of the group's single parameters (raw values); between_chain_share_y = same for the outcome; ",
                  "fitted_var_share_pct = var(fitted x) / var(outcome) x 100."),
    how  = paste0("Between-chain share near 0 = the chains overlap along x (mixed at that level); near 1 = each chain sits in its own part of the axis. ",
                  "If between_chain_share_x is clearly lower than median_single_param_share, the group mixes better than its parameters do. ",
                  "chain_coverage_min_x near 1 = every chain spans the full range of x."))


## FIGURES ----
group_order <- c(names(PARAM_GROUPS), "Unassigned")
group_order <- group_order[group_order %in% unique(summary_df$group)]

x_labels <- c(fitted = "Group's combined effect: fitted % reduction, E[outcome | group's parameters]",
              pc1    = "First principal component of the group's parameters (standardized; sign arbitrary)")
y_label  <- paste0("% reduction in total incidence, ", EVAL.YEAR, " (", INTERVENTION, " vs no doxy-PEP)")

for (type in X.TYPES) {
    f.pdf <- file.path(OUT.DIR, paste0("gam_group_mixing_", type, "_", CALIB.CODE, "_", STAMP, ".pdf"))
    pdf(f.pdf, width = 16, height = 7)
    for (g in group_order) {
        s     <- filter(summary_df, x_type == type, group == g)
        strip <- setNames(paste0(s$city, "\nbetween-chain share of x: ", round(100 * s$between_chain_share_x), "%",
                                 " (single params: ", round(100 * s$median_single_param_share), "%)"),
                          s$city)
        d <- filter(points_df, x_type == type, group == g) %>%
            mutate(panel = factor(strip[city], levels = strip[intersect(names(LOCS), s$city)]))

        p <- ggplot(d, aes(x = x, y = y, colour = chain)) +
            geom_point(size = 0.7, alpha = 0.5) +
            facet_wrap(~ panel, nrow = 2, scales = "free") +
            scale_colour_manual(values = CHAIN.COLORS, name = "MCMC chain") +
            labs(x = x_labels[[type]], y = y_label,
                 title = paste0(g, " (", s$n_params[1], " parameter", if (s$n_params[1] > 1) "s", ")"),
                 subtitle = "One point per posterior simulation. Mixed at the group level = the four colours spread over the whole x axis") +
            guides(colour = guide_legend(override.aes = list(size = 2.5, alpha = 1))) +
            theme_bw() +
            theme(strip.text = element_text(size = 8), legend.position = "bottom")
        print(p)
    }
    dev.off()
    cat("Saved", f.pdf, "\n")
}


## CONSOLE SUMMARY ----
# Median across cities: does the group mix better than its single parameters?
cat("\nBetween-chain share (median across cities): group summary x vs. its single parameters\n")
summary_df %>%
    group_by(group, x_type) %>%
    summarize(group_x = median(between_chain_share_x),
              single_params = median(median_single_param_share),
              .groups = "drop") %>%
    pivot_wider(names_from = x_type, values_from = group_x, names_prefix = "group_") %>%
    arrange(match(group, group_order)) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    print(n = Inf)

cat("Saved", f.csv, "\n")
