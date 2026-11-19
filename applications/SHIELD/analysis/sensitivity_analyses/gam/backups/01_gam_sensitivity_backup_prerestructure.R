# GAM-based sensitivity analysis (first-order variance shares)
#
# Question: how much of the posterior uncertainty in the doxy-PEP effect is
# explained by each group of calibrated parameters?
#
# Method (Strong et al. 2014, PMID 24246566; Jackson et al. 2021, PMID 35127249):
#   For outcome Y and parameter group X, regress Y on X across the posterior
#   simulations. Var(E[Y | X]) / Var(Y) = share of the outcome's variance explained
#   by X ("if we knew X exactly, this % of the uncertainty would go away").
#   Computed with voi::evppivar(); GAM for groups of <= 2 parameters, MARS
#   (earth) for larger groups. No new simulations are needed.
#
# Outcomes (2030, one MSA at a time, sims paired within MSA):
#   % reduction in incidence vs. no intervention: total population, MSM, women
#
# Overfitting check: each fit is repeated with Y randomly shuffled. The share
# obtained on shuffled Y is what the method finds by chance ("null share").
#
# Run from the jheem_analyses folder. Settings, parameter groups and TEST.MSA
# are in gam_settings.R.

cat("\n==== GAM step 1/2: variance shares (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/gam_settings.R')

# SETTINGS (this script) ----
# Also compute the share for each parameter on its own (single-parameter GAM).
# Shows which individual parameters matter, e.g. whether one bridging parameter
# is the top single parameter even when its group ranks lower. No chance-level
# fit is run for these; for a single-parameter GAM it is about 1%.
RUN.SINGLE.PARAMS <- TRUE

param_matrices <- lapply(locations, get_param_matrix)

total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results   <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# OUTCOMES ----
# % reduction in EVAL.YEAR incidence vs. no intervention (positive = fewer cases), and
# "Women minus MSM" = women's % reduction - MSM's % reduction (percentage points;
# negative = women benefit less than MSM). It asks what determines how much women
# benefit RELATIVE to MSM, without the instability of a ratio.
get_outcomes <- function(city) {
    total <- -1 * total_calc_results[EVAL.YEAR, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    msm   <- -1 * sex_calc_results[EVAL.YEAR, "msm", , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    women <- -1 * sex_calc_results[EVAL.YEAR, "female", , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    list("Total" = total, "MSM" = msm, "Women" = women, "Women minus MSM" = women - msm)
}

# VARIANCE SHARE ----
# Share (%) of Var(y) explained by the parameters in `pars`
variance_share <- function(y, inputs, pars) {
    method <- if (length(pars) <= MAX.GAM.PARS) "gam" else "earth"
    ev <- evppivar(outputs = y, inputs = inputs[, pars, drop = FALSE],
                   pars = pars, method = method, verbose = FALSE)
    100 * ev$evppi / var(y)
}

# RUN ----
set.seed(SEED)
results <- list()
single  <- list()

for (city in locations) {
    cat("MSA:", city, "at", format(Sys.time()), "\n")

    inputs <- as.data.frame(param_matrices[[city]])
    inputs <- inputs[, apply(inputs, 2, var) > 0, drop = FALSE]   # drop fixed parameters
    groups <- c(assign_groups(colnames(inputs)),
                list("All parameters" = colnames(inputs)))         # whole-model reference

    group_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))[colnames(inputs)]

    outcomes <- get_outcomes(city)
    stopifnot(all(lengths(outcomes) == nrow(inputs)))              # sims must line up with parameters

    cat("  Correlation between outcomes across sims:\n")
    print(round(cor(do.call(cbind, outcomes), use = "pairwise.complete.obs"), 2))

    for (outcome in names(outcomes)) {
        y  <- outcomes[[outcome]]
        ok <- !is.na(y)
        if (sum(!ok) > 0) cat("  ", outcome, ": dropped", sum(!ok), "sims with NA outcome\n")
        y_shuffled <- sample(y[ok])

        for (g in names(groups)) {
            results[[length(results) + 1]] <- tibble(
                msa            = city,
                outcome        = outcome,
                group          = g,
                n_params       = length(groups[[g]]),
                method         = if (length(groups[[g]]) <= MAX.GAM.PARS) "gam" else "earth",
                n_sims         = sum(ok),
                mean_reduction = mean(y[ok]),
                sd_reduction   = sd(y[ok]),
                share_pct      = variance_share(y[ok], inputs[ok, , drop = FALSE], groups[[g]]),
                null_share_pct = variance_share(y_shuffled, inputs[ok, , drop = FALSE], groups[[g]])
            )
        }

        if (RUN.SINGLE.PARAMS) {
            single_city <- tibble(
                msa       = city,
                outcome   = outcome,
                parameter = colnames(inputs),
                group     = unname(group_of),
                share_pct = sapply(colnames(inputs), function(p)
                    variance_share(y[ok], inputs[ok, , drop = FALSE], p))
            ) %>% arrange(desc(share_pct))
            cat("  ", outcome, "- top 5 single parameters:\n")
            print(head(select(single_city, parameter, group, share_pct), 5))
            single[[length(single) + 1]] <- single_city
        }
    }
}

results <- bind_rows(results) %>%
    mutate(share_above_null_pct = pmax(share_pct - null_share_pct, 0),
           across(where(is.numeric), ~ round(.x, 2)))

# SAVE ----
save(results, file = paste0(OUT.PATH, "gam_variance_shares", FILE.SUFFIX, ".Rdata"))

csv.file <- paste0(OUT.PATH, "gam_variance_shares", FILE.SUFFIX, ".csv")
writeLines(paste0('"', c(
    paste0("# First-order variance shares of the ", EVAL.YEAR, " % reduction in incidence vs. no intervention (",
           INTERVENTION, ", ", CALIB.CODE, "), by MSA, outcome (Total/MSM/Women; Women minus MSM = difference in % reduction, percentage points) and parameter group."),
    paste0("# share_pct = % of the outcome's posterior variance explained by the group (Var(E[Y|group])/Var(Y)); ",
           "null_share_pct = same fit on shuffled Y (chance level); share_above_null_pct = share_pct - null_share_pct (floored at 0); ",
           "'All parameters' = all groups together (a lower bound: the regression underfits 173 parameters). Shares overlap and do not sum to 100%. Sims are paired within MSA only.")
), '"'), csv.file)
suppressWarnings(write.table(results, csv.file, sep = ",", row.names = FALSE, append = TRUE))

if (RUN.SINGLE.PARAMS) {
    single <- bind_rows(single) %>% mutate(share_pct = round(share_pct, 2))
    save(single, file = paste0(OUT.PATH, "gam_single_param_shares", FILE.SUFFIX, ".Rdata"))

    single.file <- paste0(OUT.PATH, "gam_single_param_shares", FILE.SUFFIX, ".csv")
    writeLines(paste0('"', c(
        paste0("# Single-parameter variance shares (GAM, one parameter at a time) of the ", EVAL.YEAR,
               " % reduction in incidence vs. no intervention (", INTERVENTION, ", ", CALIB.CODE, "), by MSA and outcome."),
        "# share_pct = % of the outcome's posterior variance explained by that parameter alone (chance level about 1%); group = its GAM group. Sorted by share within MSA and outcome."
    ), '"'), single.file)
    suppressWarnings(write.table(single, single.file, sep = ",", row.names = FALSE, append = TRUE))
    cat("Single-parameter results in", single.file, "\n")
}

cat("Done. Results in", csv.file, "\n")
