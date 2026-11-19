# GAM-based sensitivity analysis, step 0: parameter correlations in the posterior sims
#
# Purpose: check the parameter groups before running the GAM. Strongly correlated
# parameters should be in the SAME group; if they are split across groups, the two
# groups' variance shares double-count.
#
# Uses exactly the sims the GAM uses (the posterior sims for each MSA, pooled
# across chains, raw parameter scale). Spearman correlation, so it is not affected
# by skewed parameters or monotone transformations (log/logit).
#
# Output: one row per parameter pair with |r| >= R.FLAG in at least one MSA,
# cross-group pairs first.
#
# Run from the jheem_analyses folder. Settings, groups and TEST.MSA are in gam_settings.R.
# Parameters are read from extracts/params/ (saved by intervention/01a); no simsets are loaded.

cat("\n==== GAM step 0/2: parameter correlations (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/gam_settings.R')

# SETTINGS ----
R.FLAG <- 0.5    # flag pairs with |Spearman r| >= this in at least one MSA

# CORRELATIONS PER MSA ----
param_matrices <- lapply(locations, get_param_matrix)

pairs <- lapply(names(param_matrices), function(city) {
    x <- param_matrices[[city]]
    x <- x[, apply(x, 2, var) > 0, drop = FALSE]          # drop fixed parameters
    r <- cor(x, method = "spearman")
    r[lower.tri(r, diag = TRUE)] <- NA                     # keep each pair once
    as.data.frame(as.table(r)) %>%
        filter(!is.na(Freq)) %>%
        transmute(param.1 = as.character(Var1), param.2 = as.character(Var2),
                  msa = city, r = Freq)
}) %>% bind_rows()

# GROUP OF EACH PARAMETER ----
groups <- assign_groups(unique(c(pairs$param.1, pairs$param.2)))
group_of <- setNames(rep(names(groups), lengths(groups)), unlist(groups))

# SUMMARIZE ACROSS MSAs ----
msa_names <- setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)

flagged <- pairs %>%
    group_by(param.1, param.2) %>%
    filter(any(abs(r) >= R.FLAG)) %>%
    summarize(n_msas_flagged = sum(abs(r) >= R.FLAG),
              n_msas         = n(),
              median_r       = median(r),
              min_r          = min(r),
              max_r          = max(r),
              msas_flagged   = paste(msa_names[msa[abs(r) >= R.FLAG]], collapse = "; "),
              .groups = "drop") %>%
    mutate(group.1     = group_of[param.1],
           group.2     = group_of[param.2],
           cross_group = group.1 != group.2,
           across(c(median_r, min_r, max_r), ~ round(.x, 2))) %>%
    arrange(desc(cross_group), desc(n_msas_flagged), desc(abs(median_r)))

# Group pairs that are linked by at least one flagged parameter pair
group_links <- flagged %>%
    filter(cross_group) %>%
    group_by(group.1, group.2) %>%
    summarize(n_param_pairs = n(),
              max_n_msas    = max(n_msas_flagged),
              strongest_r   = median_r[which.max(abs(median_r))],
              .groups = "drop") %>%
    arrange(desc(max_n_msas), desc(abs(strongest_r)))

# SAVE ----
csv.file <- paste0(OUT.PATH, "param_correlations", FILE.SUFFIX, ".csv")
writeLines(paste0('"', c(
    paste0("# Spearman correlations between calibrated parameters across the ", CALIB.CODE,
           " posterior sims, per MSA. Only pairs with |r| >= ", R.FLAG, " in at least one MSA are listed."),
    paste0("# n_msas_flagged = MSAs with |r| >= ", R.FLAG, "; median/min/max_r across all MSAs; group.1/group.2 = GAM groups ",
           "(gam_settings.R); cross_group = TRUE means the pair is split across groups and the groups may need merging.")
), '"'), csv.file)
suppressWarnings(write.table(flagged, csv.file, sep = ",", row.names = FALSE, append = TRUE))

# PRINT ----
cat("\nFlagged pairs (|r| >=", R.FLAG, "in >= 1 MSA):", nrow(flagged),
    "| split across groups:", sum(flagged$cross_group), "\n\n")
cat("Group pairs linked by correlated parameters (candidates for merging):\n")
print(group_links, n = Inf)
cat("\nCross-group parameter pairs:\n")
print(filter(flagged, cross_group) %>% select(param.1, param.2, group.1, group.2, n_msas_flagged, median_r),
      n = Inf)
cat("\nSaved:", csv.file, "\n")
