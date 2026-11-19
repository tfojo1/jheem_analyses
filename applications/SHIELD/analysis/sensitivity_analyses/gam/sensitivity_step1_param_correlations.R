# GAM sensitivity analysis step 1: parameter correlations in the posterior sims
# Pipeline: step1 parameter correlations -> step2 variance shares -> step3 heatmap
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
# Run from the jheem_analyses folder. Settings, groups and TEST.MSA are in 00_gam_settings.R.
# Parameters are read from extracts/params/ (saved by intervention/01a); no simsets are loaded.

cat("\n==== GAM step 1/3: parameter correlations (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/00_gam_settings.R')

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
csv.file <- paste0(OUT.PATH, "param_correlations.csv")
writeLines(paste0('"', c(
    paste0("# Spearman correlations between calibrated parameters across the ", CALIB.CODE,
           " posterior sims, per MSA. Only pairs with |r| >= ", R.FLAG, " in at least one MSA are listed."),
    paste0("# n_msas_flagged = MSAs with |r| >= ", R.FLAG, "; median/min/max_r across all MSAs; group.1/group.2 = GAM groups ",
           "(00_gam_settings.R); cross_group = TRUE means the pair is split across groups and the groups may need merging.")
), '"'), csv.file)
suppressWarnings(write.table(flagged, csv.file, sep = ",", row.names = FALSE, append = TRUE))

# GROUP CHECK ----
# A pair of groups should be merged if a parameter in one and a parameter in the other have
# |r| >= GROUP.CHECK.R in at least GROUP.CHECK.N.MSAS MSAs (or in every MSA analysed, if fewer).
# Linked groups are combined into merge sets (A-B and B-C -> A+B+C).
# Status: PASS = no such links; FAIL = links found (full run: step 2 will not run);
#         WARN = links found in a one-MSA test run (reported, step 2 not blocked).
n_needed <- min(GROUP.CHECK.N.MSAS, length(locations))

strong_links <- pairs %>%
    mutate(group.1 = group_of[param.1], group.2 = group_of[param.2]) %>%
    filter(group.1 != group.2) %>%
    group_by(param.1, param.2, group.1, group.2) %>%
    summarize(n_msas_strong = sum(abs(r) >= GROUP.CHECK.R),
              median_r      = round(median(r), 2),
              max_abs_r     = round(max(abs(r)), 2),
              msas_strong   = paste(msa_names[msa[abs(r) >= GROUP.CHECK.R]], collapse = "; "),
              .groups = "drop") %>%
    filter(n_msas_strong >= n_needed) %>%
    arrange(desc(n_msas_strong), desc(max_abs_r))

# Merge sets: groups connected through strong links
merge_sets <- list()
if (nrow(strong_links) > 0) {
    linked <- distinct(strong_links, group.1, group.2)
    member <- unique(c(linked$group.1, linked$group.2))
    set_id <- setNames(seq_along(member), member)
    repeat {
        changed <- FALSE
        for (i in seq_len(nrow(linked))) {
            a <- set_id[[linked$group.1[i]]]; b <- set_id[[linked$group.2[i]]]
            if (a != b) { set_id[set_id == max(a, b)] <- min(a, b); changed <- TRUE }
        }
        if (!changed) break
    }
    merge_sets <- unname(split(names(set_id), set_id))
}

check_status <- if (nrow(strong_links) == 0) "PASS" else if (is.null(TEST.MSA)) "FAIL" else "WARN"
group_check  <- list(status     = check_status,
                     signature  = GROUPS.SIGNATURE,
                     msas       = unname(locations),
                     rule       = paste0("|r| >= ", GROUP.CHECK.R, " in >= ", n_needed, " MSA(s)"),
                     links      = strong_links,
                     merge_sets = merge_sets,
                     date       = Sys.time())
save(group_check, file = paste0(OUT.PATH, "group_check.Rdata"))

links.file <- paste0(OUT.PATH, "group_check_links.csv")
writeLines(paste0('"', c(
    paste0("# Group check: parameter pairs in DIFFERENT GAM groups with |Spearman r| >= ", GROUP.CHECK.R,
           " in >= ", n_needed, " MSA(s). Status: ", check_status, ". Empty table = the groups pass."),
    "# n_msas_strong = MSAs where |r| >= the threshold; median_r across MSAs (sign: - = trade-off, + = move together); max_abs_r = largest |r|; each row means group.1 and group.2 should be merged (or the parameters moved to the same group)."
), '"'), links.file)
suppressWarnings(write.table(strong_links, links.file, sep = ",", row.names = FALSE, append = TRUE))

# PRINT ----
cat("\nFlagged pairs (|r| >=", R.FLAG, "in >= 1 MSA):", nrow(flagged),
    "| split across groups:", sum(flagged$cross_group), "\n\n")
cat("Group pairs linked by correlated parameters (candidates for merging):\n")
print(group_links, n = Inf)
cat("\nCross-group parameter pairs:\n")
print(filter(flagged, cross_group) %>% select(param.1, param.2, group.1, group.2, n_msas_flagged, median_r),
      n = Inf)
cat("\nSaved:", csv.file, "\n")

# Group check verdict (printed last, so it is the first thing you see)
cat("\n", strrep("=", 78), "\n", sep = "")
cat("GROUP CHECK: ", check_status, "   (rule: cross-group |r| >= ", GROUP.CHECK.R, " in >= ", n_needed,
    " of ", length(locations), " MSA(s))\n", sep = "")
cat(strrep("=", 78), "\n", sep = "")
if (check_status == "PASS") {
    cat("No strongly correlated parameters are split across groups. The groups are ready for step 2.\n")
} else {
    cat("These groups are linked by strongly correlated parameters and should be MERGED\n",
        "(or the parameters moved into the same group):\n", sep = "")
    for (i in seq_along(merge_sets)) cat("  ", i, ". ", paste(merge_sets[[i]], collapse = "  +  "), "\n", sep = "")
    cat("\nParameter pairs behind these links:\n")
    print(select(strong_links, param.1, group.1, param.2, group.2, n_msas_strong, median_r), n = Inf)
    if (check_status == "FAIL") {
        cat("\nSTEP 2 WILL NOT RUN until this is fixed: edit PARAM_GROUPS in 00_gam_settings.R, then rerun step 1.\n",
            "If you have reviewed these links and want to keep the groups, set GROUP.CHECK.OVERRIDE <- TRUE.\n", sep = "")
    } else {
        cat("\nTest run (one MSA): reported only, step 2 is not blocked. Run all MSAs to confirm.\n")
    }
}
cat(strrep("=", 78), "\n", sep = "")
cat("Saved:", links.file, "\n")
