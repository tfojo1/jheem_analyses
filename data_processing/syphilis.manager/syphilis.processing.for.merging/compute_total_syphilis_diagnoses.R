# Compute total.syphilis.diagnoses as the sum of PS + Early + Unknown syphilis
# diagnoses, for every stratification available on PS.
#
# Replaces test_total_with_restratification.R, which bailed out on stratifications
# where the three component outcomes had differently-sized location dimensions
# (notably year__location__age and its sub-stratifications, due to county-level
# privacy suppression leaving different counties present for each outcome).
#
# This version aligns the three component arrays to the union of their dimnames,
# fills missing cells with NA, and sums element-wise. Wherever any component is
# NA at a given cell, the total is NA. Where all three components are present,
# the total is their sum.
#
# The restratification branch in the previous version is no longer needed: all
# three outcomes now share the cdc.sti ontology with identical age groups.
library(jheem2)

SOURCE_NAME <- 'cdc.sti'
TARGET_ONTOLOGY <- 'cdc.sti'
PUT_SOURCE <- 'cdc.summed'
URL <- "https://gis.cdc.gov/grasp/nchhstpatlas/main.html"
DETAILS <- "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from CDC AtlasPlus"

# --- Helpers ---------------------------------------------------------------

#' Align a list of numeric arrays to a common set of dimensions.
#'
#' For each dimension, the target dimnames are the sorted union of values
#' across all input arrays. Each array is then reshaped to the full target
#' shape with NA_real_ in cells the input didn't cover.
align_arrays_to_union <- function(arrays) {
  dim_names_list <- lapply(arrays, dimnames)
  all_dim_names <- names(dim_names_list[[1]])

  # All arrays must have the same dimensions (same set of axes)
  for (dn in dim_names_list) {
    if (!identical(names(dn), all_dim_names)) {
      stop("align_arrays_to_union: arrays must share the same dimension names (got: ",
           paste(sapply(dim_names_list, function(x) paste(names(x), collapse = ",")), collapse = " | "), ")")
    }
  }

  union_dimnames <- lapply(all_dim_names, function(d) {
    sort(unique(unlist(lapply(dim_names_list, function(dn) dn[[d]]))))
  })
  names(union_dimnames) <- all_dim_names

  target_dims <- sapply(union_dimnames, length)

  lapply(arrays, function(arr) {
    out <- array(NA_real_, dim = target_dims, dimnames = union_dimnames)
    # Index by this array's own dimnames to place values in the right cells.
    idx_args <- lapply(all_dim_names, function(d) dimnames(arr)[[d]])
    do.call(`[<-`, c(list(out), idx_args, list(value = arr)))
  })
}

# --- Main ------------------------------------------------------------------

cat("=== COMPUTING total.syphilis.diagnoses (PS + Early + Unknown) ===\n")
cat(sprintf("Source: %s, Ontology: %s, Put as source: %s\n",
            SOURCE_NAME, TARGET_ONTOLOGY, PUT_SOURCE))

if (!('total.syphilis.diagnoses' %in% names(syphilis.manager$outcome.info))) {
  stop("total.syphilis.diagnoses outcome is not registered")
}

ps_strats <- names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[SOURCE_NAME]][[TARGET_ONTOLOGY]])
cat(sprintf("Stratifications to process: %d\n\n", length(ps_strats)))

n_success <- 0
n_skipped <- 0
for (strat in ps_strats) {
  ps    <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[SOURCE_NAME]][[TARGET_ONTOLOGY]][[strat]]
  early <- syphilis.manager$data$early.syphilis.diagnoses$estimate[[SOURCE_NAME]][[TARGET_ONTOLOGY]][[strat]]
  unk   <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate[[SOURCE_NAME]][[TARGET_ONTOLOGY]][[strat]]

  if (is.null(ps) || is.null(early) || is.null(unk)) {
    cat(sprintf("  %s: SKIP (missing component)\n", strat))
    n_skipped <- n_skipped + 1
    next
  }

  aligned <- align_arrays_to_union(list(ps, early, unk))
  total <- aligned[[1]] + aligned[[2]] + aligned[[3]]  # NA propagates naturally

  tryCatch({
    syphilis.manager$put(
      data = total,
      outcome = 'total.syphilis.diagnoses',
      source = PUT_SOURCE,
      ontology.name = TARGET_ONTOLOGY,
      url = URL,
      details = DETAILS
    )
    non_na <- sum(!is.na(total))
    cat(sprintf("  %s: OK (shape %s, %d/%d non-NA cells, %.1f%%)\n",
                strat,
                paste(dim(total), collapse = "x"),
                non_na, length(total),
                100 * non_na / length(total)))
    n_success <- n_success + 1
  }, error = function(e) {
    cat(sprintf("  %s: FAILED (%s)\n", strat, e$message))
  })
}

cat(sprintf("\nDone. %d stratifications processed, %d skipped.\n", n_success, n_skipped))
