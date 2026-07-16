# Compute total.syphilis.diagnoses as the sum of PS + Early + Unknown syphilis
# diagnoses, for every stratification where all three components are present.
#
# Runs for one or more configurations (see TOTAL_CONFIGS below). Each config
# reads the three component outcomes from a given source/ontology and writes the
# summed total to a target source/ontology. Only stratifications present on all
# three components are summed; the rest are skipped (e.g. the surveillance
# reports carry Early and Unknown only at year__location, so only that level is
# summable there).
#
# Component arrays are aligned to the union of their dimension values, missing
# cells filled with NA, and summed element-wise. Wherever any component is NA at
# a given cell, the total is NA — a cell only gets a total if all three stages
# are present. Alignment is by dimension-name, so differing value order between
# components does not affect the result.
#
# Replaces the earlier test_total_with_restratification.R, which bailed out on
# stratifications where the three components had differently-sized location
# dimensions. Restratification is no longer needed: within a given source the
# three outcomes share an ontology with identical age groups.
library(jheem2)

# --- Configuration ---------------------------------------------------------
# read_source/read_ontology:   where the PS/Early/Unknown components live
# write_source/write_ontology: where the summed total is written (Zoe's mapping)
#
# Each config sums PS + Early + Unknown at the read source/ontology and writes
# the total to the write source/ontology. Every non-AtlasPlus config writes the
# total back to the same source/ontology as its components, per Zoe's spec:
#   cdc.sti.surveillance.reports / cdc.pdf.report
#   lhd / state.health.dept
#   lhd / california.health.dept
# These computed totals (PS + Early + Unknown) are the single definition of
# "total" for each source, matching the AtlasPlus cdc.summed total. Where a
# source also carries a directly-reported total (e.g. the surveillance reports'
# all-stages figure), that reported figure should be dropped in its own
# processing script so the computed total is authoritative.
#
# A config whose read source/ontology carries no PS data in the manager at hand
# is skipped with a warning rather than treated as an error, so a build whose
# section files predate a given source still succeeds.
TOTAL_CONFIGS <- list(
  list(
    read_source    = 'cdc.sti',
    read_ontology  = 'cdc.sti',
    write_source   = 'cdc.summed',
    write_ontology = 'cdc.sti',
    url     = "https://gis.cdc.gov/grasp/nchhstpatlas/main.html",
    details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from CDC AtlasPlus"
  ),
  list(
    read_source    = 'cdc.sti.surveillance.reports',
    read_ontology  = 'cdc.pdf.report',
    write_source   = 'cdc.sti.surveillance.reports',
    write_ontology = 'cdc.pdf.report',
    url     = "https://www.cdc.gov/sti-statistics/index.html",
    details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from CDC STI Surveillance Reports"
  ),
  list(
    read_source    = 'lhd',
    read_ontology  = 'state.health.dept',
    write_source   = 'lhd',
    write_ontology = 'state.health.dept',
    url     = "https://www.cdc.gov/sti-statistics/index.html",
    details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from state health department reports"
  ),
  list(
    read_source    = 'lhd',
    read_ontology  = 'california.health.dept',
    write_source   = 'lhd',
    write_ontology = 'california.health.dept',
    url     = "https://www.cdph.ca.gov/",
    details = "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses from California health department reports"
  )
)

COMPONENT_OUTCOMES <- c('ps.syphilis.diagnoses',
                        'early.syphilis.diagnoses',
                        'unknown.duration.or.late.syphilis.diagnoses')

# --- Helpers ---------------------------------------------------------------

#' Align a list of numeric arrays to a common set of dimensions.
#'
#' For each dimension, the target dimnames are the sorted union of values
#' across all input arrays. Each array is then reshaped to the full target
#' shape with NA_real_ in cells the input didn't cover. Values are placed by
#' dimension-name, so input order does not matter.
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

#' Compute and write total.syphilis.diagnoses for a single configuration.
#' Returns the number of stratifications written, or NULL if the config's read
#' source/ontology carries no PS data in this manager (config not applicable).
compute_total_for_config <- function(manager, cfg) {
  ps_est <- manager$data$ps.syphilis.diagnoses$estimate[[cfg$read_source]][[cfg$read_ontology]]
  if (is.null(ps_est)) {
    return(NULL)
  }

  n_written <- 0
  for (strat in names(ps_est)) {
    components <- lapply(COMPONENT_OUTCOMES, function(outcome) {
      manager$data[[outcome]]$estimate[[cfg$read_source]][[cfg$read_ontology]][[strat]]
    })

    if (any(vapply(components, is.null, logical(1)))) {
      cat(sprintf("  %s: SKIP (component not present at this source/ontology)\n", strat))
      next
    }

    aligned <- align_arrays_to_union(components)
    # Missingness propagates through +: a cell gets a total only where all three
    # components are present. Missing cells stay missing (the manager stores
    # missing as NaN, so NA and NaN are equivalent here).
    total <- Reduce(`+`, aligned)

    # No tryCatch: a put() failure here is a real problem, not something to
    # swallow and continue past — let it surface at the point of the cause.
    manager$put(
      data = total,
      outcome = 'total.syphilis.diagnoses',
      source = cfg$write_source,
      ontology.name = cfg$write_ontology,
      url = cfg$url,
      details = cfg$details
    )

    non_na <- sum(!is.na(total))
    cat(sprintf("  %s: OK (shape %s, %d/%d non-NA cells, %.1f%%)\n",
                strat,
                paste(dim(total), collapse = "x"),
                non_na, length(total),
                100 * non_na / length(total)))
    n_written <- n_written + 1
  }

  n_written
}

# --- Main ------------------------------------------------------------------

if (!('total.syphilis.diagnoses' %in% names(syphilis.manager$outcome.info))) {
  stop("total.syphilis.diagnoses outcome is not registered")
}

for (cfg in TOTAL_CONFIGS) {
  cat(sprintf("=== total.syphilis.diagnoses: %s/%s -> %s/%s ===\n",
              cfg$read_source, cfg$read_ontology, cfg$write_source, cfg$write_ontology))

  n_written <- compute_total_for_config(syphilis.manager, cfg)

  if (is.null(n_written)) {
    cat(sprintf("  SKIP: no PS data at %s / %s — section files likely predate this source; skipping config.\n\n",
                cfg$read_source, cfg$read_ontology))
  } else if (n_written == 0) {
    stop(sprintf("No totals produced for %s/%s -> %s/%s — PS data present but nothing summed",
                 cfg$read_source, cfg$read_ontology, cfg$write_source, cfg$write_ontology))
  } else {
    cat(sprintf("  %d stratification(s) written.\n\n", n_written))
  }
}
