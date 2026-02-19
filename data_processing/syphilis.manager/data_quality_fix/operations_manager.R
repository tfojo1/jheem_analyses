# Operations Management System
# Clean, enterprise-grade file organization and operation tracking

library(jheem2)

#' Create operation directory structure
#' @param operation_name Short, descriptive operation name
#' @param description Human-readable description
create_operation_directory <- function(operation_name, description = NULL) {
  
  # Create timestamp-based operation ID
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  operation_id <- sprintf("%s_%s", timestamp, operation_name)
  
  # Create base operations directory
  base_dir <- "data_processing/syphilis.manager/data_quality_fix/operations"
  if (!dir.exists(base_dir)) {
    dir.create(base_dir, recursive = TRUE)
  }
  
  # Create operation-specific directory
  operation_dir <- file.path(base_dir, operation_id)
  dir.create(operation_dir, recursive = TRUE)
  
  # Standard file paths
  paths <- list(
    base = operation_dir,
    summary = file.path(operation_dir, "summary.txt"),
    removed_data = file.path(operation_dir, "removed_data.csv"),
    metadata = file.path(operation_dir, "metadata.json"),
    backup = file.path(operation_dir, "backup.rdata"),
    operation_log = file.path(operation_dir, "operation_log.txt")
  )
  
  # Create operation manifest
  manifest <- list(
    operation_id = operation_id,
    operation_name = operation_name,
    description = description,
    created = Sys.time(),
    status = "initialized",
    files = list(
      summary = "summary.txt",
      removed_data = "removed_data.csv", 
      metadata = "metadata.json",
      backup = "backup.rdata",
      operation_log = "operation_log.txt"
    )
  )
  
  # Save manifest
  jsonlite::write_json(manifest, file.path(operation_dir, "manifest.json"), 
                      pretty = TRUE, auto_unbox = TRUE)
  
  cat(sprintf("Created operation directory: %s\n", operation_dir))
  
  return(list(
    operation_id = operation_id,
    paths = paths,
    manifest = manifest
  ))
}

#' Enhanced backup with clean organization
create_operation_backup <- function(manager, operation_name, combinations, 
                                  script_name, criteria_description) {
  
  # Create clean operation directory
  operation <- create_operation_directory(operation_name, criteria_description)
  
  # Gather system metadata (fixed newlines)
  git_commit <- tryCatch({
    system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE)
  }, error = function(e) NA)
  
  # Create comprehensive metadata
  metadata <- list(
    operation_id = operation$operation_id,
    operation_name = operation_name,
    script_name = script_name,
    criteria_description = criteria_description,
    timestamp = Sys.time(),
    user = Sys.getenv("USER"),
    r_version = R.version.string,
    git_commit = git_commit,
    total_combinations = length(combinations)
  )
  
  # Create backup structure
  backup <- list(
    metadata = metadata,
    original_data = list()
  )
  
  # Store original data for each combination
  for (i in seq_along(combinations)) {
    combo <- combinations[[i]]
    key <- sprintf("%s_%s_%s_%s", combo$outcome, combo$msa, combo$year, combo$stratification)
    
    tryCatch({
      original_race_data <- manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location__race[combo$year, combo$msa, ]
      original_total_data <- manager$data[[combo$outcome]]$estimate$cdc.aggregated.county[[combo$ontology]]$year__location[combo$year, combo$msa]
      
      backup$original_data[[key]] <- list(
        combination = combo,
        race_data = original_race_data,
        total_data = original_total_data,
        backup_key = key
      )
      
    }, error = function(e) {
      cat(sprintf("Warning: Could not backup %s - %s\n", key, e$message))
    })
  }
  
  # Generate impact summary
  summary <- generate_impact_summary(combinations, backup$original_data, metadata)
  
  # Save all files to operation directory
  save(backup, summary, file = operation$paths$backup)
  write_summary_file(summary, operation$paths$summary)
  write_removed_data_csv(backup$original_data, operation$paths$removed_data, operation$operation_id)
  jsonlite::write_json(summary, operation$paths$metadata, pretty = TRUE, auto_unbox = TRUE)
  
  # Print summary (fixed newlines)
  cat("\n")
  print_summary(summary)
  cat(sprintf("\nOperation files created in: %s\n", operation$paths$base))
  
  return(operation$paths$backup)
}

#' Generate impact summary (fixed logic)
generate_impact_summary <- function(combinations, backup_data, metadata) {
  
  total_combinations <- length(combinations)
  total_data_points_removed <- 0
  total_cases_affected <- 0
  
  by_msa_summary <- list()
  by_outcome_summary <- list()
  
  for (key in names(backup_data)) {
    item <- backup_data[[key]]
    if (!is.null(item$race_data)) {
      # Count non-NA race data points
      non_na_race_points <- sum(!is.na(item$race_data))
      total_data_points_removed <- total_data_points_removed + non_na_race_points
      
      # Add to total cases
      if (!is.na(item$total_data)) {
        total_cases_affected <- total_cases_affected + item$total_data
      }
      
      # By MSA
      msa_name <- item$combination$msa_name
      if (!(msa_name %in% names(by_msa_summary))) {
        by_msa_summary[[msa_name]] <- list(combinations = 0, data_points = 0, cases = 0)
      }
      by_msa_summary[[msa_name]]$combinations <- by_msa_summary[[msa_name]]$combinations + 1
      by_msa_summary[[msa_name]]$data_points <- by_msa_summary[[msa_name]]$data_points + non_na_race_points
      by_msa_summary[[msa_name]]$cases <- by_msa_summary[[msa_name]]$cases + ifelse(is.na(item$total_data), 0, item$total_data)
      
      # By outcome
      outcome <- item$combination$outcome
      if (!(outcome %in% names(by_outcome_summary))) {
        by_outcome_summary[[outcome]] <- list(combinations = 0, data_points = 0, cases = 0)
      }
      by_outcome_summary[[outcome]]$combinations <- by_outcome_summary[[outcome]]$combinations + 1
      by_outcome_summary[[outcome]]$data_points <- by_outcome_summary[[outcome]]$data_points + non_na_race_points
      by_outcome_summary[[outcome]]$cases <- by_outcome_summary[[outcome]]$cases + ifelse(is.na(item$total_data), 0, item$total_data)
    }
  }
  
  summary <- list(
    metadata = metadata,
    impact = list(
      total_combinations = total_combinations,
      total_data_points_removed = total_data_points_removed,
      total_cases_affected = total_cases_affected
    ),
    by_msa = by_msa_summary,
    by_outcome = by_outcome_summary,
    sample_removals = head(backup_data, 3)
  )
  
  return(summary)
}

#' Write human-readable summary file (fixed newlines)
write_summary_file <- function(summary, file_path) {

  sink(file_path)
  on.exit(sink(), add = TRUE)

  cat(sprintf("REMOVAL SUMMARY - %s\n", summary$metadata$operation_id))
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Script: %s\n", summary$metadata$script_name))
  cat(sprintf("Criteria: %s\n", summary$metadata$criteria_description))
  cat(sprintf("Timestamp: %s\n", summary$metadata$timestamp))
  cat(sprintf("User: %s\n", summary$metadata$user))
  if (!is.na(summary$metadata$git_commit)) {
    cat(sprintf("Git Commit: %s\n", substr(summary$metadata$git_commit, 1, 8)))
  }
  cat("\n")

  cat("IMPACT:\n")
  cat(sprintf("- %d combinations affected across %d MSAs\n",
              summary$impact$total_combinations,
              length(summary$by_msa)))
  cat(sprintf("- %d individual race data points removed\n", summary$impact$total_data_points_removed))
  cat(sprintf("- %d total cases affected (preserved in totals)\n", summary$impact$total_cases_affected))
  cat("\n")

  cat("BY MSA:\n")
  for (msa_name in names(summary$by_msa)) {
    msa_info <- summary$by_msa[[msa_name]]
    cat(sprintf("- %s: %d combinations, %d data points, %d cases\n",
                msa_name, msa_info$combinations, msa_info$data_points, msa_info$cases))
  }
  cat("\n")

  cat("BY OUTCOME:\n")
  for (outcome in names(summary$by_outcome)) {
    outcome_info <- summary$by_outcome[[outcome]]
    outcome_display <- gsub("\\.", " ", gsub("^([a-z]+)\\.", "", outcome))
    cat(sprintf("- %s: %d combinations, %d data points, %d cases\n",
                outcome_display, outcome_info$combinations, outcome_info$data_points, outcome_info$cases))
  }
}

#' Print summary to console (fixed newlines)
print_summary <- function(summary) {
  cat(sprintf("REMOVAL SUMMARY - %s\n", summary$metadata$operation_id))
  cat(paste(rep("=", 60), collapse=""), "\n")
  cat(sprintf("Combinations affected: %d across %d MSAs\n", 
              summary$impact$total_combinations, length(summary$by_msa)))
  cat(sprintf("Data points removed: %d\n", summary$impact$total_data_points_removed))
  cat(sprintf("Total cases affected: %d (preserved in totals)\n", summary$impact$total_cases_affected))
}

#' Write complete removed data CSV
write_removed_data_csv <- function(backup_data, file_path, operation_id) {
  
  removed_data_df <- data.frame()
  
  for (key in names(backup_data)) {
    item <- backup_data[[key]]
    combo <- item$combination
    
    if (!is.null(item$race_data)) {
      race_names <- names(item$race_data)
      
      for (i in seq_along(item$race_data)) {
        if (!is.na(item$race_data[i])) {
          row <- data.frame(
            operation_id = operation_id,
            msa_code = combo$msa,
            msa_name = combo$msa_name,
            year = combo$year,
            outcome = combo$outcome,
            stratification = combo$stratification,
            ontology = combo$ontology,
            race_category = race_names[i],
            removed_value = item$race_data[i],
            total_for_combination = item$total_data,
            county_coverage = combo$county_coverage,
            removal_reason = sprintf("County coverage %.1f%% < 90%% threshold", combo$county_coverage * 100),
            stringsAsFactors = FALSE
          )
          removed_data_df <- rbind(removed_data_df, row)
        }
      }
    }
  }
  
  write.csv(removed_data_df, file_path, row.names = FALSE)
}

#' Restore function
restore_operation <- function(manager, operation_backup_path) {
  
  cat(sprintf("Restoring from operation backup: %s\n", operation_backup_path))
  
  # Load backup
  load(operation_backup_path)
  
  if (!exists("backup")) {
    stop("Invalid backup file - no backup object found")
  }
  
  metadata <- backup$metadata
  cat(sprintf("Operation: %s\n", metadata$operation_id))
  cat(sprintf("Script: %s\n", metadata$script_name))
  cat(sprintf("Created: %s\n", metadata$timestamp))
  cat(sprintf("Combinations: %d\n", metadata$total_combinations))
  
  # Restore all combinations
  successful_restorations <- 0
  
  for (key in names(backup$original_data)) {
    tryCatch({
      backup_item <- backup$original_data[[key]]
      combo <- backup_item$combination
      
      # Create proper array structure
      race_array <- array(backup_item$race_data, 
                         dim = length(backup_item$race_data),
                         dimnames = list(race = names(backup_item$race_data)))
      
      manager$put(
        data = race_array,
        outcome = combo$outcome,
        source = "cdc.aggregated.county",
        ontology.name = combo$ontology,
        dimension.values = list(location = combo$msa, year = combo$year),
        url = "restoration_from_backup",
        details = sprintf("Restored from operation: %s", metadata$operation_id),
        allow.na.to.overwrite = TRUE
      )
      
      successful_restorations <- successful_restorations + 1
      
    }, error = function(e) {
      cat(sprintf("Error restoring %s: %s\n", key, e$message))
    })
  }
  
  cat(sprintf("Restoration complete: %d/%d successful\n", 
              successful_restorations, length(backup$original_data)))
  
  return(successful_restorations)
}

#' List operations with clean display
list_operations <- function() {
  
  operations_dir <- "data_processing/syphilis.manager/data_quality_fix/operations"
  
  if (!dir.exists(operations_dir)) {
    cat("No operations directory found\n")
    return(invisible(NULL))
  }
  
  operation_dirs <- list.dirs(operations_dir, recursive = FALSE, full.names = FALSE)
  
  if (length(operation_dirs) == 0) {
    cat("No operations found\n")
    return(invisible(NULL))
  }
  
  cat("Operations:\n")
  
  for (op_dir in sort(operation_dirs, decreasing = TRUE)) {  # Most recent first
    manifest_path <- file.path(operations_dir, op_dir, "manifest.json")
    
    if (file.exists(manifest_path)) {
      tryCatch({
        manifest <- jsonlite::read_json(manifest_path)
        cat(sprintf("  %s: %s (%s)\n", 
                    op_dir, 
                    manifest$operation_name, 
                    manifest$created))
      }, error = function(e) {
        cat(sprintf("  %s: Error reading manifest\n", op_dir))
      })
    } else {
      cat(sprintf("  %s: No manifest\n", op_dir))
    }
  }
}
