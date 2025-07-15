# Code to add to section2.processing.R (before the save command)

# Register total syphilis diagnoses outcome
syphilis.manager$register.outcome(
  'total.syphilis.diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Total Syphilis Diagnoses',
    axis.name = 'Total Syphilis Diagnoses', 
    units = 'cases',
    description = "Total Syphilis Diagnoses (sum of PS + Early + Unknown Duration/Late)"))

# Create composite total diagnosis data for both sources
component_outcomes <- c('ps.syphilis.diagnoses', 'early.syphilis.diagnoses', 'unknown.duration.or.late.syphilis.diagnoses')
sources_to_process <- c('cdc.sti', 'cdc.aggregated.county')

for (source_name in sources_to_process) {
  
  cat(sprintf("Creating total diagnosis composite for source: %s\n", source_name))
  
  # Get all stratifications available for PS syphilis (use as template)
  if ('cdc.sti' %in% names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]])) {
    ontology_name <- 'cdc.sti'
  } else {
    next # Skip if data doesn't exist for this source
  }
  
  available_stratifications <- names(syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]])
  
  for (stratification in available_stratifications) {
    
    cat(sprintf("  Processing stratification: %s\n", stratification))
    
    # Check if all three components have data for this stratification
    component_data_exists <- sapply(component_outcomes, function(outcome) {
      !is.null(syphilis.manager$data[[outcome]]$estimate[[source_name]][[ontology_name]][[stratification]])
    })
    
    if (!all(component_data_exists)) {
      cat(sprintf("    Skipping - not all components available\n"))
      next
    }
    
    # Get data arrays for all three components
    ps_data <- syphilis.manager$data$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    early_data <- syphilis.manager$data$early.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    unknown_data <- syphilis.manager$data$unknown.duration.or.late.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    
    # Ensure all arrays have the same dimensions
    if (!identical(dim(ps_data), dim(early_data)) || !identical(dim(ps_data), dim(unknown_data))) {
      cat(sprintf("    Skipping - component arrays have different dimensions\n"))
      next
    }
    
    if (!identical(dimnames(ps_data), dimnames(early_data)) || !identical(dimnames(ps_data), dimnames(unknown_data))) {
      cat(sprintf("    Skipping - component arrays have different dimnames\n"))
      next  
    }
    
    # Create total by summing components (handle NAs appropriately)
    total_data <- ps_data + early_data + unknown_data
    
    # If any component is NA, the total should be NA (conservative approach)
    na_mask <- is.na(ps_data) | is.na(early_data) | is.na(unknown_data)
    total_data[na_mask] <- NA
    
    # Put the total data into the manager
    # Get URL and details from PS syphilis data (as representative)
    ps_url <- syphilis.manager$url$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    ps_details <- syphilis.manager$details$ps.syphilis.diagnoses$estimate[[source_name]][[ontology_name]][[stratification]]
    
    # Unhash to get actual strings
    url_strings <- syphilis.manager$unhash.url(ps_url)
    detail_strings <- syphilis.manager$unhash.details(ps_details)
    
    # Use first non-NA URL and create composite details
    url_to_use <- url_strings[!is.na(url_strings)][1]
    if (is.na(url_to_use)) url_to_use <- "composite calculation"
    
    details_to_use <- "Sum of PS + Early + Unknown Duration/Late syphilis diagnoses"
    
    # Put the data
    syphilis.manager$put(
      data = total_data,
      outcome = 'total.syphilis.diagnoses',
      source = source_name,
      ontology.name = ontology_name,
      url = url_to_use,
      details = details_to_use
    )
    
    cat(sprintf("    Successfully created total diagnosis data\n"))
  }
}

cat("Total syphilis diagnosis composite creation completed!\n")
