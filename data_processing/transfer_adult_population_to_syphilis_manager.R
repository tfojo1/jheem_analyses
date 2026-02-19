# Transfer Adult Population Data from Surveillance Manager to Syphilis Manager
# Updated: 2025-07-19
# Purpose: Transfer complete adult.population outcome using import.data() method

library(jheem2)

cat("Loading data managers...\n")

# Load both managers
surv_path <- Sys.getenv("SURVEILLANCE_MANAGER_PATH", "Q:/data_managers/surveillance.manager.rdata")
surveillance.manager <- load.data.manager(surv_path)
#syphilis.manager <- load.data.manager("Q:/data_managers/syphilis.manager.rdata")

cat("✅ Both managers loaded successfully\n")

cat("\nChecking source data structure...\n")
cat("Available sources in surveillance manager:\n")
print(names(surveillance.manager$data$adult.population$estimate))

cat("Census.population stratifications:\n")
print(names(surveillance.manager$data$adult.population$estimate$census.population$census))

cat("Census.aggregated.adult.population stratifications:\n")
print(names(surveillance.manager$data$adult.population$estimate$census.aggregated.adult.population$census))

cat("\nRegistering missing ontologies and sources...\n")

# Register missing ontologies (following import.data pattern)
for (ontology.name in surveillance.manager$ontology.names) {
    if (!(ontology.name %in% syphilis.manager$ontology.names)) {
        cat("Registering", ontology.name, "ontology...\n")
        syphilis.manager$register.ontology(
            ontology.name,
            surveillance.manager$ontologies[[ontology.name]]
        )
        cat("✅", ontology.name, "ontology registered\n")
    }
}

# Register aggregated source if missing
if (!("census.aggregated.adult.population" %in% names(syphilis.manager$source.info))) {
    syphilis.manager$register.source(
        source = "census.aggregated.adult.population",
        parent.source = "census",
        full.name = "Census Aggregated Adult Population",
        short.name = "census.agg.pop"
    )
    cat("✅ Aggregated source registered\n")
} else {
    cat("✅ Aggregated source already exists\n")
}

cat("\nTransferring adult population data using import.data() pattern...\n")

# Follow the import.data() pattern for just adult.population outcome
outcome <- "adult.population"

# Register outcome if needed
if (!(outcome %in% syphilis.manager$outcomes)) {
    syphilis.manager$register.outcome(
        outcome = surveillance.manager$outcome.info[[outcome]][['outcome']],
        metadata = surveillance.manager$outcome.info[[outcome]][['metadata']], 
        denominator.outcome = surveillance.manager$outcome.info[[outcome]][['denominator.outcome']]
    )
}

# Transfer data following import.data() nested loop pattern
for (metric in names(surveillance.manager$data[[outcome]])) {
    for (source in names(surveillance.manager$data[[outcome]][[metric]])) {
        for (ontology in names(surveillance.manager$data[[outcome]][[metric]][[source]])) {
            for (stratification in names(surveillance.manager$data[[outcome]][[metric]][[source]][[ontology]])) {
                
                cat("  Transferring", source, ontology, stratification, "\n")
                
                # Get the data and metadata for this stratification
                data_array <- surveillance.manager$data[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                details_array <- surveillance.manager$details[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                url_array <- surveillance.manager$url[[outcome]][[metric]][[source]][[ontology]][[stratification]]
                
                # Handle unique details/url combinations (simplified version)
                unique_details <- unique(as.vector(details_array))
                unique_urls <- unique(as.vector(url_array))
                
                for (one_details in unique_details) {
                    for (one_url in unique_urls) {
                        if (is.na(one_details) || is.na(one_url)) next
                        
                        # Get indices for this details/url combination
                        indices <- which(details_array == one_details & url_array == one_url)
                        
                        # Create array with NAs except at matching indices
                        put_data <- array(NA, dim(data_array), dimnames(data_array))
                        put_data[indices] <- data_array[indices]
                        
                        # Unhash the details and url
                        unhashed_details <- surveillance.manager$unhash.details(one_details)
                        unhashed_url <- surveillance.manager$unhash.url(one_url)
                        
                        # Put the data
                        syphilis.manager$put(
                            data = put_data,
                            outcome = outcome,
                            metric = metric,
                            source = source,
                            ontology.name = ontology,
                            url = unhashed_url,
                            details = unhashed_details,
                            allow.na.to.overwrite = FALSE
                        )
                    }
                }
            }
        }
    }
}

cat("✅ Adult population data transferred with all stratifications preserved\n")

cat("\nVerifying import results...\n")
cat("Available stratifications in census.population:\n")
print(names(syphilis.manager$data$adult.population$estimate$census.population$census))

cat("Available stratifications in census.aggregated.adult.population:\n")
print(names(syphilis.manager$data$adult.population$estimate$census.aggregated.adult.population$census))

cat("\n✅ Transfer completed successfully! All stratifications preserved.\n")

# cat("\nSaving updated manager...\n")
# new_filename <- paste0("cached/syphilis.manager_complete_", Sys.Date(), ".rdata")
# save(syphilis.manager, file = new_filename)
# cat("✅ Saved to:", new_filename, "\n")

