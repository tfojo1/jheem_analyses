# aggregate county to MSA checking contributions of counties

#' @description
#' Aggregate county data from a single source to the MSA level (or other
#' geographic types), and PUT the data to the data manager. Too much missing
#' data will prevent aggregation according to certain rules (see param "required
#' coverage").
#' 
#' @param required.coverage The fraction of the population (or other
#' denominator) that must have non-missing data before aggregation will be
#' allowed. For example, if one county is missing, but the other two which are
#' not make up at least 90% of the population of that MSA, then the two with
#' data will be aggregated.
#' @param outcome.for.relative.contribution,source.for.relative.contribution,ontology.for.relative.contribution
#' For use with "required.coverage" to determine when there is enough data to
#' permit aggregation.
#' 
put.msa.data.as.new.source <- function(data.manager,
                                       outcome,
                                       from.source.name,
                                       to.source.name,
                                       to.locations,
                                       geographic.type.from,
                                       geographic.type.to,
                                       details.for.new.data,
                                       outcome.for.relative.contribution,
                                       source.for.relative.contribution,
                                       ontology.for.relative.contribution,
                                       required.coverage=0.9,
                                       metric = 'estimate') {
    
    error_prefix = "Cannot estimate data from contained location data: "
    
    outcome_data_all_ontologies = data.manager$data[[outcome]][[metric]][[from.source.name]]
    outcome_url_all_ontologies = data.manager$url[[outcome]][[metric]][[from.source.name]]
    
    for (to_location in to.locations) {
        
        from_locations <- locations::get.contained.locations(to_location, geographic.type.from)
        
        # Contribution data - each location as a fraction of all in that year
        relative_contribution_data <- get_relative_contributions(data.manager,
                                                                 outcome.for.relative.contribution,
                                                                 source.for.relative.contribution,
                                                                 ontology.for.relative.contribution,
                                                                 from_locations)
        
        for (ont_name in names(outcome_data_all_ontologies)) {
            
            for (strat_name in names(outcome_data_all_ontologies[[ont_name]])) {
                
                strat_data <- outcome_data_all_ontologies[[ont_name]][[strat_name]]
                strat_url <- outcome_url_all_ontologies[[ont_name]][[strat_name]]
                
                from_locations_present <- intersect(from_locations, dimnames(strat_data)$location)
                
                years_in_this_strat_data <- dimnames(strat_data)$year
                
                strat_data_from_locs_only <- subset_by_year_location(strat_data,
                                                                     years_in_this_strat_data,
                                                                     from_locations_present)
                strat_url_from_locs_only <- subset_by_year_location(strat_url,
                                                                    years_in_this_strat_data,
                                                                    from_locations_present)
                if (is.null(strat_data_from_locs_only) || is.null(strat_url_from_locs_only)) next
                
                # Will be aggregating across location
                non_location_margin = setdiff(names(dim(strat_data_from_locs_only)), 'location')
                
                strata_with_enough_data <- get_strata_with_enough_county_contribution(strat_data_from_locs_only,
                                                                                      relative_contribution_data,
                                                                                      required.coverage,
                                                                                      from_locations_present,
                                                                                      years_in_this_strat_data,
                                                                                      non_location_margin)
                aggregated_data <- aggregate_counts(strat_data_from_locs_only,
                                                    strata_with_enough_data,
                                                    non_location_margin)
                if (all(is.na(aggregated_data))) next
                
                post_aggregation_dimnames <- dimnames(aggregated_data)
                
                # Unhash url
                strat_url_from_locs_only <- data.manager$unhash.url(strat_url_from_locs_only)

                # Put the data
                data.manager$put(data = aggregated_data,
                                 outcome = outcome,
                                 source = to.source.name,
                                 ontology.name = ont_name,
                                 dimension.values = c(list(location=to_location), post_aggregation_dimnames),
                                 url = get_url_for_put(strat_url_from_locs_only, post_aggregation_dimnames),
                                 details = details.for.new.data,
                                 allow.na.to.overwrite = F)
                
            }
        }
    }
}

#' @description
#' Get the relative contributions we expect each from_location to have to the
#' to_location so that we can decide how important it is to have data from them
#' when we aggregate.
#' 
get_relative_contributions <- function(data.manager,
                                       outcome.for.relative.contribution,
                                       source.for.relative.contribution,
                                       ontology.for.relative.contribution,
                                       from.locations) {
    
    relative_contribution_data <- data.manager$data[[outcome.for.relative.contribution]][["estimate"]][[source.for.relative.contribution]][[ontology.for.relative.contribution]][["year__location"]][,from.locations]

    # check if we got data. If a county is missing... we have trouble!

    relative_contributions <- t(apply(relative_contribution_data, "year", function(x) {x / sum(x)}))
    
    # We can't have any missing. Or if we do, we must skip those years?
    
}

#' @description
#' Check whether the counties we have data for account for enough of the total
#' populations/cases/etc. to justify summing their values for the MSA.
#' 
get_strata_with_enough_county_contribution <- function(count.data,
                                                       relative.contributions.data,
                                                       required.coverage,
                                                       from.locations.present,
                                                       years.present,
                                                       non.location.margin) {
    
    # Match count.data shape, including years and locations and other dimensions.
    relative_contributions_data <- relative.contributions.data[years.present,
                                                               from.locations.present]
    
    expand_by_factor <- prod(dim(count.data)[setdiff(names(dim(count.data)), c("year", "location"))])
    relative_contributions_data <- array(rep(relative_contributions_data, expand_by_factor),
                                         dim(count.data), dimnames(count.data))
    
    # If count.data was only totals, and thus had same dimensions as contributions data
    apply(relative_contributions_data * !is.na(count.data),
          non.location.margin,
          function(x) {
              sum(x) >= required.coverage
          })
}

#' @description
#' Aggregate all count data without regard to NA, but then convert values to NA
#' that we externally determined should be NA due to insufficient data.
#' 
aggregate_counts <- function(count.data,
                             strata.with.enough.data,
                             non.location.margin) {
    all_aggregated_counts <- apply.robust(count.data, non.location.margin, sum, na.rm=T)
    all_aggregated_counts[!strata.with.enough.data] <- NA
    all_aggregated_counts
}

#' @description
#' Pick one url to use for this put. Zoe said it is safe to use the first
#' non-missing url we get, since we assume that all the points have the same
#' url's. Url's are NA if they correspond to points with no data.
#' 
get_url_for_put <- function(url.data,
                            post.agg.dimnames) {
    aggregated_url <- aggregate_url(url.data, post.agg.dimnames)
    url <- unique(unlist(aggregated_url))
    url[!sapply(url, is.na)][[1]]
}

#' @description
#' Aggregate url data. I can never remember why it has to work this way until I
#' look at it again.
#' 
aggregate_url <- function(url_data,
                          post_agg_dimnames) {
    aggregated_url <- array(apply(url_data, names(post_agg_dimnames), function(x) {list(unique(unlist(x)))}),
                            sapply(post_agg_dimnames, length),
                            post_agg_dimnames)
    aggregated_url <- array(lapply(aggregated_url, function(x) {x[[1]]}),
                            sapply(post_agg_dimnames, length),
                            post_agg_dimnames)
}

#' @description
#' Subset the data to only the years and locations we have present.
#' Works on any number of dimensions.
#' 
subset_by_year_location <- function(data, select.years, select.locations) {
    subset_arguments <- c(list(data),
                          lapply(names(dim(data)), function(d) {
                              if (d == 'year') select.years
                              else if (d == 'location') select.locations
                              else 1:dim(data)[[d]]
                          }),
                          drop = F)
    do.call('[', subset_arguments)
}