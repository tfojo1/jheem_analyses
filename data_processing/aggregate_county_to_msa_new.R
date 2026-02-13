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
#' @param skip.coverage.condition Should a proportion be aggregated with
#' available data even if the available counties aren't sufficient according to 
#' coverage? Can also be used on counts, though caution is advised.
#' 
put.msa.data.as.new.source.NEW <- function(data.manager,
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
                                           skip.coverage.condition=F,
                                           metric = 'estimate',
                                           source.for.denominator=NULL,
                                           ontology.for.denominator=NULL,
                                           debug=F) {
    
    error_prefix <- paste0("Cannot estimate ", geographic.type.to, " ", outcome, " data from ", geographic.type.from, " data: ")
    
    scale <- data.manager$outcome.info[[outcome]]$metadata$scale
    
    outcome_data_all_ontologies <- data.manager$data[[outcome]][[metric]][[from.source.name]]
    outcome_url_all_ontologies <- data.manager$url[[outcome]][[metric]][[from.source.name]]
    
    if (debug) browser()
    if (scale == "proportion") {
        denominator_outcome <- data.manager$outcome.info[[outcome]]$denominator.outcome
        denominator_data_for_ontology <- data.manager$data[[denominator_outcome]][["estimate"]][[source.for.denominator]][[ontology.for.denominator]]
    }
    
    for (to_location in to.locations) {
        
        from_locations <- locations::get.contained.locations(to_location, geographic.type.from)
        
        error_prefix <- paste0("Cannot aggregate ", geographic.type.from, " data for 'to.location' ", to_location, ": ")
        
        if (length(from_locations)==0)
            stop(paste0(error_prefix, "No locations of type '", geographic.type.from, "' found for location '", to_location, "'"))
        
        if (!skip.coverage.condition) {
        
            # Contribution data - each location as a fraction of all in that year
            relative_contribution_data <- get_relative_contributions(data.manager,
                                                                     outcome.for.relative.contribution,
                                                                     source.for.relative.contribution,
                                                                     ontology.for.relative.contribution,
                                                                     from_locations,
                                                                     error.prefix = error_prefix)
            
            years_with_contribution_data <- dimnames(relative_contribution_data)$year
            
            # Proportions COULD get a temporary pass since they could possibly have complete denominator data...
            if (length(years_with_contribution_data)==0) {
                print(paste0("Skipping '", to_location, "' because no relative contribution data was found"))
                next
            }
        }
        
        for (ont_name in names(outcome_data_all_ontologies)) {
            
            if (scale == "proportion" && ont_name != ontology.for.denominator) next
            
            for (strat_name in names(outcome_data_all_ontologies[[ont_name]])) {
                
                # if (strat_name == "year__location__race") browser()
                
                if (scale == "proportion" && !(strat_name %in% names(denominator_data_for_ontology))) next
                
                strat_data <- outcome_data_all_ontologies[[ont_name]][[strat_name]]
                strat_url <- outcome_url_all_ontologies[[ont_name]][[strat_name]]
                
                from_locations_present <- intersect(from_locations, dimnames(strat_data)$location)
                if (length(from_locations_present)==0) {
                    print(paste0("Skipping stratification '", strat_name,"' of '", to_location, "' because no data was found at the ", geographic.type.from, " level"))
                    next
                }
                
                if (!skip.coverage.condition) {
                    # For proportions, we don't need contr data necessarily, but will simplify code by assuming we DO have it, since we normally do.
                    years_in_this_strat_and_contr_data <- intersect(dimnames(strat_data)$year, years_with_contribution_data)
                    if (length(years_in_this_strat_and_contr_data)==0) next
                    
                    strat_data_from_locs_only <- subset_by_year_location(strat_data,
                                                                         years_in_this_strat_and_contr_data,
                                                                         from_locations_present)
                    strat_url_from_locs_only <- subset_by_year_location(strat_url,
                                                                        years_in_this_strat_and_contr_data,
                                                                        from_locations_present)
                } else {
                    strat_data_from_locs_only <- subset_by_year_location(strat_data,
                                                                         dimnames(strat_data)$year,
                                                                         from_locations_present)
                    strat_url_from_locs_only <- subset_by_year_location(strat_url,
                                                                         dimnames(strat_url)$year,
                                                                         from_locations_present)
                }
                if (is.null(strat_data_from_locs_only) || is.null(strat_url_from_locs_only)) next
                
                # Will be aggregating across location
                non_location_margin = setdiff(names(dim(strat_data_from_locs_only)), 'location')
                
                if (scale == "non.negative.number") {
                    if (!skip.coverage.condition) {
                        strata_with_enough_data <- get_strata_with_enough_county_contribution(strat_data_from_locs_only,
                                                                                              relative_contribution_data,
                                                                                              required.coverage,
                                                                                              from_locations_present,
                                                                                              years_in_this_strat_and_contr_data,
                                                                                              non_location_margin)
                        aggregated_data <- aggregate_counts(strat_data_from_locs_only,
                                                            strata_with_enough_data,
                                                            non_location_margin)
                    } else {
                        aggregated_data <- aggregate_counts(strat_data_from_locs_only,
                                                            strata.with.enough.data = NULL,
                                                            non_location_margin,
                                                            skip.contr.condition = T)
                    }
                    
                }
                
                if (scale == "proportion") {
                    
                    # if locs A,B,C in prop data, but locs A,B,D in denom data, but there actually exist A:E,
                    # we will do weighted average of A and B IF
                    # A and B account for >90% of the A:E relative contribution outcome total?
                    
                    strat_denominator_data <- denominator_data_for_ontology[[strat_name]]
                    denominator_from_locations_present <- intersect(from_locations, dimnames(strat_denominator_data)$location)
                    from_locations_in_both <- intersect(from_locations_present, denominator_from_locations_present)
                    if (length(from_locations_in_both)==0) next
                    
                    # We need from_locations_in_both to be sufficient in terms of total denominator in the to_location,
                    # but since there's a high likelihood of the denominator data being missing in enough places that it count
                    # suffice to determine coverage, we'll just skip to using the relative_contribution_data to decide if our
                    # from_locations_in_both are sufficient.
                    
                    if (!skip.coverage.condition) {
                        years_in_this_denominator_and_contr_data <- intersect(dimnames(strat_denominator_data)$year,
                                                                              years_with_contribution_data)
                        years_in_both <- intersect(years_in_this_strat_and_contr_data, years_in_this_denominator_and_contr_data)
                    } else {
                        years_in_both <- intersect(dimnames(strat_data)$year, dimnames(strat_denominator_data)$year)
                    }
                    if (length(years_in_both)==0) next
                    
                    strat_data_from_locs_only <- subset_by_year_location(strat_data_from_locs_only,
                                                                         years_in_both,
                                                                         from_locations_in_both)
                    
                    strat_denominator_data_from_locs_only <- subset_by_year_location(strat_denominator_data,
                                                                                     years_in_both,
                                                                                     from_locations_in_both)
                    
                    if (!skip.coverage.condition) {
                        # See if the counties we have prop data for have enough denominator. Just use relative contribution data?
                        strata_with_enough_prop_data <- get_strata_with_enough_county_contribution(strat_data_from_locs_only,
                                                                                                   relative_contribution_data,
                                                                                                   required.coverage,
                                                                                                   from_locations_in_both,
                                                                                                   years_in_both,
                                                                                                   non_location_margin)
                        
                        aggregated_data <- aggregate_proportions(proportion.data = strat_data_from_locs_only,
                                                                 denominator.data = strat_denominator_data_from_locs_only,
                                                                 strata.with.enough.data = strata_with_enough_prop_data,
                                                                 non.location.margin = non_location_margin)
                    } else {
                        aggregated_data <- aggregate_proportions(proportion.data = strat_data_from_locs_only,
                                                                 denominator.data = strat_denominator_data_from_locs_only,
                                                                 strata.with.enough.data = NULL,
                                                                 non.location.margin = non_location_margin,
                                                                 skip.contr.condition = T)
                    }
                }
                
                if (all(is.na(aggregated_data))) next
                
                post_aggregation_dimnames <- dimnames(aggregated_data)
                
                # Unhash url
                strat_url_from_locs_only <- data.manager$unhash.url(strat_url_from_locs_only)

                # Put the data
                # Doesn't work for American Somoa
                # tryCatch({data.manager$put(data = aggregated_data,
                #                            outcome = outcome,
                #                            source = to.source.name,
                #                            ontology.name = ont_name,
                #                            dimension.values = c(list(location=to_location), post_aggregation_dimnames),
                #                            url = get_url_for_put(strat_url_from_locs_only, post_aggregation_dimnames),
                #                            details = details.for.new.data,
                #                            allow.na.to.overwrite = F)}, error=function(e){browser()})
                data.manager$put(data = aggregated_data,
                                 outcome = outcome,
                                 source = to.source.name,
                                 ontology.name = ont_name,
                                 dimension.values = c(list(location=to_location), post_aggregation_dimnames),
                                 url = get_url_for_put(strat_url_from_locs_only, post_aggregation_dimnames),
                                 details = details.for.new.data,
                                 allow.na.to.overwrite = F)
                
                cat(paste0("put data for ", to_location, " at stratification '", strat_name,"'\n"))
                
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
                                       from.locations,
                                       error.prefix,
                                       debug=F) {
    if (debug) browser()
    tryCatch(
        {relative_contribution_data <- data.manager$data[[outcome.for.relative.contribution]][["estimate"]][[source.for.relative.contribution]][[ontology.for.relative.contribution]][["year__location"]][,from.locations,drop=F]},
        error=function(e){stop(paste0(error.prefix, "Incomplete ", outcome.for.relative.contribution, " data found"))})
    
    if (is.null(relative_contribution_data))
        stop(paste0(error.prefix, "Incomplete ", outcome.for.relative.contribution, " data found"))
    
    if (dim(relative_contribution_data)[["location"]] == 1)
        return(relative_contribution_data[!is.na(relative_contribution_data),,drop=F])

    # check if we got data. If a county is missing... we have trouble!
    relative_contributions <- t(apply(relative_contribution_data, "year", function(x) {x / sum(x)}))
    relative_contributions[apply(!is.na(relative_contributions), "year", all),,drop=F]
    
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
                                                       non.location.margin,
                                                       debug=F) {
    
    # BUG: what if we don't have enough years in the relative contribution data?
    # Meaning, we cannot tell what counties are big enough in certain years,
    # because there is no population data somehow.
    if (debug) browser()
    
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
                             non.location.margin,
                             skip.contr.condition=F) {
    all_aggregated_counts <- apply.robust(count.data, non.location.margin, sum, na.rm=T)
    if (!skip.contr.condition)
        all_aggregated_counts[!strata.with.enough.data] <- NA
    all_aggregated_counts
}

aggregate_proportions <- function(proportion.data,
                                  denominator.data,
                                  strata.with.enough.data,
                                  non.location.margin,
                                  skip.contr.condition=F) {
    
    # Wherever proportion.data is NA, make denominator.data NA as well or it will be too large
    denominator.data[is.na(proportion.data)] <- NA
    
    numerator <- proportion.data * denominator.data
    
    aggregated_numerator <- apply.robust(numerator, non.location.margin, sum, na.rm=T)
    aggregated_denominator <- apply.robust(denominator.data, non.location.margin, sum, na.rm=T)
    all_aggregated_proportions <- aggregated_numerator / aggregated_denominator
    
    if (!skip.contr.condition)
        all_aggregated_proportions[!strata.with.enough.data] <- NA
    all_aggregated_proportions
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