put.msa.data.as.new.source = function(outcome,
                                      years = NULL,
                                      from.source.name,
                                      to.source.name,
                                      metric = 'estimate',
                                      to.locations,
                                      geographic.type.from,
                                      geographic.type.to,
                                      details.for.new.data,
                                      data.manager) {
    
    error.prefix = "Cannot estimate data from contained location data: "
    # validate if desired
    if (!(from.source.name %in% names(data.manager$data[[outcome]][[metric]])))
        stop(paste0(error.prefix, "'", from.source.name, "' is not a registered source for outcome '", outcome, "'"))
    
    # print(outcome)
    outcome.data.all.ontologies = data.manager$data[[outcome]][[metric]][[from.source.name]]
    outcome.details.all.ontologies = data.manager$details[[outcome]][[metric]][[from.source.name]]
    outcome.url.all.ontologies = data.manager$url[[outcome]][[metric]][[from.source.name]]
    
    for (to.location in to.locations) {
        from.locations = locations::get.contained.locations(to.location, geographic.type.from)
        for (ont.name in names(outcome.data.all.ontologies)) {
            for (strat.name in names(outcome.data.all.ontologies[[ont.name]])) {
                strat.data = outcome.data.all.ontologies[[ont.name]][[strat.name]]
                strat.details = outcome.details.all.ontologies[[ont.name]][[strat.name]]
                strat.url = outcome.url.all.ontologies[[ont.name]][[strat.name]]
                
                from.locations.in.this.strat.data = intersect(dimnames(strat.data)$location, from.locations)
                if (length(from.locations.in.this.strat.data) == 0) next
                
                if (!is.null(years)) years.in.this.strat.data = intersect(dimnames(strat.data)$year, years)
                else years.in.this.strat.data = dimnames(strat.data)$year
                
                strat.data.from.locs.only = do.call('[', get.subset.arguments(strat.data, years.in.this.strat.data, from.locations.in.this.strat.data))
                strat.details.from.locs.only = do.call('[', get.subset.arguments(strat.details, years.in.this.strat.data, from.locations.in.this.strat.data))
                strat.url.from.locs.only = do.call('[', get.subset.arguments(strat.url, years.in.this.strat.data, from.locations.in.this.strat.data))
                if (is.null(strat.data.from.locs.only)) next # CHECK THAT THIS IS THE RIGHT THING TO SAY
                if (all(is.na(strat.data.from.locs.only))) next
                
                # Aggregate across location
                non.location.margin = setdiff(names(dim(strat.data.from.locs.only)), 'location')
                post.agg.dimnames = dimnames(strat.data.from.locs.only)[non.location.margin]
                aggregated.data = apply(strat.data.from.locs.only, non.location.margin, function(x) {sum(x, na.rm = strat.name == "year__location")})
                aggregated.data = array(aggregated.data, sapply(post.agg.dimnames, length), post.agg.dimnames)
                aggregated.details = aggregate.details.or.url(strat.details.from.locs.only, names(post.agg.dimnames), post.agg.dimnames)
                aggregated.url = aggregate.details.or.url(strat.url.from.locs.only, names(post.agg.dimnames), post.agg.dimnames)
                if (all(is.na(aggregated.data))) next
                
                # Details and url should be the same for all the data, but check just in case they aren't.
                details = aggregated.details[!sapply(aggregated.details, is.null)][[1]]
                url = aggregated.url[!sapply(aggregated.url, is.null)][[1]]
                if (any(sapply(aggregated.details, function(x) {!identical(x, details) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", from.source.name, "' data do not all have the same 'details'"))
                if (any(sapply(aggregated.url, function(x) {!identical(x, url) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", from.source.name, "' data do not all have the same 'url'"))
                
                # Details should now have the custom message appended to it indicating the use of this script
                details = c(details, details.for.new.data)
                
                # Put the data
                data.manager$put(data = aggregated.data,
                                 outcome = outcome,
                                 source = to.source.name,
                                 ontology.name = ont.name,
                                 dimension.values = c(list(location = to.location), post.agg.dimnames),
                                 url = url,
                                 details = details,
                                 allow.na.to.overwrite = F)
            }
            
        }
        
    }
    
}

#-- HELPERS --#

get.subset.arguments = function(data, select.years, select.locations) {
    c(list(data),
      lapply(names(dim(data)), function(d) {
          if (d == 'year') select.years
          else if (d == 'location') select.locations
          else 1:dim(data)[[d]]
      }),
      drop = F)
}

aggregate.details.or.url = function(data, keep.dimensions, post.agg.dimnames) {
    aggregated.data = apply(data, keep.dimensions, function(x) {list(unique(unlist(x)))})
    dim(aggregated.data) = sapply(post.agg.dimnames, length)
    dimnames(aggregated.data) = post.agg.dimnames

    aggregated.data = lapply(aggregated.data, function(x) {x[[1]]})
    dim(aggregated.data) = sapply(post.agg.dimnames, length)
    dimnames(aggregated.data) = post.agg.dimnames
    
    aggregated.data
}

#-- EXAMPLE USAGE --#

# SURVEILLANCE.MANAGER$register.source(source = 'cdc.aggregated.county', full.name = 'CDC Aggregated County', short.name = 'cdc aggd county')
# put.msa.data.as.new.source(outcome = 'diagnosed.prevalence',
#                            from.source.name = 'cdc',
#                            to.source.name = 'cdc.aggregated.county',
#                            to.locations = 'C.12580',
#                            geographic.type.from = 'COUNTY',
#                            geographic.type.to = 'CBSA',
#                            details.for.new.data = 'estimated from county data',
#                            data.manager = SURVEILLANCE.MANAGER)
