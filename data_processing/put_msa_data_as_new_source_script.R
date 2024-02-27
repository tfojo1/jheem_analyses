# Implementing the county aggregation with the NA suppressed values being estimated

put.msa.data.as.new.source = function(outcome,
                                      years = NULL,
                                      from.source.name,
                                      to.source.name,
                                      metric = 'estimate',
                                      to.locations,
                                      geographic.type.from,
                                      geographic.type.to,
                                      details.for.new.data,
                                      data.manager,
                                      source.for.denominator=NULL,
                                      ontology.for.denominator=NULL,
                                      maximum.suppressed.value = 4,
                                      tolerable.fraction.suppressed = 0.05,
                                      tolerable.fraction.suppressed.in.denominator = 0.1) {
    # browser()
    error.prefix = "Cannot estimate data from contained location data: "
    # validate if desired
    if (!(from.source.name %in% names(data.manager$data[[outcome]][[metric]])))
        stop(paste0(error.prefix, "'", from.source.name, "' is not a registered source for outcome '", outcome, "'"))
    # check scale
    scale = data.manager$outcome.info[[outcome]]$metadata$scale
    denominator.outcome = data.manager$outcome.info[[outcome]]$denominator.outcome
    if (!(scale %in% c('non.negative.number', 'proportion')))
        stop(paste0(error.prefix, "this method does not currently support outcomes of scale '", scale, "'"))
    if (scale == 'proportion' && is.null(source.for.denominator))
        stop(paste0(error.prefix, "outcomes of scale '", scale, "' must have a 'source.for.denominator' specified"))
    if (scale == 'proportion' && is.null(ontology.for.denominator))
        stop(paste0(error.prefix, "outcomes of scale '", scale, "' must have an 'ontology.for.denominator' specified"))
    if (scale == 'proportion' && is.null(denominator.outcome))
        stop(paste0(error.prefix, "a denominator outcome could not be found"))
    if (scale == 'proportion' && !is.null(source.for.denominator) && !(source.for.denominator %in% names(data.manager$data[[denominator.outcome]][['estimate']]))) #always aggregate with estimate, right?
        stop(paste0(error.prefix, "'", source.for.denominator, "' is not a registered source for denominator outcome '", denominator.outcome, "'"))
    if (scale == 'proportion' && !is.null(source.for.denominator) && !(ontology.for.denominator %in% names(data.manager$data[[denominator.outcome]][['estimate']][[source.for.denominator]])))
        stop(paste0(error.prefix, "'", ontology.for.denominator, "' is not a registered ontology for source '", source.for.denominator, "'"))
    
    # print(outcome)
    outcome.data.all.ontologies = data.manager$data[[outcome]][[metric]][[from.source.name]]
    outcome.details.all.ontologies = data.manager$details[[outcome]][[metric]][[from.source.name]]
    outcome.url.all.ontologies = data.manager$url[[outcome]][[metric]][[from.source.name]]
    
    if (scale=='proportion') {
        denominator.data.used.ontology = data.manager$data[[denominator.outcome]][['estimate']][[source.for.denominator]][[ontology.for.denominator]] #assumption: one ontology holds all our denominator data
    }
    
    for (to.location in to.locations) {
        # browser()
        from.locations = locations::get.contained.locations(to.location, geographic.type.from)
        for (ont.name in names(outcome.data.all.ontologies)) {
            
            # if we have a proportion, we should only use proportion data from the ontology that our denominator is using
            if (scale=='proportion' && ont.name != ontology.for.denominator) next
            
            for (strat.name in names(outcome.data.all.ontologies[[ont.name]])) {
                strat.data = outcome.data.all.ontologies[[ont.name]][[strat.name]]
                strat.details = outcome.details.all.ontologies[[ont.name]][[strat.name]]
                strat.url = outcome.url.all.ontologies[[ont.name]][[strat.name]]

                # We must have data for all counties... if it's a count. Proportions just need it all in the denominator.
                from.locations.present = intersect(from.locations, dimnames(strat.data)$location)
                if (scale == 'non.negative.number' && length(setdiff(from.locations, from.locations.present)) > 0) next
                
                if (!is.null(years)) years.in.this.strat.data = intersect(dimnames(strat.data)$year, years)
                else years.in.this.strat.data = dimnames(strat.data)$year
                
                strat.data.from.locs.only = do.call('[', get.subset.arguments(strat.data, years.in.this.strat.data, from.locations.present))
                strat.details.from.locs.only = do.call('[', get.subset.arguments(strat.details, years.in.this.strat.data, from.locations.present))
                strat.url.from.locs.only = do.call('[', get.subset.arguments(strat.url, years.in.this.strat.data, from.locations.present))
                if (is.null(strat.data.from.locs.only)) next
                if (all(is.na(strat.data.from.locs.only))) next
                
                # Aggregate across location
                non.location.margin = setdiff(names(dim(strat.data.from.locs.only)), 'location')
                
                if (scale == 'non.negative.number') {
                    # skip years where >=2/3 of counties are missing from all strata
                    years.with.enough.data = apply(
                        apply(strat.data.from.locs.only, c('year', 'location'), function(x) {all(is.na(x))}),
                        MARGIN='year',
                        function(x) {
                            sum(is.na(x)) < (2/3) * length(x)
                        }
                    )
                    
                    strat.data.from.locs.only = array.access(strat.data.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    if (length(strat.data.from.locs.only)==0) next
                    strat.details.from.locs.only = array.access(strat.details.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    strat.url.from.locs.only = array.access(strat.url.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    
                    # aggregate
                    aggregated.data = apply.robust(strat.data.from.locs.only, non.location.margin, function(x) {
                        
                        ## Where our new logic goes
                        # only use the sum if we have enough data
                        non.na.sum = sum(x, na.rm=T)
                        if (non.na.sum == 0) {
                            if (any(is.na(x))) return (NA)
                            else return (0)
                        }
                        max.expected.from.suppression = sum(is.na(x))*maximum.suppressed.value
                        if (max.expected.from.suppression / non.na.sum > tolerable.fraction.suppressed) return (NA)
                        else return(non.na.sum)
                    })
                    
                }
                
                
                
                if (scale == 'proportion') {

                    if (!(strat.name %in% names(denominator.data.used.ontology))) next
                    denominator.data = denominator.data.used.ontology[[strat.name]]

                    # We must have denominator data for all counties, not necessarily proportion data
                    if(length(setdiff(from.locations, dimnames(denominator.data)$location))>0) next

                    years.in.this.denom.data = intersect(dimnames(denominator.data)$year, years.in.this.strat.data)
                    if (length(years.in.this.denom.data)==0) next
                    
                    denominator.data.from.locs.only = do.call('[', get.subset.arguments(denominator.data, years.in.this.denom.data, from.locations))
                    if (is.null(denominator.data.from.locs.only)) next
                    if (all(is.na(denominator.data.from.locs.only))) next
                    
                    # skip years where >=2/3 of counties are missing from all strata of the denominator
                    years.with.enough.data = apply(
                        apply(denominator.data.from.locs.only, c('year', 'location'), function(x) {all(is.na(x))}),
                        MARGIN='year',
                        function(x) {
                            sum(is.na(x)) < (2/3) * length(x)
                        }
                    )
                    denominator.data.from.locs.only = array.access(denominator.data.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    if (length(denominator.data.from.locs.only)==0) next
                    
                    # also subset proportion data and details to these years
                    strat.data.from.locs.only = array.access(strat.data.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    if (length(strat.data.from.locs.only)==0) next
                    strat.details.from.locs.only = array.access(strat.details.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    strat.url.from.locs.only = array.access(strat.url.from.locs.only, year=names(years.with.enough.data)[years.with.enough.data])
                    
                    # Aggregate denominator
                    aggregated.denominator = apply.robust(denominator.data.from.locs.only, non.location.margin, function(x) {
                        max.expected.from.suppression = sum(is.na(x))*maximum.suppressed.value
                        non.na.sum = sum(x, na.rm=T)
                        max.sum = non.na.sum + max.expected.from.suppression
                        if (max.sum==0) return (NA)
                        if (max.expected.from.suppression / max.sum > tolerable.fraction.suppressed.in.denominator) return (NA)
                        else return(max.sum)
                    })
                    
                    # Subset denominator to match locations available in proportion data
                    denominator.data.from.prop.locs.only = array.access(denominator.data.from.locs.only, location=from.locations.present)
                    
                    # Aggregate product of proportion and denominator
                    aggregated.numerator = apply.robust(
                        strat.data.from.locs.only * denominator.data.from.prop.locs.only,
                        non.location.margin,
                        function(x) {
                            if (all(is.na(x))) return(NA)
                            else return(sum(x, na.rm=T))
                        })
                    
                    aggregated.data = aggregated.numerator / aggregated.denominator
                    
                }
                
                post.agg.dimnames = dimnames(aggregated.data)
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
#                            from.source.name = 'cdc.hiv',
#                            to.source.name = 'cdc.aggregated.county',
#                            to.locations = 'C.12580',
#                            geographic.type.from = 'COUNTY',
#                            geographic.type.to = 'CBSA',
#                            details.for.new.data = 'estimated from county data',
#                            data.manager = SURVEILLANCE.MANAGER)

# for testing

ss=SURVEILLANCE.MANAGER$clone()
ss$register.source(source = 'cdc.aggregated.proportion', parent.source='NHSS', full.name = 'CDC Aggregated County', short.name = 'cdc aggd county')
put.msa.data.as.new.source('suppression',
                           from.source.name = 'cdc.hiv',
                           to.source.name='cdc.aggregated.proportion',
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'county',
                           geographic.type.to = 'cbsa',
                           details.for.new.data = 'estimated from county data',
                           data.manager = ss,
                           source.for.denominator = 'cdc.hiv',
                           ontology.for.denominator = 'cdc')
