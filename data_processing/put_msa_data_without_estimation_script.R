put.msa.data.strict = function(census.outcome.name = 'population',
                               put.outcome.name = 'adult.population',
                               locations,
                               contained.geographic.type = 'county', # if this is NULL, it won't get any subtype, just stay at the main geographic level
                               stratification.name = "year__location__age__race__ethnicity__sex",
                               put.stratifications = list('age','race', 'ethnicity', c('race', 'ethnicity'), 'sex', c('age', 'race', 'ethnicity'), c('age', 'sex'), c('race', 'ethnicity', 'sex')), # race and ethnicity are individually needed by the proportion tested likelihood obs-n calculation
                               age.lower.limit = 13,
                               age.upper.limit = Inf,
                               data.manager,
                               census.manager,
                               onts.to.ignore = 'stratified.census')
{
    # register the parent source(s), source(s), ontologies, and/or outcome if necessary
    if (!(put.outcome.name %in% data.manager$outcomes))
        stop(paste0("Error: outcome '", put.outcome.name, "' has not yet been registered"))
    
    for (parent.source.name in names(census.manager$parent.source.info))
        if (!(parent.source.name %in% names(data.manager$parent.source.info)))
            data.manager$register.parent.source(parent.source = census.manager$parent.source.info[[parent.source.name]]$parent.source,
                                                full.name = census.manager$parent.source.info[[parent.source.name]]$full.name,
                                                short.name = census.manager$parent.source.info[[parent.source.name]]$short.name)
    
    for (source.name in names(census.manager$source.info))
        if (!(source.name %in% names(data.manager$source.info)))
            data.manager$register.source(source = census.manager$source.info[[source.name]]$source,
                                         parent.source = census.manager$source.info[[source.name]]$parent.source,
                                         full.name = census.manager$source.info[[source.name]]$full.name,
                                         short.name = census.manager$source.info[[source.name]]$short.name)
    
    for (ont.name in census.manager$ontology.names)
    {
        if (ont.name %in% onts.to.ignore) next
        if (!(ont.name %in% data.manager$ontology.names))
            data.manager$register.ontology(ont.name, census.manager$ontologies[[ont.name]])}
    
    has.put.totals.data.for.locations = logical(length(locations))
    has.put.stratified.data.for.locations = logical(length(locations))
    names(has.put.totals.data.for.locations)=locations
    names(has.put.stratified.data.for.locations)=locations
    
    all.data = census.manager$data[[census.outcome.name]][['estimate']]
    all.url = census.manager$url[[census.outcome.name]][['estimate']]
    all.details = census.manager$details[[census.outcome.name]][['estimate']]
    
    error.prefix = paste0("Error putting '", put.outcome.name, "' data:")
    # browser()
    for (source.name in names(all.data))
    {
        data.this.source = all.data[[source.name]]
        url.this.source = all.url[[source.name]]
        details.this.source = all.details[[source.name]]
        
        for (ont.name in names(data.this.source))
        {
            if (ont.name %in% onts.to.ignore) next
            data.this.ont = data.this.source[[ont.name]][[stratification.name]]
            url.this.ont = url.this.source[[ont.name]][[stratification.name]]
            details.this.ont = details.this.source[[ont.name]][[stratification.name]]
            
            if (is.null(data.this.ont) || is.null(url.this.ont) || is.null(details.this.ont)) next
                # stop(paste0(error.prefix, "data with stratification '", stratification.name, "' could not be found for source '", source.name, "' and ontology '", ont.name, "'"))
           
            # Subset by age
            ont.ages = dimnames(data.this.ont)$age
            parsed.ages = parse.age.strata.names(ont.ages)
            ages.within.limit = ont.ages[parsed.ages$upper <= age.upper.limit & parsed.ages$lower >= age.lower.limit]
            subset.by.ages.arguments = lapply(names(dim(data.this.ont)), function(d) {
                if (d == 'age') ages.within.limit
                else 1:dim(data.this.ont)[[d]]
            })
            # browser()
            data.from.locs.only = do.call('[', c(list(data.this.ont), subset.by.ages.arguments, list(drop=F)))
            url.from.locs.only = do.call('[', c(list(url.this.ont), subset.by.ages.arguments, list(drop=F)))
            details.from.locs.only = do.call('[', c(list(details.this.ont), subset.by.ages.arguments, list(drop=F)))
            
            for (location in locations) {
                # print(location)
                # loctime = Sys.time()
                if (!is.null(contained.geographic.type)) contained.locations = locations::get.contained.locations(location, sub.type =  contained.geographic.type)
                else contained.locations = location
                if (length(contained.locations)==0) next
                # loctime = Sys.time()-loctime
                # other.time = Sys.time()
                from.locations.present = intersect(contained.locations, dimnames(data.from.locs.only)$location)
                if (!setequal(from.locations.present, contained.locations)) next # print warning?
                
                subset.by.locs.arguments = lapply(names(dim(data.from.locs.only)), function(d) {
                    if (d == 'location') from.locations.present
                    else 1:dim(data.from.locs.only)[[d]]
                })
                # browser()
                data.from.this.location.only = do.call('[', c(list(data.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                url.from.this.location.only = do.call('[', c(list(url.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                details.from.this.location.only = do.call('[', c(list(details.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                
                # If no data found, must skip or risk errors below
                if (all(is.na(data.from.this.location.only))) next
                
                # Convert url and details to their character forms
                url.from.this.location.only = census.manager$unhash.url(url.from.this.location.only)
                details.from.this.location.only = census.manager$unhash.details(details.from.this.location.only)
                
                # Details and URL should be the same for all data, but check just in case they aren't
                url = url.from.this.location.only[[1]]
                details = details.from.this.location.only[[1]]
                if (any(sapply(details.from.this.location.only, function(x) {!identical(x, details) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                if (any(sapply(url.from.this.location.only, function(x) {!identical(x, url) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))
                
                ## -- TOTALS -- #
                aggregated.totals.data = apply(data.from.this.location.only, 'year', sum, na.rm=T)
                post.agg.dimnames = list(year=names(aggregated.totals.data))
                aggregated.totals.data = array(aggregated.totals.data, sapply(post.agg.dimnames, length), post.agg.dimnames)
                data.manager$put(data = aggregated.totals.data,
                                 outcome = put.outcome.name,
                                 source = source.name,
                                 ontology.name = ont.name,
                                 dimension.values = list(location = location),
                                 url = url,
                                 details = details)
                has.put.totals.data.for.locations[[location]] = T
                
                ## -- STRATIFIED -- ##
                for (stratification in put.stratifications) {
                    # print(paste0("on stratification: ", stratification, " at time ", Sys.time()))
                    # Hand-aggregate the stratified data to each stratification
                    margin.of.aggregation = c('year', stratification)
                    aggregated.data = apply(data.from.this.location.only, margin.of.aggregation, sum, na.rm=T)
                    data.manager$put(data = aggregated.data,
                                     outcome = put.outcome.name,
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                    has.put.stratified.data.for.locations[[location]] = T
                    
                }
                # other.time = Sys.time()-other.time
                # print(paste0("location: ", location, " loctime: ", loctime, " othertime: ", other.time))
            }
        }
    }
    
    # print warning for locations we skipped for some reason
    skipped.totals.locations = names(locations)[!has.put.totals.data.for.locations]
    if (length(skipped.totals.locations)>0)
        warning(paste0("putting totals data for the following location(s) failed: "), paste0(skipped.totals.locations, collapse=", "))
    skipped.stratified.locations = names(locations)[!has.put.stratified.data.for.locations]
    if (length(skipped.stratified.locations)>0 && length(put.stratifications)>0)
        warning(paste0("putting totals data for the following location(s) failed: "), paste0(skipped.stratified.locations, collapse=", "))
}

# Need to source until it's in the package.
source('../jheem2/R/HELPERS_age_year_helpers.R')

put.msa.data.strict.for.stratified.census = function(census.outcome.name = 'population',
                                                     put.outcome.name = 'adult.population',
                                                     locations,
                                                     contained.geographic.type = 'county', # if this is NULL, it won't get any subtype, just stay at the main geographic level
                                                     stratification.name = "year__location__age",
                                                     put.stratifications = list(),
                                                     age.lower.limit = 13,
                                                     age.upper.limit = Inf,
                                                     data.manager,
                                                     census.manager,
                                                     source.name = 'census.population',
                                                     ont.name = 'stratified.census')
{
    # register the parent source(s), source(s), ontologies, and/or outcome if necessary
    if (!(put.outcome.name %in% data.manager$outcomes))
        stop(paste0("Error: outcome '", put.outcome.name, "' has not yet been registered"))
    
    to.ont = ontology(year=NULL,location=NULL, incomplete.dimensions = c('year', 'location'))
    if (!(ont.name %in% data.manager$ontology.names))
        data.manager$register.ontology(ont.name, to.ont)
    
    has.put.totals.data.for.locations = logical(length(locations))
    has.put.stratified.data.for.locations = logical(length(locations))
    names(has.put.totals.data.for.locations)=locations
    names(has.put.stratified.data.for.locations)=locations
    
    all.data = census.manager$data[[census.outcome.name]][['estimate']]
    all.url = census.manager$url[[census.outcome.name]][['estimate']]
    all.details = census.manager$details[[census.outcome.name]][['estimate']]
    
    error.prefix = paste0("Error putting '", put.outcome.name, "' data:")
    # browser()
    for (source.name in source.name)
    {
        data.this.source = all.data[[source.name]]
        url.this.source = all.url[[source.name]]
        details.this.source = all.details[[source.name]]
        
        for (ont.name in ont.name)
        {
            data.this.ont = data.this.source[[ont.name]][[stratification.name]]
            url.this.ont = url.this.source[[ont.name]][[stratification.name]]
            details.this.ont = details.this.source[[ont.name]][[stratification.name]]
            
            if (is.null(data.this.ont) || is.null(url.this.ont) || is.null(details.this.ont)) next
            # stop(paste0(error.prefix, "data with stratification '", stratification.name, "' could not be found for source '", source.name, "' and ontology '", ont.name, "'"))
            
            # Subset by age
            ont.ages = dimnames(data.this.ont)$age
            parsed.ages = parse.age.strata.names(ont.ages)
            ages.within.limit = ont.ages[parsed.ages$upper <= age.upper.limit & parsed.ages$lower >= age.lower.limit]
            subset.by.ages.arguments = lapply(names(dim(data.this.ont)), function(d) {
                if (d == 'age') ages.within.limit
                else 1:dim(data.this.ont)[[d]]
            })
            # browser()
            data.from.locs.only = do.call('[', c(list(data.this.ont), subset.by.ages.arguments, list(drop=F)))
            url.from.locs.only = do.call('[', c(list(url.this.ont), subset.by.ages.arguments, list(drop=F)))
            details.from.locs.only = do.call('[', c(list(details.this.ont), subset.by.ages.arguments, list(drop=F)))
            
            for (location in locations) {
                # print(location)
                # loctime = Sys.time()
                if (!is.null(contained.geographic.type)) contained.locations = locations::get.contained.locations(location, sub.type =  contained.geographic.type)
                else contained.locations = location
                if (length(contained.locations)==0) next
                # loctime = Sys.time()-loctime
                # other.time = Sys.time()
                from.locations.present = intersect(contained.locations, dimnames(data.from.locs.only)$location)
                if (!setequal(from.locations.present, contained.locations)) next # print warning?
                
                subset.by.locs.arguments = lapply(names(dim(data.from.locs.only)), function(d) {
                    if (d == 'location') from.locations.present
                    else 1:dim(data.from.locs.only)[[d]]
                })
                # browser()
                data.from.this.location.only = do.call('[', c(list(data.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                url.from.this.location.only = do.call('[', c(list(url.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                details.from.this.location.only = do.call('[', c(list(details.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                
                # If no data found, must skip or risk errors below
                if (all(is.na(data.from.this.location.only))) next
                
                # Convert url and details to their character forms
                url.from.this.location.only = census.manager$unhash.url(url.from.this.location.only)
                details.from.this.location.only = census.manager$unhash.details(details.from.this.location.only)
                
                # Details and URL should be the same for all data, but check just in case they aren't
                url = url.from.this.location.only[[1]]
                details = details.from.this.location.only[[1]]
                if (any(sapply(details.from.this.location.only, function(x) {!identical(x, details) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                if (any(sapply(url.from.this.location.only, function(x) {!identical(x, url) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))
                
                ## -- TOTALS -- #
                aggregated.totals.data = apply(data.from.this.location.only, 'year', sum, na.rm=T)
                post.agg.dimnames = list(year=names(aggregated.totals.data))
                aggregated.totals.data = array(aggregated.totals.data, sapply(post.agg.dimnames, length), post.agg.dimnames)
                data.manager$put(data = aggregated.totals.data,
                                 outcome = put.outcome.name,
                                 source = source.name,
                                 ontology.name = ont.name,
                                 dimension.values = list(location = location),
                                 url = url,
                                 details = details)
                has.put.totals.data.for.locations[[location]] = T
                
                ## -- STRATIFIED -- ##
                for (stratification in put.stratifications) {
                    # print(paste0("on stratification: ", stratification, " at time ", Sys.time()))
                    # Hand-aggregate the stratified data to each stratification
                    margin.of.aggregation = c('year', stratification)
                    aggregated.data = apply(data.from.this.location.only, margin.of.aggregation, sum, na.rm=T)
                    data.manager$put(data = aggregated.data,
                                     outcome = put.outcome.name,
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                    has.put.stratified.data.for.locations[[location]] = T
                    
                }
                # other.time = Sys.time()-other.time
                # print(paste0("location: ", location, " loctime: ", loctime, " othertime: ", other.time))
            }
        }
    }
    # print warning for locations we skipped for some reason
    skipped.totals.locations = names(locations)[!has.put.totals.data.for.locations]
    if (length(skipped.totals.locations)>0)
        warning(paste0("putting totals data for the following location(s) failed: "), paste0(skipped.totals.locations, collapse=", "))
    skipped.stratified.locations = names(locations)[!has.put.stratified.data.for.locations]
    if (length(skipped.stratified.locations)>0 && length(put.stratifications)>0)
        warning(paste0("putting totals data for the following location(s) failed: "), paste0(skipped.stratified.locations, collapse=", "))
}