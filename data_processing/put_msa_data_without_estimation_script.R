put.msa.data.strict = function(census.outcome.name = 'population',
                               put.outcome.name = 'adult.population',
                               locations,
                               contained.geographic.type = 'county', # if this is NULL, it won't get any subtype, just stay at the main geographic level
                               stratification.name = "year__location__age__race__ethnicity__sex",
                               put.stratifications = list('age','race', 'ethnicity', c('race', 'ethnicity'), 'sex', c('age', 'race', 'ethnicity'), c('age', 'sex'), c('race', 'ethnicity', 'sex')), # race and ethnicity are individually needed by the proportion tested likelihood obs-n calculation
                               age.lower.limit = 13,
                               age.upper.limit = Inf,
                               data.manager,
                               census.manager)
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
        if (!(ont.name %in% data.manager$ontology.names))
            data.manager$register.ontology(ont.name, census.manager$ontologies[[ont.name]])
    
    all.data = census.manager$data[[census.outcome.name]][['estimate']]
    all.url = census.manager$url[[census.outcome.name]][['estimate']]
    all.details = census.manager$details[[census.outcome.name]][['estimate']]
    
    error.prefix = paste0("Error putting '", put.outcome.name, "' data:")
    
    for (source.name in names(all.data))
    {
        data.this.source = all.data[[source.name]]
        url.this.source = all.url[[source.name]]
        details.this.source = all.details[[source.name]]
        
        for (ont.name in names(data.this.source))
        {
            data.this.ont = data.this.source[[ont.name]][[stratification.name]]
            url.this.ont = url.this.source[[ont.name]][[stratification.name]]
            details.this.ont = details.this.source[[ont.name]][[stratification.name]]
            
            if (is.null(data.this.ont) || is.null(url.this.ont) || is.null(details.this.ont))
                stop(paste0(error.prefix, "data with stratification '", stratification.name, "' could not be found for source '", source.name, "' and ontology '", ont.name, "'"))
           
            # Subset by age
            ont.ages = dimnames(data.this.ont)$age
            parsed.ages = parse.age.strata.names(ont.ages)
            ages.within.limit = ont.ages[parsed.ages$upper <= age.upper.limit & parsed.ages$lower >= age.lower.limit]
            subset.by.ages.arguments = lapply(names(dim(data.this.ont)), function(d) {
                if (d == 'age') ages.within.limit
                else 1:dim(data.this.ont)[[d]]
            })
            
            data.from.locs.only = do.call('[', c(list(data.this.ont), subset.by.ages.arguments, list(drop=F)))
            url.from.locs.only = do.call('[', c(list(url.this.ont), subset.by.ages.arguments, list(drop=F)))
            details.from.locs.only = do.call('[', c(list(details.this.ont), subset.by.ages.arguments, list(drop=F)))
            
            for (location in locations) {
                # print(location)
                # loctime = Sys.time()
                if (!is.null(contained.geographic.type)) contained.locations = locations::get.overlapping.locations(location, type =  contained.geographic.type)
                else contained.locations = location
                # loctime = Sys.time()-loctime
                # other.time = Sys.time()
                from.locations.present = intersect(contained.locations, dimnames(data.from.locs.only)$location)
                if (!setequal(from.locations.present, contained.locations)) next # print warning?
                
                subset.by.locs.arguments = lapply(names(dim(data.from.locs.only)), function(d) {
                    if (d == 'location') from.locations.present
                    else 1:dim(data.from.locs.only)[[d]]
                })
                # browser()
                data.from.locs.only = do.call('[', c(list(data.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                url.from.locs.only = do.call('[', c(list(url.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                details.from.locs.only = do.call('[', c(list(details.from.locs.only), subset.by.locs.arguments, list(drop=F)))
                
                # Convert url and details to their character forms
                url.from.locs.only = census.manager$unhash.url(url.from.locs.only)
                details.from.locs.only = census.manager$unhash.details(details.from.locs.only)
                
                # Details and URL should be the same for all data, but check just in case they aren't
                url = url.from.locs.only[[1]]
                details = details.from.locs.only[[1]]
                if (any(sapply(details.from.locs.only, function(x) {!identical(x, details) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                if (any(sapply(url.from.locs.only, function(x) {!identical(x, url) && !is.null(x)})))
                    stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))
                
                ## -- TOTALS -- #
                aggregated.totals.data = apply(data.from.locs.only, 'year', sum, na.rm=T)
                post.agg.dimnames = list(year=names(aggregated.totals.data))
                aggregated.totals.data = array(aggregated.totals.data, sapply(post.agg.dimnames, length), post.agg.dimnames)
                data.manager$put(data = aggregated.totals.data,
                                 outcome = put.outcome.name,
                                 source = source.name,
                                 ontology.name = ont.name,
                                 dimension.values = list(location = location),
                                 url = url,
                                 details = details)
                
                ## -- STRATIFIED -- ##
                for (stratification in put.stratifications) {
                    # print(paste0("on stratification: ", stratification, " at time ", Sys.time()))
                    # Hand-aggregate the stratified data to each stratification
                    margin.of.aggregation = c('year', stratification)
                    aggregated.data = apply(data.from.locs.only, margin.of.aggregation, sum, na.rm=T)
                    
                    data.manager$put(data = aggregated.data,
                                     outcome = put.outcome.name,
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                    
                }
                # other.time = Sys.time()-other.time
                # print(paste0("location: ", location, " loctime: ", loctime, " othertime: ", other.time))
            }
        }
    }
}

# Need to source until it's in the package.
source('../jheem2/R/HELPERS_age_year_helpers.R')

    
    