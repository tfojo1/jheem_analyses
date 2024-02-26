put.msa.data.strict = function(census.outcome.name = 'population',
                               put.outcome.name = 'adult.population',
                               locations,
                               contained.geographic.type = 'county', # if this is NULL, it won't get any subtype, just stay at the main geographic level
                               fully.stratified.dimensions = c('year', 'age', 'race', 'ethnicity', 'sex'),
                               put.stratifications = list('age', c('race', 'ethnicity'), 'sex', c('age', 'race', 'ethnicity'), c('age', 'sex'), c('race', 'ethnicity', 'sex')),
                               age.lower.limit = 13,
                               age.penultimate.upper = 84,
                               age.upper.limit.name = '85+',
                               data.manager,
                               census.manager)
{
    # browser()
    
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
  
  census.adult.ages = paste0(c(as.character(age.lower.limit:age.penultimate.upper), age.upper.limit.name), " years")
    
    for (location in locations) {

        error.prefix = paste0("Error putting '", put.outcome.name, "' data for '", location, "':")
        
        if (!is.null(contained.geographic.type)) contained.locations = locations::get.contained.locations(location, sub.type = contained.geographic.type)
        else contained.locations = location
        
        for (source.name in names(census.manager$source.info))
        {
            source.ontology.names = names(census.manager$data[[census.outcome.name]][['estimate']][[source.name]])
            
            for (ont.name in source.ontology.names) {

                census.data.stratified = census.manager$pull(outcome = census.outcome.name,
                                                             keep.dimensions = fully.stratified.dimensions,
                                                             dimension.values = list(location = contained.locations, age = census.adult.ages),
                                                             from.ontology.names = ont.name,
                                                             sources = source.name,
                                                             append.attributes = c('details', 'url'),
                                                             debug = F)

                if (!is.null(census.data.stratified)) {
                    dimnames.without.source = dimnames(census.data.stratified)[names(dim(census.data.stratified)) != 'source']
                    dim(census.data.stratified) = sapply(dimnames.without.source, length)
                    dimnames(census.data.stratified) = dimnames.without.source
                    
                    # Details and URL should be the same for all data, but check just in case they aren't
                    details = attr(census.data.stratified, 'details')[[1]]
                    url = attr(census.data.stratified, 'url')[[1]]
                    
                    if (any(sapply(attr(census.data.stratified, 'details'), function(x) {!identical(x, details) && !is.null(x)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                    if (any(sapply(attr(census.data.stratified, 'url'), function(x) {!identical(x, url) && !is.null(x)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))

                    ## -- TOTALS -- ##
                    
                    # Hand-aggregate the stratified data to total
                    aggregated.totals.data = apply(census.data.stratified, 'year', sum, na.rm=T)
                    dimnames.because.r.apply.is.annoying = list(year=names(aggregated.totals.data))
                    aggregated.totals.data = array(aggregated.totals.data, sapply(dimnames.because.r.apply.is.annoying, length), dimnames.because.r.apply.is.annoying)
                    data.manager$put(data = aggregated.totals.data,
                                     outcome = put.outcome.name,
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                    
                    ## -- STRATIFIED -- ##
                    for (stratification in put.stratifications) {
                        
                        # Hand-aggregate the stratified data to each stratification
                        margin.of.aggregation = c('year', stratification)
                        aggregated.data = apply(census.data.stratified, margin.of.aggregation, sum, na.rm=T)
                        
                        data.manager$put(data = aggregated.data,
                                         outcome = put.outcome.name,
                                         source = source.name,
                                         ontology.name = ont.name,
                                         dimension.values = list(location=location),
                                         url = url,
                                         details = details)
                    }  
                }
            }
        }
    }
}

# census.manager = load.data.manager('../jheem_analyses/cached/smaller.census.manager.rdata')

# practice.data.manager = create.data.manager('practice.data.manager', 'Data manager to test msa population putting')
# practice.data.manager$register.outcome('adult.population', metadata = create.outcome.metadata(scale = 'non.negative.number', display.name = 'Adult Population', axis.name = 'Adult Population (n)', units = 'persons', description = 'Population Ages 13+'))
# test = put.population.data(locations = c('C.12580'), # MSAS.OF.INTEREST
#                            data.manager = practice.data.manager, # surveillance.manager
#                            census.manager = CENSUS.MANAGER)



##### -- TO USE -- #####
# SEE DEFAULTS IN FUNCTION DEFINITION ABOVE.
# 'fully.stratified.dimensions' SHOULD INCLUDE EVERY DIMENSION EXCEPT LOCATION
# 'RACE' AND 'ETHNICITY' ARE PUT AS A TWO-WAY STRATIFICATION SINCE WE WILL NEED
# THEM BOTH SIMULTANEOUSLY TO MAP TO OTHER ONTOLOGIES' 'RACE' DIMENSION
# REACH OUT TO ANDREW WITH ANY QUESTIONS

