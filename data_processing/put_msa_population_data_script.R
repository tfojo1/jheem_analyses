put.population.data = function(locations,
                               contained.geographic.type = 'county',
                               fully.stratified.dimensions = c('year', 'age', 'race', 'ethnicity', 'sex'),
                               put.stratifications = list('age', c('race', 'ethnicity'), 'sex'),
                               data.manager,
                               census.manager)
{
    
    # register the source(s), ontologies, and/or outcome if necessary
    if (!('population' %in% data.manager$outcomes))
        data.manager$register.outcome(outcome = 'population',
                                      metadata = census.manager$outcome.info$population$metadata)
    for (source.name in names(census.manager$source.info))
        if (!(source.name %in% names(data.manager$source.info)))
            data.manager$register.source(source = census.manager$source.info[[source.name]]$source,
                                         full.name = census.manager$source.info[[source.name]]$full.name,
                                         short.name = census.manager$source.info[[source.name]]$short.name)

    for (ont.name in census.manager$ontology.names)
        if (!(ont.name %in% data.manager$ontology.names))
            data.manager$register.ontology(ont.name, census.manager$ontologies[[ont.name]])
    
    for (location in locations) {
        
        error.prefix = paste0("Error putting population data for '", location, "':")
        
        contained.locations = locations::get.contained.locations(location, sub.type = contained.geographic.type)
        
        for (source.name in names(census.manager$source.info))
        {
            source.ontology.names = names(census.manager$data$population[[source.name]])
            
            for (ont.name in source.ontology.names) {
                
                ## -- TOTALS -- ##
                census.data.totals = census.manager$pull(outcome = 'population',
                                                         keep.dimensions = 'year',
                                                         dimension.values = list(location = contained.locations),
                                                         from.ontology.names = ont.name,
                                                         sources = source.name,
                                                         append.attributes = c('details', 'url'))
                
                if (!is.null(census.data.totals)) {
                    dimnames.without.source = dimnames(census.data.totals)[names(dim(census.data.totals)) != 'source']
                    dim(census.data.totals) = sapply(dimnames.without.source, length)
                    dimnames(census.data.totals) = dimnames.without.source
                    
                    # Details and URL should be the same for all data, but check just in case they aren't
                    details = attr(census.data.totals, 'details')[[1]]
                    url = attr(census.data.totals, 'url')[[1]]
                    
                    if (any(sapply(attr(census.data.totals, 'details'), function(x) {!identical(x, details)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                    if (any(sapply(attr(census.data.totals, 'url'), function(x) {!identical(x, url)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))
                    
                    data.manager$put(data = census.data.totals,
                                     outcome = 'population',
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                }

                ## -- STRATIFIED -- ##
                census.data.stratified = census.manager$pull(outcome = 'population',
                                                             keep.dimensions = fully.stratified.dimensions,
                                                             dimension.values = list(location = contained.locations),
                                                             from.ontology.names = ont.name,
                                                             sources = source.name,
                                                             append.attributes = c('details', 'url'))

                if (!is.null(census.data.stratified)) {
                    dimnames.without.source = dimnames(census.data.stratified)[names(dim(census.data.stratified)) != 'source']
                    dim(census.data.stratified) = sapply(dimnames.without.source, length)
                    dimnames(census.data.stratified) = dimnames.without.source
                    
                    # Details and URL should be the same for all data, but check just in case they aren't
                    details = attr(census.data.stratified, 'details')[[1]]
                    url = attr(census.data.stratified, 'url')[[1]]
                    
                    if (any(sapply(attr(census.data.stratified, 'details'), function(x) {!identical(x, details)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'details'"))
                    if (any(sapply(attr(census.data.stratified, 'url'), function(x) {!identical(x, url)})))
                        stop(paste0(error.prefix, "'", source.name, "' data do not all have the same 'url'"))
                    
                    for (stratification in put.stratifications) {
                        
                        # Hand-aggregate the stratified data to each stratification
                        margin.of.aggregation = c('year', stratification)
                        aggregated.data = apply(census.data.stratified, margin.of.aggregation, sum)
                        
                        data.manager$put(data = aggregated.data,
                                         outcome = 'population',
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

practice.data.manager = create.data.manager('practice.data.manager', 'Data manager to test msa population putting')

test = put.population.data(locations = c('C.12580'), # MSAS.OF.INTEREST
                           data.manager = practice.data.manager, # surveillance.manager
                           census.manager = census.manager)

##### -- TO USE -- #####
# SEE DEFAULTS IN FUNCTION DEFINITION ABOVE.
# 'fully.stratified.dimensions' SHOULD INCLUDE EVERY DIMENSION EXCEPT LOCATION
# 'RACE' AND 'ETHNICITY' ARE PUT AS A TWO-WAY STRATIFICATION SINCE WE WILL NEED
# THEM BOTH SIMULTANEOUSLY TO MAP TO OTHER ONTOLOGIES' 'RACE' DIMENSION
# REACH OUT TO ANDREW WITH ANY QUESTIONS

