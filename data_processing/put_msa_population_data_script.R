put.population.data = function(locations,
                               contained.geographic.type = 'county',
                               fully.stratified.dimensions = c('year', 'age', 'race', 'ethnicity', 'sex'),
                               put.stratifications = list('age', c('race', 'ethnicity'), 'sex', c('age', 'race', 'ethnicity'), c('age', 'sex'), c('race', 'ethnicity', 'sex')),
                               data.manager,
                               census.manager)
{
    # browser()
    # register the source(s), ontologies, and/or outcome if necessary
    if (!('adult.population' %in% data.manager$outcomes))
      stop(paste0("Error: outcome 'adult.population' has not yet been registered"))
    for (source.name in names(census.manager$source.info))
        if (!(source.name %in% names(data.manager$source.info)))
            data.manager$register.source(source = census.manager$source.info[[source.name]]$source,
                                         full.name = census.manager$source.info[[source.name]]$full.name,
                                         short.name = census.manager$source.info[[source.name]]$short.name)

    for (ont.name in census.manager$ontology.names)
        if (!(ont.name %in% data.manager$ontology.names))
            data.manager$register.ontology(ont.name, census.manager$ontologies[[ont.name]])
  
  age.lower.limit = 13
  age.penultimate.upper = 84
  age.upper.limit.name = '85+'
  census.adult.ages = paste0(c(as.character(age.lower.limit:age.penultimate.upper), age.upper.limit.name), " years")
    
    for (location in locations) {
        
        error.prefix = paste0("Error putting population data for '", location, "':")
        
        contained.locations = locations::get.contained.locations(location, sub.type = contained.geographic.type)
        
        for (source.name in names(census.manager$source.info))
        {
            source.ontology.names = names(census.manager$data$population[[source.name]])
            
            for (ont.name in source.ontology.names) {
              
                census.data.stratified = census.manager$pull(outcome = 'population',
                                                             keep.dimensions = fully.stratified.dimensions,
                                                             dimension.values = list(location = contained.locations, age = census.adult.ages),
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

                    ## -- TOTALS -- ##
                    
                    # Hand-aggregate the stratified data to total
                    aggregated.totals.data = apply(census.data.stratified, 'year', sum)
                    dimnames.because.r.apply.is.annoying = list(year=names(aggregated.totals.data))
                    aggregated.totals.data = array(aggregated.totals.data, sapply(dimnames.because.r.apply.is.annoying, length), dimnames.because.r.apply.is.annoying)
                    data.manager$put(data = aggregated.totals.data,
                                     outcome = 'adult.population',
                                     source = source.name,
                                     ontology.name = ont.name,
                                     dimension.values = list(location=location),
                                     url = url,
                                     details = details)
                    
                    ## -- STRATIFIED -- ##
                    for (stratification in put.stratifications) {
                        
                        # Hand-aggregate the stratified data to each stratification
                        margin.of.aggregation = c('year', stratification)
                        aggregated.data = apply(census.data.stratified, margin.of.aggregation, sum)
                        
                        data.manager$put(data = aggregated.data,
                                         outcome = 'adult.population',
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

census.manager = load.data.manager('../../cached/smaller.census.manager.rdata') 

# practice.data.manager = create.data.manager('practice.data.manager', 'Data manager to test msa population putting')
# practice.data.manager$register.outcome('adult.population', metadata = create.outcome.metadata(scale = 'non.negative.number', display.name = 'Adult Population', axis.name = 'Adult Population (n)', units = 'persons', description = 'Population Ages 13+'))
# test = put.population.data(locations = c('C.12580'), # MSAS.OF.INTEREST
#                            data.manager = practice.data.manager, # surveillance.manager
#                            census.manager = census.manager)



##### -- TO USE -- #####
# SEE DEFAULTS IN FUNCTION DEFINITION ABOVE.
# 'fully.stratified.dimensions' SHOULD INCLUDE EVERY DIMENSION EXCEPT LOCATION
# 'RACE' AND 'ETHNICITY' ARE PUT AS A TWO-WAY STRATIFICATION SINCE WE WILL NEED
# THEM BOTH SIMULTANEOUSLY TO MAP TO OTHER ONTOLOGIES' 'RACE' DIMENSION
# REACH OUT TO ANDREW WITH ANY QUESTIONS

