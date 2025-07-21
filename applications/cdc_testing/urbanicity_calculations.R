
# From County-level Urban and Rural information for the 2020 Census


urbanicity.data = read.csv(file = 'applications/cdc_testing/2020_UA_COUNTY.csv', stringsAsFactors = F)
ALL.COUNTY.URBANICITY = as.numeric(gsub("%", '', urbanicity.data[,'POPPCT_URB'])) / 100
names(ALL.COUNTY.URBANICITY) = paste0(
    formatC(urbanicity.data[,1], width = 2, format = 'd', flag = "0"), 
    formatC(urbanicity.data[,2], width = 3, format = 'd', flag = "0")
)


get.urbanicity.metric <- function(locations,
                                  outcome,
                                  years,
                                  data.manager = SURVEILLANCE.MANAGER)
{
    sapply(locations, function(loc){
        
        counties = get.contained.locations(loc, 'county')
        
        n = data.manager$pull(outcome = outcome,
                                  dimension.values = list(year=as.character(years),
                                                          location=counties),
                              keep.dimensions = 'location')
        
        if (is.null(n))
            stop(paste0("There were no '", outcome, "' data for the counties in '", loc, "' in year(s) ",
                        paste0(years, collapse=', ')))
            
        n = apply(n, 'location', mean, na.rm=T)
        
        sum(n * ALL.COUNTY.URBANICITY[names(n)], na.rm=T) / sum(n, na.rm=T)
    })
}

CDC.TESTING.LOCATIONS.URBANICITY = get.urbanicity.metric(locations = CDC.TESTING.LOCATIONS, years=2021, outcome='diagnosed.prevalence')
#CDC.TESTING.LOCATIONS.URBANICITY = get.urbanicity.metric(locations = CDC.TESTING.LOCATIONS, years=2021, outcome='diagnoses')
sort(CDC.TESTING.LOCATIONS.URBANICITY)



#@ Ruchita - example for how I'm getting the correlation
if (1==2)
{
    #@ Ruchita - load your results
    
    YEARS = as.character(2025:2030)
    INT.CODES = c('cdct.end','cdct.pintr','cdct.bintr')
    
    abs.inc.noint = apply(total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum)
    
    abs.excess.inc = apply(total.results[YEARS,,'incidence',,INT.CODES] - as.numeric(total.results[YEARS,,'incidence',,'noint']), c('sim', 'location', 'intervention'), sum)
    rel.excess.inc = abs.excess.inc / as.numeric(apply(total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum))
    rel.excess.inc.means = apply(rel.excess.inc, c('location','intervention'), mean)
    
    rel.increase = rel.excess.inc.means[,1][CDC.TESTING.LOCATIONS]

    qplot(CDC.TESTING.LOCATIONS.URBANICITY, rel.increase)
    cor(CDC.TESTING.LOCATIONS.URBANICITY, rel.increase, method='spearman')
    
}