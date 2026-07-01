unzip('../jheem_analyses/applications/cdc_testing/2020_UA_COUNTY.csv.zip',exdir = '../jheem_analyses/applications/cdc_testing/' )
urbanicity.data = read.csv(file = '../jheem_analyses/applications/cdc_testing/2020_UA_COUNTY.csv', stringsAsFactors = F)
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