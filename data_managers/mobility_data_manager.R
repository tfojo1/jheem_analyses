
if (1==2)
{
    MOBILITY.DATA.MANAGER = read.mobility.data.manager()
    save(MOBILITY.DATA.MANAGER, file='cached/MOBILITY.DATA.MANAGER.Rdata')
}


library(data.table)

MOBILITY.MAPPING = c(
    workplace="workplaces_percent_change_from_baseline",
    grocery.pharmacy="grocery_and_pharmacy_percent_change_from_baseline",
    transit="transit_stations_percent_change_from_baseline",
    retail="retail_and_recreation_percent_change_from_baseline",
    parks="parks_percent_change_from_baseline",
    residential="residential_percent_change_from_baseline"
)

ALL.MOBILITY.TYPES = names(MOBILITY.MAPPING)

NON.PARK.MOBILITY.TYPES = setdiff(ALL.MOBILITY.TYPES, 'parks')

read.mobility.data.manager <- function(files=c('cleaned_data/covid/2020_US_Region_Mobility_Report.csv',
                                               'cleaned_data/covid/2021_US_Region_Mobility_Report.csv'),
                                       verbose=T)
{
    #-- LOAD FILES --#
    if (verbose)
        print("Loading files...")
    dfs = lapply(files, read.csv, stringsAsFactors=F)
    
    df = as.data.frame(rbindlist(dfs))
    df = df[!is.na(df$census_fips_code),]
    
    #-- SET UP ARRAY STRUCTURE --##
    dates = unique(df$date)
    fips = unique(df$census_fips_code)
    
    dim.names = list(
        location=as.character(fips),
        date=dates,
        type=ALL.MOBILITY.TYPES
    )
    arr = array(NaN, dim=sapply(dim.names, length), dimnames=dim.names)

    #-- PULL FROM DATA FRAME TO ARRAY --#
        
    if (verbose)
        print(paste0("Reading ", format(dim(df)[1], big.mark = ','), " rows from the data"))
              
    for (i in 1:dim(df)[1])
    {
        arr[as.character(df$census_fips_code[i]), df$date[i], ALL.MOBILITY.TYPES] = 
            as.numeric(df[i, MOBILITY.MAPPING]) / 100
    }
    
    #-- PARSE METADATA --#
    
    rv = list(
        data = arr
    )

    rv$dates = as.Date(dates)
    rv$years = as.numeric(gsub('(\\d\\d\\d\\d)-\\d\\d-\\d\\d', '\\1', dates))
    rv$months = as.numeric(gsub('\\d\\d\\d\\d-(\\d\\d)-\\d\\d', '\\1', dates))
    rv$days = as.numeric(gsub('\\d\\d\\d\\d-\\d\\d-(\\d\\d)', '\\1', dates))
        
    this.year = as.Date(paste0(rv$year, '-01-01'))
    next.year = as.Date(paste0(rv$year+1, '-01-01'))
    rv$times = rv$years + as.numeric(difftime(rv$dates, this.year, units='days')) / 
        as.numeric(difftime(next.year, this.year, units='days'))
    
    rv$min.year = min(rv$years)
    rv$min.month = min(rv$months[rv$years==rv$min.year])
        
    rv$max.year = max(rv$years)
    rv$max.month = max(rv$months[rv$years==rv$max.year])
    
    #-- RETURN --#   
    rv
}


get.mobility.data.by.month <- function(mdm,
                                       locations,
                                       types=ALL.MOBILITY.TYPES,
                                       from.year=2020,
                                       from.month=3,
                                       to.year=max(mdm$years),
                                       to.month=max(mdm$months[mdm$years==to.year]),
                                       census.totals=ALL.DATA.MANAGERS$census.totals)
{
    #-- Check years --#
    if (from.year < min(mdm$years) || from.month < min(mdm$months[mdm$years==from.year]))
        stop(paste0("No mobility data are available prior to ", mdm$months[1], "-", mdm$years[1]))
    if (to.year > max(mdm$years) || to.month > max(mdm$months[mdm$years==to.year]))
        stop(paste0("No mobility data are available after ", mdm$months[length(mdm$months)], "-", mdm$years[length(mdm$years)]))
    
    #-- Pull Populations for Weighting --#
    unique.years = from.year:to.year
    
    populations = get.census.totals(census.totals, location=locations, years=unique.years, collapse.counties = F)
    
    locations = dimnames(populations)[[2]]
    
    #-- Set up years/months --#
    
    if (from.year==to.year)
    {
        months = from.month:to.month
        years = rep(from.year, length(months))
    }
    else
    {
        months = c(
            from.month:12,
            rep(1:12, length(unique.years)-2),
            1:to.month
        )
        
        years = c(
            rep(from.year,12-from.month+1),
            rep(unique.years[-c(1,length(unique.years))], each=12),
            rep(to.year, to.month)
        )
    }
    
    rv = sapply(1:length(months), function(i){
        mask = mdm$months==months[i] & mdm$years==years[i]
        data.for.time = mdm$data[locations,mask,types]
        dim(data.for.time) = c(length(locations), sum(mask), length(types))
        mean.data.for.time = apply(data.for.time, c(1,3), mean, na.rm=T)
        
        pops = populations[as.character(years[i]),]
        colSums(mean.data.for.time * pops/sum(pops), na.rm=T)
    })
    
    dim.names = list(
        type=types,
        time=paste0(months,"-",years)
    )
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}