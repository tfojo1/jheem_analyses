
if (1==2)
{
    census.totals = read.census.totals()

    source('../code/baltimore.R')
    source('../code/data_managers/locale_mappings.R')
    load('../code/cached/ALL.DATA.MANAGERS.Rdata')
    baltimore.totals = get.census.totals(census.totals, BALTIMORE.MSA.COUNTIES)

    baltimore.check = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=BALTIMORE.MSA.COUNTIES,
                                      aggregate.counties = T, aggregate.ages = T, aggregate.races = T, aggregate.sexes = T)

    range(baltimore.check - baltimore.totals[names(baltimore.check),])

    format(cbind(baltimore.check, baltimore.totals[names(baltimore.check),]), big.mark=',')
}

read.census.totals <- function(dir='../data2/Census/county_totals_sans_kids/')
{
    filenames = list.files(dir, full.names = T)

    df = NULL
    for (filename in filenames)
    {
        print(paste0("Reading ", filename))
        df = rbind(df,
                   read.table(filename, sep='\t', stringsAsFactors = F, header = T))
    }

    df$Population[df$Population == 'Missing'] = NA
    df$Population = as.numeric(df$Population)

    years = unique(as.character(df$Yearly.July.1st.Estimates))
    locations = unique(as.character(df$County.Code))

    dim.names = list(year=years, location=locations)
    rv = list(data=array(as.numeric(NA), dim=sapply(dim.names, length), dimnames=dim.names),
              years=years,
              locations=locations)

    for (i in 1:dim(df)[1])
        rv$data[as.character(df$Yearly.July.1st.Estimates[i]), as.character(df$County.Code[i])] = df$Population[i]

    rv
}

get.census.totals <- function(census.totals, location, years=census.totals$years,
                              collapse.counties=T, flatten.single.dim.array=F,
                              interpolate.missing.years=c('all','none','when.some.years.not.missing')[1], 
                                    #^ a value of TRUE is interpreted as 'all'
                                    # a value of FALSE is interpreted as 'none'
                              throw.error.if.missing.years=F)
{
    collapsed.name = NULL
    missing.locations = setdiff(location, census.totals$locations)
    if (length(missing.locations)>0)
    {
        location.msa.names = msa.names(location, use.division.names = T)
        if (all(!is.na(location.msa.names)))
        {
            if (length(unique(location))==1)
                collapsed.name = location

            location = counties.for.msa.or.division(location)
            missing.locations=character()
        }
    }

    location = unique(location)

    if (length(missing.locations)>0)
        stop(paste0("The following locations are not present in the census totals data:",
                    paste0(location, collapse=', ')))

    years.to.pull = years
    years = sort(union(census.totals$years, years))
    
    rv = t(sapply(as.character(years), function(year){
        if (any(as.numeric(year)==census.totals$years))
            values = census.totals$data[year, location]
        else
            values = rep(NaN, length(location))
    #    {
     #       nearest.year = census.totals$years[order(abs(as.numeric(census.totals$years)-as.numeric(year)))][1]
      #      values = census.totals$data[nearest.year, location]
       # }
        
        values
    }))
    
    if (length(location)==1)
    {
        dim(rv) = c(year=length(years), location=1)
        dimnames(rv) = list(year=as.character(years), location=location)
    }
    
    if (any(is.na(rv[as.character(years.to.pull),])))
    {
        if ((is.logical(interpolate.missing.years) && interpolate.missing.years) ||
            interpolate.missing.years=='all')
        {
            dim.names = dimnames(rv)
            dim.rv = dim(rv)
            
            rv = apply(rv, 2, function(values){
                if (all(is.na(values)))
                    stop("No census total values")
                else if (any(is.na(values)))
                    interpolate.parameters(values = values[!is.na(values)],
                                           value.times = as.numeric(years[!is.na(values)]),
                                           desired.times = as.numeric(years),
                                           return.list = F)
                else
                    values
            })
            
            dim(rv) = dim.rv
            dimnames(rv) = dim.names
        }
        else if ((is.logical(interpolate.missing.years) && !interpolate.missing.years) ||
                 interpolate.missing.years=='none')
        {
            if (throw.error.if.missing.years)
                stop(paste0("Missing data for the requested locations"))
        }
        else if (interpolate.missing.years=='when.some.years.not.missing')#interpolate missing years == when.some.years.not.missing
        {
            all.missing.by.year = apply(is.na(rv), 1, all)
            any.missing.by.year = apply(is.na(rv), 1, any)
            
            if (any(any.missing.by.year & !all.missing.by.year))
            {
                to.interpolate.mask = !all.missing.by.year
                
                rv[to.interpolate.mask,] = apply(rv, 2, function(values){
                    if (any(is.na(values[to.interpolate.mask])))
                    {
                        interpolate.parameters(values = values[to.interpolate.mask & !is.na(values)],
                                               value.times = as.numeric(years[to.interpolate.mask & !is.na(values)]),
                                               desired.times = as.numeric(years[to.interpolate.mask]),
                                               return.list = F)
                    }
                    else
                        values[to.interpolate.mask]
                })
            }
        }
        else
            stop("Invalid value for interpolate.missing.years")
    }
    
    dim.names = list(year=years, location=location)
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    # pull out just the years we're actually interested in
    rv = rv[as.character(years.to.pull),]
    dim.names = list(year=years.to.pull, location=location)
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    if (length(location)==1)
    {
        if (flatten.single.dim.array)
            rv = rv[,1]
    }
    else if (collapse.counties)
    {
        rv = rowSums(rv)
        if (!flatten.single.dim.array)
        {
            dim.names = list(year=years.to.pull, location=collapsed.name)
            dim(rv) = c(year=length(years.to.pull), location=1)
            dimnames(rv) = dim.names
        }
    }

    rv
}
