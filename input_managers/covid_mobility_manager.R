
if (1==2)
{
    COVID.MOBILITY.DATA = read.monthly.county.mobility.data()
    save(COVID.MOBILITY.DATA, file='../jheem_analyses/cached/google_mobility_data.Rdata')
    
    # test
    get.covid.mobility.measure(mobility.data,
                               location = 'c.12580',
                               to.month = 3,
                               to.year = 2022,
                               census.manager = NULL)
    
    load('../jheem_analyses/cached/google_mobility_data.Rdata')
    
    x = get.covid.effect.expression(location = 'c.12580',
                                    effect.magnitude.parameter.names = c('black.suppression.effect','young.effect'),
                                    mobility.correlation.parameter.name = 'covid.effects.mobility.correlation',
                                    n.months = 25,
                                    census.manager = NULL)
    
    eval(x, list(black.suppression.effect=.6, young.effect=1.2, covid.effects.mobility.correlation=0.5))
}

read.monthly.county.mobility.data <- function(dir='../../data/raw/google_mobility_reports/')
{
    #-- Read the Files --#
    files = list.files(dir, full.names = T)
    
    df = NULL
    for (file in files)
    {
        one.df = read.csv(file)
        
        # Keep just the counties
        location.type = get.location.type(one.df$census_fips_code)
        mask = !is.na(location.type) & location.type=='COUNTY'
        one.df = one.df[mask,]
        
        df = rbind(df, one.df)
    }

    #-- Set up the dim names --#
    
    # Parse the month and year
    year.last = grepl("[0-9][0-9][0-9][0-9]$", df$date)
    df$year = as.character(NA)
    df$year[year.last] = substr(df$date[year.last], nchar(df$date[year.last])-3, nchar(df$date[year.last]))
    df$year[!year.last] = substr(df$date[!year.last], 1, 4)
    
    df$month = as.character(NA)
    df$month[!year.last] = substr(df$date[!year.last], 6, 7)
    one.digit.month.first = year.last & substr(df$date, 2,2) == '/'
    two.digit.month.first = year.last & !one.digit.month.first
    df$month[one.digit.month.first] = paste0(0, substr(df$date[one.digit.month.first], 1,1))
    df$month[two.digit.month.first] = substr(df$date[two.digit.month.first], 1, 2)
    
    df$time = paste0(df$year, "-", df$month)
    
    times = sort(unique(df$time))
    
    counties = unique(df$census_fips_code)
    
    type.suffix = '_percent_change_from_baseline'
    types = gsub(type.suffix, '', names(df)[grepl(type.suffix, names(df))])
    col.names = paste0(types, type.suffix)
    
    dim.names = list(
        type = types,
        time = times,
        location = counties
    )
    
    #-- Populate the RV --#
    rv = sapply(counties, function(loc){
        
        mask = df$census_fips_code==loc
        
        sapply(times, function(time){
            
            mask[mask] = df$time[mask]==time
            
            colMeans(df[mask, col.names], na.rm=T)/100
                
        })
    })
    
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    #-- Reorder --#
    rv = apply(rv, c('time','type','location'), function(x){x})
    
    #-- Return --#
    rv
}

get.covid.mobility.measure <- function(mobility.data,
                                       location,
                                       n.covid.months,
                                       types = NULL,
                                       census.manager)
{
    from.month = 3
    from.year = 2020
    
    to.month = 1+(3+n.covid.months-1)%%12
    to.year = floor(2020 + 3/12 + n.covid.months/12)

    if (is.null(types))
        types = setdiff(dimnames(mobility.data)$type, 'parks')
  
    counties = get.sub.locations(location, 'county', T)
    month.names = c(paste0('0', 1:9), as.character(10:12))
    times = unlist(sapply(from.year:to.year, function(year){
        
        if (year==from.year)
            paste0(year, "-", month.names[from.month:12])
        else if (year==to.year)
            paste0(year, "-", month.names[1:to.month])
        else
            paste0(year, "-", month.names)
    }))
    
    sub.data = mobility.data[times, types, counties, drop=F]
    proportions.data = apply(sub.data, 2:3, function(x){
        norm.to = x[2]
        x / norm.to
    })
    dimnames(proportions.data)[[1]] = dimnames(sub.data)[[1]]
    
    if (length(counties)>1)
    {

    }
    
    apply(proportions.data, 'time', mean, na.rm=T)
}

get.covid.taper <- function(n.covid.months = 25,
                            n.months.until.taper = 13)
{
    taper.over.n.months = n.covid.months - n.months.until.taper
    taper = c(rep(1, n.months.until.taper+1),
              (1 - (1:taper.over.n.months) / taper.over.n.months)
    )
    
    taper
}

get.covid.effect.expression <- function(effect.magnitude.parameter.names,
                                        mobility.correlation.parameter.name,
                                        mobility.name = 'mobility',
                                        effect.taper.name = 'taper')
{
    if (length(effect.magnitude.parameter.names)==0)
        stop(paste0("Cannot create COVID effect expression: 'effect.magnitude.parameter.names' must contain at least one element"))
  
    # This is what we're going for   
    #    expression(1 - (1-effect.magnitude) * (mobility.proportion * mobility.correlation + (1-mobility.correlation)) * taper)
    #    expression(1 - (1-effect.magnitude) * (1 + mobility.correlation * (mobility.proportion-1) ) * taper)
    
    ch.exp = paste0("1 - (1 - ",
                    paste0(effect.magnitude.parameter.names, collapse = " * "),
                    ") * (1 + ",
                    mobility.correlation.parameter.name,
                    " * (",
                    mobility.name,
                    " - 1)) * ",
                    effect.taper.name)

    exp = parse(text=ch.exp)
    exp
}

get.covid.start.time <- function()
{
    2020 + 2 / 12 # March 1, 2020
}

get.covid.end.time <- function(n.covid.months)
{
    2020 + (3 + n.covid.months) / 12
}

get.covid.effect.times <- function(n.covid.months=25)
{
    2020 + (2 + 0.5 + 0:n.covid.months) / 12
}

create.covid.foreground <- function(quantity.name,
                                    scale,
                                    n.covid.months,
                                    n.months.until.taper,
                                    age.effect.magnitude.parameter.names,
                                    age.target.populations,
                                    race.effect.magnitude.parameter.names,
                                    race.target.populations,
                                    mobility.correlation.parameter.name,
                                    mobility.data=COVID.MOBILITY.DATA)
{
    # Check arguments
    error.prefix = "Cannot create COVID foreground: "
    if (!is.list(age.effect.magnitude.parameter.names) || 
        length(age.effect.magnitude.parameter.names)==0)
          stop(past0(error.prefix,
                     "'age.effect.magnitude.parameter.names' must be a non-empty list"))
    if (any(!sapply(age.effect.magnitude.parameter.names, is.character) &
            !sapply(age.effect.magnitude.parameter.names, is.null)))
        stop(past0(error.prefix,
                   "The elements of 'age.effect.magnitude.parameter.names' must be either character vectors or NULL"))
    
    if (!is.list(age.target.populations))
        stop(past0(error.prefix,
                   "'age.target.populations' must be a list"))
    if (any(!sapply(age.target.populations, is, 'target.population')))
        stop(past0(error.prefix,
                   "The elements of 'age.target.populations' must be 'target.population' objects"))
    
    if (length(age.effect.magnitude.parameter.names) != length(age.target.populations))
        stop(paste0(error.prefix,
                    "'age.effect.magnitude.parameter.names' and 'age.target.populations' must have the same length"))
    
    if (!is.list(race.effect.magnitude.parameter.names))
        stop(past0(error.prefix,
                   "'race.effect.magnitude.parameter.names' must be a non-empty, named list"))
    if (is.null(names(race.effect.magnitude.parameter.names)))
        stop(past0(error.prefix,
                   "'race.effect.magnitude.parameter.names' must be a NAMED list"))
    if (any(!sapply(race.effect.magnitude.parameter.names, is.character) &
            !sapply(race.effect.magnitude.parameter.names, is.null)))
        stop(past0(error.prefix,
                   "The elements of 'race.effect.magnitude.parameter.names' must be either character vectors or NULL"))
    
    if (!is.list(race.target.populations))
        stop(past0(error.prefix,
                   "'race.target.populations' must be a list"))
    if (is.null(names(race.target.populations)))
      stop(past0(error.prefix,
                 "'race.target.populations' must be a NAMED list"))
    if (any(!sapply(race.target.populations, is, 'target.population')))
        stop(past0(error.prefix,
                   "The elements of 'race.target.populations' must be 'target.population' objects"))
    if (length(race.effect.magnitude.parameter.names) != length(race.target.populations))
        stop(paste0(error.prefix,
                    "'race.effect.magnitude.parameter.names' and 'race.target.populations' must have the same length"))
    if (!setequal(names(race.effect.magnitude.parameter.names),
                  names(race.target.populations)))
        stop(paste0(error.prefix,
                    "'race.effect.magnitude.parameter.names' and 'race.target.populations' must have the same names"))
  
    # Loop through all combos of age and race
    target.populations = list()
    effects = list()
    
    for (age.index in 1:length(age.effect.magnitude.parameter.names))
    {
        age.tpop = age.target.populations[[age.index]]
      
        for (race in names(race.effect.magnitude.parameter.names))
        {
            race.tpop = race.target.populations[[race]]
          
            target.populations = c(target.populations,
                                   list(intersect.target.populations(age.tpop, race.tpop)))
            
            effects = c(effects,
                        list(create.covid.intervention.effect(
                            quantity.name = quantity.name,
                            scale = scale,
                            n.covid.months = n.covid.months,
                            n.months.until.taper = n.months.until.taper,
                            effect.magnitude.parameter.names = c(
                                age.effect.magnitude.parameter.names[[age.index]],
                                race.effect.magnitude.parameter.names[[race]]
                            ),
                            mobility.correlation.parameter.name = mobility.correlation.parameter.name,
                            mobility.data = mobility.data
                        )))
        }
    }
    
    # Make the effect and return
    create.model.foreground(target.populations,
                            effects,
                            error.prefix = error.prefix)
}

create.covid.intervention.effect <- function(quantity.name,
                                             scale,
                                             n.covid.months,
                                             n.months.until.taper,
                                             effect.magnitude.parameter.names,
                                             mobility.correlation.parameter.name,
                                             mobility.data=COVID.MOBILITY.DATA)
{
    taper = get.covid.taper(n.covid.months = n.covid.months,
                            n.months.until.taper = n.months.until.taper)
    
    effect.exp = get.covid.effect.expression(effect.magnitude.parameter.names,
                                             mobility.correlation.parameter.name,
                                             mobility.name = 'mobility',
                                             effect.taper.name = 'taper')
    
    create.intervention.effect(quantity.name = quantity.name,
                               scale = scale,
                               apply.effects.as = 'multiplier',
                               allow.values.less.than.otherwise = T,
                               allow.values.greater.than.otherwise = T,
                               start.time = get.covid.start.time(),
                               end.time = get.covid.end.time(n.covid.months),
                               times = get.covid.effect.times(n.covid.months),
                               effect.values = effect.exp,
                               bindings = list(mobility=get.covid.mobility.measure,
                                               taper=taper),
                               mobility.data = mobility.data,
                               n.covid.months = n.covid.months,
                               census.manager = NULL)
}
