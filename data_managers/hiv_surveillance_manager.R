
ALLOWED.DATA.TYPES = c('prevalence','new','mortality',
                       'diagnosed','diagnosed.ci.lower','diagnosed.ci.upper',
                       'suppression','suppression.ci.lower','suppression.ci.upper', 'prevalence.for.continuum',
                       'suppression.of.engaged',
                       'estimated.prevalence', 'estimated.prevalence.ci.lower','estimated.prevalence.ci.upper', 'estimated.prevalence.rse',
                       'cumulative.aids.mortality', 'aids.diagnoses',
                       'linkage','engagement','new.for.continuum','retention',
                       'prep',
                       'testing','testing.n')

# This is also set in parallel in hiv_surveillance_manager_reader.R
SURVEILLANCE.DELIMITER = ';'

##-----------------##
##-----------------##
##-- THE GETTERS --##
##-----------------##
##-----------------##


get.surveillance.data <- function(surv=msa.surveillance,
                                  location.codes=BALTIMORE.MSA,
                                  data.type=c('new', 'prevalence','mortality','diagnosed')[1],
                                  years=NULL,
                                  age=F,
                                  race=F,
                                  sex=F,
                                  risk=F,
                                  aggregate.locations=T,
                                  aggregate.years=F,
                                  aggregate.by=c('sum','mean','weight')[1],
                                  aggregate.weight.data.type=NULL,
                                  throw.error.if.missing.years=F,
                                  throw.error.if.missing.data=T,
                                  na.rm=F,
                                  get.source=F,
                                  get.url=F,
                                  get.details=F)
{
    #Check the data.type argument
    
    if (all(data.type!=ALLOWED.DATA.TYPES))
        stop(paste0("Invalid data type ('", data.type, "'). " ,
                    "data.type must be one of: ",
                    paste0(paste0("'", ALLOWED.DATA.TYPES, "'"), collapse=", ")))
    
    if (class(data.type) != 'character' || length(data.type)>1)
        stop("data.type must be a character of length one")
    
    if (data.type=='suppression.of.engaged')
    {
        supp.val = get.surveillance.data(surv=surv,
                                     location.codes=location.codes,
                                     data.type='suppression',
                                     years=years,
                                     age=age,
                                     race=race,
                                     sex=sex,
                                     risk=risk,
                                     aggregate.locations=aggregate.locations,
                                     aggregate.years=aggregate.years,
                                     throw.error.if.missing.years=throw.error.if.missing.years,
                                     throw.error.if.missing.data=throw.error.if.missing.data,
                                     na.rm=na.rm,
                                     get.source=get.source,
                                     get.url=get.url)
        
        eng.val = get.surveillance.data(surv=surv,
                                        location.codes=location.codes,
                                        data.type='engagement',
                                        years=years,
                                        age=age,
                                        race=race,
                                        sex=sex,
                                        risk=risk,
                                        aggregate.locations=aggregate.locations,
                                        aggregate.years=aggregate.years,
                                        throw.error.if.missing.years=throw.error.if.missing.years,
                                        throw.error.if.missing.data=throw.error.if.missing.data,
                                        na.rm=na.rm,
                                        get.source=get.source,
                                        get.url=get.url)

        if (get.source || get.url || get.details)
        {
            rv = paste0(supp.val, SURVEILLANCE.DELIMITER, eng.val)
            rv[is.na(supp.val) | supp.val==''] = eng.val
            rv[is.na(eng.val) | eng.val==''] = supp.val
            
            return (rv)
        }
        else
        {
            return(supp.val / eng.val)
        }
    }
    
    categories = c('sex','age','race','risk')[c(sex, age, race, risk)]
    data.name = get.surveillance.data.name(data.type=data.type,
                                           sex=sex,
                                           age=age,
                                           race=race,
                                           risk=risk)
    
    data.suffix = substr(data.name,
                         nchar(data.type)+2,
                         nchar(data.name))
    
    #Check to make sure the requested data exists
    if (get.source)
        data = surv$source[[data.name]]
    else if (get.url)
        data = surv$url[[data.name]]
    else if (get.details)
        data = surv$details[[data.name]]
    else
        data = surv[[data.name]]
    
    data.type.name = data.type
    if (data.type=='new')
        data.type.name = 'new cases'
    if (is.null(data))
    {
        if (throw.error.if.missing.data)
            stop(paste0("This surveillance manager does not have any data on ",
                        ifelse(get.source, "the source of ", ''),
                        ifelse(get.url, "the url for ", ''),
                        "'",
                        data.type.name, "' for ",
                        ifelse(data.suffix=='all',
                               "the aggregate population.",
                               paste0("strata of ",
                                      paste0(categories, collapse = ' x '))
                        )))
        else
            return (NULL)
    }
    
    
    #check to make sure all location codes are present in the data
    missing.code = sapply(location.codes, function(code){
        all(dimnames(data)[['location']]!=code)
    })
    
    if (any(missing.code))
    {
        if (throw.error.if.missing.data)
            stop(paste0("The following location code(s) are not present in the requested data ('",
                        data.type, "'): ",
                        paste0(location.codes[missing.code], collapse=', ')))
        else
            return (NULL)
    }
    
    #Pull and check the years
    if (is.null(years))
        years = dimnames(data)[['year']]
    
    years = as.character(years)
    missing.years = sapply(years, function(year){
        all(dimnames(data)[['year']] != year)
    })
    
    if (any(missing.years))
    {
        if (throw.error.if.missing.years)
            stop(paste0("The following year(s) are not present in the requested data: ",
                        paste0(years[missing.years], collapse=', ')))
    }
    
    requested.years = years
    years = years[!missing.years]
    
    if (length(years)==0)
    {
        if (throw.error.if.missing.data)
            stop("No data is available for any of the requested years")
        else
            return (NULL)
    }
    
    #pull the data
    target.dimnames = dimnames(data)[c('year','location',categories)]
    target.dimnames[['year']] = years
    target.dimnames[['location']] = location.codes
    
    overwrite.dimnames = c(dimnames(data)[1:2],list(other=NULL))
    overwrite.dim = c(dim(data)[1:2], other=as.numeric(prod(dim(data))/dim(data)[1]/dim(data)[2]))
    dim(data) = overwrite.dim
    dimnames(data) = overwrite.dimnames
        
    data = data[years, location.codes, ]
    

    dim(data) = sapply(target.dimnames, length)
    dimnames(data) = target.dimnames
    
    
    #collapse as needed and return
    keep.dimnames = dimnames(data)
    keep.dimnames.names = names(keep.dimnames)
    
    if (aggregate.locations)
        keep.dimnames.names = setdiff(keep.dimnames.names, 'location')
    if (aggregate.years)
        keep.dimnames.names = setdiff(keep.dimnames.names, 'year')
    
    keep.dimnames = keep.dimnames[keep.dimnames.names]
    
    if (setequal(keep.dimnames.names, names(dimnames(data))))
    {
        if (!all(keep.dimnames.names==names(dimnames(data))))
            data = apply(data, keep.dimnames.names, function(x){x})
    }
    else if (get.source || get.url || get.details)
        data = apply(data, keep.dimnames.names, merge.surveillance.codes)
    else if (aggregate.by=='sum')
        data = apply(data, keep.dimnames.names, sum, na.rm=na.rm)
    else if (aggregate.by=='mean')
        data = apply(data, keep.dimnames.names, mean, na.rm=na.rm)
    else #aggregate by = 'weighted'
    {
        if (is.null(aggregate.weight.data.type))
            stop("Must set 'aggregate.weight.data.type' in order to aggregate by weight")
        denominators = get.surveillance.data(surv=surv,
                                             location.codes=location.codes,
                                             data.type=aggregate.weight.data.type,
                                             years=years,
                                             age=age,
                                             race=race,
                                             sex=sex,
                                             risk=risk,
                                             aggregate.locations=F,
                                             aggregate.years=F,
                                             throw.error.if.missing.years=throw.error.if.missing.years,
                                             throw.error.if.missing.data=throw.error.if.missing.data)
 
        denominators[is.na(data)] = NA
        data = apply(data*denominators, keep.dimnames.names, sum, na.rm=na.rm) /
            apply(denominators, keep.dimnames.names, sum, na.rm=na.rm)
    }
    
    dim(data) = sapply(keep.dimnames, length)
    dimnames(data) = keep.dimnames
    
    if (setequal(years, requested.years))
    {
        attr(data, 'years') = as.numeric(years)
        data
    }
    else
    {
        rv.dim.names = dimnames(data)
        rv.dim.names[['year']] = as.character(requested.years)
        rv = array(as.numeric(NA), dim=sapply(rv.dim.names, length), dimnames=rv.dim.names)
        access(rv, year=as.character(years)) = data
        rv
    }
}

#internal helper for getting the right array
get.surveillance.data.name <- function(data.type,
                                       sex,
                                       age,
                                       race,
                                       risk)
{
    
    categories = c('sex','age','race','risk')[c(sex, age, race, risk)]
    
    data.suffix = paste0(categories, collapse='.')
    if (data.suffix=='')
        data.suffix  = 'all'
    
    data.name = paste0(data.type, '.', data.suffix)
    
    data.name
}

get.locations.for.surveillance.data <- function(surv,
                                                data.type,
                                                sex=F,
                                                age=F,
                                                race=F,
                                                risk=F)
{
    data.name = get.surveillance.data.name(data.type,
                                           sex=sex,
                                           age=age,
                                           race=race,
                                           risk=risk)
    
    dimnames(surv[[data.name]])$location
}

# Need this function for compatility in setup for location
get.surveillance.data.rate <- function(surv,
                                       location.codes,
                                       years=NULL,
                                       data.type=c('new', 'prevalence','mortality')[1],
                                       age=F,
                                       race=F,
                                       sex=F,
                                       aggregate.years=F,
                                       census,
                                       throw.error.if.missing.data=T)
{
    numerators = get.surveillance.data(surv, location.codes=location.codes,
                                       years=years, data.type=data.type,
                                       age=age, race=race, sex=sex,
                                       aggregate.locations=T,
                                       aggregate.years=aggregate.years, 
                                       throw.error.if.missing.data = throw.error.if.missing.data)

    if (is.null(numerators) || all(is.na(numerators)))
        return (NULL)
    
    years = attr(numerators, 'years')
    
    denominators = get.census.data(census, years=years,
                                   fips=counties.for.msa(location.codes),
                                   aggregate.counties = T,
                                   aggregate.years = aggregate.years,
                                   aggregate.ages=!age,
                                   aggregate.races=!race,
                                   aggregate.sexes=!sex)

    if (!is.null(dim(denominators)))
    {
        if (race)
        {
            mapping = get.ontology.mapping(from.ontology = dimnames(denominators)['race'], to.ontology = dimnames(numerators)['race'])
            denominators = mapping$apply(denominators)
#            denominators = collapse.races(denominators, races=dimnames(numerators)$race)
        }
        denominators = apply(denominators, names(dimnames(numerators)), function(x){x})
    }
    
    numerators / denominators
}


merge.surveillance.codes <- function(codes)
{
    if (length(codes)==1)
        codes
    else if (length(codes)==2)
    {
        if (length(codes[1])==0 || is.na(codes[1]) || codes[1]=='')
            codes[2]
        else if (length(codes[2])==0 || is.na(codes[2]) || codes[2]=='')
            codes[1]
        else
            paste0(union(codes[1], codes[2]), collapse=SURVEILLANCE.DELIMITER)
    }
    else
        merge.surveillance.codes(codes[1], 
                                 merge.surveillance.codes(codes[-1]))
}

parse.surveillance.code <- function(surv,
                                    code)
{
    if (length(code)==0 || is.na(code) || code=='' || code=='NA')
        character()
    else
    {
        codes = strsplit(code, split=SURVEILLANCE.DELIMITER, fixed = T)[[1]]
        codes = unique(codes)
        codes = codes[!is.na(codes) & codes!='' & codes!='NA']
        codes = as.integer(codes)
        
        surv$code.map[codes]
    }
}


get.surveillance.data.source <- function(surv=msa.surveillance,
                                  location.codes=BALTIMORE.MSA,
                                  data.type=c('new', 'prevalence','mortality','diagnosed')[1],
                                  years=NULL,
                                  age=F,
                                  race=F,
                                  sex=F,
                                  risk=F,
                                  na.rm=F,
                                  wrap='\\n')
{
    data = get.surveillance.data(surv=surv, location.codes=location.codes, data.type=data.type,
                                 years=years,
                                 age=age, race=race, sex=sex, risk=risk,
                                 throw.error.if.missing.data = F, throw.error.if.missing.years = F)
    
    if (is.null(data) || all(is.na(data)))
        'N/A'
    else if (data.type=='prep')
        'AidsVu'
    else if (data.type=='linkage' || data.type=='suppression')
        'Local Health Department'
    else
        'CDC'
}



get.surveillance.data.details <- function(surv=msa.surveillance,
                                          location.codes=BALTIMORE.MSA,
                                          data.type=c('new', 'prevalence','mortality','diagnosed')[1],
                                          years=NULL,
                                          age=F,
                                          race=F,
                                          sex=F,
                                          risk=F,
                                          aggregate.locations=T,
                                          aggregate.years=F,
                                          throw.error.if.missing.years=F,
                                          throw.error.if.missing.data=T,
                                          pull.source.if.missing=T)
{
    rv = get.surveillance.data(surv=surv,
                               location.codes=location.codes,
                               data.type=data.type,
                               years=years,
                               age=age,
                               race=race,
                               sex=sex,
                               risk=risk,
                               aggregate.locations=aggregate.locations,
                               aggregate.years=aggregate.years,
                               throw.error.if.missing.years=throw.error.if.missing.years,
                               throw.error.if.missing.data=throw.error.if.missing.data,
                               get.source=F,
                               get.url=F,
                               get.details=T)
    
    if (pull.source.if.missing)
    {
        src = get.surveillance.data(surv=surv,
                                    location.codes=location.codes,
                                    data.type=data.type,
                                    years=years,
                                    age=age,
                                    race=race,
                                    sex=sex,
                                    risk=risk,
                                    aggregate.locations=aggregate.locations,
                                    aggregate.years=aggregate.years,
                                    throw.error.if.missing.years=throw.error.if.missing.years,
                                    throw.error.if.missing.data=throw.error.if.missing.data,
                                    get.source=T,
                                    get.url=F,
                                    get.details=F)
        
        if (is.null(rv))
            rv = src
        else
        {
            missing = is.na(rv) | rv==''
            rv[missing] = src[missing]
        }
    }
    
    rv
}


has.location.surveillance <- function(surv, location)
{
    any(sapply(surv, function(elem){
        !is.null(dim(elem)) && !is.null(dimnames(elem)[['location']]) && any(location==dimnames(elem)[['location']])
    }))
}

##------------------------##
##-- DEPRECATED GETTERS --##
##------------------------##


get.state.averaged.knowledge.of.status <- function(msa,
                                                   state.surveillance,
                                                   years,
                                                   census.totals = ALL.DATA.MANAGERS$census.totals, 
                                                   throw.error.if.missing=F)
{
    states = states.for.msa(msa)
    if (length(states)==1)
        get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years, throw.error.if.missing.data = throw.error.if.missing)
    else
    {
        p = get.surveillance.data(state.surveillance, states, data.type = 'diagnosed', years=years, aggregate.locations = F, throw.error.if.missing.data = throw.error.if.missing)
        
        counties = counties.for.msa(msa)
        county.populations = get.census.totals(census.totals, location=counties, years=years, collapse.counties=F)
        states.by.county = state.for.county(counties)
        state.populations.from.counties = sapply(states, function(st){
            sapply(years, function(year){
                sum(county.populations[as.character(year), states.by.county==st])
            })
        })
        
        rowSums(p * state.populations.from.counties / rowSums(state.populations.from.counties))
    }
}

##---------------------------##
##-- READ MSA SURVEILLANCE --##
##---------------------------##






##-----------------------------##
##-- READ STATE SURVEILLANCE --##
##-----------------------------##

OLD.read.state.surveillance.manager <- function(dir='cleaned_data',
                                            verbose=T)
{
    if (verbose)
        print("Reading state surveillance data (reported, prevalence, deaths)")
    files = list.files(file.path(dir, 'hiv_surveillance/state'))
    is.female = grepl('female', files, ignore.case = T)

    dfs = lapply(file.path(dir, 'hiv_surveillance/state', files), read.hiv.atlas.file) 
    
    df = NULL
    for (i in 1:length(dfs))
    {
        if (is.female[i])
            dfs[[i]]$Sex = 'female'
        else
            dfs[[i]]$Sex = 'male'
        
        df = rbind(df, dfs[[i]])
    }

    df$State = state.name.to.abbreviation(df$Geography)
    states = unique(df$State)
    years = as.character(unique(df$Year))
    df$Age.Group = paste0(df$Age.Group, " years")
    ages = sort(unique(as.character(df$Age.Group)))

    dim.names = list(year=years, location=states, age=ages, race=c('black','hispanic','other'),
                     sex=c('male','female'), risk=c('msm','idu','msm_idu','heterosexual'))


    rv = list()
    rv$new.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$prevalence.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    rv$mortality.master = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    for (i in 1:dim(df)[1])
    {
        if (df$Indicator[i]=="HIV deaths")
            elem = 'mortality.master'
        else if (df$Indicator[i]=='HIV diagnoses')
            elem = 'new.master'
        else if (df$Indicator[i]=='HIV prevalence')
            elem = 'prevalence.master'
        else
            stop("Only indicators 'HIV deaths', 'HIV diagnoses', and 'HIV prevalence' are supported")

        if (grepl('hispanic', df$Race.Ethnicity[i], ignore.case = T))
            race.to = 'hispanic'
        else if (grepl('black', df$Race.Ethnicity[i], ignore.case = T))
            race.to = 'black'
        else
            race.to = 'other'

        if (grepl('injection', df$Transmission.Category[i], ignore.case = T))
        {
            if (grepl('male-to-male', df$Transmission.Category[i], ignore.case = T))
                risk.to = 'msm_idu'
            else
                risk.to = "idu"
        }
        else if (grepl('male-to-male', df$Transmission.Category[i], ignore.case = T))
            risk.to = 'msm'
        else
            risk.to = 'heterosexual'
        
        rv[[elem]][as.character(df$Year[i]), df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to] = df$Cases[i] +
            rv[[elem]][as.character(df$Year[i]), df$State[i], df$Age.Group[i], race.to, df$Sex[i], risk.to]

    }

    ##-- Estimated Prevalence and Diagnosis --##
    
    if (verbose)
        print("Reading state estimated prevalence and diagnosis rate")
    df.dx = read.hiv.atlas.file(file.path(dir, 'continuum/state/state_diagnosis_all.csv'))
    df.dx$State = state.name.to.abbreviation(df.dx$Geography)
    
    dx.dim.names = list(year=sort(unique(df.dx$Year)), location=unique(df.dx$State))
    rv$estimated.prevalence.all = rv$estimated.prevalence.ci.lower.all =
        rv$estimated.prevalence.ci.upper.all = rv$estimated.prevalence.rse.all =
        rv$diagnosed.all =
        array(0, dim=sapply(dx.dim.names, length), dimnames=dx.dim.names)

    for (i in 1:dim(df.dx)[1])
    {
        if (df.dx$Indicator[i]=='Knowledge of Status')
        {
            rv$estimated.prevalence.rse.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$RSE[i]
            rv$diagnosed.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Percent[i]/100 
        }
        else
        {
            rv$estimated.prevalence.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases[i]
            rv$estimated.prevalence.ci.lower.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases.LCI[i]
            rv$estimated.prevalence.ci.upper.all[as.character(df.dx$Year[i]), df.dx$State[i]] = df.dx$Cases.UCI[i]
        }
    }

    rv
}



read.state.diagnoses <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    regex = '([0-9]+\\.*[0-9]*) \\(.*'
    pct = rep(NA, dim(df)[1])
    matches = grepl(regex, df[,4])
    
    pct[matches] = as.numeric(gsub(regex, '\\1', df[matches,4]))
    years = as.character(df$Year)
    states = state.name.to.abbreviation(df$Geography)
    
    dim.names = list(year=sort(unique(years)), location=unique(states))
    rv = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (i in 1:length(pct))
        rv[as.character(years[i]), states[i]] = pct[i]/100
    
    rv
}


read.state.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    cases = gsub(',','', df$Cases)
    na.cases = cases=='Data suppressed'
    df$Cases = as.numeric(NA)
    df$Cases[!na.cases] = as.numeric(cases[!na.cases])
    df$State = state.name.to.abbreviation(df$Geography)
    df$Age.Group = paste0(df$Age.Group, ' years')
    df$Year = as.character(df$Year)

    df
}

read.state.estimated.prevalence.file <- function(file)
{

    df = read.csv(file, stringsAsFactors = F)
    df$State = state.name.to.abbreviation(df$Geography)
    cases = df$Cases..95..CI.RSE..

    regex = "([0-9,]+[0-9]) \\(([0-9,]+[0-9]) - ([0-9,]+[0-9]), ([0-9,\\.]+[0-9])\\)"

    df$estimate = as.numeric(gsub(',', '', gsub(regex, '\\1', cases)))
    df$ci.lower = as.numeric(gsub(',', '', gsub(regex, '\\2', cases)))
    df$ci.upper = as.numeric(gsub(',', '', gsub(regex, '\\3', cases)))
    df$rse = as.numeric(gsub(',', '', gsub(regex, '\\4', cases)))

    df
}

##--------------------------------##
##-- READ NATIONAL SURVEILLANCE --##
##--------------------------------##

read.national.surveillance.manager <- function(dir='cleaned_data',
                                               aggregate.race.as.bho=T,
                                               verbose=T)
{
    rv = list()
    
    if (verbose)
        print("Reading Suppression Data")
    rv = read.national.suppression(rv,
                                   file.path(dir, 'continuum/national/suppression'),
                                   collapse.race.as.bho = aggregate.race.as.bho)
    
    if (verbose)
        print("Reading Diagnosis Data")
    rv = read.national.diagnoses(rv,
                                 dir=file.path(dir, 'continuum/national/diagnosis'),
                                 aggregate.race.as.bho = aggregate.race.as.bho)
    
    rv
}

read.national.suppression <- function(surv,
                                      dir='cleaned_data/continuum/national/suppression',
                                      collapse.race.as.bho=T)
{
    files = list.files(dir)
    files = files[grepl('[0-9][0-9][0-9][0-9].*', files)]
    years = substr(files, 1,4)
    
    dfs = lapply(file.path(dir, files), read.national.continuum.file)
    
    if (!collapse.race.as.bho)
        stop("We are currently only set up to collapse races as black/hispanic/other")
    
    dim.names.all = list(year = sort(years),
                         location='national',
                         age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                         race=c('black','hispanic','other'),
                         sex=c('female','male'),
                         risk=c('msm','idu','msm_idu','heterosexual'))
    
    surv$suppression.all = surv$prevalence.for.continuum.all = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location')], length),
              dimnames=dim.names.all[c('year','location')])
    surv$suppression.sex =  surv$prevalence.for.continuum.sex = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','sex')], length),
              dimnames=dim.names.all[c('year','location','sex')])
    surv$suppression.age =  surv$prevalence.for.continuum.age = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','age')], length),
              dimnames=dim.names.all[c('year','location','age')])
    surv$suppression.race =  surv$prevalence.for.continuum.race = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','race')], length),
              dimnames=dim.names.all[c('year','location','race')])
    surv$suppression.risk = surv$prevalence.for.continuum.risk = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','risk')], length),
              dimnames=dim.names.all[c('year','location','risk')])
    surv$suppression.sex.risk =  surv$prevalence.for.continuum.sex.risk = 
        array(as.numeric(NA),
              dim=sapply(dim.names.all[c('year','location','sex','risk')], length),
              dimnames=dim.names.all[c('year','location','sex','risk')])
    
    race.mapping = c('American Indian/Alaska Native' = 'other',
                     'Asian' = 'other',
                     'Black/African American' = 'black',
                     'Hispanic/Latino' = 'hispanic',
                     'Native Hawaiian/Other Pacific Islander' = 'other',
                     'White' = 'other',
                     'Multiple races' = 'other')
    
    for (i in 1:length(dfs))
    {
        df = dfs[[i]]
        year = years[i]
        
        surv$suppression.all[year,1] = df$pct.suppressed[df$category=='Total']/100
        surv$prevalence.for.continuum.all[year,1] = df$n[df$category=='Total']
        
        surv$suppression.sex[year,1,'female'] = df$pct.suppressed[df$category=='Female']/100
        surv$prevalence.for.continuum.sex[year,1,'female'] = df$n[df$category=='Female']
        surv$suppression.sex[year,1,'male'] = df$pct.suppressed[df$category=='Male']/100
        surv$prevalence.for.continuum.sex[year,1,'male'] = df$n[df$category=='Male']
        
        surv$suppression.age[year,1,1] = df$pct.suppressed[df$category=='13-24']/100
        surv$prevalence.for.continuum.age[year,1,1] = df$n[df$category=='13-24']
        surv$suppression.age[year,1,2] = df$pct.suppressed[df$category=='25-34']/100
        surv$prevalence.for.continuum.age[year,1,2] = df$n[df$category=='25-34']
        surv$suppression.age[year,1,3] = df$pct.suppressed[df$category=='35-44']/100
        surv$prevalence.for.continuum.age[year,1,3] = df$n[df$category=='35-44']
        surv$suppression.age[year,1,4] = df$pct.suppressed[df$category=='45-54']/100
        surv$prevalence.for.continuum.age[year,1,4] = df$n[df$category=='45-54']
        surv$suppression.age[year,1,5] = df$pct.suppressed[df$category=='55+']/100
        surv$prevalence.for.continuum.age[year,1,5] = df$n[df$category=='55+']
        
        race.numerators = race.denominators = sapply(dim.names.all[['race']], function(x){0})
        for (race.from in names(race.mapping))
        {
            race.to = race.mapping[race.from]
            
            race.numerators[race.to] = race.numerators[race.to] + df$n.suppressed[df$category==race.from]
            race.denominators[race.to] = race.denominators[race.to] + df$n[df$category==race.from]
        }
        surv$suppression.race[year,1,] = race.numerators / race.denominators
        surv$prevalence.for.continuum.race[year,1,] = race.denominators
        
        #Risk and sex x risk
        surv$suppression.risk[year,1,'msm'] = surv$suppression.sex.risk[year,1,'male','msm'] =
            df$pct.suppressed[df$category=='msm']/100
        surv$prevalence.for.continuum.risk[year,1,'msm'] = surv$prevalence.for.continuum.sex.risk[year,1,'male','msm'] =
            df$n[df$category=='msm']
        surv$suppression.risk[year,1,'msm_idu'] = surv$suppression.sex.risk[year,1,'male','msm_idu'] =
            df$pct.suppressed[df$category=='idu.msm']/100
        surv$prevalence.for.continuum.risk[year,1,'msm_idu'] = surv$prevalence.for.continuum.sex.risk[year,1,'male','msm_idu'] =
            df$n[df$category=='idu.msm']
        
        surv$suppression.sex.risk[year,1,'female','msm'] = surv$suppression.sex.risk[year,1,'female','msm_idu'] = 0
        surv$prevalence.for.continuum.sex.risk[year,1,'female','msm'] = surv$prevalence.for.continuum.sex.risk[year,1,'female','msm_idu'] = 0
        
        surv$suppression.sex.risk[year,1,'female','idu'] = df$pct.suppressed[df$category=='idu.female']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'female','idu'] = df$n[df$category=='idu.female']
        surv$suppression.sex.risk[year,1,'male','idu'] = df$pct.suppressed[df$category=='idu.male']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'male','idu'] = df$n[df$category=='idu.male']
        
        surv$suppression.sex.risk[year,1,'female','heterosexual'] = df$pct.suppressed[df$category=='het.female']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'female','heterosexual'] = df$n[df$category=='het.female']
        surv$suppression.sex.risk[year,1,'male','heterosexual'] = df$pct.suppressed[df$category=='het.male']/100
        surv$prevalence.for.continuum.sex.risk[year,1,'male','heterosexual'] = df$n[df$category=='het.male']
        
        surv$suppression.risk[year,1,'idu'] = (df$n.suppressed[df$category=='idu.female'] + df$n.suppressed[df$category=='idu.male']) /
            (df$n[df$category=='idu.female'] + df$n[df$category=='idu.male'])
        surv$prevalence.for.continuum.risk[year,1,'idu'] = df$n[df$category=='idu.female'] + df$n[df$category=='idu.male']
        surv$suppression.risk[year,1,'heterosexual'] = (df$n.suppressed[df$category=='het.female'] + df$n.suppressed[df$category=='het.male']) /
            (df$n[df$category=='het.female'] + df$n[df$category=='het.male'])
        surv$prevalence.for.continuum.risk[year,1,'heterosexual'] = df$n[df$category=='het.female'] + df$n[df$category=='het.male']
    }
    
    if (any(is.na(surv$suppression.all)))
        stop("NAs generated in reading national suppression data (totals)")
    if (any(is.na(surv$suppression.sex)))
        stop("NAs generated in reading national suppression data (by sex)")
    if (any(is.na(surv$suppression.age)))
        stop("NAs generated in reading national suppression data (by age)")
    if (any(is.na(surv$suppression.race)))
        stop("NAs generated in reading national suppression data (by race)")
    if (any(is.na(surv$suppression.risk)))
        stop("NAs generated in reading national suppression data (by risk)")
    if (any(is.na(surv$suppression.sex.risk)))
        stop("NAs generated in reading national suppression data (by sex x risk)")
        
    #Return
    surv
}

read.national.diagnoses <- function(surv,
                                    dir='cleaned_data/continuum/national/diagnosis/',
                                    aggregate.race.as.bho=T)
{
    
    df.all = read.hiv.atlas.file(file.path(dir, 'diagnosis_all.csv'))
    df.age = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_age.csv'))
    df.sex = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_sex.csv'))
    df.race = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_race.csv'))
    df.risk = read.hiv.atlas.file(file.path(dir, 'diagnosis_by_risk.csv'))
    
    
    years = sort(unique(c(unique(df.all$Year),
                          unique(df.age$Year),
                          unique(df.sex$Year),
                          unique(df.race$Year),
                          unique(df.risk$Year))))
    
    dim.names.all = list(year = sort(years),
                         location='national',
                         age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'),
                         race=c('black','hispanic','other'),
                         sex=c('female','male'),
                         risk=c('msm','idu','msm_idu','heterosexual'))
    
    surv$diagnosed.all = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location')], length),
                                 dimnames=dim.names.all[c('year','location')])
    surv$diagnosed.sex = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location','sex')], length),
                                 dimnames=dim.names.all[c('year','location','sex')])
    surv$diagnosed.age = array(as.numeric(NA),
                                 dim=sapply(dim.names.all[c('year','location','age')], length),
                                 dimnames=dim.names.all[c('year','location','age')])
    surv$diagnosed.race = race.numerators = race.denominators =
        array(0,
              dim=sapply(dim.names.all[c('year','location','race')], length),
              dimnames=dim.names.all[c('year','location','race')])
    surv$diagnosed.risk = array(as.numeric(NA),
                                  dim=sapply(dim.names.all[c('year','location','risk')], length),
                                  dimnames=dim.names.all[c('year','location','risk')])
    
##    surv$diagnosed.sex.race.risk = sex.race.risk.numerators = sex.race.risk.denominators = 
#        array(as.numeric(0),
#              dim=sapply(dim.names.all[c('year','location','race','sex','risk')], length),
#              dimnames=dim.names.all[c('year','location','race','sex','risk')])
#    surv$diagnosed.sex.age.race = sex.age.race.numerators = sex.age.race.denominators = 
#        array(as.numeric(0),
#              dim=sapply(dim.names.all[c('year','location','age','race','sex')], length),
#              dimnames=dim.names.all[c('year','location','age','race','sex')])
    
    
    race.mapping = c('American Indian/Alaska Native' = 'other',
                     'Asian' = 'other',
                     'Black/African American' = 'black',
                     'Hispanic/Latino' = 'hispanic',
                     'Native Hawaiian/Other Pacific Islander' = 'other',
                     'White' = 'other',
                     'Multiple races' = 'other')
    
    age.mapping = c('13-24' = 1,
                    '25-34' = 2,
                    '35-44' = 3,
                    '45-54' = 4,
                    '55+' = 5)
    
    risk.mapping = c('Male-to-male sexual contact' = 'msm',
                     'Injection drug use' = 'idu',
                     'Male-to-male sexual contact and injection drug use' = 'msm_idu',
                     'Heterosexual contact' = 'heterosexual',
                     'Other' = 'other')
    
    sex.mapping = c('Male'='male','Female'='female')
    
    for (i in 1:dim(df.all)[1])
    {
        if (df.all$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.all[as.character(df.all$Year[i]),1] = df.all$Percent[i]/100
    }
    
    for (i in 1:dim(df.age)[1])
    {
        if (df.age$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.age[as.character(df.age$Year[i]),1,age.mapping[df.age$Age.Group[i]]] = df.age$Percent[i]/100
    }
    
    for (i in 1:dim(df.race)[1])
    {
        if (df.race$Indicator[i]=="Knowledge of Status" && !is.na(df.race$Percent[i]))
        {
            race.numerators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]] = 
                df.race$Cases[i] +
                race.numerators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]]
            
            race.denominators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]] = 
                df.race$Cases[i] * 100 / df.race$Percent[i] +
                race.denominators[as.character(df.race$Year[i]),1,race.mapping[df.race$Race.Ethnicity[i]]]
        }
    }
    surv$diagnosed.race = race.numerators / race.denominators
    
    for (i in 1:dim(df.risk)[1])
    {
        if (df.risk$Indicator[i]=="Knowledge of Status" &&
            any(risk.mapping[df.risk$Transmission.Category[i]] == dim.names.all[['risk']]))
            surv$diagnosed.risk[as.character(df.risk$Year[i]),1,risk.mapping[df.risk$Transmission.Category[i]]] = df.risk$Percent[i]/100
    }
    
    for (i in 1:dim(df.sex)[1])
    {
        if (df.sex$Indicator[i]=="Knowledge of Status")
            surv$diagnosed.sex[as.character(df.sex$Year[i]),1,sex.mapping[df.sex$Sex[i]]] = df.sex$Percent[i]/100
    }
    
    if (any(is.na(surv$diagnosed.all)))
        stop("NAs generated in reading national diagnosis data (totals)")
    if (any(is.na(surv$diagnosed.sex)))
        stop("NAs generated in reading national diagnosis data (by sex)")
    if (any(is.na(surv$diagnosed.age)))
        stop("NAs generated in reading national diagnosis data (by age)")
    if (any(is.na(surv$diagnosed.race)))
        stop("NAs generated in reading national diagnosis data (by race)")
    if (any(is.na(surv$diagnosed.risk)))
        stop("NAs generated in reading national diagnosis data (by risk)")
    if (any(is.na(surv$diagnosed.sex.risk)))
        stop("NAs generated in reading national diagnosis data (by sex x risk)")
        
    surv
}

read.national.continuum.file <- function(file)
{
    df = read.csv(file, stringsAsFactors = F)
    for (j in 2:dim(df)[2])
    {
        if (class(df[,j])=='character')
        {
            df[,j] = gsub(' |,','',df[,j])
            df[df[,j]=='',j] = NA
            df[,j] = as.numeric(df[,j])
        }
    }
    
    df$category = gsub('â€“', '-', df$category)
    
    df
}



HIV.ATLAS.NUMERIC.COLUMNS = c('Cases',
                              'Cases.LCI',
                              'Cases.UCI',
                              'Percent',
                              'Percent.LCI',
                              'Percent.UCI',
                              'Rate.per.100000',
                              'Rate.LCI',
                              'Rate.UCI',
                              'RSE',
                              'Population')
read.hiv.atlas.file <- function(file)
{
    print(paste0("Reading HIV Atlas File: ", file))
    df = read.csv(file, stringsAsFactors = F)
    cols = intersect(names(df), HIV.ATLAS.NUMERIC.COLUMNS)

    for (col in cols)
    {
        if (class(df[,col])=='character')
        {
            df[,col] = gsub(',| ','',df[,col])
            df[!is.na(df[,col]) & (df[,col]=='' | df[,col]=='Data suppressed'), col] = NA
            df[,col] = suppressWarnings(as.numeric(df[,col]))
        }
    }
    
    df
}

##------------------------##
##-- MISC OTHER HELPERS --##
##------------------------##


#pull out a location subset
subset.surveillance <- function(surv, codes)
{
    codes = as.character(codes)
    lapply(surv, function(elem){
        if (length(dim(elem))==2)
            elem[,codes]
        else if (length(dim(elem))==3)
            elem[,codes,]
        else if (length(dim(elem))==4)
            elem[,codes,,]
        else
            stop("The subset.surveillance function is currently only set up to handle components with 2, 3, or 4 dimensions")
    })
}

