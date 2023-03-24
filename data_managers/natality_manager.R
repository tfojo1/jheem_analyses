#source('../code/data_managers/census_manager.R')
library(jheem)

MAP.RACES.FOR.FERTILITY = c('White'='white',
                            'Black or African American'='black',
                            'American Indian or Alaska Native'='american_indian_or_alaska_native',
                            'Asian or Pacific Islander'='asian')

read.fertility <- function(dir='../data2/Natality/',
                           census=census,
                           years=2007:2017,
                           age.cutoffs.county=c(13,25,35,45,50,Inf),
                           age.cutoffs.state=age.cutoffs.county,
                           per.population=1000,
                           collapse.last.two.ages.to=55)
{
##-- Set up the return value --##
    rv = list()
    rv$per.population = 1000
    rv$years=years


##-- Set up ages --##
    rv$age.county = make.age.strata(age.cutoffs.county)
    rv$age.state = make.age.strata(age.cutoffs.state)

    county.age.labels = gsub(' years', '', rv$age.county$labels)
    state.age.labels = gsub(' years', '', rv$age.state$labels)

##-- Read in files --##
    hispanic.by.county = lapply(1:length(county.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'Hispanic ', county.age.labels[i], ' natality by county.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.county$lowers[i],
                                 age.upper=rv$age.county$uppers[i]-1,
                                 all.races=F,
                                 hispanic.only=T,
                                 is.state=F,
                                 per.population=per.population)
        set
    })
    nonhispanic.by.county = lapply(1:length(county.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'Non-hispanic ', county.age.labels[i], ' natality by county.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.county$lowers[i],
                                 age.upper=rv$age.county$uppers[i]-1,
                                 all.races=F,
                                 hispanic.only=F,
                                 is.state=F,
                                 per.population=per.population)
        set
    })
    all.by.county = lapply(1:length(county.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'All race ', county.age.labels[i], ' natality by county.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.county$lowers[i],
                                 age.upper=rv$age.county$uppers[i]-1,
                                 all.races=T,
                                 hispanic.only=F,
                                 is.state=F,
                                 per.population=per.population)
        set
    })

    hispanic.by.state = lapply(1:length(state.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'Hispanic ', state.age.labels[i], ' natality by state.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.state$lowers[i],
                                 age.upper=rv$age.state$uppers[i]-1,
                                 all.races=F,
                                 hispanic.only=T,
                                 is.state=T,
                                 per.population=per.population)
        set
    })
    nonhispanic.by.state = lapply(1:length(state.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'Non-hispanic ', state.age.labels[i], ' natality by state.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.state$lowers[i],
                                 age.upper=rv$age.state$uppers[i]-1,
                                 all.races=F,
                                 hispanic.only=F,
                                 is.state=T,
                                 per.population=per.population)
        set
    })
    all.by.state = lapply(1:length(state.age.labels), function(i){
        set = read.natality.file(paste0(dir, 'All race ', state.age.labels[i], ' natality by state.txt'),
                                 census=census,
                                 years=years,
                                 age.lower=rv$age.state$lowers[i],
                                 age.upper=rv$age.state$uppers[i]-1,
                                 all.races=T,
                                 hispanic.only=F,
                                 is.state=T,
                                 per.population=per.population)
        set
    })

    hispanic.births = read.natality.file(paste0(dir, 'Hispanic births by county.txt'))
    non.hispanic.births = read.natality.file(paste0(dir, 'Non-hispanic births by county.txt'))
    all.births = read.natality.file(paste0(dir, 'All race births by county.txt'))

    hispanic.births.by.state = read.natality.file(paste0(dir, 'Hispanic births by state.txt'), is.state=T)
    non.hispanic.births.by.state = read.natality.file(paste0(dir, 'Non-hispanic births by state.txt'), is.state=T)

##-- Set up the arrays and indices --##
    states = unique(all.by.state[[1]]$State)
    rv$counties = counties = format.combined.fips(unique(all.by.county[[1]]$County.Code))

    rv$races = BRIDGED.RACE.PLUS.HISPANIC

    county.dim.names.by.race = list(county=counties, age=rv$age.county$labels, race=BRIDGED.RACE.PLUS.HISPANIC)
    state.dim.names.by.race = list(state=states, age=rv$age.state$labels, race=BRIDGED.RACE.PLUS.HISPANIC)
    birth.rate.dim.names = list(county=counties, race=BRIDGED.RACE.PLUS.HISPANIC)
    state.birth.rate.dim.names = list(state=states, race=BRIDGED.RACE.PLUS.HISPANIC)

    rv$county.data.by.race = array(NA,
                            dim=sapply(county.dim.names.by.race, length),
                            dimnames = county.dim.names.by.race)
    rv$county.data.all.races = array(NA,
                              dim=sapply(county.dim.names.by.race[-3], length),
                              dimnames = county.dim.names.by.race[-3])

    rv$county.birth.rates = array(NA,
                                  dim=sapply(birth.rate.dim.names, length),
                                  dimnames = birth.rate.dim.names)


    rv$state.data.by.race = array(NA,
                                   dim=sapply(state.dim.names.by.race, length),
                                   dimnames = state.dim.names.by.race)
    rv$state.data.all.races = array(NA,
                                     dim=sapply(state.dim.names.by.race[-3], length),
                                     dimnames = state.dim.names.by.race[-3])

    rv$state.birth.rates = array(NA,
                                  dim=sapply(state.birth.rate.dim.names, length),
                                  dimnames = state.birth.rate.dim.names)
##-- Populate the arrays --##

    for (set.index in 1:length(county.age.labels))
    {
        set = nonhispanic.by.county[[set.index]]
        for (i in 1:dim(set)[1])
            rv$county.data.by.race[set$County.Code[i], set.index, MAP.RACES.FOR.FERTILITY[set$Bridged.Race[i]]] = set$Fertility.Rate[i]

        set = hispanic.by.county[[set.index]]
        for (i in 1:dim(set)[1])
            rv$county.data.by.race[set$County.Code[i], set.index, HISPANIC] = set$Fertility.Rate[i]

        set = all.by.county[[set.index]]
        for (i in 1:dim(set)[1])
            rv$county.data.all.races[set$County.Code[i], set.index] = set$Fertility.Rate[i]
    }

    for (set.index in 1:length(state.age.labels))
    {
        set = nonhispanic.by.state[[set.index]]
        for (i in 1:dim(set)[1])
            rv$state.data.by.race[set$State[i], set.index, MAP.RACES.FOR.FERTILITY[set$Bridged.Race[i]]] = set$Fertility.Rate[i]

        set = hispanic.by.state[[set.index]]
        for (i in 1:dim(set)[1])
            rv$state.data.by.race[set$State[i], set.index, HISPANIC] = set$Fertility.Rate[i]

        set = all.by.state[[set.index]]
        for (i in 1:dim(set)[1])
            rv$state.data.all.races[set$State[i], set.index] = set$Fertility.Rate[i]
    }

    #Birth rates
    set = non.hispanic.births.by.state
    for (i in 1:dim(set)[1])
        rv$state.birth.rates[set$State[i], MAP.RACES.FOR.FERTILITY[set$Bridged.Race[i]]] = set$Birth.Rate[i]

    set = hispanic.births.by.state
    for (i in 1:dim(set)[1])
        rv$state.birth.rates[set$State[i], HISPANIC] = set$Birth.Rate[i]


    set = non.hispanic.births
    for (i in 1:dim(set)[1])
        rv$county.birth.rates[set$County.Code[i], MAP.RACES.FOR.FERTILITY[set$Bridged.Race[i]]] = set$Birth.Rate[i]

    set = hispanic.births
    for (i in 1:dim(set)[1])
        rv$county.birth.rates[set$County.Code[i], HISPANIC] = set$Birth.Rate[i]


    #fill in missing rates with the state-level data
    for (county in counties)
    {
        missing = is.na(rv$county.birth.rates[county,])
        if (any(missing))
            rv$county.birth.rates[county,missing] = rv$state.birth.rates[state.for.county(county),missing]
    }

    #fill in missing rates for race with the overall county rate
    # At this point, this code is never actually used, because state-level data fills in
    set = all.births
    for (i in 1:dim(set)[1])
        rv$county.birth.rates[set$County.Code[i],
                              is.na(rv$county.birth.rates[set$County.Code[i],])] =
                set$Birth.Rate[i]

##-- Collapse last two age strata --##

    if (!is.na(collapse.last.two.ages.to))
    {

    ##-- Set up ages --##

        county.rates = get.fertility.rates(rv, fips=counties, aggregate.counties = F, per.population = per.population)

        n.old.county.ages = length(rv$age.county$labels)
        n.old.state.ages = length(rv$age.state$labels)

        old.county.penultimate.age = age.cutoffs.county[length(age.cutoffs.county)-1]
        old.state.penultimate.age = age.cutoffs.state[length(age.cutoffs.state)-1]

        age.cutoffs.county = c(age.cutoffs.county[1:(length(age.cutoffs.county)-2)], collapse.last.two.ages.to, Inf)
        age.cutoffs.state = c(age.cutoffs.state[1:(length(age.cutoffs.state)-2)], collapse.last.two.ages.to, Inf)

        rv$age.county = make.age.strata(age.cutoffs.county)
        rv$age.state = make.age.strata(age.cutoffs.state)

    ##-- Set up arrays --##
        county.dim.names.by.race = list(counties, rv$age.county$labels, BRIDGED.RACE.PLUS.HISPANIC)
        state.dim.names.by.race = list(states, rv$age.state$labels, BRIDGED.RACE.PLUS.HISPANIC)

        new.county.data.by.race = array(0,
                                       dim=sapply(county.dim.names.by.race, length),
                                       dimnames = county.dim.names.by.race)
        new.county.data.all.races = array(0,
                                         dim=sapply(county.dim.names.by.race[-3], length),
                                         dimnames = county.dim.names.by.race[-3])


        new.state.data.by.race = array(0,
                                      dim=sapply(state.dim.names.by.race, length),
                                      dimnames = state.dim.names.by.race)
        new.state.data.all.races = array(0,
                                        dim=sapply(state.dim.names.by.race[-3], length),
                                        dimnames = state.dim.names.by.race[-3])

    ##-- Copy over all but the last two new age strata --##

        n.new.county.ages = length(rv$age.county$labels)
        n.new.state.ages = length(rv$age.state$labels)

        if (n.new.county.ages > 2)
        {
            new.county.data.by.race[,1:(n.new.county.ages-2),] = rv$county.data.by.race[,1:(n.new.county.ages-2),]
            new.county.data.all.races[,1:(n.new.county.ages-2)] = rv$county.data.all.races[,1:(n.new.county.ages-2)]
        }

        if (n.new.state.ages > 2)
        {
            new.state.data.by.race[,1:(n.new.state.ages-2),] = rv$state.data.by.race[,1:(n.new.state.ages-2),]
            new.state.data.all.races[,1:(n.new.state.ages-2)] = rv$state.data.all.races[,1:(n.new.state.ages-2)]
        }

    ##-- Combine the last two age strata from the old into penultimate age stratum from the new --##

        county.female.populations.penultimate = get.census.data(census, years=years, fips=counties, races=rv$races, sexes='female',
                                                                ages = rv$age.county$lower[n.new.county.ages-1]:(old.county.penultimate.age-1),
                                                                aggregate.years=T, aggregate.ages=T, aggregate.sexes=T)
        county.female.populations.last = get.census.data(census, years=years, fips=counties, races=rv$races, sexes='female',
                                                                ages = old.county.penultimate.age:(rv$age.county$uppers[n.new.county.ages-1]-1),
                                                                aggregate.years=T, aggregate.ages=T, aggregate.sexes=T)
        county.female.penultimate.proportions.by.race = county.female.populations.penultimate /
                                                        (county.female.populations.penultimate + county.female.populations.last)
        county.female.penultimate.proportions.all.races = rowSums(county.female.populations.penultimate) /
                                                          (rowSums(county.female.populations.penultimate) + rowSums(county.female.populations.last))

        new.county.data.by.race[,n.new.county.ages-1,] = county.female.penultimate.proportions.by.race * county.rates[,n.old.county.ages-1,] +
                                                         (1-county.female.penultimate.proportions.by.race) * county.rates[,n.old.county.ages,]
        new.county.data.all.races[,n.new.county.ages-1] = county.female.penultimate.proportions.all.races * rv$county.data.all.races[,n.old.county.ages-1] +
                                                          (1-county.female.penultimate.proportions.all.races) * rv$county.data.all.races[,n.old.county.ages]

        state.female.populations.penultimate = t(sapply(states, function(state){
            fips = counties.for.state(state)
            get.census.data(census, years=years, fips=fips, races=rv$races, sexes='female',
                            ages = rv$age.county$lower[n.new.county.ages-1]:(old.county.penultimate.age-1),
                            aggregate.years=T, aggregate.counties=T, aggregate.ages=T, aggregate.sexes=T)
        }))

        state.female.populations.last = t(sapply(states, function(state){
            fips = counties.for.state(state)
            get.census.data(census, years=years, fips=fips, races=rv$races, sexes='female',
                            ages = old.county.penultimate.age:(rv$age.county$uppers[n.new.county.ages-1]-1),
                            aggregate.years=T, aggregate.counties=T, aggregate.ages=T, aggregate.sexes=T)
        }))


        state.female.penultimate.proportions.by.race = state.female.populations.penultimate /
                                                        (state.female.populations.penultimate + state.female.populations.last)
        state.female.penultimate.proportions.all.races = rowSums(state.female.populations.penultimate) /
                                                        (rowSums(state.female.populations.penultimate) + rowSums(state.female.populations.last))

        state.data.all.races.last = rv$state.data.all.races[,n.old.state.ages]
        state.data.all.races.last[is.na(state.data.all.races.last)] = 0

        new.state.data.by.race[,n.new.state.ages-1,] = state.female.penultimate.proportions.by.race * rv$state.data.by.race[,n.old.state.ages-1,] +
                                                        (1-state.female.penultimate.proportions.by.race) * rv$state.data.by.race[,n.old.state.ages,]
        new.state.data.all.races[,n.new.state.ages-1] = state.female.penultimate.proportions.all.races * rv$state.data.all.races[,n.old.state.ages-1] +
                                                        (1-state.female.penultimate.proportions.all.races) * state.data.all.races.last

    ##-- Overwrite old arrays --##
        rv$county.data.by.race = new.county.data.by.race
        rv$county.data.all.races = new.county.data.all.races
        rv$state.data.by.race = new.state.data.by.race
        rv$state.data.all.races = new.state.data.all.races
    }



##-- Return the result --##
    rv
}

get.birth.rates <- function(fertility.manager,
                                fips,
                                races=fertility.manager$races,
                                aggregate.counties=T,
                                aggregate.county.years=fertility.manager$years,
                                census=NULL,
                                per.population=1)
{
    fips = format.combined.fips(fips)

    #First-pass pull - by county and race
    dim.names = list(county=fips, race=races)

    fips.with.match = intersect(fips, fertility.manager$counties)
    fips.no.match = setdiff(fips, fips.with.match)
    rv = array(NA, dim=sapply(dim.names, length), dimnames = dim.names)

    rv[fips.with.match, races] = fertility.manager$county.birth.rates[fips.with.match, races]
    rv[fips.no.match, races] = fertility.manager$state.birth.rates[state.for.county(fips.no.match), races]

    #Aggregate across counties
    if (aggregate.counties & length(fips)==1)
        rv = rv[1,]
    else if (aggregate.counties)
    {
        populations = get.census.data(census, years=aggregate.county.years, fips=fips,
                                      races=races,
                                      aggregate.years=T, aggregate.ages=T, aggregate.sexes=T)

        rv = colSums(rv * populations) / colSums(populations)
    }

    #Multiply per population and return
    rv = rv * per.population / fertility.manager$per.population

    rv
}

get.fertility.rates <- function(fertility.manager,
                                fips,
                                ages=fertility.manager$age.county$labels,
                                races=fertility.manager$races,
                                aggregate.counties=T,
                                aggregate.county.years=fertility.manager$years,
                                census=NULL,
                                per.population=1)
{
    fips = format.combined.fips(fips)

    #First-pass pull - by county and race
    dim.names = list(county=fips, age=ages, race=races)

    fips.with.match = intersect(fips, fertility.manager$counties)
    fips.no.match = setdiff(fips, fips.with.match)
    rv = array(NA, dim=sapply(dim.names, length), dimnames = dim.names)

    rv[fips.with.match, ages, races] = fertility.manager$county.data.by.race[fips.with.match, ages, races]
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    N = prod(dim(rv))

    #Second-pass pull - by county (all race)
    missing = as.logical(is.na(rv))
    if (any(missing))
    {
        fips.array = age.array = race.array = array('', dim=sapply(dim.names, length))
        for (i in 1:length(fips))
            fips.array[i,,] = fips[i]
        for (i in 1:length(ages))
            age.array[,i,] = ages[i]
        for (i in 1:length(races))
            race.array[,,i] = races[i]

#        print("There were low counts for the following strata: ")
#        for (i in (1:length(rv))[missing])
#            print(paste0("  ", state.array[i], ' x ', age.array[i], ' x ', race.array[i], ' x ', sex.array[i]))
#        print("All-race mortality rates will be used for these strata.")

        for (i in (1:N)[missing])
        {
            if (any(fips.with.match==fips.array[i]))
                rv[i] =  fertility.manager$county.data.all.races[fips.array[i], age.array[i]]
        }
    }


    #Third-pass pull - by state and race
    missing = as.logical(is.na(rv))
    if (any(missing))
    {
        for (i in (1:N)[missing])
            rv[i] =  fertility.manager$state.data.by.race[state.for.county(fips.array[i]), age.array[i], race.array[i]]
    }

    #Fourth-pass pull - by state (all races)
    missing = as.logical(is.na(rv))
    if (any(missing))
    {
        for (i in (1:N)[missing])
            rv[i] =  fertility.manager$state.data.all.races[state.for.county(fips.array[i]), age.array[i]]
    }

    #Fifth-pass pull - plug in zero
    missing = as.logical(is.na(rv))
    if (any(missing))
    {
        rv[(1:N)[missing]] = 0
    }

    #Aggregate across counties
    if (aggregate.counties & length(fips)==1)
        rv = rv[1,,]
    else if (aggregate.counties)
    {
        female.populations = array(NA, dim=dim(rv), dimnames=dimnames(rv))
        for (age in ages)
        {
            age.index = (1:length(fertility.manager$age.county$labels))[fertility.manager$age.county$labels==age]
            age.lower = fertility.manager$age.county$lowers[age.index]
            age.upper = min(fertility.manager$age.county$uppers[age.index]-1, max(census$age.lowers))

            female.populations[,age,] = get.census.data(census, years=aggregate.county.years, fips=fips,
                                                        ages=age.lower:age.upper,
                                                        races=races, sexes='female',
                                                        aggregate.years=T, aggregate.ages=T, aggregate.sexes=T)
        }

        rv = colSums(rv * female.populations) / colSums(female.populations)
    }

    #Multiply per population and return
    rv = rv * per.population / fertility.manager$per.population

    rv
}


read.natality.file <- function(file,
                               census,
                               years,
                               age.lower,
                               age.upper,
                               all.races=F,
                               hispanic.only=F,
                               is.state=F,
                               per.population=1000)
{
    df = read.table(file, header=T, sep='\t', stringsAsFactors = F)
    df = df[is.na(df$Notes) | df$Notes != 'Total',]
    if (!is.state)
    {
        df = df[!grepl('Unidentified Counties', df$County),]
        df$County.Code = format.combined.fips(df$County.Code)
    }
    else
        df$State = state.name.to.abbreviation(df$State)

    df$Births[df$Births=='Suppressed' | df$Births=='Missing County'] = NA
    df$Births = as.integer(df$Births)

    if (any(names(df)=='Fertility.Rate'))
    {
        df$Fertility.Rate[df$Fertility.Rate=='Suppressed' | df$Fertility.Rate=='Unreliable' | df$Fertility.Rate=='Not Available'] = NA
        df$Fertility.Rate = as.numeric(df$Fertility.Rate)
    }

    if (any(names(df)=='Birth.Rate'))
    {
        df$Birth.Rate[df$Birth.Rate=='Suppressed' | df$Birth.Rate=='Unreliable' | df$Birth.Rate=='Not Available' | df$Birth.Rate=='Missing County'] = NA
        df$Birth.Rate = as.numeric(df$Birth.Rate)
    }

    missing.denominator=F
    if (any(names(df)=='Female.Population'))
    {
        df$Female.Population[df$Female.Population=='Not Available'] = NA
        df$Female.Population = as.integer(df$Female.Population)

        missing.denominator = is.na(df$Female.Population) & !is.na(df$Births) & is.na(df$Fertility.Rate)
    }


    if (any(missing.denominator))
    {
        N = length(missing.denominator)

        if (age.upper==Inf)
            age.upper = max(census$age.lowers)

        denominators = sapply((1:N)[missing.denominator], function(i){
            if (is.state)
                fips = counties.for.state(df$State[i])
            else
                fips = format.combined.fips(df$County.Code[i])

            if (all.races)
                races = census$races
            else if (hispanic.only)
                races = HISPANIC
            else
                races = MAP.RACES.FOR.FERTILITY[df$Bridged.Race[i]]

            census.data = get.census.data(census,
                                          years=years,
                                          fips=fips,
                                          ages=as.character(age.lower:age.upper),
                                          races=races,
                                          sexes='female',
                                          aggregate.years=T,
                                          aggregate.counties=T,
                                          aggregate.ages=T,
                                          aggregate.races=T,
                                          aggregate.sexes=T)
        })

        df$Fertility.Rate[missing.denominator] = per.population * df$Births[missing.denominator] / denominators
    }

    names(df)[names(df)=='Mother.s.Bridged.Race'] = 'Bridged.Race'

    df
}


#testing
if (1==2)
{
    df = read.natality.file('../data2/Natality/Non-hispanic 25-34 natality by county.txt')
    read('../data2/parsed/five_age_census.Rdata')

    cd = get.census.data(census, fips=BALTIMORE, years=2007:2017)

    cd['2017',1,2,,'female']
    colSums(cd[,1,2,,'female'])
    df[df$County.Code==BALTIMORE,]

    mask = df$County.Code==BALTIMORE
    given.rates = df$Fertility.Rate[mask]
    calc.rates = df$Births[mask] / colSums(cd[,1,2,,'female'])[c(3,4,2,1)] * 1000
    cbind(given.rates, calc.rates)


    get.birth.rates(ALL.DATA.MANAGERS$fertility,
                    BALTIMORE.MSA.COUNTIES,
                    census=ALL.DATA.MANAGERS$census.collapsed)
}
