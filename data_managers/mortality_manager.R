#source('../code/data_managers/census_manager.R')

MAP.RACES.FOR.MORTALITY = c('White'='white',
                            'Black or African American'='black',
                            'American Indian or Alaska Native'='american_indian_or_alaska_native',
                            'Asian or Pacific Islander'='asian')

read.mortality <- function(dir='../data2/Mortality/', suffix=' mortality metro 2007-2017.txt')
{
    # Read in files
    hispanic.13.24 = read.mortality.file(paste0(dir, 'Hispanic 13-24', suffix))
    hispanic.25.54 = read.mortality.file(paste0(dir, 'Hispanic 25-54', suffix))
    hispanic.55.plus = read.mortality.file(paste0(dir, 'Hispanic 55+', suffix))

    nonhispanic.13.24 = read.mortality.file(paste0(dir, 'Non-hispanic 13-24', suffix))
    nonhispanic.25.54 = read.mortality.file(paste0(dir, 'Non-hispanic 25-54', suffix))
    nonhispanic.55.plus = read.mortality.file(paste0(dir, 'Non-hispanic 55+', suffix))

    all.13.24 = read.mortality.file(paste0(dir, 'All race 13-24', suffix))
    all.25.54 = read.mortality.file(paste0(dir, 'All race 25-54', suffix))
    all.55.plus = read.mortality.file(paste0(dir, 'All race 55+', suffix))

    # Set up the return value
    rv = list()
    rv$per.population = 100000

    # Set up the arrays and indices
    states = unique(all.25.54$State)
    states = states[states!='']

    rv$ages = ages = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years')
    rv$sexes = sexes = c('male', 'female')

    rv$races = BRIDGED.RACE.PLUS.HISPANIC

    dim.names.by.race = list(state=states, age=ages, race=BRIDGED.RACE.PLUS.HISPANIC, sex=sexes)
    rv$data.by.race = array(NA,
                            dim=sapply(dim.names.by.race, length),
                            dimnames = dim.names.by.race)
    rv$data.all.races = array(NA,
                              dim=sapply(dim.names.by.race[-3], length),
                              dimnames = dim.names.by.race[-3])

    # Populate the arrays

    ## Non-hispanic
    set = nonhispanic.13.24
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], 1, MAP.RACES.FOR.MORTALITY[set$Race[i]], set$Gender[i]] = set$Crude.Rate[i]
    }

    set = nonhispanic.25.54
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], set$Ten.Year.Age.Groups[i], MAP.RACES.FOR.MORTALITY[set$Race[i]], set$Gender[i]] = set$Crude.Rate[i]
    }

    set = nonhispanic.55.plus
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], 5, MAP.RACES.FOR.MORTALITY[set$Race[i]], set$Gender[i]] = set$Crude.Rate[i]
    }

    ## Hispanic
    set = hispanic.13.24
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], 1, HISPANIC, set$Gender[i]] = set$Crude.Rate[i]
    }

    set = hispanic.25.54
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], set$Ten.Year.Age.Groups[i], HISPANIC, set$Gender[i]] = set$Crude.Rate[i]
    }

    set = hispanic.55.plus
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.by.race[set$State[i], 5, HISPANIC, set$Gender[i]] = set$Crude.Rate[i]
    }

    ## All Race
    set = all.13.24
    set = set[set$Notes != 'Total',]
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.all.races[set$State[i], 1, set$Gender[i]] = set$Crude.Rate[i]
    }

    set = all.25.54
    set = set[set$Notes != 'Total',]
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.all.races[set$State[i], set$Ten.Year.Age.Groups[i], set$Gender[i]] = set$Crude.Rate[i]
    }

    set = all.55.plus
    set = set[set$Notes != 'Total',]
    set$Gender = tolower(set$Gender)
    for (i in 1:dim(set)[1])
    {
        rv$data.all.races[set$State[i], 5, set$Gender[i]] = set$Crude.Rate[i]
    }

    # Re-index by state abbreviation
    rv$states = dimnames(rv$data.by.race)[[1]] = dimnames(rv$data.all.races)[[1]] = state.name.to.abbreviation(states)

    # Return the result
    rv
}

read.mortality.file <- function(file)
{
    df = read.table(file, header=T, sep='\t', stringsAsFactors = F)

    df$Deaths[df$Deaths=='Suppressed'] = NA
    df$Deaths = as.integer(df$Deaths)

    df$Crude.Rate[df$Crude.Rate=='Suppressed' | df$Crude.Rate=='Unreliable'] = NA
    df$Crude.Rate = as.numeric(df$Crude.Rate)

    df
}

get.mortality.rates <- function(mortality.manager,
                                states=mortality.manager$states,
                                ages=mortality.manager$ages,
                                races=mortality.manager$races,
                                sexes=mortality.manager$sexes,
                                per.population = 1,
                                verbose=T)
{
    #map to state abbreviations
    orig.states = states
    unmatched.states = sapply(states, function(st){all(st!=mortality.manager$states)})
    if (any(unmatched.states))
    {
        states[unmatched.states] = state.name.to.abbreviation(states[unmatched.states])
        unmatched.states = is.na(states)
    }
    if (any(unmatched.states))
    {
        states[unmatched.states] = state.fips.to.abbreviation(orig.states[unmatched.states])
        unmatched.states = is.na(states)
    }

    unmatched.states[!unmatched.states] = sapply(states[!unmatched.states], function(st){all(st!=mortality.manager$states)})
    if (any(unmatched.states))
        stop(paste0("The mortality manager does not have any data on the following states: ", paste0(paste0("'", orig.states[unmatched.states], "'"), collapse=', ')))

    #extract the data by race
    dim.names = list(state=states, age=ages, race=races, sex=sexes)
    rv = mortality.manager$data.by.race[states, ages, races, sexes]
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names

    #fill in any missing value with all-race mortality
    missing = as.logical(is.na(rv))
    if (any(missing))
    {
        state.array = age.array = race.array = sex.array = array('', dim=sapply(dim.names, length))
        for (i in 1:length(states))
            state.array[i,,,] = states[i]
        for (i in 1:length(ages))
            age.array[,i,,] = ages[i]
        for (i in 1:length(races))
            race.array[,,i,] = races[i]
        for (i in 1:length(sexes))
            sex.array[,,,i] = sexes[i]

        state.array = as.character(state.array)
        age.array = as.character(age.array)
        race.array = as.character(race.array)
        sex.array = as.character(sex.array)

        if (verbose)
        {
            print("There were low counts for the following strata: ")
            for (i in (1:length(rv))[missing])
                print(paste0("  ", state.array[i], ' x ', age.array[i], ' x ', race.array[i], ' x ', sex.array[i]))
            print("All-race mortality rates will be used for these strata.")
        }

        rv = as.numeric(rv)
        for (i in (1:length(rv))[missing])
        {
            rv[i] = mortality.manager$data.all.races[state.array[i], age.array[i], sex.array[i]]
            if (is.na(rv[i]))
            {
                age.index = (1:length(mortality.manager$ages))[mortality.manager$ages==age.array[i]]
                nearest.age = (1:length(mortality.manager$ages))[order(abs(age.index-1:length(mortality.manager$ages)))][2]

                rv[i] = mortality.manager$data.all.races[state.array[i], nearest.age, sex.array[i]]
            }
        }

        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
    }

    rv = rv * per.population / mortality.manager$per.population

    rv
}
