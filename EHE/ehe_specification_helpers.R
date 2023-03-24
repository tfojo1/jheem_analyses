
DEFAULT.POPULATION.YEARS = 2007

TRATE.KNOT.TIMES = c(rate0=2000, rate1=2010, rate2=2020)
TRATE.AFTER.TIME = 2030

CENSUS.BHO.RACE.MAPPING = create.other.catchall.ontology.mapping('race', 
                                                                 from.values=ALL.DATA.MANAGERS$census.full$races,
                                                                 to.values=c('black','hispanic','other'))

##-----------##
##-- Aging --##
##-----------##

get.default.aging.rates <- function(location, specification.info,
                                    active.idu.proportion.youngest.stratum.aging.up=0.25,
                                    prior.idu.proportion.youngest.stratum.aging.up=0.25)
{
    rv = array(1 / specification.info$age.spans,
               dim=sapply(specification.info$dim.names[c('age','risk')], length),
               dimnames=specification.info$dim.names[c('age','risk')])
    
    rv[1,specification.info$compartment.aliases$active.idu.states] = active.idu.proportion.youngest.stratum.aging.up
    rv[1,specification.info$compartment.aliases$prior.idu.states] = prior.idu.proportion.youngest.stratum.aging.up
    
    rv
}


##--------------------##
##-- Proportion MSM --##
##--------------------##

get.proportion.msm.of.male.by.race.functional.form <- function(location, specification.info)
{
    best.guess.proportions = get.best.guess.msm.proportions.by.race(counties.for.msa(location),
                                                                    census = ALL.DATA.MANAGERS$census.full,
                                                                    years = DEFAULT.POPULATION.YEARS,
                                                                    min.age = specification.info$age.lower.bounds[1])
    
    best.guess.proportions = array(best.guess.proportions, 
                                   dim=c(race=length(best.guess.proportions)),
                                   dimnames=list(race=names(best.guess.proportions)))
    
    create.static.functional.form(best.guess.proportions,
                                  link = 'log',
                                  value.is.on.transformed.scale = F)
}

# Assumes that within each county, relative risks of being MSM are as in MSM.PROPORTIONS
# and total risk of being MSM is as per read.msm.proportions
get.best.guess.msm.proportions.by.race <- function(fips,
                                                   census,
                                                   min.age=0,
                                                   years=DEFAULT.POPULATION.YEARS,
                                                   msm.proportions.by.race = c(black=1-.806, hispanic=1-.854, other=1-.848),
                                                   # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4516312/
                                                   return.proportions=T,
                                                   keep.ages = F)
{
    #    props = read.msm.proportions()[as.character(fips)]
    props = ALL.DATA.MANAGERS$msm.proportions[as.character(fips)]
    males = get.census.data(census, years=years, fips=fips,
                            sexes = 'male',
                            ages = census$age.names[census$age.lowers>=min.age],
                            aggregate.ages = !keep.ages,
                            aggregate.years = T,
                            aggregate.races = F,
                            aggregate.sexes = T)
    
    males = CENSUS.BHO.RACE.MAPPING$apply(males)

    iterated.msm.proportions.by.race = rep(msm.proportions.by.race, each=prod(dim(males))/dim(males)['race'])
    numerators = props * iterated.msm.proportions.by.race * males *
        rowSums(males) / rowSums(males * iterated.msm.proportions.by.race) 
    
    
    if (return.proportions)
        colSums(numerators) / colSums(males)
    else
        colSums(numerators) / length(years)
}

##---------------##
##-- Fertility --##
##---------------##

get.model.fertility.rates <- function(location, specification.info, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = counties.for.msa(location)
    
    # Get the population (to weight as we aggregate across races)
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         year=population.years, aggregate.years = T) / length(population.years)
    population = colSums(county.populations)
    
    # Weight rates by population
    birth.rates = get.birth.rates(ALL.DATA.MANAGERS$fertility,
                                  fips=counties,
                                  census=ALL.DATA.MANAGERS$census.collapsed,
                                  aggregate.counties = T)
    birth.rates = array(birth.rates, dim=c(race=length(birth.rates)), dimnames=list(race=names(birth.rates)))
    pop.by.race = apply(population, 'race', sum)
    pop.by.race = array(pop.by.race, dim=c(race=length(pop.by.race)), dimnames=list(race=names(pop.by.race)))
    
    numerators = CENSUS.BHO.RACE.MAPPING$apply(pop.by.race * birth.rates)
    denominators = CENSUS.BHO.RACE.MAPPING$apply(pop.by.race)
    
    numerators / denominators
}


##---------------##
##-- MORTALITY --##
##---------------##

get.non.idu.general.mortality.rates <- function(location, specification.info, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = counties.for.msa(location)
    
    # Get the raw rates
    raw.mortality.rates = get.mortality.rates(ALL.DATA.MANAGERS$mortality, states=states.for.msa(location), verbose=F)
    
    # Get the population (to weight as we aggregate across races)
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         year=population.years, aggregate.years = T) / length(population.years)
    population = colSums(county.populations)
    
    # Weight rates by population
    mortality.numerators = rowSums(sapply(counties, function(county){
        county.populations[county,,,] * raw.mortality.rates[state.for.county(county),,,]
    }))
    mortality.rates.all.counties = mortality.numerators / population
    
    numerators = CENSUS.BHO.RACE.MAPPING$apply(population * mortality.rates.all.counties)
    denominators = CENSUS.BHO.RACE.MAPPING$apply(population)
    
    mortality.rates = numerators / denominators
    
    # Stratify into MSM
    stratify.males.by.orientation(mortality.rates, msm.multiplier = 1, heterosexual.multiplier = 1) #this function is in the JHEEM package
}

##------------------------##
##-- Initial Population --##
##------------------------##

get.base.initial.male.population <- function(location, specification.info, years=DEFAULT.POPULATION.YEARS)
{
    if (length(specification.info$dim.names$location)>1)
        stop("We need to specify what to do with more than one location")
    
    counties = counties.for.msa(location)
    
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         sexes = 'male', aggregate.sexes = T,
                                         aggregate.counties = T,
                                         year=years, aggregate.years = T) / length(years)
    
    CENSUS.BHO.RACE.MAPPING$apply(county.populations)
}

get.base.initial.female.population <- function(location, specification.info, years=DEFAULT.POPULATION.YEARS)
{
    if (length(specification.info$dim.names$location)>1)
        stop("We need to specify what to do with more than one location")
    
    counties = counties.for.msa(location)
    
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         sexes = 'female', aggregate.sexes = T,
                                         aggregate.counties = T,
                                         year=years, aggregate.years = T) / length(years)
    
    CENSUS.BHO.RACE.MAPPING$apply(county.populations)
}

get.location.ever.idu.prevalence <- function(location, specification.info)
{
    counties = counties.for.msa(location)
    
    idu.ever.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, 
                                             ALL.DATA.MANAGERS$census.full.msm, 
                                             age.cutoffs = specification.info$age.endpoints,
                                             use.ever=T, 
                                             counties=counties, 
                                             aggregate.counties = T)
    
    idu.ever.prevalence
}

get.location.active.idu.prevalence <- function(location, specification.info)
{
    counties = counties.for.msa(location)
    
    idu.30d.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, 
                                            ALL.DATA.MANAGERS$census.full.msm, 
                                            age.cutoffs = specification.info$age.endpoints,
                                            use.30d=T, 
                                            counties=counties,
                                            aggregate.counties = T)
    
    idu.30d.prevalence
}

get.seed.rate.per.stratum <- function(location, specification.info, population.year=DEFAULT.POPULATION.YEARS)
{
    prevalence.sex.age = get.surveillance.data.rate(msa.surveillance,
                                                    location.codes=location,
                                                    data.type='prevalence',
                                                    census=ALL.DATA.MANAGERS$census.collapsed,
                                                    age=T, sex=T, 
                                                    years=intersect(ALL.DATA.MANAGERS$census.collapsed$years,
                                                                    dimnames(msa.surveillance$prevalence.sex.age)$year),
                                                    throw.error.if.missing.data=F)
    
    prevalence.sex.race = get.surveillance.data.rate(msa.surveillance,
                                                     location.codes=location,
                                                     data.type='prevalence',
                                                     census=ALL.DATA.MANAGERS$census.collapsed,
                                                     race=T, sex=T,
                                                     years=intersect(ALL.DATA.MANAGERS$census.collapsed$years,
                                                                     dimnames(msa.surveillance$prevalence.sex.race)$year),
                                                     throw.error.if.missing.data=F)
    
    prev.years = as.numeric(dimnames(prevalence.sex.age)[['year']])
    diff.to.pop.year = abs(prev.years - population.year)
    prev.year = prev.years[diff.to.pop.year==min(diff.to.pop.year)][1]
    
    if (is.null(prevalence.sex.race) && is.null(prevalence.sex.age))
        min.prevalence = min(get.surveillance.data.rate(msa.surveillance,
                                                        location.codes=location,
                                                        data.type='prevalence',
                                                        census=ALL.DATA.MANAGERS$census.collapsed,
                                                        years=intersect(ALL.DATA.MANAGERS$census.collapsed$years,
                                                                        dimnames(msa.surveillance$prevalence.all)$year),
                                                        throw.error.if.missing.data=T),
                             na.rm=T)
    else
    {
        min.prevalence = suppressWarnings(min(access(prevalence.sex.age, year=as.character(prev.year)),
                                              access(prevalence.sex.race, year=as.character(prev.year), race=c('black','hispanic','other')),
                                              na.rm=T))
        
        if (is.infinite(min.prevalence))
            min.prevalence = min(prevalence.sex.age, prevalence.sex.race, na.rm=T)
    }
    
    min.prevalence
}


#--------------------#
#-- Contact Arrays --#
#--------------------#

get.female.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = counties.for.msa(location)
    rv = get.census.data(ALL.DATA.MANAGERS$census.full, 
                         fips = counties,
                         years = population.years,
                         sexes = 'female',
                         aggregate.years = T,
                         aggregate.counties = T,
                         aggregate.ages = F,
                         aggregate.races = T,
                         aggregate.sexes = T) / length(population.years)
    array(rv, dim=c(age=length(rv)), dimnames=list(age=names(rv)))
}

get.msm.single.year.age.counts <- function(location, specification.info,
                                           population.years=DEFAULT.POPULATION.YEARS)
{
    counties = counties.for.msa(location)
    rv = rowSums(get.best.guess.msm.proportions.by.race(fips=counties,
                                                        census = ALL.DATA.MANAGERS$census.full, 
                                                        years = population.years,
                                                        return.proportions = F,
                                                        min.age = 0,
                                                        keep.ages = T))
    
    ages = names(rv)
    dim(rv) = c(age=length(rv))
    dimnames(rv) = list(age=ages)
    
    rv
}

get.heterosexual.male.single.year.age.counts <- function(location, specification.info,
                                                         population.years=DEFAULT.POPULATION.YEARS)
{
    counties = counties.for.msa(location)
    get.census.data(ALL.DATA.MANAGERS$census.full,
                    fips=counties,
                    years = population.years,
                    sexes = 'male',
                    aggregate.years = T,
                    aggregate.counties = T,
                    aggregate.ages = F,
                    aggregate.races = T,
                    aggregate.sexes = T) / length(population.years) - 
        get.msm.single.year.age.counts(location=location, specification.info=specification.info,
                                       population.years=population.years)
}


get.msm.sexual.age.contact.proportions <- function(age.mixing.sd.mult,
                                                   single.year.msm.age.counts,
                                                   single.year.age.sexual.availability,
                                                   specification.info)
{
    do.get.age.contact.proportions.for.model(specification.info=specification.info,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$msm,
                                             age.counts = single.year.msm.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.heterosexual.male.sexual.age.contact.proportions <- function(age.mixing.sd.mult, 
                                                                 single.year.heterosexual.male.age.counts,
                                                                 single.year.age.sexual.availability,
                                                                 specification.info)
{
    do.get.age.contact.proportions.for.model(specification.info=specification.info,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$heterosexual_male,
                                             age.counts = single.year.heterosexual.male.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.female.sexual.age.contact.proportions <- function(age.mixing.sd.mult, 
                                                      single.year.female.age.counts,
                                                      single.year.age.sexual.availability,
                                                      specification.info)
{
    do.get.age.contact.proportions.for.model(specification.info=specification.info,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$female,
                                             age.counts = single.year.female.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.idu.age.contact.proportions <- function(age.mixing.sd.mult,
                                            single.year.age.counts,
                                            single.year.age.idu.availability,
                                            specification.info)
{
    do.get.age.contact.proportions.for.model(specification.info=specification.info,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$idu.age.model,
                                             age.counts = single.year.age.counts,
                                             availability = single.year.age.idu.availability)
}

get.idu.sex.contact.proportions <- function(idu.transmission.sex.oes,
                                            base.initial.population)
{
    get.pairing.proportions(oe.ratios = idu.transmission.sex.oes,
                            marginal.counts = apply(base.initial.population, 'sex', sum))
}


do.get.age.contact.proportions.for.model <- function(specification.info,
                                                     location,
                                                     age.mixing.sd.mult,
                                                     age.model,
                                                     age.counts,
                                                     availability)
    
{
    #-- Call the function --#
    age.cutoffs = specification.info$age.endpoints
    age.cutoffs[length(age.cutoffs)] = min(age.cutoffs[length(age.cutoffs)],
                                           max(as.numeric(names(age.counts))))
    
    get.age.mixing.proportions(age.delta.intercept.mean=age.model['mean.intercept'],
                               age.delta.slope.mean=age.model['mean.slope'],
                               age.delta.intercept.sd=age.model['sd.intercept'],
                               age.delta.slope.sd=age.model['sd.slope'],
                               age.cutoffs=age.cutoffs,
                               age.labels=specification.info$dim.names$age,
                               single.year.age.counts=age.counts*availability,
                               sd.multiplier=age.mixing.sd.mult)
}

get.sexual.availability <- function()
{
    rv = rep(1, length(ALL.DATA.MANAGERS$census.full$age.names))
    names(rv) = ALL.DATA.MANAGERS$census.full$age.names
    
    # Assume no sex under 13
    rv[as.character(0:12)] = 0
    
    # From Abma 2017
    availability.13.19 = c('13'=.076, #from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6547075/
                           '14'=.108, #halfway between
                           '15'=.13,
                           '16'=.26,
                           '17'=.41,
                           '18'=.55,
                           '19'=.68) / .75
    rv[names(availability.13.19)] = availability.13.19
    
    #from Tessler 2008
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2426743/
    rv[as.character(65:74)] = mean(c(67.0/83.7,39.5/61.6))
    rv[as.character(75:as.numeric(names(rv)[length(rv)]))] = mean(c(38.5/83.7,16.7/61.6))
    
    # Return
    array(rv, dim=c(age=length(rv)), dimnames=list(age=names(rv)))
}

get.idu.availability <- function()
{
    rv = rep(1, length(ALL.DATA.MANAGERS$census.full$age.names))
    names(rv) = ALL.DATA.MANAGERS$census.full$age.names
    
    # Assume no IDU under 13
    rv[as.character(0:12)] = 0
    
    #from NSDUH 2015-2018
    availability.13.24 = get.idu.availability.13.24()
    for (age.bracket in names(availability.13.24))
    {
        ages = as.numeric(substr(age.bracket, 1,2)):as.numeric(substr(age.bracket, 4,5))
        rv[as.character(ages)] = availability.13.24[age.bracket]
    }
    
    #NSDUH 2015 and 2016 - 
    # https://www.samhsa.gov/data/report/results-2016-national-survey-drug-use-and-health-detailed-tables
    older.idu.availability = (4.2 + 5.3) / (14.1+10.1  + 10.0+15.0)
    rv[as.character(65:as.numeric(names(rv)[length(rv)]))] = older.idu.availability
    
    # Return
    array(rv, dim=c(age=length(rv)), dimnames=list(age=names(rv)))
}