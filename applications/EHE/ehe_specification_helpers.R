
# This code depends on
# commoncode/age_smoothers.R

##--------------------##
##-- Some Constants --##
##--------------------##

DEFAULT.POPULATION.YEARS = 2007

TRATE.KNOT.TIMES = c(rate0=2000, rate1=2010, rate2=2020)
TRATE.AFTER.TIME = 2030

CENSUS.BHO.RACE.MAPPING = create.other.catchall.ontology.mapping('race', 
                                                                 from.values=ALL.DATA.MANAGERS$census.full$races,
                                                                 to.values=c('black','hispanic','other'))

##-----------##
##-- Aging --##
##-----------##

get.default.aging.rates <- function(location, specification.metadata,
                                    active.idu.proportion.youngest.stratum.aging.up=0.25,
                                    prior.idu.proportion.youngest.stratum.aging.up=0.25)
{
    dim.names = specification.metadata$dim.names[c('age','risk')]
    dim.names$age = dim.names$age[-length(dim.names$age)]
    rv = array(1 / specification.metadata$age.spans[-specification.metadata$n.ages],
               dim=sapply(dim.names, length),
               dimnames=dim.names)
    
    rv[1,specification.metadata$compartment.aliases$active.idu.states] = active.idu.proportion.youngest.stratum.aging.up
    rv[1,specification.metadata$compartment.aliases$prior.idu.states] = prior.idu.proportion.youngest.stratum.aging.up
    
    rv
}

get.empiric.aging.rates <- function(location, specification.metadata,
                                    years=c(2010,2020,2030,2040),
                                    active.idu.proportion.youngest.stratum.aging.up=0.25,
                                    prior.idu.proportion.youngest.stratum.aging.up=0.25)
{
    counties = locations::get.contained.locations(location, 'county')
    
    years.to.pull = min(years):max(years)
    
    pop = CENSUS.MANAGER$pull(outcome='population',
                              dimension.values = list(location = counties,
                                                      year = as.character(years.to.pull)),
                              keep.dimensions = c('year','age','race','ethnicity','sex'))
    
    if (is.null(pop))
        stop("There was no population data at all for location ", location, " between ", min(years), " and ", max(years))
    
    max.year = max(as.numeric(dimnames(pop)$year))
    min.year = min(as.numeric(dimnames(pop)$year))
    
    race.mapping = get.ontology.mapping(dimnames(pop)[c('race','ethnicity')],
                                        specification.metadata$dim.names['race'])
    
    pop = apply(pop, c('year','age','race','ethnicity','sex'), sum, na.rm=T)
    pop = race.mapping$apply(pop)
    
    aging.rates = lapply(years, function(year){
      year = as.character(min(max(year,min.year),max.year))
      
        raw.rates = sapply(1:(specification.metadata$n.ages-1), function(age.index){
            
            age.upper.bound = specification.metadata$age.upper.bounds[age.index] - 1
            age.bracket.ages = specification.metadata$age.lower.bounds[age.index]:age.upper.bound
          
            pop[year, paste0(age.upper.bound, ' years'),,] /
              colSums(pop[year, paste0(age.bracket.ages, ' years'),,], dims=1)
        })
        
        dim.names = c(dimnames(pop)[c('race','sex')], list(age=specification.metadata$dim.names$age[-1]))
        dim(raw.rates) = sapply(dim.names, length)
        dimnames(raw.rates) = dim.names
        
        desired.dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
        desired.dim.names$age = desired.dim.names$age[-length(desired.dim.names$age)]
        
        sex.mapping = c(msm='male', heterosexual_male='male', female='female')
        rates.by.sex = sapply(desired.dim.names$sex, function(sex){
          
            sex.from = sex.mapping[sex]
            if (is.na(sex.from))
                stop("'get.empiric.aging.rates' in the ehe_specification_helpers is hard-coded for sex = [msm, heterosexual_male, female]. You will need to modify this function to accomodate additional sex categories")
            
            raw.rates[,sex.from,]
        })
        
        dim.names = desired.dim.names[c('race','age','sex')]
        dim(rates.by.sex) = sapply(dim.names, length)
        dimnames(rates.by.sex) = dim.names
        
        rv = expand.array(rates.by.sex, target.dim.names = desired.dim.names)
        
        rv[1,,,specification.metadata$compartment.aliases$active.idu.states] = active.idu.proportion.youngest.stratum.aging.up
        rv[1,,,specification.metadata$compartment.aliases$prior.idu.states] = prior.idu.proportion.youngest.stratum.aging.up
        
        rv
    })
    
    names(aging.rates) = as.character(years)
    names(years) = as.character(years)
    
    create.natural.spline.functional.form(knot.times = years,
                                          knot.values = aging.rates,
                                          link = 'log',
                                          knots.are.on.transformed.scale = F)
                                          
}

##--------------------##
##-- Proportion MSM --##
##--------------------##

get.proportion.msm.of.male.by.race.functional.form <- function(location, specification.metadata)
{
    best.guess.proportions = get.best.guess.msm.proportions.by.race(get.contained.locations(location, 'county'),
                                                                    census = ALL.DATA.MANAGERS$census.full,
                                                                    years = DEFAULT.POPULATION.YEARS,
                                                                    min.age = specification.metadata$age.lower.bounds[1])
    
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
    
    to.dim.names = dimnames(males)
    to.dim.names$race = names(msm.proportions.by.race)
    males = CENSUS.BHO.RACE.MAPPING$apply(males, to.dim.names = to.dim.names)
    
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

# for this to be different, will also need to add alphas into ehe_parameters and map them in ehe_parameter_mapping
get.model.fertility.rates.functional.form = function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.model.fertility.rates(location=location,
                                    specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
  
  #create.log.linear.functional.form(intercept = rates,
  #                                  slope = 1, # log(1) = 0, so this means no slope on the non-transformed scale
  #                                  parameters.are.on.log.scale = F,
  #                                  anchor.year = 2007)
  
}

get.model.fertility.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
    
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

get.non.idu.general.mortality.rates.functional.form = function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.non.idu.general.mortality.rates(location=location,
                                              specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
}

get.non.idu.general.mortality.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
    
    # Get the raw rates
    raw.mortality.rates = get.mortality.rates(ALL.DATA.MANAGERS$mortality, 
                                              states=get.containing.locations(location, 'state'), 
                                              verbose=F)
    
    # Get the population (to weight as we aggregate across races)
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         year=population.years, aggregate.years = T) / length(population.years)
    population = colSums(county.populations)
    
    # Weight rates by population
    mortality.numerators = rowSums(sapply(counties, function(county){
        county.populations[county,,,] * 
            raw.mortality.rates[get.containing.locations(county, 'state'),,,]
    }))
    mortality.rates.all.counties = mortality.numerators / population
    
    numerators = CENSUS.BHO.RACE.MAPPING$apply(population * mortality.rates.all.counties)
    denominators = CENSUS.BHO.RACE.MAPPING$apply(population)
    
    mortality.rates = numerators / denominators
    
    # Stratify into MSM
    stratify.males.by.orientation(mortality.rates, msm.multiplier = 1, heterosexual.multiplier = 1) #this function is in the JHEEM package
}

##---------------##
##-- Migration --##
##---------------##


get.immigration.rates.functional.form <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.immigration.rates(location=location,
                                specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
}

get.immigration.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  # this will be one top-level beta for the MSA, then we'll include alphas by strata (race and age only, not sex)? 

  immigration.numbers = SURVEILLANCE.MANAGER$pull(outcome = "immigration",
                                                location = location,
                                                year = "2011-2015") / 5 # because it's 5-year aggregate data     

  population = mean(SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                                           location = location,
                                           year = as.character(c(2011:2015)))) 

  immigration.rates = immigration.numbers/population
  
  c(immigration.rates)
}

get.emigration.rates.functional.form <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.emigration.rates(location=location,
                               specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
}

get.emigration.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  # this will be one top-level beta for the MSA, then we'll include alphas by strata (race and age only, not sex)? 
  emigration.numbers = SURVEILLANCE.MANAGER$pull(outcome = "emigration",
                                                 location = location,
                                                 year = "2011-2015") / 5 # because it's 5-year aggregate data 
    
    population = mean(SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                                                location = location,
                                                year = as.character(c(2011:2015)))) 
    
    emigration.rates = emigration.numbers/population
    
    c(emigration.rates)
    
}  

##--------------------##
##-- COVID MOBILITY --##
##--------------------##

get.covid.mobility.for.location = function(location){
  
  covid.mobility = get.covid.mobility.measure(COVID.MOBILITY.DATA,
                                              location = location,
                                              n.covid.months = N.COVID.MONTHS,
                                              census.manager = CENSUS.MANAGER)
  
  covid.times = 2020 + (1.5 + 1:length(covid.mobility))/12
  names(covid.times) = names(covid.mobility)
  
  create.linear.spline.functional.form(knot.times = covid.times,
                                       knot.values = as.list(covid.mobility))
}

get.covid.max.testing.effect = function(specification.metadata){
  
  create.static.functional.form(value = get.q2.full.stratified.covid.reduction.in.testing(specification.metadata),
                                link = 'log',
                                value.is.on.transformed.scale = F)
}

##------------------------##
##-- Initial Population --##
##------------------------##

get.base.initial.male.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
    if (length(specification.metadata$dim.names$location)>1)
        stop("We need to specify what to do with more than one location")
    
    counties = get.contained.locations(location, 'county')
    
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         sexes = 'male', aggregate.sexes = T,
                                         aggregate.counties = T,
                                         year=years, aggregate.years = T) / length(years)
    
    CENSUS.BHO.RACE.MAPPING$apply(county.populations)
}

get.base.initial.female.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
    if (length(specification.metadata$dim.names$location)>1)
        stop("We need to specify what to do with more than one location")
    
    counties = get.contained.locations(location, 'county')
    
    county.populations = get.census.data(ALL.DATA.MANAGERS$census.collapsed, fips=counties,
                                         sexes = 'female', aggregate.sexes = T,
                                         aggregate.counties = T,
                                         year=years, aggregate.years = T) / length(years)
    
    CENSUS.BHO.RACE.MAPPING$apply(county.populations)
}

get.location.ever.idu.prevalence <- function(location, specification.metadata)
{
    counties = get.contained.locations(location, 'county')
    
    idu.ever.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, 
                                             ALL.DATA.MANAGERS$census.full.msm, 
                                             age.cutoffs = specification.metadata$age.endpoints,
                                             use.ever=T, 
                                             counties=counties, 
                                             aggregate.counties = T)
    
    idu.ever.prevalence
}

get.location.active.idu.prevalence <- function(location, specification.metadata)
{
    counties = get.contained.locations(location, 'county')
    
    idu.30d.prevalence = get.idu.prevalence(ALL.DATA.MANAGERS$idu, 
                                            ALL.DATA.MANAGERS$census.full.msm, 
                                            age.cutoffs = specification.metadata$age.endpoints,
                                            use.30d=T, 
                                            counties=counties,
                                            aggregate.counties = T)
    
    idu.30d.prevalence
}

get.seed.rate.per.stratum <- function(location, specification.metadata, population.year=DEFAULT.POPULATION.YEARS)
{
    # A hack for now to harmonize with old locations codes
    location = gsub('^c\\.','',location,ignore.case = T)
        
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

#-----------------#
#-- Aging Rates --#
#-----------------#

#assumes that the n in each age = m*a + b
# if we set a=0 for the last age in the two brackets, then
# -35*m + 5*b = n.first.5
#    and
# -10*m + 5*b = n.second.5 ( = (-4*m + b) + (-3*m + b) + (-2*m + b) + (-1*m + b) + (0*m + b) )s
get.aging.rate.last.of.10 <- function(n.first.5, n.second.5)
{
    m = (n.second.5 - n.first.5) / 25
    b = (n.second.5 + 10*m) / 5
    
    b / (n.first.5 + n.second.5)
}

#assumes that the n in each age = m*a + b
# if we set a=0 for the last of the first 10 age brackets, then
# -45*m + 10*b = n.first.10
#    and
# 55*m + 10*b = n.second.10
get.aging.rate.mid.of.20 <- function(n.first.10, n.second.10)
{
    m = (n.second.10 - n.first.10) / 100
    b = (n.first.10 + 45*m) / 10
    
    b / n.first.10
}


#--------------------#
#-- Contact Arrays --#
#--------------------#

get.female.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
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

get.msm.single.year.age.counts <- function(location, specification.metadata,
                                           population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
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

get.heterosexual.male.single.year.age.counts <- function(location, specification.metadata,
                                                         population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
    get.census.data(ALL.DATA.MANAGERS$census.full,
                    fips=counties,
                    years = population.years,
                    sexes = 'male',
                    aggregate.years = T,
                    aggregate.counties = T,
                    aggregate.ages = F,
                    aggregate.races = T,
                    aggregate.sexes = T) / length(population.years) - 
        get.msm.single.year.age.counts(location=location, specification.metadata=specification.metadata,
                                       population.years=population.years)
}


get.msm.sexual.age.contact.proportions <- function(age.mixing.sd.mult,
                                                   single.year.msm.age.counts,
                                                   single.year.age.sexual.availability,
                                                   specification.metadata)
{
    do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$msm,
                                             age.counts = single.year.msm.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.heterosexual.male.sexual.age.contact.proportions <- function(age.mixing.sd.mult, 
                                                                 single.year.heterosexual.male.age.counts,
                                                                 single.year.age.sexual.availability,
                                                                 specification.metadata)
{
    do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$heterosexual_male,
                                             age.counts = single.year.heterosexual.male.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.female.sexual.age.contact.proportions <- function(age.mixing.sd.mult, 
                                                      single.year.female.age.counts,
                                                      single.year.age.sexual.availability,
                                                      specification.metadata)
{
    do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
                                             location=location,
                                             age.mixing.sd.mult = age.mixing.sd.mult,
                                             age.model = ALL.DATA.MANAGERS$pairing$sex.age.models$female,
                                             age.counts = single.year.female.age.counts,
                                             availability = single.year.age.sexual.availability)
}

get.idu.age.contact.proportions <- function(age.mixing.sd.mult,
                                            single.year.age.counts,
                                            single.year.age.idu.availability,
                                            specification.metadata)
{
    do.get.age.contact.proportions.for.model(specification.metadata=specification.metadata,
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


do.get.age.contact.proportions.for.model <- function(specification.metadata,
                                                     location,
                                                     age.mixing.sd.mult,
                                                     age.model,
                                                     age.counts,
                                                     availability)
    
{
    #-- Call the function --#
    age.cutoffs = specification.metadata$age.endpoints
    age.cutoffs[length(age.cutoffs)] = min(age.cutoffs[length(age.cutoffs)],
                                           max(as.numeric(names(age.counts))))
    
    get.age.mixing.proportions(age.delta.intercept.mean=age.model['mean.intercept'],
                               age.delta.slope.mean=age.model['mean.slope'],
                               age.delta.intercept.sd=age.model['sd.intercept'],
                               age.delta.slope.sd=age.model['sd.slope'],
                               age.cutoffs=age.cutoffs,
                               age.labels=specification.metadata$dim.names$age,
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