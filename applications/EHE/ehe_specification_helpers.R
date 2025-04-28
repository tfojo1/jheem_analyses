
# This code depends on
# commoncode/age_smoothers.R

##--------------------##
##-- Some Constants --##
##--------------------##

DEFAULT.POPULATION.YEARS = 2007



CENSUS.AGES = as.character(sort( parse.age.strata.names(CENSUS.MANAGER$ontologies$census$age)$lower ))

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
                                    years=c('2010'=2010,'2020'=2020,'2030'=2030,'2040'=2040),
                                    active.idu.proportion.youngest.stratum.aging.up=0.25,
                                    prior.idu.proportion.youngest.stratum.aging.up=0.25)
{
    aging.rates = do.get.empiric.aging.rates(location = location,
                                             specification.metadata = specification.metadata,
                                             years = years,
                                             active.idu.proportion.youngest.stratum.aging.up = active.idu.proportion.youngest.stratum.aging.up,
                                             prior.idu.proportion.youngest.stratum.aging.up = active.idu.proportion.youngest.stratum.aging.up)
    
    create.natural.spline.functional.form(knot.times = years,
                                          knot.values = aging.rates,
                                          link = 'log',
                                          knots.are.on.transformed.scale = F)
}

do.get.empiric.aging.rates <- function(location, specification.metadata,
                                       years=c('2010'=2010,'2020'=2020,'2030'=2030,'2040'=2040),
                                       active.idu.proportion.youngest.stratum.aging.up=0.25,
                                       prior.idu.proportion.youngest.stratum.aging.up=0.25,
                                       force.match.age.brackets.to.before.smoothing = NULL)
{
    counties = locations::get.contained.locations(location, 'county')
    
    years.to.pull = min(years):max(years)
    
    pop = CENSUS.MANAGER$pull(outcome='population',
                              dimension.values = list(location = counties,
                                                      year = as.character(years.to.pull)),
                              keep.dimensions = c('year','age','race','ethnicity','sex'),
                              from.ontology.names = 'census')
    
    if (is.null(pop))
        stop("There was no population data at all for location ", location, " between ", min(years), " and ", max(years))
    
    max.year = max(as.numeric(dimnames(pop)$year))
    min.year = min(as.numeric(dimnames(pop)$year))
    
    race.mapping = get.ontology.mapping(dimnames(pop)[c('race','ethnicity')],
                                        specification.metadata$dim.names['race'])
    
    pop = apply(pop, c('year','age','race','ethnicity','sex'), sum, na.rm=T)
    pop = race.mapping$apply(pop)
    
    if (!is.null(force.match.age.brackets.to.before.smoothing))
    {
        age.mapping = get.ontology.mapping(from.ontology = dimnames(pop)['age'],
                                           to.ontology = list(age=force.match.age.brackets.to.before.smoothing))
        
        if (is.null(age.mapping))
            stop("Cannot infer empiric aging rates: don't know how to map age to the brackets given in 'force.match.age.brackets.to.before.smoothing'")
        
        pop2 = age.mapping$apply(pop)
        pop = restratify.age.counts(pop2,
                                    desired.age.brackets = dimnames(pop)$age,
                                    smooth.infinite.age.to = 101,
                                    allow.extrapolation = T)
    }
    
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
    
    names(aging.rates) = names(years)
    aging.rates
}

get.empiric.hiv.aging.rates <- function(location, 
                                        specification.metadata,
                                        years=c(pre.spike = 1980, time0=2000, time1=2010, time2=2020),
                                        year.range.span = 5,
                                        min.years.per.range = 3,
                                        weight.to.national=1)
{
    states = get.overlapping.locations(location, 'state')
    
    if (length(states)>1)
    {
        counties.in.states = get.contained.locations(states, 'county')
        counties.in.location = get.contained.locations(location, 'county')
        
        state.weights = matrix(1/length(states), nrow=length(years), ncol=length(states))
    }
    else
        state.weights = matrix(1, nrow=length(years), ncol=1)
    
    weights = cbind(weight.to.national, (1-weight.to.national)*state.weights)
    data.locations = c("US", states)
    
    data.locations = data.locations[colSums(weights)>0]
    weights = weights[colSums(weights)>0]
    
    aging.rates.by.location = lapply(data.locations, 
                                     do.get.empiric.hiv.aging.rates,
                                     specification.metadata = specification.metadata,
                                     years = years,
                                     year.range.span = year.range.span,
                                     min.years.per.range = min.years.per.range)
    
    if (length(data.locations)==1)
        merged.aging.rates.by.year = aging.rates.by.location[[1]]
    else
    {
        merged.aging.rates.by.year = lapply(1:length(aging.rates.by.location[[1]], function(y){
            rv = 0
            for (loc in 2:length(data.lotcations))
                rv = aging.rates.by.location[[loc]][[y]] * weights[y,loc] + rv
            rv
        }))
        
        names(merged.aging.rates.by.year) = names(aging.rates.by.location[[1]])
    }
    
    create.natural.spline.functional.form(knot.times = years,
                                          knot.values = merged.aging.rates.by.year,
                                          link = 'log',
                                          knots.are.on.transformed.scale = F)
  
}

do.get.empiric.hiv.aging.rates <- function(location, 
                                           specification.metadata,
                                           years,
                                           year.range.span = 5,
                                           min.years.per.range = 3,
                                           force.match.age.brackets.before.smoothing = F)
{
    if (location=='US')
        pop = SURVEILLANCE.MANAGER$pull(outcome='diagnosed.prevalence',
                                        dimension.values = list(location = location),
                                        keep.dimensions = c('year','age','race','sex','risk'),
                                        from.ontology.names = 'cdc.national')
    else
        pop = SURVEILLANCE.MANAGER$pull(outcome='diagnosed.prevalence',
                                        dimension.values = list(location = location),
                                        keep.dimensions = c('year','age','race','sex','risk'),
                                        from.ontology.names = 'cdc')
    
    if (is.null(pop))
        stop(paste0("Cannot infer empiric HIV aging rates: there was no fully stratified, HIV prevalence data at all for '", location, "'"))
    
    
    pop = rowMeans(pop, dims=length(dim(pop))-1, na.rm=T) # sum out source
    
    
    
    map.to.jheem.sex.risk = create.ontology.mapping(
      from.dimensions = c('risk','sex'),
      mappings = rbind(
        c('msm', 'male', 'never_IDU', 'msm'),
        c('msm', 'female', NA, NA),
        
        c('msm_idu', 'male', 'active_IDU', 'msm'),
        c('msm_idu', 'male', 'IDU_in_remission', 'msm'),
        c('msm_idu', 'female', NA, NA),
        
        c('idu', 'male', 'active_IDU', 'heterosexual_male'),
        c('idu', 'male', 'IDU_in_remission', 'heterosexual_male'),
        c('idu', 'female', 'active_IDU', 'female'),
        c('idu', 'female', 'IDU_in_remission', 'female'),
        
        c('heterosexual', 'male', 'never_IDU','heterosexual_male'),
        c('heterosexual', 'female', 'never_IDU','female')
        # c('other', 'male', 'never_IDU','heterosexual_male'),
        # c('other', 'female', 'never_IDU','female')
      )
      
    )
    
    aging.rates.2001 = NULL
    if (any(years<=2001))
    {
        pop.alive.2001 = SURVEILLANCE.MANAGER$pull(outcome = "aids.diagnoses.alive.by.2001",
                                                   dimension.values = list(location = location),
                                                   keep.dimensions = c('year','age','race','sex','risk'))
        
        
        if (is.null(pop.alive.2001))
        {
            if (location=='US')
                stop(paste0("Cannot infer empiric HIV aging rates: there was no fully stratified, HIV prevalence data prior to 2001 at all for '", location, "'"))
        }
        else
        {
            pop.alive.2001 = rowMeans(pop.alive.2001, dims=length(dim(pop.alive.2001))-1, na.rm=T) # sum out source
          
            race.mapping = get.ontology.mapping(from.ontology = dimnames(pop.alive.2001)['race'],
                                                to.ontology = specification.metadata$dim.names['race'])
            
            if (is.null(race.mapping))
              stop(paste0("Cannot infer empiric HIV aging rates: cannot find ontology mappings to align pre-2001 HIV data with the '",
                          specification.metadata$version, "' version's race"))
            
            pop.alive.2001 = race.mapping$apply(pop.alive.2001, na.rm=T)
            pop.alive.2001 = map.to.jheem.sex.risk$apply(pop.alive.2001, na.rm=T)
            
            pop.alive.2001[is.na(pop.alive.2001)] = 0
            
            restratified.pop.alive.2001 = restratify.age.counts(pop.alive.2001, 
                                                                desired.age.brackets = 0:101,
                                                                smooth.infinite.age.to = 101)
            
            year.diagnosed.alive.2001 = as.numeric(dimnames(restratified.pop.alive.2001)$year)
            n.non.year.non.age = prod(dim(restratified.pop.alive.2001)) / prod(dim(restratified.pop.alive.2001)[1:2])

            aging.rates.2001 = t(sapply(1:(specification.metadata$n.ages-1), function(age.index){
                ages = specification.metadata$age.lower.bounds[age.index]:(specification.metadata$age.upper.bounds[age.index]-1)
                
                age.counts = sapply(ages, function(age.in.2001){
                    rowSums(sapply(year.diagnosed.alive.2001, function(year){
                        age.diagnosed = age.in.2001 + (2001 - year)
                        
                        if (age.diagnosed<0)
                            rep(0, n.non.year.non.age)
                        else
                            restratified.pop.alive.2001[as.character(year), age.diagnosed+1,,,]
                    }))
                })
                
                age.counts[,length(ages)] / rowSums(age.counts)
            }))
            
            dim.names = c(list(age=specification.metadata$dim.names$age[-specification.metadata$n.ages]),
                             dimnames(restratified.pop.alive.2001)[-(1:2)])
            dim(aging.rates.2001) = sapply(dim.names, length)
            dimnames(aging.rates.2001) = dim.names
        }
    }
  

    pop.years = as.numeric(dimnames(pop)$year)
    parsed.ages = parse.age.strata.names(dimnames(pop)$age)
    min.age = min(parsed.ages$lower)
    max.age = min(101, max(parsed.ages$upper))

    
    pop2 = map.to.jheem.sex.risk$apply(pop)
    
    race.mapping = get.ontology.mapping(from.ontology = dimnames(pop)['race'],
                                        to.ontology = specification.metadata$dim.names['race'])
    if (is.null(race.mapping))
        stop("Cannot infer empiric HIV aging rates: don't know how to map race to the desired ontology for the specification")
    
    pop2 = race.mapping$apply(pop2)
    
    if (force.match.age.brackets.before.smoothing)
    {
        age.mapping = get.ontology.mapping(from.ontology = dimnames(pop)['age'],
                                           to.ontology = specification.metadata$dim.names['age'])
        
        if (is.null(age.mapping))
          stop("Cannot infer empiric HIV aging rates: don't know how to map age to the desired ontology for the specification (and we have set force.match.age.brackets.before.smoothing==T)")
        
        pop2 = age.mapping$apply(pop2)
    }
    
    aging.rates.by.year = lapply(years, function(year){
      
      if (year <= 2001 && !is.null(aging.rates.2001))
          return (aging.rates.2001)
      
      delta = ceiling((year.range.span-1)/2)
      desired.years.to.pull = (year-delta):(year+delta)
      years.to.pull = intersect(desired.years.to.pull, pop.years)  
      
      if (length(years.to.pull)==0)
        return(NULL)
      
      if (length(years.to.pull) < min.years.per.range)
        stop(paste0("Cannot infer empiric HIV aging rates: there ",
                    ifelse(length(years.to.pull)==0, "are no years",
                           ifelse(length(years.to.pull)==1, "is only one year",
                                  paste0("are only ", length(years.to.pull), " years"))),
                    " with fully stratified, HIV prevalence data for '", location, "' between ",
                    min(desired.years.to.pull), " and ", max(desired.years.to.pull),
                    ". We require a minimum of ", min.years.per.range, " years with data"))
      
      pop.for.year = colSums(pop2[as.character(years.to.pull),,,,,drop=F])
      single.year.ages.pop = restratify.age.counts(pop.for.year,
                                                   desired.age.brackets = min.age:max.age,
                                                   smooth.infinite.age.to = max.age)
      
      aging.rates = t(sapply(1:(specification.metadata$n.ages-1), function(age.index){
        ages = specification.metadata$age.lower.bounds[age.index]:(specification.metadata$age.upper.bounds[age.index]-1)
        
        last.in.bracket = single.year.ages.pop[ages[length(ages)]-min.age+1,,,]
        all.in.bracket = colSums(single.year.ages.pop[ages-min.age+1,,,,drop=F])
        rates = last.in.bracket / all.in.bracket
        
        rates[is.na(rates)] = mean(rates, na.rm=T)
        
        rates
      }))
      
      dim.names = c(list(age=specification.metadata$dim.names$age[-specification.metadata$n.ages]),
                    dimnames(single.year.ages.pop)[-1])
      dim(aging.rates) = sapply(dim.names, length)
      dimnames(aging.rates) = dim.names
      
      
      aging.rates
    })
    
    null.data.for.year.mask = sapply(aging.rates.by.year, is.null)
    if (all(null.data.for.year.mask))
      stop(paste0("Cannot infer empiric HIV aging rates: none of the requested years (",
                  collapse.with.and(years), 
                  ") had fully stratified, HIV prevalence data for '", location, "'"))
    
    interpolated.aging.rates.by.year = interpolate(values = aging.rates.by.year[!null.data.for.year.mask],
                                                   value.times = years[!null.data.for.year.mask],
                                                   desired.times = years)
    names(interpolated.aging.rates.by.year) = names(years)
    
    for (i in (1:length(interpolated.aging.rates.by.year))[-1])
    {
        interpolated.aging.rates.by.year[[i]] = array.access(interpolated.aging.rates.by.year[[i]],
                                                             dimnames(interpolated.aging.rates.by.year[[1]]))
    }
    
    interpolated.aging.rates.by.year
}

get.fraction.over.age <- function(location,
                                 specification.metadata,
                                 age,
                                 years=DEFAULT.POPULATION.YEARS)
{
    counties = locations::get.contained.locations(location, 'COUNTY')
    n.per = prod(sapply(specification.metadata$dim.names[c('race','sex')], length))
    
    rv = t(sapply(1:specification.metadata$n.ages, function(i){
      
        if (specification.metadata$age.upper.bounds[i]<=18)
            rep(0, n.per)
        else if (specification.metadata$age.lower.bounds[i]>=18)
            rep(1,n.per)
        else
        {
            ages = specification.metadata$age.lower.bounds[i]:(specification.metadata$age.upper.bounds[i]-1)
            
            pop = CENSUS.MANAGER$pull(outcome='population',
                                      dimension.values = list(location=counties,
                                                              year=years,
                                                              age=paste0(ages, ' years')),
                                      keep.dimensions = c('age','sex','race','ethnicity'),
                                      from.ontology.names = 'census')
            
            age.brackets = make.age.strata.names(endpoints = c(ages[1], 18, ages[length(ages)]))
            
            pop2 = map.value.ontology(pop, 
                                      target.dim.names = c(list(age=age.brackets),
                                                           specification.metadata$dim.names[c('race','sex')]))
            
            pop2[2,,] / colSums(pop2)
        }
    }))

    dim.names = specification.metadata$dim.names[c('age','race','sex')]
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    rv
}

##--------------------##
##-- Proportion MSM --##
##--------------------##

get.proportion.msm.of.male.by.race.functional.form <- function(location, specification.metadata)
{
    best.guess.proportions = get.best.guess.msm.proportions(location = location,
                                                            specification.metadata = specification.metadata,
                                                            years = DEFAULT.POPULATION.YEARS,
                                                            keep.race = T,
                                                            keep.age = F)
  
    # best.guess.proportions = get.best.guess.msm.proportions.by.race(location,
    #                                                                 specification.metadata = specification.metadata,
    #                                                                 years = DEFAULT.POPULATION.YEARS,
    #                                                                 min.age = specification.metadata$age.lower.bounds[1],
    #                                                                 return.proportions = T)
    
    #best.guess.proportions = array(best.guess.proportions, 
    #                               dim=c(race=length(best.guess.proportions)),
    #                               dimnames=list(race=names(best.guess.proportions)))
    
    create.static.functional.form(best.guess.proportions,
                                  link = 'log',
                                  value.is.on.transformed.scale = F)
}


get.best.guess.msm.proportions <- function(location,
                                           specification.metadata,
                                           years = 2013,
                                           ages = specification.metadata$dim.names$age,
                                           keep.age = T,
                                           keep.race = T,
                                           return.proportions = T)
{
    counties = locations::get.contained.locations(location, 'county')
    states = locations::get.overlapping.locations(location, 'state')
   
    # Get county-level proportions
    proportion.msm.by.county = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                                         dimension.values = list(location=counties,
                                                                                 sex='male'))
    if (is.null(proportion.msm.by.county) || !setequal(counties, dimnames(proportion.msm.by.county)$location))
        stop(paste0("Cannot get best-guess msm proportions: we don't have data on proportion msm for all counties in location '", location, "'"))
    
    proportion.msm.by.county = apply(proportion.msm.by.county, 'location', mean, na.rm=T)
    if (any(is.na(proportion.msm.by.county)))
        stop(paste0("Cannot get best-guess msm proportions: we don't have data on proportion msm for all counties in location '", location, "' (we get some NAs)"))
    
    # Get number male and flatten race/ethnicity
    males = CENSUS.MANAGER$pull(outcome = 'population',
                                keep.dimensions = c('location', 'age','race','ethnicity'),
                                dimension.values = list(location = counties,
                                                        year = years,
                                                        sex = 'male'),
                                from.ontology.names = 'census')[,,,,1,drop=F]
    
    dim.names = dimnames(males)[-length(dim(males))]
    dim(males) = sapply(dim.names, length)
    dimnames(males) = dim.names
    
    if (is.null(males))
        stop("Cannot get best-guess msm proportions: we are unable to pull any census data on the number of males")
    
    if (is.null(ages))
        ages = dimnames(males)$age
        
    flat.census.reth = c(dimnames(males)$race, 'hispanic')
    flat.census.reth.mapping = create.ontology.mapping(
        from.dimensions = c('race','ethnicity'),
        to.dimensions = 'race',
        mappings = rbind(
            cbind(dimnames(males)$race,
                  'not hispanic',
                  dimnames(males)$race),
            cbind(dimnames(males)$race,
                  'hispanic',
                  'hispanic')
        )
    )
    
    
    males = flat.census.reth.mapping$apply(males)
    males[males==0] = 1
    
    # Estimate a proportion msm for each race
    if (length(states)==1 && states=='AL')
        states.for.p.msm = 'MS'
    else
        states.for.p.msm = states
    raw.proportion.msm.by.race = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                                           keep.dimensions = c('year','location','race'),
                                                           dimension.values = list(location = states.for.p.msm,
                                                                                   sex = 'male'))
    
    if (is.null(raw.proportion.msm.by.race))
        stop(paste0("There were no data on proportion.msm for state(s) ",
                    collapse.with.and("'", states.for.p.msm, "'")))
    
    raw.proportion.msm.by.race = apply(raw.proportion.msm.by.race, 'race', mean, na.rm=T)
    
    proportions.msm.by.race = c(
        white = as.numeric(raw.proportion.msm.by.race['white']),
        black = as.numeric(raw.proportion.msm.by.race['black']),
        'american indian or alaska native' = as.numeric(raw.proportion.msm.by.race['american indian/alaska native']),
        'asian or pacific islander' = sum(.9*raw.proportion.msm.by.race['asian'] + .1*raw.proportion.msm.by.race['native hawaiian/other pacific islander']),
        hispanic = as.numeric(raw.proportion.msm.by.race['hispanic'])
    )
    
    if (all(is.na(proportions.msm.by.race)))
        stop("Cannot get best-guess msm proportions: we are getting NA proportions MSM by race at the state level (from BRFSS)")
    
    proportions.msm.by.race[is.na(proportions.msm.by.race)] = mean(proportions.msm.by.race[setdiff(names(proportions.msm.by.race), 'american indian or alaska native')], na.rm=T)

    # Make a guess as to the n msm
    first.guess.n.msm = sapply(dimnames(males)$race, function(r){
        males[,,r] * proportions.msm.by.race[r]
    })
    dim.names = dimnames(males)
    dim(first.guess.n.msm) = sapply(dim.names, length)
    dimnames(first.guess.n.msm) = dim.names
    
    # Scale it to hit the overall proportions in each msm
    parsed.census.ages = parse.age.strata.names(dimnames(first.guess.n.msm)$age)
    adult.mask = parsed.census.ages$lower >=13
    
    first.guess.p.by.county = rowSums(first.guess.n.msm[,adult.mask,,drop=F]) / rowSums(males[,adult.mask,,drop=F])
    scale.factor.by.county = proportion.msm.by.county / first.guess.p.by.county
    
    fitted.n.msm = first.guess.n.msm * scale.factor.by.county
    
    # Map races
    if (keep.race)
    {
        race.mapping = get.ontology.mapping(from.ontology = dimnames(males)['race'],
                                            to.ontology = specification.metadata$dim.names['race'])
        if (is.null(race.mapping))
            stop("Cannot get best-guess msm proportions: we don't have a mapping from census races to the specification's races")
        
        fitted.n.msm = race.mapping$apply(fitted.n.msm)
        if (return.proportions)
            males = race.mapping$apply(males)
    }
    
    # Map ages
    age.mapping = get.ontology.mapping(from.ontology = dimnames(males)['age'],
                                       to.ontology = list(age=ages))
    if (is.null(age.mapping))
        stop("Cannot get best-guess msm proportions: we don't have a mapping from census ages to the specification's age brackets")
    
    fitted.n.msm = age.mapping$apply(fitted.n.msm)
    if (return.proportions)
        males = age.mapping$apply(males)

    # Marginalize to get the probabilities
    keep.dimensions = c('age','race')[c(keep.age, keep.race)]
    
    if (length(keep.dimensions)==0)
    {
        if (return.proportions)
            sum(fitted.n.msm) / sum(males)
        else
            sum(fitted.n.msm)
    }
    else
    {
        if (return.proportions)
            rv = apply(fitted.n.msm, keep.dimensions, sum) / apply(males, keep.dimensions, sum)
        else
            rv = apply(fitted.n.msm, keep.dimensions, sum)
        
        dim.names = dimnames(fitted.n.msm)[keep.dimensions]
        
        dim(rv) = sapply(dim.names, length)
        dimnames(rv) = dim.names
        rv
    }
}


# Assumes that within each county, relative risks of being MSM are as in MSM.PROPORTIONS
# and total risk of being MSM is as per read.msm.proportions
get.best.guess.msm.proportions.by.race <- function(location,
                                                   specification.metadata,
                                                   min.age=0,
                                                   years=DEFAULT.POPULATION.YEARS,
                                                   msm.proportions.by.race = c(black=1-.806, hispanic=1-.854, white=1-.848, other=1-.802),
                                                   # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4516312/
                                                   return.proportions=T,
                                                   keep.ages = F)
{
  stop("This function is deprecated - use get.best.guess.msm.proportions() instead")
    fips = get.contained.locations(location, 'county')
    
    proportion.msm.by.location = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                      dimension.values = list(location=fips,
                                                              sex='male'))
    if (is.null(proportion.msm.by.location) || !setequal(fips, dimnames(proportion.msm.by.location)$location) || any(is.na(proportion.msm.by.location)))
        stop(paste0("Cannot get best-guess msm proportions: we don't have data on proportion msm for all counties in location '", location, "'"))
      
    
    proportion.msm.by.location = apply(proportion.msm.by.location, 'location', mean, na.rm=T)
    
    keep.dimensions = c('location', 'age','race','ethnicity')
    males = CENSUS.MANAGER$pull(outcome = 'population',
                                      keep.dimensions = keep.dimensions,
                                      dimension.values = list(location = fips,
                                                              year = years,
                                                              sex = 'male'),
                                from.ontology.names = 'census')
    if (is.null(males))
        stop(paste0("Cannot get best-guess msm proportions: no population data for location '", location, "' are available"))
    
    males = apply(males, keep.dimensions, mean, na.rm=T)
    
    # A hack for now while we wait for the real function
    states = get.overlapping.locations(location, 'state')
    raw.proportion.msm.by.race = SURVEILLANCE.MANAGER$pull(outcome = 'proportion.msm',
                                                       keep.dimensions = c('year','location','race'),
                                                       dimension.values = list(location = states,
                                                                               sex = 'male'))
    raw.proportion.msm.by.race = apply(raw.proportion.msm.by.race, 'race', mean, na.rm=T)
    proportion.msm.by.race = c(black = as.numeric(raw.proportion.msm.by.race['Black']),
                               hispanic = as.numeric(raw.proportion.msm.by.race['Hispanic']),
                               white = as.numeric(raw.proportion.msm.by.race['White']),
                               other = mean(raw.proportion.msm.by.race[setdiff(names(raw.proportion.msm.by.race),
                                                                               c("Black",'Hispanic','White','Native Hawaiian/Other Pacific Islander','American Indian/Alaska Native'))]))
    
    if (min.age > 0)
    {
        parsed.ages = parse.age.strata.names(dimnames(males)$age)
        age.mask = parsed.ages$lower >= min.age
        
        if (names(dim(males))[2]!='age')
            stop(paste0("Cannot get best-guess msm proportions: we assume that 'location' is the first dimension in the census data"))
        
        males = males[,age.mask,,]
    }
    
    if (!keep.ages)
        males = apply(males, setdiff(keep.dimensions, 'age'), sum, na.rm=T)
    if (!setequal(fips, dimnames(males)$location) || any(is.na(males)))
        stop(paste0("Cannot get best-guess msm proportions: we don't have census data for all counties in location '", location, "'"))
    
    if (names(dim(males))[1]!='location')
        stop(paste0("Cannot get best-guess msm proportions: we assume that 'location' is the first dimension in the census data"))
    if (all(dimnames(males)$race != 'white') || all(dimnames(males)$race != 'black'))
        stop("Cannot get best-guess msm proportions: we assume that population data include 'black' and 'white' race categories")
    if (all(dimnames(males)$ethnicity != 'hispanic') || all(dimnames(males)$race != 'black'))
        stop("Cannot get best-guess msm proportions: we assume that population data include 'black' and 'white' race categories")
    
    msm.proportions.aligned.to.males = sapply(dimnames(males)$ethnicity, function(e){
        if (e=='hispanic')
            rep(as.numeric(proportion.msm.by.race['hispanic']), dim(males)['race'])
        else
          sapply(dimnames(males)$race, function(r){
              rv = as.numeric(proportion.msm.by.race[r])
              if (is.na(rv))
                  as.numeric(proportion.msm.by.race['other'])
              else
                  rv
          })
    })
    dimnames(msm.proportions.aligned.to.males) = dimnames(males)[c('race','ethnicity')]
    msm.proportions.aligned.to.males = expand.array(msm.proportions.aligned.to.males, target.dim.names = dimnames(males))  
    
    numerators = proportion.msm.by.location * msm.proportions.aligned.to.males * males *
        rowSums(males) / rowSums(males * msm.proportions.aligned.to.males) 
    
    race.mapping = get.ontology.mapping(from.ontology = dimnames(males)[c('race','ethnicity')],
                                        to.ontology = specification.metadata$dim.names['race'])
    if (is.null(race.mapping))
        stop("Cannot get best-guess msm proportions: we cannot map from the census data's race/ethnicity to the requested races")
    
    target.dim.names = specification.metadata$dim.names['race']
    if (keep.ages)
        target.dim.names = c(dimnames(males)['age'], target.dim.names)
    
    numerators2 = race.mapping$apply(numerators, to.dim.names = target.dim.names)
    
    if (return.proportions)
        numerators2 / race.mapping$apply(males, to.dim.names = target.dim.names)
    else
        numerators2 / length(years)
}

##---------------##
##-- Fertility --##
##---------------##

# for this to be different, will also need to add alphas into ehe_parameters and map them in ehe_parameter_mapping
get.location.birth.rates.functional.form = function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.location.birth.rates(location=location,
                                    specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = rates,
                                link = "log",
                                value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
  # create.natural.spline.functional.form(
  #     knot.times = c(time1 = 2010, time2 = 2020),
  #     knot.values = list(time1 = rates, time2 = rates),
  #     link = 'identity',
  #     knot.link = 'log',
  #     min = 0,
  #     knots.are.on.transformed.scale = F
  # )
}

get.location.birth.rates <- function(location,
                                     specification.metadata,
                                     years = DEFAULT.POPULATION.YEARS)
{
    counties = locations::get.contained.locations(location, 'county')
    
    # Pull the births into an array
    # I imagine this should be indexed [year, county, race, ethnicity] - not necessarily in that order
    births = CENSUS.MANAGER$pull(outcome = 'births', location = counties, year= years, keep.dimensions = c('race', 'ethnicity', 'location'), na.rm=TRUE)[,,,1] #keep all race, all ethnicities, first source#
    if (is.null(births))
      stop(paste0("Cannot get.location.birth.rates() - no 'births' data are available in the CENSUS.MANAGER for the counties in location '", location, "' (",
                  locations::get.location.name(location), ")"))
    births = apply(births, c('race','ethnicity'), sum, na.rm=T)
    
    # Pull population into an array
    population = CENSUS.MANAGER$pull(outcome = 'population', 
                                         location = counties, 
                                         year = years, 
                                         keep.dimensions = c('age', 'race', 'ethnicity', 'sex'), 
                                         from.ontology.names = 'census',
                                         na.rm=TRUE)
    if (is.null(population))
        stop(paste0("Cannot get.location.birth.rates() - no 'population' data are available in the SURVEILLANCE.MANAGER for location '", location, "' (",
                    locations::get.location.name(location), ")"))
    
    
    parsed.pop.ages = parse.age.strata.names(dimnames(population)$age)
    pop.age.mask = parsed.pop.ages$lower >= specification.metadata$age.lower.bounds[1] &
      parsed.pop.ages$upper <= specification.metadata$age.upper.bounds[specification.metadata$n.ages]
    population = apply(population[pop.age.mask,,,,], c('race','ethnicity'), sum)
    
    
    # Map the ontologies
    target.dim.names = specification.metadata$dim.names[c('race')]

    rv = map.value.ontology(births, target.dim.names=target.dim.names) / 
        map.value.ontology(population, target.dim.names=target.dim.names)
    
    dim(rv) = sapply(specification.metadata$dim.names['race'], length)
    dimnames(rv) = specification.metadata$dim.names['race']
    rv
}

get.location.mortality.rates.functional.form = function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS)
{
    rates = get.location.mortality.rates(location=location,
                                     specification.metadata = specification.metadata) 
    
   create.static.functional.form(value = rates,
                                 link = "log",
                                 value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
    
      # create.natural.spline.functional.form(
      #     knot.times = c(time1 = 2010, time2 = 2020),
      #     knot.values = list(time1 = rates, time2 = rates),
      #     link = 'identity',
      #     knot.link = 'log',
      #     min = 0,
      #     knots.are.on.transformed.scale = F
      # )
}

get.location.mortality.rates <- function(location,
                                         specification.metadata,
                                         year.ranges = c('2001-2010','2011-2020'),
                                         backup.locations = c(AL='TN', MS='TN'))
  
{
    if (get.location.type(location)=='state')
    {
        counties = locations::get.contained.locations(location, 'county')
        deaths = CENSUS.MANAGER$pull(outcome = 'state.deaths', location = location, year= year.ranges, keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
        
        if (is.null(deaths))
            stop("Error in get.location.mortality.rates() - unable to pull any state.deaths data for the requested years")
        
        # Pull the population - I expect this will be similarly index by year, county, race, ethnicity, and sex
        population = CENSUS.MANAGER$pull(outcome = 'state.deaths.denominator', location = location, year= year.ranges, keep.dimensions = c('year','age','race', 'ethnicity', 'sex'))
        # 
        # parsed.year.ranges = parse.year.ranges(year.ranges)
        # years.for.year.ranges = unlist(lapply(1:length(year.ranges), function(i){as.numeric(parsed.year.ranges$start[i]):as.numeric(parsed.year.ranges$end[i])}))
        # population = CENSUS.MANAGER$pull('population', location = counties, year= years.for.year.ranges, keep.dimensions = c('year','age','race', 'ethnicity', 'sex'))
        # 
        if (is.null(population))
            stop("Error in get.location.mortality.rates() - unable to pull any state.deaths.denominator data for the requested years")
        
        population[is.na(deaths)] = NA
        deaths[is.na(population)] = NA
        
        # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
        # then divide the two
        target.dim.names = specification.metadata$dim.names[c('age','race','sex')]
        mapped.deaths = map.value.ontology(value = deaths, target.dim.names = target.dim.names)
        mapped.population = map.value.ontology(value = population, target.dim.names = target.dim.names)
        
        rv = mapped.deaths / mapped.population
        if (any(rv==0))
            stop("getting zero's in the calculated mortality rates from get.location.mortality.rates() for state")
        
        rv
    }
    else
    {
        counties = locations::get.contained.locations(location, 'county')
        states = unique(locations::get.containing.locations(counties, 'state'))
        
        # Pull the deaths - I expect this will be indexed by year, county, race, ethnicity, and sex (not necessarily in that order)
        deaths = CENSUS.MANAGER$pull(outcome = 'metro.deaths', location = states, year= year.ranges, keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
        
        if (is.null(deaths))
          stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths data for the requested years")
        
        # Pull the population - I expect this will be similarly index by year, county, race, ethnicity, and sex
        population = CENSUS.MANAGER$pull(outcome = 'metro.deaths.denominator', location = states, year= year.ranges, keep.dimensions = c('year','age','race', 'ethnicity', 'sex', 'location'))
        
        if (is.null(population))
          stop("Error in get.location.mortality.rates() - unable to pull any metro.deaths.denominator data for the requested years")
        
        population[is.na(deaths)] = NA
        
        # Map numerator (deaths) and denominator (population) to the age, race, and sex of the model specification
        # then divide the two
        target.dim.names = c(list(location=states), specification.metadata$dim.names[c('age','race','sex')])
    
        rates.by.state.numerator = map.value.ontology(deaths, target.dim.names=target.dim.names, na.rm = T)
        rates.by.state.denominator = map.value.ontology(population, target.dim.names=target.dim.names, na.rm = T)
        
        rates.by.state = rates.by.state.numerator / rates.by.state.denominator
        rates.by.state[rates.by.state.numerator==0 & rates.by.state.denominator==0] = 0
    
        if (any(is.na(rates.by.state)))
          stop("getting NA values in rates.by.state in get.location.mortality.rates()")
        
        if (length(states)==1)
            rv = rates.by.state[1,,,]
        else
        {
            county.populations = CENSUS.MANAGER$pull(outcome = 'population',
                                                     dimension.values = list(location=counties,
                                                                             year=year.ranges),
                                                     from.ontology.names = 'census')
            
            if (is.null(county.populations))
                stop("Error in get.location.mortality.rates(): cannot get populations for the component counties")
            county.populations = apply(county.populations,
                                       'location', mean, na.rm=T)
            total.population = sum(county.populations, na.rm=T)
            
            state.weights = sapply(states, function(st){
                counties.in.state.and.loc = intersect(counties,
                                                      locations::get.contained.locations(st, 'county'))
                
                sum(county.populations[counties.in.state.and.loc], na.rm=T)/total.population
            })
            
            rv = apply(state.weights * rates.by.state, c('age','race','sex'), sum)
        }
        
        if (any(rv==0))
        {
            backup.loc = backup.locations[location]
            if (is.na(backup.loc))
            {
                stop("getting zero's in the calculated mortality rates from get.location.mortality.rates()")
            }
            else
            {
                backup.rates = get.location.mortality.rates(location = backup.loc,
                                                            specification.metadata = specification.metadata,
                                                            year.ranges = year.ranges)
                
                for (sex in dimnames(rv)$sex)
                {
                    for (race in dimnames(rv)$race)
                    {
                        if (any(rv[,race,sex]==0))
                            rv[,race,sex] = backup.rates[,race,sex]
                    }
                }
                
                if (any(rv==0))
                    stop("getting zero's in the calculated mortality rates from get.location.mortality.rates(), even AFTER applying a backup location")
            }
        }
    
        
        rv
    }
}


##-------------##
##-- Pairing --##
##-------------##

# within.county.race.oes[to,from] is how much more likely someone it is for a person of race to to have a partner of race from, relative to race from's population prevalence
get.geographically.aggregated.race.oes <- function(location,
                                                   specification.metadata,
                                                   within.county.race.oes,
                                                   years = DEFAULT.POPULATION.YEARS,
                                                   as.functional.form=T)
{
    if (!is.numeric(within.county.race.oes) || is.null(dim(within.county.race.oes)) || length(dim(within.county.race.oes))!=2)
        stop("Cannot get.geographically.aggregated.race.oes() - within.county.race.oes must be a 2-dimensional matrix")
    
    if (is.null(dimnames(within.county.race.oes)) || 
        !setequal(dimnames(within.county.race.oes)[[1]], specification.metadata$dim.names$race) ||
        !setequal(dimnames(within.county.race.oes)[[2]], specification.metadata$dim.names$race))
        stop("Cannot get.geographically.aggregated.race.oes() - within.county.race.oes must have dimnames set, and the names of both dimensions must match the specification's race categories")
    
    counties = locations::get.contained.locations(location, 'county')
    population = CENSUS.MANAGER$pull(outcome='population', 
                                     dimension.values = list(location=counties, year=years),
                                     keep.dimensions = c('location','age','race','ethnicity', 'sex'),
                                     from.ontology.names = 'census')
    
    if (is.null(population))
        stop(paste0("Cannot get.geographically.aggregated.race.oes() - no census data were available for the counties of '", location, "'"))
    
    parsed.ages = parse.age.strata.names(dimnames(population)$age)
    age.mask = parsed.ages$lower >= specification.metadata$age.lower.bounds[1] &
        parsed.ages$upper <= specification.metadata$age.upper.bounds[specification.metadata$n.ages]
    
    race.mapping = get.ontology.mapping(from.ontology = dimnames(population)[c('race','ethnicity')],
                                        to.ontology = specification.metadata$dim.names['race'])
    
    if (is.null(race.mapping))
        stop("Cannot get.geographically.aggregated.race.oes() - don't know how to map race to the desired ontology for the specification")
    
    mapped.dim.names = c(list(location=counties),
                         specification.metadata$dim.names['race'])
    
    mapped.population = race.mapping$apply(population[,age.mask,,,,,drop=F], to.dim.names = mapped.dim.names)
    
    #population.race.fractions = mapped.population / rowSums(mapped.population)
    
    races = specification.metadata$dim.names$race
    projected.n.partners = sapply(races, function(race.from){
        sapply(races, function(race.to){
            sum(within.county.race.oes[race.to, race.from] * mapped.population[,race.from] * mapped.population[,race.to] / rowSums(mapped.population))
        })
    })
    
    race.counts = colSums(mapped.population)
    expected.race.proportions = race.counts / sum(race.counts)
    
    projected.race.proportions = projected.n.partners / rowSums(projected.n.partners)
    
    oes = projected.race.proportions / rep(expected.race.proportions, each=length(races))
    
    dim.names = list(race.to = races,
                     race.from = races)
    dim(oes) = sapply(dim.names, length)
    dimnames(oes) = dim.names
    
    if (as.functional.form)
        create.static.functional.form(value = oes, link='log', value.is.on.transformed.scale = F)
    else
        oes
}

oes.to.proportions <- function(oes, population)
{
    raw = oes * rep(population[ dimnames(oes)[[1]] ], each=length(population))
    raw / rowSums(raw)
}

sexual.oes.to.contact.proportions <- function(race.sexual.oes,
                                              race.population.counts)
{
    oes.to.proportions(oes = race.sexual.oes, population = race.population.counts)
}

idu.oes.to.contact.proportions <- function(race.idu.oes,
                                           race.population.counts)
{
    oes.to.proportions(oes = race.idu.oes, population = race.population.counts)
}


get.idu.sexual.oe.functional.form = function(specification.metadata)
{
  create.static.functional.form(value = get.idu.sexual.oes(specification.metadata), # see idu_sexual_oes.R
                                link = "log", 
                                value.is.on.transformed.scale = F) 
  
}

##---------------##
##-- Migration --##
##---------------##


get.immigration.rates.functional.form <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.immigration.rates(location=location,
                                specification.metadata = specification.metadata) 
  
  create.natural.spline.functional.form(knot.times = c(time.1 = 2010,time.2 = 2020),
                                        knot.values = list(time.1 = rates, time.2 = rates),
                                        link = "identity",
                                        knot.link = 'log',
                                        min = 0,
                                        after.time = 2030,
                                        after.modifier = 0.1,
                                        knots.are.on.transformed.scale = F)
  
  # create.static.functional.form(value = rates,
  #                               link = "log",
  #                               value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
}

get.immigration.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  # this will be one top-level beta for the MSA, then we'll include alphas by strata (race and age only, not sex)? 

  if (get.location.type(location)=='CBSA')
  {
      immigration.numbers = SURVEILLANCE.MANAGER$pull(outcome = "immigration",
                                                    location = location,
                                                    year = "2011-2015",
                                                    from.ontology.names = 'census.immigration'
                                                    ) / 5 # because it's 5-year aggregate data     
  }
  else
  {
      immigration.numbers = sum(SURVEILLANCE.MANAGER$pull(outcome = "immigration",
                                                      location = location,
                                                      year = as.character(2011:2015),
                                                      from.ontology.names = 'census'
      )) / 5 # because it's 5-year aggregate data    
  }
  
  population = mean(SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                                           location = location,
                                           year = as.character(c(2011:2015)))) 

  immigration.rates = immigration.numbers/population
  
  c(immigration.rates)
}

get.emigration.rates.functional.form <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  rates = get.emigration.rates(location=location,
                               specification.metadata = specification.metadata) 
  
  create.natural.spline.functional.form(knot.times = c(time.1 = 2010,time.2 = 2020),
                                        knot.values = list(time.1 = rates, time.2 = rates),
                                        link = "identity",
                                        knot.link = 'log',
                                        min = 0,
                                        after.time = 2030,
                                        after.modifier = 0.1,
                                        knots.are.on.transformed.scale = F)
  
  # create.static.functional.form(value = rates,
  #                               link = "log",
  #                               value.is.on.transformed.scale = F) # not giving the log rates; don't need to transform this value
  
}

get.emigration.rates <- function(location, specification.metadata, population.years=DEFAULT.POPULATION.YEARS){
  
  # this will be one top-level beta for the MSA, then we'll include alphas by strata (race and age only, not sex)? 
  if (get.location.type(location)=='CBSA')
  {
      emigration.numbers = SURVEILLANCE.MANAGER$pull(outcome = "emigration",
                                                     location = location,
                                                     year = "2011-2015",
                                                     from.ontology.names = 'census.immigration') / 5 # because it's 5-year aggregate data 
  }
  else
  {
    emigration.numbers = sum(SURVEILLANCE.MANAGER$pull(outcome = "emigration",
                                                   location = location,
                                                        year = as.character(2011:2015),
                                                        from.ontology.names = 'census'
    )) / 5 # because it's 5-year aggregate data    
  }
    
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
  
  knot.values = as.list(pmax(0, covid.mobility))
  names(knot.values) = names(covid.mobility)
  
  create.linear.spline.functional.form(knot.times = covid.times,
                                       knot.values = knot.values, 
                                       min = 0)
}

get.covid.max.testing.effect = function(specification.metadata){
  
  create.static.functional.form(value = get.q2.full.stratified.covid.reduction.in.testing(specification.metadata),
                                link = 'logit',
                                value.is.on.transformed.scale = F)
}

get.covid.max.sexual.transmission.effect = function(specification.metadata){
  
  create.static.functional.form(value = get.covid.reduction.in.sexual.transmission(specification.metadata),
                                link = 'logit',
                                value.is.on.transformed.scale = F)
}

##------------------------##
##-- Initial Population --##
##------------------------##

get.base.initial.male.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
    get.base.initial.population.for.sex(location,
                                             specification.metadata = specification.metadata,
                                             sex = 'male',
                                             years = years)
}

get.base.initial.female.population <- function(location, specification.metadata, years=DEFAULT.POPULATION.YEARS)
{
    get.base.initial.population.for.sex(location,
                                             specification.metadata = specification.metadata,
                                             sex = 'female',
                                             years = years)
}

get.base.initial.population.for.sex <- function(location, specification.metadata, sex, years=DEFAULT.POPULATION.YEARS)
{
    if (length(specification.metadata$dim.names$location)>1)
        stop("We need to specify what to do with more than one location")
    
    counties = locations::get.contained.locations(location, 'county')
    pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=years, location=counties, sex=sex),
                              keep.dimensions = c('age','race','ethnicity','sex'),
                              from.ontology.names = 'census') / length(years)
    
    mapping = get.ontology.mapping(from.ontology = dimnames(pop),
                                   to.ontology = specification.metadata$dim.names[c('age','race')])
    
    if (is.null(mapping))
        stop(paste0("Cannot get.base.initial.population.for.sex('",
                    sex,
                    "') - unable to find a mapping from the census to the specification's age and race categorizations"))
    
    mapping$apply(pop, to.dim.names = specification.metadata$dim.names[c('age','race')])
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

X.get.location.active.idu.prevalence <- function(location,
                                               specification.metadata,
                                               idu.incidence,
                                               idu.relapse,
                                               idu.remission,
                                               uninfected.aging)
{
  
}

get.prior.to.active.idu.ratio <- function(location,
                                          specification.metadata,
                                          population.year = DEFAULT.POPULATION.YEARS[1],
                                          idu.mortality=0.0166)
{
    remission = get.idu.remission.rates(specification.metadata)
    relapse = get.idu.relapse.rates(specification.metadata)
    
#    (remission + idu.mortality) / relapse
    remission / relapse / 2
       
#    (remission + idu.mortality) / relapse / 2
    
    # n.ages = specification.metadata$n.ages
    # 
    # remission = get.idu.remission.rates(specification.metadata)
    # relapse = get.idu.relapse.rates(specification.metadata)
    # 
    # raw.aging = do.get.empiric.aging.rates(location, specification.metadata, years = population.year)[[1]][,,,'IDU_in_remission']
    # 
    # target.dim.names = dimnames(raw.aging)
    # target.dim.names[names(dimnames(remission))] = dimnames(remission)
    # target.dim.names[names(dimnames(relapse))] = dimnames(relapse)
    # 
    # aging = array(0, dim=sapply(target.dim.names, length), dimnames=target.dim.names)
    # array.access(aging, age=1:(n.ages-1)) = raw.aging * (1-(0:(n.ages-2))/(n.ages-1))^2
    # 
    # #array.access(aging, age=1) = array.access(raw.aging, age=1)
    # 
    # remission = expand.array(remission, target.dim.names)
    # relapse = expand.array(relapse, target.dim.names)
    # 
    # # From first principles, we would get steady state by either:
    # #  active * remission = prior * relapse
    # # But in empirical testing, adding aging in to the denominator 
    # #   in the first age bracket helps us approximate
    # #   the steady solution better, so here it is
    # remission / (relapse + aging)
}

get.active.to.never.idu.ratio <- function(location,
                                          specification.metadata,
                                          population.year = DEFAULT.POPULATION.YEARS[1])
{
    n.ages = specification.metadata$n.ages
    
    raw.incidence = get.idu.incidence.rates(specification.metadata)
    raw.mortality = EHE_BASE_PARAMETER_VALUES['idu.mortality']
    raw.aging = do.get.empiric.aging.rates(location, specification.metadata, years = population.year)[[1]][,,,'active_IDU']
    raw.female.pop = get.base.initial.female.population('C.12580', specification.metadata)
    raw.male.pop = get.base.initial.male.population('C.12580', specification.metadata)
    pop.dim.names = c(dimnames(raw.female.pop), specification.metadata$dim.names['sex'])
    raw.pop = expand.array(raw.male.pop, pop.dim.names)
    array.access(raw.pop, sex='female') = raw.female.pop
    
    if (!setequal(specification.metadata$dim.names$sex, c('msm','female','heterosexual_male')))
      stop("Cannot get.active.to.never.idu.ratio() - the function as written assumes that sex is <'heterosexual_male','msm','female'>")
    
    
    non.age.dim.names = dimnames(raw.aging)
    non.age.dim.names[names(dimnames(raw.incidence))] = dimnames(raw.incidence)
    # we know that mortality is a scalar
    non.age.dim.names = non.age.dim.names[setdiff(names(non.age.dim.names), 'age')]
    
    incidence = lapply(1:n.ages, function(i){
        expand.array(array.access(raw.incidence, age=i, drop = T), non.age.dim.names)
    })
    
    aging = lapply(1:(n.ages-1), function(i){
        expand.array(array.access(raw.aging, age=i, drop = T), non.age.dim.names)
    })    
    aging[[n.ages]] = 0
    
    mortality = lapply(1:n.ages, function(i){
        raw.mortality
    })
    
    pop = lapply(1:n.ages, function(i){
        expand.array(array.access(raw.pop, age=i, drop = T), non.age.dim.names)
    })
    
    # Put it together into a list
    p.active = list()
    p.active[[1]] = incidence[[1]] / (incidence[[1]] + mortality[[1]] + aging[[1]])
    for (i in 2:n.ages)
        p.active[[i]] = (incidence[[i]] + p.active[[i-1]]*aging[[i-1]]*pmin(1,pop[[i-1]]/pop[[i]])) /
                        (incidence[[i]] + mortality[[i]] + aging[[i]])

    # Turn into an array
    rv = t(sapply(p.active, function(p){
        p / (1-p)
    }))
    dim.names = c(specification.metadata$dim.names['age'],
                  non.age.dim.names)
    dim(rv) = sapply(dim.names, length)
    dimnames(rv) = dim.names
    
    # Scale to age-specific targets
    # I think this is more complicated than is worthwhile
#    print("In get.active.to.non.idu.ratio(), we need to implement scaling to the p observed in data")
    
    # Return
    rv
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

get.seed.population <- function(location, specification.metadata)
{
    dim.names = specification.metadata$dim.names[c('age','sex')]
    rv = array(0, dim=sapply(dim.names, length), dimnames = dim.names)
    
    seed.age = ceiling(specification.metadata$n.ages/2)
    array.access(rv, age=seed.age, sex='msm') = 1
    
    rv
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

get.race.population.counts <- function(location, 
                                       specification.metadata, 
                                       years=DEFAULT.POPULATION.YEARS)
{
    counties = locations::get.contained.locations(location, 'county')
    population = CENSUS.MANAGER$pull(outcome='population', 
                                     dimension.values = list(location=counties, year=years),
                                     keep.dimensions = c('location','age','race','ethnicity', 'sex'),
                                     from.ontology.names = 'census')
    
    if (is.null(population))
      stop(paste0("Cannot get.race.population.counts() - no census data were available for the counties of '", location, "'"))
    
    parsed.ages = parse.age.strata.names(dimnames(population)$age)
    age.mask = parsed.ages$lower >= specification.metadata$age.lower.bounds[1] &
      parsed.ages$upper <= specification.metadata$age.upper.bounds[specification.metadata$n.ages]
    
    race.mapping = get.ontology.mapping(from.ontology = dimnames(population)[c('race','ethnicity')],
                                        to.ontology = specification.metadata$dim.names['race'])
    
    if (is.null(race.mapping))
      stop("Cannot get.race.population.counts() - don't know how to map race to the desired ontology for the specification")
    
    race.mapping$apply(population[,age.mask,,,,], to.dim.names = specification.metadata$dim.names['race'])
}

get.female.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
    pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='female'),
                              keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'census') / length(population.years)
    
    array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

get.male.single.year.age.counts <- function(location, population.years=DEFAULT.POPULATION.YEARS)
{
    counties = get.contained.locations(location, 'county')
    pop = CENSUS.MANAGER$pull(outcome='population', dimension.values = list(year=population.years, location=counties, sex='male'),
                              keep.dimensions = c('age','race','sex','ethnicity'), from.ontology.names = 'census') / length(population.years)
    
    array(apply(pop, 'age', sum), dim=c(age=length(CENSUS.AGES)), dimnames=list(age=CENSUS.AGES))
}

get.msm.single.year.age.counts <- function(location, specification.metadata,
                                           population.years=DEFAULT.POPULATION.YEARS)
{
    # rv = rowSums(get.best.guess.msm.proportions.by.race(location,
    #                                                     specification.metadata = specification.metadata,
    #                                                     years = population.years,
    #                                                     return.proportions = F,
    #                                                     min.age = 0,
    #                                                     keep.ages = T))
    # 
    # ages = parse.age.strata.names(names(rv))$lower
    # dim(rv) = c(age=length(rv))
    # dimnames(rv) = list(age=ages)
    # 
    # rv
    
    rv = get.best.guess.msm.proportions(location,
                                   specification.metadata = specification.metadata,
                                   years=population.years,
                                   keep.age = T,
                                   keep.race = F,
                                   return.proportions = F,
                                   ages = NULL)
    ages = parse.age.strata.names(dimnames(rv)$age)$lower
    dimnames(rv)$age = as.character(ages)
    
    rv
}

get.heterosexual.male.single.year.age.counts <- function(location, specification.metadata,
                                                         population.years=DEFAULT.POPULATION.YEARS)
{
    get.male.single.year.age.counts(location = location, population.years = population.years)
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
                                             age.model = PAIRING.INPUT.MANAGER$sex.age.models$msm,
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
                                             age.model = PAIRING.INPUT.MANAGER$sex.age.models$heterosexual_male,
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
                                             age.model = PAIRING.INPUT.MANAGER$sex.age.models$female,
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
                                             age.model = PAIRING.INPUT.MANAGER$idu.age.model,
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
                               single.year.age.counts=age.counts[names(availability)]*availability,
                               sd.multiplier=age.mixing.sd.mult)
}

get.sexual.availability <- function()
{
    rv = rep(1, length(CENSUS.AGES))
    names(rv) = CENSUS.AGES
    
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
    rv = rep(1, length(CENSUS.AGES))
    names(rv) = CENSUS.AGES
    
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


##-------------##
##-- Testing --##
##-------------##

get.testing.functional.form = function(specification.metadata){
  
  testing.prior = get.cached.object.for.version(name = "testing.prior",
                                                version = specification.metadata$version) 
  
  testing.functional.form = create.logistic.linear.functional.form(intercept = testing.prior$intercepts - log(0.9), #helps counteract max value below a bit
                                                                   slope = testing.prior$slopes,
                                                                   anchor.year = 2010,
                                                                   max = 0.9,
                                                                   parameters.are.on.logit.scale = T)                                         
    
  testing.functional.form
}



get.undiagnosed.testing.increase.functional.form <- function(specification.metadata){
  
  multipliers = get.undiagnosed.testing.rr(specification.metadata = specification.metadata) 
  if(any(multipliers<=1))
    stop("Error in get.undiagnosed.testing.increase.functional.form must be >1")
  
  # do this so that when we sample the multiplier, we're actually sampling a multiplier of how much greater than 1 it is 
  #(don't want it to go below 1)
  increase = multipliers - 1 
  
  create.static.functional.form(value = increase,
                                link = "log",
                                value.is.on.transformed.scale = F) 
  
}

get.undiagnosed.testing.rr <- function(specification.metadata){
  
  high.risk.testing.rate.ratios = get.cached.object.for.version(name = "high.risk.testing.rate.ratios",
                                                version = specification.metadata$version) 

}

## COVID-specific multipliers for undiagnosed testing rr
## (regular years value) * (this multiplier) = (covid years value)
get.undiagnosed.testing.covid.rr.functional.form <- function(specification.metadata){
  
  multipliers = get.undiagnosed.testing.covid.rrr(specification.metadata = specification.metadata) 
  
  create.static.functional.form(value = multipliers,
                                link = "log",
                                value.is.on.transformed.scale = F) 
  
}

get.undiagnosed.testing.covid.rrr <- function(specification.metadata){
  
  high.risk.testing.covid.ratio.of.ratios = get.cached.object.for.version(name = "high.risk.testing.covid.ratio.of.ratios",
                                                                          version = specification.metadata$version) 
  
}

##----------##
##-- PrEP --##
##----------##

get.fraction.sexual.transmission.avoidable.logit.parameter <- function(get.mean=T)
{  
  # values from ~../jheem_analyses/input_managers/fraction_preventable_with_prep.R

   # Heterosexual: STI in the past year
  # het.mean = 0.3447421
  # het.sd =  0.08971986
    het.param = get.approx.logitnorm.params(expit.mean = 0.3447421,
                                            expit.sd = 0.08971986)[c(get.mean, !get.mean)][[1]]
    # MSM: >1 partner in the past year & condomless sex
    # msm.mean = 0.9529412
    # msm.sd =  0.02296911
    msm.param = get.approx.logitnorm.params(expit.mean = 0.9529412,
                                            expit.sd = 0.02296911)[c(get.mean, !get.mean)][[1]]
    
    array(c(het.param, msm.param, het.param),
          dim = c(sex=3),
          dimnames = list(sex=c("heterosexual_male","msm","female")))
}

##---------------------------------##
##-- AIDS to HIV DIANGOSIS RATIO --##
##---------------------------------##

get.aids.to.hiv.diagnosis.ratio.07.08 <- function(location)
{
    aids.dx = SURVEILLANCE.MANAGER$pull(outcome = 'aids.diagnoses', location=location, year='2007')
    if (is.null(aids.dx) || all(is.na(aids.dx)))
      stop(paste0("Cannot get data to create aids-to-hiv diagnosis ratio likelihood: there are no data on AIDS diagnoses for location ", location, " in 2007"))          
    aids.dx = mean(aids.dx, na.rm = T)
    
    hiv.dx = SURVEILLANCE.MANAGER$pull(outcome = 'diagnoses', location=location, year='2008')
    if (is.null(hiv.dx) || all(is.na(hiv.dx)))
      stop(paste0("Cannot get data to create aids-to-hiv diagnosis ratio likelihood: there are no data on HIV diagnoses for location ", location, " in 2008"))          
    hiv.dx = mean(hiv.dx, na.rm = T)
    
    aids.dx / hiv.dx
}

get.aids.to.hiv.diagnosis.ratio.functional.form <- function(location)
{
    create.linear.spline.functional.form(knot.times = c(time.peak=1980,
                                                        time.0 = 1995,#1998,
                                                        time.1 = 2007),#2005),
                                         knot.values = list(time.peak = 1.09264522805781, # see aids_diagnoses_multiplier.R
                                                            time.0 = 0.163728677501474,
                                                            time.1 = log(get.aids.to.hiv.diagnosis.ratio.07.08(location))),
                                         overwrite.knot.values.with.alphas = F,
                                         link = 'log', 
                                         knot.link = 'log',
                                         knots.are.on.transformed.scale = T,
                                         min = 0,
                                         max = Inf)
}
