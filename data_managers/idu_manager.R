# TO ACCESS NSHDUH
# https://pdas.samhsa.gov/saes/substate

#source('../code/data_managers/locale_mappings.R')
#source('../code/data_managers/census_manager.R')
library(sas7bdat)
library(jheem)

if (1==2)
{
    idu2016 = read.idu.manager('../../v1_data/IDU/NSDUH_2016', 2016)
    idu2015 = read.idu.manager('../../v1_data/IDU/NSDUH_2015', 2015)
#    idu2014 = read.idu.manager('../../v1_data/IDU/NSDUH_2014', 2014)

    idu.manager = combine.idu.managers(idu2016, idu2015)

    counties = c("24510", "24005")
    load('../../v1_data/parsed/full_age_census_msm.Rdata')

    counties = counties[1]

    test.age.cutoffs = c(12, STANDARD.AGE.CUTOFFS[-1])
    dim.names = dimnames(idu.manager$idu.1yr.risk)
    dim.names[['age']] = make.age.strata(test.age.cutoffs)$labels
    pop1 = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    for (age in 1:dim(pop)['age'])
        pop1[age,,] = get.census.data(census, years=idu.manager$years, fips=counties,
                                              sexes=dimnames(idu.manager$idu.1yr.risk)[['sex']],
                                              races=dimnames(idu.manager$idu.1yr.risk)[['race']],
                                              ages=test.age.cutoffs[age]:min((test.age.cutoffs[age+1]-1), max(census$age.lowers)),
                                              aggregate.years = T, aggregate.ages = T, aggregate.counties=T)
    dim.names[['race']] = c('black','hispanic','other')
    pop = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    pop[,'black',] = pop1[,'black',]
    pop[,'hispanic',] = pop1[,'hispanic',]
    pop[,'other',] = pop1[,'white',] + pop1[,'asian',] + pop1[,'american_indian_or_alaska_native',]



    test.prev = get.idu.prevalence(idu.manager, census, age.cutoffs = test.age.cutoffs,
                                   counties=counties)
    total.prev = sum(test.prev * pop) / sum(pop)
    total.prev / idu.manager$idu.to.heroin.ratio[1]

    remission.rates = get.idu.remission.rates(idu.manager, census, counties)
    relapse.rates = get.idu.relapse.rates(idu.manager, census, counties)
    init.rates = get.incident.idu.rates(idu.manager, census, counties)
}

##-----------------##
##-- THE GETTERS --##
##-----------------##

BLACK.HISPANIC.OTHER.MAPPING = c(black='black', hispanic='hispanic',
                                 american_indian_or_alaska_native='other',
                                 asian='other', white='other')
KEEP.RACE.MAPPING = c(black='black', hispanic='hispanic',
                      american_indian_or_alaska_native='american_indian_or_alaska_native',
                      asian='asian', white='white')
STANDARD.AGE.CUTOFFS = c(13,25,35,45,55,Inf)
DEFAULT.YEAR = 2016

get.incident.idu.model <- function(idu.manager,
                                   census,
                                   location,
                                   specification.info,
                                   static,
                                   knot.times=c(2000,2016))
{
    rates = get.incident.idu.rates(idu.manager,
                                   census=census,
                                   counties=counties.for.msa(location),
                                   age.cutoffs=specification.info$age.endpoints,
                                   race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                                   years=idu.manager$years)
    
    if (static)
        create.static.functional.form(value=rates, link='log')
    else
    {
        knot.values = lapply(1:length(knot.times), function(i){rates})
        names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
        create.natural.spline.functional.form(knot.times = knot.times,
                                              knot.values = knot.values,
                                              before.time = min(knot.times)-10,
                                              after.time = max(knot.times)+10,
                                              link='log',
                                              after.modifier = .025 / (1-.05-.025),
                                              before.modifier = .05 / (1-.05-.025),
                                              after.modifier.application = 'multiplicative.of.change.on.value.scale',
                                              before.modifier.application = 'multiplicative.of.change.on.value.scale')
    }
}

get.idu.remission.model <- function(idu.manager,
                             census,
                             location,
                             specification.info,
                             static,
                             knot.times=c(2000,2016))
{
    rates = get.idu.remission.rates(idu.manager,
                           census=census,
                           counties=counties.for.msa(location),
                           age.cutoffs=specification.info$age.endpoints,
                           race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                           years=idu.manager$years)

    if (static)
        create.static.functional.form(value=rates, link='log')
    else    
    {
        knot.values = lapply(1:length(knot.times), function(i){rates})
        names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
        create.natural.spline.functional.form(knot.times = knot.times,
                                              knot.values = knot.values,
                                              before.time = min(knot.times)-10,
                                              after.time = max(knot.times)+10,
                                              link='log',
                                              after.modifier = .025 / (1-.05-.025),
                                              before.modifier = .05 / (1-.05-.025),
                                              after.modifier.application = 'multiplicative.of.change.on.value.scale',
                                              before.modifier.application = 'multiplicative.of.change.on.value.scale')
    }
}

get.idu.relapse.model <- function(idu.manager,
                              census,
                              location,
                              specification.info,
                              static,
                              knot.times=c(2000,2016))
{
    rates = get.idu.relapse.rates(idu.manager,
                            census=census,
                            counties=counties.for.msa(location),
                            age.cutoffs=specification.info$age.endpoints,
                            race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                            years=idu.manager$years)
    if (static)
        create.static.functional.form(value=rates, link='log')
    else
    {
        knot.values = lapply(1:length(knot.times), function(i){rates})
        names(knot.values) = names(knot.times) = paste0("time", (0:length(knot.values))[-length(knot.values)])
        create.natural.spline.functional.form(knot.times = knot.times,
                                              knot.values = knot.values,
                                              before.time = min(knot.times)-10,
                                              after.time = max(knot.times)+10,
                                              link='log',
                                              after.modifier = .025 / (1-.05-.025),
                                              before.modifier = .05 / (1-.05-.025),
                                              after.modifier.application = 'multiplicative.of.change.on.value.scale',
                                              before.modifier.application = 'multiplicative.of.change.on.value.scale')
    }
}


get.incident.idu.rates <- function(idu.manager,
                                   census,
                                   counties,
                                   age.cutoffs=STANDARD.AGE.CUTOFFS,
                                   race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                                   years=idu.manager$years)
{
    ages = make.age.strata(age.cutoffs)
    max.init.age = length(idu.manager$init.denominators.by.age)

    init.at.age.risk = sapply(1:length(ages$labels), function(i){
        mask = (1:max.init.age) >= ages$lowers[i] & (1:max.init.age) < ages$uppers[i]
        sum(idu.manager$init.numerators.by.age[mask]) / sum(idu.manager$init.denominators.by.age[mask])
    })

    #map the five-age risks to our desired ages,
    # using the init denominators to aggregate
    idu.ever.by.age = sapply(1:length(ages$labels), function(i){
        mask = (1:max.init.age) >= ages$lowers[i] & (1:max.init.age) < ages$uppers[i]
        denominator.components = idu.manager$init.denominators.by.age[mask]
        risks = sapply((1:max.init.age)[mask], function(age){
            mask.into.five = age >= idu.manager$five.age.cutoffs[-length(idu.manager$five.age.cutoffs)] &
                age < idu.manager$five.age.cutoffs[-1]
            sum(idu.manager$idu.by.age[1:3, mask.into.five]) / sum(idu.manager$idu.by.age[,mask.into.five])
        })

        sum(risks * denominator.components) / sum(denominator.components)
    })


    idu.ever.for.counties = get.idu.prevalence(idu.manager, census,
                                               counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                               years=years,
                                               aggregate.counties=T,
                                               use.30d=F)

    init.at.age.risk * idu.ever.for.counties / idu.ever.by.age
}

get.idu.remission.rates <- function(idu.manager,
                                    census,
                                    counties,
                                    age.cutoffs=STANDARD.AGE.CUTOFFS,
                                    race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                                    years=idu.manager$years)
{
    idu.30d = get.idu.prevalence(idu.manager, census,
                                 counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                 years=years,
                                 aggregate.counties=T,
                                 use.30d=T)

    idu.1yr = get.idu.prevalence(idu.manager, census,
                                 counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                 years=years,
                                 aggregate.counties=T,
                                 use.30d=F)

    proportions.unquit = idu.30d / idu.1yr
    -log(proportions.unquit)
}

get.idu.relapse.rates <- function(idu.manager,
                                  census,
                                  counties,
                                  age.cutoffs=STANDARD.AGE.CUTOFFS,
                                  race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                                  years=idu.manager$years)
{
    idu.30d = get.idu.prevalence(idu.manager, census,
                                 counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                 years=years,
                                 aggregate.counties=T,
                                 use.30d=T)

    idu.ever = get.idu.prevalence(idu.manager, census,
                                  counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                  years=years,
                                  aggregate.counties=T,
                                  use.ever = T)

    remission.rates = get.idu.remission.rates(idu.manager, census,
                                              counties=counties, age.cutoffs=age.cutoffs, race.mapping=race.mapping,
                                              years=years)

    idu.30d * remission.rates / idu.ever
}

get.idu.prevalence <- function(idu.manager,
                               census,
                               counties,
                               use.30d=F,
                               use.ever=F,
                               age.cutoffs=STANDARD.AGE.CUTOFFS,
                               race.mapping=BLACK.HISPANIC.OTHER.MAPPING,
                               years=idu.manager$years,
                               aggregate.counties = T,
                               do.not.scale.proportions.to.match.overall=F)
{
    counties = format.combined.fips(counties)
    
    #Get the population sizes
    dim.names = c(list(county=counties), dimnames(idu.manager$idu.1yr.risk))
    populations = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    
    for (age in 1:dim(populations)['age'])
        populations[,age,,] = get.census.data(census, years=years, fips=counties,
                                              sexes=dimnames(idu.manager$idu.1yr.risk)[['sex']],
                                              races=dimnames(idu.manager$idu.1yr.risk)[['race']],
                                              ages=idu.manager$five.age.cutoffs[age]:min((idu.manager$five.age.cutoffs[age+1]-1), max(census$age.lowers)),
                                              aggregate.years = T, aggregate.ages = T)

    numerators = array(NA, dim=dim(populations), dimnames = dimnames(populations))
    for (fips in counties)
    {
        if (!any(names(idu.manager$substate.heroin.use.all.ages)==fips))
            stop("The county of '", paste0(county.names(fips, include.state.abbreviation = T), "' does not map to a substate region in the dataset for heroin use"))

        population = populations[fips,,,]

        #Estimate the substate IDU 1-year prevalence
        heroin.by.age = idu.manager$substate.heroin.use.by.age[fips,]
        heroin.all.ages = idu.manager$substate.heroin.use.all.ages[fips]
        metro.size = URBANIZATION.TO.LARGE.SMALL.NON.METRO[county.urbanization(fips, return.as.text = F)]
        metro.size = c(large=1, small=2, non=3)[metro.size]

        idu.1yr.by.age = heroin.by.age * idu.manager$idu.to.heroin.ratio[metro.size]
        idu.1yr.all.ages = heroin.all.ages * idu.manager$idu.to.heroin.ratio[metro.size]
        use.all.ages = any(is.na(idu.1yr.by.age))


        #Scale the idu manager's population risk
        estimated.population.risk = idu.manager$idu.1yr.risk
        estimated.numerators = estimated.population.risk * populations[fips,,,]

        estimated.risk.all = sum(estimated.numerators) / sum(population)
        estimated.risk.1 = sum(estimated.numerators[1,,]) / sum(population[1,,])
        estimated.risk.2 = sum(estimated.numerators[2,,]) / sum(population[2,,])
        estimated.risk.3 = sum(estimated.numerators[3:5,,]) / sum(population[3:5,,])

        if (do.not.scale.proportions.to.match.overall)
            correction.multipliers = c(1,1,1,1,1)
        else if (use.all.ages)
            correction.multipliers = rep(idu.1yr.all.ages/estimated.risk.all, 5)
        else
            correction.multipliers = c(idu.1yr.by.age[1]/estimated.risk.1,
                                       idu.1yr.by.age[2]/estimated.risk.2,
                                       rep(idu.1yr.by.age[3]/estimated.risk.3, 3))

        estimated.population.risk = correction.multipliers * estimated.population.risk

        numerators[fips,,,] = estimated.population.risk * population
    }

    proportions = numerators / populations

    if (use.30d)
    {
        for (fips in counties)
            proportions[fips,,,] = proportions[fips,,,] * idu.manager$idu.30d.risk / idu.manager$idu.1yr.risk
    }
    else if (use.ever)
    {
        for (fips in counties)
           proportions[fips,,,] = proportions[fips,,,] * idu.manager$idu.ever.risk / idu.manager$idu.1yr.risk
    }

    rv = map.proportions.to.restratified.population(proportions, census,
                                               age.cutoffs.from=idu.manager$five.age.cutoffs,
                                               age.cutoffs.to=age.cutoffs,
                                               race.mapping=race.mapping,
                                               counties=counties,
                                               years=years,
                                               aggregate.counties=T)
    rv
}

#to help with likelihood
get.aggregate.idu.proportions <- function(idu.manager,
                                    census,
                                    fips,
                                    years)
{
    #heroin.by.age = idu.manager$substate.heroin.use.by.age[fips,]
    heroin.all.ages = idu.manager$substate.heroin.use.all.ages[fips]
    metro.size = URBANIZATION.TO.LARGE.SMALL.NON.METRO[county.urbanization(fips, return.as.text = F)]
    metro.size = c(large=1, small=2, non=3)[metro.size]

    idu.30d.to.heroin.by.urbanization = apply(idu.manager$heroin.vs.idu.by.urbanization['within_30d',,], 'urbanization', sum) /
        apply(idu.manager$heroin.vs.idu.by.urbanization[,,'within_1yr'], 'urbanization', sum)
    idu.ever.to.heroin.by.urbanization = apply(idu.manager$heroin.vs.idu.by.urbanization[c('within_30d','30d_to_1yr','more_than_1yr'),,], 'urbanization', sum) /
        apply(idu.manager$heroin.vs.idu.by.urbanization[,,'within_1yr'], 'urbanization', sum)
    idu.prior.to.heroin.by.urbanization = apply(idu.manager$heroin.vs.idu.by.urbanization[c('30d_to_1yr','more_than_1yr'),,], 'urbanization', sum) /
        apply(idu.manager$heroin.vs.idu.by.urbanization[,,'within_1yr'], 'urbanization', sum)

    idu.30d.by.county = heroin.all.ages * idu.30d.to.heroin.by.urbanization[metro.size]
    idu.ever.by.county = heroin.all.ages * idu.ever.to.heroin.by.urbanization[metro.size]
    idu.prior.by.county = heroin.all.ages * idu.prior.to.heroin.by.urbanization[metro.size]

    populations = get.census.data(census, years=years, fips=fips, aggregate.years = T,
                                  aggregate.ages = T, aggregate.races = T, aggregate.sexes = T)

    numerators.30d = idu.30d.by.county * populations
    numerators.ever = idu.ever.by.county * populations
    numerators.prior = idu.prior.by.county * populations

    list(idu.30d=sum(numerators.30d)/sum(populations),
         idu.ever=sum(numerators.ever)/sum(populations),
         idu.prior=sum(numerators.prior)/sum(populations))
}

get.relative.idu.proportions <- function(idu.manager,
                                         census,
                                         fips,
                                         years,
                                         relative.to.sex='female',
                                         relative.to.age=3,
                                         relative.to.race='other',
                                         age.cutoffs=STANDARD.AGE.CUTOFFS,
                                         race.mapping=BLACK.HISPANIC.OTHER.MAPPING)
{
    dim.names = list(county=as.character(fips),
                     age=dimnames(idu.manager$idu.by.age)[['age']],
                     race=dimnames(idu.manager$idu.by.race)[['race']],
                     sex=c('male','female'))

    #-- By Sex --#
    idu.30d.proportions = idu.manager$idu.by.sex['within_30d',]/colSums(idu.manager$idu.by.sex)
    idu.ever.proportions = colSums(idu.manager$idu.by.sex[c('within_30d','30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.sex)
    idu.prior.proportions = colSums(idu.manager$idu.by.sex[c('30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.sex)

    rv = list(idu.30d.by.sex = idu.30d.proportions / idu.30d.proportions[relative.to.sex],
              idu.ever.by.sex = idu.ever.proportions / idu.ever.proportions[relative.to.sex],
              idu.prior.by.sex = idu.prior.proportions / idu.prior.proportions[relative.to.sex])

    #-- By Age --#
    idu.30d.proportions = idu.manager$idu.by.age['within_30d',]/colSums(idu.manager$idu.by.age)
    idu.ever.proportions = colSums(idu.manager$idu.by.age[c('within_30d','30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.age)
    idu.prior.proportions = colSums(idu.manager$idu.by.age[c('30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.age)

    idu.30d.proportions = map.proportions.to.restratified.population(expand.population(idu.30d.proportions, dim.names),
                                                                     census,
                                                                     age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                     age.cutoffs.to=age.cutoffs,
                                                                     race.mapping=race.mapping,
                                                                     counties=fips,
                                                                     years=years,
                                                                     aggregate.counties=T)
    idu.30d.proportions = apply(idu.30d.proportions, 'age', mean)

    idu.ever.proportions = map.proportions.to.restratified.population(expand.population(idu.ever.proportions, dim.names),
                                                                     census,
                                                                     age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                     age.cutoffs.to=age.cutoffs,
                                                                     race.mapping=race.mapping,
                                                                     counties=fips,
                                                                     years=years,
                                                                     aggregate.counties=T)
    idu.ever.proportions = apply(idu.ever.proportions, 'age', mean)

    idu.prior.proportions = map.proportions.to.restratified.population(expand.population(idu.prior.proportions, dim.names),
                                                                      census,
                                                                      age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                      age.cutoffs.to=age.cutoffs,
                                                                      race.mapping=race.mapping,
                                                                      counties=fips,
                                                                      years=years,
                                                                      aggregate.counties=T)
    idu.prior.proportions = apply(idu.prior.proportions, 'age', mean)

    rv$idu.30d.by.age = idu.30d.proportions / idu.30d.proportions[relative.to.age]
    rv$idu.ever.by.age = idu.ever.proportions / idu.ever.proportions[relative.to.age]
    rv$idu.prior.by.age = idu.prior.proportions / idu.prior.proportions[relative.to.age]

    #-- By Race --#
    idu.30d.proportions = idu.manager$idu.by.race['within_30d',]/colSums(idu.manager$idu.by.race)
    idu.ever.proportions = colSums(idu.manager$idu.by.race[c('within_30d','30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.race)
    idu.prior.proportions = colSums(idu.manager$idu.by.race[c('30d_to_1yr','more_than_1yr'),])/colSums(idu.manager$idu.by.race)

    idu.30d.proportions = map.proportions.to.restratified.population(expand.population(idu.30d.proportions, dim.names),
                                                                     census,
                                                                     age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                     age.cutoffs.to=age.cutoffs,
                                                                     race.mapping=race.mapping,
                                                                     counties=fips,
                                                                     years=years,
                                                                     aggregate.counties=T)
    idu.30d.proportions = apply(idu.30d.proportions, 'race', mean)

    idu.ever.proportions = map.proportions.to.restratified.population(expand.population(idu.ever.proportions, dim.names),
                                                                      census,
                                                                      age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                      age.cutoffs.to=age.cutoffs,
                                                                      race.mapping=race.mapping,
                                                                      counties=fips,
                                                                      years=years,
                                                                      aggregate.counties=T)
    idu.ever.proportions = apply(idu.ever.proportions, 'race', mean)

    idu.prior.proportions = map.proportions.to.restratified.population(expand.population(idu.prior.proportions, dim.names),
                                                                      census,
                                                                      age.cutoffs.from=idu.manager$five.age.cutoffs,
                                                                      age.cutoffs.to=age.cutoffs,
                                                                      race.mapping=race.mapping,
                                                                      counties=fips,
                                                                      years=years,
                                                                      aggregate.counties=T)
    idu.prior.proportions = apply(idu.prior.proportions, 'race', mean)

    rv$idu.30d.by.race = idu.30d.proportions / idu.30d.proportions[relative.to.age]
    rv$idu.ever.by.race = idu.ever.proportions / idu.ever.proportions[relative.to.age]
    rv$idu.prior.by.race = idu.prior.proportions / idu.prior.proportions[relative.to.age]

    #-- Return --#
    rv
}


map.proportions.to.restratified.population <- function(proportions, census,
                                                       age.cutoffs.from, age.cutoffs.to,
                                                       race.mapping,
                                                       counties, years, aggregate.counties)
{
    proportions[is.na(proportions)] = 0

    #Map age
    ages.to = make.age.strata(age.cutoffs.to)
    n.ages.to = length(ages.to$labels)
    ages.from = make.age.strata(age.cutoffs.from)
    n.ages.from = length(ages.from$labels)

    dim.names.2 = dimnames(proportions)
    dim.names.2[['age']] = ages.to$labels

    numerators.2 = denominators.2 = array(0, dim=sapply(dim.names.2, length), dimnames=dim.names.2)

    for (age.to in 1:n.ages.to)
    {
        for (age.from in 1:n.ages.from)
        {
            age.overlap = intersect(ages.to$lowers[age.to]:min(ages.to$uppers[age.to]-1, max(census$age.lowers)),
                                    ages.from$lowers[age.from]:min(ages.from$uppers[age.from]-1, max(census$age.lowers)))

            if (length(age.overlap) > 0)
            {
                pop = get.census.data(census, years=years, fips=counties, ages=age.overlap,
                                      races = dimnames(proportions)[['race']], sexes = dimnames(proportions)[['sex']],
                                      aggregate.years = T, aggregate.ages = T)
                
                access(denominators.2, age=age.to) = access(denominators.2, age=age.to) + pop[counties,,]
                access(numerators.2, age=age.to) = access(numerators.2, age=age.to) + pop[counties,,] * access(proportions, age=age.from)
            }
        }
    }

    #Map to race
    races.from = names(race.mapping)
    races.to = unique(race.mapping)

    dim.names.3 = dimnames(numerators.2)
    dim.names.3[['race']] = races.to

    numerators.3 = denominators.3 = array(0, dim=sapply(dim.names.3, length), dimnames=dim.names.3)

    for (race.from in races.from)
    {
        race.to = race.mapping[race.from]
        access(numerators.3, race=race.to) = access(numerators.3, race=race.to) + access(numerators.2, race=race.from)
        access(denominators.3, race=race.to) = access(denominators.3, race=race.to) + access(denominators.2, race=race.from)
    }

    if (aggregate.counties)
    {
        numerators.3 = colSums(numerators.3)
        denominators.3 = colSums(denominators.3)
    }

    numerators.3 / denominators.3
}

##----------------------------##
##-- DATA READING FUNCTIONS --##
##----------------------------##


combine.idu.managers <- function(...)
{
    managers = list(...)

    rv = managers[[1]]

    for (manager in managers[-1])
    {
        for (elem.name in names(manager))
        {
            if (!grepl('cutoff', elem.name))
                rv[[elem.name]] = rv[[elem.name]] + manager[[elem.name]]
        }
    }
    rv$years = unique(sapply(managers, function(m){m$years}))

    rv = crunch.idu.calcs(rv)

    rv
}

read.idu.manager <- function(dir='../../v1_data/IDU/NSDUH_2016',
                             year=2016, verbose=T)
{
    rv = list(years=year)

##-- SOME SETUP --##

    past.year.use = c('within_1yr', 'not_within_1yr')
    use = c('within_30d', '30d_to_1yr', 'more_than_1yr', 'never')

    metro = c('large_metro', 'small_metro', 'non_metro')
    ages = c('12-17 years', '18-25 years', '26-34 years', '35-49 years', '50+ years')
    races = c('white','black',"american_indian_or_alaska_native","asian",'hispanic')
    sexes = c('heterosexual_male', 'msm', 'female')

    sex.codes = c('male', 'female')
    orientation.codes = c('heterosexual', 'homosexual')

    age.cutoffs = c(12,18,26,35,50,Inf)
    rv$five.age.cutoffs = age.cutoffs

##-- SUBSTATE HEROIN RATES --##

    if (verbose)
        print('Reading in Substate Heroin Use Prevalences...')

    # if this is breaking, it's because we changed to use the locale mappings object
    heroin.all.ages = read.csv(paste0(dir, '/substate_heroin_all_ages.csv'), stringsAsFactors = F)
    heroin.all.ages$substate.code = substate.region.code(region=heroin.all.ages$Substate.Region,
                                                         state=heroin.all.ages$State)
    heroin.all.ages = heroin.all.ages[!is.na(heroin.all.ages$substate.code),]
    heroin.all.ages$use.proportion = parse.percent.value(heroin.all.ages$Small.Area.Estimate)

    heroin.by.age = read.csv(paste0(dir, '/substate_heroin_by_age.csv'), stringsAsFactors = F)
    heroin.by.age$substate.code = substate.region.code(region=heroin.by.age$Substate.Region,
                                                         state=heroin.by.age$State)
    heroin.by.age = heroin.by.age[!is.na(heroin.by.age$substate.code),]
    heroin.by.age$use.proportion.12.17 = parse.percent.value(heroin.by.age$X12.17.Estimate)
    heroin.by.age$use.proportion.18.25 = parse.percent.value(heroin.by.age$X18.25.Estimate)
    heroin.by.age$use.proportion.26.plus = parse.percent.value(heroin.by.age$X26.or.Older.Estimate)

    substate.counties = unique(SUBSTATE.TO.COUNTY.MAPPING$combined.fips)
    rv$raw.substate.heroin.use.all.ages = sapply(substate.counties, function(fips){
        substate.codes = county.to.substate.codes(fips)

        all.mask = sapply(heroin.all.ages$substate.code, function(code){any(code==substate.codes)})
        if (any(all.mask))
            all.age.est = mean(heroin.all.ages$use.proportion[all.mask])
        else
            all.age.est = NA

        all.age.est
    })

    rv$raw.substate.heroin.use.by.age = t(sapply(substate.counties, function(fips){
        substate.codes = county.to.substate.codes(fips)

        by.age.mask = sapply(heroin.by.age$substate.code, function(code){any(code==substate.codes)})
        if (any(by.age.mask))
            rv = c(age.12.17=mean(heroin.by.age$use.proportion.12.17[by.age.mask]),
                   age.18.25=mean(heroin.by.age$use.proportion.18.25[by.age.mask]),
                   age.26.plus=mean(heroin.by.age$use.proportion.26.plus[by.age.mask]))
        else
            rv = c(NA,NA,NA)

        rv
    }))

    rv$substate.age.cutoffs = c(12,18,26,Inf)
    rv$substate.denominator = 1

##-- IDU and HEROIN by URBANIZATION --##

    if (verbose)
        print('Reading in IDU vs Heroin Use by Urbanization...')

    #Heroin vs IDU
    dim.names = list(idu = use, urbanization=metro, heroin=past.year.use)
    rv$heroin.vs.idu.by.urbanization = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    df = read.csv(paste0(dir, '/heroin_any_idu_by_metro.csv'), stringsAsFactors = F)
    df$metro = parse.demo.code(df$COUNTY.METRO.NONMETRO.STATUS..2013.3.LEVEL.)
    df = df[!is.na(df$metro) & df$metro<=3,]
    df$heroin = parse.demo.code(df$HEROIN.RECENCY...IMPUTATION.REVISED)
    df = df[!is.na(df$heroin),]
    df$heroin[df$heroin==9] = 4
    df$idu = parse.demo.code(df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE)
    df = df[!is.na(df$idu) & df$idu<=4, ]
    df$use = parse.demo.code(df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE)
    df = df[!is.na(df$use) & df$use >=1 & df$use <=4, ]

    dim.names = list(idu = use, urbanization=metro, heroin=use)
    heroin.use.vs.idu = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
    for (i in 1:dim(df)[1])
        heroin.use.vs.idu[df$use[i], df$metro[i], df$heroin[i]] = df$Weighted.Count[i]

    rv$heroin.vs.idu.by.urbanization[,,1] = heroin.use.vs.idu[,,1] + heroin.use.vs.idu[,,2]
    rv$heroin.vs.idu.by.urbanization[,,2] = heroin.use.vs.idu[,,3] + heroin.use.vs.idu[,,4]

##-- IDU by AGE, SEX, RACE --##

    if (verbose)
        print('Reading in IDU risk by age, race, and sex...')

    #-- Counts by age --#
    dim.names = list(use=use, age=ages)
    rv$idu.by.age = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    df = read.csv(paste0(dir, '/IDU_age.csv'), stringsAsFactors = F)
    df$age = parse.demo.code(df$RC.AGE.CATEGORY.RECODE..5.LEVELS.)
    df = df[!is.na(df$age),]
    df$use = parse.demo.code(df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE)
    df = df[!is.na(df$use) & df$use >=1 & df$use <=4, ]

    for (i in 1:dim(df)[1])
        rv$idu.by.age[df$use[i], df$age[i]] = df$Weighted.Count[i]

    #-- Counts by race --#
    dim.names = list(use=use, race=races)
    rv$idu.by.race = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

    df = read.csv(paste0(dir, '/IDU_race.csv'), stringsAsFactors = F)
    df$race = parse.demo.code(df$RC.RACE.HISPANICITY.RECODE..7.LEVELS.)
    df = df[!is.na(df$race) & df$race >=1 & df$race <=5,]
    df$use = parse.demo.code(df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE)
    df = df[!is.na(df$use) & df$use >=1 & df$use <=4, ]

    for (i in 1:dim(df)[1])
        rv$idu.by.race[df$use[i], df$race[i]] = df$Weighted.Count[i]


    #-- Counts by sex --#

    dim.names = list(use=use, sex=sexes)
    rv$idu.by.sex = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    df = read.csv(paste0(dir, '/IDU_sex.csv'), stringsAsFactors = F)
    df$sex = parse.demo.code(df$IMPUTATION.REVISED.GENDER)
    df = df[!is.na(df$sex),]
    df$sex = sex.codes[df$sex]
    df$orientation = parse.demo.code(df$SEXUAL.IDENTITY)
    df = df[!is.na(df$orientation) & df$orientation >=1 & df$orientation <=2,]
    df$orientation = orientation.codes[df$orientation]
    df$use = parse.demo.code(df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE)
    df = df[!is.na(df$use) & df$use >=1 & df$use <=4, ]

    for (i in 1:dim(df)[1])
    {
        if (df$sex[i]=='female')
            rv$idu.by.sex[df$use[i], 'female'] = rv$idu.by.sex[df$use[i], 'female'] + df$Weighted.Count[i]
        else if (df$orientation[i]=='heterosexual')
            rv$idu.by.sex[df$use[i], 'heterosexual_male'] = rv$idu.by.sex[df$use[i], 'heterosexual_male'] + df$Weighted.Count[i]
        else if (df$orientation[i]=='homosexual')
            rv$idu.by.sex[df$use[i], 'msm'] = rv$idu.by.sex[df$use[i], 'msm'] + df$Weighted.Count[i]
        else
            stop('Error: sex must be either male or female, and orientation either heterosexual or homosexual')

    }

##-- INITIATION BY AGE --##

    if (verbose)
        print('Reading in Heroin Initiation Rates by Age...')

    all.age.lowers = c(12,13,14,15,16,17,18,19,20,21,22,24,26,30,35,50)
    all.age.uppers = c(12,13,14,15,16,17,18,19,20,21,23,25,29,34,49,64)
    all.age.cutoffs = c(all.age.lowers, all.age.uppers[length(all.age.uppers)])

    df = rbind(read.csv(paste0(dir, '/heroin_init_age_12_to_20.csv'), stringsAsFactors = F),
               read.csv(paste0(dir, '/heroin_init_age_21_to_64.csv'), stringsAsFactors = F))

    df$age.now = parse.demo.code(df$RECODE...FINAL.EDITED.AGE)
    df = df[!is.na(df$age.now) & df$age.now <= length(all.age.lowers),]
    df$init.age = parse.demo.code(df$HEROIN.AGE.OF.FIRST.USE...IMPUTATION.REVISED)
    never.used = df[!is.na(df$init.age) & df$init.age==991,]
    df = df[!is.na(df$init.age) & df$init.age<=max(all.age.uppers),]
    df = df[df$init.age <= all.age.uppers[df$age.now],]


    age.numerators = numeric()
    age.denominators = numeric()

    for (age.index in 1:length(all.age.lowers))
    {
        age.subset = df[df$age.now==age.index,]
        lower = all.age.lowers[age.index]
        upper = all.age.uppers[age.index]

        span = 1 + upper - lower
        denominator.all = never.used$Weighted.Count[never.used$age.now==age.index] +
            sum(age.subset$Weighted.Count[age.subset$init.age>=(lower-1)]) #this is simplified

        for (age in lower:upper)
        {
            fraction.init.to.count = 1/(upper+1 - age)

            if (any(age.subset$init.age==age))
                init.at.age = age.subset$Weighted.Count[age.subset$init.age==age]
            else
                init.at.age = 0

            if (any(age.subset$init.age==(age-1)))
                init.at.age.minus.one = age.subset$Weighted.Count[age.subset$init.age==(age-1)]/2
            else
                init.at.age.minus.one = 0

            age.denominators[age] = denominator.all / span
            age.numerators[age] = fraction.init.to.count * (init.at.age + init.at.age.minus.one)

#            age.numerators[age.index] = age.numerators[age.index] +
#                fraction.init.to.count * (init.at.age + init.at.age.minus.one)

        }
    }

    rv$init.numerators.by.age = age.numerators
    rv$init.denominators.by.age = age.denominators
#    rv$init.numerators.by.age = rv$init.denominators.by.age = numeric(length(ages))
#    names(rv$init.numerators.by.age) = names(rv$init.denominators.by.age) = ages

#    for (age.index in 1:length(ages))
#    {
#        lower = age.cutoffs[age.index]
#        upper = age.cutoffs[age.index+1]
#        mask = all.age.lowers >= lower & all.age.uppers < upper
#        rv$init.numerators.by.age[age.index] = sum(age.numerators[mask])
#        rv$init.denominators.by.age[age.index] = sum(age.denominators[mask])
#    }


##-- RETURN IT --##
    crunch.idu.calcs(rv)
}

crunch.idu.calcs <- function(idu.manager)
{

#-- IDU to heroin ratio by urbanization --#

    heroin = colSums(idu.manager$heroin.vs.idu.by.urbanization, dims=1)
    heroin.proportion = heroin[,1] / rowSums(heroin)

    idu = rowSums(idu.manager$heroin.vs.idu.by.urbanization, dims=2)
#    idu.manager$idu.30d.proportion = idu[1,] / colSums(idu)
    idu.1yr.proportion = (idu[1,] + idu[2,]) / colSums(idu)

    idu.manager$idu.to.heroin.ratio = idu.1yr.proportion / heroin.proportion

#-- Risk of IDU by strata --#

    use = dimnames(idu.manager$idu.by.age)[[1]]
    ages = dimnames(idu.manager$idu.by.age)[[2]]
    races = dimnames(idu.manager$idu.by.race)[[2]]
    sexes = dimnames(idu.manager$idu.by.sex)[[2]]

    multiply.marginal.risks <- function(dims.to.use, prior.fraction=0.01)
    {
        dim.names = list(age=ages, race=races, sex=sexes)
        rv = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)

        num.by.age = sum(idu.manager$idu.by.age)
        overall.risk.for.age = sum(idu.manager$idu.by.age[dims.to.use,]) / num.by.age
        age.risk = (flex.col.sums(idu.manager$idu.by.age[dims.to.use,]) + overall.risk.for.age * prior.fraction * num.by.age) /
            (colSums(idu.manager$idu.by.age) + prior.fraction * num.by.age)

        num.by.race = sum(idu.manager$idu.by.race)
        overall.risk.for.race = sum(idu.manager$idu.by.race[dims.to.use,]) / num.by.race
        race.risk = (flex.col.sums(idu.manager$idu.by.race[dims.to.use,]) + overall.risk.for.race * prior.fraction * num.by.race) /
            (colSums(idu.manager$idu.by.race) + prior.fraction * num.by.race)
        race.risk.ratios = race.risk / overall.risk.for.race

        num.by.sex = sum(idu.manager$idu.by.sex)
        overall.risk.for.sex = sum(idu.manager$idu.by.sex[dims.to.use,]) / num.by.sex
        sex.risk = (flex.col.sums(idu.manager$idu.by.sex[dims.to.use,]) + overall.risk.for.sex * prior.fraction * num.by.sex) /
            (colSums(idu.manager$idu.by.sex) + prior.fraction * num.by.sex)
        sex.risk.ratios = sex.risk / overall.risk.for.sex

        for (a in 1:length(ages))
        {
            for (r in 1:length(races))
            {
                for (s in 1:length(sexes))
                {
                    rv[a,r,s] = age.risk[a] * race.risk.ratios[r] * sex.risk.ratios[s]
                }
            }
        }

        rv
    }

    idu.manager$idu.30d.risk = multiply.marginal.risks(1)
    idu.manager$idu.1yr.risk = idu.manager$idu.30d.risk + multiply.marginal.risks(2)
    idu.manager$idu.ever.risk = idu.manager$idu.1yr.risk + multiply.marginal.risks(3)

#-- Substates --#

    idu.manager$substate.heroin.use.all.ages = idu.manager$raw.substate.heroin.use.all.ages / idu.manager$substate.denominator
    idu.manager$substate.heroin.use.by.age = idu.manager$raw.substate.heroin.use.by.age / idu.manager$substate.denominator

#-- Return it --#
    idu.manager
}


flex.col.sums <- function(arr, dims=1)
{
    if (is.null(dim(arr)))
        arr
    else
        colSums(arr, dims=dims)
}


parse.demo.code <- function(values)
{
    rv = gsub('^([0-9]+).*', '\\1', values)#substr(values, 1, 1)
    is.digit = grepl('[0-9]+', rv)
    rv[!is.digit] = NA
    as.numeric(rv)
}

parse.percent.value <- function(values)
{
    present.mask = grepl('[0-9\\.]+%$', values)
    rv = rep(NA, length(values))
    rv[present.mask] = as.numeric(gsub('%', '', values[present.mask])) / 100

    rv
}

##-------------------------##
##-- FRACTION USE BY AGE --##
##-------------------------##

get.idu.by.age.counts <- function(dir='../../v1_data/IDU', years=2015:2018)
{
    age.mapping = c('1 - Respondent is 12 years old'='12 yo',
                    '2 - Respondent is 13 years old'='13 yo',
                    '3 - Respondent is 14 years old'='14 yo',
                    '4 - Respondent is 15 years old'='15 yo',
                    '5 - Respondent is 16 years old'='16 yo',
                    '6 - Respondent is 17 years old'='17 yo',
                    '7 - Respondent is 18 years old'='18 yo',
                    '8 - Respondent is 19 years old'='19 yo',
                    '9 - Respondent is 20 years old'='20 yo',
                    '10 - Respondent is 21 years old'='21 yo',
                    '11 - Respondent is 22 or 23 years old'='22-23 yo',
                    '12 - Respondent is 24 or 25 years old'='24-25 yo',
                    '13 - Respondent is between 26 and 29 years old'= '26-29 yo',
                    '14 - Respondent is between 30 and 34 years old'= '30-34 yo',
                    '15 - Respondent is between 35 and 49 years old'= '35-49 yo',
                    '16 - Respondent is between 50 and 64 years old'= '50-64 yo',
                    '17 - Respondent is 65 years old or older'= '65+yo',
                    'Overall'=NA)

    use.mapping = c('1 - Within the past 30 days'= 'Within_30d',
                    '13 - More than 12 months ago LOGICALLY ASSIGNED'= 'More_than_30d',
                    '2 - More than 30 days ago but within the past 12 mos'= 'More_than_30d',
                    '3 - More than 12 months ago'= 'More_than_30d',
                    '81 - NEVER USED COC/HER/STM W/NEEDLE Log assn'= 'Never',
                    '9 - At some point in the lifetime LOG ASSN'= 'More_than_30d',
                    '91 - NEVER USED COC/HER/STM WITH A NEEDLE'= "Never",
                    '97 - REFUSED'= NA,
                    '98 - MISSING'= NA,
                    'Overall'= NA)

    ages = age.mapping[!is.na(age.mapping)]
    uses = c('Within_30d', 'More_than_30d', 'Never')
    dim.names = list(age=ages, use=uses)
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    for (year in years)
    {
        file = file.path(dir, paste0('NSDUH_', year), 'IDU_single_age.csv')
        df = read.csv(file, stringsAsFactors = F)

        for (i in 1:dim(df)[1])
        {
            age = age.mapping[df$RECODE...FINAL.EDITED.AGE[i]]
            use = use.mapping[df$RC.MOST.RECENT.USE.OF.ANY.DRUG.WITH.A.NEEDLE[i]]

            if (!is.na(age) && !is.na(use))
                rv[age, use] =  rv[age, use] + df$Weighted.Count[i]
        }
    }

    rv
}

get.idu.13.24.frac.24 <- function(dir='../../v1_data/IDU', years=2015:2018)
{
    arr = get.idu.by.age.counts(dir=dir, years=years)

    ages = as.character(13:24)
    dim.names = list(age=ages, use=c('active','prior','never'))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    rv[1:9,] = arr[2:10,]
    rv['22',] = arr[11,] / 2
    rv['23',] = arr[11,] / 2
    rv['24',] = arr[12,] / 2

    rv = rv/rowSums(rv)
    rv['24',] / colSums(rv, na.rm=T)
}

get.idu.availability.13.24 <- function(dir='../../v1_data/IDU', years=2015:2018)
{
    arr = get.idu.by.age.counts(dir=dir, years=years)

    arr = get.idu.by.age.counts(dir=dir, years=years)

    ages = as.character(13:24)
    dim.names = list(age=ages, use=c('active','prior','never'))
    rv = array(0, dim=sapply(dim.names, length), dimnames=dim.names)

    rv[1:9,] = arr[2:10,]
    rv['22',] = arr[11,] / 2
    rv['23',] = arr[11,] / 2
    rv['24',] = arr[12,] / 2

    rv = rv/rowSums(rv)

    c('13-14'=sum(rv[as.character(13:14),1]) / sum(rv[as.character(19:24),1]),
      '15-18'=sum(rv[as.character(15:18),1]) / sum(rv[as.character(19:24),1]),
      '19-24'=1)
}

##------------------------------------------##
##-- MAPPING SUBSTATE REGIONS TO COUNTIES --##
##------------------------------------------##

#Set up the mapping
#if (!exists('SUBSTATE.TO.COUNTY.MAPPING'))
if (1==2) #DEPRECATED - now in locale mappings
{
print('Reading in Substate-Region-to-County Mapping')
keep.cols = c('sbst16n','sbst16','sbsta16n', 'sbstag16', 'county','state')
mapping.44 = read.sas7bdat('../../v1_data/IDU/substate_county141516.sas7bdat')
mapping.44 = mapping.44[,keep.cols]
mapping.7 = read.sas7bdat('../../v1_data/IDU/substate_tract141516.sas7bdat')
mapping.7 = mapping.7[,keep.cols]
mapping.7 = unique(mapping.7)

SUBSTATE.TO.COUNTY.MAPPING = rbind(mapping.44, mapping.7)
SUBSTATE.TO.COUNTY.MAPPING$combined.fips = combined.fips(SUBSTATE.TO.COUNTY.MAPPING$state, SUBSTATE.TO.COUNTY.MAPPING$county)
SUBSTATE.TO.COUNTY.MAPPING$state.abbreviation = state.fips.to.abbreviation(SUBSTATE.TO.COUNTY.MAPPING$state)

SUBSTATE.TO.COUNTY.MAPPING$region.code = SUBSTATE.TO.COUNTY.MAPPING$sbst16
SUBSTATE.TO.COUNTY.MAPPING$region.name = SUBSTATE.TO.COUNTY.MAPPING$sbst16n

SUBSTATE.TO.COUNTY.MAPPING$substate.code = combined.fips(SUBSTATE.TO.COUNTY.MAPPING$state, SUBSTATE.TO.COUNTY.MAPPING$region.code)
}

counties.for.substate.region <- function(substate.codes)
{
    return (counties.for.nsduh.substate.regions(region.codes = substate.codes))
    
    # DEPRECATED
    substate.codes = as.character(substate.codes)
    unique(unlist(lapply(substate.codes, function(code){
        mask = code == SUBSTATE.TO.COUNTY.MAPPING$substate.code
        if (any(mask))
            SUBSTATE.TO.COUNTY.MAPPING$combined.fips[mask]
        else
            character()
    })))
}

county.to.substate.codes <- function(combined.fips)
{
    return (ndsuh.substate.regions.for.counties(county.fips = combined.fips))
    
    # DEPRECATED
    combined.fips = format.combined.fips(combined.fips)
    unique(unlist(lapply(combined.fips, function(fips){
        mask = fips == SUBSTATE.TO.COUNTY.MAPPING$combined.fips
        if (any(mask))
            SUBSTATE.TO.COUNTY.MAPPING$substate.code[mask]
        else
            character()
    })))
}

#region can be either the name or the numeric code
substate.region.code <- function(region, state)
{
    state = convert.to.state.abbreviation(state)
    return (state.and.region.name.to.nsduh.region.code(states=state, region.names=region))
    
    # DEPRECATED
    state = convert.to.state.abbreviation(state)

    match.region.name = class(region)=='character'

    sapply(1:length(region), function(i){
        if (match.region.name)
            mask = region[i] == SUBSTATE.TO.COUNTY.MAPPING$region.name & state[i] == SUBSTATE.TO.COUNTY.MAPPING$state.abbreviation
        else
            mask = region[i] == SUBSTATE.TO.COUNTY.MAPPING$region.code & state[i] == SUBSTATE.TO.COUNTY.MAPPING$state.abbreviation

        if (!is.na(state[i]) & any(mask))
            SUBSTATE.TO.COUNTY.MAPPING$substate.code[mask][1]
        else
            NA
    })
}
