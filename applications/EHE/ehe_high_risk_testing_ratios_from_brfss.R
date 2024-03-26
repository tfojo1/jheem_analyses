source('../jheem_analyses/applications/EHE/ehe_specification.R')

# NON-COVID YEARS
high.risk.testing.rate.ratios = get.high.risk.testing.ratios(version = 'ehe', location = 'C.12580')

cache.object.for.version(object = high.risk.testing.rate.ratios, 
                         name = "high.risk.testing.rate.ratios", 
                         version = 'ehe', overwrite=T)

# COVID YEARS
high.risk.testing.rate.ratios.covid = get.high.risk.testing.ratios.during.covid(version = 'ehe', location = 'C.12580')
high.risk.testing.covid.ratio.of.ratios = high.risk.testing.rate.ratios.covid/high.risk.testing.rate.ratios
# ^ this multiplier * non-covid values = covid values

cache.object.for.version(object = high.risk.testing.covid.ratio.of.ratios, 
                         name = "high.risk.testing.covid.ratio.of.ratios", 
                         version = 'ehe', overwrite=T)


get.high.risk.testing.ratios = function(version, location){
  
  load("../jheem_analyses/cached/brfss.subset.RData")
  
  #-- RESTRATIFY THE DATA --#
  
  # Just keep when we know MSM and high-risk
  df = appended[!is.na(appended$msm),]
  
  specification.metadata = get.specification.metadata(version=version, location=location)
  
  df = df[(df$year<2020 | df$year>2022),]
  
  # Restratify the ages
  given.ages = unique(df$age)
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age)
  # need to make sure this is not NULL
  
  df$orig.age = df$age
  df$age = age.map[df$orig.age]
  df = df[!is.na(df$age),]
  
  # Set reference levels
  reference.age.index = ceiling(specification.metadata$n.ages/2)
  age.levels = c(specification.metadata$dim.names$age[reference.age.index],
                 specification.metadata$dim.names$age[-reference.age.index])
  df$age = factor(df$age, levels=age.levels)
  
  # Map race
  df$orig.race = df$race
  race.ontology.mapping = get.ontology.mapping(from.ontology = list(race=tolower(unique(df$race))),
                                               to.ontology = specification.metadata$dim.names['race'])
  # Need to make sure this is not NULL
  race.map = race.ontology.mapping$get.mapping.vector()
  df$race = race.map[tolower(df$orig.race)]
  race.levels = union('other', specification.metadata$dim.names$race)
  df$race = factor(df$race, levels = race.levels)
  
  # Map sex
  df$orig.sex = df$sex
  df$sex[df$msm==1] = 'msm'
  df$sex[df$sex=='male'] = 'heterosexual_male'
  df$sex = factor(df$sex, levels = c('msm', 'heterosexual_male','female'))
  
  # Map high.risk to logical
  df$orig.high.risk = df$high.risk
  df$high.risk = df$high.risk==1
  
  ##-- GET TESTING RATE MULTIPLIERS --##
  
  fit.high.risk = glm(tested.past.year ~ age*sex + age*race + race*sex +
                        high.risk + age:high.risk + sex:high.risk + race:high.risk,
                      family='binomial', data=df, weights = df$weighting.var)
  
  # fit.high.risk = glm(tested.past.year ~ age*sex*high.risk + age*race*high.risk + race*sex*high.risk,
  #                     family='binomial', data=df, weights = df$weighting.var)
  
  fit.overall = glm(tested.past.year ~ age*sex + age*race + race*sex,
                    family='binomial', data=df, weights = df$weighting.var)
  
  # make this into an array
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  overall.p = predict(fit.overall, iterated.values, type = 'response')
  
  high.risk.values = cbind(iterated.values, high.risk=T)
  high.risk.p = predict(fit.high.risk, high.risk.values, type = 'response')
  
  high.risk.testing.rate.ratios = -log(1-high.risk.p) / -log(1-overall.p)
  dim(high.risk.testing.rate.ratios) = sapply(dim.names, length)
  dimnames(high.risk.testing.rate.ratios) = dim.names
  
  high.risk.testing.rate.ratios
  
}

get.high.risk.testing.ratios.during.covid = function(version, location){
  
  load("../jheem_analyses/cached/brfss.subset.RData")
  
  #-- RESTRATIFY THE DATA --#
  
  # Just keep when we know MSM and high-risk
  df = appended[!is.na(appended$msm),]
  
  specification.metadata = get.specification.metadata(version=version, location=location)
  
  # ONLY 2020 THIS TIME 
  df = df[(df$year==2020),]
  
  # Restratify the ages
  given.ages = unique(df$age)
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age)
  # need to make sure this is not NULL
  
  df$orig.age = df$age
  df$age = age.map[df$orig.age]
  df = df[!is.na(df$age),]
  
  # Set reference levels
  reference.age.index = ceiling(specification.metadata$n.ages/2)
  age.levels = c(specification.metadata$dim.names$age[reference.age.index],
                 specification.metadata$dim.names$age[-reference.age.index])
  df$age = factor(df$age, levels=age.levels)
  
  # Map race
  df$orig.race = df$race
  race.ontology.mapping = get.ontology.mapping(from.ontology = list(race=tolower(unique(df$race))),
                                               to.ontology = specification.metadata$dim.names['race'])
  # Need to make sure this is not NULL
  race.map = race.ontology.mapping$get.mapping.vector()
  df$race = race.map[tolower(df$orig.race)]
  race.levels = union('other', specification.metadata$dim.names$race)
  df$race = factor(df$race, levels = race.levels)
  
  # Map sex
  df$orig.sex = df$sex
  df$sex[df$msm==1] = 'msm'
  df$sex[df$sex=='male'] = 'heterosexual_male'
  df$sex = factor(df$sex, levels = c('msm', 'heterosexual_male','female'))
  
  # Map high.risk to logical
  df$orig.high.risk = df$high.risk
  df$high.risk = df$high.risk==1
  
  ##-- GET TESTING RATE MULTIPLIERS --##
  
  fit.high.risk = glm(tested.past.year ~ age*sex + age*race + race*sex +
                        high.risk + age:high.risk + sex:high.risk + race:high.risk,
                      family='binomial', data=df, weights = df$weighting.var)
  
  # fit.high.risk = glm(tested.past.year ~ age*sex*high.risk + age*race*high.risk + race*sex*high.risk,
  #                     family='binomial', data=df, weights = df$weighting.var)
  
  fit.overall = glm(tested.past.year ~ age*sex + age*race + race*sex,
                    family='binomial', data=df, weights = df$weighting.var)
  
  # make this into an array
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  overall.p = predict(fit.overall, iterated.values, type = 'response')
  
  high.risk.values = cbind(iterated.values, high.risk=T)
  high.risk.p = predict(fit.high.risk, high.risk.values, type = 'response')
  
  high.risk.testing.rate.ratios = -log(1-high.risk.p) / -log(1-overall.p)
  dim(high.risk.testing.rate.ratios) = sapply(dim.names, length)
  dimnames(high.risk.testing.rate.ratios) = dim.names
  
  high.risk.testing.rate.ratios
  
}

