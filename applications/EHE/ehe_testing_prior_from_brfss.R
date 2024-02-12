# make this a function that takes location and version; get specification metadata object from that
# save this in applications/ehe; this will not be sourced though 
source('../jheem_analyses/applications/EHE/ehe_specification.R')

testing.prior = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580')
cache.object.for.version(object = testing.prior, 
                         name = "testing.prior", 
                         version = 'ehe', overwrite=T)

get.testing.intercepts.and.slopes = function(version, location){
  load("../jheem_analyses/cached/brfss.subset.RData")
  
  #-- RESTRATIFY THE DATA --#
  
  # Just keep when we know MSM and high-risk
  df = appended[!is.na(appended$msm),]
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
  
  df = df[(df$year<2020 | df$year>2022),]
  # df$is.after.covid = df$year>=2020 & df$year<=2022 # USE THIS LATER FOR COVID EFFECT
  
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
  
  testing.anchor.year = 2010
  df$year = df$year-testing.anchor.year
  
  # try one interaction term - probably race with something; try with everything interacted with everything; then two-ways
  # check for extreme single cell values (e.g., 97% black 13-24 msm in 2035)
  # mean(df$tested.past.year[df$race=="black" & df$msm==1 & df$age=="13-24 years" & df$year=="2019"])
  # delta = values[,,,"2030"] - values[,,,"2020"]
  # apply(delta,c("age","race"),max)
  fit.p.testing = glm(tested.past.year ~ age + sex + race + 
                        year + year:age + year:sex + year:race, data=df,
                                       family='binomial', weights = df$weighting.var)
  
  # these are the ORs to get effect of covid on testing: is.after.covid + is.after.covid:age + ...
  
  dim.names = specification.metadata$dim.names[c('age','race','sex')]

  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data = cbind(iterated.values, year=0) 
  year1.data = cbind(iterated.values, year=1)
  
  # this will work, even with interaction terms as long as you use a standard linear year term (not squared, splined, etc.), 
  intercepts = predict(fit.p.testing, year0.data, type = 'link') 
  slopes = predict(fit.p.testing, year1.data, type='link') - intercept
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  rv = list(intercepts=intercepts,
            slopes=slopes)
  rv
}


# old code - these are the same (off by tiny fractions); but would have to redo the formula each time I added interactions
if(1==2){
  intercepts = sapply(dim.names$sex, function(sex){
    sapply(dim.names$race, function(race){
      sapply(dim.names$age, function(age){
        
        sum(c(fit.p.testing$coefficients[1],
              fit.p.testing$coefficients[paste0("age",age)],
              fit.p.testing$coefficients[paste0("sex",sex)], 
              fit.p.testing$coefficients[paste0("race",race)]), na.rm = T) 
      })
    })
  })
  
  slopes = sapply(dim.names$sex, function(sex){
    sapply(dim.names$race, function(race){
      sapply(dim.names$age, function(age){
        
        sum(c(fit.p.testing$coefficients["year"],
              fit.p.testing$coefficients[paste0("age",age,":year")], 
              fit.p.testing$coefficients[paste0("sex",sex,":year")], 
              fit.p.testing$coefficients[paste0("race",race,":year")]), na.rm = T)
      })
    })
  })
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
}



