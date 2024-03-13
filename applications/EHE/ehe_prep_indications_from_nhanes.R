# make this a function that takes location and version; get specification metadata object from that
# save this in applications/ehe; this will not be sourced though 
source('../jheem_analyses/applications/EHE/ehe_specification.R')
library(ggplot2)

get.prep.indication.intercepts.and.slopes = function(version,
                                                     location,
                                                     dataset # msm, heterosexual.female, or heterosexual.male
                                                     ){
  if(dataset=="msm"){
    load("../jheem_analyses/cached/datasets_from_Zoe/nhanes.msm.RData") 
    df = nhanes.msm
    df = df[!is.na(df$sex.in.past.year.and.condomless),] # remove NA outcomes, but 77% NAs??? # THIS IS AN ISSUE 
  } else if (dataset=="heterosexual.female"){
    load("../jheem_analyses/cached/datasets_from_Zoe/nhanes.hetero.females.RData")  
    df = nhanes.hetero.females
    df = df[!is.na(df$gonorrhea.past.year),] # remove NA outcomes
  } else if (dataset=="heterosexual.male"){
    load("../jheem_analyses/cached/datasets_from_Zoe/nhanes.hetero.males.RData")  
    df = nhanes.hetero.males
    df = df[!is.na(df$gonorrhea.past.year),] # remove NA outcomes
  } else 
    stop("invalid dataset")
  
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)

  # Restratify the ages
  given.ages = unique(df$age.group)
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age)
  df$orig.age = df$age
  df$orig.age.group = df$age.group
  df$age = age.map[df$orig.age.group]
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
  race.map = race.ontology.mapping$get.mapping.vector()
  df$race = race.map[tolower(df$orig.race)]
  race.levels = union('other', specification.metadata$dim.names$race)
  df$race = factor(df$race, levels = race.levels)

  # Year mapping - better way to do this? 
  df$year = as.numeric(substr(df$survey.year,1,4))
  
  prep.anchor.year = 2006
  df$year = df$year-prep.anchor.year
  
  # Make outcome numeric
  df$gonorrhea.past.year[df$gonorrhea.past.year=="no"] = 0
  df$gonorrhea.past.year[df$gonorrhea.past.year=="yes"] = 1
  df$gonorrhea.past.year = as.numeric(df$gonorrhea.past.year)
  
  df$sex.in.past.year.and.condomless = as.numeric(df$sex.in.past.year.and.condomless)
  
  if(dataset=="msm"){
    fit = glm(sex.in.past.year.and.condomless ~ age + race + year + 
                year:age + year:race, data=df,
              family='binomial')  
  } else {
    fit = glm(gonorrhea.past.year ~ age + race + year + 
                year:age + year:race, data=df,
              family='binomial')  #gaussian for linear
  }
  
  dim.names = specification.metadata$dim.names[c('age','race')]
  
  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data = cbind(iterated.values, year=0) 
  year1.data = cbind(iterated.values, year=1)
  
  # this will work, even with interaction terms as long as you use a standard linear year term (not squared, splined, etc.), 
  intercepts = predict(fit, year0.data, type = 'link') 
  slopes = predict(fit, year1.data, type='link') - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  rv = list(intercepts=intercepts,
            slopes=slopes,
            df=df)
  rv
}

# review projections to make sure they look okay in the future 
if(1==2){
  prep.indications.msm = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                   location = 'C.12580',
                                                                   dataset="msm")
  
  prep.indications.female = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                   location = 'C.12580',
                                                                   dataset="heterosexual.female")
  
  prep.indications.male = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                      location = 'C.12580',
                                                                      dataset="heterosexual.male")
  
  msm.prep.indications.functional.form = create.logistic.linear.functional.form(intercept = prep.indications.msm$intercepts,
                                                                           slope = prep.indications.msm$slopes,
                                                                           anchor.year = 2006,
                                                                           parameters.are.on.logit.scale = T)  
  
  female.prep.indications.functional.form = create.logistic.linear.functional.form(intercept = prep.indications.female$intercepts,
                                                                                slope = prep.indications.female$slopes,
                                                                                anchor.year = 2006,
                                                                                parameters.are.on.logit.scale = T)  
  
  male.prep.indications.functional.form = create.logistic.linear.functional.form(intercept = prep.indications.male$intercepts,
                                                                                   slope = prep.indications.male$slopes,
                                                                                   anchor.year = 2006,
                                                                                   parameters.are.on.logit.scale = T)  
  
  values.msm = msm.prep.indications.functional.form$project(2015:2035) 
  values.msm = array(unlist(values.msm), 
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  
  values.female = female.prep.indications.functional.form$project(2015:2035) 
  values.female = array(unlist(values.female), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))
  
  values.male = male.prep.indications.functional.form$project(2015:2035) 
  values.male = array(unlist(values.male), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))

  # check for extreme single cell values (e.g., 93% black 25-34 msm in 2035, 95% black 35-44 male in 2035??)
  values.msm["25-34 years","black","2035"]
  values.male["35-44 years","black","2035"]
  
  head(sort(values.msm,decreasing = T),50) # highest 50 values
  head(sort(values.female,decreasing = T),50) # highest 50 values
  head(sort(values.male,decreasing = T),50) # highest 50 values
  
  # add datapoints from actual nhanes data
  nhanes.msm.means = sapply(c(1,3,5,7,9), function(year){ # 2007-2016 (year anchored at 2006)
      sapply(dim.names$race, function(race){
        sapply(dim.names$age, function(age){
          mean(prep.indications.msm$df$sex.in.past.year.and.condomless[
            prep.indications.msm$df$year==year & prep.indications.msm$df$race==race & prep.indications.msm$df$age==age],na.rm=T)
        })
      })
    })
  nhanes.female.means = sapply(c(1,3,5,7,9), function(year){ # 2007-2016 (year anchored at 2006)
    sapply(dim.names$race, function(race){
      sapply(dim.names$age, function(age){
        mean(prep.indications.female$df$gonorrhea.past.year[
          prep.indications.female$df$year==year & prep.indications.female$df$race==race & prep.indications.female$df$age==age],na.rm=T)
      })
    })
  })
  nhanes.male.means = sapply(c(1,3,5,7,9), function(year){ # 2007-2016 (year anchored at 2006)
    sapply(dim.names$race, function(race){
      sapply(dim.names$age, function(age){
        mean(prep.indications.male$df$gonorrhea.past.year[
          prep.indications.male$df$year==year & prep.indications.male$df$race==race & prep.indications.male$df$age==age],na.rm=T)
      })
    })
  })
  
  dim(nhanes.msm.means) = dim(nhanes.female.means) =  dim(nhanes.male.means) = 
    c(sapply(dim.names, length), length(c(1,3,5,7,9)))
  dimnames(nhanes.msm.means) =dimnames(nhanes.female.means) =dimnames(nhanes.male.means) = 
    c(dim.names,list(year=c(2007,2009,2011,2013,2015)))
  
  # plot.age = 
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.msm, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_line(data=reshape2::melt(apply(nhanes.msm.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(nhanes.msm.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,1) + 
    ggtitle("MSM, age") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.female, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_line(data=reshape2::melt(apply(nhanes.female.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(nhanes.female.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,0.1) + 
    ggtitle("Female, age") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.male, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_line(data=reshape2::melt(apply(nhanes.male.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(nhanes.male.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,0.4) + 
    ggtitle("Male, age") 
  
  # plot.race = 
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.msm, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_line(data=reshape2::melt(apply(nhanes.msm.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(nhanes.msm.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,1) + 
    ggtitle("MSM, race") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.female, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_line(data=reshape2::melt(apply(nhanes.female.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(nhanes.female.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,.25) + 
    ggtitle("Female, race") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.male, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_line(data=reshape2::melt(apply(nhanes.male.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(nhanes.male.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,.4) + 
    ggtitle("Male, race") 

}


