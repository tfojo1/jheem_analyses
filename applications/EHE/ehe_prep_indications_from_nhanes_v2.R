source('../jheem_analyses/applications/EHE/ehe_specification.R')

get.prep.indication.intercepts.and.slopes = function(version,
                                                     location,
                                                     dataset # msm.cdc, msm.jhu - NOT USING, or heterosexual
                                                     ){
  if(dataset=="msm.cdc"){
    load("../jheem_analyses/cached/datasets_from_Zoe/nhanes.msm.cdc.RData") 
    df = nhanes.msm.cdc
    # total msm = 216 (total # of rows, also outcome $msm.cdc==1)
    # prep indication = 85 (outcome $mmwr.prep.indications==1)
    
  } else if (dataset=="msm.jhu"){
    load("../jheem_analyses/cached/datasets_from_Zoe/nhanes.msm.jhu.RData")  
    df = nhanes.msm.jhu
    # total msm = 1987 (total # of rows, also outcome $msm.adjusted==1)
    # prep indication = 180 (outcome $mmwr.prep.indications==1)
    
  } else if (dataset=="heterosexual"){
    load("../jheem_analyses/cached/datasets_from_Zoe/heterosexual.with.sti.RData")  
    df = heterosexual.with.sti
    # total het = 24407 (total # of rows)
    # prep indication = 1269 (outcome $hx.any.sti==1)
    
  } else stop("invalid dataset")
  
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
  df$orig.year = df$year
  df$year = df$year-prep.anchor.year
  
  # Make outcome numeric
  df$hx.any.sti = as.numeric(df$hx.any.sti)

  if(dataset=="heterosexual"){
    fit = glm(hx.any.sti ~ age + race + year, data=df, #weights = df$WTMEC2YR,
              family='binomial')  
  } else {
    fit = glm(mmwr.prep.indications ~ age + race + year, data=df, #weights = df$WTMEC2YR,
              family='binomial')  
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
  prep.indications.msm.cdc = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                       location = 'C.12580',
                                                                       dataset="msm.cdc")
  
  prep.indications.msm.jhu = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                       location = 'C.12580',
                                                                       dataset="msm.jhu")
  
  prep.indications.het = get.prep.indication.intercepts.and.slopes(version = 'ehe', 
                                                                   location = 'C.12580',
                                                                   dataset="heterosexual")
  
  msm.cdc.indications.ff = create.logistic.linear.functional.form(intercept = prep.indications.msm.cdc$intercepts,
                                                                  slope = prep.indications.msm.cdc$slopes,
                                                                  anchor.year = 2006,
                                                                  parameters.are.on.logit.scale = T)  
  
  msm.jhu.indications.ff = create.logistic.linear.functional.form(intercept = prep.indications.msm.jhu$intercepts,
                                                                  slope = prep.indications.msm.jhu$slopes,
                                                                  anchor.year = 2006,
                                                                  parameters.are.on.logit.scale = T)  
  
  het.indications.ff = create.logistic.linear.functional.form(intercept = prep.indications.het$intercepts,
                                                                  slope = prep.indications.het$slopes,
                                                                  anchor.year = 2006,
                                                                  parameters.are.on.logit.scale = T)  

  
  values.msm.cdc = msm.cdc.indications.ff$project(2015:2035) 
  values.msm.cdc = array(unlist(values.msm.cdc), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))
  
  # DON'T TRUST THESE VALUES - USE CDC ONE INSTEAD
  values.msm.jhu = msm.jhu.indications.ff$project(2015:2035) 
  values.msm.jhu = array(unlist(values.msm.jhu), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))
  
  values.het = het.indications.ff$project(2015:2035) 
  values.het = array(unlist(values.het), 
                     dim = c(sapply(dim.names, length),length(2015:2035)),
                     dimnames = c(dim.names, list(year=2015:2035)))
  
  plotting.df = reshape2::melt((values.het))
  
  ggplot(plotting.df,aes(x=year,y=value,color=age,group=age)) + geom_line() + facet_wrap(~race,scales = "free_y")
  
  

  
  
  # add datapoints from actual nhanes data
  nhanes.msm.cdc.means = sapply(c(1,3,5,7,9), function(year){ # 2007-2016 (year anchored at 2006)
      sapply(dim.names$race, function(race){
        sapply(dim.names$age, function(age){
          mean(prep.indications.msm.cdc$df$mmwr.prep.indications[
            prep.indications.msm.cdc$df$year==year & prep.indications.msm.cdc$df$race==race & prep.indications.msm.cdc$df$age==age],na.rm=T)
        })
      })
    })

  nhanes.het.means = sapply(c(1,3,5,7,9), function(year){ # 2007-2016 (year anchored at 2006)
    sapply(dim.names$race, function(race){
      sapply(dim.names$age, function(age){
        mean(prep.indications.het$df$hx.any.sti[
          prep.indications.het$df$year==year & prep.indications.het$df$race==race & prep.indications.het$df$age==age],na.rm=T)
      })
    })
  })
  
  dim(nhanes.msm.cdc.means) = dim(nhanes.het.means) = 
    c(sapply(dim.names, length), length(c(1,3,5,7,9)))
  dimnames(nhanes.msm.cdc.means) =dimnames(nhanes.het.means) =
    c(dim.names,list(year=c(2007,2009,2011,2013,2015)))
  
  # plot.age = 
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.msm.cdc, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_line(data=reshape2::melt(apply(nhanes.msm.cdc.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(nhanes.msm.cdc.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,1) 
    ggtitle("MSM (CDC definition), age") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.het, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_line(data=reshape2::melt(apply(nhanes.het.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(nhanes.het.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,0.1) + 
    ggtitle("Heterosexual, age") 
  
  # plot.race = 
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.msm.cdc, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_line(data=reshape2::melt(apply(nhanes.msm.cdc.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(nhanes.msm.cdc.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,1) + 
    ggtitle("MSM (CDC definition), race") 
  
  ggplot() + 
    geom_line(data=reshape2::melt(apply(values.het, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_line(data=reshape2::melt(apply(nhanes.het.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(nhanes.het.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,.25) + 
    ggtitle("Heterosexual, race") 

}


