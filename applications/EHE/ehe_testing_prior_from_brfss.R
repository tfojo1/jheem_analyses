# make this a function that takes location and version; get specification metadata object from that
# save this in applications/ehe; this will not be sourced though 
source('../jheem_analyses/applications/EHE/ehe_specification.R')

testing.prior = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580')

cache.object.for.version(object = testing.prior, 
                         name = "testing.prior", 
                         version = 'ehe', overwrite=T)

get.testing.intercepts.and.slopes = function(version, location,
                                             model="two.way" # or fully.interacted or one.way
                                             ){
  load("../jheem_analyses/cached/brfss.subset.RData")
  
  #-- RESTRATIFY THE DATA --#
  
  # Just keep when we know MSM; remove high-risk
  df = appended[!is.na(appended$msm),]
  # df = df[!is.na(df$high.risk) & df$high.risk==0,]
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
  
  df = df[(df$year<2020 | df$year>2022),]
  # df$is.after.covid = df$year>=2020 & df$year<=2022 # USE THIS LATER FOR COVID EFFECT
  # these are the ORs to get effect of covid on testing: is.after.covid + is.after.covid:age + ...
  
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
  
  if(model=="two.way"){
    fit = glm(tested.past.year ~ age*sex + age*race + race*sex + 
                          year + year:age + year:sex + year:race, data=df,
                        family='binomial', weights = df$weighting.var)    
  } else if(model=="fully.interacted"){
    fit = glm(tested.past.year ~ age*sex*race +
                          year + year:age + year:sex + year:race, data=df,
                        family='binomial', weights = df$weighting.var)
  } else if(model=="one.way"){
    fit = glm(tested.past.year ~ age + sex + race +
                          year + year:age + year:sex + year:race, data=df,
                        family='binomial', weights = df$weighting.var)
  } else
    stop("model can only be two.way, fully.interacted, or one.way")

  
  dim.names = specification.metadata$dim.names[c('age','race','sex')]

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
            slopes=slopes)
  rv
}




# review projections to make sure they look okay in the future 
if(1==2){
  testing.prior.two.way = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580',
                                                            model="two.way")
  
  testing.prior.one.way = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580',
                                                            model="one.way")
  
  testing.prior.fully.interacted = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580',
                                                            model="fully.interacted")
  
  testing.functional.form.two.way = create.logistic.linear.functional.form(intercept = testing.prior.two.way$intercepts,
                                                                   slope = testing.prior.two.way$slopes,
                                                                   anchor.year = 2010,
                                                                   parameters.are.on.logit.scale = T)  
  
  testing.functional.form.one.way = create.logistic.linear.functional.form(intercept = testing.prior.one.way$intercepts,
                                                                   slope = testing.prior.one.way$slopes,
                                                                   anchor.year = 2010,
                                                                   parameters.are.on.logit.scale = T)  
  
  testing.functional.form.fully.interacted = create.logistic.linear.functional.form(intercept = testing.prior.fully.interacted$intercepts,
                                                                   slope = testing.prior.fully.interacted$slopes,
                                                                   anchor.year = 2010,
                                                                   parameters.are.on.logit.scale = T)  
  
  testing.functional.form = create.logistic.linear.functional.form(intercept = testing.prior$intercepts - log(0.9),
                                                                   slope = testing.prior$slopes,
                                                                   anchor.year = 2010,
                                                                   max = 0.9,
                                                                   parameters.are.on.logit.scale = T)  
  
  values = testing.functional.form$project(2015:2035) 
  values = array(unlist(values), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))
  
  values.two.way = testing.functional.form.two.way$project(2015:2035) 
  values.two.way = array(unlist(values.two.way), 
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  
  values.one.way = testing.functional.form.one.way$project(2015:2035) 
  values.one.way = array(unlist(values.one.way), 
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  
  values.fully.interacted = testing.functional.form.fully.interacted$project(2015:2035) 
  values.fully.interacted = array(unlist(values.fully.interacted), 
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  
  # check for extreme single cell values (e.g., 97% black 13-24 msm in 2035)
  values["13-24 years","black","msm","2035"]
  
  head(sort(values.two.way,decreasing = T),50) # highest 50 values
  head(sort(values.one.way,decreasing = T),50) # highest 50 values
  head(sort(values.fully.interacted,decreasing = T),50) # highest 50 values

  # add datapoints from actual brfss data
  brfss.means = sapply(4:12, function(year){ # 2014-2022 (year anchored at 2010)
    sapply(dim.names$sex, function(sex){
      sapply(dim.names$race, function(race){
        sapply(dim.names$age, function(age){
          mean(df$tested.past.year[df$year==year & df$race==race & df$sex==sex & df$age==age])
        })
      })
    })
  })
  
  dim(brfss.means) = c(sapply(dim.names, length), length(2014:2022))
  dimnames(brfss.means) = c(dim.names,list(year=2014:2022))
  
  #values = values.two.way # values.two.way - no real differences in the plots for two-way vs full; both better than one-way
  
  # plot.age = 
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(brfss.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ylim(0,1) + 
    #ggtitle("Testing Projection vs. BRFSS data, Age") +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  # plot.race = 
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(brfss.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ylim(0,1) + 
    #ggtitle("Testing Projection vs. BRFSS data, Race") +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  # plot.sex = 
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
    geom_point(data=reshape2::melt(apply(brfss.means, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
    ylim(0,1) + 
    #ggtitle("Testing Projection vs. BRFSS data, Sex") +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  
  ggsave(filename = "prelim_results/testing_comparison_age.jpeg",
         plot.age,width = 10,height = 7,dpi = 350)
  ggsave(filename = "prelim_results/testing_comparison_race.jpeg",
         plot.race,width = 10,height = 7,dpi = 350)
  ggsave(filename = "prelim_results/testing_comparison_sex.jpeg",
         plot.sex,width = 10,height = 7,dpi = 350)
  
  # mean(df$tested.past.year[df$race=="black" & df$msm==1 & df$age=="13-24 years" & df$year=="2019"])
  delta = values[,,,"2030"] - values[,,,"2020"]
  range(delta)
  rel.delta = delta/values[,,,"2020"]
  range(rel.delta)
  apply(delta,c("sex"),range)
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



