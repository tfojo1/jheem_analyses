library(ggplot2)
# make this a function that takes location and version; get specification metadata object from that
# save this in applications/ehe; this will not be sourced though 
source('../jheem_analyses/applications/EHE/ehe_specification.R') #this is sourcing the EHE application to set up the environment. should be changed to SHIELD's


# Main functions # -----
# Read and clean the BRFSS data according to the model requirements 
clean.brfss.data<-function(version, location){
  
  load("../jheem_analyses/cached/brfss.subset.RData") #@Navid: you will need to update your cashed folder to include all the files that are on OneDrive
  df=appended
  
  # inspect the data columns 
  #'@Navid: please check the definitions of these columns with Melissa to understand what they represent 
  # this is the step where you need to inspct your data and decide about a cleanup strategy before fititng a model 
  names(df)
  apply(df[1:6],2,table,useNA='always')
  
  #-- RESTRATIFY THE DATA --#
  # 1- Just keep when we know MSM; remove high-risk
  df = df[!is.na(df$msm),]
  # apply(df[1:6],2,table,useNA='always')
  #'@Navid: I can see that after removing MSM, we still have a number of sex=NA's. we need to make sure that this deosnt create a problem in the models that use sex as a predictor
  #'@Navid: do you think we should filter sex=NA too? 
  
  
  # read the specification metadata from the model (EHE or SHIELD)
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
  
  #2- remove years 2020-2022 to minimize impact of COVID-19
  df = df[(df$year<2020 | df$year>2022),]
  # df$is.after.covid = df$year>=2020 & df$year<=2022 # USE THIS LATER FOR COVID EFFECT
  # these are the ORs to get effect of covid on testing: is.after.covid + is.after.covid:age + ...
  
  #3- restratify the ages
  given.ages = unique(df$age)
  # need to make sure this is not NULL
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age) #'@Navid: this is a function that maps the ages to the age brackets defined in the model.
  #'@Navid: please find this function and check how it works 
  
  #
  df$orig.age = df$age
  df$age = age.map[df$orig.age]
  # remove age= NA's
  df = df[!is.na(df$age),]
  
  #'@Navid: I am not sure what this section is doing beside reordering the age brackets
  # Set reference levels
  reference.age.index = ceiling(specification.metadata$n.ages/2)
  age.levels = c(specification.metadata$dim.names$age[reference.age.index],
                 specification.metadata$dim.names$age[-reference.age.index])
  df$age = factor(df$age, levels=age.levels)
  # table(df$age)
  
  #3- restratify the race
  # remove race= NA's
  df = df[!is.na(df$race),]
  df$orig.race = df$race
  race.ontology.mapping = get.ontology.mapping(from.ontology = list(race=tolower(unique(df$race))),
                                               to.ontology = specification.metadata$dim.names['race'])
  # Need to make sure this is not NULL
  race.map = race.ontology.mapping$get.mapping.vector()
  df$race = race.map[tolower(df$orig.race)]
  race.levels = union('other', specification.metadata$dim.names$race)
  df$race = factor(df$race, levels = race.levels)
  # table(df$race)
  
  # 3- restratify the sex
  df = df[!is.na(df$sex),] #'@Navid: I added this line, please check if it is correct. 
  df$orig.sex = df$sex
  df$sex[df$msm==1] = 'msm'
  df$sex[df$sex=='male'] = 'heterosexual_male'
  df$sex = factor(df$sex, levels = c('msm', 'heterosexual_male','female'))
  
  #4- setting up the year 
  #'@Navid: why do we need an anchor year, and what's the impact of different values on future projections? 
  testing.anchor.year = 2010
  df$year = df$year-testing.anchor.year
  
  return(df)
}


# Function to fit a given model on data -----
# A framework to fit various models to data, compare them, and choose the best model to use in the analysis 
get.testing.intercepts.and.slopes = function(version, 
                                             location,
                                             model="two.way" # or fully.interacted or one.way
){
  #read data
  df<-clean.brfss.data(version, location)
  # dim(df)
  # read the specification metadata from the model (EHE or SHIELD)
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
  # Alternativemodels to fit ----
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
  
  # to extract the intercept and slops with respect to one unit change in time, 
  # we set up a data frame with all possible combinations of the strata:
  
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  # full interactions between all dimensions
  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data = cbind(iterated.values, year=0) 
  year1.data = cbind(iterated.values, year=1)
  
  # this will work, even with interaction terms as long as you use a standard linear year term (not squared, splined, etc.), 
  # For a binomial GLM (which is often used for binary outcomes like yes/no or success/failure), the default link function is the logit link, which is the log-odds transformation.
  #(If you want probabilities (i.e., predicted outcomes on the probability scale), you would use type = 'response')
  #This would return the predicted probabilities of the outcome being 1 (for example, "tested" or "success") for each combination of age, sex, and race when year = 0.
  intercepts = predict(fit, year0.data, type = 'link') 
  slopes = predict(fit, year1.data, type='link') - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  #'@Navid: how do you think we can interpret these values ? what does an intercept value of -1.27 mean for this model?
  #'Log-odds of -1.27 means the odds of testing in the past year are about 0.280 to 1, or the event is about 28% as likely to happen as it is to not happen.
  # The probability of testing in the past year for this baseline group (before adjusting for other predictors) is about 21.9%.
  
  rv = list(intercepts=intercepts,
            slopes=slopes)
  rv
}

# Function to compare fit accross alternative models ----
# review projections to make sure they look okay in the future 
if(1==1){
  version = 'ehe'
  location = 'C.12580'
  # read and clearn BRFSS data
  df<-clean.brfss.data(version, location)
  # read the specification metadata from the model (EHE or SHIELD)
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
    # fit the 3 models:
  testing.prior.two.way = get.testing.intercepts.and.slopes(version  , location  , model="two.way")
  testing.prior.one.way = get.testing.intercepts.and.slopes(version  , location  , model="one.way")
  testing.prior.fully.interacted = get.testing.intercepts.and.slopes(version  , location  , model="fully.interacted")
  
  # now use the intercepts/slopes from each model to build a logistic regression model in the JHEEM that can predict the future values for the model 
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
  
  # predict future values from each model
  values.one.way = testing.functional.form.one.way$project(2015:2035) 
  values.one.way = array(unlist(values.one.way), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))
  #
  values.two.way = testing.functional.form.two.way$project(2015:2035) 
  values.two.way = array(unlist(values.two.way), 
                         dim = c(sapply(dim.names, length),length(2015:2035)),
                         dimnames = c(dim.names, list(year=2015:2035)))

  #
  values.fully.interacted = testing.functional.form.fully.interacted$project(2015:2035) 
  values.fully.interacted = array(unlist(values.fully.interacted), 
                                  dim = c(sapply(dim.names, length),length(2015:2035)),
                                  dimnames = c(dim.names, list(year=2015:2035)))
  
  
  # checking for exterme values among projections 
  # head(sort(values.two.way,decreasing = T),50) # highest 50 values
  # head(sort(values.one.way,decreasing = T),50) # highest 50 values
  # head(sort(values.fully.interacted,decreasing = T),50) # highest 50 values
  
  # add datapoints from actual brfss data
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
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
  
  # loop over data files and plot them with regard to each strata
  # we will use these plots to assess the fit 
  VALUES=list(values.one.way,values.two.way,values.fully.interacted)
  lapply(c(1:3),function(x){
    values = VALUES[[x]] # values.two.way - no real differences in the plots for two-way vs full; both better than one-way
    plot.age =
      ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Age") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
    
    plot.race =
      ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Race") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
    
    plot.sex =
      ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Sex") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
    
    
    ggsave(filename = paste("prelim_results/testing_comparison_age_",x,".jpeg"),
           plot.age,width = 10,height = 7,dpi = 350)
    ggsave(filename = paste("prelim_results/testing_comparison_race_",x,".jpeg"),
           plot.race,width = 10,height = 7,dpi = 350)
    ggsave(filename = paste("prelim_results/testing_comparison_sex_",x,".jpeg"),
           plot.sex,width = 10,height = 7,dpi = 350)
    
  })
  
}

# old code - these are the same (off by tiny fractions); but would have to redo the formula each time I added interactions
if(1==2){
  #'@Navid: you should read more about the create.logistic.linear.functional.form to understand all inputs and their roles 
  #'@Navid: why are we using a max of 0.9 here?
  testing.prior = get.testing.intercepts.and.slopes(version = 'ehe', location = 'C.12580',
                                                    model="two.way")
  
  testing.functional.form = create.logistic.linear.functional.form(intercept = testing.prior$intercepts - log(0.9),
                                                                   slope = testing.prior$slopes,
                                                                   anchor.year = 2010,
                                                                   max = 0.9,
                                                                   parameters.are.on.logit.scale = T)
  
  values = testing.functional.form$project(2015:2035)
  values = array(unlist(values),
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  { #Visual inspection
    
    # plot.age =
    ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Age") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
    
    # plot.race =
    ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Race") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
    
    # plot.sex =
    ggplot() + 
      geom_line(data=reshape2::melt(apply(values, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
      geom_point(data=reshape2::melt(apply(brfss.means, c("sex","year"),mean)), aes(x=year, y=value, color=sex)) + 
      ylim(0,1) + 
      ggtitle("Testing Projection vs. BRFSS data, Sex") +
      theme(plot.title = element_text(hjust = 0.5,size = 25))
  }
  
  # check for extreme single cell values (e.g., 97% black 13-24 msm in 2035)
  #mean(df$tested.past.year[df$race=="black" & df$msm==1 & df$age=="13-24 years" & df$year=="9"])
  # values["13-24 years","black","msm","2035"]
  
  #inspect the changes over time to 2030
  delta = values[,,,"2030"] - values[,,,"2020"]
  range(delta)
  rel.delta = delta/values[,,,"2020"]
  range(rel.delta)
  apply(delta,c("sex"),range)
  
  
  #'@Navid: not sure what these are 
  # intercepts = sapply(dim.names$sex, function(sex){
  #   sapply(dim.names$race, function(race){
  #     sapply(dim.names$age, function(age){
  #       sum(c(fit.p.testing$coefficients[1],
  #             fit.p.testing$coefficients[paste0("age",age)],
  #             fit.p.testing$coefficients[paste0("sex",sex)], 
  #             fit.p.testing$coefficients[paste0("race",race)]), na.rm = T) 
  #     })
  #   })
  # })
  # 
  # slopes = sapply(dim.names$sex, function(sex){
  #   sapply(dim.names$race, function(race){
  #     sapply(dim.names$age, function(age){
  #       
  #       sum(c(fit.p.testing$coefficients["year"],
  #             fit.p.testing$coefficients[paste0("age",age,":year")], 
  #             fit.p.testing$coefficients[paste0("sex",sex,":year")], 
  #             fit.p.testing$coefficients[paste0("race",race,":year")]), na.rm = T)
  #     })
  #   })
  # })
  # 
  # dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  # dimnames(intercepts) = dimnames(slopes) = dim.names
}

# Use the function below to estiamte the prior value for parameters (inlcuding intercenpt and slope)
testing.prior = get.testing.intercepts.and.slopes(version = 'ehe', 
                                                  location = 'C.12580')

# Cache this prior for future use in the EHE (or SHIELD)
cache.object.for.version(object = testing.prior, 
                         name = "testing.prior", 
                         version = 'ehe', 
                         overwrite=T)
print("the object testing.prior is cached sucessfully!")

#'@Navid: Once your work on this code is over, can you please use Andrew's documentation style to document this code?