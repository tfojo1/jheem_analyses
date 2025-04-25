# Data: Proportion of births receiving prenatal care ine ach trimester (p1,p2,p3) and those not receiving any prenatal care(p0)
# Zoe has pulled this data from CDC Wonderby year, age and race of moms
# (p0,p1,p2,p3) form a multinomial distribution in each year and category 
# to simplify, I model p1, and restructure p2 to represent the proportion of births receiving prenatal care in 
# the second trimester out of those not receiving it in the first trimester (0-1), and similarly for p3
library(ggplot2)
library(dplyr)
source("../jheem_analyses/applications/SHIELD/shield_specification.R")


# Clean CDC Wonder data ----
reshapeData<-function(q1,q2,q3,q0,denom){
  df=reshape2::melt(q1,value.name = "p1") #melt into a dataframe-row per category
  df$p2=as.numeric(q2)
  df$p3=as.numeric(q3)
  df$p0=as.numeric(q0)
  df$w=as.numeric(denom) #add denominator as weight
  # head(df); 
  # rowSums(df[,c('p1','p2','p3','p0')],na.rm=TRUE) #check that the sum of the proportions is 1
  
  # instead of restratifying race/ethnicity, we construct a new variable called 'race1' and set it to 
  # approperiate values for each combination of race/ethnicity in the data: 
  
  df$race1="-1"
  df[df$race=="black or african american" & df$ethnicity==  "not hispanic or latino",]$race1<-"black"
  df[ df$ethnicity==  "hispanic or latino" ,]$race1<-"hispanic"
  df[ df$race1=="-1" ,]$race1<-"other"
  table(df$race1)
  return(df[,c('year','age','race1','p1','p2','p3','p0','w')])
}

#Fitting alternative models and returning intercepts/slopes -----
get.intercepts.and.slopes = function(df,  model ){
  library(splines)
  
  if(model=="fully.interacted"){
    fit = glm(P ~ year + age + race +
                year:age + year:race +
                year:age:race, data=df,
              family='binomial', weights = df$w)    
  } else if(model=="two.way"){
    fit = glm(P ~ year + age + race +
                year:age + year:race, data=df,
              family='binomial', weights = df$w)
  } else if(model=="one.way"){
    fit = glm(P ~ year + age + race, data=df,
              family='binomial', weights = df$w) 
  } else
    stop("model can only be two.way, fully.interacted, or one.way")
  fit
  
  ages<-names(table(df$age));ages
  races<-names(table(df$race));races
  dim.names <- list(age=ages, race=races)
  iterated.values = as.data.frame(get.every.combination(dim.names))
  iterated.values
  
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



# PREPARING DATA ----
# Format Independant Probabilities for modeling  -----
# Define independant variables for modeling the prior (P1,P2,P3):
clean.data<- as.data.frame(reshapeData(q1 = SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,],
                                       q2= SURVEILLANCE.MANAGER$data$prenatal.care.initiation.second.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,],
                                       q3 =SURVEILLANCE.MANAGER$data$prenatal.care.initiation.third.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,],
                                       q0= SURVEILLANCE.MANAGER$data$no.prenatal.care$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,],
                                       denom =SURVEILLANCE.MANAGER$data$prenatal.screening.denominator$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,])
)
df<-clean.data
df <- df %>%
  mutate(p2.exc = p2 / (1 - p1)) %>% #proportion of births not receiving prenatal care in trimester 1 that receive it second trimester
  mutate(p3.exc = p3 / (1 - p1 - p2)) %>% #proportion of births not receiving prenatal care in trimester 1 and 2 that receive it third trimester
  # dplyr::select(-p2, -p3, -p0) %>%
  rename(race = race1) 


# CHECKING THE PERFORMANCE TO CHOOSE THE BEST MODEL ----
check.model.performance<-function(df,
                                  trimester,
                                  selected.model,
                                  filter.covid=F){
  sd<-as.data.frame(df)
  if(trimester == 'first.trimester') df$P <- df$p1
  else if (trimester == 'second.trimester')  df$P <- df$p2
  else if (trimester == 'third.trimester')  df$P <- df$p3
  else stop("trimester must be 1st, 2nd, or 3rd")
  #
  df$P[df$P>0.99]<-0.99 #to avoid infinite values in the logit
  # Filter to pre-covid?
  if (filter.covid) df<-df%>%filter(year<=2020) 
  # set the anchor year
  anchor.year = 2016
  df$year = df$year-anchor.year
  ###
  print(paste("Checking performance for ",selected.model, " fitted to prenatal data from ",trimester))
  ###
  prior= get.intercepts.and.slopes(df, model=selected.model)
  
  functional.form = create.logistic.linear.functional.form(intercept = prior$intercepts,
                                                           slope = prior$slopes,
                                                           anchor.year = anchor.year,
                                                           parameters.are.on.logit.scale = T)  
  ages<-names(table(df$age));ages
  races<-names(table(df$race));races
  dim.names = list(age=ages, race=races)
  
  values = functional.form$project(2015:2035) 
  values = array(unlist(values), 
                 dim = c(sapply(dim.names, length),length(2015:2035)),
                 dimnames = c(dim.names, list(year=2015:2035)))
  
  years<-as.numeric(names(table(df$year)));years
  calendar.years<-years+anchor.year
  
  
  data.means = sapply(years, function(year){ # 2016-2023 (year anchored at 2015)
    sapply(races, function(race){
      sapply(ages, function(age){
        weighted.mean(df$P[df$year==year & df$race==race & df$age==age],na.rm=TRUE,w = df$w[df$year==year & df$race==race & df$age==age]) #take a weighted mean
      })
    })
  })
  #add year dimension
  dim(data.means) = c(sapply(dim.names, length), length(calendar.years))
  dimnames(data.means) = c(dim.names,list(year=calendar.years))
  data.means
  
  plot.age <-
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    geom_point(data=reshape2::melt(apply(data.means, c("age","year"),mean)), aes(x=year, y=value, color=age)) + 
    ggtitle(paste0("Prenatal ",trimester," Projection vs. WONDER data, Age")) +
    theme(plot.title = element_text(hjust = 0.5,size = 10))
   print(plot.age)
  
   plot.race <-
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(data.means, c("race","year"),mean)), aes(x=year, y=value, color=race)) + 
    ggtitle(paste0("Prenatal ",trimester," Projection vs. WONDER data, Race")) +
    theme(plot.title = element_text(hjust = 0.5,size = 10))
   print(plot.race)
  
   x=apply(values, c("year"),mean);dim(x)=c(length(x));dimnames(x)=list(year=2015:2035);x=reshape2::melt(x)
  y=apply(data.means, c("year"),mean);dim(y)=c(length(y));dimnames(y)=list(year=dimnames(data.means)$year);y=reshape2::melt(y)
  plot.year <-
    ggplot() + 
    geom_line(data=x, aes(x=year, y=value)) + 
    geom_point(data=y, aes(x=year, y=value)) + 
        ggtitle(paste0("Prenatal ",trimester," Projection vs. WONDER data, Total")) +
    theme(plot.title = element_text(hjust = 0.5,size = 10))
   print(plot.year)
   
  plot.age.race <-
    ggplot() + 
    geom_line(data=reshape2::melt(apply(values, c("age", "race","year"),mean)), aes(x=year, y=value, color=race)) + 
    geom_point(data=reshape2::melt(apply(data.means, c("age", "race","year"),mean)), aes(x=year, y=value, color=race)) + facet_wrap(~age)+
    ggtitle(paste0("Prenatal ",trimester," Projection vs. WONDER data, age&Race")) +
    theme(plot.title = element_text(hjust = 0.5,size = 10))
  print(plot.age.race)
  # SAVE ###########
  ggsave(filename = paste0("prelim_results/prenatal_",trimester,"_",selected.model,"_year.jpeg"),
         plot.year,width = 10,height = 7,dpi = 350)
  
  ggsave(filename = paste0("prelim_results/prenatal_",trimester,"_",selected.model,"_age.jpeg"),
         plot.age,width = 10,height = 7,dpi = 350)
  ggsave(filename = paste0("prelim_results/prenatal_",trimester,"_",selected.model,"_race.jpeg"),
         plot.race,width = 10,height = 7,dpi = 350)
  ggsave(filename = paste0("prelim_results/prenatal_",trimester,"_",selected.model,"_age_race.jpeg"),
         plot.age.race,width = 10,height = 7,dpi = 350)
  
  print(paste("A ",selected.model, " was fitted and all plots are saved in the folder prelim_results."))
}

# CHECKS/PLOTS:
# A fully interacted model captures age-race values the best
check.model.performance(df,trimester = "first.trimester", selected.model = "fully.interacted",filter.covid = T)
check.model.performance(df,trimester = "first.trimester", selected.model = "two.way",filter.covid = T)
check.model.performance(df,trimester = "second.trimester", selected.model = "fully.interacted",filter.covid = T)
check.model.performance(df,trimester = "second.trimester", selected.model = "two.way",filter.covid = T)
check.model.performance(df,trimester = "third.trimester", selected.model = "fully.interacted",filter.covid = T)
check.model.performance(df,trimester = "third.trimester", selected.model = "two.way",filter.covid = T)


# CACHING THE FINAL PRIOR -----
cache.final.prior<-function(df,
                            trimester,
                            selected.model,
                            filter.covid=F){
  if(trimester == 'first.trimester') df$P <- df$p1
  else if (trimester == 'second.trimester')  df$P <- df$p2
  else if (trimester == 'third.trimester')  df$P <- df$p3
  else stop("trimester must be 1st, 2nd, or 3rd")
  #
  df$P[df$P>0.99]<-0.99 #to avoid infinite values in the logit
  # Filter to pre-covid?
  if (filter.covid) df<-df%>%filter(year<=2020) 
  # set the anchor year
  anchor.year = 2016
  df$year = df$year-anchor.year
  ###
  print(paste("Cacching prior for a ",selected.model, " fitted to prenatal data from ",trimester))
  #
  prior= get.intercepts.and.slopes(df, model=selected.model)
  #
  cache.object.for.version(object = prior,
                           name = paste0("prenatal.care.initiation.",trimester,".prior"),
                           version = 'shield',
                           overwrite=T)
  print(paste0("cashed "," prenatal.care.initiation.",trimester,".prior "," for SHIELD"))
}

cache.final.prior(df,trimester = "first.trimester", selected.model = "fully.interacted",filter.covid = T)
cache.final.prior(df,trimester = "second.trimester", selected.model = "fully.interacted",filter.covid = T)
cache.final.prior(df,trimester = "third.trimester", selected.model = "fully.interacted",filter.covid = T)
