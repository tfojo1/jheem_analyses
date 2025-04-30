# MAPPING FERTILITY RATE DATA AS A FUNCTION OF TIME AND OTHER COVARIATES 
library(reshape2)
library(ggplot2)
library(splines)

# DATA PREPRATION ----
# Loading census manager
source('applications/SHIELD/shield_source_code.R')
# CENSUS.MANAGER$outcomes
# CENSUS.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity

# years for which fertility data is reported:
YEARS=2007:2023
YEARS.PROJECT=2007:2040
location='US'

# extracting the data
fertility.rate = CENSUS.MANAGER$pull(outcome='fertility.rate',
                                     location = 'US',
                                     year= YEARS,
                                     keep.dimensions = c('location','age','race', 'ethnicity','year'), #@Todd,Andrew: it should work without the location but it fails
                                     na.rm=TRUE)
dimnames(fertility.rate)
# extracting dimensions 
# specification.metadata=get.specification.metadata('shield','US')
# specification.metadata$dim.names
# target.dimnames=c(list( year = as.character(YEARS)), specification.metadata$dim.names[c('age','race')])  

# target.dimnames: we set this manually to include female of childbearing ages and correct years
target.dimnames=target.dimnames <- list(
  age = c(  "15-19 years", "20-24 years", "25-29 years", "30-34 years",
            "35-39 years", "40-44 years"  ),
  race = c("black", "hispanic", "other"),
  year = as.character(YEARS)
);target.dimnames
mapped.fertility.rate=map.value.ontology(fertility.rate, 
                                         target.dim.names = target.dimnames,
                                         na.rm = TRUE)
print("data is ready")
# Explanatory analysis ----
# To convert a multi-dimensional object to dataframe
df= melt(mapped.fertility.rate, varnames = c("age", "race", "year"), value.name = "fertility.rate")
ggplot(df, aes(x = factor(year), y = fertility.rate)) + 
  geom_boxplot(fill = "lightblue") +  labs(x = "Year", y = "Fertility Rate") +  theme_minimal() 
#slight reduction over time
ggplot(df, aes(x = factor(year), y = fertility.rate)) +facet_wrap(~age)+  geom_boxplot(fill = "lightblue") +  labs(x = "Year", y = "Fertility Rate") +  theme_minimal() 
#significant variation by age, changes in direction by time within different agegroups 
ggplot(df, aes(x = factor(year), y = fertility.rate)) +facet_wrap(~race)+  geom_boxplot(fill = "lightblue") +  labs(x = "Year", y = "Fertility Rate") +  theme_minimal() 
ggplot(df, aes(x = factor(year), y = fertility.rate)) +facet_wrap(~age*race)+  geom_boxplot(fill = "lightblue") +  labs(x = "Year", y = "Fertility Rate") +  theme_minimal() 
# df%>%filter(race=='black') %>%
#   ggplot( aes(x = fertility.rate)) + 
#   geom_density() + 
#   facet_grid(rows = vars(year)) + 
#   theme_minimal()

### FITTING MODELS ### ----
# GLM: main effects and interactions 
# quasibinomial is used when the dependent variable is a proportion (0 < fertility.rate < 1).
# It assumes that the variance is proportional to the mean but does not require the data to follow the strict binomial variance assumption.
# GLM.quasibinomial uses a logit link funciton to map the data
# GLM.gaussian uses identity link
# fit.models----
fit.models<- function( type="glm.one.way",family='quasibinomial'  ){
  if(type=="glm.one.way"){
    fit=glm(fertility.rate ~ age + race + year, 
            data = df, family =family )
  } else if(type=="glm.two.way1"){
    fit=glm(fertility.rate ~ age * race + year, 
            data = df, family =family )
  } else if(type=="glm.two.way2"){
    fit=glm(fertility.rate ~ age * race + year + year:age , 
            data = df, family =family )
  } else if(type=="glm.two.way3"){
    fit=glm(fertility.rate ~ age * race + year +  year:race, 
            data = df, family =family )
  }else if(type=="glm.two.way4"){
    fit=glm(fertility.rate ~ age * race + year + year:age + year:race, 
            data = df, family =family )
  }else if(type=="glm.three.way"){
    fit=glm(fertility.rate ~ age * race*year, 
            data = df, family =family )
  } 

  return(fit)
}
# plot.fitted.values ----
plot.fitted.values<-function(df.fitted){
  df=df.fitted
  # BY YEAR
  df_mean <- df %>%
    group_by(year) %>%
    summarize(mean_fertility = mean(fertility.rate, na.rm = TRUE),  
              mean_predicted_fertility = mean(predicted_fertility, na.rm = TRUE), 
              .groups = 'drop')
  b0<-ggplot(df_mean, aes(x = year, y = mean_fertility, group = 1)) +
    geom_line(aes(color = "Actual"), size = 1) +                             # Actual values
    geom_line(aes(y = mean_predicted_fertility, color = "Fitted"), size = 1, linetype = "dashed") +  # Fitted values                                                     # Facet by age groups
    theme_minimal() +
    labs(title = "Fertility Rate: Actual vs. Fitted Over Time",
         x = "Year", y = "Fertility Rate",
         color = "Line Type") +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +    # Set custom colors
    theme(legend.position = "bottom")
  # BY AGE YEAR
  df_mean <- df %>%
    group_by(age,year) %>%
    summarize(mean_fertility = mean(fertility.rate, na.rm = TRUE),  
              mean_predicted_fertility = mean(predicted_fertility, na.rm = TRUE), 
              .groups = 'drop')
  b1<-ggplot(df_mean, aes(x = year, y = mean_fertility, group = 1)) +
    geom_line(aes(color = "Actual"), size = 1) +                             # Actual values
    geom_line(aes(y = mean_predicted_fertility, color = "Fitted"), size = 1, linetype = "dashed") +  # Fitted values
    facet_wrap(~ age) +                                                      # Facet by age groups
    theme_minimal() +
    labs(title = "Fertility Rate: Actual vs. Fitted Over Time",
         x = "Year", y = "Fertility Rate",
         color = "Line Type") +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +    # Set custom colors
    theme(legend.position = "bottom")
  ####
  # BY RACE YEAR
  df_mean <- df %>%
    group_by(race,year) %>%
    summarize(mean_fertility = mean(fertility.rate, na.rm = TRUE),  
              mean_predicted_fertility = mean(predicted_fertility, na.rm = TRUE), 
              .groups = 'drop')
  b2<-ggplot(df_mean, aes(x = year, y = mean_fertility, group = 1)) +
    geom_line(aes(color = "Actual"), size = 1) +                             # Actual values
    geom_line(aes(y = mean_predicted_fertility, color = "Fitted"), size = 1, linetype = "dashed") +  # Fitted values
    facet_wrap(~ race) +                                                      # Facet by age groups
    theme_minimal() +
    labs(title = "Fertility Rate: Actual vs. Fitted Over Time",
         x = "Year", y = "Fertility Rate",
         color = "Line Type") +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +    # Set custom colors
    theme(legend.position = "bottom")
  ####
  # BY AGE RACE YEAR
  b3<-ggplot(df, aes(x = year, y = fertility.rate, group = 1)) +
    geom_line(aes(color = "Actual"), size = 1) +                             # Actual values
    geom_line(aes(y = predicted_fertility, color = "Fitted"), size = 1, linetype = "dashed") +  # Fitted values
    facet_wrap(~ age*race,scales = "free_y") +                                                      # Facet by age groups
    theme_minimal() +
    labs(title = "Fertility Rate: Actual vs. Fitted Over Time",
         x = "Year", y = "Fertility Rate",
         color = "Line Type") +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +    # Set custom colors
    theme(legend.position = "bottom")
  
  return(list(b0,b1,b2,b3))
}
# return_intercept_slope ----
return_intercept_slope<-function(fit){
  # Unpack the intercepts and slopes into arrays indexed ['age','race'] to build ocrresponding functional form
  # Intercept
  # The intercept represents the expected value of the response variable (in this case, fertility rate) when all predictor variables are at their reference levels.
  # For categorical variables like age and race, the reference level is typically the first level alphabetically (or as specified in the model). Therefore, the intercept will reflect the average fertility rate for this reference category when the numerical variable year is equal to 0.
  # Slopes
  # Year: The coefficient (slope) for year indicates how the fertility rate changes with each unit increase in the year. If the coefficient for year is positive, it implies that fertility rates are increasing over time; if negative, fertility rates are decreasing.
  # Age and Race: For categorical variables like age and race, the model will estimate a separate coefficient (slope) for each level of the factor, compared to the reference level. This means:
  # For age & race, the slopes will represent the change in fertility rate for each age/race group compared to the reference age group.
  
  dim.names = target.dimnames[c('age','race')]
  iterated.values = as.data.frame(get.every.combination(dim.names))
  # 
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data = cbind(iterated.values, year=0) #gives you the intercepts for each stratum (combination of age and race) since the year terms are effectively removed or set to zero.
  year1.data = cbind(iterated.values, year=1) #The difference between these two predictions (slopes) provides the effect of moving from year 0 to year 1, which represents the slope or change in predicted values associated with the increase in year.
  
  # fit$coefficients
  #@TODD: what is the 'link' here?
  intercepts = predict(fit, year0.data, type="link") #makes the prediction in the "link" scale in which case logit
  slopes = predict(fit, year1.data, type="link") - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  rv = list(intercepts=intercepts,
            slopes=slopes)
  rv
}

# plot.projected.values ----
plot.projected.values<-function(type='glm.three.way',family='gaussian'){
  # fit the model
  fit=fit.models(type=type,family=family);
  # extract slope and intercept
  rv=return_intercept_slope(fit)
  ff<-NULL
  if (family=="gaussian"){   
    ff=create.linear.functional.form(intercept = rv$intercepts,
                                     slope = rv$slopes,
                                     anchor.year = 0)
  }else{ 
    ff=create.logistic.linear.functional.form(intercept = rv$intercepts,
                                              slope = rv$slopes,
                                              anchor.year = 0,
                                              parameters.are.on.logit.scale=T)
  }
  YEARS.PROJECT=2007:2040
  sim=ff$project(YEARS.PROJECT)
  sim.target.dimnames=c(target.dimnames[1:2],list(year=YEARS.PROJECT))
  sim=array(unlist(sim),
            dim = sapply(sim.target.dimnames,length),
            dimnames =sim.target.dimnames)
  # PLOT BY YEAR
  varnames = c( "year")
  mean_sim <- apply(sim, varnames, mean)
  mean_sim <- as.data.frame(cbind(year=as.numeric(names(mean_sim)), fertility.rate=as.numeric(mean_sim)))
  mean_data <- apply(mapped.fertility.rate, varnames, mean)
  mean_data <- as.data.frame(cbind(year=as.numeric(names(mean_data)), fertility.rate=as.numeric(mean_data)))
  
  b0=ggplot() + 
    geom_line(data=mean_sim, aes(x=year, y=fertility.rate)) +
    geom_point(data=mean_data, aes(x=year, y=fertility.rate)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  # PLOT BY AGE
  varnames = c("age", "year")
  mean_sim=melt(apply(sim, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  mean_data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b1=ggplot() + 
    geom_line(data=mean_sim, aes(x=year, y=fertility.rate, color=age)) +
    geom_point(data=mean_data, aes(x=year, y=fertility.rate, color=age)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  # PLOT BY RACE
  varnames = c("race", "year")
  mean_sim=melt(apply(sim, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  mean_data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b2=ggplot() + 
    geom_line(data=mean_sim, aes(x=year, y=fertility.rate, color=race)) +
    geom_point(data=mean_data, aes(x=year, y=fertility.rate, color=race)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  # PLOT BY AGE RACE 
  varnames = c("age", "race", "year")
  mean_sim=melt(apply(sim, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  mean_data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b3=ggplot() + 
    geom_line(data=mean_sim, aes(x=year, y=fertility.rate, color=race)) + 
    geom_point(data=mean_data, aes(x=year, y=fertility.rate, color=race)) + 
    facet_wrap(~age)+
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  
  return(list(b0,b1,b2,b3))
}

## ----
plot.projected.values(type='glm.two.way2',family='gaussian')[4]
plot.projected.values(type='glm.three.way',family='quasibinomial')[4]
 



## defining a spline function ----
# spline function here with 2 knots create a linear function 

# we can use projections from the linear model for the spline knots 
knot1=apply(mapped.fertility.rate[,,as.character(2008:2012)],c('age','race'),mean)
knot2=apply(mapped.fertility.rate[,,as.character(2018:2022)],c('age','race'),mean)
ff=create.natural.spline.functional.form(knot.times = c(time1=2010, time2=2020, time3=2030),
                                         knot.values = list(time1=knot1,
                                                            time2=knot2, 
                                                            time3=knot2+0.5*(knot2-knot1)))
qplot(2007:2040,sapply(ff$project(2007:2040),function(x){x[[12]]}))

#there is acode for sampling knot3 coef as a function of knot2
#logistic tail






# Manual EXAMPLE: ----
fit=fit.models(type="glm.three.way",family="quasibinomial");fit


# approach1:
predicted_values <- predict(fit, type = "response")  # type = "response" gives the predictions on the scale of the response variable (proportion)
df.fitted=df;df.fitted$predicted_fertility <- predicted_values
b=plot.fitted.values(df.fitted)
print(b[[4]]);

# approach2:
# how to estimate the fit for 15-19 black in 2007 manually:
expit<-function(x){1/(1+exp(-x))} 
v=expit(fit$coefficients[1]+ #intercept
          fit$coefficients[9]*2007 #year
        )
# compare v against df.fitted[1,'predicted_fertility'] should be the same
df.fitted[1,'predicted_fertility']

# approach3:
# we can also extract the intercept and slope and use them for prediction
rv=return_intercept_slope(fit)
vv=expit(rv$intercepts[1]+rv$slopes[1]*2007)





