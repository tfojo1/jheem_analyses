# MAPPING FERTILITY RATE DATA AS A FUNCTION OF TIME AND OTHER COVARIATES 
# DATA PREPRATION ----
# Loading census manager
if (!exists('CENSUS.MANAGER')){
  # cat("Loading Census Manager (may take a minute or two)...")
  CENSUS.MANAGER = load.data.manager.from.cache('census.manager.rdata', set.as.default=F)
  # print("Census manager read")
}
# CENSUS.MANAGER$outcomes
# CENSUS.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity

# years for which fertility data is reported:
YEARS=2007:2023
YEARS.PROJECT=2007:2035
location='US'
# extracting the data
fertility.rate = CENSUS.MANAGER$pull(outcome='fertility.rate',
                                     location = 'US',
                                     year= YEARS,
                                     keep.dimensions = c('location','age','race', 'ethnicity','year'), #@Todd,Andrew: it should work without the location but it fails
                                     na.rm=TRUE)
dimnames(fertility.rate)
# extracting dimensions 
specification.metadata=get.specification.metadata('shield','US')
# specification.metadata$dim.names
# target.dimnames=specification.metadata$dim.names[c('age','race')] #Todd: this doesnt have year

target.dimnames=target.dimnames <- list(
  age = c(  "15-19 years", "20-24 years", "25-29 years", "30-34 years",
            "35-39 years", "40-44 years"  ),
  race = c("black", "hispanic", "other"),
  year = as.character(YEARS)
);target.dimnames

mapped.fertility.rate=map.value.ontology(fertility.rate, 
                                         target.dim.names = target.dimnames,
                                         na.rm = TRUE)

# FITTING MODELS ----
# To convert a multi-dimensional object to dataframe
library(reshape2)
df= melt(mapped.fertility.rate, varnames = c("age", "race", "year"), value.name = "fertility.rate")
ggplot(df, aes(x=fertility.rate)) + geom_density() #bi-modal distribution
ggplot(df, aes(x=fertility.rate)) + geom_density()+facet_wrap(~age) # bi-modal dists 
ggplot(df, aes(x=fertility.rate)) + geom_density()+facet_wrap(~race) # bi-modal dists
ggplot(df, aes(x=fertility.rate)) + geom_density()+facet_wrap(~race*age) # bi-modal dists> there is a year component too

df%>%filter(race=='black') %>%
  ggplot( aes(x = fertility.rate)) + 
  geom_density() + 
  facet_grid(rows = vars(year)) + 
  theme_minimal()


# GLM: main effects and interactions
##@TODD:is it corretc to use binomial family here?
fit.models= function( model="glm.one.way"){
  if(model=="glm.one.way"){
    fit=glm(fertility.rate ~ age + race + year, 
            data = df, family = 'binomial')
  } else if(model=="glm.two.way"){
    fit=glm(fertility.rate ~ age * race + year, 
            data = df, family = 'binomial')
  } else if(model=="glm.three.way1"){
    fit=glm(fertility.rate ~ age * race + year + year:age , 
            data = df, family = 'binomial')
  } else if(model=="glm.three.way2"){
    fit=glm(fertility.rate ~ age * race + year +  year:race, 
            data = df, family = 'binomial')
  }else if(model=="glm.three.way3"){
    fit=glm(fertility.rate ~ age * race + year + year:age + year:race, 
            data = df, family = 'binomial')
  }
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
  
  #@TODD: is there another way to extract these for more complicated models?
  # fit$coefficients
  # this will work, even with interaction terms as long as you use a standard linear year term (not squared, splined, etc.), 
  #@TODD: what is the 'link' here?
  intercepts = predict(fit, year0.data, type="link") 
  slopes = predict(fit, year1.data, type="link") - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  rv = list(intercepts=intercepts,
            slopes=slopes)
  rv
}

plot.fit=function(type="glm.one.way"){
  
  # 1- fit the models
  model = fit.models(type )
  #evaluate functional forms
  fertility.functional.form = create.logistic.linear.functional.form(intercept = model$intercepts,
                                                            slope = model$slopes,
                                                            anchor.year = YEARS[1],#2007 
                                                            parameters.are.on.logit.scale=T) 
  
  #project values from this fucnitonal form
  values = fertility.functional.form$project(YEARS.PROJECT) 
  values = array(unlist(values), 
                 dim = c(sapply(dim.names, length),length(YEARS.PROJECT)),
                 dimnames = c(dim.names, list(year=YEARS.PROJECT)))
  
  
  # PLOT BY AGE
  varnames = c("age", "year")
  sim=melt(apply(values, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b1=ggplot() + 
    geom_line(data=sim, aes(x=year, y=fertility.rate, color=age)) +
    geom_point(data=data, aes(x=year, y=fertility.rate, color=age)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  b1
  # PLOT BY RACE
  varnames = c("race", "year")
  sim=melt(apply(values, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b2=ggplot() + 
    geom_line(data=sim, aes(x=year, y=fertility.rate, color=race)) +
    geom_point(data=data, aes(x=year, y=fertility.rate, color=race)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  b2
  # PLOT BY AGE RACE 
  varnames = c("age", "race", "year")
  sim=melt(apply(values, varnames,mean), varnames = varnames, value.name = "fertility.rate")
  data=melt(apply(mapped.fertility.rate, varnames,mean),varnames = varnames, value.name = "fertility.rate")
  b3=ggplot() + 
    geom_line(data=sim, aes(x=year, y=fertility.rate, color=race)) + 
    geom_point(data=data, aes(x=year, y=fertility.rate, color=race)) + 
    facet_wrap(~age)+
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  b3
  return(list(b1,b2,b3))
}  

plot.fit( "glm.two.way")[3]
plot.fit( "glm.three.way1")[1]
plot.fit("glm.three.way2")[3]
plot.fit( "glm.three.way3")[3]
