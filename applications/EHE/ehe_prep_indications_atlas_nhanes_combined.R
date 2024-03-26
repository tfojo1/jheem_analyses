source('../jheem_analyses/applications/EHE/ehe_specification.R')

get.male.prep.indication.OR.from.nhanes = function(version,
                                                   location){
  
  load("../jheem_analyses/cached/datasets_from_Zoe/heterosexual.with.sti.RData")  
  df = heterosexual.with.sti
  # total het = 24407 (total # of rows)
  # prep indication = 1269 (outcome $hx.any.sti==1)
  
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
  
  df$year = as.numeric(substr(df$survey.year,1,4))
  
  prep.anchor.year = 2006
  df$orig.year = df$year
  df$year = df$year-prep.anchor.year
  
  # Make outcome numeric
  df$hx.any.sti = as.numeric(df$hx.any.sti)
  
  fit = glm(hx.any.sti ~ age + race + gender + year, data=df, #weights = df$WTMEC2YR,
            family='binomial')  
  
  rv = unname(exp(fit$coefficients["gendermale"]))
  
  rv
}


get.female.prep.indications.atlas = function(version,
                                             location){
  
  specification.metadata = get.specification.metadata(version=version,
                                                      location=location)
  
  df = read.csv("cached/AtlasPlus_gonorrhea/AtlasPlusTableData_stratified.csv",header = T, skip = 7)
  
  df = df[,c(2,5:8,10)]
  df$Year = as.numeric(substr(df$Year,1,4))
  df = df[df$Year<2022,] # 2022 points look like maybe incomplete data; remove
  df$Population = as.numeric(gsub(",","",df$Population))
  df$Cases = as.numeric(gsub(",","",df$Cases))
  
  df$pop.negative = df$Population - df$Cases
  
  df.cases = df[,c(1:5)]
  df.cases$outcome = 1
  df.pop = df[,c(1:4,7)]
  df.pop$outcome = 0
  
  names(df.cases) = names(df.pop) = c("year","age","race","sex","weight","outcome")
  
  df = rbind(df.cases,df.pop)
  
  # remove NA weights
  df = df[!is.na(df$weight),]
  
  # Restratify the ages
  given.ages = unique(df$age)
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age)
  df$orig.age = df$age
  df$age = age.map[df$orig.age]
  df = df[!is.na(df$age),]
  
  # Set reference levels
  reference.age.index = ceiling(specification.metadata$n.ages/2)
  age.levels = c(specification.metadata$dim.names$age[reference.age.index],
                 specification.metadata$dim.names$age[-reference.age.index])
  df$age = factor(df$age, levels=age.levels)
  
  # Map race
  df = df[df$race!="Unknown",] 
  df$orig.race = df$race
  race.ontology.mapping = get.ontology.mapping(from.ontology = list(race=unique(df$race)),
                                               to.ontology = specification.metadata$dim.names['race'])   
  race.map = race.ontology.mapping$get.mapping.vector()
  df$race = race.map[df$orig.race]
  race.levels = union('other', specification.metadata$dim.names$race)
  df$race = factor(df$race, levels = race.levels)
  
  # set anchor year
  anchor.year = 2010
  df$orig.year = df$year
  df$year = df$year-anchor.year
  
  df.female = df[df$sex=="Female",]
  df.female = df.female[,-4]
  
  fit = glm(outcome ~ age + race + year + year:age + year:race, data=df.female,weights = df.female$weight,
            family='gaussian')  # CHANGED TO LINEAR
  
  dim.names = specification.metadata$dim.names[c('age','race')]
  iterated.values = as.data.frame(get.every.combination(dim.names))
  
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data = cbind(iterated.values, year=0) 
  year1.data = cbind(iterated.values, year=1)
  
  # this will work, even with interaction terms, as long as you use a standard linear year term (not squared, splined, etc.) 
  intercepts = predict(fit, year0.data, type = 'link') 
  slopes = predict(fit, year1.data, type='link') - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  rv = list(intercepts = intercepts,
            slopes = slopes,
            df = df.female)
  
  rv
}


female.prep.indications.atlas = get.female.prep.indications.atlas(version = 'ehe', location = 'C.12580')

female.prep.indications.functional.form = create.linear.functional.form(intercept = female.prep.indications.atlas$intercepts,
                                                                        slope = female.prep.indications.atlas$slopes,
                                                                        anchor.year = 2010,
                                                                        #parameters.are.on.logit.scale = F, 
                                                                        # only for logistic.linear.functional.form
                                                                        min = 0,max = 1)  