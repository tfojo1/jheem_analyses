source('../jheem_analyses/applications/EHE/ehe_specification.R')

specification.metadata = get.specification.metadata(version='ehe',
                                                    location='C.12580')

df = read.csv("cached/AtlasPlus_gonorrhea/AtlasPlusTableData_stratified.csv",header = T, skip = 7)

df = df[,c(2,5:8,10)]
df$Year = as.numeric(substr(df$Year,1,4))
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
          family='binomial')  

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

female.prep.indications.functional.form = create.logistic.linear.functional.form(intercept = intercepts,
                                                                                 slope = slopes,
                                                                                 anchor.year = 2010,
                                                                                 parameters.are.on.logit.scale = T)  
values.female = female.prep.indications.functional.form$project(2010:2035) 
values.female = array(unlist(values.female), 
                      dim = c(sapply(dim.names, length),length(2010:2035)),
                      dimnames = c(dim.names, list(year=2010:2035)))

plotting.df = reshape2::melt((values.female))

ggplot(plotting.df,aes(x=year,y=value,color=age,group=age)) + geom_line() + facet_wrap(~race,scales = "free_y")

# add actual data to plots (naive average marginals); as geom_point
# if we don't believe exponential takeoffs once compared to actual data; change to linear regression; min 0 in functional form 
