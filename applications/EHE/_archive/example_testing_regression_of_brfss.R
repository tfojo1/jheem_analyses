source('../jheem_analyses/applications/EHE/ehe_specification.R')

get.high.testing.rate.ratios

load("Q:data_clean/brfss/brfss.subset.RData")




#-- RESTRATIFY THE DATA --#

# Just keep when we know MSM and high-risk
df = appended[!is.na(appended$msm),]

# Restratify the ages
source('../jheem_analyses/applications/EHE/ehe_specification.R')
specification.metadata = get.specification.metadata('ehe', 'C.12580')

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

# Map high.risk to logical
df$orig.high.risk = df$high.risk
df$high.risk = df$high.risk==1



##-- BELOW IS HOW WE CAN GET TESTING RATE MULTIPLIERS --##

fit = glm(tested.past.year ~ age + sex + race + high.risk + age:high.risk + sex:high.risk + race:high.risk,
          family='binomial', data=df, weights = df$weighting.var)


# make this into an array
dim.names = specification.metadata$dim.names[c('age','race','sex')]
iterated.values = as.data.frame(get.every.combination(dim.names))

low.risk.values = cbind(iterated.values, high.risk=F)
low.risk.p = predict(fit, low.risk.values, type = 'response')

high.risk.values = cbind(iterated.values, high.risk=T)
high.risk.p = predict(fit, high.risk.values, type = 'response')

high.risk.testing.rate.ratios = -log(1-high.risk.p) / -log(1-low.risk.p)
dim(high.risk.testing.rate.ratios) = sapply(dim.names, length)
dimnames(high.risk.testing.rate.ratios) = dim.names

high.risk.testing.rate.ratios


