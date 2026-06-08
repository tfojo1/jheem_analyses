

source("applications/adap_cuts/process_adap_data.R")

specification.metadata = get.specification.metadata(version="rw",
                                                    location="FL")

dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]

median.income.array = array(log(params.total.all.years$mean),
                            dim = sapply(dim.names,length),
                            dimnames = dim.names)

dim(median.income.array)

for (age.index in 1:length(dim.names$age)) {
    median.income.array[age.index,,,] =  median.income.array[age.index,,,] + betas.age[age.index]
}

for (race.index in 1:length(dim.names$race)) {
    median.income.array[,race.index,,] = median.income.array[,race.index,,] + betas.race[race.index]
}

median.income.array[,,,"active_IDU"] = median.income.array[,,,"active_IDU"] + beta.pwid.all.years

median.income.array[,,"female",] = median.income.array[,,"female",] + beta.female.all.years # from ADAP reports 

## Figuring out MSM and Heterosexual Male 

# First, multiply both male categories by the male multiplier from ADAP reports 
median.income.array[,,"msm",] = median.income.array[,,"msm",] + beta.male.all.years
median.income.array[,,"heterosexual_male",] = median.income.array[,,"heterosexual_male",] + beta.male.all.years

# Next, multiply msm by the msm multiplier (relative to all male) from MMP 
median.income.array[,,"msm",] = median.income.array[,,"msm",] + beta.msm.all.years

# Next, multiply het male by the het male multiplier (relative to all male) from MMP 
median.income.array[,,"heterosexual_male",] = median.income.array[,,"heterosexual_male",] + beta.het.male.all.years

# we will uptune everyone so that the median is closer to overall 116; come up with an estimated count; what do we need to add to the dist to get there

# easier option: x = median.income.array + log(116) - median(median.income.array)
# then check median(exp(x)); this is probably good enough for the prior 

# review values
#dimnames(median.income.array)
exp(median.income.array) 
#exp(median.income.array[,,"msm","active_IDU"])
exp(median.income.array[,,"msm","never_IDU"])
exp(median.income.array[,,"female","never_IDU"])
exp(median.income.array[,,"heterosexual_male","never_IDU"]) # very slightly higher than female 


# from ADAP reports: 
params.total.all.years$mean

exp(betas.race)
exp(betas.age)
exp(beta.female.all.years) # 0.7611259 
exp(beta.male.all.years) # 1.069898


# het male multiplier: 
# (male:general * het male:all male) = 1.069898*0.7190184
exp(beta.male.all.years) * exp(beta.het.male.all.years)

# msm multiplier: 
# (male:general * msm:all male) = 1.069898*1.151057
exp(beta.male.all.years) * exp(beta.msm.all.years)

exp(beta.pwid.all.years) 

msm.mmp.2021
het.male.mmp.2021

total.dist.all.years
rbind(total.mmp.2021,total.mmp.2022)




# From MMP reports: 
params.total.mmp$mean 
exp(beta.het.male.all.years) 
# when it's capped at 5000 and relative to total: 0.5649366    
# when it's capped at 5000 and relative to all male: 0.4653559
# when it's capped at 400 and relative to all male: 0.7190184
exp(beta.msm.all.years) 
# when it's capped at 5000 and relative to total: 1.479455
# when it's capped at 5000 and relative to all male: 1.218673
# when it's capped at 400 and relative to all male: 1.151057
exp(beta.cis.male.all.years) 
# when it's capped at 5000: 1.213988
# when it's capped at 400: 1.125697
#exp(beta.cis.female.all.years) 
# when it's capped at 5000 and relative to total: 0.4884746 
# when it's capped at 5000 and relative to all male: 0.5090962 
# when it's capped at 400 and relative to total: 0.6795281
# when it's capped at 400 and relative to all male: 0.603651


# These do the same thing as the for loops above 
# median.income.array <- sweep(
#     median.income.array,
#     MARGIN = 1,   # age dimension
#     STATS = betas.age,
#     FUN = "+"
# )
# 
# median.income.array <- sweep(
#     median.income.array,
#     MARGIN = 2,   # race
#     STATS = betas.race,
#     FUN = "+"
# )