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

median.income.array[,,"female",] = median.income.array[,,"female",] + beta.female.all.years


exp(betas.race)
exp(betas.age)

exp(beta.male.all.years)
exp(beta.female.all.years)

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