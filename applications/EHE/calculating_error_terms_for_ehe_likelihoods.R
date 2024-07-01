

all.values1 = numeric()
all.values2 = numeric()


data1 = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.aggregated.proportion$cdc
data2 = SURVEILLANCE.MANAGER$data$suppression$estimate$lhd$lhd

# TOTAL
years.in.both.total = intersect(dimnames(data1$year__location)$year,
                                dimnames(data2$year__location)$year)
locations.in.both.total = intersect(dimnames(data1$year__location)$location,
                                    dimnames(data2$year__location)$location)

values1 = data1$year__location[years.in.both.total, locations.in.both.total]
values2 = data2$year__location[years.in.both.total, locations.in.both.total]

all.values1 = c(all.values1, values1)
all.values2 = c(all.values2, values2)

# AGE
years.in.both.age = intersect(dimnames(data1$year__location__age)$year,
                               dimnames(data2$year__location__age)$year)
locations.in.both.age = intersect(dimnames(data1$year__location__age)$location,
                                   dimnames(data2$year__location__age)$location)
ages.in.both = intersect(dimnames(data1$year__location__age)$age,
                          dimnames(data2$year__location__age)$age) 

values1 = data1$year__location__age[years.in.both.age, locations.in.both.age,ages.in.both]
values2 = data2$year__location__age[years.in.both.age, locations.in.both.age,ages.in.both]

all.values1 = c(all.values1, values1)
all.values2 = c(all.values2, values2)


# RISK
years.in.both.risk = intersect(dimnames(data1$year__location__risk)$year,
                              dimnames(data2$year__location__risk)$year)
locations.in.both.risk = intersect(dimnames(data1$year__location__risk)$location,
                                  dimnames(data2$year__location__risk)$location)
risks.in.both = intersect(dimnames(data1$year__location__risk)$risk,
                         dimnames(data2$year__location__risk)$risk) 

values1 = data1$year__location__risk[years.in.both.risk, locations.in.both.risk,risks.in.both]
values2 = data2$year__location__risk[years.in.both.risk, locations.in.both.risk,risks.in.both]

all.values1 = c(all.values1, values1)
all.values2 = c(all.values2, values2)

# SEX
years.in.both.sex = intersect(dimnames(data1$year__location__sex)$year,
                               dimnames(data2$year__location__sex)$year)
locations.in.both.sex = intersect(dimnames(data1$year__location__sex)$location,
                                   dimnames(data2$year__location__sex)$location)
sexes.in.both = intersect(dimnames(data1$year__location__sex)$sex,
                          dimnames(data2$year__location__sex)$sex) 

values1 = data1$year__location__sex[years.in.both.sex, locations.in.both.sex,sexes.in.both]
values2 = data2$year__location__sex[years.in.both.sex, locations.in.both.sex,sexes.in.both]

all.values1 = c(all.values1, values1)
all.values2 = c(all.values2, values2)


# RACE
# years.in.both.race = intersect(dimnames(data1$year__location__race)$year,
#                                dimnames(data2$year__location__race)$year)
# locations.in.both.race = intersect(dimnames(data1$year__location__race)$location,
#                                    dimnames(data2$year__location__race)$location)
# races.in.both = intersect(dimnames(data1$year__location__race)$race,
#                           dimnames(data2$year__location__race)$race) # HAVE TO DO MAPPINGS HERE FOR RACE TO WORK 
# 
# values1 = data1$year__location__race[years.in.both.race, locations.in.both.race,]
# values2 = data2$year__location__race[years.in.both.race, locations.in.both.race,]
# 
# all.values1 = c(all.values1, values1)
# all.values2 = c(all.values2, values2)

# Calculate it
errors = all.values1 - all.values2
sqrt(sum(errors^2, na.rm=T)/sum(!is.na(errors)))
