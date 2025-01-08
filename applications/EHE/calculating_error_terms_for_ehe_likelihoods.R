
# POPULATION: 0.1561269 (ethnicity); 0.1939618 (age-ethnicity)
calculate.error.terms.pop(stratification = "ethnicity",
                          is.cv = T)

calculate.error.terms.pop(stratification = "age-ethnicity",
                          is.cv = T)
# other estimates: 0.3023304 (all); 0.1031615 (race); 0.2771433 (age-race)


# SUPPRESSION: 0.04560282 
calculate.error.terms(data.type = "suppression",
                      data.source.1 = "cdc.aggregated.proportion",
                      data.source.2 = "lhd",
                      is.cv = F)

# DIAGNOSES:  0.04110863
# 1/8 value: 0.1005035
calculate.lhd.error.terms("diagnoses")
# (old diagnoses value = 0.04621778)
# calculate.error.terms(data.type = "diagnoses",
#                       data.source.1 = "cdc.surveillance.reports",
#                       data.source.2 = "cdc.aggregated.county",
#                       is.cv = T)

# PREVALENCE: 0.07436122
# 1/8 value: 0.1218498
calculate.lhd.error.terms("diagnosed.prevalence")
# (old prevalence value = 0.04711922)
# calculate.error.terms(data.type = "diagnosed.prevalence",
#                       data.source.1 = "cdc.surveillance.reports",
#                       data.source.2 = "cdc.aggregated.county",
#                       is.cv = T)

# AIDS DIAGNOSES: 0.2277531 - from 1993-1997 only; one source only has totals so no stratifications anyway 
calculate.error.terms(data.type = "aids.diagnoses",
                      data.source.1 = "cdc.surveillance.reports",
                      data.source.2 = "cdc.aids",
                      years = c(1993:1997),
                      is.cv = T)
# PREP UPTAKE: 0.01239159
calculate.error.terms(data.type = "prep",
                      data.source.1 = "aidsvu",
                      data.source.2 = "cdc.prep",
                      is.cv = T)

calculate.lhd.error.terms = function(data.type){
  lhd.data = read.csv("input_managers/LHD_Diagnoses_and_Diagnosed_Prevalence.csv")
  lhd.data = lhd.data[lhd.data$MSA!="Indianapolis",]
  
  if(data.type=="diagnoses"){
    all.values1 = as.numeric(lhd.data$LHD.New.Diganoses)
    all.values2 = as.numeric(lhd.data$Atlas.Plus.Summed.New.Diagnoses..Data.Manager.)
    
  } else if(data.type=="diagnosed.prevalence"){
    
    all.values1 = as.numeric(lhd.data$LHD.Diagnosed.Prevalence)
    all.values2 = as.numeric(lhd.data$Atlas.Plus.Summed.Prevalence..Data.Manager.)    
  } else 
    stop("only set up for diagnoses and diagnosed prevalence")

  cvs.1 = (all.values1 - all.values2)/all.values1 
  cvs.2 = (all.values1 - all.values2)/all.values2
  
  cvs = c(cvs.1,cvs.2)
  cvs[is.nan(cvs)] = 0
  cvs = cvs[!is.na(cvs) & !is.infinite(cvs)]
  
  rv = sqrt(sum(cvs^2, na.rm=T)/sum(!is.na(cvs))) 
  
  rv
}



calculate.error.terms = function(data.type,
                                 data.source.1,
                                 data.source.2,
                                 years=NULL,
                                 use.totals = T,
                                 use.age = T,
                                 use.risk = T,
                                 use.sex = T,
                                 use.race = F,
                                 is.cv){
  
  all.values1 = numeric()
  all.values2 = numeric()
  
  #data1 = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.aggregated.proportion$cdc
  #data2 = SURVEILLANCE.MANAGER$data$suppression$estimate$lhd$lhd # local health departments 
  data1 = SURVEILLANCE.MANAGER$data[[data.type]]$estimate[[data.source.1]][[1]]
  data2 = SURVEILLANCE.MANAGER$data[[data.type]]$estimate[[data.source.2]][[1]]
  
  
  # TOTAL
  if(use.totals){
    years.in.both.total = intersect(dimnames(data1[["year__location"]])$year,
                                    dimnames(data2[["year__location"]])$year)
    if(!is.null(years)){
      years.in.both.total = intersect(years.in.both.total,years)
    }
    locations.in.both.total = intersect(dimnames(data1[["year__location"]])$location,
                                        dimnames(data2[["year__location"]])$location)
    
    values1 = data1[["year__location"]][years.in.both.total, locations.in.both.total]
    values2 = data2[["year__location"]][years.in.both.total, locations.in.both.total]
    
    all.values1 = c(all.values1, values1)
    all.values2 = c(all.values2, values2)
  }

  if(use.age){
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
  }


if(use.risk){
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
}
  if(use.sex){
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

  }
  
  if(use.race){
  # RACE
  years.in.both.race = intersect(dimnames(data1$year__location__race)$year,
                                 dimnames(data2$year__location__race)$year)
  locations.in.both.race = intersect(dimnames(data1$year__location__race)$location,
                                     dimnames(data2$year__location__race)$location)
  races.in.both = intersect(dimnames(data1$year__location__race)$race,
                            dimnames(data2$year__location__race)$race) # HAVE TO DO MAPPINGS HERE FOR RACE TO WORK

  values1 = data1$year__location__race[years.in.both.race, locations.in.both.race,]
  values2 = data2$year__location__race[years.in.both.race, locations.in.both.race,]

  all.values1 = c(all.values1, values1)
  all.values2 = c(all.values2, values2)
  }
  
  if(is.cv){
    # Calculate it
    cvs.1 = (all.values1 - all.values2)/all.values1 
    cvs.2 = (all.values1 - all.values2)/all.values2
    
    cvs = c(cvs.1,cvs.2)
    cvs[is.nan(cvs)] = 0
    cvs = cvs[!is.na(cvs) & !is.infinite(cvs)]
    
    # check for outliers
    mean.cv = mean(cvs)
    sd.cv = sd(cvs)
    outliers.mask = cvs<(mean.cv - (3*sd.cv)) | cvs>(mean.cv + (3*sd.cv))
    cvs = cvs[!outliers.mask]
    rv = sqrt(sum(cvs^2, na.rm=T)/sum(!is.na(cvs))) 
    
  } else {
    
    # Calculate it
    errors = all.values1 - all.values2
    rv = sqrt(sum(errors^2, na.rm=T)/sum(!is.na(errors))) 
  }

  rv
  
}


calculate.error.terms.pop = function(stratification,
                                     is.cv){
  
  all.values1 = numeric()
  all.values2 = numeric()

  # anything other than age alone, sex alone, and age-sex
  
  if(stratification=="age-ethnicity"){
    data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity["2019",,,]
    data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity["2020",,,]  
  } else if(stratification=="ethnicity"){
    data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__ethnicity["2019",,]
    data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__ethnicity["2020",,]
  } else if(stratification=="age-race"){
    data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race["2019",,,]
    data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race["2020",,,]
  } else if(stratification=="race"){
    data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race["2019",,]
    data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race["2020",,]
  } else if(stratification=="all"){
    data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex["2019",,,,,]
    data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex["2020",,,,,]
  } else
    stop("right now only set up for age-ethnicity, ethnicity, age-race, race, or all")

  all.values1 = data1
  all.values2 = data2
  
  if(is.cv){
    # Calculate it
    #cvs.1 = (all.values1 - all.values2)/all.values1 
    cvs.2 = (all.values1 - all.values2)/all.values2
    
    cvs = c(cvs.2) #,cvs.2)
    cvs[is.nan(cvs)] = 0
    cvs = cvs[!is.na(cvs) & !is.infinite(cvs)]
    
    # check for outliers
    mean.cv = mean(cvs)
    sd.cv = sd(cvs)
    outliers.mask = cvs<(mean.cv - (3*sd.cv)) | cvs>(mean.cv + (3*sd.cv))
    cvs = cvs[!outliers.mask]
    rv = sqrt(sum(cvs^2, na.rm=T)/sum(!is.na(cvs))) 
    
  } else {
    
    # Calculate it
    errors = all.values1 - all.values2
    rv = sqrt(sum(errors^2, na.rm=T)/sum(!is.na(errors))) 
  }
  
  rv
  
}
