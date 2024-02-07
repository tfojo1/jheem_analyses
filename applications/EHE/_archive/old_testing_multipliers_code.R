## N.NEGATIVE ## 
# https://www.census.gov/data/tables/2021/demo/age-and-sex/2021-age-sex-composition.html
# https://www.census.gov/data/tables/2021/demo/hispanic-origin/2021-cps.html 
# https://www.census.gov/topics/population/race/data/tables.2021.List_523928342.html 
n.negative.total = 1000*c("total" = 326195)
n.negative.age = 1000*c("10-14 years" = 20651,"15-19 years" = 20655, "20-24 years" = 21022,
                        "25-29 years" = 22592, "30-34 years" = 22691,"35-39 years" = 21607, 
                        "40-44 years" = 20397, "45-49 years"  =19406, "50-54 years" = 20273,
                        "55-59 years" = 20682, "60-64 years" = 21094, "65-69 years" = 18162,
                        "70-74 years" = 14878, "75-79 years" = 10172, "80-84 years" = 6632,
                        "85+ years" = 5992)
n.negative.race = 1000*c("black" = 43490,
                         "hispanic" = 61304, 
                         "other" = 194524)
n.negative.risk = 1000*c("msm" = (.039*159977), #3.9% of male pop; https://publichealth.jmir.org/2016/1/e14/ 
                         "idu" = 3694.5, # https://pubmed.ncbi.nlm.nih.gov/35791261/ 
                         "heterosexual_male" = 159977, 
                         "heterosexual_female" = 166219)

n.negative.age.restratified = restratify.age.counts(counts = n.negative.age,
                                                    desired.age.brackets = specification.metadata$dim.names$age,
                                                    smooth.infinite.age.to = 101)

n.negative = c(n.negative.total, n.negative.age.restratified, n.negative.race, n.negative.risk)

## N.POSITIVE.UNDIAGNOSED ## 
# https://gis.cdc.gov/grasp/nchhstpatlas/tables.html 
# undiagnosed and diagnosed - diagnosed
# Atlas plus age groups: 13-24, 25-34, 35-44, 45-54, 55+ 
n.positive.undiagnosed.total = c("total" = (1212400-1071005))
n.positive.undiagnosed.age = c("13-24 years" = (41900-28056),
                               "25-34 years" = (217100-163431),
                               "35-44 years" = (238300-203194),
                               "45-54 years" = (263000-243129),
                               "55+ years" = (452000-433195))
n.positive.undiagnosed.race = c("black" = (487500-432227),
                                "hispanic" = (297200-255101),
                                "other" = (342000-306428))
n.positive.undiagnosed.risk = c("msm" = (716900-615019),
                                "idu" = (70400-64530),
                                "heterosexual_male" = (94100-79991),
                                "heterosexual_female" = (212600-190265))

n.positive.undiagnosed.age.restratified = restratify.age.counts(counts = n.positive.undiagnosed.age,
                                                                desired.age.brackets = specification.metadata$dim.names$age,
                                                                smooth.infinite.age.to = 101)   

n.positive.undiagnosed = c(n.positive.undiagnosed.total, n.positive.undiagnosed.age.restratified,
                           n.positive.undiagnosed.race, n.positive.undiagnosed.risk)

## TESTS ## 
# https://www.cdc.gov/hiv/library/reports/testing/2021/index.html, Appendix Table 4 
tests.total = c("total" = 1736850)
tests.age = c("13-19 years" = 88767,
              "20-29 years" = 583150,
              "30-39 years" = 466054,
              "40-49 years" = 257568,
              "50+ years" = 317968)
tests.race = c("black" = 654965,
               "hispanic" = 450209,
               "other" = 479511)
tests.risk = c("msm" = 96643,
               "idu" = 27866,
               "heterosexual_male" = 114364,
               "heterosexual_female" = 128567)

tests.age.restratified = restratify.age.counts(counts = tests.age,
                                               desired.age.brackets = specification.metadata$dim.names$age,
                                               smooth.infinite.age.to = 101) 

tests = c(tests.total, tests.age, tests.race, tests.risk)

## POSITIVE TESTS ## 
# https://www.cdc.gov/hiv/library/reports/testing/2021/index.html, Appendix Table 4 
tests.positive.total = c("total" = 8149)
tests.positive.age = c("13-19 years" = 206,
                       "20-29 years" = 3253,
                       "30-39 years" = 2576,
                       "40-49 years" = 1072,
                       "50+ years" = 876)
tests.positive.race = c("black" = 3621,
                        "hispanic" = 2509,
                        "other" = 1599)
tests.positive.risk = c("msm" = 1873,
                        "idu" = 101,
                        "heterosexual_male" = 389,
                        "heterosexual_female" = 235)

tests.positive.age.restratified = restratify.age.counts(counts = tests.positive.age,
                                                        desired.age.brackets = specification.metadata$dim.names$age,
                                                        smooth.infinite.age.to = 101) 

tests.positive = c(tests.positive.total, tests.positive.age.restratified, tests.positive.race, tests.positive.risk)

## CALCULATE MULTIPLIER ##
# r+/r- = (n-/n+) * (y/1-y)
yield = tests.positive/tests
multiplier = (n.negative/n.positive.undiagnosed)*(yield/(1-yield))

# df = data.frame(value = multiplier,
#                 age = "all",
#                 race = "all",
#                 risk = "all")
# df$age[2:6] = names(multiplier)[2:6]
# df$race[7:9] = names(multiplier)[7:9]
# df$risk[10:13] = names(multiplier)[10:13]
# df$age = factor(df$age, levels = c("all","13-24 years","25-34 years","35-44 years","45-54 years","55+ years"))
# fit = lm(log(value) ~ age + race + risk, data = df)

dim.names = list(age = names(tests.age.restratified),
                 race = names(tests.race),
                 risk = names(tests.risk))

# First array with all of the multipliers multiplied out
full.array = array(multiplier[1],
                   dim = sapply(dim.names, length),
                   dimnames = dim.names)

full.array = sapply(dim.names$risk, function(risk){
  sapply(dim.names$race, function(race){
    sapply(dim.names$age, function(age){
      full.array[age,race,risk]*multiplier[age]*multiplier[race]*multiplier[risk]
    })
  })
})

dim(full.array) = sapply(dim.names,length)
dimnames(full.array) = dim.names

# Second array with only the risk multipliers 
risk.only.array = array(multiplier[1],
                        dim = sapply(dim.names, length),
                        dimnames = dim.names)

risk.only.array = sapply(dim.names$risk, function(risk){
  risk.only.array[,,risk]*multiplier[risk]
})

dim(risk.only.array) = sapply(dim.names,length)
dimnames(risk.only.array) = dim.names

# Average the two arrays 
combined.arrays = array(c(full.array,risk.only.array),
                        dim = c(dim(full.array),2),
                        dimnames = c(dim.names,list(array = c("full","risk.only"))))

averaged.array = apply(combined.arrays,c("age","race","risk"),mean)

averaged.array