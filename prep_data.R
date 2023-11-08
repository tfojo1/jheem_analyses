# PrEP Use ----------------------------

# Function to fit logistic model

logit <- function(p){
  log(p/(1-p))
}
expit <- function(lo){
  exp(lo)/(1+exp(lo))
}

# Function to change age categories to what we need

age_mutate <- function(df){
  df <- df |> 
    dplyr::mutate(
      age1 = age18.24,
      age2 = age25.29 + 0.5*age30.39,
      age3 = 0.5*age30.39 + 0.5*age40.49,
      age4 = age40.49 + 0.5*age50ge,
      age5 = age50ge
    )
  
  return(df)
}

##  MSM -----
p.msm.2017 <- data.frame(
  total = 12.7,
  age18.24 = 5.5,
  age25.29 = 17.6,
  age30.39 = 21.5,
  age40.49 = 13.0,
  age50ge = 13.0, # ages 50 or greater
  black = 14.0,
  hisp = 10.9,
  nbnh = 13.1,
  white = 13.0
)


p.msm.2017 <- age_mutate(p.msm.2017)

p.msm.2017 <- p.msm.2017 / 100

### PrEP Use in 2018 among MSM (American Men's Internet Survey) -------
## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2018-United-States-Report.pdf 

p.msm.2018 <- data.frame(
  total = 13.8,
  age18.24 = 6.3,
  age25.29 = 16.2,
  age30.39 = 22.3,
  age40.49 = 19.9,
  age50ge = 19.9, # ages 50 or greater
  black = 11.6,
  hisp = 12.0,
  nbnh = 14.4,
  white = 14.6
)

p.msm.2018 <- age_mutate(p.msm.2018)

p.msm.2018 <- p.msm.2018 / 100

### PrEP Use in 2019 (AMIS) ======
## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2019-United-States-Report.pdf 
## not used - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8488229/ 

p.msm.2019 <- data.frame( 
  total = 15.2,
  age18.24 = 7.8,
  age25.29 = 21.3,
  age30.39 = 25.4,
  age40.49 = 18.9,
  age50ge = 18.9, # ages 50 or greater
  black = 23.0,
  hisp = 12.8,
  nbnh = 14.6,
  white = 14.5
)

p.msm.2019 <- age_mutate(p.msm.2019)
p.msm.2019 <- p.msm.2019 / 100

# PrEP Use in 2021 among MSM (CDC)
## https://www.cdc.gov/hiv/pdf/library/reports/cdc-hiv-surveillance-special-report-number-31.pdf

p.msm.2021 <- data.frame(
  total = 41.6,
  age18.24 = 39.7,
  age25.29 = 40.6,
  age30.39 = 46.2,
  age40.49 = 42.9,
  age50ge = 33.5, # ages 50 or greater
  black = 24.3,
  hisp = 44.1,
  nbnh = 54.8,
  white = 56.7
)

p.msm.2021 <- age_mutate(p.msm.2021)
p.msm.2021 <- p.msm.2021 / 100

p.msm.black <- c(
  p.msm.2017$black,
  p.msm.2018$black,
  p.msm.2019$black,
  p.msm.2021$black
)

p.msm.hisp <- c(
  p.msm.2017$hisp,
  p.msm.2018$hisp,
  p.msm.2019$hisp,
  p.msm.2021$hisp
)

p.msm.nbnh <- c(
  p.msm.2017$nbnh,
  p.msm.2018$nbnh,
  p.msm.2019$nbnh,
  p.msm.2021$nbnh
)

p.msm.age1 <- c(
  p.msm.2017$age1,
  p.msm.2018$age1,
  p.msm.2019$age1,
  p.msm.2021$age1
)

p.msm.age2 <- c(
  p.msm.2017$age2,
  p.msm.2018$age2,
  p.msm.2019$age2,
  p.msm.2021$age2
)

p.msm.age3 <- c(
  p.msm.2017$age3,
  p.msm.2018$age3,
  p.msm.2019$age3,
  p.msm.2021$age3
)

p.msm.age4 <- c(
  p.msm.2017$age4,
  p.msm.2018$age4,
  p.msm.2019$age4,
  p.msm.2021$age4
)

p.msm.age5 <- c(
  p.msm.2017$age5,
  p.msm.2018$age5,
  p.msm.2019$age5,
  p.msm.2021$age5
)

years <- c(2017:2019,2021) - 2020
p.max <- 0.6

# race
fit.msm.black <- lm(logit(p.msm.black/p.max) ~ years)
fit.msm.hisp <- lm(logit(p.msm.hisp/p.max) ~ years)
fit.msm.nbnh <- lm(logit(p.msm.nbnh/p.max) ~ years)

#age 
fit.msm.age1 <- lm(logit(p.msm.age1/p.max) ~ years)
fit.msm.age2 <- lm(logit(p.msm.age2/p.max) ~ years)
fit.msm.age3 <- lm(logit(p.msm.age3/p.max) ~ years)
fit.msm.age4 <- lm(logit(p.msm.age4/p.max) ~ years)
fit.msm.age5 <- lm(logit(p.msm.age5/p.max) ~ years)

## PWID ------

### PrEP Use in 2015 among PWID (NHBS) ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf

p.idu.2015 <- data.frame(
  total = 0.3,
  male = 0.3,
  female = 0.3,
  age18.24 = 0.5,
  age25.29 = 0.5,
  age30.39 = 0.6,
  age40.49 = 0.3,
  age50ge = 0.1, 
  black = 0.2,
  hisp = 0.4,
  nbnh = 0.2
)

p.idu.2015 <- (age_mutate(p.idu.2015))/100

### PrEP Use in 2017 among PWID (NHBS) -------
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf

p.idu.2017 <- data.frame(
  total = 1.1,
  male = 0.9,
  female = 1.5,
  age18.24 = 1.7,
  age25.29 = 1.1,
  age30.39 = 1.3,
  age40.49 = 1.4,
  age50ge = 0.8, # ages 50 or greater
  black = 0.7,
  hisp = 1.4,
  nbnh = 0.6,
  white = 1.2
)

p.idu.2017 <- age_mutate(p.idu.2017)

p.idu.2017 <- p.idu.2017 / 100

p.idu.black <- c(
  p.idu.2015$black,
  p.idu.2017$black
)

p.idu.hisp <- c(
  p.idu.2015$hisp,
  p.idu.2017$hisp
)

p.idu.nbnh <- c(
  p.idu.2015$nbnh,
  p.idu.2017$nbnh
)

p.idu.age1 <- c(
  p.idu.2015$age1,
  p.idu.2017$age1
)

p.idu.age2 <- c(
  p.idu.2015$age2,
  p.idu.2017$age2
)

p.idu.age3 <- c(
  p.idu.2015$age3,
  p.idu.2017$age3
)

p.idu.age4 <- c(
  p.idu.2015$age4,
  p.idu.2017$age4
)

p.idu.age5 <- c(
  p.idu.2015$age5,
  p.idu.2017$age5
)

p.idu.male <- c(
  p.idu.2015$male,
  p.idu.2017$male
)

p.idu.female <- c(
  p.idu.2015$female,
  p.idu.2017$female
)

years.idu <- c(2015,2017) - 2020

fit.idu.black <- lm(logit(p.idu.black/p.max) ~ years.idu)
fit.idu.hisp <- lm(logit(p.idu.hisp/p.max) ~ years.idu)
fit.idu.nbnh <- lm(logit(p.idu.nbnh/p.max) ~ years.idu)

fit.idu.age1 <- lm(logit(p.idu.age1/p.max) ~ years.idu)
fit.idu.age2 <- lm(logit(p.idu.age2/p.max) ~ years.idu)
fit.idu.age3 <- lm(logit(p.idu.age3/p.max) ~ years.idu)
fit.idu.age4 <- lm(logit(p.idu.age4/p.max) ~ years.idu)
fit.idu.age5 <- lm(logit(p.idu.age5/p.max) ~ years.idu)

fit.idu.hetmale <- lm(logit(p.idu.male/p.max) ~ years.idu)
fit.idu.female <- lm(logit(p.idu.female/p.max) ~ years.idu)

## Heterosexual -----
### PrEP Use in 2016 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf

p.het.2016 <- data.frame(
  total = 0.2,
  male = 0.1,
  female = 0.2,
  age18.24 = 0.1,
  age25.29 = 0.2,
  age30.39 = 0.1,
  age40.49 = 0.1,
  age50ge = 0.2, 
  black = 0.2,
  hisp = 0.2, # this is assumed to be the same rate as black for now
  nbnh = 0.3
)

p.het.2016 <- (age_mutate(p.het.2016))/100


### PrEP Use in 2019 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf

p.het.2019 <- data.frame(
  total = 0.4,
  male = 0.4,
  female = 0.5,
  age18.24 = 0.3,
  age25.29 = 0.4,
  age30.39 = 0.6,
  age40.49 = 0.7,
  age50ge = 0.2, 
  black = 0.5,
  hisp = 0.2,
  nbnh = 0.7
)

p.het.2019 <- (age_mutate(p.het.2019))/100

years.het <- c(2016,2019) - 2020

p.het.black <- c(
  p.het.2016$black,
  p.het.2019$black
)

p.het.hisp <- c(
  p.het.2016$hisp,
  p.het.2019$hisp
)

p.het.nbnh <- c(
  p.het.2016$nbnh,
  p.het.2019$nbnh
)

p.het.age1 <- c(
  p.het.2016$age1,
  p.het.2019$age1
)

p.het.age2 <- c(
  p.het.2016$age2,
  p.het.2019$age2
)

p.het.age3 <- c(
  p.het.2016$age3,
  p.het.2019$age3
)

p.het.age4 <- c(
  p.het.2016$age4,
  p.het.2019$age4
)

p.het.age5 <- c(
  p.het.2016$age5,
  p.het.2019$age5
)

p.het.male <- c(
  p.het.2016$male,
  p.het.2019$male
)

p.het.female <- c(
  p.het.2016$female,
  p.het.2019$female
)

fit.het.black <- lm(logit(p.het.black/p.max) ~ years.het)
fit.het.hisp <- lm(logit(p.het.hisp/p.max) ~ years.het)
fit.het.nbnh <- lm(logit(p.het.nbnh/p.max) ~ years.het)

fit.het.male <- lm(logit(p.het.male/p.max) ~ years.het)
fit.het.female <- lm(logit(p.het.female/p.max) ~ years.het)

fit.het.age1 <- lm(logit(p.het.age1/p.max) ~ years.het)
fit.het.age2 <- lm(logit(p.het.age2/p.max) ~ years.het)
fit.het.age3 <- lm(logit(p.het.age3/p.max) ~ years.het)
fit.het.age4 <- lm(logit(p.het.age4/p.max) ~ years.het)
fit.het.age5 <- lm(logit(p.het.age5/p.max) ~ years.het)

# PrEP Indications ------
## MSM ------

# function to adjust age intervals
pi.age.mutate <- function(df){
  return(df |> 
    dplyr::mutate(
      age1 = age15.24,
      age2 = age25.29 + 0.5*age30.39,
      age3 = 0.5*age30.39 + 0.5*age40ge,
      age4 = age40ge,
      age5 = age40ge
    ))
}

# 2017
## <!-- https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2017-United-States-tables-REV_20171204.pdf -->

pi.msm.2017 <- data.frame(
  total = 19.0,
  age15.24 = 17.7,
  age25.29 = 20.1,
  age30.39 = 21.9,
  age40ge = 18.5, # ages 40 or greater
  black = 25.0,
  hisp = 20.6,
  nbnh = 18.4
)

pi.msm.2017 <- pi.age.mutate(pi.msm.2017) / 100

# 2018
## 2018 : https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2018-United-States-Report.pdf

pi.msm.2018 <- data.frame(
  total = 21.6,
  age15.24 = 19.1,
  age25.29 = 23.0,
  age30.39 = 22.0,
  age40ge = 24.6, # ages 40 or greater
  black = 28.5,
  hisp = 23.6,
  nbnh = 20.8
)

pi.msm.2018 <- pi.age.mutate(pi.msm.2018) / 100

# 2019
## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2019-United-States-Report.pdf 

pi.msm.2019 <- data.frame(
  total = 22.0,
  age15.24 = 19.7,
  age25.29 = 23.3,
  age30.39 = 25.7,
  age40ge = 23.3, # ages 40 or greater
  black = 30.6,
  hisp = 24.9,
  nbnh = 19.9
)

pi.msm.2019 <- pi.age.mutate(pi.msm.2019) / 100

pi.msm.black <- c(
  pi.msm.2017$black,
  pi.msm.2018$black,
  pi.msm.2019$black
)

pi.msm.hisp <- c(
  pi.msm.2017$hisp,
  pi.msm.2018$hisp,
  pi.msm.2019$hisp
)

pi.msm.nbnh <- c(
  pi.msm.2017$nbnh,
  pi.msm.2018$nbnh,
  pi.msm.2019$nbnh
)

pi.msm.age1 <- c(
  pi.msm.2017$age1,
  pi.msm.2018$age1,
  pi.msm.2019$age1
)

pi.msm.age2 <- c(
  pi.msm.2017$age2,
  pi.msm.2018$age2,
  pi.msm.2019$age2
)

pi.msm.age3 <- c(
  pi.msm.2017$age3,
  pi.msm.2018$age3,
  pi.msm.2019$age3
)

pi.msm.age4 <- c(
  pi.msm.2017$age4,
  pi.msm.2018$age4,
  pi.msm.2019$age4
)

pi.msm.age5 <- c(
  pi.msm.2017$age5,
  pi.msm.2018$age5,
  pi.msm.2019$age5
)

years.pi <- c(2017:2019) - 2020
pi.max <- 1

fit.pi.msm.black <- lm(logit(pi.msm.black/pi.max) ~ years.pi)
fit.pi.msm.hisp <- lm(logit(pi.msm.hisp/pi.max) ~ years.pi)
fit.pi.msm.nbnh <- lm(logit(pi.msm.nbnh/pi.max) ~ years.pi)

fit.pi.msm.age1 <- lm(logit(pi.msm.age1/pi.max) ~ years.pi)
fit.pi.msm.age2 <- lm(logit(pi.msm.age2/pi.max) ~ years.pi)
fit.pi.msm.age3 <- lm(logit(pi.msm.age3/pi.max) ~ years.pi)
fit.pi.msm.age4 <- lm(logit(pi.msm.age4/pi.max) ~ years.pi)
fit.pi.msm.age5 <- lm(logit(pi.msm.age5/pi.max) ~ years.pi)
