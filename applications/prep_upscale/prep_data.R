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
  p.msm.2019$black
  # p.msm.2021$black
)

p.msm.hisp <- c(
  p.msm.2017$hisp,
  p.msm.2018$hisp,
  p.msm.2019$hisp
  # p.msm.2021$hisp
)

p.msm.nbnh <- c(
  p.msm.2017$nbnh,
  p.msm.2018$nbnh,
  p.msm.2019$nbnh
  # p.msm.2021$nbnh
)

p.msm.age1 <- c(
  p.msm.2017$age1,
  p.msm.2018$age1,
  p.msm.2019$age1
  # p.msm.2021$age1
)

p.msm.age2 <- c(
  p.msm.2017$age2,
  p.msm.2018$age2,
  p.msm.2019$age2
  # p.msm.2021$age2
)

p.msm.age3 <- c(
  p.msm.2017$age3,
  p.msm.2018$age3,
  p.msm.2019$age3
  # p.msm.2021$age3
)

p.msm.age4 <- c(
  p.msm.2017$age4,
  p.msm.2018$age4,
  p.msm.2019$age4
  # p.msm.2021$age4
)

p.msm.age5 <- c(
  p.msm.2017$age5,
  p.msm.2018$age5,
  p.msm.2019$age5
  # p.msm.2021$age5
)

p.msm.total <- c(
  p.msm.2017$total,
  p.msm.2018$total,
  p.msm.2019$total
)


#### creating a combined model for msm -----
p.msm.df <- data.frame(
  year = c(2017:2019),
  black = p.msm.black,
  hisp = p.msm.hisp,
  nbnh = p.msm.nbnh,
  age1 = p.msm.age1,
  age2 = p.msm.age2,
  age3 = p.msm.age3,
  age4 = p.msm.age4,
  age5 = p.msm.age5,
  total = p.msm.total
)

# p.max and anchor year ----------
p.max <- 0.6
anchor.year <- 2009

library(tidyr)
p.msm.df.long <- pivot_longer(p.msm.df, cols = c(black, hisp, nbnh, age1, age2, age3, age4, age5, total),
                              names_to = "variable", values_to = "p")

p.msm.df.long$raceid <- ifelse(grepl("black", p.msm.df.long$variable), "black",
                               ifelse(grepl("hisp", p.msm.df.long$variable), "hispanic", 
                                      ifelse(grepl("nbnh", p.msm.df.long$variable), "other", "ALL")))
p.msm.df.long$ageid <- ifelse(grepl("age1", p.msm.df.long$variable), "age1",
                              ifelse(grepl("age2", p.msm.df.long$variable), "age2",
                                     ifelse(grepl("age3", p.msm.df.long$variable), "age3",
                                            ifelse(grepl("age4", p.msm.df.long$variable), "age4",
                                                   ifelse(grepl("age5", p.msm.df.long$variable), "age5", "ALL")))))
p.msm.df.long$risk <- rep("msm", length(p.msm.df.long$raceid))
p.msm.df.long$year <- p.msm.df.long$year - anchor.year

fit.p.msm <- lm(logit(p.msm.df.long$p/p.max) ~ year + factor(raceid) + factor(ageid), 
                data = p.msm.df.long)

# years <- c(2017:2019,2021) - 2020
# p.max <- 0.6
# 
# # race
# fit.msm.black <- lm(logit(p.msm.black/p.max) ~ years)
# fit.msm.hisp <- lm(logit(p.msm.hisp/p.max) ~ years)
# fit.msm.nbnh <- lm(logit(p.msm.nbnh/p.max) ~ years)
# 
# #age 
# fit.msm.age1 <- lm(logit(p.msm.age1/p.max) ~ years)
# fit.msm.age2 <- lm(logit(p.msm.age2/p.max) ~ years)
# fit.msm.age3 <- lm(logit(p.msm.age3/p.max) ~ years)
# fit.msm.age4 <- lm(logit(p.msm.age4/p.max) ~ years)
# fit.msm.age5 <- lm(logit(p.msm.age5/p.max) ~ years)

## PWID ------

### PrEP Use in 2015 among PWID (NHBS) ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf

# numerator: # took PrEP
# denominator: # any receptive sharing

p.idu.2015 <- data.frame(
  total = 33/5867,
  male = 24/4169,
  female = 8/1677,
  age18.24 = 3/440,
  age25.29 = 6/837,
  age30.39 = 13/1588,
  age40.49 = 6/1285,
  age50ge = 5/1717, 
  black = 7/1586,
  hisp = 9/1320,
  nbnh = 17/(64+19+10+2620+240)
)

p.idu.2015 <- (age_mutate(p.idu.2015))

### PrEP Use in 2017 among PWID (NHBS) -------
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf

# numerator: # took PrEP
# denominator: # any receptive sharing

p.idu.2017 <- data.frame(
  total = 120/6350,
  male = 65/4287,
  female = 49/2015,
  age18.24 = 7/290,
  age25.29 = 13/812,
  age30.39 = 37/1922,
  age40.49 = 33/1448,
  age50ge = 30/1878, 
  black = 25/1698,
  hisp = 30/1255,
  nbnh = (3+1+53+8)/(77+20+5+2927+364)
)

p.idu.2017 <- age_mutate(p.idu.2017)

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


#### big model for IDU -----

p.idu.df <- data.frame(
  year = c(2015,2017),
  p.idu.black,
  p.idu.hisp,
  p.idu.nbnh,
  p.idu.age1,
  p.idu.age2,
  p.idu.age3,
  p.idu.age4,
  p.idu.age5,
  p.idu.male,
  p.idu.female
)

p.idu.df.long <- gather(p.idu.df, key = "group", value = "p", -year)
p.idu.df.long$year <- p.idu.df.long$year - anchor.year

p.idu.df.long$raceid <- ifelse(p.idu.df.long$group == "p.idu.black", "black", 
                               ifelse(p.idu.df.long$group == "p.idu.hisp", "hispanic", 
                                      ifelse(p.idu.df.long$group == "p.idu.nbnh", "other", "ALL")))
p.idu.df.long$ageid <- ifelse(p.idu.df.long$group == "p.idu.age1", "age1", 
                              ifelse(p.idu.df.long$group == "p.idu.age2", "age2", 
                                     ifelse(p.idu.df.long$group == "p.idu.age3", "age3", 
                                            ifelse(p.idu.df.long$group == "p.idu.age4", "age4", 
                                                   ifelse(p.idu.df.long$group == "p.idu.age5", "age5", "ALL")))))
p.idu.df.long$sexid <- ifelse(p.idu.df.long$group == "p.idu.male", "male",
                              ifelse(p.idu.df.long$group == "p.idu.female", "female", "ALL"))

fit.p.idu <- lm(logit(p) ~ year + raceid + ageid + sexid, data = p.idu.df.long)


## Heterosexual -----
### PrEP Use in 2016 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf

# denominator: any STI

p.het.2016 <- data.frame(
  total = 11/507,
  male = 2/176,
  female = 9/331,
  age18.24 = 2/173,
  age25.29 = 2/114,
  age30.39 = 2/86,
  age40.49 = 2/68,
  age50ge = 3/66, 
  black = 9/415,
  hisp = 1/55,  # there were 0 people who took PrEP this year among the hispanic ethnicity category. This threw an Inf error in the fit and so I've changed it to 1 for now.
  nbnh = 2/(4+3+1+8+20)
)

p.het.2016 <- (age_mutate(p.het.2016))


### PrEP Use in 2019 ----
## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf

# denominator: any STI

p.het.2019 <- data.frame(
  total = 42/658,
  male = 18/218,
  female = 24/440,
  age18.24 = 5/216,
  age25.29 = 6/120,
  age30.39 = 14/143,
  age40.49 = 12/86,
  age50ge = 5/93, 
  black = 32/499,
  hisp = 4/102,
  nbnh = (1+2+3)/(6+2+1+14+34)
)

p.het.2019 <- (age_mutate(p.het.2019))

years.het <- c(2016,2019) - anchor.year

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

p.het.df <- data.frame(
  years.het,
  p.het.black,
  p.het.hisp,
  p.het.nbnh,
  p.het.age1,
  p.het.age2,
  p.het.age3,
  p.het.age4,
  p.het.age5,
  p.het.male,
  p.het.female
)

#### het model -----
p.het.df.long <- gather(p.het.df, key = "group", value = "p", -years.het)
p.het.df.long$year <- p.het.df.long$years.het 
p.het.df.long <- p.het.df.long[,-1]

p.het.df.long$raceid <- ifelse(p.het.df.long$group == "p.het.black", "black", 
                               ifelse(p.het.df.long$group == "p.het.hisp", "hispanic", 
                                      ifelse(p.het.df.long$group == "p.het.nbnh", "other", "ALL")))
p.het.df.long$ageid <- ifelse(p.het.df.long$group == "p.het.age1", "age1", 
                              ifelse(p.het.df.long$group == "p.het.age2", "age2", 
                                     ifelse(p.het.df.long$group == "p.het.age3", "age3", 
                                            ifelse(p.het.df.long$group == "p.het.age4", "age4", 
                                                   ifelse(p.het.df.long$group == "p.het.age5", "age5", "ALL")))))
p.het.df.long$sexid <- ifelse(p.het.df.long$group == "p.het.female", "female",
                              ifelse(p.het.df.long$group == "p.het.male", "male", "ALL"))

fit.p.het <- lm(logit(p/p.max) ~ year + raceid + ageid + sexid, data = p.het.df.long) 

# fit.het.black <- lm(logit(p.het.black/p.max) ~ years.het)
# fit.het.hisp <- lm(logit(p.het.hisp/p.max) ~ years.het)
# fit.het.nbnh <- lm(logit(p.het.nbnh/p.max) ~ years.het)
# 
# fit.het.male <- lm(logit(p.het.male/p.max) ~ years.het)
# fit.het.female <- lm(logit(p.het.female/p.max) ~ years.het)
# 
# fit.het.age1 <- lm(logit(p.het.age1/p.max) ~ years.het)
# fit.het.age2 <- lm(logit(p.het.age2/p.max) ~ years.het)
# fit.het.age3 <- lm(logit(p.het.age3/p.max) ~ years.het)
# fit.het.age4 <- lm(logit(p.het.age4/p.max) ~ years.het)
# fit.het.age5 <- lm(logit(p.het.age5/p.max) ~ years.het)

# One big model (PrEP Use) ------

p.het.df.long <- p.het.df.long |> dplyr::select(-group) |> 
  dplyr::mutate(risk = rep("het", length(p.het.df.long$ageid)))
p.msm.df.long <- p.msm.df.long |> dplyr::select(-variable) |> 
  dplyr::mutate(sexid = rep("msm", length(p.msm.df.long$ageid)), 
                risk = rep("msm", length(p.msm.df.long$ageid)))
p.idu.df.long <- p.idu.df.long |> dplyr::select(-group) |> 
  dplyr::mutate(risk = rep("idu", length(p.idu.df.long$ageid)))

big.df <- rbind(p.idu.df.long, p.het.df.long, p.msm.df.long)

big.df$raceid <- relevel(factor(big.df$raceid), ref = "ALL")
big.df$ageid <- relevel(factor(big.df$ageid), ref = "ALL")
big.df$sexid <- relevel(factor(big.df$sexid), ref = "ALL")
big.df$risk <- relevel(factor(big.df$risk), ref = "msm")
big.df$sexid[big.df$sexid=="msm"] <- "male"

big.df$female <- as.numeric(big.df$sexid=="female")

big.df$nonmsm <- as.numeric(big.df$risk!="msm")
big.df$idu <- as.numeric(big.df$risk=="idu")


# making 2 separate big models -- nonmsm and msm -------

msm.bigp.df <- p.msm.df.long
msm.bigp.df$ageid <- relevel(factor(msm.bigp.df$ageid), ref = "ALL")
msm.bigp.df$raceid <- relevel(factor(msm.bigp.df$raceid), ref="ALL")
nonmsm.big.df <- subset(big.df, nonmsm == 1)

idu.big.df <- subset(big.df, risk == "idu")
het.big.df <- subset(big.df, risk == "het")

# fit.p.msm <- lm(logit(p/p.max) ~ year + raceid + ageid,
#                 data = msm.bigp.df)

fit.p.msm <- lm((p) ~ year + raceid + ageid,
                data = msm.bigp.df)
fit.p.msm

fit.p.nonmsm <- lm((p) ~ year + raceid + ageid + female + idu,
                   data = nonmsm.big.df)

fit.p.idu <- lm(p ~ year + raceid + ageid + female, data = idu.big.df)
fit.p.het <- lm(p ~ year + raceid + ageid + female, data = het.big.df)


# 
# create.logistic.tail.functional.form(
#   # everything else the same
#   
#   logistic.after.frac.of.span = 0.5,
#   )


# PrEP Indications ------
anchor.year.pi <- 2009
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

### CDC surveillance special report data ----
# indicator: condomless anal sex with casual male partner

#https://www.cdc.gov/hiv/library/reports/hiv-surveillance-special-reports/no-31/index.html

pi.msm.cdc.2021 <- data.frame(
  total = 53.4,
  age18.24 = 40.1,
  age25.29 = 50.9,
  age30.39 = 56.2,
  age40.49 = 58.1,
  age50ge = 52.0,
  black = 40.4,
  hisp = 59.4,
  nbnh = (3+66+7+390+57)/(4+110+15+597+111)*100
)

pi.msm.cdc.2021 <- age_mutate(pi.msm.cdc.2021)/100

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf 

pi.msm.cdc.2017 <- data.frame(
  total = 46.7,
  age18.24 = 43.6,
  age25.29 = 51.5,
  age30.39 = 50.1,
  age40.49 = 44.4,
  age50ge = 36.7,
  black = 39.5,
  hisp = 45.3,
  nbnh = (14+90+18+1437+177)/(49+192+32+2774+365)*100
)

pi.msm.cdc.2017 <- age_mutate(pi.msm.cdc.2017)/100

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-15.pdf

pi.msm.cdc.2014 <- data.frame(
  total = 36.8,
  age18.24 = 33.5,
  age25.29 = 40.2,
  age30.39 = 38.9,
  age40.49 = 38.3,
  age50ge = 30.7,
  black = 33.1,
  hisp = 37.1,
  nbnh = (13+54+15+1076+121)/(43+144+37+2796+319)*100
)

pi.msm.cdc.2014 <- age_mutate(pi.msm.cdc.2014)/100

pi.msm.cdc.black <- c(
  pi.msm.cdc.2014$black,
  pi.msm.cdc.2017$black,
  pi.msm.cdc.2021$black
)

pi.msm.cdc.hisp <- c(
  pi.msm.cdc.2014$hisp,
  pi.msm.cdc.2017$hisp,
  pi.msm.cdc.2021$hisp
)

pi.msm.cdc.nbnh <- c(
  pi.msm.cdc.2014$nbnh,
  pi.msm.cdc.2017$nbnh,
  pi.msm.cdc.2021$nbnh
)

pi.msm.cdc.age1 <- c(
  pi.msm.cdc.2014$age1,
  pi.msm.cdc.2017$age1,
  pi.msm.cdc.2021$age1
)

pi.msm.cdc.age2 <- c(
  pi.msm.cdc.2014$age2,
  pi.msm.cdc.2017$age2,
  pi.msm.cdc.2021$age2
)

pi.msm.cdc.age3 <- c(
  pi.msm.cdc.2014$age3,
  pi.msm.cdc.2017$age3,
  pi.msm.cdc.2021$age3
)

pi.msm.cdc.age4 <- c(
  pi.msm.cdc.2014$age4,
  pi.msm.cdc.2017$age4,
  pi.msm.cdc.2021$age4
)

pi.msm.cdc.age5 <- c(
  pi.msm.cdc.2014$age5,
  pi.msm.cdc.2017$age5,
  pi.msm.cdc.2021$age5
)

pi.msm.cdc.total <- c(
  pi.msm.cdc.2014$total,
  pi.msm.cdc.2017$total,
  pi.msm.cdc.2021$total
)

years.pi.cdc <- c(2014, 2017, 2021) - anchor.year.pi

### AMIS -----

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

pi.msm.total <- c(
  pi.msm.2017$total,
  pi.msm.2018$total,
  pi.msm.2019$total
)

years.pi <- c(2017:2019) - anchor.year.pi
# pi.max <- 1

# fit.pi.msm.black <- lm(logit(pi.msm.black/pi.max) ~ years.pi)
# fit.pi.msm.hisp <- lm(logit(pi.msm.hisp/pi.max) ~ years.pi)
# fit.pi.msm.nbnh <- lm(logit(pi.msm.nbnh/pi.max) ~ years.pi)
# 
# fit.pi.msm.age1 <- lm(logit(pi.msm.age1/pi.max) ~ years.pi)
# fit.pi.msm.age2 <- lm(logit(pi.msm.age2/pi.max) ~ years.pi)
# fit.pi.msm.age3 <- lm(logit(pi.msm.age3/pi.max) ~ years.pi)
# fit.pi.msm.age4 <- lm(logit(pi.msm.age4/pi.max) ~ years.pi)
# fit.pi.msm.age5 <- lm(logit(pi.msm.age5/pi.max) ~ years.pi)

## IDU ------

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf 

# criteria - any receptive sharing of injection equipment

# https://www.cdc.gov/mmwr/preview/mmwrhtml/ss6306a1.htm 
pi.idu.2009 <- data.frame(
  total = (4249+1696)/(6992+2660),
  male = 4249/6992,
  female = 1696/2660,
  age1 = 745/995,
  age2 = 0.5*((1174/1760)+(1802/2961)),
  age3 = 0.5*((1802/2961)+(1916/3303)),
  age4 = 0.5*((1916/3303)+(308/633)),
  age5 = 308/633,
  black = 2544/4436,
  hisp = 1312/2095,
  nbnh = (56+25+1816+182)/(88+39+2673+306)
)

# pi.idu.2012 <- pi.idu.2012/100

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-11.pdf 
pi.idu.2012 <- data.frame(
  total = 60.6,
  male = 60.0,
  female = 62.3,
  age1 = 74.2,
  age2 = (0.5*475 + 0.5*1194)/(0.5*639+0.5*1759)*100,
  age3 = (1194+1438)/(1759+2431)*100,
  age4 = (1438+2143)/(2431+3910)*100,
  age5 = 54.8,
  black = 54.7,
  hisp = 61.6,
  nbnh = (54+14+10+1753+199)/(84+27+15+2550+323)*100
)

pi.idu.2012 <- pi.idu.2012/100

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf

pi.idu.2015 <- data.frame(
  total = 60.6,
  male = 60.0,
  female = 62.5,
  age1 = 76.1,
  age2 = (837+0.5*1588)/(1148+0.5*2317)*100,
  age3 = (0.5*1588+0.5*1285)/(0.5*2317+0.5*2159)*100,
  age4 = (0.5*1285+0.5*1717)/(0.5*2159+0.5*3473)*100,
  age5 = 49.4,
  black = 50.6,
  hisp = 61.1,
  nbnh = (64+19+10+2620+240)/(99+28+15+3837+380)*100
)
  
pi.idu.2015 <- pi.idu.2015/100

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf 

pi.idu.2018 <- data.frame(
  total = 59.8,
  male = 58.5,
  female = 62.6,
  age18.24 = 72.0,
  age25.29 = (812+0.5*1922)/(1152+0.5*2861)*100,
  age30.39 = (0.5*1922+0.5*1448)/(0.5*2861+0.5*2430)*100,
  age40.49 = (0.5*1448+0.5*1878)/(0.5*2430+0.5*3771)*100,
  age50ge = 49.8,
  black = 49.8,
  hisp = 57.8,
  nbnh = 67.4
)

pi.idu.2018 <- pi.idu.2018/100

years.idu <- c(2015, 2018) - anchor.year.pi

pi.idu.total <- c(
  pi.idu.2009$total,
  pi.idu.2012$total,
  pi.idu.2015$total,
  pi.idu.2018$total
)

pi.idu.black <- c(
  pi.idu.2009$black,
  pi.idu.2012$black,
  pi.idu.2015$black,
  pi.idu.2018$black
)

pi.idu.hisp <- c(
  pi.idu.2009$hisp,
  pi.idu.2012$hisp,
  pi.idu.2015$hisp,
  pi.idu.2018$hisp
)

pi.idu.nbnh <- c(
  pi.idu.2009$nbnh,
  pi.idu.2012$nbnh,
  pi.idu.2015$nbnh,
  pi.idu.2018$nbnh
)

pi.idu.age1 <- c(
  pi.idu.2009$age1,
  pi.idu.2012$age1,
  pi.idu.2015$age1,
  pi.idu.2018$age1
)

pi.idu.age2 <- c(
  pi.idu.2009$age2,
  pi.idu.2012$age2,
  pi.idu.2015$age2,
  pi.idu.2018$age2
)

pi.idu.age3 <- c(
  pi.idu.2009$age3,
  pi.idu.2012$age3,
  pi.idu.2015$age3,
  pi.idu.2018$age3
)

pi.idu.age4 <- c(
  pi.idu.2009$age4,
  pi.idu.2012$age4,
  pi.idu.2015$age4,
  pi.idu.2018$age4
)

pi.idu.age5 <- c(
  pi.idu.2009$age5,
  pi.idu.2012$age5,
  pi.idu.2015$age5,
  pi.idu.2018$age5
)

pi.idu.male <- c(
  pi.idu.2009$male,
  pi.idu.2012$male,
  pi.idu.2015$male,
  pi.idu.2018$male
)

pi.idu.female <- c(
  pi.idu.2009$female,
  pi.idu.2012$female,
  pi.idu.2015$female,
  pi.idu.2018$female
)

years.pi.idu <- c(2009, 2012, 2015, 2018) - anchor.year.pi

## Het -------

# criteria - condomless sex with casual partners

# https://www.cdc.gov/mmwr/preview/mmwrhtml/ss6314a1.htm

pi.het.f.2010 <- data.frame(
  total = (167+366+288+428+563+349)/(452+916+699+943+1197+748),
  age18.24 = (167+366)/(452+916),
  age25.29 = 288/699,
  age30.39 = 428/943,
  age40.49 = 563/1197,
  age50ge  = 349/748,
  black = 1561/3530,
  hisp = 449/1034,
  nbnh = (10+21+54+64)/(32+64+132+157)
)

pi.het.f.2010 <- age_mutate(pi.het.f.2010)

pi.het.m.2010 <- data.frame(
  total = (190+356+259+397+600+477)/(377+732+484+757+1128+845),
  age18.24 = (190+356)/(377+732),
  age25.29 = 259/484,
  age30.39 = 397/757,
  age40.49 = 600/1128,
  age50ge  = 477/845,
  black = 1616/3105,
  hisp = 479/887,
  nbnh = (11+17+86+67)/(24+44+134+125)
)

pi.het.m.2010 <- age_mutate(pi.het.m.2010)

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-13.pdf

pi.het.f.2013 <- data.frame(
  total = 41.9,
  age18.24 = 37.4,
  age25.29 = 39.0,
  age30.39 = 42.9,
  age40.49 = 47.1,
  age50ge = 42.5,
  black = 43.0,
  hisp = 34.9,
  nbnh = (15+5+3+53+61)/(33+6+8+113+140)*100
)

pi.het.f.2013 <- age_mutate(pi.het.f.2013)/100

pi.het.m.2013 <- data.frame(
  total = 49.9,
  age18.24 = 47.5,
  age25.29 = 50.6,
  age30.39 = 50.4,
  age40.49 = 52.7,
  age50ge = 49.1,
  black = 50.8,
  hisp = 45.7,
  nbnh = (15+6+6+59+46)/(26+10+14+104+105)*100
)

pi.het.m.2013 <- age_mutate(pi.het.m.2013)/100

# 2016 
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf

pi.het.f.2016 <- data.frame(
  total = 40.1,
  age18.24 = 35.2,
  age25.29 = 40.6,
  age30.39 = 40.4,
  age40.49 = 42.5,
  age50ge = 41.7,
  black = 39.4,
  hisp = 42.2,
  nbnh = (16+6+4+55+56)/(28+9+7+143+149)*100
)

pi.het.f.2016 <- age_mutate(pi.het.f.2016)/100

pi.het.m.2016 <- data.frame(
  total = 44.8,
  age18.24 = 44.1,
  age25.29 = 42.5,
  age30.39 = 44.9,
  age40.49 = 46.4,
  age50ge = 45.6,
  black = 43.6,
  hisp = 48.9,
  nbnh = (11+3+9+35+63)/(24+3+14+78+133)*100
)

pi.het.m.2016 <- age_mutate(pi.het.m.2016)/100

# criteria - any STI
# 
# pi.het.2016 <- data.frame(
#   total = 6.9,
#   male = 5.4,
#   female = 8.2,
#   age18.24 = 11.4,
#   age25.29 = 10.7,
#   age30.39 = 5.6,
#   age40.49 = 4.7,
#   age50ge = 3.8,
#   black = 7.9,
#   hisp = 3.7,
#   nbnh = (4+3+1+8+20)/(52+12+21+221+282)*100
# )

# pi.het.2016 <- age_mutate(pi.het.2016)/100

# 2019 
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf

pi.het.f.2019 <- data.frame(
  total = 39.0,
  age18.24 = 35.4,
  age25.29 = 39.9,
  age30.39 = 37.2,
  age40.49 = 42.9,
  age50ge = 40.2,
  black = 38.8,
  hisp = 38.7,
  nbnh = (15+1+4+106+84)/(34+9+18+260+194)*100
)

pi.het.f.2019 <- age_mutate(pi.het.f.2019)/100

pi.het.m.2019 <- data.frame(
  total = 42.4,
  age18.24 = 44.9,
  age25.29 = 42.4,
  age30.39 = 42.6,
  age40.49 = 40.8,
  age50ge = 41.3,
  black = 42.6,
  hisp = 41.8,
  nbnh = (14+2+7+65+64)/(24+8+15+153+168)*100
)

pi.het.m.2019 <- age_mutate(pi.het.m.2019)/100

# criteria - condomless sex with casual partner
# pi.het.2019 <- data.frame(
#   total = 7.0,
#   male = 5.2,
#   female = 8.5,
#   age18.24 = 11.8,
#   age25.29 = 8.9,
#   age30.39 = 6.3,
#   age40.49 = 4.9,
#   age50ge = 4.3,
#   black = 7.8,
#   hisp = 4.9,
#   nbnh = (6+2+1+14+34)/(58+17+33+413+362)*100
# )
# 
# pi.het.2019 <- age_mutate(pi.het.2019)/100

# criteria - any STI
# pi.het.2019 <- data.frame(
#   total = 7.0,
#   male = 5.2,
#   female = 8.5,
#   age18.24 = 11.8,
#   age25.29 = 8.9,
#   age30.39 = 6.3,
#   age40.49 = 4.9,
#   age50ge = 4.3,
#   black = 7.8,
#   hisp = 4.9,
#   nbnh = (6+2+1+14+34)/(58+17+33+413+362)*100
# )
# 
# pi.het.2019 <- age_mutate(pi.het.2019)/100

pi.het.total <- c(
  pi.het.m.2010$total,
  pi.het.f.2010$total,
  pi.het.m.2013$total,
  pi.het.f.2013$total,
  pi.het.m.2016$total,
  pi.het.f.2016$total,
  pi.het.m.2019$total,
  pi.het.f.2019$total
)

pi.het.black <- c(
  pi.het.m.2010$black,
  pi.het.f.2010$black,
  pi.het.m.2013$black,
  pi.het.f.2013$black,
  pi.het.m.2016$black,
  pi.het.f.2016$black,
  pi.het.m.2019$black,
  pi.het.f.2019$black
)

pi.het.hisp <- c(
  pi.het.m.2010$hisp,
  pi.het.f.2010$hisp,
  pi.het.m.2013$hisp,
  pi.het.f.2013$hisp,
  pi.het.m.2016$hisp,
  pi.het.f.2016$hisp,
  pi.het.m.2019$hisp,
  pi.het.f.2019$hisp
)

pi.het.nbnh <- c(
  pi.het.m.2010$nbnh,
  pi.het.f.2010$nbnh,
  pi.het.m.2013$nbnh,
  pi.het.f.2013$nbnh,
  pi.het.m.2016$nbnh,
  pi.het.f.2016$nbnh,
  pi.het.m.2019$nbnh,
  pi.het.f.2019$nbnh
)

pi.het.age1 <- c(
  pi.het.m.2010$age1,
  pi.het.f.2010$age1,
  pi.het.m.2013$age1,
  pi.het.f.2013$age1,
  pi.het.m.2016$age1,
  pi.het.f.2016$age1,
  pi.het.m.2019$age1,
  pi.het.f.2019$age1
)

pi.het.age2 <- c(
  pi.het.m.2010$age2,
  pi.het.f.2010$age2,
  pi.het.m.2013$age2,
  pi.het.f.2013$age2,
  pi.het.m.2016$age2,
  pi.het.f.2016$age2,
  pi.het.m.2019$age2,
  pi.het.f.2019$age2
)

pi.het.age3 <- c(
  pi.het.m.2010$age3,
  pi.het.f.2010$age3,
  pi.het.m.2013$age3,
  pi.het.f.2013$age3,
  pi.het.m.2016$age3,
  pi.het.f.2016$age3,
  pi.het.m.2019$age3,
  pi.het.f.2019$age3
)

pi.het.age4 <- c(
  pi.het.m.2010$age4,
  pi.het.f.2010$age4,
  pi.het.m.2013$age4,
  pi.het.f.2013$age4,
  pi.het.m.2016$age4,
  pi.het.f.2016$age4,
  pi.het.m.2019$age4,
  pi.het.f.2019$age4
)

pi.het.age5 <- c(
  pi.het.m.2010$age5,
  pi.het.f.2010$age5,
  pi.het.m.2013$age5,
  pi.het.f.2013$age5,
  pi.het.m.2016$age5,
  pi.het.f.2016$age5,
  pi.het.m.2019$age5,
  pi.het.f.2019$age5
)

pi.het.sex <- c(
  "male",
  "female",
  "male",
  "female"
)

pi.years.het <- c(2010, 2010, 2013, 2013, 2016, 2016, 2019, 2019) - anchor.year.pi

# pi.het.male <- c(
#   pi.het.2016$male,
#   pi.het.2019$male
# )
# 
# pi.het.female <- c(
#   pi.het.2016$female,
#   pi.het.2019$female
# )
# 

## formatting data ------

pi.msm.cdc.df <- data.frame(
  years = years.pi.cdc,
  black = pi.msm.cdc.black,
  hisp = pi.msm.cdc.hisp,
  nbnh = pi.msm.cdc.nbnh,
  age1 = pi.msm.cdc.age1,
  age2 = pi.msm.cdc.age2,
  age3 = pi.msm.cdc.age3,
  age4 = pi.msm.cdc.age4,
  age5 = pi.msm.cdc.age5,
  total = pi.msm.cdc.total
) 

pi.msm.amis.df <- data.frame(
  years = years.pi,
  black = pi.msm.black,
  hisp = pi.msm.hisp,
  nbnh = pi.msm.nbnh,
  age1 = pi.msm.age1,
  age2 = pi.msm.age2,
  age3 = pi.msm.age3,
  age4 = pi.msm.age4,
  age5 = pi.msm.age5,
  total = pi.msm.total
) 

pi.ratio.msm <- (pi.msm.amis.df/pi.msm.cdc.df) %>% select(-years)

pi.ratio.df <- pi.ratio.msm %>%
  summarize(across(everything(), mean))

pi.idu.df <- data.frame(
  years = years.pi.idu,
  black = pi.idu.black,
  hisp = pi.idu.hisp,
  nbnh = pi.idu.nbnh,
  age1 = pi.idu.age1,
  age2 = pi.idu.age2,
  age3 = pi.idu.age3,
  age4 = pi.idu.age4,
  age5 = pi.idu.age5,
  male = pi.idu.male,
  female = pi.idu.female,
  total = pi.idu.total
)

pi.het.df <- data.frame(
  years = pi.years.het,
  black = pi.het.black,
  hisp = pi.het.hisp,
  nbnh = pi.het.nbnh,
  age1 = pi.het.age1,
  age2 = pi.het.age2,
  age3 = pi.het.age3,
  age4= pi.het.age4,
  age5 = pi.het.age5,
  total = pi.het.total
  # male = pi.het.male,
  # female = pi.het.female
)

# multiplying it by the amis/cdc proportion of PrEP indication

pi.het.df <- pi.het.df %>%
  select(-years) %>%
  mutate(across(everything(), ~ . * pi.ratio.df[[cur_column()]])) %>%
  mutate(years = pi.years.het) %>% 
  select(years, everything())

pi.msm.cdc.long <- gather(pi.msm.cdc.df, key = "group", value = "pi", -years)
pi.msm.cdc.long$sexid <- rep("msm", length(pi.msm.cdc.long$pi))
pi.msm.cdc.long$riskid <- rep("msm", length(pi.msm.cdc.long$pi))
pi.msm.cdc.long$dataid <- rep("cdc", length(pi.msm.cdc.long$pi))

pi.msm.amis.long <- gather(pi.msm.amis.df, key = "group", value = "pi", -years)
pi.msm.amis.long$sexid <- rep("msm", length(pi.msm.amis.long$pi))
pi.msm.amis.long$riskid <- rep("msm", length(pi.msm.amis.long$pi))
pi.msm.amis.long$dataid <- rep("amis", length(pi.msm.amis.long$pi))

# pi.msm.combined <- pi.msm.cdc.long

pi.msm.combined <- rbind(pi.msm.cdc.long, pi.msm.amis.long)

pi.idu.df.long <- gather(pi.idu.df, key = "group", value = "pi", -years)
pi.idu.df.long$riskid <- rep("idu", length(pi.idu.df.long$pi))
pi.idu.df.long$sexid <- rep("idu", length(pi.idu.df.long$pi))
pi.idu.df.long$dataid <- rep("cdc", length(pi.idu.df.long$pi))

pi.het.df.long <- gather(pi.het.df, key = "group", value = "pi", -years)
pi.het.df.long$riskid <- rep("het", length(pi.het.df.long$pi))
pi.het.df.long$sexid <- rep(c("het-male","het-female"), 
                            length(pi.het.df.long$pi)/2)
pi.het.df.long$dataid <- rep("cdc", length(pi.het.df.long$pi))
# pi.het.df.long$sexid <- rep("het", length(pi.het.df.long$pi))

# pi.df.long <- rbind(pi.msm.df.long, pi.idu.df.long, pi.het.df.long)
pi.df.long <- rbind(pi.msm.combined, pi.idu.df.long, pi.het.df.long)

pi.big.df <- pi.df.long |> dplyr::mutate(raceid = ifelse(group == "black", "black", 
                                                   ifelse(group == "hisp", "hisp", 
                                                          ifelse(group == "nbnh", "nbnh", "ALL"))),
                                          ageid = ifelse(group == "age1", "age1", 
                                                   ifelse(group == "age2", "age2", 
                                                          ifelse(group == "age3", "age3", 
                                                                 ifelse(group == "age4", "age4", 
                                                                        ifelse(group == "age5", "age5", "ALL"))))),
                                          sexid = ifelse(sexid == "msm", "msm",
                                                         ifelse(sexid == "het-male", "het-male",
                                                            ifelse(sexid == "het-female", "het-female",
                                                              ifelse(group == "male", "male",
                                                                ifelse(group == "female", "female",
                                                                    "ALL")))) ))
                                                              
                                                         

# pi.df.long$sexid <- rep("msm", nrow(pi.df.long))
# # pi.df.long$riskid <- rep("msm", nrow(pi.df.long))
# pi.big.df <- pi.df.long |> dplyr::mutate(sexid = ifelse(group == "male", "male",
#                                                          ifelse(group == "female", "female", "msm")))

## big model PrEP indication ----------------------------------------------------


# pi.big.df$sexrisk <- paste(pi.big.df$sexid, pi.big.df$risk, sep = "_")
# pi.big.df$sexrisk <- ifelse(pi.big.df$sexrisk == "msm_ALL", "msm", pi.big.df$sexrisk)

pi.big.df$raceid <- relevel(factor(pi.big.df$raceid), ref = "ALL")
pi.big.df$ageid <- relevel(factor(pi.big.df$ageid), ref = "ALL")
pi.big.df$sexid <- relevel(factor(pi.big.df$sexid), ref = "ALL")
# pi.big.df$sexrisk <- relevel(factor(pi.big.df$sexrisk), ref = "msm")

pi.big.df$sexid[pi.big.df$sexid=="msm"] <- "male"
pi.big.df$female <- ifelse(pi.big.df$sexid=="het-female", 1,
                           ifelse(pi.big.df$sexid=="female", 1, 0))


# pi.big.df$male <- as.numeric(pi.big.df$sexid=="male")

pi.big.df$nonmsm <- as.numeric(pi.big.df$riskid!="msm")
pi.big.df$idu <- as.numeric(pi.big.df$riskid=="idu")

msm.pi.df <- subset(pi.big.df, nonmsm==0)
idu.pi.df <- subset(pi.big.df, idu==1)
het.pi.df <- subset(pi.big.df, riskid=="het")
# nonmsm.pi.df <- subset(pi.big.df, nonmsm==1)

# fit.pi.df <- lm(logit(pi) ~ years + raceid + ageid + sexrisk, data = pi.big.df)

pi.max <- 0.85

# fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid, data = msm.pi.df)
fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid + ageid, data = msm.pi.df)

fit.pi.msm

fit.pi.idu <- lm(logit(pi) ~ years + raceid + ageid + female, data = idu.pi.df)
fit.pi.idu 

fit.pi.het <- lm(logit(pi) ~ years + raceid + ageid + female, data = het.pi.df)
fit.pi.het

# fit.pi.nonmsm <- lm(logit(pi) ~ years + raceid + ageid + male + idu, 
#                     data = nonmsm.pi.df)
# fit.pi.nonmsm

# PrEP persistence ------

# [2012-2017 Persistence Data]; SF; 12 months of observation
# https://academic.oup.com/ofid/article/6/4/ofz101/5365426
# sample size - 364

pp.2012 <- c(
  total = 38.0,
  age1 = 30,
  age2 = 35,
  age3 = 35,
  age4 = 44,
  age5 = 60/134*100,
  black = 33,
  hispanic = 41,
  nbnh = (12+56+16)/(29+136+59)*100,
  het = 41,
  msm = 40,
  idu = 33
)

pp.2012 <- pp.2012/100
pp.2012

pp.df <- data.frame(pp = pp.2012)
pp.df$group <- rownames(pp.df)

pp.df$raceid <- ifelse(pp.df$group == "black", "black",
                       ifelse(pp.df$group == "hispanic", "hispanic",
                              ifelse(pp.df$group == "nbnh", "other", "ALL")))
pp.df$riskid <- ifelse(pp.df$group == "msm", "msm", 
                       ifelse(pp.df$group == "idu", "idu",
                              ifelse(pp.df$group == "het", "het", "ALL")))
pp.df$ageid <- ifelse(pp.df$group == "age1", "age1",
                      ifelse(pp.df$group == "age2", "age2",
                             ifelse(pp.df$group == "age3", "age3",
                                    ifelse(pp.df$group == "age4", "age4",
                                           ifelse(pp.df$group == "age5", "age5",
                                                  "ALL")))))

pp.df$ageid <- relevel(factor(pp.df$ageid), ref = "ALL")
pp.df$raceid <- relevel(factor(pp.df$raceid), ref = "ALL")
pp.df$riskid <- relevel(factor(pp.df$riskid), ref = "ALL")

fit.pp <- lm(pp ~ 1 + raceid + riskid + ageid, data = pp.df)
fit.pp
