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
anchor.year <- 2017

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

# years.idu <- c(2015,2017) - 2020
# 
# fit.idu.black <- lm(logit(p.idu.black/p.max) ~ years.idu)
# fit.idu.hisp <- lm(logit(p.idu.hisp/p.max) ~ years.idu)
# fit.idu.nbnh <- lm(logit(p.idu.nbnh/p.max) ~ years.idu)
# 
# fit.idu.age1 <- lm(logit(p.idu.age1/p.max) ~ years.idu)
# fit.idu.age2 <- lm(logit(p.idu.age2/p.max) ~ years.idu)
# fit.idu.age3 <- lm(logit(p.idu.age3/p.max) ~ years.idu)
# fit.idu.age4 <- lm(logit(p.idu.age4/p.max) ~ years.idu)
# fit.idu.age5 <- lm(logit(p.idu.age5/p.max) ~ years.idu)
# 
# fit.idu.hetmale <- lm(logit(p.idu.male/p.max) ~ years.idu)
# fit.idu.female <- lm(logit(p.idu.female/p.max) ~ years.idu)


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

big.df <- rbind(p.idu.df.long, p.het.df.long)

big.df$sexrisk <- paste(big.df$sexid, big.df$risk, sep = "_")
big.df$sexrisk <- ifelse(big.df$sexrisk == "msm_msm", "msm", big.df$sexrisk)

big.df$raceid <- relevel(factor(big.df$raceid), ref = "ALL")
big.df$ageid <- relevel(factor(big.df$ageid), ref = "ALL")
big.df$sexid <- relevel(factor(big.df$sexid), ref = "ALL")
big.df$risk <- relevel(factor(big.df$risk), ref = "msm")
big.df$sexrisk <- relevel(factor(big.df$sexrisk), ref="msm")
big.df$sexid[big.df$sexid=="msm"] <- "male"

big.df$female <- as.numeric(big.df$sexid=="female")

big.df$nonmsm <- as.numeric(big.df$risk!="msm")
big.df$idu <- as.numeric(big.df$risk=="idu")

# fitting the big model
# fit.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + sexid + risk, data = big.df)

# # fitting an alternative model - interacting sex with risk
# fit2.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + sexid*risk, data = big.df)

# fit3.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + sexid*risk, data = big.df[1:50,])



# female model
# fit3.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + risk + female, data = big.df)

# fit2.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + sexid*risk, data = big.df[1:40,])

# 
# # nonmsm + female model
# fit4.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + nonmsm + idu + female, data = big.df)
# 
# # interacting nonmsm with age
# fit5.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + nonmsm + idu + female + nonmsm*ageid, 
#                   data = big.df) 
# 
# # interacting nonmsm with year
# fit6.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + nonmsm + idu + female + nonmsm*year,
#                   data = big.df)
# 
# # interacting nonmsm with year and race
# fit7.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + nonmsm + idu + female +
#                     nonmsm*raceid + nonmsm*year,
#                   data = big.df)
# 
# # interacting nonmsm with race, year, and age
# fit8.big.df <- lm(logit(p/p.max) ~ year + raceid + ageid + nonmsm + idu + female + nonmsm*raceid +
#                     nonmsm*year + nonmsm*ageid,
#                   data = big.df)

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

pi.msm.total <- c(
  pi.msm.2017$total,
  pi.msm.2018$total,
  pi.msm.2019$total
)

years.pi <- c(2017:2019) - anchor.year
pi.max <- 1

fit.pi.msm.black <- lm(logit(pi.msm.black/pi.max) ~ years.pi)
fit.pi.msm.hisp <- lm(logit(pi.msm.hisp/pi.max) ~ years.pi)
fit.pi.msm.nbnh <- lm(logit(pi.msm.nbnh/pi.max) ~ years.pi)

fit.pi.msm.age1 <- lm(logit(pi.msm.age1/pi.max) ~ years.pi)
fit.pi.msm.age2 <- lm(logit(pi.msm.age2/pi.max) ~ years.pi)
fit.pi.msm.age3 <- lm(logit(pi.msm.age3/pi.max) ~ years.pi)
fit.pi.msm.age4 <- lm(logit(pi.msm.age4/pi.max) ~ years.pi)
fit.pi.msm.age5 <- lm(logit(pi.msm.age5/pi.max) ~ years.pi)

## IDU ------

# # https://journals.lww.com/stdjournal/FullText/2018/04000/An_Exploration_of_Factors_Impacting_Preexposure.1.aspx?casa_token=rVyxbUR_Wc8AAAAA:noQgD0j2phdyeYybIz-7kY3y5LngYlOnMBzNgVB53J8BmR74EJsyb6iiZdPte3KnzKYHkyZocw_vHyUPTktaR5tc&casa_token=PTZU195-rC4AAAAA:qxzCsh5TwpyByzcVN4Nix2L4X5pPIFCirYj6O_0Gf1Oga1mv4oOJAfGaBFGEIvdXotaDOf9sl86IUGx5gzJKJMAU 
# pi.idu.2016 <- data.frame(
#   total = 89.9,
#   female = 95.4,
#   male = 84.9
# )
# 
# pi.idu.2016 <- pi.idu.2016 / 100
# 
# # https://www.thelancet.com/action/showPdf?pii=S2214-109X%2823%2900057-8
# pi.idu.2017 <- 31.8 / 100 

# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf 
p.idu.2015 <- data.frame(
  total = 60.6,
  male = 60.0,
  female = 62.5,
  age18.24 = 76.1,
  age25.29 = 72.9,
  age30.39 = 68.5,
  age40.49 = 59.5,
  age50ge = 49.4,
  black = 50.6,
  hisp = 61.1,
  nbnh = 67.7
)

p.idu.2015 <- age_mutate(p.idu.2015)/100


# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf 
p.idu.2018 <- data.frame(
  total = 59.8,
  male = 58.5,
  female = 62.6,
  age18.24 = 72.0,
  age25.29 = 70.5,
  age30.39 = 67.2,
  age40.49 = 59.6,
  age50ge = 49.8,
  black = 49.8,
  hisp = 57.8,
  nbnh = 67.4
)

p.idu.2018 <- age_mutate(p.idu.2018)/100

years.idu <- c(2015, 2018) - anchor.year

pi.idu.black <- c(
  p.idu.2015$black,
  p.idu.2018$black
)

pi.idu.hisp <- c(
  p.idu.2015$hisp,
  p.idu.2018$hisp
)

pi.idu.nbnh <- c(
  p.idu.2015$nbnh,
  p.idu.2018$nbnh
)

pi.idu.age1 <- c(
  p.idu.2015$age1,
  p.idu.2018$age1
)

pi.idu.age2 <- c(
  p.idu.2015$age2,
  p.idu.2018$age2
)

pi.idu.age3 <- c(
  p.idu.2015$age3,
  p.idu.2018$age3
)

pi.idu.age4 <- c(
  p.idu.2015$age4,
  p.idu.2018$age4
)

pi.idu.age5 <- c(
  p.idu.2015$age5,
  p.idu.2018$age5
)

pi.idu.male <- c(
  p.idu.2015$male,
  p.idu.2018$male
)

pi.idu.female <- c(
  p.idu.2015$female,
  p.idu.2018$female
)

## Het -------

# 2016 
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf
# criteria - condomless vaginal sex (among heterosexually active men + women)
pi.het.2016 <- data.frame(
  total = (2881 + 3740)/(3280 + 4306),
  male = 87.8,
  female = 92.7,
  age18.24 = (621 + 748)/(715 + 803),
  age25.29 = (412 + 566)/(461 + 606),
  age30.39 = (566 + 850)/(633 + 913),
  age40.49 = (558 + 756)/(636 + 819),
  age50ge = (724 + 820)/(835 + 895),
  black = (2134 + 2565)/(2451 + 2794),
  hisp = (517 + 858)/(570 + 901),
  nbnh = (225 + 314)/(252 + 336)
)

pi.het.2016 <- age_mutate(pi.het.2016)/100

# 2019 
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf
# criteria - condomless vaginal sex (among heterosexually active men + women)
pi.het.2019 <- data.frame(
  total = (3502 + 4596)/(4176 + 5183),
  male = 83.9,
  female = 88.7,
  age18.24 = (683 + 923)/(806 + 1030),
  age25.29 = (522 + 694)/(590 + 764),
  age30.39 = (839 + 1152)/(969 + 1299),
  age40.49 = (649 + 877)/(777 + 979),
  age50ge = (809 + 950)/(1034 + 1111),
  black = (2452 + 3021)/(2945 + 3432),
  hisp = (726 + 1098)/(851 + 1227),
  nbnh = (316 + 470)/(368 + 515)
)

pi.het.2019 <- age_mutate(pi.het.2019)/100


pi.het.black <- c(
  pi.het.2016$black,
  pi.het.2019$black
)

pi.het.hisp <- c(
  pi.het.2016$hisp,
  pi.het.2019$hisp
)

pi.het.nbnh <- c(
  pi.het.2016$nbnh,
  pi.het.2019$nbnh
)

pi.het.age1 <- c(
  pi.het.2016$age1,
  pi.het.2019$age1
)

pi.het.age2 <- c(
  pi.het.2016$age2,
  pi.het.2019$age2
)

pi.het.age3 <- c(
  pi.het.2016$age3,
  pi.het.2019$age3
)

pi.het.age4 <- c(
  pi.het.2016$age4,
  pi.het.2019$age4
)

pi.het.age5 <- c(
  pi.het.2016$age5,
  pi.het.2019$age5
)

pi.het.male <- c(
  pi.het.2016$male,
  pi.het.2019$male
)

pi.het.female <- c(
  pi.het.2016$female,
  pi.het.2019$female
)


## formatting data ------
pi.msm.df <- data.frame(
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

pi.idu.df <- data.frame(
  years = years.idu,
  black = pi.idu.black,
  hisp = pi.idu.hisp,
  nbnh = pi.idu.nbnh,
  age1 = pi.idu.age1,
  age2 = pi.idu.age2,
  age3 = pi.idu.age3,
  age4 = pi.idu.age4,
  age5 = pi.idu.age5,
  male = pi.idu.male,
  female = pi.idu.female
)

pi.het.df <- data.frame(
  years = years.het,
  black = pi.het.black,
  hisp = pi.het.hisp,
  nbnh = pi.het.nbnh,
  age1 = pi.het.age1,
  age2 = pi.het.age2,
  age3 = pi.het.age3,
  age4= pi.het.age4,
  age5 = pi.het.age5,
  male = pi.het.male,
  female = pi.het.female
)

pi.msm.df.long <- gather(pi.msm.df, key = "group", value = "pi", -years)
pi.msm.df.long$sexid <- rep("msm", length(pi.msm.df.long$pi))
pi.msm.df.long$riskid <- rep("msm", length(pi.msm.df.long$pi))

pi.idu.df.long <- gather(pi.idu.df, key = "group", value = "pi", -years)
pi.idu.df.long$riskid <- rep("idu", length(pi.idu.df.long$pi))
pi.idu.df.long$sexid <- rep("idu", length(pi.idu.df.long$pi))

pi.het.df.long <- gather(pi.het.df, key = "group", value = "pi", -years)
pi.het.df.long$riskid <- rep("het", length(pi.het.df.long$pi))
pi.het.df.long$sexid <- rep("het", length(pi.het.df.long$pi))

pi.df.long <- rbind(pi.msm.df.long, pi.idu.df.long, pi.het.df.long)

pi.big.df <- pi.df.long |> dplyr::mutate(raceid = ifelse(group == "black", "black", 
                                                   ifelse(group == "hisp", "hisp", 
                                                          ifelse(group == "nbnh", "nbnh", "ALL"))),
                                          ageid = ifelse(group == "age1", "age1", 
                                                   ifelse(group == "age2", "age2", 
                                                          ifelse(group == "age3", "age3", 
                                                                 ifelse(group == "age4", "age4", 
                                                                        ifelse(group == "age5", "age5", "ALL"))))),
                                          sexid = ifelse(sexid == "msm", "msm",
                                                         ifelse(group == "male", "male",
                                                                ifelse(group == "female", "female",
                                                                       "ALL")))) 

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
pi.big.df$female <- as.numeric(pi.big.df$sexid=="female")
pi.big.df$male <- as.numeric(pi.big.df$sexid=="male")

pi.big.df$nonmsm <- as.numeric(pi.big.df$riskid!="msm")
pi.big.df$idu <- as.numeric(pi.big.df$riskid=="idu")

msm.pi.df <- subset(pi.big.df, nonmsm==0)
nonmsm.pi.df <- subset(pi.big.df, nonmsm==1)

fit.pi.df <- lm(logit(pi) ~ years + raceid + ageid + sexrisk, data = pi.big.df)

fit.pi.msm <- lm(logit(pi) ~ years + raceid + ageid, data = msm.pi.df)
fit.pi.msm

fit.pi.nonmsm <- lm(logit(pi) ~ years + raceid + ageid + male + idu, data = nonmsm.pi.df)
fit.pi.nonmsm

# PrEP persistence ------


## PrEP Persistence


# 2013-2019 clinical data - https://www.liebertpub.com/doi/full/10.1089/apc.2021.0074?casa_token=7Pq9qYIOoeAAAAAA%3ALvWdcft-Qq0LT0wkma1KMR7grGwGODoOjebSB_OqFPIRO98OPk1_RaGyaoac1O-RjfffcOovwbqu

pp.2019 <- data.frame(
  total = 274/(274+380),
  white = 187/(187+233),
  black = 20/(20+39),
  other = 59/(59+102),
  hisp = 46/(46+74),
  male = 268/(268+360),
  female = 6/(6+20)
)

pp.2019

#[2015-17 Persistence Data](https://link.springer.com/article/10.1007/s10461-019-02654-x/tables/1)

pp.2017 <- data.frame( 
  total = 31.9,
  age18.24 = 43.0,
  age25.29 = 53,
  age30.39 = 56.0,
  age40.49 = 64.0,
  age50ge = 65.0, # ages 50 or greater
  male = 57,
  female = 34
)

pp.2017 <- pp.2017/100
pp.2017

# [2012-2017 Persistence Data]; SF; 12 months of observation
# https://academic.oup.com/ofid/article/6/4/ofz101/5365426
# sample size - 364

pp.2012 <- data.frame(
  total = 38.0,
  age18.24 = 30,
  age25.29 = 35,
  age30.39 = 35,
  age40.49 = 44,
  age50ge = 60/134*100,
  black = 33,
  hispanic = 41,
  nbnh = (12+56+16)/(29+136+59)*100,
  het = 41,
  msm = 40,
  idu = 33
)

pp.2012 <- pp.2012/100
pp.2012

# 2011-2014 Persistence Data; Fenway Health Cohort - Boston; 24 months total follow-up
# https://onlinelibrary.wiley.com/doi/epdf/10.1002/jia2.25250
# sample size - 663

pp.2014 <- data.frame(
  total = 376/663,
  age18.24 = 37/88,
  age25.29 = 69/168,
  age30.39 = 130/208,
  age40.49 = 140/199,
  age50ge = 140/199,
  male = (367+5)/(636+2),
  female = (2+2)/(3+4),
  black = 20/43,
  hispanic = 22/44,
  nbnh = (289+10+35)/(481+24+61)
)

pp.2014

pp.total <- c(pp.2014$total, pp.2012$total)
pp.age1 <- c(pp.2014$age18.24, pp.2012$age18.24)
pp.age2 <- c(pp.2014$age25.29, pp.2012$age25.29)
pp.age3 <- c(pp.2014$age30.39, pp.2012$age30.39)
pp.age4 <- c(pp.2014$age40.49, pp.2012$age40.49)
pp.age5 <- c(pp.2014$age50ge, pp.2012$age50ge)
pp.black <- c(pp.2014$black, pp.2012$black)
pp.hisp <- c(pp.2014$hispanic, pp.2012$hispanic)
pp.nbnh <- c(pp.2014$nbnh, pp.2012$nbnh)

years.pp <- c(2014, 2012)-anchor.year

pp.df <- data.frame(
  years = years.pp,
  total = pp.total,
  black = pp.black,
  hisp = pp.hisp,
  nbnh = pp.nbnh,
  age1 = pp.age1,
  age2 = pp.age2,
  age3 = pp.age3,
  age4= pp.age4,
  age5 = pp.age5
)
### prep persistence model ------
pp.df.long <- gather(pp.df, key = "group", value = "pp", -years)
pp.df.long <- pp.df.long |> dplyr::mutate(raceid = ifelse(group == "black", "black", 
                                                          ifelse(group == "hisp", "hisp", 
                                                                 ifelse(group == "nbnh", "nbnh", "ALL"))),
                                          ageid = ifelse(group == "age1", "age1", 
                                                         ifelse(group == "age2", "age2", 
                                                                ifelse(group == "age3", "age3", 
                                                                       ifelse(group == "age4", "age4", 
                                                                              ifelse(group == "age5", "age5", "ALL"))))))
pp.df.long$raceid <- relevel(factor(pp.df.long$raceid), ref = "ALL")
pp.df.long$ageid <- relevel(factor(pp.df.long$ageid), ref = "ALL")

fit.pp <- lm(logit(pp) ~ years + raceid + ageid, data = pp.df.long)

# 2014-2017 persistence; cohort study; 24 month assessments
# https://link.springer.com/article/10.1007/s10461-018-2045-1/tables/1

pp.msm.2016 <- data.frame(
  total = ((55-12)+(117-19))/(55+117+12+19),
  white = (117-19)/117,
  nonwhite = (55-12)/55
)

# 
#[2020 Persistence Data](https://www.tandfonline.com/doi/full/10.1080/09540121.2023.2217375) - MSM
pp.msm.2020 <- data.frame(
  total = 77.3,
  age18.24 = 66.7,
  age25.29 = 77.7,
  age30.39 = 82.0,
  age40.49 = 84.8,
  age50ge = 84.8, # ages 50 or greater
  black = 83.2,
  hisp = 78.9,
  nbnh = 76.0
)




# MSM and transgender folks, 2017 - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6698689/ -- prevalence ratios
