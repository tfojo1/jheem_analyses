
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

# criteria - any STI

pi.het.2016 <- data.frame(
  total = 6.9,
  male = 5.4,
  female = 8.2,
  age18.24 = 11.4,
  age25.29 = 10.7,
  age30.39 = 5.6,
  age40.49 = 4.7,
  age50ge = 3.8,
  black = 7.9,
  hisp = 3.7,
  nbnh = (4+3+1+8+20)/(52+12+21+221+282)*100
)

pi.het.2016 <- age_mutate(pi.het.2016)/100


# criteria - any STI
pi.het.2019 <- data.frame(
  total = 7.0,
  male = 5.2,
  female = 8.5,
  age18.24 = 11.8,
  age25.29 = 8.9,
  age30.39 = 6.3,
  age40.49 = 4.9,
  age50ge = 4.3,
  black = 7.8,
  hisp = 4.9,
  nbnh = (6+2+1+14+34)/(58+17+33+413+362)*100
)

pi.het.2019 <- age_mutate(pi.het.2019)/100

pi.het.total <- c(
  pi.het.2016$total,
  pi.het.2019$total
)

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

pi.years.het <- c(2016, 2019) - anchor.year.pi

pi.het.male <- c(
  pi.het.2016$male,
  pi.het.2019$male
)

pi.het.female <- c(
  pi.het.2016$female,
  pi.het.2019$female
)


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

pi.msm.2017.new <- pi.msm.2017 %>% 
  dplyr::rename(age18.24 = age15.24, age50ge = age40ge) %>%
  dplyr::mutate(age40.49 = age50ge) %>%
  dplyr::select(total, age18.24, age25.29, age30.39, age40.49, age50ge,
                black, hisp, nbnh, age1, age2, age3, age4, age5)

pi.ratio.msm <- (pi.msm.2017.new/pi.msm.cdc.2017) 
# pi.ratio.df <- pi.ratio.msm %>%
#   summarize(across(everything(), mean))

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
  total = pi.het.total,
  male = pi.het.male,
  female = pi.het.female
)

# multiplying it by the amis/cdc proportion of PrEP indication

pi.ratio.msm$male <- pi.ratio.msm$total
pi.ratio.msm$female <- pi.ratio.msm$total

pi.het.df <- pi.het.df %>%
  dplyr::select(-years) %>%
  dplyr::mutate(across(everything(), ~ . * pi.ratio.msm[[dplyr::cur_column()]])) %>%
  dplyr::mutate(years = pi.years.het) %>% 
  dplyr::select(years, everything())

pi.msm.cdc.df <- pi.msm.cdc.df %>%
  dplyr::select(-years) %>%
  dplyr::mutate(across(everything(), ~ . * pi.ratio.msm[[dplyr::cur_column()]])) %>%
  dplyr::mutate(years = sort(unique(pi.msm.cdc.df$years))) %>% 
  dplyr::select(years, everything())

pi.msm.amis.df <- pi.msm.amis.df %>%
  dplyr::select(-years) %>%
  dplyr::mutate(across(everything(), ~ . * pi.ratio.msm[[dplyr::cur_column()]])) %>%
  dplyr::mutate(years = sort(unique(pi.msm.amis.df$years))) %>% 
  dplyr::select(years, everything())

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
pi.het.df.long$sexid <- rep("het", length(pi.het.df.long$pi))

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
                                         sexid = ifelse(group == "msm", "msm",
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

# prep indication plots -----

ff <-  get.prep.indication.functional.form(specification.metadata = metadata)
anchor.year <- 2009
x <- ff$project(anchor.year:2030)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff$minimum.dim.names, list('year'=anchor.year:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names

y2 <- apply(y, c('year','race'), mean)

# y2

df <- reshape2::melt(y2)
df.pts <- subset(p.msm.df.long, raceid != "ALL") |> dplyr::mutate(year = year + anchor.year)
race.plot <- ggplot(df, aes(x=year, y=value, color=race)) + geom_line(linewidth = 1) +
  ylim(0,1) + 
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  # geom_point(aes(x=year, y = p, color=raceid), data = df.pts)
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal() 

df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) + 
  ylim(0,1) + 
  scale_color_manual(values = c("#008080", "#FF8C00", "#9932CC")) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal()


df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) + 
  ylim(0,1) + 
  scale_color_manual(values = c("#008080", "#FF8C00", "#9932CC"))+
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal()

df5 <- reshape2::melt(apply(y, c('year','age'), mean))
age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
  ylim(0,1) + 
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal() 

# arrange all 4 plots
combined.plot.pi <- ggpubr::ggarrange(race.plot, sex.plot, risk.plot, age.plot,
                                      ncol = 2, nrow = 2, labels = c("Race","Sex","Risk","Age"))
combined.plot.pi

msm.pi.df$shape <- ifelse(msm.pi.df$dataid=="cdc", 1, 8)

msm.race <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','race'), mean))
df.pts <- subset(msm.pi.df, raceid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$raceid <- factor(df.pts$raceid, levels = c("black","hisp","nbnh"))
msm.race$race <- factor(msm.race$race, levels = c("black","hispanic","other"))
levels(df.pts$raceid) <- levels(msm.race$race)
msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=years, y = pi, color=raceid, 
                 shape = as.factor(dataid)), 
             data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Race", shape = "Data Source") +
  theme_minimal() 

msm.age <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','age'), mean))
df.pts <- subset(msm.pi.df, ageid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
msm.age$age <- factor(msm.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(msm.age$age) 

msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  geom_point(aes(x=years, y = pi, color=ageid,
                 shape = as.factor(dataid)), data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Age", shape = "Data Source") +
  theme_minimal()

msm.plots.pi <- ggpubr::ggarrange(msm.race.plot, msm.age.plot,
                                  ncol=1, nrow=2, 
                                  labels = c("MSM - Race", "MSM - Age")) 
msm.plots.pi

# idu plots
idu.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'race'), mean))
df.pts <- subset(idu.pi.df, raceid!="ALL")  %>%
  dplyr::mutate(years = years + anchor.year) 

df.pts$raceid <- factor(df.pts$raceid, levels = c("black", "hisp", "nbnh"))
idu.race$race <- factor(idu.race$race, levels = c("black","hispanic","other"))

levels(df.pts$raceid) <- levels(idu.race$race) 

idu.race.plot <- ggplot(idu.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Race") +
  theme_minimal()

idu.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'age'), mean))
df.pts <- subset(idu.pi.df, ageid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year) 

df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
idu.age$age <- factor(idu.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(idu.age$age) 

idu.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  geom_point(aes(x = years, y = pi, color=ageid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Age") +
  theme_minimal()

idu.sex <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'sex'), mean))
df.pts <- subset(idu.pi.df, sexid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year)

df.pts$sexid <- factor(df.pts$sexid, levels = c("male", "female"))
idu.sex$sex <- factor(idu.sex$sex, levels = c("heterosexual_male", "female"))

levels(df.pts$sexid) <- levels(idu.sex$sex) 

idu.sex.plot <- ggplot(idu.sex, aes(year, value, color=sex)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#4169E1", "#DC143C")) +
  geom_point(aes(x = years, y = pi, color=sexid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Sex") +
  theme_minimal()  

idu.plots.pi <- ggpubr::ggarrange(idu.race.plot, idu.age.plot, idu.sex.plot,
                                  nrow = 3, ncol=1, labels = c("PWID - Race", "PWID - Age", 
                                                               "PWID - Sex"))
idu.plots.pi

het.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),"never_IDU",], c('year', 'race'), mean))
df.pts <- subset(het.pi.df, raceid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year) 

df.pts$raceid <- factor(df.pts$raceid, levels = c("black", "hisp", "nbnh"))
het.race$race <- factor(het.race$race, levels = c("black","hispanic","other"))

levels(df.pts$raceid) <- levels(het.race$race) 

het.race.plot <- ggplot(het.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a")) +
  geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Race") +
  theme_minimal()

het.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'never_IDU',], c('year', 'age'), mean))
df.pts <- subset(het.pi.df, ageid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year)

df.pts$ageid <- factor(df.pts$ageid, 
                       levels = c("age1", "age2", "age3", "age4", "age5"))
het.age$age <- factor(het.age$age, 
                      levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(het.age$age) 

het.age.plot <- ggplot(het.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  geom_point(aes(x = years, y = pi, color=ageid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Age") +
  theme_minimal()

het.sex <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'never_IDU',], c('year', 'sex'), mean))
df.pts <- subset(het.pi.df, sexid!="ALL" & 
                   raceid =="ALL" &
                   ageid == "ALL") %>%
  dplyr::mutate(years = years + anchor.year)

df.pts$sexid <- factor(df.pts$sexid, levels = c("male", "female"))
het.sex$sex <- factor(het.sex$sex, levels = c("heterosexual_male", "female"))

levels(df.pts$sexid) <- levels(het.sex$sex) 

het.sex.plot <- ggplot(het.sex, aes(year, value, color=sex)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = years, y = pi, color=sexid), data = df.pts) +
  scale_color_manual(values = c("#4169E1", "#DC143C")) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Sex") +
  theme_minimal()  

het.plots.pi <- ggpubr::ggarrange(het.race.plot, het.age.plot, het.sex.plot,
                                  nrow = 3, ncol = 1, labels=c("Het - Race", "Het - Age", "Het - Sex"))

pdf("PrEP_Indication_Plots_STI.pdf", width = 18, height = 10)
combined.plot.pi
msm.plots.pi
idu.plots.pi
het.plots.pi
dev.off()
