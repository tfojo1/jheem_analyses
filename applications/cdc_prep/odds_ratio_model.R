
#Read data frame 
source("../jheem_analyses/applications/cdc_prep/cdc_prep_parameters.R")


get.fraction.diagnoses.from.cdc.model.spline <- function(specification.metadata){
    
    
    df <- read.csv("../jheem_analyses/applications/cdc_testing/cdc_diagnoses.csv")
    
    #Set reference categories
    
    specification.metadata = get.specification.metadata("cdct","MD")
    dim.names = specification.metadata$dim.names[c("age","race","sex")]
    
    df$race <- relevel(factor(df$race), ref = "all")
    age.mapping = dim.names$age
    names(age.mapping) = paste0("cat",1:5)
    age.mapping = c(age.mapping,all = "all")
    df$age = age.mapping[df$age]
    df$age <- relevel(factor(df$age), ref = "all")
    df$sex <- relevel(factor(df$sex), ref = "all")
    
    df$logit.diagnoses <- log(df$diagnoses) - log(1-df$diagnoses)
    splines::ns(df$year)
    
    
    
    
    spline_model <- suppressWarnings(lm(logit.diagnoses ~ as.factor(race) + as.factor(age) + as.factor(sex) + splines::ns(year), data = df, weights = number))
    summary(spline_model)
    
    #spline_model_2 <- suppressWarnings(lm(logit.diagnoses ~ as.factor(race) + as.factor(age) + as.factor(sex) + year + pmax(0,year-2010) + pmax(0, year-2015) + pmax(0, year-2020) , data = df, weights = number))
    #summary(spline_model_2)
    
    dummy.for.spline = as.data.frame(get.every.combination(dim.names))
    dummy.for.spline$year = 2010
    diagnoses.knot0 = predict(spline_model,newdata = dummy.for.spline)
    
    dummy.for.spline$year = 2015
    diagnoses.knot1 = predict(spline_model,newdata = dummy.for.spline)
    
    
    dummy.for.spline$year = 2020
    diagnoses.knot2 = predict(spline_model,newdata = dummy.for.spline)
    
    
    dim(diagnoses.knot0) = sapply(dim.names,length)
    dimnames(diagnoses.knot0) = dim.names
    
    dim(diagnoses.knot1) = sapply(dim.names,length)
    dimnames(diagnoses.knot1) = dim.names
    
    dim(diagnoses.knot2) = sapply(dim.names,length)
    dimnames(diagnoses.knot2) = dim.names
    
    create.natural.spline.functional.form(knot.times = c(time0 = 2010,time1 = 2015, time2 = 2020), 
                                          knot.values = list(
                                              time0 = diagnoses.knot0,
                                              time1 = diagnoses.knot1,
                                              time2 = diagnoses.knot2
                                          ),
                                          link = "logit", 
                                          knots.are.on.transformed.scale = T,
                                          after.time = 2025,
                                          after.modifier = 0.5,
                                          modifiers.apply.to.change = T)
}



get.fraction.prep.referred <- function(specification.metadata)
{
    
    df <- read.csv("../jheem_analyses/applications/cdc_prep/prep_referral.csv")
    
    #Set reference categories
    
    specification.metadata = get.specification.metadata("cdcp","MD")
    dim.names = specification.metadata$dim.names[c("age","race","sex")]
    
    df$race <- relevel(factor(df$race), ref = "all")
    age.mapping = dim.names$age
    names(age.mapping) = paste0("cat",1:5)
    age.mapping = c(age.mapping,all = "all")
    df$age = age.mapping[df$age]
    df$age <- relevel(factor(df$age), ref = "all")
    df$sex <- relevel(factor(df$sex), ref = "all")
    #Logistic regression
    oddsratio_model <- suppressWarnings(glm(referrals ~ as.factor(race)*year + as.factor(age)*year + as.factor(sex)*year, data = df, weights = number, family = binomial(link = "logit")))
    summary(oddsratio_model)
    
    
    dummy.for.intercept = as.data.frame(get.every.combination(dim.names))
    dummy.for.intercept$year = 2021
    referrals.intercept = predict(oddsratio_model,newdata = dummy.for.intercept)
    
    dummy.for.slope = dummy.for.intercept 
    dummy.for.slope$year = 2022
    referrals.slope = predict(oddsratio_model,newdata = dummy.for.slope) -referrals.intercept
    
    dim(referrals.intercept) = sapply(dim.names,length)
    dimnames(referrals.intercept) = dim.names
    
    dim(referrals.slope) = sapply(dim.names,length)
    dimnames(referrals.slope) = dim.names
    
    create.logistic.linear.functional.form(intercept = referrals.intercept, 
                                           slope = referrals.slope, 
                                           anchor.year = 2021, 
                                           parameters.are.on.logit.scale = TRUE)
}


get.fraction.prep.eligible <- function(specification.metadata)
{
    
    df <- read.csv("../jheem_analyses/applications/cdc_prep/prep_eligible.csv")
    
    #Set reference categories
    
    specification.metadata = get.specification.metadata("cdcp","MD")
    dim.names = specification.metadata$dim.names[c("age","race","sex")]
    
    df$race <- relevel(factor(df$race), ref = "all")
    age.mapping = dim.names$age
    names(age.mapping) = paste0("cat",1:5)
    age.mapping = c(age.mapping,all = "all")
    df$age = age.mapping[df$age]
    df$age <- relevel(factor(df$age), ref = "all")
    df$sex <- relevel(factor(df$sex), ref = "all")
    
    #Logistic regression
    oddsratio_model <- suppressWarnings(glm(eligible ~ as.factor(race)*year + as.factor(age)*year + as.factor(sex)*year, data = df, weights = number, family = binomial(link = "logit")))
    summary(oddsratio_model)
    
    
    dummy.for.intercept = as.data.frame(get.every.combination(dim.names))
    dummy.for.intercept$year = 2021
    eligible.intercept = predict(oddsratio_model,newdata = dummy.for.intercept)
    
    dummy.for.slope = dummy.for.intercept 
    dummy.for.slope$year = 2022
    eligible.slope = predict(oddsratio_model,newdata = dummy.for.slope) - eligible.intercept
    
    dim(eligible.intercept) = sapply(dim.names,length)
    dimnames(eligible.intercept) = dim.names
    
    dim(eligible.slope) = sapply(dim.names,length)
    dimnames(eligible.slope) = dim.names
    
    create.logistic.linear.functional.form(intercept = eligible.intercept, 
                                           slope = eligible.slope, 
                                           anchor.year = 2021, 
                                           parameters.are.on.logit.scale = TRUE)
}
