
#Read data frame #chagne working direcotry
# source("../jheem_analyses/applications/cdc_testing/cdc_testing_parameters.R")

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
