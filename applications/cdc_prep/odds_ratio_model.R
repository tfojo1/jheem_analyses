
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
    oddsratio_model <- suppressWarnings(glm(diagnoses ~ as.factor(race)*year + as.factor(age)*year + as.factor(sex)*year, data = df, weights = number, family = binomial(link = "logit")))
    summary(oddsratio_model)
    
    
    dummy.for.intercept = as.data.frame(get.every.combination(dim.names))
    dummy.for.intercept$year = 2021
    diagnoses.intercept = predict(oddsratio_model,newdata = dummy.for.intercept)
    
    dummy.for.slope = dummy.for.intercept 
    dummy.for.slope$year = 2022
    diagnoses.slope = predict(oddsratio_model,newdata = dummy.for.slope) - diagnoses.intercept
    
    dim(diagnoses.intercept) = sapply(dim.names,length)
    dimnames(diagnoses.intercept) = dim.names
    
    dim(diagnoses.slope) = sapply(dim.names,length)
    dimnames(diagnoses.slope) = dim.names
    
    create.logistic.linear.functional.form(intercept = diagnoses.intercept, 
                                           slope = diagnoses.slope, 
                                           anchor.year = 2021, 
                                           parameters.are.on.logit.scale = TRUE)
}

