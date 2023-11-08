
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
  
    ff <-  get.prep.use.functional.form(specification.metadata = metadata)
    x <- ff$project(2020:2030, alphas = NULL, dim.names = ff$minimum.dim.names)
    length(x)
    x[[1]]
    sapply(x, max)
    sapply(x, min)
    sapply(x, mean)
    y <- sapply(x, function(z) {return(z)})
    dim.names <- c(ff$minimum.dim.names, list('year'=2020:2030))
    dim(y) <- sapply(dim.names, length)
    dimnames(y) <- dim.names
    
    y2 <- apply(y, c('year','race'), mean)
    y2
    
    df <- reshape2::melt(y2)
    ggplot(df, aes(x=year, y=value, color=race)) + geom_line() +
      ylim(min(df$value),0.6)
   
    
    df2 <- reshape2::melt(y)
    ggplot(df2, aes(x=year, y=value, color=sex)) + geom_line() + 
      ylim(0,0.6) 
    ggplot(df2, aes(x=year, y=value, color=risk)) + geom_line() + 
      ylim(0,0.6) 
    ggplot(df2, aes(x=year, y=value, color=age)) + geom_line() + 
      ylim(0,0.6) 
    df2$category <- paste0(df2$age,',',df2$race,',',df2$sex,',',df2$risk)
    # ggplot(df2, aes(x=year, y=value, color=category)) + geom_line() + 
    #   ylim(0,0.6) # can be a validation test with the methods 
}

#-- The Functions to Implement --#


get.prep.use.functional.form <- function(specification.metadata)
{
    # Some variables you will use
    anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
    max.prep.coverage = 0.6 # modify as you see fit
      
    # Set up our intercept/slope arrays
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    int = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    slope = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    
    
    # The code for how you can parse the age strata we need
    age.info = parse.age.strata.names(dim.names$age)
    age.spans = age.info$upper - age.info$lower
  
    # Do the work
    
    # int = 0 # for now
    # slope = 0 # for now
    
    # MSM
    int[,'black','msm',] <- int[,'black','msm',] + coef(fit.msm.black)[1] # black log odds ratio
    int[,'hispanic','msm',] <- int[,'hispanic','msm',] + coef(fit.msm.hisp)[1] # hispanic log odds ratio
    int[,'other','msm',] <- int[,'other','msm',] + coef(fit.msm.nbnh)[1] # other log odds ratio (reference)
    
    int["13-24 years",,'msm',] <- int["13-24 years",,'msm',] + coef(fit.msm.age1)[1] # 13-24 log odds ratio
    int["25-34 years",,'msm',] <- int["25-34 years",,'msm',] + coef(fit.msm.age2)[1] # 25-34 log odds ratio
    int["35-44 years",,'msm',] <- int["35-44 years",,'msm',] + coef(fit.msm.age3)[1] # 35-44 log odds ratio (reference)
    int["45-54 years",,'msm',] <- int["45-54 years",,'msm',] + coef(fit.msm.age4)[1] # 45-54 log odds ratio
    int["55+ years",,'msm',] <- int["55+ years",,'msm',] + coef(fit.msm.age5)[1] # 55+ log odds ratio
    
    slope[,'black','msm',] <- slope[,'black','msm',] + coef(fit.msm.black)[2] 
    slope[,'hispanic','msm',] <- slope[,'hispanic','msm',] + coef(fit.msm.hisp)[2]
    slope[,'other','msm',] <- slope[,'other','msm',] + coef(fit.msm.nbnh)[2]
    
    slope["13-24 years",,'msm',] <- slope["13-24 years",,'msm',] + coef(fit.msm.age1)[2]
    slope["25-34 years",,'msm',] <- slope["25-34 years",,'msm',] + coef(fit.msm.age2)[2]
    slope["35-44 years",,'msm',] <- slope["35-44 years",,'msm',] + coef(fit.msm.age3)[2]
    slope["45-54 years",,'msm',] <- slope["45-54 years",,'msm',] + coef(fit.msm.age4)[2]
    slope["55+ years",,'msm',] <- slope["55+ years",,'msm',] + coef(fit.msm.age5)[2]
    
    # PWID - uses 2015 and 2017 data 
    
    int[,'black',,'active_IDU'] <- int[,'black',,'active_IDU'] + coef(fit.idu.black)[1] 
    int[,'hispanic',,'active_IDU'] <- int[,'hispanic',,'active_IDU'] + coef(fit.idu.hisp)[1] 
    int[,'other',,'active_IDU'] <- int[,'other',,'active_IDU'] + coef(fit.idu.nbnh)[1]

    int["13-24 years",,,'active_IDU'] <- int["13-24 years",,,'active_IDU'] + coef(fit.idu.age1)[1] 
    int["25-34 years",,,'active_IDU'] <- int["25-34 years",,,'active_IDU'] + coef(fit.idu.age2)[1]
    int["35-44 years",,,'active_IDU'] <- int["35-44 years",,,'active_IDU'] + coef(fit.idu.age3)[1] 
    int["45-54 years",,,'active_IDU'] <- int["45-54 years",,,'active_IDU'] + coef(fit.idu.age4)[1]
    int["55+ years",,,'active_IDU'] <- int["55+ years",,,'active_IDU'] + coef(fit.idu.age5)[1]
    
    int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.idu.hetmale)[1]
    int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.idu.female)[1]
    
    slope[,'black',,'active_IDU'] <- slope[,'black',,'active_IDU'] + coef(fit.idu.black)[2]
    slope[,'hispanic',,'active_IDU'] <- slope[,'hispanic',,'active_IDU'] + coef(fit.idu.hisp)[2]
    slope[,'other',,'active_IDU'] <- slope[,'other',,'active_IDU'] + coef(fit.idu.nbnh)[2]
    
    slope["13-24 years",,,'active_IDU'] <- slope["13-24 years",,,'active_IDU'] + coef(fit.idu.age1)[2]
    slope["25-34 years",,,'active_IDU'] <- slope["25-34 years",,,'active_IDU'] + coef(fit.idu.age2)[2]
    slope["35-44 years",,,'active_IDU'] <- slope["35-44 years",,,'active_IDU'] + coef(fit.idu.age3)[2]
    slope["45-54 years",,,'active_IDU'] <- slope["45-54 years",,,'active_IDU'] + coef(fit.idu.age4)[2]
    slope["55+ years",,,'active_IDU'] <- slope["55+ years",,,'active_IDU'] + coef(fit.idu.age5)[2]
    
    slope[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.idu.hetmale)[2]
    slope[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.idu.female)[2]
    
    # Het - uses 2016 and 2019 data
    
    int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.het.male)[1]
    int[,,"female",] <- int[,,"female",] + coef(fit.het.female)[1] 
    
    int[,"black","heterosexual_male",] <- int[,"black","heterosexual_male",] + coef(fit.het.black)[1]
    int[,"hispanic","heterosexual_male",] <- int[,"hispanic","heterosexual_male",] + coef(fit.het.hisp)[1]
    int[,"other","heterosexual_male",] <- int[,"other","heterosexual_male",] + coef(fit.het.nbnh)[1]
    
    int[,"black","female",] <- int[,"black","female",] + coef(fit.het.black)[1]
    int[,"hispanic","female",] <- int[,"hispanic","female",] + coef(fit.het.hisp)[1]
    int[,"other","female",] <- int[,"other","female",] + coef(fit.het.nbnh)[1]
    
    int["13-24 years",,"heterosexual_male",] <- int["13-24 years",,"heterosexual_male",] + coef(fit.het.age1)[1]
    int["25-34 years",,"heterosexual_male",] <- int["25-34 years",,"heterosexual_male",] + coef(fit.het.age2)[1]
    int["35-44 years",,"heterosexual_male",] <- int["35-44 years",,"heterosexual_male",] + coef(fit.het.age3)[1]
    int["45-54 years",,"heterosexual_male",] <- int["45-54 years",,"heterosexual_male",] + coef(fit.het.age4)[1]
    int["55+ years",,"heterosexual_male",] <- int["55+ years",,"heterosexual_male",] + coef(fit.het.age5)[1]
    
    int["13-24 years",,"female",] <- int["13-24 years",,"female",] + coef(fit.het.age1)[1]
    int["25-34 years",,"female",] <- int["25-34 years",,"female",] + coef(fit.het.age2)[1]
    int["35-44 years",,"female",] <- int["35-44 years",,"female",] + coef(fit.het.age3)[1]
    int["45-54 years",,"female",] <- int["45-54 years",,"female",] + coef(fit.het.age4)[1]
    int["55+ years",,"female",] <- int["55+ years",,"female",] + coef(fit.het.age5)[1]
    
    slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit.het.male)[2]
    slope[,,"female",] <- slope[,,"female",] + coef(fit.het.female)[2]
    
    slope[,"black","heterosexual_male",] <- slope[,"black","heterosexual_male",] + coef(fit.het.black)[2]
    slope[,"black","heterosexual_male",] <- slope[,"black","heterosexual_male",] + coef(fit.het.black)[2]
    slope[,"hispanic","heterosexual_male",] <- slope[,"hispanic","heterosexual_male",] + coef(fit.het.hisp)[2]
    slope[,"other","heterosexual_male",] <- slope[,"other","heterosexual_male",] + coef(fit.het.nbnh)[2]
    
    slope[,"black","female",] <- slope[,"black","female",] + coef(fit.het.black)[2]
    slope[,"hispanic","female",] <- slope[,"hispanic","female",] + coef(fit.het.hisp)[2]
    slope[,"other","female",] <- slope[,"other","female",] + coef(fit.het.nbnh)[2]
    
    slope["13-24 years",,"heterosexual_male",] <- slope["13-24 years",,"heterosexual_male",] + coef(fit.het.age1)[2]
    slope["25-34 years",,"heterosexual_male",] <- slope["25-34 years",,"heterosexual_male",] + coef(fit.het.age2)[2]
    slope["35-44 years",,"heterosexual_male",] <- slope["35-44 years",,"heterosexual_male",] + coef(fit.het.age3)[2]
    slope["45-54 years",,"heterosexual_male",] <- slope["45-54 years",,"heterosexual_male",] + coef(fit.het.age4)[2]
    slope["55+ years",,"heterosexual_male",] <- slope["55+ years",,"heterosexual_male",] + coef(fit.het.age5)[2]
    
    slope["13-24 years",,"female",] <- slope["13-24 years",,"female",] + coef(fit.het.age1)[2]
    slope["25-34 years",,"female",] <- slope["25-34 years",,"female",] + coef(fit.het.age2)[2]
    slope["35-44 years",,"female",] <- slope["35-44 years",,"female",] + coef(fit.het.age3)[2]
    slope["45-54 years",,"female",] <- slope["45-54 years",,"female",] + coef(fit.het.age4)[2]
    slope["55+ years",,"female",] <- slope["55+ years",,"female",] + coef(fit.het.age5)[2]
    
    
    # Make and return the functional form object
    create.logistic.linear.functional.form(
        intercept = int,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.prep.coverage, 
        parameters.are.on.logit.scale = T
    ) 
}

get.prep.indication.functional.form <- function(specification.metadata)
{
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.indication = 0.8 # modify as you see fit
  
  # Set up our intercept/slope arrays
  # We are going to define these on the LOGIT scale
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  int = array(0, dim=sapply(dim.names, length),
              dimnames = dim.names)
  slope = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
  
  
  # The code for how you can parse the age strata we need
  age.info = parse.age.strata.names(dim.names$age)
  age.spans = age.info$upper - age.info$lower
  
  # Intercepts and Slopes
  int[,'black','msm',] <- int[,'black','msm',] + coef(fit.pi.msm.black)[1]
  int[,'hispanic','msm',] <- int[,'hispanic','msm',] + coef(fit.pi.msm.hisp)[1]
  int[,'other','msm',] <- int[,'other','msm',] + coef(fit.pi.msm.nbnh)[1]
  
  int["13-24 years",,'msm',] <- int["13-24 years",,'msm',] + coef(fit.pi.msm.age1)[1]
  int["25-34 years",,'msm',] <- int["25-34 years",,'msm',] + coef(fit.pi.msm.age2)[1]
  int["35-44 years",,'msm',] <- int["35-44 years",,'msm',] + coef(fit.pi.msm.age3)[1]
  int["45-54 years",,'msm',] <- int["45-54 years",,'msm',] + coef(fit.pi.msm.age4)[1]
  int["55+ years",,'msm',] <- int["55+ years",,'msm',] + coef(fit.pi.msm.age5)[1]
  
  
  
}

get.prep.persistence.functional.form <- function(specification.metadata)
{
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.coverage = 0.6 # modify as you see fit
  
  # Set up our intercept/slope arrays
  # We are going to define these on the LOGIT scale
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  int = array(0, dim=sapply(dim.names, length),
              dimnames = dim.names)
  slope = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
  
  
  # The code for how you can parse the age strata we need
  age.info = parse.age.strata.names(dim.names$age)
  age.spans = age.info$upper - age.info$lower
}