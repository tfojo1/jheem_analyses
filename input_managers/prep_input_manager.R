
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    
    get.prep.use.functional.form(specification.metadata = metadata)
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
    int[,'black','msm',] <- int[,'black','msm',] + 0.97 # black log odds ratio
    int[,'hispanic','msm',] <- int[,'hispanic','msm',] + 0.83 # hispanic log odds ratio
    int[,'other','msm',] <- int[,'other','msm',] + 1 # other log odds ratio (reference)
    
    int["13-24 years",,'msm',] <- int["13-24 years",,'msm',] + 0.46 # 13-24 log odds ratio
    int["25-34 years",,'msm',] <- int["25-34 years",,'msm',] + 1.48 # 25-34 log odds ratio
    int["35-44 years",,'msm',] <- int["35-44 years",,'msm',] + 1 # 35-44 log odds ratio (reference)
    int["45-54 years",,'msm',] <- int["45-54 years",,'msm',] + 1.26 # 45-54 log odds ratio
    int["55+ years",,'msm',] <- int["55+ years",,'msm',] + 0.81 # 55+ log odds ratio
    
    # PWID
    int[,'black',,'active_IDU'] <- int[,'black',,'active_IDU'] + 1.17 # black log odds ratio
    int[,'hispanic',,'active_IDU'] <- int[,'hispanic',,'active_IDU'] + 2.35 # hispanic log odds ratio
    
    int["13-24 years",,,'active_IDU'] <- int["13-24 years",,,'active_IDU'] + 1.26 # 13-24 log odds ratio
    int["25-34 years",,,'active_IDU'] <- int["25-34 years",,,'active_IDU'] + 1.30 # 25-34 log odds ratio
    int["35-44 years",,,'active_IDU'] <- int["35-44 years",,,'active_IDU'] + 1 # 35-44 log odds ratio
    int["45-54 years",,,'active_IDU'] <- int["45-54 years",,,'active_IDU'] + 1.34 # 45-54 log odds ratio
    int["55+ years",,,'active_IDU'] <- int["55+ years",,,'active_IDU'] + 0.59 # 55+ log odds ratio
    
    
    
    
    slope[,'black','msm',] <- slope[,'black','msm',] + 0.14 # does this come from the logistic models we fit earlier?
    slope[,'hispanic','msm',] <- slope[,'hispanic','msm',] + 0.31 
    slope[,'other','msm',] <- slope[,'other','msm',] + 0.23 
    
    slope["13-24 years",,'msm',] <- slope["13-24 years",,'msm',] + 0.44 
    slope["25-34 years",,'msm',] <- slope["25-34 years",,'msm',] + 0.18 
    slope["35-44 years",,'msm',] <- slope["35-44 years",,'msm',] + 0.20
    slope["45-54 years",,'msm',] <- slope["45-54 years",,'msm',] + 0.21
    slope["55+ years",,'msm',] <- slope["55+ years",,'msm',] + 0.17
    
    
    # Make and return the functional form object
    create.logistic.linear.functional.form(
        intercept = int,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.prep.coverage, 
        parameters.are.on.logit.scale = T
    ) # does this need to be a return statement?
}

get.prep.indication.functional.form <- function(specification.metadata)
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