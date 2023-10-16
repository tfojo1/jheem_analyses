

get.prep.use.functional.form <- function(specification.metadata)
{
    # Some variables you will use
    anchor.year = 2020 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
    max.prep.coverage = 0.75 # modify as you see fit
      
    # Set up our intercept/slope arrays
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    rv = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    
    # The code for how you can parse the age strata we need
    age.info = parse.age.strata.names(dim.names$age)
    age.spans = age.info$upper - age.info$lower
  
    # Do the work
    
    # Make and return the functional form object
    create.logistic.linear.functional.form(
        intercept = intercept,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.prep.coverage, 
        parameters.are.on.logit.scale = T
    )
}

get.prep.indication.functional.form <- function(specification.metadata)
{
    
}

get.prep.persistence.functional.form <- function(specification.metadata)
{
    
}