
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
    # source('prepuse_plot.R')
    # source('prepindication_plot.R')
    # source('preppersistence_plot.R')
}

#-- The Functions to Implement --#


get.prep.use.functional.form <- function(specification.metadata)
{
    # Some variables you will use
    anchor.year = 2015 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
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
    
    # int[,,"msm",] <- int[,,"msm",] + coef(fit.p.msm)["(Intercept)"]
    int[,,"msm",] <- int[,,"msm",] + coef(fit.p.msm)["(Intercept)"]
    int[,,"female",] <- int[,,"female",] + coef(fit.p.nonmsm)["(Intercept)"]
    int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.p.nonmsm)["(Intercept)"]
    
    # int[,"black","msm",] <- int[,"black","msm",] + coef(fit.p.msm)["raceidblack"]
    int[,"black","msm",] <- int[,"black","msm",] + coef(fit.p.msm)["raceidblack"]
    int[,"hispanic","msm",] <- int[,"hispanic","msm",] + coef(fit.p.msm)["raceidhispanic"]
    int[,"other","msm",] <- int[,"other","msm",] + coef(fit.p.msm)["raceidother"]
    
    int[,"black","heterosexual_male",] <- int[,"black","heterosexual_male",] + coef(fit.p.nonmsm)["raceidblack"]
    int[,"hispanic","heterosexual_male",] <- int[,"hispanic","heterosexual_male",] + coef(fit.p.nonmsm)["raceidhispanic"]
    int[,"other","heterosexual_male",] <- int[,"other","heterosexual_male",] + coef(fit.p.nonmsm)["raceidother"]
    
    int[,"black","female",] <- int[,"black","female",] + coef(fit.p.nonmsm)["raceidblack"]
    int[,"hispanic","female",] <- int[,"hispanic","female",] + coef(fit.p.nonmsm)["raceidhispanic"]
    int[,"other","female",] <- int[,"other","female",] + coef(fit.p.nonmsm)["raceidother"]
    
    int["13-24 years",,"msm",] <- int["13-24 years",,"msm",] + coef(fit.p.msm)["ageidage1"]
    int["25-34 years",,"msm",] <- int["25-34 years",,"msm",] + coef(fit.p.msm)["ageidage2"]
    int["35-44 years",,"msm",] <- int["35-44 years",,"msm",] + coef(fit.p.msm)["ageidage3"]
    int["45-54 years",,"msm",] <- int["45-54 years",,"msm",] + coef(fit.p.msm)["ageidage4"]
    int["55+ years",,"msm",] <- int["55+ years",,"msm",] + coef(fit.p.msm)["ageidage5"]
    
    int["13-24 years",,"heterosexual_male",] <- int["13-24 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage1"]
    int["25-34 years",,"heterosexual_male",] <- int["25-34 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage2"]
    int["35-44 years",,"heterosexual_male",] <- int["35-44 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage3"]
    int["45-54 years",,"heterosexual_male",] <- int["45-54 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage4"]
    int["55+ years",,"heterosexual_male",] <- int["55+ years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage5"]
    
    int["13-24 years",,"female",] <- int["13-24 years",,"female",] + coef(fit.p.nonmsm)["ageidage1"]
    int["25-34 years",,"female",] <- int["25-34 years",,"female",] + coef(fit.p.nonmsm)["ageidage2"]
    int["35-44 years",,"female",] <- int["35-44 years",,"female",] + coef(fit.p.nonmsm)["ageidage3"]
    int["45-54 years",,"female",] <- int["45-54 years",,"female",] + coef(fit.p.nonmsm)["ageidage4"]
    int["55+ years",,"female",] <- int["55+ years",,"female",] + coef(fit.p.nonmsm)["ageidage5"]
    
    int[,,"female",] <- int[,,"female",] + coef(fit.p.nonmsm)["female"]
    # int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit2.big.df)["nonmsm"]
    # int[,,"msm",] <- int[,,"msm",] + 0 # msm is sexrisk REF
    
    # int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.p.nonmsm)["idu"]
    int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.p.nonmsm)["idu"]
    int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.p.nonmsm)["idu"] #coef(fit2.big.df)[14] (NA)

    slope[,,"msm",] <- slope[,,"msm",] + coef(fit.p.msm)["year"]
    slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit.p.nonmsm)["year"]
    slope[,,"female",] <- slope[,,"female",] + coef(fit.p.nonmsm)["year"]
    
    # slope[,,"female",] <- slope[,,"female",] + coef(fit2.big.df)["year:nonmsm"]
    # slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit2.big.df)["year:nonmsm"]
    
    # 
    # # Make and return the functional form object
    # create.logistic.linear.functional.form(
    #     intercept = int,
    #     slope = slope,
    #     anchor.year = anchor.year,
    #     min = 0,
    #     max = max.prep.coverage,
    #     parameters.are.on.logit.scale = T
    # )

    create.logistic.tail.functional.form(
      # everything else the same
      intercept = pmax(int,0),
      slope = slope,
      anchor.year = anchor.year,
      min = 0,
      max = max.prep.coverage,
      # parameters.are.on.logit.scale = T,
      logistic.after.frac.of.span = 0.5,
      parameters.are.on.transformed.scale = F
    )

    # create.linear.functional.form(
    #   intercept = int,
    #   slope = slope,
    #   anchor.year = anchor.year,
    #   min = 0,
    #   max = max.prep.coverage
    # )
}

get.prep.indication.functional.form <- function(specification.metadata){
  
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.indication = 0.85 # modify as you see fit
  
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
  
  # Intercepts and slopes
  int[,,"msm",] <- int[,,"msm",] + coef(fit.pi.msm)["(Intercept)"]
  
  int[,,"heterosexual_male","never_IDU"] <- int[,,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["(Intercept)"]
  int[,,"female","never_IDU"] <- int[,,"female","never_IDU"] + coef(fit.pi.het)["(Intercept)"]
  
  int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["(Intercept)"]
  int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.pi.idu)["(Intercept)"]
  
  int[,"black","msm",] <- int[,"black","msm",] + coef(fit.pi.msm)["raceidblack"]
  int[,"hispanic","msm",] <- int[,"hispanic","msm",] + coef(fit.pi.msm)["raceidhisp"]
  int[,"other","msm",] <- int[,"other","msm",] + coef(fit.pi.msm)["raceidnbnh"]
  
  int[,"black","heterosexual_male","never_IDU"] <- int[,"black","heterosexual_male","never_IDU"] + coef(fit.pi.het)["raceidblack"]
  int[,"hispanic","heterosexual_male","never_IDU"] <- int[,"hispanic","heterosexual_male","never_IDU"] + coef(fit.pi.het)["raceidhisp"]
  int[,"other","heterosexual_male","never_IDU"] <- int[,"other","heterosexual_male","never_IDU"] + coef(fit.pi.het)["raceidnbnh"]
  
  int[,"black","female","never_IDU"] <- int[,"black","female","never_IDU"] + coef(fit.pi.het)["raceidblack"]
  int[,"hispanic","female","never_IDU"] <- int[,"hispanic","female","never_IDU"] + coef(fit.pi.het)["raceidhisp"]
  int[,"other","female","never_IDU"] <- int[,"other","female","never_IDU"] + coef(fit.pi.het)["raceidnbnh"]
  
  int[,"black","heterosexual_male","active_IDU"] <- int[,"black","heterosexual_male","active_IDU"] + coef(fit.pi.idu)["raceidblack"]
  int[,"hispanic","heterosexual_male","active_IDU"] <- int[,"hispanic","heterosexual_male","active_IDU"] + coef(fit.pi.idu)["raceidhisp"]
  int[,"other","heterosexual_male","active_IDU"] <- int[,"other","heterosexual_male","active_IDU"] + coef(fit.pi.idu)["raceidnbnh"]
  
  int[,"black","female","active_IDU"] <- int[,"black","female","active_IDU"] + coef(fit.pi.idu)["raceidblack"]
  int[,"hispanic","female","active_IDU"] <- int[,"hispanic","female","active_IDU"] + coef(fit.pi.idu)["raceidhisp"]
  int[,"other","female","active_IDU"] <- int[,"other","female","active_IDU"] + coef(fit.pi.idu)["raceidnbnh"]
  
  int["13-24 years",,"msm",] <- int["13-24 years",,"msm",] + coef(fit.pi.msm)["ageidage1"]
  int["25-34 years",,"msm",] <- int["25-34 years",,"msm",] + coef(fit.pi.msm)["ageidage2"]
  int["35-44 years",,"msm",] <- int["35-44 years",,"msm",] + coef(fit.pi.msm)["ageidage3"]
  int["45-54 years",,"msm",] <- int["45-54 years",,"msm",] + coef(fit.pi.msm)["ageidage4"]
  int["55+ years",,"msm",] <- int["55+ years",,"msm",] + coef(fit.pi.msm)["ageidage5"]
  
  int["13-24 years",,"heterosexual_male","never_IDU"] <- int["13-24 years",,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["ageidage1"]
  int["25-34 years",,"heterosexual_male","never_IDU"] <- int["25-34 years",,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["ageidage2"]
  int["35-44 years",,"heterosexual_male","never_IDU"] <- int["35-44 years",,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["ageidage3"]
  int["45-54 years",,"heterosexual_male","never_IDU"] <- int["45-54 years",,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["ageidage4"]
  int["55+ years",,"heterosexual_male","never_IDU"] <- int["55+ years",,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["ageidage5"]
  
  int["13-24 years",,"female","never_IDU"] <- int["13-24 years",,"female","never_IDU"] + coef(fit.pi.het)["ageidage1"]
  int["25-34 years",,"female","never_IDU"] <- int["25-34 years",,"female","never_IDU"] + coef(fit.pi.het)["ageidage2"]
  int["35-44 years",,"female","never_IDU"] <- int["35-44 years",,"female","never_IDU"] + coef(fit.pi.het)["ageidage3"]
  int["45-54 years",,"female","never_IDU"] <- int["45-54 years",,"female","never_IDU"] + coef(fit.pi.het)["ageidage4"]
  int["55+ years",,"female","never_IDU"] <- int["55+ years",,"female","never_IDU"] + coef(fit.pi.het)["ageidage5"]
  
  int["13-24 years",,"heterosexual_male","active_IDU"] <- int["13-24 years",,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["ageidage1"]
  int["25-34 years",,"heterosexual_male","active_IDU"] <- int["25-34 years",,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["ageidage2"]
  int["35-44 years",,"heterosexual_male","active_IDU"] <- int["35-44 years",,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["ageidage3"]
  int["45-54 years",,"heterosexual_male","active_IDU"] <- int["45-54 years",,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["ageidage4"]
  int["55+ years",,"heterosexual_male","active_IDU"] <- int["55+ years",,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["ageidage5"]
  
  int["13-24 years",,"female","active_IDU"] <- int["13-24 years",,"female","active_IDU"] + coef(fit.pi.idu)["ageidage1"]
  int["25-34 years",,"female","active_IDU"] <- int["25-34 years",,"female","active_IDU"] + coef(fit.pi.idu)["ageidage2"]
  int["35-44 years",,"female","active_IDU"] <- int["35-44 years",,"female","active_IDU"] + coef(fit.pi.idu)["ageidage3"]
  int["45-54 years",,"female","active_IDU"] <- int["45-54 years",,"female","active_IDU"] + coef(fit.pi.idu)["ageidage4"]
  int["55+ years",,"female","active_IDU"] <- int["55+ years",,"female","active_IDU"] + coef(fit.pi.idu)["ageidage5"]
  
  int[,,"female","never_IDU"] <- int[,,"female","never_IDU"] + coef(fit.pi.het)["female"]
  int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.pi.idu)["female"]
  
  # int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.pi.nonmsm)["idu"] 
  
  slope[,,"msm",] <- slope[,,"msm",] + coef(fit.pi.msm)["years"]
  
  slope[,,"heterosexual_male","never_IDU"] <- slope[,,"heterosexual_male","never_IDU"] + coef(fit.pi.het)["years"]
  slope[,,"female","never_IDU"] <- slope[,,"female","never_IDU"] + coef(fit.pi.het)["years"]
  
  slope[,,"heterosexual_male","active_IDU"] <- slope[,,"heterosexual_male","active_IDU"] + coef(fit.pi.idu)["years"]
  slope[,,"female","active_IDU"] <- slope[,,"female","active_IDU"] + coef(fit.pi.idu)["years"]
  
  # Make and return the functional form object
  create.logistic.linear.functional.form(
    intercept = int,
    slope = slope,
    anchor.year = anchor.year,
    min = 0,
    max = max.prep.indication,
    parameters.are.on.logit.scale = T
  )
  
}

get.prep.persistence.functional.form <- function(specification.metadata)
{
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.persistence = 0.8 # modify as you see fit
  
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
  
  
  # Intercepts and slopes
  
  int[,,,] <- int[,,,] + coef(fit.pp)["(Intercept)"]
  
  int[,"black",,] <- int[,"black",,] + coef(fit.pp)["raceidblack"]
  int[,"hispanic",,] <- int[,"hispanic",,] + coef(fit.pp)["raceidhispanic"]
  int[,"other",,] <- int[,"other",,] + coef(fit.pp)["raceidother"]
  
  int["13-24 years",,,] <- int["13-24 years",,,] + coef(fit.pp)["ageidage1"]
  int["25-34 years",,,] <- int["25-34 years",,,] + coef(fit.pp)["ageidage2"]
  int["35-44 years",,,] <- int["35-44 years",,,] + coef(fit.pp)["ageidage3"]
  int["45-54 years",,,] <- int["45-54 years",,,] + coef(fit.pp)["ageidage4"]
  int["55+ years",,,] <- int["55+ years",,,] + coef(fit.pp)["ageidage5"]
  
  int[,,"msm",] <- int[,,"msm",] + coef(fit.pp)["riskidmsm"]
  int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.pp)["riskidhet"]
  int[,,"female",] <- int[,,"female",] + coef(fit.pp)["riskidhet"]
  int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.pp)["riskididu"]
  
  # slope[,,,] <- slope[,,,] + coef(fit.pp)["years"]
  
  # Make and return the functional form object
  # create.logistic.linear.functional.form(
  #   intercept = int,
  #   slope = slope,
  #   anchor.year = anchor.year,
  #   min = 0,
  #   max = max.prep.persistence, 
  #   parameters.are.on.logit.scale = T
  # ) 
  
  create.linear.functional.form(
    intercept = int,
    slope = slope,
    anchor.year = anchor.year,
    min = 0,
    max = max.prep.persistence, 
  )
  
}
