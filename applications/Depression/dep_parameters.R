# depression parameters

DEP.PARAMETERS.PRIOR <- join.distributions(
  # + age categories (5), + race (3), + sex (3), + IDU status (3 - never, prior, current) // differences supported by data // can do relative
  # + any interactions / unlikely
  # for incidence and Tx / potentially also for length of depression episode (rate of remission, or probability of experiencing remission in a year)
  
  #-- Depression characteristics --#
  depression.proportion.tx <- Lognormal.Distribution(0, log(2)), ## logit function!
  depression.length        <- Lognormal.Distribution(log(6/12), log(2)/2), 
  
  #-- Suppression --#
  rr.suppression.dep.tx   <- Lognormal.Distribution(0, log(2)), 
  rr.suppression.dep.notx <- Lognormal.Distribution(0, log(2)), 
  
  #-- Sexual susceptibility --#
  rr.sex.sus.dep.hetmale <- Lognormal.Distribution(log(.88), log(2)), 
  rr.sex.sus.dep.msm     <- Lognormal.Distribution(log(.88), log(2)), 
  rr.sex.sus.dep.female  <- Lognormal.Distribution(log(.72), log(2)), 
  
  #-- IDU susceptibility --#
  rr.idu.dep.tx   <- Lognormal.Distribution(log(.71), log(2)), 
  rr.idu.dep.notx <- Lognormal.Distribution(log(1.67), log(2)), 
  
  #-- HIV Testing --#
  rr.testing.depressed <- Lognormal.Distribution(log(1.4), log(2))
)