# depression parameters

DEP.PARAMETERS.PRIOR <- join.distributions(
  
  #-- Depression characteristics --#
  depression.proportion.tx <- Lognormal.Distribution(0, log(2)), 
  depression.length        <- Lognormal.Distribution(log(6/12), log(2)), 
  
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
  rr.testing.depressed <- Lognormal.Distribution(log(1.4), log(2)), 
)