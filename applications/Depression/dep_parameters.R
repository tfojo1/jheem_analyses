# depression parameters

DEP.PARAMETERS.PRIOR <- join.distributions(
  # + age categories (5), + race (3), + sex (3), + IDU status (3 - never, prior, current) // differences supported by data, where possible // can do relative
  # + any interactions? (unlikely)
  # for incidence and Tx / potentially also for length of depression episode (rate of remission, or probability of experiencing remission in a year)
  
  #-- Depression characteristics --#
  depression.length   <- Lognormal.Distribution(log(6/12), log(2)/2), 
  
  #-Depression treatment
  age1.depression.proportion.tx     <- Lognormal.Distribution(log(.18), log(2)), ## logit function!
  age2.depression.proportion.tx     <- Lognormal.Distribution(log(.18), log(2)),
  age3.depression.proportion.tx     <- Lognormal.Distribution(log(.18), log(2)), ## make relative to age 3, 35-44yo?
  age4.depression.proportion.tx     <- Lognormal.Distribution(log(.18), log(2)),
  age5.depression.proportion.tx     <- Lognormal.Distribution(log(.18), log(2)),
  
  white.depression.proportion.tx    <- Lognormal.Distribution(log(.18), log(2)), ## make relative to white?
  black.depression.proportion.tx    <- Lognormal.Distribution(log(.18), log(2)),
  hispanic.depression.proportion.tx <- Lognormal.Distribution(log(.18), log(2)),
  
  msm.depression.proportion.tx      <- Lognormal.Distribution(log(.18), log(2)),
  hetero.depression.proportion.tx   <- Lognormal.Distribution(log(.18), log(2)),
  female.depression.proportion.tx   <- Lognormal.Distribution(log(.18), log(2)), ## make relative to female?
  
  idu.prior.depression.proportion.tx    <- Lognormal.Distribution(log(.18), log(2)),
  idu.current.depression.proportion.tx  <- Lognormal.Distribution(log(.18), log(2)),
  idu.never.depression.proportion.tx    <- Lognormal.Distribution(log(.18), log(2)),
  
  #-Incidence
  age1.depression.incidence     <- Lognormal.Distribution(log(.33*1/2), log(2)), # 13-24
  age2.depression.incidence     <- Lognormal.Distribution(log(.28*1/2), log(2)), # 25-34
  age3.depression.incidence     <- Lognormal.Distribution(log(.28*1/2), log(2)), # 35-44
  age4.depression.incidence     <- Lognormal.Distribution(log(.28*1/2), log(2)), # 45-54
  age5.depression.incidence     <- Lognormal.Distribution(log(.15*1/2), log(2)), # 55+
  
  white.depression.incidence    <- Lognormal.Distribution(log(0.091/(6/12)), log(2)), # no difference from general population by race
  black.depression.incidence    <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  hispanic.depression.incidence <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  
  msm.depression.incidence      <- Lognormal.Distribution(log(0.091/(6/12)*1.5), log(2)), # increased risk, like females *
  hetero.depression.incidence   <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  female.depression.incidence   <- Lognormal.Distribution(log(0.091/(6/12)*1.5), log(2)), # females 1.5-2 times more likely to have depression than males
  
  idu.prior.depression.incidence    <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  idu.current.depression.incidence  <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  idu.never.depression.incidence    <- Lognormal.Distribution(log(0.091/(6/12)), log(2)),
  
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