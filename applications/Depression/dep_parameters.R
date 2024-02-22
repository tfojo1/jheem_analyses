# depression parameters

DEP.PARAMETERS.PRIOR <- join.distributions(
  # + age categories (5), + race (3), + sex (3), + IDU status (3 - never, prior, current) // differences supported by data, where possible // can do relative
  # + any interactions? (unlikely)
  # for incidence and Tx / potentially also for length of depression episode (rate of remission, or probability of experiencing remission in a year)
  
  #-- Depression characteristics --#
  depression.length   = Lognormal.Distribution(log(6/12), log(2)/2), 
  
  #-Depression treatment
  age1.depression.proportion.tx.or     = Lognormal.Distribution(0, log(2)/2), ## logit function!
  age2.depression.proportion.tx.or     = Lognormal.Distribution(0, log(2)/2),
  age3.depression.proportion.tx.or     = Lognormal.Distribution(0, log(2)/2), ## make relative to age 3, 35-44yo?
  age4.depression.proportion.tx.or     = Lognormal.Distribution(0, log(2)/2),
  age5.depression.proportion.tx.or     = Lognormal.Distribution(0, log(2)/2),
  
  other.depression.proportion.tx.or    = Lognormal.Distribution(0, log(2)/2), ## make relative to white?
  black.depression.proportion.tx.or    = Lognormal.Distribution(0, log(2)/2),
  hispanic.depression.proportion.tx.or = Lognormal.Distribution(0, log(2)/2),
  
  msm.depression.proportion.tx.or      = Lognormal.Distribution(0, log(2)/2),
  hetero.depression.proportion.tx.or   = Lognormal.Distribution(0, log(2)/2),
  female.depression.proportion.tx.or   = Lognormal.Distribution(0, log(2)/2), ## make relative to female?
  
  idu.prior.depression.proportion.tx.or    = Lognormal.Distribution(0, log(2)/2),
  idu.current.depression.proportion.tx.or  = Lognormal.Distribution(0, log(2)/2),
  idu.never.depression.proportion.tx.or    = Lognormal.Distribution(0, log(2)/2),
  
  #-Incidence
  age1.depression.incidence.rr     = Lognormal.Distribution(0, log(2)/2), # 13-24
  age2.depression.incidence.rr     = Lognormal.Distribution(0, log(2)/2), # 25-34
  age3.depression.incidence.rr     = Lognormal.Distribution(0, log(2)/2), # 35-44
  age4.depression.incidence.rr     = Lognormal.Distribution(0, log(2)/2), # 45-54
  age5.depression.incidence.rr     = Lognormal.Distribution(0, log(2)/2), # 55+
  
  other.depression.incidence.rr    = Lognormal.Distribution(0, log(2)/2), # no difference from general population by race
  black.depression.incidence.rr    = Lognormal.Distribution(0, log(2)/2),
  hispanic.depression.incidence.rr = Lognormal.Distribution(0, log(2)/2),
  
  msm.depression.incidence.rr      = Lognormal.Distribution(0, log(2)/2), # increased risk, like females *
  hetero.depression.incidence.rr   = Lognormal.Distribution(0, log(2)/2),
  female.depression.incidence.rr   = Lognormal.Distribution(0, log(2)/2), # females 1.5-2 times more likely to have depression than males
  
  idu.prior.depression.incidence.rr    = Lognormal.Distribution(0, log(2)/2),
  idu.current.depression.incidence.rr  = Lognormal.Distribution(0, log(2)/2),
  idu.never.depression.incidence.rr    = Lognormal.Distribution(0, log(2)/2),
  
  #-- Suppression --#
  rr.suppression.dep.tx   = Lognormal.Distribution(0, log(2)), 
  rr.suppression.dep.notx = Lognormal.Distribution(0, log(2)), 
  
  #-- Sexual susceptibility --#
  rr.sex.sus.dep.hetmale = Lognormal.Distribution(log(.88), log(2)), 
  rr.sex.sus.dep.msm     = Lognormal.Distribution(log(.88), log(2)), 
  rr.sex.sus.dep.female  = Lognormal.Distribution(log(.72), log(2)), 
  
  #-- IDU susceptibility --#
  rr.idu.dep.tx   = Lognormal.Distribution(log(.71), log(2)), 
  rr.idu.dep.notx = Lognormal.Distribution(log(1.67), log(2)), 
  
  #-- HIV Testing --#
  rr.testing.depressed = Lognormal.Distribution(log(1.4), log(2))
)



DEP.PARAMETER.SAMPLING.BLOCKS <- as.list(DEP.PARAMETERS.PRIOR@var.names) # specify the blocks later


