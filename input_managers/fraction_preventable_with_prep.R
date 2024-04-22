##--------------------------------------------##
##-- Proportion of avoidable HIV infections --##
##--------------------------------------------##

# i.e., proportion of new infections that could have been indicated for PrEP, via the following definitions: 

# MSM: >1 partner in the past year & condomless sex
    # mean = 0.9529412
    # SD =  0.02296911

# IDU: receptive needle sharing 
    # mean = 0.6871166
    # SD =  0.02568016

# Heterosexual: STI in the past year
    # mean = 0.3447421
    # SD =  0.08971986

##---------------##
##----  MSM  ----##
##---------------##
# Celum et al, 2001, Table 1: https://academic.oup.com/jid/article/183/1/23/813951 
msm.with.risk.behaviors = (4 + 54 + 7 + 14 + 2)
total.msm = 85
msm.proportion.avoidable.mean = msm.with.risk.behaviors/total.msm 
msm.proportion.avoidable.sd = sqrt((msm.proportion.avoidable.mean*(1-msm.proportion.avoidable.mean))/total.msm) 
# variance of number of successes = (np(1-p))
# var(p) = p(1-p)/n

##---------------##
##----  IDU  ----##
##---------------##
# https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-11.pdf
idu.with.receptive.sharing = 224
total.idu = 326
idu.proportion.avoidable.mean = idu.with.receptive.sharing/total.idu # assuming ~100% 
idu.proportion.avoidable.sd = sqrt((idu.proportion.avoidable.mean*(1-idu.proportion.avoidable.mean))/total.idu) 

##------------------##
##-- Heterosexual --##
##------------------##
# Miles et al, 2013 (MMWR), Table 2: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4604824/
het.with.STI.1 = 9
total.het.1 = 89
het.proportion.avoidable.mean.1 = het.with.STI.1/total.het.1 # 10% 

# Newman et al, 2019 (Louisiana), Table 1: https://academic.oup.com/cid/article/70/6/1115/5446594 
het.with.STI.2 = 969
total.het.2 = (969 + 5186)
het.proportion.avoidable.mean.2 = het.with.STI.2/total.het.2 # 15%

# Combine the estimates
    # to account for growth in STI prevalence over time, double this estimate, * 2
    # then, assuming 75% of heterosexuals are on PrEP because of an STI, so: 
          # total.p.on.prep * 0.75 = p.on.prep.due.to.sti
          # total.p.on.prep = p.on.prep.due.to.sti / 0.75
    # mean.p = (p1 + p2)/2
    # multiplier = 2/0.75 

het.proportion.avoidable.mean.pre.multiplier = (het.proportion.avoidable.mean.1 + het.proportion.avoidable.mean.2)/2

het.1.var = (het.proportion.avoidable.mean.1*(1-het.proportion.avoidable.mean.1))/total.het.1
het.2.var = (het.proportion.avoidable.mean.2*(1-het.proportion.avoidable.mean.2))/total.het.2
het.var = ((0.5^2) *het.1.var) + ((0.5^2) *het.2.var)

het.multiplier.1 = 2
het.multiplier.2 = 1/0.75

het.multiplier.1.var = 0.25^2 # this is a guess
het.multiplier.2.var = 0.25^2 # (1/75% estimate could range from 50-100% as 95% CI, meaning SD is 12.5%)

het.multiplier = het.multiplier.1*het.multiplier.2

# Combine the variances for the two multipliers
# (from written-out math)
het.multiplier.var = (het.multiplier.1.var*het.multiplier.2.var) + ((het.multiplier.1^2)*het.multiplier.2.var) + 
  ((het.multiplier.2^2)*het.multiplier.1.var)

het.prop.avoidable.mean = het.proportion.avoidable.mean.pre.multiplier*het.multiplier

# Combine the variances for the estimate and the combined multiplier 
het.prop.avoidable.var = (het.var*het.multiplier.var) + ((het.proportion.avoidable.mean.pre.multiplier^2)*het.multiplier.var) + 
  ((het.multiplier^2)*het.var)
het.prop.avoidable.sd = sqrt(het.prop.avoidable.var)


