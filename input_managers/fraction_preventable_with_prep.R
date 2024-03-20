##--------------------------------------------##
##-- Proportion of avoidable HIV infections --##
##--------------------------------------------##

# i.e., proportion of new infections that could have been indicated for PrEP, via the following definitions: 

# MSM: >1 partner in the past year & condomless sex
    # mean = 0.9529412
    # SD =  1.952374

# Heterosexual: STI in the past year
    # mean = 0.3447421
    # SD =  37.55647

# IDU: (assuming ~100% avoidable)
    # mean = 0.95 (assumption)
    # SD =  ?


##---------------##
##----  MSM  ----##
##---------------##
# Celum et al, 2001, Table 1: https://academic.oup.com/jid/article/183/1/23/813951 
msm.with.risk.behaviors = (4 + 54 + 7 + 14 + 2)
total.msm = 85
msm.proportion.avoidable.mean = msm.with.risk.behaviors/total.msm 
msm.proportion.avoidable.sd = sqrt(total.msm*msm.proportion.avoidable.mean*(1-msm.proportion.avoidable.mean)) # sqrt(np(1-p))


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
het.proportion.avoidable.mean = (het.proportion.avoidable.mean.1 + het.proportion.avoidable.mean.2)/2 # 12.9%
total.het = total.het.1 + total.het.2

# to account for growth in STI prevalence over time, double this estimate
het.proportion.avoidable.mean = het.proportion.avoidable.mean*2 # --> 25% 

# assuming 75% of heterosexuals are on PrEP because of an STI, so: 
# total.p.on.prep * 0.75 = p.on.prep.due.to.sti
# total.p.on.prep = p.on.prep.due.to.sti / 0.75
het.proportion.avoidable.mean = het.proportion.avoidable.mean/0.75 # --> 34%
het.proportion.avoidable.sd = sqrt(total.het*het.proportion.avoidable.mean*(1-het.proportion.avoidable.mean))


##---------------##
##----  IDU  ----##
##---------------##
idu.proportion.avoidable.mean = 0.95 # assuming ~100% 
idu.proportion.avoidable.sd ## ?? 
