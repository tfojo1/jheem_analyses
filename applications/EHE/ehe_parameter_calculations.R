
##--------------------------------------------------##
##-- This file contains calculations that we make --##
##-- to get the distributions for parameters      --##
##-- based off of public literature               --##
##--------------------------------------------------##


##-------------------------##
##-- AIDS Mortality Rate --##
##-------------------------##

# https://www.sciencedirect.com/science/article/pii/S0033350616300646?via%3Dihub

aids.death.data = data.frame(
  years = c(2,4,6),
  estimate = c(0.48,0.26,0.18),
  lower = c(0.31,0.19,0.08),
  upper = c(0.65,0.34,0.29)
)

est.rate = -log(aids.death.data$estimate)/aids.death.data$years
rate.upper = -log(aids.death.data$lower)/aids.death.data$years
rate.lower = -log(aids.death.data$upper)/aids.death.data$years


mean.rate = mean(est.rate)

log.sd.per = (log(rate.upper) - log(rate.lower)) / 2 / qnorm(0.975)




mean.log.sd = mean(log.sd.per)


# https://pubmed.ncbi.nlm.nih.gov/8100272/
survival.2y = data.frame(
  p = c(cd4.lt50=0.257,
        cd4.50.to.100 = 0.514,
        cd4.100.to.150 = 0.673,
        cd4.150.to.200 = 0.765),
  n = c(552, 252, 180, 186),
  se = c(.03, .05, .05, .05)
)

naive.mean = mean(-log(survival.2y$p)/2)
naive.pooled.cv = mean(survival.2y$se / survival.2y$p) * 2 # multiply by 2 for the weighting

overall.mortality.rate = mean(c(mean.rate, naive.mean))
overall.log.sd = mean(c(mean.log.sd, naive.pooled.cv))


# With ART
aids.death.data = data.frame(
  years = c(2,4,6,8,10),
  estimate = c(0.87,0.86,0.78,0.78,0.61),
  lower = c(0.84,0.84,0.72,0.71,0.47),
  upper = c(0.90,0.89,0.84,0.85,0.74)
)

est.rate = -log(aids.death.data$estimate)/aids.death.data$years
rate.upper = -log(aids.death.data$lower)/aids.death.data$years
rate.lower = -log(aids.death.data$upper)/aids.death.data$years


mean.rate = mean(est.rate)

log.sd.per = (log(rate.upper) - log(rate.lower)) / 2 / qnorm(0.975)




mean.log.sd = mean(log.sd.per)


##-----------------------##
##-- CD4 Recovery Rate --##
##--   <200 to >200    --##
##-----------------------##

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6544279/

mean.time.to.recovery = 1.5
sd.time.to.recovery = 1.1
n.time.to.recovery = 107706 + 391076 + 358258

rate.ratio.suppressed.vs.not = c(267/201, 327/248, 359/290)
rate.ratio.male.vs.female.lt50 = c(192/159, 262/213, 301/240)
rate.ratio.male.vs.female.50.to.200 = c(305/264, 364/305, 397/323)

# What we use for the rate
mean(rate.ratio.suppressed.vs.not)/mean.time.to.recovery

# What we use to get the SD
mean(c(rate.ratio.suppressed.vs.not, rate.ratio.male.vs.female.lt50, rate.ratio.male.vs.female.50.to.200)) # use this to get the sd - the ~95% CI should cover this fold-change


##--------------------------------------##
##-- Progression to Low CD4 (ie <200) --##
##--------------------------------------##

median.time.to.progression = data.frame(
  estimate = c(
    7.93, # https://pubmed.ncbi.nlm.nih.gov/21921225/
    8,# https://pubmed.ncbi.nlm.nih.gov/11144640/ (range 8-10 y)
    10 # https://pubmed.ncbi.nlm.nih.gov/11144640/ (range 8-10 y)
  ),
  weight = c(1, 0.5, 0.5)
)
mean.time.to.progression = data.frame(
  estimate = c(
    10, # https://www.sciencedirect.com/science/article/pii/S0140673696062319?via%3Dihub (range 10-11 y)
    11 # https://www.sciencedirect.com/science/article/pii/S0140673696062319?via%3Dihub (range 10-11 y)
  ),
  weight = c(0.5, 0.5)
)

progression.rate = c(log(2)/median.time.to.progression$estimate,
         1/mean.time.to.progression$estimate)
progression.rate.weight = c(median.time.to.progression$weight, mean.time.to.progression$weight)

mean.rate = sum(progression.rate * progression.rate.weight) / sum(progression.rate.weight)
sd.rate = sqrt(sum(progression.rate.weight*(progression.rate-mean.rate)^2)/(sum(progression.rate.weight)-1))

log.mean.rate = sum(log(progression.rate) * progression.rate.weight) / sum(progression.rate.weight)
log.sd.rate = sqrt(sum(progression.rate.weight*(log(progression.rate)-log.mean.rate)^2)/(sum(progression.rate.weight)-1))


##-----------------------------------##
##--   Time to Development of OI   --##
##--                               --##
##-- (Which we will use as a proxy --##
##--    for rate of diagnosis in   --##
##--       low-CD4 patients)       --##
##-----------------------------------##

# https://academic.oup.com/ije/article/30/4/864/705938?login=false

ir = cbind(
  c(1.6,3.1,6.7,11.4),
  c(1.1,2.0,3.9,12.6),
  c(0.5,1.4,1.7,6.8),
  c(03,1.0,1.9,9.5),
  c(0.7,0.9,1.1,3.1),
  c(1.0,1.4,3.4,11.0),
  c(1.6,1.9,1.7,4.5)
)

ir.lower = cbind(
  c(1.0,3.1,6.7,11.7),
  c(0.7,1.3,2.3,9.7),
  c(0.3,0.8,0.9,5.0),
  c(0.1,0.6,1.0,7.4),
  c(0.3,0.4,0.7,2.0),
  c(0.6,0.8,2.0,8.7),
  c(0.4,0.4,0.0,1.2)
)

ir.upper = cbind(
  c(2.5,4.6,11.7,17.3),
  c(1.8,2.0,6.6,16.3),
  c(1.0,2.2,3.3,9.1),
  c(0.7,1.7,3.6,12.2),
  c(1.3,1.9,2.3,4.8),
  c(1.7,2.2,5.5,13.9),
  c(4.1,5.4,9.4,11.5)
)

py = cbind(
  c(1236,764,187,227),
  c(1513,1156,358,453),
  c(1671,1376,478,650),
  c(1673,1376,478,632),
  c(1655,1343,465,640),
  c(1656,1363,470,635),
  c(248,161,59,89)
)

combined.ir.1 = sum(rowSums(ir) * rowSums(py)) / sum(py) / 100
combined.ir.1.lower = sum(rowSums(ir.lower) * rowSums(py)) / sum(py) / 100
combined.ir.1.upper = sum(rowSums(ir.upper) * rowSums(py)) / sum(py) / 100


# We're going to use THIS
#   (and ignore the calculations above)
# In the absence of ART, the median time to an AIDS-defining condition once the CD4 cell count is below 200 cells/microL is estimated at 12 to 18 months
median.time.lower = 1
median.time.upper = 1.5

rate.upper = log(2)/median.time.lower
rate.lower = log(2)/median.time.upper

log.mean.rate = mean(log(c(rate.upper, rate.lower)))
log.sd.rate = (log(rate.upper)-log(rate.lower)) / 2 / qnorm(0.975)


##-- Acute HIV Testing Rate --##
##-- Calculated as Diagnosis rate 
##--  among acutely infected minus
##-- overall diagnosis rate
##

# https://pubmed.ncbi.nlm.nih.gov/31813966/
median.time.to.diagnosis = 3.3
overall.dx.rate = log(2)/median.time.to.diagnosis

# https://www.nyc.gov/assets/doh/downloads/pdf/dires/hiv-surveillance-annualreport-2015.pdf
n.acute = 158+4+18+26
n.total = 2493
p.acute = n.acute/n.total
r.acute = -log(1-p.acute) * (12/2.9)

delta.r.acute.vs.total = r.acute - overall.dx.rate
