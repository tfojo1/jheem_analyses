# https://www.cdc.gov/mmwr/volumes/71/wr/mm7125a2.htm
# using 2019 data 

## RATIO MEAN: 2.810587
## RATIO VARIANCE: 0.1391234

commercial.lab.tests = sum(9178836,2713628)
# assuming these two commercial labs are quest and labcorp, they conduct 45% of tests: 
              # https://foundersib.com/2018/05/30/labs-diagnostics-news-labcorp-quest/
total.tests = commercial.lab.tests/0.45
total.new.diagnoses = 36940
total.positivity = total.new.diagnoses/total.tests

cdc.tests = sum(1752586,632757)
cdc.new.diagnoses = sum(5374,3556)
cdc.positivity = cdc.new.diagnoses/cdc.tests

cdc.to.total.positivity.ratio = cdc.positivity/total.positivity # 2.68

## need uncertainty around 0.45 estimate - this dominates our uncertainty 

# this is equivalent: 
ratio = (cdc.new.diagnoses/cdc.tests)*(commercial.lab.tests/total.new.diagnoses)*(1/.45)

# assuming lognormality, can just add variances on the log scale 
cdc.term = cdc.new.diagnoses/cdc.tests
cdc.var = (cdc.term*(1-cdc.term))/cdc.tests

log.var.cdc = log((cdc.var/(cdc.term^2))+1)
log.mean.cdc = log(cdc.term) - log.var.cdc/2

mult = .45
mult.var = 0.1^2 # making this up 

log.var.mult = log((mult.var/(mult^2))+1)
log.mean.mult = log(mult) - log.var.mult/2

log.var.lab = 0
log.mean.lab = log(commercial.lab.tests)

log.var.new.diag = 0 
log.mean.new.diag = log(total.new.diagnoses)

log.mean.ratio = log.mean.cdc+log.mean.lab-log.mean.new.diag-log.mean.mult # substract the ones in the denominator (adding negative)
log.var.ratio = log.var.cdc + log.var.mult + log.var.lab + log.var.new.diag # add all of the variances 

mean.ratio = exp(log.mean.ratio + log.var.ratio/2)
var.ratio = (exp(log.var.ratio) - 1)*mean.ratio



