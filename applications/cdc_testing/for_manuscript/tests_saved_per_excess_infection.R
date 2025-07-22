
#load("Q:results/cdc_testing/cdc_testing_results_2025-07-16.Rdata")

YEARS = as.character(2025:2030)

abs.excess.inc.end = apply(total.results[YEARS,,'incidence',,'cdct.end'] - total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum)
abs.excess.cdc.tests.end = apply(total.results[YEARS,,'cdc.funded.tests',,'noint'] - total.results[YEARS,,'cdc.funded.tests',,'cdct.end'], c('sim', 'location'), sum)

tests.per.excess.inf.end = abs.excess.cdc.tests.end / abs.excess.inc.end
sort(apply(tests.per.excess.inf.end, 'location', mean))


abs.excess.inc.bintr = apply(total.results[YEARS,,'incidence',,'cdct.bintr'] - total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum)
abs.excess.cdc.tests.bintr = apply(total.results[YEARS,,'cdc.funded.tests',,'noint'] - total.results[YEARS,,'cdc.funded.tests',,'cdct.bintr'], c('sim', 'location'), sum)

tests.per.excess.inf.bintr = abs.excess.cdc.tests.bintr / abs.excess.inc.bintr
sort(apply(tests.per.excess.inf.bintr, 'location', mean))



abs.excess.inc = apply(total.results[YEARS,,'incidence',,] - as.numeric(total.results[YEARS,,'incidence',,'noint']), c('sim', 'location','intervention'), sum)
abs.excess.cdc.tests = apply(as.numeric(total.results[YEARS,,'cdc.funded.tests',,'noint']) - total.results[YEARS,,'cdc.funded.tests',,], c('sim', 'location','intervention'), sum)
tests.per.excess.inf = abs.excess.cdc.tests / abs.excess.cdc.tests

apply(tests.per.excess.inf, c('location','intervention'), mean)