
YEARS = as.character(2025:2035)
INT.CODES = c('cdct.end','cdct.pintr','cdct.bintr')

abs.inc.noint = apply(total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum)

abs.excess.inc = apply(total.results[YEARS,,'incidence',,INT.CODES] - as.numeric(total.results[YEARS,,'incidence',,'noint']), c('sim', 'location', 'intervention'), sum)
rel.excess.inc = abs.excess.inc / as.numeric(apply(total.results[YEARS,,'incidence',,'noint'], c('sim', 'location'), sum))



abs.inc.noint.means = apply(abs.inc.noint, c('location'), mean)
abs.inc.noint.lowers = apply(abs.inc.noint, c('location'), quantile, probs=0.025)
abs.inc.noint.uppers = apply(abs.inc.noint, c('location'), quantile, probs=0.975)

abs.excess.inc.means = apply(abs.excess.inc, c('location','intervention'), mean)
abs.excess.inc.lowers = apply(abs.excess.inc, c('location','intervention'), quantile, probs=0.025)
abs.excess.inc.uppers = apply(abs.excess.inc, c('location','intervention'), quantile, probs=0.975)

rel.excess.inc.means = apply(rel.excess.inc, c('location','intervention'), mean)
rel.excess.inc.lowers = apply(rel.excess.inc, c('location','intervention'), quantile, probs=0.025)
rel.excess.inc.uppers = apply(rel.excess.inc, c('location','intervention'), quantile, probs=0.975)



format(round(c(abs.inc.noint.means['Total'],
  abs.inc.noint.lowers['Total'],
  abs.inc.noint.uppers['Total'])), big.mark=',')


format(round(c(abs.excess.inc.means['Total',1],
               abs.excess.inc.lowers['Total',1],
               abs.excess.inc.uppers['Total',1])), big.mark=',')

round(100*c(rel.excess.inc.means['Total',1],
        rel.excess.inc.lowers['Total',1],
        rel.excess.inc.uppers['Total',1]),1)

