
source('commoncode/results_helpers.R')
source('presentation/make_pretty_table.R')


YEARS = as.character(2025:2030)
INT.CODES = c('cdct.end','cdct.pintr','cdct.bintr')

abs.excess.inc = apply(total.results[YEARS,,'incidence',,INT.CODES] - as.numeric(total.results[YEARS,,'incidence',,'noint']), c('sim', 'location', 'intervention'), sum)
abs.cdc.tests.saved = -apply(total.results[YEARS,,'cdc.funded.tests',,INT.CODES] - as.numeric(total.results[YEARS,,'cdc.funded.tests',,'noint']), c('sim', 'location', 'intervention'), sum)

cbind(colMeans(abs.cdc.tests.saved)['Total',],
      apply(abs.cdc.tests.saved, 2:3, quantile, probs=0.025)['Total',],
      apply(abs.cdc.tests.saved, 2:3, quantile, probs=0.975)['Total',]) / 1000000

tests.per.excess.inf = abs.cdc.tests.saved / abs.excess.inc

tests.per.excess.inf.means = apply(tests.per.excess.inf, c('location','intervention'), mean)
tests.per.excess.inf.lowers = apply(tests.per.excess.inf, c('location','intervention'), quantile, probs=0.025)
tests.per.excess.inf.uppers = apply(tests.per.excess.inf, c('location','intervention'), quantile, probs=0.975)


#-- Order them --#

n.states = dim(tests.per.excess.inf)['location'] - 1
total.index = n.states + 1
o = c(order(tests.per.excess.inf.means[1:n.states,1], decreasing = F), total.index)

#-- Make formatted means --#

formatted.means = sapply(round(tests.per.excess.inf.means), format, big.mark=',')
dim(formatted.means) = dim(tests.per.excess.inf.means)
dimnames(formatted.means) = dimnames(tests.per.excess.inf.means)

#-- Make formatted CIs --#

formatted.cis = paste0(
    "(",
    sapply(round(tests.per.excess.inf.lowers), format, big.mark=','),
    " - ",
    sapply(round(tests.per.excess.inf.uppers), format, big.mark=','),
    ")"
)
dim(formatted.cis) = dim(tests.per.excess.inf.lowers)
dimnames(formatted.cis) = dimnames(tests.per.excess.inf.lowers)

#-- Put it together --#

formatted.means.and.cis = interleave.rows(formatted.means[o,], formatted.cis[o,])

#-- Make color.by table --#

tsfx = log

color.by = tsfx(interleave.rows(tests.per.excess.inf.means[o,], tests.per.excess.inf.means[o,]))

#-- Write it --#

LEAST.EFFICIENT.COLOR = paste0("#", rgb.to.hex(c(222,237,207)))
MOST.EFFICIENT.COLOR = paste0("#", rgb.to.hex(c(10,47,81)))

# LEAST.EFFICIENT.COLOR = paste0("#", rgb.to.hex(c(226,230,226)))
# MOST.EFFICIENT.COLOR = paste0("#", rgb.to.hex(c(142,19,59)))

write.shaded.table(tab = formatted.means.and.cis,
                   color.by = color.by,
                   colors = c(MOST.EFFICIENT.COLOR, LEAST.EFFICIENT.COLOR),
                   thresholds = tsfx(c(75,4500)),
                   file = '../../results/cdc_testing/tests_per_excess_infection.xlsx')
