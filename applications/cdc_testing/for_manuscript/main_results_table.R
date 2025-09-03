
source('commoncode/results_helpers.R')
source('presentation/make_pretty_table.R')


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

#-- Set up the ordering --#
n.states = dim(abs.excess.inc)['location'] - 1
total.index = n.states + 1
o = c(order(rel.excess.inc.means[1:n.states], decreasing = T), total.index)


#-- Make the means table, formatted --#
rel.excess.inc.means.formatted = 
    paste0(sapply(round(100*rel.excess.inc.means, 1), format, nsmall = 1), "%")
dim(rel.excess.inc.means.formatted) = dim(rel.excess.inc.means)

abs.excess.inc.means.formatted = sapply(round(abs.excess.inc.means), format, big.mark=',')
dim(abs.excess.inc.means.formatted) = dim(abs.excess.inc.means)

means.table = cbind(
    sapply(round(abs.inc.noint.means), format, big.mark=','),
    interleave.columns(
            abs.excess.inc.means.formatted,
            rel.excess.inc.means.formatted
    )
)

#-- Make the CIs table, formatted --#

abs.excess.inc.ci = 
    paste0("(", 
           sapply(round(abs.excess.inc.lowers), format, big.mark=','),
           " - ",
           sapply(round(abs.excess.inc.uppers), format, big.mark=','),
           ")")
dim(abs.excess.inc.ci) = dim(abs.excess.inc.lowers)


rel.excess.inc.ci = 
    paste0("(", 
           sapply(round(100*rel.excess.inc.lowers, 1), format, nsmall=1),
           " - ",
           sapply(round(100*rel.excess.inc.uppers, 1), format, nsmall=1),
           "%)")
dim(rel.excess.inc.ci) = dim(rel.excess.inc.lowers)

cis.table = cbind(
    paste0("(", 
           sapply(round(abs.inc.noint.lowers), format, big.mark=','),
           " - ",
           sapply(round(abs.inc.noint.uppers), format, big.mark=','),
           ")"),
    interleave.columns(
        abs.excess.inc.ci,
        rel.excess.inc.ci
    )
)

#-- Put means and CIs togeter --#

means.and.cis.table = interleave.rows(
    means.table[o,],
    cis.table[o,]
)

#-- Make the color by table --#

color.by = cbind(
    -1, # absolute inc does not get color
    interleave.columns(
        interleave.rows(rel.excess.inc.means[o,], rel.excess.inc.means[o,]),
        interleave.rows(rel.excess.inc.means[o,], rel.excess.inc.means[o,])
    )
)

#-- Write the table --#

write.shaded.table(tab = means.and.cis.table,
                   color.by = color.by,
                   colors = c('white','yellow','red'),
                   thresholds = c(-1,0,0.3),
                   file = paste0('../../results/cdc_testing/main_table_', YEARS[length(YEARS)], '.xlsx'))


