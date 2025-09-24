
YEARS = as.character(2026:2030)

summed.years.inc = apply(total.incidence[YEARS,,,], c('sim','location','intervention'), sum)

abs.delta.inc = (summed.years.inc[,,2] - summed.years.inc[,,1])
rel.delta.inc = abs.delta.inc / summed.years.inc[,,1]

abs.delta.means = apply(abs.delta.inc, 'location', mean)
abs.delta.lowers = apply(abs.delta.inc, 'location', quantile, probs=0.025)
abs.delta.uppers = apply(abs.delta.inc, 'location', quantile, probs=0.975)

rel.delta.means = apply(rel.delta.inc, 'location', mean)
rel.delta.lowers = apply(rel.delta.inc, 'location', quantile, probs=0.025)
rel.delta.uppers = apply(rel.delta.inc, 'location', quantile, probs=0.975)

o = c(order(rel.delta.means[-length(rel.delta.means)], decreasing = T),
      length(rel.delta.means))

tab = data.frame(
    Location = names(rel.delta.means)[o],
    Estimate = paste0(round(100*rel.delta.means[o], 1), '%'),
    Lower = paste0(round(100*rel.delta.lowers[o], 1), '%'),
    Upper = paste0(round(100*rel.delta.uppers[o], 1), '%')
)

write.csv(tab, file='../../results/nbc/ryan_white_end_cd_incidence_relative_increase.csv', row.names = F)


o = c(order(rel.delta.means[-length(rel.delta.means)], decreasing = T),
      length(rel.delta.means))

tab2 = data.frame(
    Location = names(abs.delta.means)[o],
    Estimate = format(round(abs.delta.means[o]), big.mark=','),
    Lower = format(round(abs.delta.lowers[o]), big.mark=','),
    Upper = format(round(abs.delta.uppers[o]), big.mark=',')
)


write.csv(tab2, file='../../results/nbc/ryan_white_end_cd_incidence_absolute_increase.csv', row.names = F)

