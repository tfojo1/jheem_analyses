

load('Q:results/ryan_white/ryan_white_results_', Sys.Date(), ".Rdata")

YEARS.TO.CONSIDER = as.character(2025:2030)

abs.total.infections.averted.loseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'loseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
rel.total.infections.averted.loseRW = abs.total.infections.averted.loseRW  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

abs.total.infections.averted.temploseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'temploseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
rel.total.infections.averted.temploseRW = abs.total.infections.averted.temploseRW  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections Averted (vs Permanently Lose Ryan White): ", 
             format(round(mean(abs.total.infections.averted.loseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.total.infections.averted.loseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.total.infections.averted.loseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections if Permanently Lose Ryan White: ", 
             format(round(mean(100*rel.total.infections.averted.loseRW, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.infections.averted.loseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.infections.averted.loseRW, probs=.975)), big.mark=','),
             "%]"))

print(paste0("Infections Averted (vs Lose Ryan White until 2029): ", 
             format(round(mean(abs.total.infections.averted.temploseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.total.infections.averted.temploseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.total.infections.averted.temploseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections if Lose Ryan White until 2029: ", 
             format(round(mean(100*rel.total.infections.averted.temploseRW, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.infections.averted.temploseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.infections.averted.temploseRW, probs=.975, na.rm=T)), big.mark=','),
             "%]"))


abs.total.infections.averted.loseRW.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'loseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.loseRW.by.city = abs.total.infections.averted.loseRW.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)

abs.total.infections.averted.temploseRW.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'temploseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.temploseRW.by.city = abs.total.infections.averted.temploseRW.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)

o = order(mean=apply(rel.total.infections.averted.loseRW.by.city, 'location', mean, na.rm=T), decreasing = T)

abs.total.summary.by.city = cbind(mean=apply(abs.total.infections.averted.loseRW.by.city, 'location', mean, na.rm=T)[o],
                                  lower=apply(abs.total.infections.averted.loseRW.by.city, 'location', quantile, probs=.025, na.rm=T)[o],
                                  upper=apply(abs.total.infections.averted.loseRW.by.city, 'location', quantile, probs=.975, na.rm=T)[o])
print(format(round(abs.total.summary.by.city), big.mark=','))

rel.total.summary.by.city = cbind(mean=apply(rel.total.infections.averted.loseRW.by.city, 'location', mean, na.rm=T)[o],
                                  lower=apply(rel.total.infections.averted.loseRW.by.city, 'location', quantile, probs=.025, na.rm=T)[o],
                                  upper=apply(rel.total.infections.averted.loseRW.by.city, 'location', quantile, probs=.975, na.rm=T)[o])
dimnames(rel.total.summary.by.city)[[1]] = get.location.name(dimnames(rel.total.summary.by.city)[[1]])
print(round(100*rel.total.summary.by.city,1))



      