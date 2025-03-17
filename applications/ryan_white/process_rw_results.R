

load('Q:results/ryan_white/ryan_white_results_2025-03-13.Rdata')

total.dx = apply(full.results[,,,,,,'new',,], c('year','sim','location','intervention'), sum, na.rm=T)

YEARS.TO.CONSIDER = as.character(2025:2030)

tot.inf.noint = apply(total.dx[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
tot.inf.loseRW = apply(total.dx[YEARS.TO.CONSIDER,,,'loseRW',drop=F], c('sim'), sum, na.rm=T)
tot.inf.temploseRW = apply(total.dx[YEARS.TO.CONSIDER,,,'temploseRW',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections if Ryan White Continues: ", 
             format(round(mean(tot.inf.noint, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Infections if Ryan White Ends Indefinitely: ", 
             format(round(mean(tot.inf.loseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.loseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.loseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Infections if Ryan White Ends Indefinitely: ", 
             format(round(mean(tot.inf.temploseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.temploseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.temploseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))


tot.dx.noint = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
tot.dx.loseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'loseRW',drop=F], c('sim'), sum, na.rm=T)
tot.dx.temploseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'temploseRW',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Diagnoses if Ryan White Continues: ", 
             format(round(mean(tot.dx.noint, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.dx.noint, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.dx.noint, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Diagnoses if Ryan White Ends Indefinitely: ", 
             format(round(mean(tot.dx.loseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.dx.loseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.dx.loseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Diagnoses if Ryan White Ends Indefinitely: ", 
             format(round(mean(tot.dx.temploseRW, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.dx.temploseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.dx.temploseRW, probs=.975, na.rm=T)), big.mark=','),
             "]"))


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



abs.total.diagnoses.averted.loseRW = apply(total.dx[YEARS.TO.CONSIDER,,,'loseRW',drop=F] - total.dx[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
rel.total.diagnoses.averted.loseRW = abs.total.diagnoses.averted.loseRW  / apply(total.dx[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

abs.total.diagnoses.averted.temploseRW = apply(total.dx[YEARS.TO.CONSIDER,,,'temploseRW',drop=F] - total.dx[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)
rel.total.diagnoses.averted.temploseRW = abs.total.diagnoses.averted.temploseRW  / apply(total.dx[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)


print(paste0("Relative Increase in Diagnoses if Permanently Lose Ryan White: ", 
             format(round(mean(100*rel.total.diagnoses.averted.loseRW, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.diagnoses.averted.loseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.diagnoses.averted.loseRW, probs=.975)), big.mark=','),
             "%]"))

print(paste0("Relative Increase in Diagnoses if Lose Ryan White until 2029: ", 
             format(round(mean(100*rel.total.diagnoses.averted.temploseRW, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.diagnoses.averted.temploseRW, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.diagnoses.averted.temploseRW, probs=.975, na.rm=T)), big.mark=','),
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



      