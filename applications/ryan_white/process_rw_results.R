

load('ryan_white_results.Rdata')

YEARS.TO.CONSIDER = as.character(2025:2030)

abs.total.infections.averted.loseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'loseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum)
rel.total.infections.averted.loseRW = abs.total.infections.averted.loseRW  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum)

abs.total.infections.averted.temploseRW = apply(total.incidence[YEARS.TO.CONSIDER,,,'temploseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum)
rel.total.infections.averted.temploseRW = abs.total.infections.averted.temploseRW  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum)

print(paste0("Infections Averted (vs Permanently Lose Ryan White): ", 
             format(round(mean(abs.total.infections.averted.loseRW)), big.mark=','),
             " [",
             format(round(quantile(abs.total.infections.averted.loseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(abs.total.infections.averted.loseRW, probs=.975)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections if Permanently Lose Ryan White: ", 
             format(round(mean(100*rel.total.infections.averted.loseRW)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.infections.averted.loseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.infections.averted.loseRW, probs=.975)), big.mark=','),
             "%]"))

print(paste0("Infections Averted (vs Lose Ryan White until 2029): ", 
             format(round(mean(abs.total.infections.averted.temploseRW)), big.mark=','),
             " [",
             format(round(quantile(abs.total.infections.averted.temploseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(abs.total.infections.averted.temploseRW, probs=.975)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections if Lose Ryan White until 2029: ", 
             format(round(mean(100*rel.total.infections.averted.temploseRW)), big.mark=','),
             "% [",
             format(round(quantile(100*rel.total.infections.averted.temploseRW, probs=.025)), big.mark=','),
             " - ",
             format(round(quantile(100*rel.total.infections.averted.temploseRW, probs=.975)), big.mark=','),
             "%]"))
