

#-- SETTINGS --#

YEARS = 2026:2030
YEARS.TO.CONSIDER = as.character(YEARS)
END.NAME = 'nanb.rw.end.26'
INTR.NAME = 'nanb.rw.intr.26'
END.NAME.CONS = 'nanb.rw.end.cons.26'
INTR.NAME.CONS = 'nanb.rw.intr.cons.26'

#-- LOAD RESULTS FILE --#
if (!exists("total.incidence"))
{
    files = list.files('Q:results/ryan_white', full.names = T)
    files = files[grepl('nanb', files)]
    
    load(files[length(files)])
}

#-- TEXT RESULTS for PRIMARY ANALYSIS --#

tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections ", min(YEARS), "-", max(YEARS), " if Ryan White Continues: ", 
             format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

abs.delta.cessation.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.cessation.tot.inf.noint1 = abs.delta.cessation.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections ", min(YEARS), "-", max(YEARS), " if Targeted Programs End Indefinitely: ", 
             format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 if Targeted Programs End Indefinitely: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint1, na.rm=T) * 100, 1), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)*100, 1), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)*100, 1), big.mark=','),
             "%]"))

abs.delta.interruption.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,INTR.NAME,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.interruption.tot.inf.noint1 = abs.delta.interruption.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections ", min(YEARS), "-", max(YEARS), " if Targeted Programs Restart in 2029: ", 
             format(round(mean(abs.delta.interruption.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.interruption.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.interruption.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 if Targeted Programs Restart in 2029: ", 
             format(round(mean(rel.delta.interruption.tot.inf.noint1, na.rm=T)*100,1), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.interruption.tot.inf.noint1, probs=.025, na.rm=T)*100, 1), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.interruption.tot.inf.noint1, probs=.975, na.rm=T)*100, 1), big.mark=','),
             "%]"))


#-- TEXT RESULTS for CONSERVATIVE ANALYSIS --#

abs.delta.cessation.tot.inf.noint1.cons = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME.CONS,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.cessation.tot.inf.noint1.cons = abs.delta.cessation.tot.inf.noint1.cons / tot.inf.noint1

print(paste0("Absolute Increase in Infections ", min(YEARS), "-", max(YEARS), " if Targeted Programs End Indefinitely, CONSERVATIVE analysis: ", 
             format(round(mean(abs.delta.cessation.tot.inf.noint1.cons, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1.cons, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1.cons, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 if Targeted Programs End Indefinitely, CONSERVATIVE analysis: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint1.cons*100, na.rm=T), 1), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1.cons, probs=.025, na.rm=T)*100, 1), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1.cons, probs=.975, na.rm=T)*100, 1), big.mark=','),
             "%]"))

abs.delta.interruption.tot.inf.noint1.cons = apply(total.incidence[YEARS.TO.CONSIDER,,,INTR.NAME.CONS,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.interruption.tot.inf.noint1.cons = abs.delta.interruption.tot.inf.noint1.cons / tot.inf.noint1

print(paste0("Absolute Increase in Infections ", min(YEARS), "-", max(YEARS), " if Targeted Programs Restart in 2029, CONSERVATIVE analysis: ", 
             format(round(mean(abs.delta.interruption.tot.inf.noint1.cons, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.interruption.tot.inf.noint1.cons, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.interruption.tot.inf.noint1.cons, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 if Targeted Programs Restart in 2029, CONSERVATIVE analysis: ", 
             format(round(mean(rel.delta.interruption.tot.inf.noint1.cons, na.rm=T)*100,1), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.interruption.tot.inf.noint1.cons, probs=.025, na.rm=T)*100, 1), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.interruption.tot.inf.noint1.cons, probs=.975, na.rm=T)*100, 1), big.mark=','),
             "%]"))


#-- BOXPLOTS --#


#-- SHADED TABLES --#