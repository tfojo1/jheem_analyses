
#total.dx = apply(full.results[,,,,,,'new',,], c('year','sim','location','intervention'), sum, na.rm=T)

YEARS.TO.CONSIDER = as.character(2025:2030)
YEARS.TO.CONSIDER2 = as.character(2025:2035)

#-- Description if RW Continues --#

print("RYAN WHITE CONTINUES: ")

tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections 2025-2030 if Ryan White Continues: ", 
             format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))


tot.inf.noint2 = apply(total.incidence[YEARS.TO.CONSIDER2,,,'noint',drop=F], c('sim'), sum, na.rm=T)
print(paste0("Infections 2025-2035 if Ryan White Continues: ", 
             format(round(mean(tot.inf.noint2, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint2, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint2, probs=.975, na.rm=T)), big.mark=','),
             "]"))


total.inc.2035 = apply(total.incidence['2035',,,'noint'], 'sim', sum) / apply(total.pop['2035',,,'noint'], 'sim', sum)

print(paste0("2035 Incidence if Ryan White Continues: ", 
             format(round(mean(total.inc.2035 * 100000, na.rm=T)), big.mark=','),
             " per 100,000 [",
             format(round(quantile(total.inc.2035, probs=.025, na.rm=T)*100000), big.mark=','),
             " - ",
             format(round(quantile(total.inc.2035, probs=.975, na.rm=T)*100000), big.mark=','),
             "]"))


print("")
print("CESSATION: ")


total.suppression = apply(full.results[YEARS.TO.CONSIDER,,,,,,'suppression',,], c('year','sim','intervention'), sum) / apply(full.results[YEARS.TO.CONSIDER,,,,,,'diagnosed.prevalence',,], c('year','sim','intervention'), sum)
print(paste0("Total Suppression pre-drop in 2025: ",
             round(100*mean(total.suppression['2025',,'noint'])), '%',
             " [", round(100*quantile(total.suppression['2025',,'noint'], probs=.025)),
             " - ", round(100*quantile(total.suppression['2025',,'noint'], probs=.975)),
             "%]"))
print(paste0("Total Suppression post-drop in 2026: ",
             round(100*mean(total.suppression['2026',,'rw.end'])), '%',
             " [", round(100*quantile(total.suppression['2026',,'rw.end'], probs=.025)),
             " - ", round(100*quantile(total.suppression['2026',,'rw.end'], probs=.975)),
             "%]"))

abs.delta.cessation.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.cessation.tot.inf.noint1 = abs.delta.cessation.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections 2025-2030 if Ryan White Ends Indefinitely:", 
             format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Brief Interruption: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


abs.total.infections.averted.end.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.end.by.city = abs.total.infections.averted.end.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.end.by.city = cbind(
  mean = apply(rel.total.infections.averted.end.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.975)
)
mean.ci.rel.total.infections.averted.end.by.city = mean.ci.rel.total.infections.averted.end.by.city[order(mean.ci.rel.total.infections.averted.end.by.city[,1]),]
n.cities = nrow(mean.ci.rel.total.infections.averted.end.by.city)

print(paste0("Smallest City-Level Increase in Infections 2025-2030 if Ryan White Ends Indefinitely: ", 
             get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[1]), " - ",
             format(round(mean.ci.rel.total.infections.averted.end.by.city[1,1]*100), big.mark=','),
             "% [",
             format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,2], probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,3], probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


print(paste0("Largest City-Level Increase in Infections 2025-2030 if Ryan White Ends Indefinitely: ", 
             get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[n.cities]), " - ",
             format(round(mean.ci.rel.total.infections.averted.end.by.city[n.cities,1]*100), big.mark=','),
             "% [",
             format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,2], probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,3], probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

# By medicaid expansion
total.incidence.medicaid.expansion = total.incidence[,,RW.MEDICAID.EXPANSION.CITIES,]
total.incidence.medicaid.nonexpansion = total.incidence[,,RW.MEDICAID.NONEXPANSION.CITIES,]

total.expansion.inf.2025.2030 = apply(total.incidence.medicaid.expansion[as.character(2025:2030),,,], c('sim','intervention'), sum)
ratio.inf.expansion = (total.expansion.inf.2025.2030[,'rw.end'] - total.expansion.inf.2025.2030[,'noint']) / total.expansion.inf.2025.2030[,'noint']

total.nonexpansion.inf.2025.2030 = apply(total.incidence.medicaid.nonexpansion[as.character(2025:2030),,,], c('sim','intervention'), sum)
ratio.inf.nonexpansion = (total.nonexpansion.inf.2025.2030[,'rw.end'] - total.nonexpansion.inf.2025.2030[,'noint']) / total.nonexpansion.inf.2025.2030[,'noint']


print(paste0("Medicaid NON-Expansion Relative Increase in Infections 2025-2030 if Ryan White Ends Indefinitely: ", 
             format(round(mean(ratio.inf.nonexpansion, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(ratio.inf.nonexpansion, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(ratio.inf.nonexpansion, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

print(paste0("Medicaid Expansion Relative Increase in Infections 2025-2030 if Ryan White Ends Indefinitely: ", 
             format(round(mean(ratio.inf.expansion, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(ratio.inf.expansion, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(ratio.inf.expansion, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

print(paste0("Medicaid NON-Expansion vs Expansion Relative Increase in Infections 2025-2030 if Ryan White Ends Indefinitely: delta = +", 
             format(round(mean(ratio.inf.nonexpansion-ratio.inf.expansion, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(ratio.inf.nonexpansion-ratio.inf.expansion, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(ratio.inf.nonexpansion-ratio.inf.expansion, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


abs.delta.cessation.tot.inf.noint2 = apply(total.incidence[YEARS.TO.CONSIDER2,,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint2
rel.delta.cessation.tot.inf.noint2 = abs.delta.cessation.tot.inf.noint2 / tot.inf.noint2

print(paste0("Relative Increase in Infections 2025-2035 if Ryan White Ends Indefinitely: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint2, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint2, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint2, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


total.inc.2035.end = apply(total.incidence['2035',,,'rw.end'], 'sim', sum) / apply(total.pop['2035',,,'rw.end'], 'sim', sum)
print(paste0("2035 Incidence if Ryan White Ends: ", 
             format(round(mean(total.inc.2035.end * 100000, na.rm=T)), big.mark=','),
             " per 100,000 [",
             format(round(quantile(total.inc.2035.end, probs=.025, na.rm=T)*100000), big.mark=','),
             " - ",
             format(round(quantile(total.inc.2035.end, probs=.975, na.rm=T)*100000), big.mark=','),
             "]"))

rel.total.inc.2035.end = (total.inc.2035.end-total.inc.2035)/total.inc.2035
print(paste0("Relative Increase in 2035 Incidence if Ryan White Ends: ", 
             format(round(mean(rel.total.inc.2035.end * 100, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(rel.total.inc.2035.end, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.total.inc.2035.end, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


print("")
print("BRIEF INTERRUPTION ")


abs.delta.b.intr.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.b.intr',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.b.intr.tot.inf.noint1 = abs.delta.b.intr.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections 2025-2030 in Ryan White Brief Interruption: ", 
             format(round(mean(abs.delta.b.intr.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.b.intr.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.b.intr.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Brief Interruption: ", 
             format(round(mean(rel.delta.b.intr.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.b.intr.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.b.intr.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))



abs.delta.b.intr.tot.inf.noint2 = apply(total.incidence[YEARS.TO.CONSIDER2,,,'rw.b.intr',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint2
rel.delta.b.intr.tot.inf.noint2 = abs.delta.b.intr.tot.inf.noint2 / tot.inf.noint2

print(paste0("Relative Increase in Infections 2025-2035 in Ryan White Brief Interruption: ", 
             format(round(mean(rel.delta.b.intr.tot.inf.noint2, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.b.intr.tot.inf.noint2, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.b.intr.tot.inf.noint2, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


total.inc.2035.b.intr = apply(total.incidence['2035',,,'rw.b.intr'], 'sim', sum) / apply(total.pop['2035',,,'rw.b.intr'], 'sim', sum)
print(paste0("2035 Incidence in Ryan White Brief Interruption: ", 
             format(round(mean(total.inc.2035.b.intr * 100000, na.rm=T)), big.mark=','),
             " per 100,000 [",
             format(round(quantile(total.inc.2035.b.intr, probs=.025, na.rm=T)*100000), big.mark=','),
             " - ",
             format(round(quantile(total.inc.2035.b.intr, probs=.975, na.rm=T)*100000), big.mark=','),
             "]"))

rel.total.inc.2035.b.intr = (total.inc.2035.b.intr-total.inc.2035)/total.inc.2035
print(paste0("Relative Increase in 2035 Incidence in Ryan White Brief Interruption: ", 
             format(round(mean(rel.total.inc.2035.b.intr * 100, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(rel.total.inc.2035.b.intr, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.total.inc.2035.b.intr, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


print("")
print("PROLONGED INTERRUPTION ")


abs.delta.p.intr.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.p.intr',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.p.intr.tot.inf.noint1 = abs.delta.p.intr.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections 2025-2030 in Ryan White Prolonged Interruption: ", 
             format(round(mean(abs.delta.p.intr.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.p.intr.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.p.intr.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Prolonged Interruption: ", 
             format(round(mean(rel.delta.p.intr.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.p.intr.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.p.intr.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))



abs.delta.p.intr.tot.inf.noint2 = apply(total.incidence[YEARS.TO.CONSIDER2,,,'rw.p.intr',drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint2
rel.delta.p.intr.tot.inf.noint2 = abs.delta.p.intr.tot.inf.noint2 / tot.inf.noint2

print(paste0("Relative Increase in Infections 2025-2035 in Ryan White Prolonged Interruption: ", 
             format(round(mean(rel.delta.p.intr.tot.inf.noint2, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.p.intr.tot.inf.noint2, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.p.intr.tot.inf.noint2, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


total.inc.2035.p.intr = apply(total.incidence['2035',,,'rw.p.intr'], 'sim', sum) / apply(total.pop['2035',,,'rw.p.intr'], 'sim', sum)
print(paste0("2035 Incidence in Ryan White Prolonged Interruption: ", 
             format(round(mean(total.inc.2035.p.intr * 100000, na.rm=T)), big.mark=','),
             " per 100,000 [",
             format(round(quantile(total.inc.2035.p.intr, probs=.025, na.rm=T)*100000), big.mark=','),
             " - ",
             format(round(quantile(total.inc.2035.p.intr, probs=.975, na.rm=T)*100000), big.mark=','),
             "]"))

rel.total.inc.2035.p.intr = (total.inc.2035.p.intr-total.inc.2035)/total.inc.2035
print(paste0("Relative Increase in 2035 Incidence in Ryan White Prolonged Interruption: ", 
             format(round(mean(rel.total.inc.2035.p.intr * 100, na.rm=T)), big.mark=','),
             "% [",
             format(round(quantile(rel.total.inc.2035.p.intr, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.total.inc.2035.p.intr, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))



print("")
print("INCIDENCE BY SUBGROUP:")

incidence.by.race = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('race','sim','location','intervention'), sum)

inf.by.race.noint1 = apply(incidence.by.race[,,,'noint',drop=F], c('race','sim'), sum, na.rm=T)
abs.delta.cessation.race.inf.noint1 = apply(incidence.by.race[,,,'rw.end',drop=F], c('race','sim'), sum, na.rm=T) - inf.by.race.noint1
rel.delta.cessation.race.inf.noint1 = abs.delta.cessation.race.inf.noint1 / inf.by.race.noint1

mean.ci.rel.delta.cessation.race.inf.noint1 = cbind(
  mean = rowMeans(rel.delta.cessation.race.inf.noint1),
  lower = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.025),
  upper = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.975)
)

print(paste0("Relative Increase in Infections 2025-2030 for Black residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',3]*100), big.mark=','),
             "%]"))
print(paste0("Relative Increase in Infections 2025-2030 for Hispanic residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',3]*100), big.mark=','),
             "%]"))
print(paste0("Relative Increase in Infections 2025-2030 for Other residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',3]*100), big.mark=','),
             "%]"))

incidence.by.age = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('age','sim','location','intervention'), sum)

inf.by.age.noint1 = apply(incidence.by.age[,,,'noint',drop=F], c('age','sim'), sum, na.rm=T)
abs.delta.cessation.age.inf.noint1 = apply(incidence.by.age[,,,'rw.end',drop=F], c('age','sim'), sum, na.rm=T) - inf.by.age.noint1
rel.delta.cessation.age.inf.noint1 = abs.delta.cessation.age.inf.noint1 / inf.by.age.noint1

mean.ci.rel.delta.cessation.age.inf.noint1 = cbind(
  mean = rowMeans(rel.delta.cessation.age.inf.noint1),
  lower = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.025),
  upper = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.975)
)


incidence.not.age1 = apply(full.results[YEARS.TO.CONSIDER,-1,,,,,'incidence',,], c('sim','location','intervention'), sum)

inf.not.age1.noint1 = apply(incidence.not.age1[,,'noint',drop=F], c('sim'), sum, na.rm=T)
abs.delta.cessation.not.age1.inf.noint1 = apply(incidence.not.age1[,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - inf.not.age1.noint1
rel.delta.cessation.not.age1.inf.noint1 = abs.delta.cessation.not.age1.inf.noint1 / inf.not.age1.noint1

print(paste0("Relative Increase in Infections 2025-2030 FOR AGE 1 in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,3]*100), big.mark=','),
             "%]"))

print(paste0("Relative Increase in Infections 2025-2030 FOR AGE 2-4 in Cessation: ", 
             format(round(mean(rel.delta.cessation.not.age1.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


incidence.by.sex = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sex','sim','location','intervention'), sum)

inf.by.sex.noint1 = apply(incidence.by.sex[,,,'noint',drop=F], c('sex','sim'), sum, na.rm=T)
abs.delta.cessation.sex.inf.noint1 = apply(incidence.by.sex[,,,'rw.end',drop=F], c('sex','sim'), sum, na.rm=T) - inf.by.sex.noint1
rel.delta.cessation.sex.inf.noint1 = abs.delta.cessation.sex.inf.noint1 / inf.by.sex.noint1

mean.ci.rel.delta.cessation.sex.inf.noint1 = cbind(
  mean = rowMeans(rel.delta.cessation.sex.inf.noint1),
  lower = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.025),
  upper = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.975)
)


incidence.by.risk = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('risk','sim','location','intervention'), sum)

inf.by.risk.noint1 = apply(incidence.by.risk[,,,'noint',drop=F], c('risk','sim'), sum, na.rm=T)
abs.delta.cessation.risk.inf.noint1 = apply(incidence.by.risk[,,,'rw.end',drop=F], c('risk','sim'), sum, na.rm=T) - inf.by.risk.noint1
rel.delta.cessation.risk.inf.noint1 = abs.delta.cessation.risk.inf.noint1 / inf.by.risk.noint1

mean.ci.rel.delta.cessation.risk.inf.noint1 = cbind(
  mean = rowMeans(rel.delta.cessation.risk.inf.noint1),
  lower = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.025),
  upper = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.975)
)

incidence.msm = apply(full.results[YEARS.TO.CONSIDER,,,'msm','never_IDU',,'incidence',,], c('sim','location','intervention'), sum)
inf.msm.noint1 = apply(incidence.msm[,,'noint',drop=F], c('sim'), sum, na.rm=T)
abs.delta.cessation.msm.inf.noint1 = apply(incidence.msm[,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - inf.msm.noint1
rel.delta.cessation.msm.inf.noint1 = abs.delta.cessation.msm.inf.noint1 / inf.msm.noint1

print(paste0("Relative Increase in Infections AMONG MSM 2025-2030 FOR MSM in Cessation: ", 
             format(round(mean(rel.delta.cessation.msm.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

inf.total = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sim','location','intervention'), sum)
inf.non.msm.noint1 = apply(inf.total[,,'noint'], 'sim', sum) - inf.msm.noint1
abs.delta.cessation.non.msm.inf.noint1 = apply(inf.total[,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - apply(incidence.msm[,,'rw.end',drop=F], c('sim'), sum, na.rm=T)  - inf.non.msm.noint1
rel.delta.cessation.non.msm.inf.noint1 = abs.delta.cessation.non.msm.inf.noint1 / inf.non.msm.noint1

print(paste0("Relative Increase in Infections AMONG MSM 2025-2030 FOR Non-MSM in Cessation: ", 
             format(round(mean(rel.delta.cessation.non.msm.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


print("")
print("DIAGNOSES")

print("RYAN WHITE CONTINUES: ")

tot.dx.noint1 = apply(total.new[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Diagnoses 2025-2030 if Ryan White Continues: ", 
             format(round(mean(tot.dx.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.dx.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.dx.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))


abs.delta.cessation.tot.dx.noint1 = apply(total.new[YEARS.TO.CONSIDER,,,'rw.end',drop=F], c('sim'), sum, na.rm=T) - tot.dx.noint1
rel.delta.cessation.tot.dx.noint1 = abs.delta.cessation.tot.dx.noint1 / tot.dx.noint1

print(paste0("Absolute Increase in Infections 2025-2030 in Ryan White Cessation: ", 
             format(round(mean(abs.delta.cessation.tot.dx.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.dx.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.dx.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Cessation: ", 
             format(round(mean(rel.delta.cessation.tot.dx.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.dx.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.dx.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


abs.delta.b.intr.tot.dx.noint1 = apply(total.new[YEARS.TO.CONSIDER,,,'rw.b.intr',drop=F], c('sim'), sum, na.rm=T) - tot.dx.noint1
rel.delta.b.intr.tot.dx.noint1 = abs.delta.b.intr.tot.dx.noint1 / tot.dx.noint1

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Brief Interruption: ", 
             format(round(mean(rel.delta.b.intr.tot.dx.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.b.intr.tot.dx.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.b.intr.tot.dx.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

abs.delta.p.intr.tot.dx.noint1 = apply(total.new[YEARS.TO.CONSIDER,,,'rw.p.intr',drop=F], c('sim'), sum, na.rm=T) - tot.dx.noint1
rel.delta.p.intr.tot.dx.noint1 = abs.delta.p.intr.tot.dx.noint1 / tot.dx.noint1

print(paste0("Relative Increase in Infections 2025-2030 in Ryan White Brief Interruption: ", 
             format(round(mean(rel.delta.p.intr.tot.dx.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.p.intr.tot.dx.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.p.intr.tot.dx.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))
