if (!exists("total.incidence"))
    x = load("Q:/results/ryan_white/ryan_white_results_state_partb_2026_2025-10-23.Rdata")

# [year,sim,location,intervention]
# 2 locations (MS and total)
# 2 interventions (noint and part.b.rw.end.26)

YEARS.TO.CONSIDER = as.character(2026:2031)
END.NAME = 'part.b.rw.end.26' 

#-- Description if RW Continues --#

print("RYAN WHITE CONTINUES: ")

tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim'), sum, na.rm=T)

print(paste0("Infections 2026-2031 if Ryan White Continues: ", 
             format(round(mean(tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))


print("")
print("CESSATION (Primary): ")

total.suppression = apply(total.results[YEARS.TO.CONSIDER,,'suppression',,], c('year','sim','intervention'), sum) / apply(total.results[YEARS.TO.CONSIDER,,'diagnosed.prevalence',,], c('year','sim','intervention'), sum)
print(paste0("Total Suppression pre-drop in 2026: ",
             round(100*mean(total.suppression['2026',,'noint'])), '%',
             " [", round(100*quantile(total.suppression['2026',,'noint'], probs=.025)),
             " - ", round(100*quantile(total.suppression['2026',,'noint'], probs=.975)),
             "%]"))
print(paste0("Total Suppression post-drop in 2027: ",
             round(100*mean(total.suppression['2027',,END.NAME])), '%',
             " [", round(100*quantile(total.suppression['2027',,END.NAME], probs=.025)),
             " - ", round(100*quantile(total.suppression['2027',,END.NAME], probs=.975)),
             "%]"))

abs.delta.cessation.tot.inf.noint1 = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - tot.inf.noint1
rel.delta.cessation.tot.inf.noint1 = abs.delta.cessation.tot.inf.noint1 / tot.inf.noint1

print(paste0("Absolute Increase in Infections 2026-2031 if Ryan White Ends Indefinitely:", 
             format(round(mean(abs.delta.cessation.tot.inf.noint1, na.rm=T)), big.mark=','),
             " [",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)), big.mark=','),
             " - ",
             format(round(quantile(abs.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)), big.mark=','),
             "]"))

print(paste0("Relative Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
             format(round(mean(rel.delta.cessation.tot.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.tot.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))



print("")
print("INCIDENCE BY SUBGROUP:")

#years.incidence.by.race = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('race','sim','location','intervention'), sum)
years.incidence.by.race = apply(incidence.by.race[YEARS.TO.CONSIDER,,,, ,drop = F], c('race','sim','location','intervention'), sum)

inf.by.race.noint1 = apply(years.incidence.by.race[,,,'noint',drop=F], c('race','sim'), sum, na.rm=T)
abs.delta.cessation.race.inf.noint1 = apply(years.incidence.by.race[,,,END.NAME,drop=F], c('race','sim'), sum, na.rm=T) - inf.by.race.noint1
rel.delta.cessation.race.inf.noint1 = abs.delta.cessation.race.inf.noint1 / inf.by.race.noint1

mean.ci.rel.delta.cessation.race.inf.noint1 = cbind(
    mean = rowMeans(rel.delta.cessation.race.inf.noint1),
    lower = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.025),
    upper = apply(rel.delta.cessation.race.inf.noint1, 1, quantile, probs=0.975)
)

print(paste0("Relative Increase in Infections 2026-2031 for Black residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['black',3]*100), big.mark=','),
             "%]"))
print(paste0("Relative Increase in Infections 2026-2031 for Hispanic residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['hispanic',3]*100), big.mark=','),
             "%]"))
print(paste0("Relative Increase in Infections 2026-2031 for Other residents in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.race.inf.noint1['other',3]*100), big.mark=','),
             "%]"))

#years.incidence.by.age = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('age','sim','location','intervention'), sum)
years.incidence.by.age = apply(incidence.by.age[YEARS.TO.CONSIDER,,,,, drop = F], c('age','sim','location','intervention'), sum)

inf.by.age.noint1 = apply(years.incidence.by.age[,,,'noint',drop=F], c('age','sim'), sum, na.rm=T)
abs.delta.cessation.age.inf.noint1 = apply(years.incidence.by.age[,,,END.NAME,drop=F], c('age','sim'), sum, na.rm=T) - inf.by.age.noint1
rel.delta.cessation.age.inf.noint1 = abs.delta.cessation.age.inf.noint1 / inf.by.age.noint1

mean.ci.rel.delta.cessation.age.inf.noint1 = cbind(
    mean = rowMeans(rel.delta.cessation.age.inf.noint1),
    lower = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.025),
    upper = apply(rel.delta.cessation.age.inf.noint1, 1, quantile, probs=0.975)
)


#incidence.not.age1 = apply(full.results[YEARS.TO.CONSIDER,-1,,,,,'incidence',,], c('sim','location','intervention'), sum)
incidence.not.age1 = apply(incidence.by.age[YEARS.TO.CONSIDER,-1,,,, drop = F], c('age','sim','location','intervention'), sum)

inf.not.age1.noint1 = apply(incidence.not.age1[,,,'noint',drop=F], c('sim'), sum, na.rm=T)
abs.delta.cessation.not.age1.inf.noint1 = apply(incidence.not.age1[,,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.not.age1.noint1
rel.delta.cessation.not.age1.inf.noint1 = abs.delta.cessation.not.age1.inf.noint1 / inf.not.age1.noint1

print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 1 in Cessation: ", 
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,1]*100), big.mark=','),
             "% [",
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,2]*100), big.mark=','),
             " - ",
             format(round(mean.ci.rel.delta.cessation.age.inf.noint1[1,3]*100), big.mark=','),
             "%]"))

print(paste0("Relative Increase in Infections 2026-2031 FOR AGE 2-4 in Cessation: ", 
             format(round(mean(rel.delta.cessation.not.age1.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.not.age1.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))


#years.incidence.by.sex = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sex','sim','location','intervention'), sum)
years.incidence.by.sex = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,,,,,, drop = F], c('sex','sim','location','intervention'), sum)

inf.by.sex.noint1 = apply(years.incidence.by.sex[,,,'noint',drop=F], c('sex','sim'), sum, na.rm=T)
abs.delta.cessation.sex.inf.noint1 = apply(years.incidence.by.sex[,,,END.NAME,drop=F], c('sex','sim'), sum, na.rm=T) - inf.by.sex.noint1
rel.delta.cessation.sex.inf.noint1 = abs.delta.cessation.sex.inf.noint1 / inf.by.sex.noint1

mean.ci.rel.delta.cessation.sex.inf.noint1 = cbind(
    mean = rowMeans(rel.delta.cessation.sex.inf.noint1),
    lower = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.025),
    upper = apply(rel.delta.cessation.sex.inf.noint1, 1, quantile, probs=0.975)
)


#years.incidence.by.risk = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('risk','sim','location','intervention'), sum)
years.incidence.by.risk = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,,,,,, drop = F], c('risk','sim','location','intervention'), sum)

inf.by.risk.noint1 = apply(years.incidence.by.risk[,,,'noint',drop=F], c('risk','sim'), sum, na.rm=T)
abs.delta.cessation.risk.inf.noint1 = apply(years.incidence.by.risk[,,,END.NAME,drop=F], c('risk','sim'), sum, na.rm=T) - inf.by.risk.noint1
rel.delta.cessation.risk.inf.noint1 = abs.delta.cessation.risk.inf.noint1 / inf.by.risk.noint1

mean.ci.rel.delta.cessation.risk.inf.noint1 = cbind(
    mean = rowMeans(rel.delta.cessation.risk.inf.noint1),
    lower = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.025),
    upper = apply(rel.delta.cessation.risk.inf.noint1, 1, quantile, probs=0.975)
)

#incidence.msm = apply(full.results[YEARS.TO.CONSIDER,,,'msm','never_IDU',,'incidence',,], c('sim','location','intervention'), sum)
#incidence.msm = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,'msm','never_IDU',,,], c('sim','location','intervention'), sum)
incidence.msm = apply(incidence.by.sex.risk[YEARS.TO.CONSIDER,'msm',,,,, drop =F], c('sim','location','intervention'), sum)
print("Keeping MSM-IDU with MSM")

inf.msm.noint1 = apply(incidence.msm[,,'noint',drop=F], c('sim'), sum, na.rm=T)
abs.delta.cessation.msm.inf.noint1 = apply(incidence.msm[,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - inf.msm.noint1
rel.delta.cessation.msm.inf.noint1 = abs.delta.cessation.msm.inf.noint1 / inf.msm.noint1

print(paste0("Relative Increase in Infections AMONG MSM 2026-2031 FOR MSM in Cessation: ", 
             format(round(mean(rel.delta.cessation.msm.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))

#inf.total = apply(full.results[YEARS.TO.CONSIDER,,,,,,'incidence',,], c('sim','location','intervention'), sum)
inf.total = apply(total.results[YEARS.TO.CONSIDER,,'incidence',,, drop = F], c('sim','location','intervention'), sum)
inf.non.msm.noint1 = apply(inf.total[,,'noint'], 'sim', sum) - inf.msm.noint1
abs.delta.cessation.non.msm.inf.noint1 = apply(inf.total[,,END.NAME,drop=F], c('sim'), sum, na.rm=T) - apply(incidence.msm[,,END.NAME,drop=F], c('sim'), sum, na.rm=T)  - inf.non.msm.noint1
rel.delta.cessation.non.msm.inf.noint1 = abs.delta.cessation.non.msm.inf.noint1 / inf.non.msm.noint1

print(paste0("Relative Increase in Infections AMONG NON-MSM 2026-2031 FOR Non-MSM in Cessation: ", 
             format(round(mean(rel.delta.cessation.non.msm.inf.noint1, na.rm=T)*100), big.mark=','),
             "% [",
             format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.025, na.rm=T)*100), big.mark=','),
             " - ",
             format(round(quantile(rel.delta.cessation.non.msm.inf.noint1, probs=.975, na.rm=T)*100), big.mark=','),
             "%]"))






# State-level results: making sure it's just Mississippi
if(1==2){
    n.cities = 1#nrow(mean.ci.rel.total.infections.averted.end.by.city)
    
    abs.total.infections.averted.end.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
    rel.total.infections.averted.end.by.city = abs.total.infections.averted.end.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
    mean.ci.rel.total.infections.averted.end.by.city = cbind(
        mean = apply(rel.total.infections.averted.end.by.city, 'location', mean, na.rm=T),
        lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.025, na.rm=T),
        upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.975, na.rm=T)
    )
    mean.ci.rel.total.infections.averted.end.by.city = mean.ci.rel.total.infections.averted.end.by.city[order(mean.ci.rel.total.infections.averted.end.by.city[,1]),]

    
    print(paste0("Smallest ", RW.LOCATION.DESCRIPTOR, "-Level Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
                 get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[1]), " - ",
                 format(round(mean.ci.rel.total.infections.averted.end.by.city[1,1]*100), big.mark=','),
                 "% [",
                 format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,2], probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[1,3], probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))
    
    
    print(paste0("Largest ", RW.LOCATION.DESCRIPTOR, "-Level Increase in Infections 2026-2031 if Ryan White Ends Indefinitely: ", 
                 get.location.name(rownames(mean.ci.rel.total.infections.averted.end.by.city)[n.cities]), " - ",
                 format(round(mean.ci.rel.total.infections.averted.end.by.city[n.cities,1]*100), big.mark=','),
                 "% [",
                 format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,2], probs=.025, na.rm=T)*100), big.mark=','),
                 " - ",
                 format(round(quantile(mean.ci.rel.total.infections.averted.end.by.city[n.cities,3], probs=.975, na.rm=T)*100), big.mark=','),
                 "%]"))  
}

