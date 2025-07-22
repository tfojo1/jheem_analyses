YEARS = as.character(2025:2030)

total.inc.by.age = apply(full.incidence[YEARS,,,,,,,], c('age','sim','intervention'), sum)
total.abs.excess.inc.by.age.end = total.inc.by.age[,,'cdct.end'] - total.inc.by.age[,,'noint']
total.rel.excess.inc.by.age.end = total.abs.excess.inc.by.age.end / total.inc.by.age[,,'noint']

cbind(
    mean = apply(total.rel.excess.inc.by.age.end, 'age', mean),
    lower = apply(total.rel.excess.inc.by.age.end, 'age', quantile, probs=0.025),
    upper = apply(total.rel.excess.inc.by.age.end, 'age', quantile, probs=0.975)
)

total.rel.excess.inc.under.35 = colSums(total.inc.by.age[1:2,,'cdct.end'] - total.inc.by.age[1:2,,'noint']) / colSums(total.inc.by.age[1:2,,'noint'])
total.rel.excess.inc.over.35 = colSums(total.inc.by.age[3:5,,'cdct.end'] - total.inc.by.age[3:5,,'noint']) / colSums(total.inc.by.age[1:2,,'noint'])

rbind(
    under.35 = c(mean=mean(total.rel.excess.inc.under.35),
                 lower=quantile(total.rel.excess.inc.under.35, probs=0.025),
                 upper=quantile(total.rel.excess.inc.under.35, probs=0.975)),
    over.35 = c(mean=mean(total.rel.excess.inc.over.35),
                lower=quantile(total.rel.excess.inc.over.35, probs=0.025),
                upper=quantile(total.rel.excess.inc.over.35, probs=0.975))
)

total.inc.by.race = apply(full.incidence[YEARS,,,,,,,], c('race','sim','intervention'), sum)
total.abs.excess.inc.by.race.end = total.inc.by.race[,,'cdct.end'] - total.inc.by.race[,,'noint']
total.rel.excess.inc.by.race.end = total.abs.excess.inc.by.race.end / total.inc.by.race[,,'noint']

cbind(
    mean = apply(total.rel.excess.inc.by.race.end, 'race', mean),
    lower = apply(total.rel.excess.inc.by.race.end, 'race', quantile, probs=0.025),
    upper = apply(total.rel.excess.inc.by.race.end, 'race', quantile, probs=0.975)
)


total.inc.by.sex = apply(full.incidence[YEARS,,,,,,,], c('sex','sim','intervention'), sum)
total.abs.excess.inc.by.sex.end = total.inc.by.sex[,,'cdct.end'] - total.inc.by.sex[,,'noint']
total.rel.excess.inc.by.sex.end = total.abs.excess.inc.by.sex.end / total.inc.by.sex[,,'noint']

cbind(
    mean = apply(total.rel.excess.inc.by.sex.end, 'sex', mean),
    lower = apply(total.rel.excess.inc.by.sex.end, 'sex', quantile, probs=0.025),
    upper = apply(total.rel.excess.inc.by.sex.end, 'sex', quantile, probs=0.975)
)