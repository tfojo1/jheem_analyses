DO.FOR.CONSERVATIVE.ANALYSIS = F

YEARS.TO.CONSIDER = as.character(2025:2030)
PLOT.DIR = file.path(RW.ROOT.PLOT.DIR, paste0('shaded_table_boxplot_', tolower(RW.LOCATION.DESCRIPTOR), 
                                              ifelse(DO.FOR.CONSERVATIVE.ANALYSIS, '_conservative', '')))

if (!dir.exists(PLOT.DIR))
    dir.create(PLOT.DIR, recursive = T)

END.NAME = 'rw.end'
P.INTR.NAME = 'rw.p.intr'
B.INTR.NAME = 'rw.b.intr'

if (DO.FOR.CONSERVATIVE.ANALYSIS)
{
    END.NAME = 'rw.end.cons'
    P.INTR.NAME = 'rw.p.intr.cons'
    B.INTR.NAME = 'rw.b.intr.cons'
}


total.infections.continue.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)


# End vs Continue
abs.total.infections.averted.end.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.end.by.city = abs.total.infections.averted.end.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.end.by.city = cbind(
  mean = apply(rel.total.infections.averted.end.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.75)
)
mean.ci.rel.total.infections.averted.end.by.city = mean.ci.rel.total.infections.averted.end.by.city[order(mean.ci.rel.total.infections.averted.end.by.city[,1], decreasing = T),]

n.cities = nrow(mean.ci.rel.total.infections.averted.end.by.city)
ordered.cities = rownames(mean.ci.rel.total.infections.averted.end.by.city)

mean.ci.rel.total.infections.averted.end.by.city = rbind(
  mean.ci.rel.total.infections.averted.end.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.75))
)



mean.ci.abs.total.infections.averted.end.by.city = cbind(
  mean = apply(abs.total.infections.averted.end.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.75)
)
mean.ci.abs.total.infections.averted.end.by.city = mean.ci.abs.total.infections.averted.end.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.end.by.city = rbind(
  mean.ci.abs.total.infections.averted.end.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.75))
)

# Total Infections

mean.ci.total.infections.continue.by.city = cbind(
  mean = apply(total.infections.continue.by.city, 'location', mean),
  lower = apply(total.infections.continue.by.city, 'location', quantile, probs=0.025),
  upper = apply(total.infections.continue.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(total.infections.continue.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(total.infections.continue.by.city, 'location', quantile, probs=0.75)
)
mean.ci.total.infections.continue.by.city = mean.ci.total.infections.continue.by.city[ordered.cities,]
mean.ci.total.infections.continue.by.city = rbind(
  mean.ci.total.infections.continue.by.city,
  c(mean = mean(rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.city), probs=0.75))
)

# Brief Interruption vs Continue

abs.total.infections.averted.b.intr.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,B.INTR.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.b.intr.by.city = abs.total.infections.averted.b.intr.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.b.intr.by.city = cbind(
  mean = apply(rel.total.infections.averted.b.intr.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.75)
)
mean.ci.rel.total.infections.averted.b.intr.by.city = mean.ci.rel.total.infections.averted.b.intr.by.city[ordered.cities,]

mean.ci.rel.total.infections.averted.b.intr.by.city = rbind(
  mean.ci.rel.total.infections.averted.b.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.75))
)


mean.ci.abs.total.infections.averted.b.intr.by.city = cbind(
  mean = apply(abs.total.infections.averted.b.intr.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.75)
)
mean.ci.abs.total.infections.averted.b.intr.by.city = mean.ci.abs.total.infections.averted.b.intr.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.b.intr.by.city = rbind(
  mean.ci.abs.total.infections.averted.b.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.75))
)


# Prolonged Interruption vs Continue

abs.total.infections.averted.p.intr.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,P.INTR.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.p.intr.by.city = abs.total.infections.averted.p.intr.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.p.intr.by.city = cbind(
  mean = apply(rel.total.infections.averted.p.intr.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.75)
)
mean.ci.rel.total.infections.averted.p.intr.by.city = mean.ci.rel.total.infections.averted.p.intr.by.city[ordered.cities,]

mean.ci.rel.total.infections.averted.p.intr.by.city = rbind(
  mean.ci.rel.total.infections.averted.p.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]) / rowSums(total.infections.continue.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.75))
)


mean.ci.abs.total.infections.averted.p.intr.by.city = cbind(
  mean = apply(abs.total.infections.averted.p.intr.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.975),
  iqr.lower = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.25),
  iqr.upper = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.75)
)
mean.ci.abs.total.infections.averted.p.intr.by.city = mean.ci.abs.total.infections.averted.p.intr.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.p.intr.by.city = rbind(
  mean.ci.abs.total.infections.averted.p.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.EXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city[,RW.MEDICAID.NONEXPANSION.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.75))
)

interleave = function(v1, v2)
{
    rv = rep(v1, each=2)
    rv[2*(1:length(v2))] = v2
    rv
}

# Make table
table.city = data.frame(
    location = c(get.location.name(ordered.cities),
                 "Medicaid Expansion",
                 "Medicaid Non-Expansion",
                 "Total"),
    total.infections.continue = paste0(format(round(mean.ci.total.infections.continue.by.city[,1]), big.mark = ','),
                                       " [", format(round(mean.ci.total.infections.continue.by.city[,2]), big.mark = ','),
                                       " - ", format(round(mean.ci.total.infections.continue.by.city[,3]), big.mark = ','),
                                       
                                       "]"),
    excess.infections.end = paste0(format(round(mean.ci.abs.total.infections.averted.end.by.city[,1]), big.mark = ','),
                                   " [", format(round(mean.ci.abs.total.infections.averted.end.by.city[,2]), big.mark = ','),
                                   "-", format(round(mean.ci.abs.total.infections.averted.end.by.city[,3]), big.mark = ','),
                                   "]"),
    rel.excess.infections.end = paste0(round(100*mean.ci.rel.total.infections.averted.end.by.city[,1]), "% [",
                                       round(100*mean.ci.rel.total.infections.averted.end.by.city[,2]), "-",
                                       round(100*mean.ci.rel.total.infections.averted.end.by.city[,3]), "%]"),
    
    excess.infections.p.intr = paste0(format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,1]), big.mark = ','),
                                      "[", format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,2]), big.mark = ','),
                                      "-", format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,3]), big.mark = ','),
                                      "]"),
    rel.excess.infections.p.intr = paste0(round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,1]), "% [",
                                          round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,2]), "-",
                                          round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]"),
    
    
    excess.infections.b.intr = paste0(format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,1]), big.mark = ','),
                                      " [", format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,2]), big.mark = ','),
                                      "-", format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,3]), big.mark = ','),
                                      "]"),
    rel.excess.infections.b.intr = paste0(round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,1]), "% [",
                                          round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,2]), "-",
                                          round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,3]), "%]")
)

# Make table

if (RW.IS.STATE.LEVEL)
    location.names = locations::get.location.name(ordered.cities)
if (!RW.IS.STATE.LEVEL)
    RW.CITY.SHORT.NAMES[ordered.cities]
    
city.plus.total.names = c(location.names,
                          exp=paste0("Medicaid Expansion ", RW.LOCATION.DESCRIPTOR.PLURAL),
                          nonexp=paste0("Medicaid Non-Expansion ", RW.LOCATION.DESCRIPTOR.PLURAL),
                          total="Total")

table.city = data.frame(
  location = interleave(city.plus.total.names,
                        rep('', n.cities+3)),
  total.infections.continue = interleave(format(round(mean.ci.total.infections.continue.by.city[,1]), big.mark = ','),
                                     paste0("[", sapply(round(mean.ci.total.infections.continue.by.city[,2]), format, big.mark = ','),
                                     "-", sapply(round(mean.ci.total.infections.continue.by.city[,3]), format, big.mark = ','),
                                     
                                     "]")),
  
  excess.infections.end = interleave(format(round(mean.ci.abs.total.infections.averted.end.by.city[,1]), big.mark = ','),
                                     paste0("[", sapply(round(mean.ci.abs.total.infections.averted.end.by.city[,2]), format, big.mark = ','),
                                            "-", sapply(round(mean.ci.abs.total.infections.averted.end.by.city[,3]), format, big.mark = ','),
                                            "]")),
  rel.excess.infections.end = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.end.by.city[,1]), "%"),
                                         paste0("[",round(100*mean.ci.rel.total.infections.averted.end.by.city[,2]), "-",
                                                round(100*mean.ci.rel.total.infections.averted.end.by.city[,3]), "%]")),
  
  
  excess.infections.p.intr = interleave(format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,1]), big.mark = ','),
                                        paste0("[", sapply(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,2]), format, big.mark = ','),
                                               "-", sapply(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,3]), format, big.mark = ','),
                                               "]")),
  rel.excess.infections.p.intr = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,1]), "%"),
                                            paste0("[",round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,2]), "-",
                                                   round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]")),
  
  
  excess.infections.b.intr = interleave(format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,1]), big.mark = ','),
                                     paste0("[", sapply(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,2]), format, big.mark = ','),
                                            "-", sapply(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,3]), format, big.mark = ','),
                                            "]")),
  rel.excess.infections.b.intr = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,1]), "%"),
                                         paste0("[",round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,2]), "-",
                                                round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,3]), "%]"))
)


color.by = cbind(
    rep(-1, 2*(n.cities+3)),
    rep(-1, 2*(n.cities+3)),
    rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.b.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.b.intr.by.city[,1], each=2)
)

text.color = matrix(RW.EXP.LABEL.COLOR, nrow=nrow(color.by), ncol=ncol(color.by))
nonexp.mask = sapply(names(city.plus.total.names), function(city){
    any(city == RW.MEDICAID.NONEXPANSION.LOCATIONS)
})
nonexp.mask['nonexp'] = T
total.mask = names(city.plus.total.names) == 'total'
text.color[2*(1:length(city.plus.total.names)),][nonexp.mask,] = RW.NONEXP.LABEL.COLOR
text.color[2*(1:length(city.plus.total.names))-1,][nonexp.mask,] = RW.NONEXP.LABEL.COLOR
text.color[2*(1:length(city.plus.total.names)),][total.mask,] = RW.TOTAL.LABEL.COLOR
text.color[2*(1:length(city.plus.total.names))-1,][total.mask,] = paste0(RW.TOTAL.TEXT.COLOR,'FF')
text.color[,-1] = '#FFFFFFFF'


source('presentation/make_pretty_table.R')
write.shaded.table(tab = table.city,
                   color.by = color.by,
                   colors = c('white','yellow','red'),
                   thresholds = c(-1,0,1),
                   text.color = text.color,
                   file = file.path(PLOT.DIR, 'shaded_table.xlsx'))

write.shaded.table(tab = table.city[2*(1:length(city.plus.total.names))-1,1,drop=F],
                   color.by = color.by[2*(1:length(city.plus.total.names))-1,1,drop=F],
                   colors = c('white','yellow','red'),
                   thresholds = c(-1,0,1),
                   text.color = text.color[2*(1:length(city.plus.total.names))-1,1,drop=F],
                   file = file.path(PLOT.DIR, 'rownames_shaded_table.xlsx'))


boxplot.df.end = as.data.frame(mean.ci.rel.total.infections.averted.end.by.city)
boxplot.df.end$Scenario = '0.end'
boxplot.df.end$location = city.plus.total.names
boxplot.df.end$loc.code = names(city.plus.total.names)

boxplot.df.p.intr = as.data.frame(mean.ci.rel.total.infections.averted.p.intr.by.city)
boxplot.df.p.intr$Scenario = '1.p.intr'
boxplot.df.p.intr$location = city.plus.total.names
boxplot.df.p.intr$loc.code = names(city.plus.total.names)

boxplot.df.b.intr = as.data.frame(mean.ci.rel.total.infections.averted.b.intr.by.city)
boxplot.df.b.intr$Scenario = '2.b.intr'
boxplot.df.b.intr$location = city.plus.total.names
boxplot.df.b.intr$loc.code = names(city.plus.total.names)

boxplot.df.spacer = boxplot.df.end
boxplot.df.spacer$mean = boxplot.df.spacer$lower = boxplot.df.spacer$upper = boxplot.df.spacer$iqr.lower = boxplot.df.spacer$iqr.upper = NA
boxplot.df.spacer$Scenario = '3.spacer'
boxplot.df.spacer = boxplot.df.spacer[boxplot.df.spacer$loc.code!='total',]

boxplot.df = rbind(
  boxplot.df.spacer,
  boxplot.df.end,
  boxplot.df.p.intr,
  boxplot.df.b.intr
)

city.plus.total.names['exp'] = "Medicaid\nExpansion States"
city.plus.total.names['nonexp'] = "Medicaid\nNon-Expansion States"

# boxplot.df$location = factor(boxplot.df$location,
#                              levels = rev(c(city.plus.total.names[c('total','nonexp','exp')],
#                                         rev(location.names))))

loc.code.levels = c(names(location.names), c('exp','nonexp','total'))
boxplot.df$loc.code = factor(boxplot.df$loc.code,
                             levels = loc.code.levels)

boxplot.df$group = paste0(boxplot.df$Scenario, "_", boxplot.df$loc.code)


nonexp.mask = sapply(loc.code.levels, function(city){
    any(city==RW.MEDICAID.NONEXPANSION.LOCATIONS)
})
nonexp.mask[loc.code.levels=='nonexp'] = T
total.mask = loc.code.levels == 'total'
total.and.subtotal.mask = total.mask | loc.code.levels == 'exp' | loc.code.levels == 'nonexp'

location.colors = rep(RW.EXP.LABEL.COLOR, length(loc.code.levels))
location.colors[nonexp.mask] = RW.NONEXP.LABEL.COLOR
location.face = rep('plain', length(loc.code.levels))

location.colors[total.mask] = RW.TOTAL.LABEL.COLOR
location.face[total.and.subtotal.mask] = 'bold'


plot = ggplot() + 
  geom_boxplot(data = boxplot.df,
               aes(x = loc.code,
                   middle = mean,
                   lower = iqr.lower,
                   upper = iqr.upper,
                   min = lower,
                   max = upper,
                   fill = Scenario,
                   group = group),
               stat = 'identity',
               position = position_dodge2()) + 
  scale_fill_manual(values = c('0.end'=RW.END.COLOR,
                               '1.p.intr'=RW.P.INTR.COLOR,
                               '2.b.intr'=RW.B.INTR.COLOR),
                    labels = c('0.end'='Cessation',
                               '1.p.intr'='Prolonged Interruption',
                               '2.b.intr'='Brief Interruption'),
                    name=NULL) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = city.plus.total.names) + 
  xlab(NULL) +
  ylab("Relative Increase in HIV Infections, 2025-2030") + 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(color=location.colors, 
                                   face=location.face,
                                   angle = 45,
                                   hjust = 1)); print(plot)
  

PLOT.HEIGHT = 5
PLOT.WIDTH = 6.5
PLOT.DPI = 600
PLOT.DEVICE = 'png'

print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, 'Figure_4.png'),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
