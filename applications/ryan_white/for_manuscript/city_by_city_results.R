DO.FOR.CONSERVATIVE.ANALYSIS = F

YEARS.TO.CONSIDER = as.character(2026:2031)
PLOT.DIR = file.path(RW.ROOT.PLOT.DIR, paste0('shaded_table_boxplot_', tolower(RW.LOCATION.DESCRIPTOR), 
                                              ifelse(DO.FOR.CONSERVATIVE.ANALYSIS, '_conservative', '')))

total.infections.continue.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)


END.NAME = 'rw.end.26'
P.INTR.NAME = 'rw.p.intr.26'
B.INTR.NAME = 'rw.b.intr'

USE.B.INTR = F # to remove brief interruption

if (DO.FOR.CONSERVATIVE.ANALYSIS)
{
    END.NAME = 'rw.end.cons'
    P.INTR.NAME = 'rw.p.intr.cons'
    B.INTR.NAME = 'rw.b.intr.cons'
}

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

if(USE.B.INTR){
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
    
}


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
if(USE.B.INTR){
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
} else {
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
                                              round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]")
    )
}


# Make table

if (RW.IS.STATE.LEVEL)
    location.names = locations::get.location.name(ordered.cities)
if (!RW.IS.STATE.LEVEL)
    location.names = RW.CITY.SHORT.NAMES[ordered.cities]
    
city.plus.total.names = c(location.names,
                          exp=paste0("Medicaid Expansion ", RW.LOCATION.DESCRIPTOR.PLURAL),
                          nonexp=paste0("Medicaid Non-Expansion ", RW.LOCATION.DESCRIPTOR.PLURAL),
                          total="Total")

if(USE.B.INTR){
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
} else {
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
                                                         round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]"))
    )
}


if(USE.B.INTR){
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
} else {
    color.by = cbind(
        rep(-1, 2*(n.cities+3)),
        rep(-1, 2*(n.cities+3)),
        rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
        rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
        rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2),
        rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2)
    )
}


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


# source('presentation/make_pretty_table.R')
# write.shaded.table(tab = table.city,
#                    color.by = color.by,
#                    colors = c('white','yellow','red'),
#                    thresholds = c(-1,0,1),
#                    text.color = text.color,
#                    file = file.path(PLOT.DIR, 'shaded_table.xlsx'))
# 
# write.shaded.table(tab = table.city[2*(1:length(city.plus.total.names))-1,1,drop=F],
#                    color.by = color.by[2*(1:length(city.plus.total.names))-1,1,drop=F],
#                    colors = c('white','yellow','red'),
#                    thresholds = c(-1,0,1),
#                    text.color = text.color[2*(1:length(city.plus.total.names))-1,1,drop=F],
#                    file = file.path(PLOT.DIR, 'rownames_shaded_table.xlsx'))


boxplot.df.end = as.data.frame(mean.ci.rel.total.infections.averted.end.by.city)
boxplot.df.end$Scenario = '3.end'
boxplot.df.end$location = city.plus.total.names
boxplot.df.end$loc.code = names(city.plus.total.names)

boxplot.df.p.intr = as.data.frame(mean.ci.rel.total.infections.averted.p.intr.by.city)
boxplot.df.p.intr$Scenario = '2.p.intr'
boxplot.df.p.intr$location = city.plus.total.names
boxplot.df.p.intr$loc.code = names(city.plus.total.names)

if(USE.B.INTR){
    boxplot.df.b.intr = as.data.frame(mean.ci.rel.total.infections.averted.b.intr.by.city)
    boxplot.df.b.intr$Scenario = '1.b.intr'
    boxplot.df.b.intr$location = city.plus.total.names
    boxplot.df.b.intr$loc.code = names(city.plus.total.names) 
}


boxplot.df.spacer = boxplot.df.end
boxplot.df.spacer$mean = boxplot.df.spacer$lower = boxplot.df.spacer$upper = boxplot.df.spacer$iqr.lower = boxplot.df.spacer$iqr.upper = NA
boxplot.df.spacer$Scenario = '0.spacer'
boxplot.df.spacer = boxplot.df.spacer[boxplot.df.spacer$loc.code!='total',]

if(USE.B.INTR){
    boxplot.df = rbind(
        boxplot.df.spacer,
        boxplot.df.end,
        boxplot.df.p.intr,
        boxplot.df.b.intr
    )  
} else {
    boxplot.df = rbind(
        boxplot.df.spacer,
        boxplot.df.end,
        boxplot.df.p.intr
    )
}



boxplot.df$location = factor(boxplot.df$location,
                             levels = c(city.plus.total.names[c('total','nonexp','exp')],
                                        rev(location.names)))

boxplot.df$group = paste0(boxplot.df$Scenario, "_", boxplot.df$location)


nonexp.mask = sapply(boxplot.df$loc.code, function(city){
    any(city==RW.MEDICAID.NONEXPANSION.LOCATIONS)
})
nonexp.mask[boxplot.df$loc.code=='nonexp'] = T
total.mask = boxplot.df$loc.code == 'total'
total.and.subtotal.mask = total.mask | boxplot.df$loc.code == 'exp' | boxplot.df$loc.code == 'nonexp'

location.colors = rep(RW.EXP.LABEL.COLOR, length(boxplot.df$location))
location.colors[nonexp.mask] = RW.NONEXP.LABEL.COLOR
location.face = rep('plain', length(boxplot.df$location))

location.colors[total.mask] = RW.TOTAL.LABEL.COLOR
location.face[total.and.subtotal.mask] = 'bold'

MAX = 5
boxplot.df$upper = pmin(boxplot.df$upper, MAX)

asterisk.df = boxplot.df[!is.na(boxplot.df$upper) & boxplot.df$upper==MAX,]

if(USE.B.INTR){
    plot = ggplot() + 
        geom_boxplot(data = boxplot.df,
                     aes(y = location,
                         xmiddle = mean,
                         xlower = iqr.lower,
                         xupper = iqr.upper,
                         xmin = lower,
                         xmax = upper,
                         fill = Scenario,
                         group = group),
                     stat = 'identity',
                     position = position_dodge2()) + 
        geom_text(data = asterisk.df[asterisk.df$Scenario=='3.end',],
                  aes(y=location, x=upper, label='*', group=group, vjust=0.5, hjust=0.5),
                  nudge_y=.25, nudge_x = 0.006) +
        geom_text(data = asterisk.df[asterisk.df$Scenario!='3.end',],
                  aes(y=location, x=upper, label='*', group=group, vjust=0.5, hjust=0.5),
                  nudge_y=0, nudge_x = 0.006) +
        scale_fill_manual(values = c('3.end'=RW.END.COLOR,
                                     '2.p.intr'=RW.P.INTR.COLOR,
                                     '1.b.intr'=RW.B.INTR.COLOR),
                          labels = c('3.end'='Cessation',
                                     '2.p.intr'='Prolonged Interruption',
                                     '1.b.intr'='Brief Interruption'),
                          name=NULL) +
        theme_bw() +
        scale_x_continuous(labels = scales::percent, sec.axis = dup_axis(name=NULL)) +
        ylab(NULL) +
        xlab("Relative Increase in HIV Infections, 2025-2030") + 
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal',
              axis.text.y = element_text(color=rev(location.colors), face=rev(location.face))); print(plot)
    
    
} else {
    plot = ggplot() + 
        geom_boxplot(data = boxplot.df,
                     aes(y = location,
                         xmiddle = mean,
                         xlower = iqr.lower,
                         xupper = iqr.upper,
                         xmin = lower,
                         xmax = upper,
                         fill = Scenario,
                         group = group),
                     stat = 'identity',
                     position = position_dodge2()) + 
        geom_text(data = asterisk.df[asterisk.df$Scenario=='3.end',],
                  aes(y=location, x=upper, label='*', group=group, vjust=0.5, hjust=0.5),
                  nudge_y=.25, nudge_x = 0.006) +
        geom_text(data = asterisk.df[asterisk.df$Scenario!='3.end',],
                  aes(y=location, x=upper, label='*', group=group, vjust=0.5, hjust=0.5),
                  nudge_y=0, nudge_x = 0.006) +
        scale_fill_manual(values = c('3.end'=RW.END.COLOR,
                                     '2.p.intr'=RW.P.INTR.COLOR,
                                     '1.b.intr'=RW.B.INTR.COLOR),
                          labels = c('3.end'='Cessation',
                                     '2.p.intr'='2.5-year Interruption',
                                     '1.b.intr'='Brief Interruption'),
                          name=NULL) +
        theme_bw() +
        scale_x_continuous(labels = scales::percent, sec.axis = dup_axis(name=NULL)) +
        ylab(NULL) +
        xlab("Relative Increase in HIV Infections, 2026-2031") + 
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal',
              axis.text.y = element_text(color=rev(location.colors), face=rev(location.face))); print(plot)
    
    
}

PLOT.HEIGHT = 7
PLOT.WIDTH = 6.5
PLOT.DPI = 600
PLOT.DEVICE = 'png'

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(tolower(RW.LOCATION.DESCRIPTOR), '_boxplots.png')),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
