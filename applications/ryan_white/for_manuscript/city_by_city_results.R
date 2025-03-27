YEARS.TO.CONSIDER = as.character(2025:2030)

total.infections.continue.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)


# End vs Continue
abs.total.infections.averted.end.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.end.by.city = abs.total.infections.averted.end.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.end.by.city = cbind(
  mean = apply(rel.total.infections.averted.end.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.end.by.city, 'location', quantile, probs=0.975)
)
mean.ci.rel.total.infections.averted.end.by.city = mean.ci.rel.total.infections.averted.end.by.city[order(mean.ci.rel.total.infections.averted.end.by.city[,1], decreasing = T),]

n.cities = nrow(mean.ci.rel.total.infections.averted.end.by.city)
ordered.cities = rownames(mean.ci.rel.total.infections.averted.end.by.city)

mean.ci.rel.total.infections.averted.end.by.city = rbind(
  mean.ci.rel.total.infections.averted.end.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city) / rowSums(total.infections.continue.by.city), probs=0.975))
)



mean.ci.abs.total.infections.averted.end.by.city = cbind(
  mean = apply(abs.total.infections.averted.end.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.end.by.city, 'location', quantile, probs=0.975)
)
mean.ci.abs.total.infections.averted.end.by.city = mean.ci.abs.total.infections.averted.end.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.end.by.city = rbind(
  mean.ci.abs.total.infections.averted.end.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.city), probs=0.975))
)

# Total Infections

mean.ci.total.infections.continue.by.city = cbind(
  mean = apply(total.infections.continue.by.city, 'location', mean),
  lower = apply(total.infections.continue.by.city, 'location', quantile, probs=0.025),
  upper = apply(total.infections.continue.by.city, 'location', quantile, probs=0.975)
)
mean.ci.total.infections.continue.by.city = mean.ci.total.infections.continue.by.city[ordered.cities,]
mean.ci.total.infections.continue.by.city = rbind(
  mean.ci.total.infections.continue.by.city,
  c(mean = mean(rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.city), probs=0.975))
)

# Brief Interruption vs Continue

abs.total.infections.averted.b.intr.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.b.intr',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.b.intr.by.city = abs.total.infections.averted.b.intr.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.b.intr.by.city = cbind(
  mean = apply(rel.total.infections.averted.b.intr.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.975)
)
mean.ci.rel.total.infections.averted.b.intr.by.city = mean.ci.rel.total.infections.averted.b.intr.by.city[ordered.cities,]

mean.ci.rel.total.infections.averted.b.intr.by.city = rbind(
  mean.ci.rel.total.infections.averted.b.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.975))
)


mean.ci.abs.total.infections.averted.b.intr.by.city = cbind(
  mean = apply(abs.total.infections.averted.b.intr.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.b.intr.by.city, 'location', quantile, probs=0.975)
)
mean.ci.abs.total.infections.averted.b.intr.by.city = mean.ci.abs.total.infections.averted.b.intr.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.b.intr.by.city = rbind(
  mean.ci.abs.total.infections.averted.b.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.b.intr.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.b.intr.by.city), probs=0.975))
)


# Prolonged Interruption vs Continue

abs.total.infections.averted.p.intr.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.p.intr',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.p.intr.by.city = abs.total.infections.averted.p.intr.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.p.intr.by.city = cbind(
  mean = apply(rel.total.infections.averted.p.intr.by.city, 'location', mean),
  lower = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.975)
)
mean.ci.rel.total.infections.averted.p.intr.by.city = mean.ci.rel.total.infections.averted.p.intr.by.city[ordered.cities,]

mean.ci.rel.total.infections.averted.p.intr.by.city = rbind(
  mean.ci.rel.total.infections.averted.p.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city) / rowSums(total.infections.continue.by.city), probs=0.975))
)


mean.ci.abs.total.infections.averted.p.intr.by.city = cbind(
  mean = apply(abs.total.infections.averted.p.intr.by.city, 'location', mean),
  lower = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.p.intr.by.city, 'location', quantile, probs=0.975)
)
mean.ci.abs.total.infections.averted.p.intr.by.city = mean.ci.abs.total.infections.averted.p.intr.by.city[ordered.cities,]
mean.ci.abs.total.infections.averted.p.intr.by.city = rbind(
  mean.ci.abs.total.infections.averted.p.intr.by.city,
  c(mean = mean(rowSums(abs.total.infections.averted.p.intr.by.city)),
    lower = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.p.intr.by.city), probs=0.975))
)

interleave = function(v1, v2)
{
    rv = rep(v1, each=2)
    rv[2*(1:length(v2))] = v2
    rv
}

# Make table
table.city = data.frame(
    location = c(get.location.name(ordered.cities),"Total"),
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
    
    excess.infections.b.intr = paste0(format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,1]), big.mark = ','),
                                   " [", format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,2]), big.mark = ','),
                                   "-", format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,3]), big.mark = ','),
                                   "]"),
    rel.excess.infections.b.intr = paste0(round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,1]), "% [",
                                       round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,2]), "-",
                                       round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,3]), "%]"),
    
    excess.infections.p.intr = paste0(format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,1]), big.mark = ','),
                                      "[", format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,2]), big.mark = ','),
                                      "-", format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,3]), big.mark = ','),
                                      "]"),
    rel.excess.infections.p.intr = paste0(round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,1]), "% [",
                                          round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,2]), "-",
                                          round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]")
)

# Make table
table.city = data.frame(
  location = interleave(c(get.location.name(ordered.cities),"Total"),rep('', n.cities+1)),
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
  
  
  excess.infections.b.intr = interleave(format(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,1]), big.mark = ','),
                                     paste0("[", sapply(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,2]), format, big.mark = ','),
                                            "-", sapply(round(mean.ci.abs.total.infections.averted.b.intr.by.city[,3]), format, big.mark = ','),
                                            "]")),
  rel.excess.infections.b.intr = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,1]), "%"),
                                         paste0("[",round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,2]), "-",
                                                round(100*mean.ci.rel.total.infections.averted.b.intr.by.city[,3]), "%]")),
  
  
  excess.infections.p.intr = interleave(format(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,1]), big.mark = ','),
                                     paste0("[", sapply(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,2]), format, big.mark = ','),
                                            "-", sapply(round(mean.ci.abs.total.infections.averted.p.intr.by.city[,3]), format, big.mark = ','),
                                            "]")),
  rel.excess.infections.p.intr = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,1]), "%"),
                                         paste0("[",round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,2]), "-",
                                                round(100*mean.ci.rel.total.infections.averted.p.intr.by.city[,3]), "%]"))
)


color.by = cbind(
    rep(-1, 2*(n.cities+1)),
    rep(-1, 2*(n.cities+1)),
    rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.end.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.b.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.b.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.p.intr.by.city[,1], each=2)
)

source('presentation/make_pretty_table.R')
write.shaded.table(tab = table.city,
                   color.by = color.by,
                   colors = c('white','yellow','red'),
                   thresholds = c(-1,0,1),
                   file = '../../results/ryan_white/shaded_table.xlsx')
