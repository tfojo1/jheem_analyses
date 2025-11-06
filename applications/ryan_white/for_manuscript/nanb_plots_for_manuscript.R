DO.FOR.CONSERVATIVE.ANALYSIS = F

YEARS.TO.CONSIDER = as.character(2026:2030)
PLOT.DIR = "../../results/ryan_white_nanb"
ROUND.DIGITS = 1

total.infections.continue.by.loc = apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)


END.NAME = 'nanb.rw.end.26'
INTR.NAME = 'nanb.rw.intr.26'
FILE.SUFFIX = ''
if (DO.FOR.CONSERVATIVE.ANALYSIS)
{
    END.NAME = 'nanb.rw.end.cons.26'
    INTR.NAME = 'nanb.rw.intr.cons.26'
    FILE.SUFFIX = "_cons"
}

RW.INTR.COLOR = RW.P.INTR.COLOR

EHE.PRIORITY.LOCATIONS = c('AL','AR','KY','MO','MS','OK','SC')
NON.EHE.PRIORITY.LOCATIONS = setdiff(RW.LOCATIONS, EHE.PRIORITY.LOCATIONS)

# End vs Continue
abs.total.infections.averted.end.by.loc = apply(total.incidence[YEARS.TO.CONSIDER,,,END.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.end.by.loc = abs.total.infections.averted.end.by.loc  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.end.by.loc = cbind(
  mean = apply(rel.total.infections.averted.end.by.loc, 'location', mean),
  lower = apply(rel.total.infections.averted.end.by.loc, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.end.by.loc, 'location', quantile, probs=0.975),
  iqr.lower = apply(rel.total.infections.averted.end.by.loc, 'location', quantile, probs=0.25),
  iqr.upper = apply(rel.total.infections.averted.end.by.loc, 'location', quantile, probs=0.75)
)
mean.ci.rel.total.infections.averted.end.by.loc = mean.ci.rel.total.infections.averted.end.by.loc[order(mean.ci.rel.total.infections.averted.end.by.loc[,1], decreasing = T),]

n.locs = nrow(mean.ci.rel.total.infections.averted.end.by.loc)
ordered.locs = rownames(mean.ci.rel.total.infections.averted.end.by.loc)

mean.ci.rel.total.infections.averted.end.by.loc = rbind(
  mean.ci.rel.total.infections.averted.end.by.loc,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc) / rowSums(total.infections.continue.by.loc)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.75))
)


mean.ci.abs.total.infections.averted.end.by.loc = cbind(
  mean = apply(abs.total.infections.averted.end.by.loc, 'location', mean),
  lower = apply(abs.total.infections.averted.end.by.loc, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.end.by.loc, 'location', quantile, probs=0.975),
  iqr.lower = apply(abs.total.infections.averted.end.by.loc, 'location', quantile, probs=0.25),
  iqr.upper = apply(abs.total.infections.averted.end.by.loc, 'location', quantile, probs=0.75)
)
mean.ci.abs.total.infections.averted.end.by.loc = mean.ci.abs.total.infections.averted.end.by.loc[ordered.locs,]
mean.ci.abs.total.infections.averted.end.by.loc = rbind(
  mean.ci.abs.total.infections.averted.end.by.loc,
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.end.by.loc)),
    lower = quantile(rowSums(abs.total.infections.averted.end.by.loc), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.end.by.loc), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.end.by.loc), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.end.by.loc), probs=0.75))
)

# Total Infections

mean.ci.total.infections.continue.by.loc = cbind(
  mean = apply(total.infections.continue.by.loc, 'location', mean),
  lower = apply(total.infections.continue.by.loc, 'location', quantile, probs=0.025),
  upper = apply(total.infections.continue.by.loc, 'location', quantile, probs=0.975),
  iqr.lower = apply(total.infections.continue.by.loc, 'location', quantile, probs=0.25),
  iqr.upper = apply(total.infections.continue.by.loc, 'location', quantile, probs=0.75)
)
mean.ci.total.infections.continue.by.loc = mean.ci.total.infections.continue.by.loc[ordered.locs,]
mean.ci.total.infections.continue.by.loc = rbind(
  mean.ci.total.infections.continue.by.loc,
  c(mean = mean(rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(total.infections.continue.by.loc)),
    lower = quantile(rowSums(total.infections.continue.by.loc), probs=0.025),
    upper = quantile(rowSums(total.infections.continue.by.loc), probs=0.975),
    iqr.lower = quantile(rowSums(total.infections.continue.by.loc), probs=0.25),
    iqr.upper = quantile(rowSums(total.infections.continue.by.loc), probs=0.75))
)


# Interruption vs Continue

abs.total.infections.averted.intr.by.loc = apply(total.incidence[YEARS.TO.CONSIDER,,,INTR.NAME,drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.intr.by.loc = abs.total.infections.averted.intr.by.loc  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
mean.ci.rel.total.infections.averted.intr.by.loc = cbind(
  mean = apply(rel.total.infections.averted.intr.by.loc, 'location', mean),
  lower = apply(rel.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.025),
  upper = apply(rel.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.975),
  iqr.lower = apply(rel.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.25),
  iqr.upper = apply(rel.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.75)
)
mean.ci.rel.total.infections.averted.intr.by.loc = mean.ci.rel.total.infections.averted.intr.by.loc[ordered.locs,]

mean.ci.rel.total.infections.averted.intr.by.loc = rbind(
  mean.ci.rel.total.infections.averted.intr.by.loc,
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]) / rowSums(total.infections.continue.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc) / rowSums(total.infections.continue.by.loc)),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc) / rowSums(total.infections.continue.by.loc), probs=0.75))
)


mean.ci.abs.total.infections.averted.intr.by.loc = cbind(
  mean = apply(abs.total.infections.averted.intr.by.loc, 'location', mean),
  lower = apply(abs.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.025),
  upper = apply(abs.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.975),
  iqr.lower = apply(abs.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.25),
  iqr.upper = apply(abs.total.infections.averted.intr.by.loc, 'location', quantile, probs=0.75)
)
mean.ci.abs.total.infections.averted.intr.by.loc = mean.ci.abs.total.infections.averted.intr.by.loc[ordered.locs,]
mean.ci.abs.total.infections.averted.intr.by.loc = rbind(
  mean.ci.abs.total.infections.averted.intr.by.loc,
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS])),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc[,NON.EHE.PRIORITY.LOCATIONS]), probs=0.75)),
  c(mean = mean(rowSums(abs.total.infections.averted.intr.by.loc)),
    lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc), probs=0.025),
    upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc), probs=0.975),
    iqr.lower = quantile(rowSums(abs.total.infections.averted.intr.by.loc), probs=0.25),
    iqr.upper = quantile(rowSums(abs.total.infections.averted.intr.by.loc), probs=0.75))
)

interleave = function(v1, v2)
{
    rv = rep(v1, each=2)
    rv[2*(1:length(v2))] = v2
    rv
}

# Make table

table.loc = data.frame(
    location = c(get.location.name(ordered.locs),
                 "Medicaid Expansion",
                 "Medicaid Non-Expansion",
                 "Total"),
    total.infections.continue = paste0(format(round(mean.ci.total.infections.continue.by.loc[,1]), big.mark = ','),
                                       " [", format(round(mean.ci.total.infections.continue.by.loc[,2]), big.mark = ','),
                                       " - ", format(round(mean.ci.total.infections.continue.by.loc[,3]), big.mark = ','),
                                       
                                       "]"),
    excess.infections.end = paste0(format(round(mean.ci.abs.total.infections.averted.end.by.loc[,1]), big.mark = ','),
                                   " [", format(round(mean.ci.abs.total.infections.averted.end.by.loc[,2]), big.mark = ','),
                                   "-", format(round(mean.ci.abs.total.infections.averted.end.by.loc[,3]), big.mark = ','),
                                   "]"),
    rel.excess.infections.end = paste0(round(100*mean.ci.rel.total.infections.averted.end.by.loc[,1]), "% [",
                                       round(100*mean.ci.rel.total.infections.averted.end.by.loc[,2]), "-",
                                       round(100*mean.ci.rel.total.infections.averted.end.by.loc[,3]), "%]"),
    
    excess.infections.intr = paste0(format(round(mean.ci.abs.total.infections.averted.intr.by.loc[,1]), big.mark = ','),
                                      "[", format(round(mean.ci.abs.total.infections.averted.intr.by.loc[,2]), big.mark = ','),
                                      "-", format(round(mean.ci.abs.total.infections.averted.intr.by.loc[,3]), big.mark = ','),
                                      "]"),
    rel.excess.infections.intr = paste0(round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,1]), "% [",
                                          round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,2]), "-",
                                          round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,3]), "%]")
)



# Make table

if (RW.IS.STATE.LEVEL)
    location.names = locations::get.location.name(ordered.locs)
if (!RW.IS.STATE.LEVEL)
    location.names = RW.loc.SHORT.NAMES[ordered.locs]
    
ehe.mask = sapply(ordered.locs, function(loc){
    any(loc==EHE.PRIORITY.LOCATIONS)
})
location.names[ehe.mask] = paste0(location.names[ehe.mask], '*')

loc.plus.total.names = c(location.names,
                          ehe="EHE Priority States",
                          nonehe="Other States",
                          total="Total")


table.loc = data.frame(
    location = interleave(loc.plus.total.names,
                          rep('', n.locs+3)),
    total.infections.continue = interleave(format(round(mean.ci.total.infections.continue.by.loc[,1]), big.mark = ','),
                                           paste0("[", sapply(round(mean.ci.total.infections.continue.by.loc[,2]), format, big.mark = ','),
                                                  "-", sapply(round(mean.ci.total.infections.continue.by.loc[,3]), format, big.mark = ','),
                                                  
                                                  "]")),
    
    excess.infections.end = interleave(format(round(mean.ci.abs.total.infections.averted.end.by.loc[,1]), big.mark = ','),
                                       paste0("[", sapply(round(mean.ci.abs.total.infections.averted.end.by.loc[,2]), format, big.mark = ','),
                                              "-", sapply(round(mean.ci.abs.total.infections.averted.end.by.loc[,3]), format, big.mark = ','),
                                              "]")),
    rel.excess.infections.end = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.end.by.loc[,1]), "%"),
                                           paste0("[",round(100*mean.ci.rel.total.infections.averted.end.by.loc[,2]), "-",
                                                  round(100*mean.ci.rel.total.infections.averted.end.by.loc[,3]), "%]")),
    
    
    excess.infections.intr = interleave(format(round(mean.ci.abs.total.infections.averted.intr.by.loc[,1]), big.mark = ','),
                                          paste0("[", sapply(round(mean.ci.abs.total.infections.averted.intr.by.loc[,2]), format, big.mark = ','),
                                                 "-", sapply(round(mean.ci.abs.total.infections.averted.intr.by.loc[,3]), format, big.mark = ','),
                                                 "]")),
    rel.excess.infections.intr = interleave(paste0(round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,1]), "%"),
                                              paste0("[",round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,2]), "-",
                                                     round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,3]), "%]"))
)

table.loc.single.line = data.frame(
    location = loc.plus.total.names,
    total.infections.continue = paste(format(round(mean.ci.total.infections.continue.by.loc[,1]), big.mark = ','),
                                           paste0("[", sapply(round(mean.ci.total.infections.continue.by.loc[,2]), format, big.mark = ','),
                                                  "-", sapply(round(mean.ci.total.infections.continue.by.loc[,3]), format, big.mark = ','),
                                                  
                                                  "]")),
    
    excess.infections.end = paste(format(round(mean.ci.abs.total.infections.averted.end.by.loc[,1]), big.mark = ','),
                                       paste0("[", sapply(round(mean.ci.abs.total.infections.averted.end.by.loc[,2]), format, big.mark = ','),
                                              "-", sapply(round(mean.ci.abs.total.infections.averted.end.by.loc[,3]), format, big.mark = ','),
                                              "]")),
    rel.excess.infections.end = paste(paste0(format(round(100*mean.ci.rel.total.infections.averted.end.by.loc[,1], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "%"),
                                           paste0("[",format(round(100*mean.ci.rel.total.infections.averted.end.by.loc[,2], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "-",
                                                  format(round(100*mean.ci.rel.total.infections.averted.end.by.loc[,3], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "%]")),
    
    
    excess.infections.intr = paste(format(round(mean.ci.abs.total.infections.averted.intr.by.loc[,1]), big.mark = ','),
                                        paste0("[", sapply(round(mean.ci.abs.total.infections.averted.intr.by.loc[,2]), format, big.mark = ','),
                                               "-", sapply(round(mean.ci.abs.total.infections.averted.intr.by.loc[,3]), format, big.mark = ','),
                                               "]")),
    rel.excess.infections.intr = paste(paste0(format(round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,1], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "%"),
                                            paste0("[",format(round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,2], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "-",
                                                   format(round(100*mean.ci.rel.total.infections.averted.intr.by.loc[,3], digits = ROUND.DIGITS), nsmall = ROUND.DIGITS), "%]"))
)

color.by = cbind(
    rep(-1, 2*(n.locs+3)),
    rep(-1, 2*(n.locs+3)),
    rep(mean.ci.rel.total.infections.averted.end.by.loc[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.end.by.loc[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.intr.by.loc[,1], each=2),
    rep(mean.ci.rel.total.infections.averted.intr.by.loc[,1], each=2)
)

color.by.single.line = cbind(
    rep(-1, (n.locs+3)),
    rep(-1, (n.locs+3)),
    mean.ci.rel.total.infections.averted.end.by.loc[,1],
    mean.ci.rel.total.infections.averted.end.by.loc[,1],
    mean.ci.rel.total.infections.averted.intr.by.loc[,1],
    mean.ci.rel.total.infections.averted.intr.by.loc[,1]
)


# text.color = matrix(RW.EXP.LABEL.COLOR, nrow=nrow(color.by), ncol=ncol(color.by))
# ehe.mask = sapply(names(loc.plus.total.names), function(loc){
#     any(loc == EHE.PRIORITY.LOCATIONS)
# })
# ehe.mask['ehe'] = T
# total.mask = names(loc.plus.total.names) == 'total'
# text.color[2*(1:length(loc.plus.total.names)),][!ehe.mask,] = RW.NONEXP.LABEL.COLOR
# text.color[2*(1:length(loc.plus.total.names))-1,][nonexp.mask,] = RW.NONEXP.LABEL.COLOR
# text.color[2*(1:length(loc.plus.total.names)),][total.mask,] = RW.TOTAL.LABEL.COLOR
# text.color[2*(1:length(loc.plus.total.names))-1,][total.mask,] = paste0(RW.TOTAL.TEXT.COLOR,'FF')
# text.color[,-1] = '#FFFFFFFF'


source('presentation/make_pretty_table.R')
write.shaded.table(tab = table.loc.single.line,
                   color.by = color.by.single.line,
                   colors = c('white','yellow','red'),
                   thresholds = c(-0.5,0,0.5),
                 #  text.color = text.color,
                   file = file.path(PLOT.DIR, paste0('shaded_table', FILE.SUFFIX, '.xlsx')))
# 
# write.shaded.table(tab = table.loc[2*(1:length(loc.plus.total.names))-1,1,drop=F],
#                    color.by = color.by[2*(1:length(loc.plus.total.names))-1,1,drop=F],
#                    colors = c('white','yellow','red'),
#                    thresholds = c(-1,0,1),
#                    text.color = text.color[2*(1:length(loc.plus.total.names))-1,1,drop=F],
#                    file = file.path(PLOT.DIR, 'rownames_shaded_table.xlsx'))


boxplot.df.end = as.data.frame(mean.ci.rel.total.infections.averted.end.by.loc)
boxplot.df.end$Scenario = '3.end'
boxplot.df.end$location = loc.plus.total.names
boxplot.df.end$loc.code = names(loc.plus.total.names)

boxplot.df.intr = as.data.frame(mean.ci.rel.total.infections.averted.intr.by.loc)
boxplot.df.intr$Scenario = '2.intr'
boxplot.df.intr$location = loc.plus.total.names
boxplot.df.intr$loc.code = names(loc.plus.total.names)

boxplot.df.spacer = boxplot.df.end
boxplot.df.spacer$mean = boxplot.df.spacer$lower = boxplot.df.spacer$upper = boxplot.df.spacer$iqr.lower = boxplot.df.spacer$iqr.upper = NA
boxplot.df.spacer$Scenario = '0.spacer'
boxplot.df.spacer = boxplot.df.spacer[boxplot.df.spacer$loc.code!='total',]

boxplot.df = rbind(
    boxplot.df.spacer,
    boxplot.df.end,
    boxplot.df.intr
)




boxplot.df$location = factor(boxplot.df$location,
                             levels = c(loc.plus.total.names[c('total','nonehe','ehe')],
                                        rev(location.names)))

boxplot.df$group = paste0(boxplot.df$Scenario, "_", boxplot.df$location)


nonehe.mask = sapply(boxplot.df$loc.code, function(loc){
    any(loc==NON.EHE.PRIORITY.LOCATIONS)
})
nonehe.mask[boxplot.df$loc.code=='nonehe'] = T
total.mask = boxplot.df$loc.code == 'total'
total.and.subtotal.mask = total.mask | boxplot.df$loc.code == 'ehe' | boxplot.df$loc.code == 'nonehe'

location.colors = rep(RW.EXP.LABEL.COLOR, length(boxplot.df$location))
location.colors[nonehe.mask] = RW.NONEXP.LABEL.COLOR
location.face = rep('plain', length(boxplot.df$location))

location.colors[total.mask] = RW.TOTAL.LABEL.COLOR
location.face[total.and.subtotal.mask] = 'bold'

MAX = 0.85
boxplot.df$upper = pmin(boxplot.df$upper, MAX)

asterisk.df = boxplot.df[!is.na(boxplot.df$upper) & boxplot.df$upper==MAX,]

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
    # ggtext::geom_richtext(data = asterisk.df[asterisk.df$Scenario=='3.end',],
    #           aes(y=location, x=upper, label="&#8224;", group=group, vjust=0.5, hjust=0.5),
    #           nudge_y=.25, nudge_x = 0.006) +
    geom_text(data = asterisk.df[asterisk.df$Scenario=='3.end',],
              aes(y=location, x=upper, label="**", group=group, vjust=0.5, hjust=0.5),
              nudge_y=.25, nudge_x = 0.006) +
    geom_text(data = asterisk.df[asterisk.df$Scenario!='3.end',],
              aes(y=location, x=upper, label='**', group=group, vjust=0.5, hjust=0.5),
              nudge_y=0, nudge_x = 0.006) +
    scale_fill_manual(values = c('3.end'=RW.END.COLOR,
                                 '2.intr'=RW.INTR.COLOR),
                      labels = c('3.end'='Cessation',
                                 '2.intr'='3-year Interruption'),
                      name=NULL) +
    theme_bw() +
    scale_x_continuous(labels = scales::percent, sec.axis = dup_axis(name=NULL)) +
    ylab(NULL) +
    xlab("Relative Increase in HIV Infections, 2026-2030") + 
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          axis.text.y = element_text(face=rev(location.face))); print(plot)



PLOT.HEIGHT = 7
PLOT.WIDTH = 6.5
PLOT.DPI = 600
PLOT.DEVICE = 'png'

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(tolower(RW.LOCATION.DESCRIPTOR), '_boxplots', FILE.SUFFIX,'.png')),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
