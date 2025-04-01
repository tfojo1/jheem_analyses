
PLOT.DIR = '../../results/ryan_white/'
PLOT.HEIGHT = 4
PLOT.WIDTH = 6
PLOT.DPI = 600
PLOT.DEVICE = 'png'

YEARS.TO.CONSIDER = as.character(2025:2030)

n.sim = dim(all.parameters)['simulation']
n.sim.quantile = ceiling(n.sim/5)
quantile.p = n.sim.quantile/n.sim

# Total, for reference
excess.total = (apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,,,'noint'], 'sim', sum)


# ADAP
order.adap.effect = order(all.parameters['lose.adap.nonexpansion.effect',,1,'rw.end'])
adap.low.indices = order.adap.effect[1:n.sim.quantile]
adap.high.indices = order.adap.effect[n.sim + 1 - (n.sim.quantile:1)]

range(all.parameters['lose.adap.nonexpansion.effect',adap.low.indices,1,'rw.end'])
range(all.parameters['lose.adap.nonexpansion.effect',adap.high.indices,1,'rw.end'])
range(all.parameters['lose.oahs.nonexpansion.effect',adap.low.indices,1,'rw.end'])
range(all.parameters['lose.oahs.nonexpansion.effect',adap.high.indices,1,'rw.end'])
range(all.parameters['lose.rw.support.nonexpansion.effect',adap.low.indices,1,'rw.end'])
range(all.parameters['lose.rw.support.nonexpansion.effect',adap.high.indices,1,'rw.end'])

excess.adap.low = (apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum)

excess.adap.high = (apply(total.incidence[YEARS.TO.CONSIDER,adap.high.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,adap.high.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,adap.high.indices,,'noint'], 'sim', sum)

print(paste0("For the ",
             100*quantile.p, "% of simulations with adap,effect between ",
             round(100*min(all.parameters['lose.adap.nonexpansion.effect',adap.low.indices,1,'rw.end'],
                           all.parameters['lose.adap.expansion.effect',adap.low.indices,1,'rw.end'])),
             "% and ",
             round(100*max(all.parameters['lose.adap.nonexpansion.effect',adap.low.indices,1,'rw.end'],
                           all.parameters['lose.adap.expansion.effect',adap.low.indices,1,'rw.end'])),
             "%, the mean relative increase in infections was ",
             round(100*mean(excess.adap.low)),
             "% [",
             round(100*quantile(excess.adap.low, probs=0.025)),
             " to ",
             round(100*quantile(excess.adap.low, probs=0.975)),
             "%]"))


print(paste0("For the ",
             100*quantile.p, "% of simulations with adap,effect between ",
             round(100*min(all.parameters['lose.adap.nonexpansion.effect',adap.high.indices,1,'rw.end'],
                           all.parameters['lose.adap.expansion.effect',adap.high.indices,1,'rw.end'])),
             "% and ",
             round(100*max(all.parameters['lose.adap.nonexpansion.effect',adap.high.indices,1,'rw.end'],
                           all.parameters['lose.adap.expansion.effect',adap.high.indices,1,'rw.end'])),
             "%, the mean relative increase in infections was ",
             round(100*mean(excess.adap.high)),
             "% [",
             round(100*quantile(excess.adap.high, probs=0.025)),
             " to ",
             round(100*quantile(excess.adap.high, probs=0.975)),
             "%]"))



# OAHS
order.oahs.effect = order(all.parameters['lose.oahs.nonexpansion.effect',,1,'rw.end'])
oahs.low.indices = order.oahs.effect[1:n.sim.quantile]
oahs.high.indices = order.oahs.effect[n.sim + 1 - (n.sim.quantile:1)]

excess.oahs.low = (apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'noint'], 'sim', sum)

excess.oahs.high = (apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'noint'], 'sim', sum)

# Support
order.rw.support.effect = order(all.parameters['lose.rw.support.nonexpansion.effect',,1,'rw.end'])
rw.support.low.indices = order.rw.support.effect[1:n.sim.quantile]
rw.support.high.indices = order.rw.support.effect[n.sim + 1 - (n.sim.quantile:1)]

excess.rw.support.low = (apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'noint'], 'sim', sum)

excess.rw.support.high = (apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'noint'], 'sim', sum)

# Print about the lowest vs highest combo about all three

rw.param.names = c(
    'lose.adap.expansion.effect',
    'lose.adap.nonexpansion.effect',
    'lose.oahs.expansion.effect',
    'lose.oahs.nonexpansion.effect',
    'lose.rw.support.expansion.effect',
    'lose.rw.support.nonexpansion.effect'
)
params = all.parameters[rw.param.names,,1,'rw.end']
full.rw.params = RW.effect.values[c(1,4,2,5,3,6),]
param.medians = apply(params, 1, median)
param.q25 = apply(params, 1, quantile, probs=0.25)
param.q75 = apply(params, 1, quantile, probs=0.75)
param.less.than.median = t(sapply(1:length(param.medians), function(i){
    params[i,] < param.medians[i]
}))

mask.all.lt.median = apply(param.less.than.median, 2, all)
mask.all.gt.median = apply(!param.less.than.median, 2, all)

lt.median.max.values = apply(params[,mask.all.lt.median], 1, max)
gt.median.min.values = apply(params[,mask.all.gt.median], 1, min)

mask.all.lt.50 = apply(params < 0.5, 2, all)
sum(mask.all.lt.50)
mean(excess.total[mask.all.lt.50])

mask.all.gt.60 = apply(params >0.6, 2, all)
sum(mask.all.gt.60)
mean(excess.total[mask.all.gt.60])

print(paste0("For the ",
             100*mean(mask.all.lt.50), "% of simulations with all suppression effects < 50%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.all.lt.50])),
             "% [",
             round(100*quantile(excess.total[mask.all.lt.50], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.all.lt.50], probs=0.975)),
             "%]"))

print(paste0("For the ",
             100*mean(mask.all.gt.60), "% of simulations with all suppression effects > 60%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.all.gt.60])),
             "% [",
             round(100*quantile(excess.total[mask.all.gt.60], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.all.gt.60], probs=0.975)),
             "%]"))



mask.all.adap.lt.45 = apply(params[1:2,] < 0.45, 2, all)
sum(mask.all.adap.lt.45)
mean(excess.total[mask.all.adap.lt.45])

mask.all.adap.gt.90 = apply(params[1:2,] > 0.9, 2, all)
sum(mask.all.adap.gt.90)
mean(excess.total[mask.all.adap.gt.90])

sum(apply(full.rw.params[1:2,] > 0.9, 2, all))
sum(apply(full.rw.params[1:2,] < 0.45, 2, all))

print(paste0("For the ",
             100*mean(mask.all.adap.lt.45), "% of simulations with all ADAP effects < 45%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.all.adap.lt.45])),
             "% [",
             round(100*quantile(excess.total[mask.all.adap.lt.45], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.all.adap.lt.45], probs=0.975)),
             "%]"))

print(paste0("For the ",
             100*mean(mask.all.adap.gt.90), "% of simulations with all ADAP suppression effects >90%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.all.adap.gt.90])),
             "% [",
             round(100*quantile(excess.total[mask.all.adap.gt.90], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.all.adap.gt.90], probs=0.975)),
             "%]"))


true.mask.lt.50.50.30 = apply(full.rw.params[1:2,] < 0.5, 2, all) & apply(full.rw.params[3:4,] < 0.5, 2, all) & apply(full.rw.params[5:6,] < 0.3, 2, all)
mask.lt.50.50.30 = apply(params[1:2,] < 0.5, 2, all) & apply(params[3:4,] < 0.5, 2, all) & apply(params[5:6,] < 0.3, 2, all)
mean(mask.lt.50.50.30)

true.mask.gt.90.50.30 = apply(full.rw.params[1:2,] > 0.9, 2, all) & apply(full.rw.params[3:4,] > 0.9, 2, all) & apply(full.rw.params[5:6,] > 0.3, 2, all)
mask.gt.90.50.30 = apply(params[1:2,] > 0.9, 2, all) & apply(params[3:4,] > 0.5, 2, all) & apply(params[5:6,] > 0.3, 2, all)
mean(mask.gt.90.50.30)

print(paste0("For the ",
             100*mean(mask.lt.50.50.30), "% of simulations with ADAP effects < 50%, OAHS effects <50%, support effects <30%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.lt.50.50.30])),
             "% [",
             round(100*quantile(excess.total[mask.lt.50.50.30], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.lt.50.50.30], probs=0.975)),
             "%]"))

print(paste0("For the ",
             100*mean(mask.gt.90.50.30), "% of simulations with ADAP effects >90%, OAHS effects >50%, and support effects >30%",
             ", the mean relative increase in infections was ",
             round(100*mean(excess.total[mask.gt.90.50.30])),
             "% [",
             round(100*quantile(excess.total[mask.gt.90.50.30], probs=0.025)),
             " to ",
             round(100*quantile(excess.total[mask.gt.90.50.30], probs=0.975)),
             "%]"))

# Make the Plot
values = list(adap=excess.adap.low,
              adap=excess.adap.high,
              oahs=excess.oahs.low,
              oahs=excess.oahs.high,
              rw.support=excess.rw.support.low,
              rw.support=excess.rw.support.high)

variable.names = c(
  adap="Proportion Receiving\nAIDS Drug Assistance\nWho Lose Viral Suppression",
  oahs="Proportion Receiving Outpatient\nAmbulatory Health Services\nbut NOT AIDS Drug Assistance\nWho Lose Viral Suppression",
  rw.support="Proportion Receiving Any Other\nRyan White Services\nWho Lose Viral Suppression"
)

df.high.low = data.frame(
  short.varname = names(values),
  quantile = rep(c('low','high'), length(values)/2),
  estimate = sapply(values, mean),
  iqr.lower = sapply(values, quantile, probs=0.25),
  iqr.upper = sapply(values, quantile, probs=0.75),
  ci.lower = sapply(values, quantile, probs=0.025),
  ci.upper = sapply(values, quantile, probs=0.975)
)
df.high.low$group = paste0(df.high.low$quantile, "_", df.high.low$short.varname)
df.high.low$variable = variable.names[df.high.low$short.varname]

plot = ggplot() + 
  geom_boxplot(data=df.high.low,
               aes(y=variable, 
                   xmiddle=estimate,
                   xlower=iqr.lower,
                   xupper=iqr.upper,
                   xmin=ci.lower,
                   xmax=ci.upper,
                   fill=quantile,
                   group=group), 
               stat = 'identity',
               position = 'dodge') +
  scale_fill_manual(NULL,
                    values = c(low=RW.PALETTE[2],
                               high=RW.PALETTE[3]),
                    label = c(c(low = "Lowest 20% of Parameter Values", 
                                high = "Highest 20% of Parameter Values"))) +
  scale_x_continuous(labels=scales::percent, name='Relative Increase in\nHIV Infections 2025-2030') +
  ylab(NULL) +
  theme_bw(base_size = 10) + theme(legend.position = 'bottom', legend.direction = 'horizontal') + 
 # guide_legend(position = 'bottom') +
  geom_vline(linetype='dashed', xintercept = mean(excess.total)); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "sensitivity_high_low.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

