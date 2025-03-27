
PLOT.DIR = '../../results/ryan_white/'
PLOT.HEIGHT = 4
PLOT.WIDTH = 6
PLOT.DPI = 600
PLOT.DEVICE = 'png'

YEARS.TO.CONSIDER = as.character(2025:2030)

n.sim = dim(all.parameters)['simulation']
n.sim.quantile = ceiling(n.sim/5)

# Total, for reference
excess.total = (apply(total.incidence[YEARS.TO.CONSIDER,,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,,,'noint'], 'sim', sum)


# ADAP
order.adap.effect = order(all.parameters['lose.adap.effect',,1,'rw.end'])
adap.low.indices = order.adap.effect[1:n.sim.quantile]
adap.high.indices = order.adap.effect[n.sim + 1 - (n.sim.quantile:1)]

excess.adap.low = (apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum)

excess.adap.low = (apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,adap.low.indices,,'noint'], 'sim', sum)

# OAHS
order.oahs.effect = order(all.parameters['lose.oahs.effect',,1,'rw.end'])
oahs.low.indices = order.oahs.effect[1:n.sim.quantile]
oahs.high.indices = order.oahs.effect[n.sim + 1 - (n.sim.quantile:1)]

excess.oahs.low = (apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,oahs.low.indices,,'noint'], 'sim', sum)

excess.oahs.high = (apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,oahs.high.indices,,'noint'], 'sim', sum)

# Support
order.rw.support.effect = order(all.parameters['lose.rw.support.effect',,1,'rw.end'])
rw.support.low.indices = order.rw.support.effect[1:n.sim.quantile]
rw.support.high.indices = order.rw.support.effect[n.sim + 1 - (n.sim.quantile:1)]

excess.rw.support.low = (apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'rw.end'], 'sim', sum) -
                     apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,rw.support.low.indices,,'noint'], 'sim', sum)

excess.rw.support.high = (apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'rw.end'], 'sim', sum) -
                      apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'noint'], 'sim', sum) ) /
  apply(total.incidence[YEARS.TO.CONSIDER,rw.support.high.indices,,'noint'], 'sim', sum)


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

