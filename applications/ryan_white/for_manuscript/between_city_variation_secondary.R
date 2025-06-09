
library(ggplot2)
library(tibble)


CUMULATIVE.YEARS = as.character(2025:2030)
BASELINE.YEAR = as.character(2025)
PRE.BASELINE.YEAR = as.character(2016)


excess.incidence.end.vs.noint = (apply(total.incidence[CUMULATIVE.YEARS,,,'rw.end',drop=F] - total.incidence[CUMULATIVE.YEARS,,,'noint',drop=F], c('location','sim'), sum, na.rm=T)) / 
  apply(total.incidence[CUMULATIVE.YEARS,,,'noint',drop=F], c('location','sim'), sum, na.rm=T)
mean.excess.incidence.end.vs.noint = apply(excess.incidence.end.vs.noint, 'location', mean, na.rm=T)

delta.new.baseline = (total.new[BASELINE.YEAR,,,'noint'] - total.new[PRE.BASELINE.YEAR,,,'noint']) / total.new[PRE.BASELINE.YEAR,,,'noint']

#total.suppression = apply(full.results[,,,,,,'suppression',,], c('year','sim','location','intervention'), sum, na.rm=T) /
#  apply(full.results[,,,,,,'diagnosed.prevalence',,], c('year','sim','location','intervention'), sum, na.rm=T)

total.suppression = total.results[,,'suppression',,] / total.results[,,'diagnosed.prevalence',,]

delta.suppression.baseline = (total.suppression[BASELINE.YEAR,,,'noint'] - total.suppression[PRE.BASELINE.YEAR,,,'noint']) / total.suppression[PRE.BASELINE.YEAR,,,'noint']


#parameter.means = apply(all.parameters[,,,'noint'], c('parameter','location'), mean, na.rm=T)



#-- The city-level scatterplots --#

PLOT.DIR = file.path('../../results/ryan_white/figure_city_variation/')
PLOT.HEIGHT = 3
PLOT.WIDTH = 3
PLOT.DPI = 600
PLOT.DEVICE = 'png'


baseline.total.results = total.results[BASELINE.YEAR,,,,'noint'] # apply(full.results[BASELINE.YEAR,,,,,,,,'noint'], c('sim','outcome','location'), sum, na.rm=T)
baseline.total.results[,'oahs.suppression',] = baseline.total.results[,'oahs.suppression',] /  baseline.total.results[,'oahs.clients',]
baseline.total.results[,'adap.suppression',] = baseline.total.results[,'adap.suppression',] /  baseline.total.results[,'adap.clients',]
for (outcome.to.div in c('suppression','rw.clients','non.adap.clients','oahs.clients','adap.clients'))
    baseline.total.results[,outcome.to.div,] = baseline.total.results[,outcome.to.div,] / baseline.total.results[,'diagnosed.prevalence',]
baseline.totals.by.city = apply(baseline.total.results, c('outcome','location'), mean, na.rm=T)


total.prevalence = apply(total.results[BASELINE.YEAR,,'diagnosed.prevalence',,'noint'], c('sim', 'location'), sum) #apply(full.results[BASELINE.YEAR,,,,,,'diagnosed.prevalence',,'noint'], c('sim','location'), sum)

df.city.summ = cbind(data.frame(
  location = names(mean.excess.incidence.end.vs.noint),
  excess = mean.excess.incidence.end.vs.noint,
  medicaid = 'Non-Expansion State',
  delta.new = apply(delta.new.baseline, 'location', mean, na.rm=T),
  delta.suppression = apply(delta.suppression.baseline, 'location', mean, na.rm=T),
  sexual.transmission = apply(total.sexual.transmission[BASELINE.YEAR,,,'noint'] / total.prevalence, 'location', mean, na.rm=T)
#  sexual.transmission = apply(total.sexual.transmission[BASELINE.YEAR,,,'noint'] / total.pop[BASELINE.YEAR,,,'noint'], 'location', mean, na.rm=T) 
),
as.data.frame(t(baseline.totals.by.city)))
df.city.summ$medicaid[city.main.state.medicaid.expansion] = "Expansion State"
df.city.summ$name = get.location.name(rownames(df.city.summ))
df.city.summ$new.per.pop = df.city.summ$new / df.city.summ$population
df.city.summ$new.per.prev = df.city.summ$new / df.city.summ$diagnosed.prevalence



pcc.df = df.city.summ[,c('rw.clients',
                   #      'adap.clients',
                #         'oahs.clients',
                         'suppression',
                 #        'oahs.suppression',
                  #       'adap.suppression',
                         'new.per.pop',
                         'new.per.prev',
#                         'sexual.transmission',
                         'medicaid',
                         'excess')]
#pcc.df = df.city.summ[,c('rw.clients','suppression','oahs.suppression','adap.suppression','new','sexual.transmission','medicaid','excess')]
prccs = epiR::epi.prcc(pcc.df); prccs[order(abs(prccs$est), decreasing = T),]
prccs.est = prccs$est
names(prccs.est) = prccs$var

prccs.est = prccs.est[order(abs(prccs.est), decreasing = T)]

cat(paste0("The top 4 PRCCs for city variation are:\n",
           paste0("- ", names(prccs.est[1:4]),
                  ": ",
                  round(prccs.est[1:4],2),
                  collapse='\n'),
           "\n"))


CITY.SIZE.SCALE = scale_size(range = c(1,4), guide = NULL)
CITY.COLOR = scale_fill_manual(name = NULL,
                               values = c("Non-Expansion State" = RW.NONEXP.COLOR,
                                "Expansion State" = RW.EXP.COLOR),
                               labels = c("Non-Expansion State" = "Non-Expansion States",
                                          "Expansion State" = "Expansion States"))
THEME =  theme_bw(base_size = 10)
SEGMENT.SIZE = 0.25
LABEL.SIZE = 3
PRCC.SIZE = 3.5

HOFFSET = -.1
VOFFSET = 0.1
base.df.label = df.city.summ
base.df.label$city.name = RW.CITY.SHORT.NAMES[base.df.label$location]


# Ryan White Clients
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA, COLUMBUS.MSA),]
df.label$x = df.label$rw.clients - 0.05
df.label$y = df.label$excess + 0.02
df.label$y[1] = df.label$excess[1] + 0.11
#df.label$x[2] = df.label$rw.clients[2] - 0.04
df.label$y[2] = df.label$excess[2] + 0 - 0.065
df.label$x[3] = df.label$rw.clients[3] + 0.05
df.label$y[3] = df.label$excess[3] - 0.03
df.label$x[4] = df.label$rw.clients[4] - 0.045
df.label$y[4] = df.label$excess[4] - 0.25
df.label$name = paste0(df.label$city.name, " ")

df.prcc = data.frame(
  value = paste0("PRCC: ", format(round(prccs.est['rw.clients'],2), nsmall = 2)),
  x = 0,
  y = 1.15
)

plot = ggplot() + 
  geom_point(data = df.city.summ, 
             aes(rw.clients, excess, size=new, fill=medicaid), 
             shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of HIV+ Residents\nReceiving Ryan White Services in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=rw.clients, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1.15)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 0.5, hjust = 0, show.legend = F) +
    geom_text(data=df.label[1,], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 0, hjust=1, show.legend = F) +
    geom_text(data=df.label[3,], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(2,4),], 
              aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 1, hjust=1, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_rw_clients.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# Total Suppression
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA, SEATTLE.MSA, SAN.DIEGO.MSA),]
df.label$x = df.label$suppression - 0.015
df.label$y = df.label$excess + 0.05
df.label$y[1] = df.label$excess[1] + 0.07
df.label$x[1] = df.label$suppression[1] - 0.02
df.label$y[2] = df.label$excess[2] - 0.08
df.label$x[3] = df.label$suppression[3] + 0.015
df.label$y[3] = df.label$excess[3] - 0.03
df.label$y[4] = df.label$excess[4] - 0.1
df.label$x[5] = df.label$suppression[5] + 0.0125
df.label$y[5] = df.label$excess[5] - 0.09
df.label$name = paste0(df.label$city.name, "")

df.prcc = data.frame(
  value = paste0("PRCC: ", round(prccs.est['suppression'],2)),
  x = 0.6,
  y = 1.15
)

plot = ggplot() + 
  geom_point(data=df.city.summ, 
             aes(suppression, excess, size=new, fill=medicaid),
             shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of All HIV+ Residents\nVirally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.15)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.6,0.9)) +
  geom_label(data=df.prcc,
            aes(x, y, label=value), size=PRCC.SIZE,
            vjust = 0.5, hjust = 0, show.legend = F) +
    geom_text(data=df.label[c(1),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.015,
              vjust = 0, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(2),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=1, show.legend = F) +
    geom_text(data=df.label[c(4),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 1, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(3),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(5),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 1, hjust=0.5, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(suppression, excess, size=new, fill=medicaid),
               shape=21, show.legend = T) + 
    ylab(NULL) +
    xlab(NULL) + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.15)) +
    scale_x_continuous(labels = scales::percent, limits = c(0.6,0.9)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 0.5, hjust = 0, show.legend = F) +
    geom_text(data=df.label[c(1),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.015,
              vjust = 0, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(2),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=1, show.legend = F) +
    geom_text(data=df.label[c(4),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 1, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(3),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(5),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 1, hjust=0.5, show.legend = F) +
    theme_bw(base_size = 10*1.2) + theme(legend.position = 'bottom') +
    guides(fill = guide_legend(override.aes = list(size = 7)));plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "dummy_for_legend.png"),
       height = PLOT.HEIGHT/2, width = PLOT.WIDTH*2, dpi = PLOT.DPI, device = PLOT.DEVICE)

# ADAP Suppression
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA),]
df.label$x = df.label$adap.suppression - 0.015
df.label$y = df.label$excess + 0.03
df.label$name = paste0(df.label$city.name, " ")


df.prcc = data.frame(
  value = paste0("PRCC: ", round(prccs.est['adap.suppression'],2)),
  x = 0.72,
  y = 1
)

plot = ggplot() + 
  geom_point(data=df.city.summ, 
             aes(adap.suppression, excess, size=new, fill=medicaid),
             shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of AIDS Drug Assistance\nClients Virally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.72,0.95)) +
  geom_label(data=df.prcc,
            aes(x, y, label=value), size=PRCC.SIZE,
            vjust = 0.5, hjust = 0, show.legend = F) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_adap_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)




# OAHS Suppression
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA),]
df.label$x = df.label$oahs.suppression - 0.005
df.label$y = df.label$excess + 0.05
df.label$name = paste0(df.label$city.name, " ")

df.prcc = data.frame(
  value = paste0("PRCC: ", round(prccs.est['oahs.suppression'],2)),
  x = 0.84,
  y = 1
)

plot = ggplot() + 
  geom_point(data=df.city.summ, 
             aes(oahs.suppression, excess, size=new, fill=medicaid),
             shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of Outpatient Ambulatory\nClients Virally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=oahs.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.84,0.94)) +
  geom_label(data=df.prcc,
            aes(x, y, label=value), size=PRCC.SIZE,
            vjust = 0.5, hjust = 0, show.legend = F) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_oahs_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)




plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(new.per.prev, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of Outpatient Ambulatory\nClients Virally Suppressed in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
 #   geom_segment(data = df.label, aes(x, y, xend=oahs.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.1)) +
#    scale_x_continuous(labels = scales::percent, limits = c(0.84,0.94)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 0.5, hjust = 0, show.legend = F) +
    geom_text(data=df.label, 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 0, hjust=1, show.legend = F); print(plot)






