
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

PLOT.DIR = file.path(RW.ROOT.PLOT.DIR, 'figure_city_variation/')
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
if(!RW.IS.STATE.LEVEL)
    df.city.summ$medicaid[city.main.state.medicaid.expansion] = "Expansion State"
if(RW.IS.STATE.LEVEL)
    df.city.summ$medicaid[sapply(df.city.summ$location,function(loc){any(loc==RW.MEDICAID.EXPANSION.LOCATIONS)}) ] = "Expansion State"
df.city.summ$name = get.location.name(rownames(df.city.summ))
df.city.summ$new.per.pop = df.city.summ$new / df.city.summ$population
df.city.summ$new.per.prev = df.city.summ$new / df.city.summ$diagnosed.prevalence

if (1==2)
    df.city.summ$awareness = apply(total.awareness['2025',,,'rw.end'], 'location', mean)

pcc.df = df.city.summ[,c('rw.clients',
                 #        'adap.clients',
                #         'oahs.clients',
                         'suppression',
                  #       'oahs.suppression',
                         'adap.suppression',
                    #     'new.per.pop',
                         'new.per.prev',
                       #  'sexual.transmission',
                         'medicaid',
                         'awareness',
                         'excess')]
#pcc.df = df.city.summ[,c('rw.clients','suppression','oahs.suppression','adap.suppression','new','sexual.transmission','medicaid','excess')]
prccs = epiR::epi.prcc(pcc.df); prccs[order(abs(prccs$est), decreasing = T),]
prccs.est = prccs$est
names(prccs.est) = prccs$var; prccs.est[order(abs(prccs.est), decreasing = T)]

prccs.est = prccs.est[order(abs(prccs.est), decreasing = T)]

cat(paste0("The top 5 PRCCs for city variation are:\n",
           paste0("- ", names(prccs.est[1:5]),
                  ": ",
                  round(prccs.est[1:5],2),
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
       filename=file.path(PLOT.DIR, "Figure_5A.png"),
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
       filename=file.path(PLOT.DIR, "Figure_5C.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# For legend
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


legend = cowplot::get_plot_component(plot, 'guide-box-bottom', return_all = TRUE)

ggsave(plot = legend, 
       filename=file.path(PLOT.DIR, "Figure_5_legend.png"),
       height = 0.25, width = 4.1, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Sexual Transmission
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA, SEATTLE.MSA, 'C.12940'),]
df.label$x = df.label$sexual.transmission - 0.015
df.label$y = df.label$excess + 0.05
df.label$y[1] = df.label$excess[1] + 0.1
df.label$x[1] = df.label$sexual.transmission[1] - 0.02
df.label$y[2] = df.label$excess[2] - 0.05
df.label$x[2] = df.label$sexual.transmission[2] + 0.015
df.label$x[3] = df.label$sexual.transmission[3] + 0.015
df.label$y[3] = df.label$excess[3] - 0.03
df.label$y[4] = df.label$excess[4] - 0.05
df.label$x[5] = df.label$sexual.transmission[5] + 0.05
df.label$y[5] = df.label$excess[5] + 0.17
df.label$name = paste0(df.label$city.name, "")

df.prcc = data.frame(
    value = paste0("PRCC: ", round(prccs.est['sexual.transmission'],2)),
    x = 0.6,
    y = 1.15
)

plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(sexual.transmission, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Average Transmission\nRate in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=sexual.transmission, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.15)) +
    scale_x_continuous(limits = c(0,0.6)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 0.5, hjust = 1, show.legend = F) +
    geom_text(data=df.label[c(1),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.015,
              vjust = 0, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(2),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(4),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 1, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(3),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(5),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.005,
              vjust = 0, hjust=0.5, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_5B.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# Number of new diagnoses
df.label = base.df.label[c(HOUSTON.MSA, BALTIMORE.MSA, RIVERSIDE.MSA, 'C.14460', 'C.12060'),]
df.label$x = df.label$new.per.pop - 0.015/1000
df.label$y = df.label$excess + 0.02
df.label$y[1] = df.label$excess[1] + 0.07
df.label$x[1] = df.label$new.per.pop[1] - 0.02/1000
df.label$y[2] = df.label$excess[2] - 0.02
df.label$x[2] = df.label$new.per.pop[2] + 0.015/1000
df.label$x[3] = df.label$new.per.pop[3] + 0.015/1000
df.label$y[3] = df.label$excess[3] - 0.03
df.label$y[4] = df.label$excess[4] + 0.13
df.label$x[4] = df.label$new.per.pop[4] + 0.015/1000
df.label$x[5] = df.label$new.per.pop[5] - 0.0125/1000
df.label$y[5] = df.label$excess[5] + 0.09
df.label$name = paste0(df.label$city.name, "")

df.prcc = data.frame(
    value = paste0("PRCC: ", round(prccs.est['new.per.pop'],2)),
    x = 3e-04,
    y = 1.15
)

plot = ggplot() + 
    geom_point(data=df.city.summ, 
               aes(new.per.pop, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("New HIV Diagnoses in 2025\nper 100,000 population") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=new.per.pop, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = function(x){format(x * 100000, big.mark=',')}, limits = c(7e-06,3e-04)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 0.5, hjust = 1, show.legend = F) +
    geom_text(data=df.label[c(1),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.015,
              vjust = 0, hjust=0.5, show.legend = F) +
    geom_text(data=df.label[c(2),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(4),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = -0.005,
              vjust = 0, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(3),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              vjust = 1, hjust=0, show.legend = F) +
    geom_text(data=df.label[c(5),], 
              aes(x, y, label=name), size=LABEL.SIZE,
              nudge_y = 0.015,
              vjust = 0, hjust=0.5, show.legend = F); print(plot)

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_5D.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Medicaid expansion

df.prcc = data.frame(
    value = paste0("PRCC: ", round(prccs.est['new.per.pop'],2)),
    x = 1.27,
    y = 7
)

plot = ggplot() + 
    geom_histogram(data=df.city.summ, 
               aes(excess, fill=medicaid), show.legend = F,
               position = 'dodge',
               bins = 12) + 
    xlab("Relative Excess HIV Infections") +
    ylab("Number of Cities") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    scale_x_continuous(labels = scales::percent, limits=c(0,1.27)) +
#    scale_x_continuous(labels = function(x){format(x * 100000, big.mark=',')}, limits = c(7e-05,3e-04)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 1, hjust = 1, show.legend = F); print(plot)



ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_5E.png"),
       height = PLOT.HEIGHT/2, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# Legend

df.prcc = data.frame(
    value = paste0("PRCC: ", round(prccs.est['medicaid'],2)),
    x = 1.27,
    y = 7
)

plot = ggplot() + 
    geom_histogram(data=df.city.summ, 
                   aes(excess, fill=medicaid), show.legend = T,
                   position = 'dodge',
                   bins = 12) + 
    xlab("Relative Excess HIV Infections") +
    ylab("Number of Cities") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    scale_x_continuous(labels = scales::percent, limits=c(0,1.27)) +
    #    scale_x_continuous(labels = function(x){format(x * 100000, big.mark=',')}, limits = c(7e-05,3e-04)) +
    geom_label(data=df.prcc,
               aes(x, y, label=value), size=PRCC.SIZE,
               vjust = 1, hjust = 1, show.legend = F); print(plot)



ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "Figure_5_legend.png"),
       height = PLOT.HEIGHT/2, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


##-- NOT USED --##


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
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.72,0.95)) +
  geom_label(data=df.prcc,
            aes(x, y, label=value), size=PRCC.SIZE,
            vjust = 0.5, hjust = 0, show.legend = F) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); print(plot)

# ggsave(plot = plot, 
#        filename=file.path(PLOT.DIR, "excess_vs_adap_suppression.png"),
#        height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)




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
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.84,0.94)) +
  geom_label(data=df.prcc,
            aes(x, y, label=value), size=PRCC.SIZE,
            vjust = 0.5, hjust = 0, show.legend = F) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); print(plot)

# ggsave(plot = plot, 
#        filename=file.path(PLOT.DIR, "excess_vs_oahs_suppression.png"),
#        height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)




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







if (1==2)
{
    values = lapply(1:ncol(df.city.summ), function(i){
        vals = df.city.summ[,i]
        names(vals)=dimnames(df.city.summ)[[1]]
        vals
    })
    names(values) = dimnames(df.city.summ)[[2]]
    
    print("RW Clients")
    sum(values$rw.clients[RW.MEDICAID.EXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS])
    sum(values$rw.clients[RW.MEDICAID.NONEXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS])
    
    print("ADAP Clients")
    sum(values$adap.clients[RW.MEDICAID.EXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS])
    sum(values$adap.clients[RW.MEDICAID.NONEXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS])
    
    
    print("ADAP Suppression")
    sum(values$adap.suppression[RW.MEDICAID.EXPANSION.LOCATIONS] * values$adap.clients[RW.MEDICAID.EXPANSION.LOCATIONS]) / sum(values$adap.clients[RW.MEDICAID.EXPANSION.LOCATIONS])
    sum(values$adap.suppression[RW.MEDICAID.NONEXPANSION.LOCATIONS] * values$adap.clients[RW.MEDICAID.NONEXPANSION.LOCATIONS]) / sum(values$adap.clients[RW.MEDICAID.NONEXPANSION.LOCATIONS])
    
    
    print("New per prev")
    sum(values$new.per.prev[RW.MEDICAID.EXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS])
    sum(values$new.per.prev[RW.MEDICAID.NONEXPANSION.LOCATIONS] * values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS])
    
    print("Awareness")
    sum(values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.EXPANSION.LOCATIONS] / values$awareness[RW.MEDICAID.EXPANSION.LOCATIONS])
    sum(values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS]) / sum(values$diagnosed.prevalence[RW.MEDICAID.NONEXPANSION.LOCATIONS] / values$awareness[RW.MEDICAID.NONEXPANSION.LOCATIONS])
}



# FOR EXPLORATION
if (1==2)
{
ggplot() + 
    geom_point(data=df.city.summ, 
               aes(adap.suppression, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of AIDS Drug Assistance\nClients Virally Suppressed in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = scales::percent)


ggplot() + 
    geom_point(data=df.city.summ, 
               aes(oahs.suppression, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of OAHS\nClients Virally Suppressed in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = scales::percent)

ggplot() + 
    geom_point(data=df.city.summ, 
               aes(adap.clients, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of PWH\nWho Are ADAP Clients in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = scales::percent)

ggplot() + 
    geom_point(data=df.city.summ, 
               aes(oahs.clients, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Proportion of PWH\nWho Are OAHS Clients in 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = scales::percent)

ggplot() + 
    geom_point(data=df.city.summ, 
               aes(awareness, excess, size=new, fill=medicaid),
               shape=21, show.legend = F) + 
    ylab("Relative Excess HIV Infections") +
    xlab("Awareness 2025") + 
    THEME + CITY.SIZE.SCALE + CITY.COLOR +
    geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
    scale_y_continuous(labels = scales::percent, limits = c(0,1.5)) +
    scale_x_continuous(labels = scales::percent)
}

# total change in adap suppression
if (1==2)
{
    exp.locations = dimnames(df.city.summ)[[1]][df.city.summ$medicaid=='Expansion State']
    nonexp.locations = dimnames(df.city.summ)[[1]][df.city.summ$medicaid!='Expansion State']
    lose.adap.n.exp = total.results['2025',,'adap.suppression',exp.locations,'rw.end'] * all.parameters['lose.adap.expansion.effect',,exp.locations,'rw.end']
    lose.adap.n.nonexp = total.results['2025',,'adap.suppression',nonexp.locations,'rw.end'] * all.parameters['lose.adap.nonexpansion.effect',,nonexp.locations,'rw.end']
    
    adap.n.exp = total.results['2025',,'adap.clients',exp.locations,'rw.end']
    adap.n.nonexp = total.results['2025',,'adap.clients',nonexp.locations,'rw.end']
    
    supp.n.exp = total.results['2025',,'suppression',exp.locations,'rw.end']
    supp.n.nonexp = total.results['2025',,'suppression',nonexp.locations,'rw.end']
    
    adap.suppression.loss.exp = rowSums(lose.adap.n.exp) / rowSums(supp.n.exp)
    adap.suppression.loss.nonexp = rowSums(lose.adap.n.nonexp) / rowSums(supp.n.nonexp)
    
    mean(adap.suppression.loss.exp)
    mean(adap.suppression.loss.nonexp)
    
    
    prev.2025 = total.results['2025',,'diagnosed.prevalence',,'rw.end']
    prev.2026 = total.results['2026',,'diagnosed.prevalence',,'rw.end']
    unsuppressed.2025 = prev.2025 - total.results['2025',,'suppression',,'rw.end']
    unsuppressed.2026 = prev.2026 - total.results['2026',,'suppression',,'rw.end']
    
    delta.suppression = rowSums(unsuppressed.2025)/rowSums(prev.2025) - rowSums(unsuppressed.2026)/rowSums(prev.2026)
    delta.suppression.exp = rowSums(unsuppressed.2026[,exp.locations])/rowSums(prev.2026[,exp.locations]) / ( rowSums(unsuppressed.2025[,exp.locations])/rowSums(prev.2025[,exp.locations]) )
    delta.suppression.nonexp = rowSums(unsuppressed.2026[,nonexp.locations])/rowSums(prev.2026[,nonexp.locations]) / ( rowSums(unsuppressed.2025[,nonexp.locations])/rowSums(prev.2025[,nonexp.locations]) )
    
    mean(delta.suppression)
    mean(delta.suppression.exp)
    mean(delta.suppression.nonexp)
    
}