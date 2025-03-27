
library(ggplot2)
library(tibble)


CUMULATIVE.YEARS = as.character(2025:2030)
BASELINE.YEAR = as.character(2025)
PRE.BASELINE.YEAR = as.character(2016)


excess.incidence.end.vs.noint = (apply(total.incidence[CUMULATIVE.YEARS,,,'rw.end',drop=F] - total.incidence[CUMULATIVE.YEARS,,,'noint',drop=F], c('location','sim'), sum, na.rm=T)) / 
  apply(total.incidence[CUMULATIVE.YEARS,,,'noint',drop=F], c('location','sim'), sum, na.rm=T)
mean.excess.incidence.end.vs.noint = apply(excess.incidence.end.vs.noint, 'location', mean, na.rm=T)

delta.new.baseline = (total.new[BASELINE.YEAR,,,'noint'] - total.new[PRE.BASELINE.YEAR,,,'noint']) / total.new[PRE.BASELINE.YEAR,,,'noint']

total.suppression = apply(full.results[,,,,,,'suppression',,], c('year','sim','location','intervention'), sum, na.rm=T) /
  apply(full.results[,,,,,,'diagnosed.prevalence',,], c('year','sim','location','intervention'), sum, na.rm=T)

delta.suppression.baseline = (total.suppression[BASELINE.YEAR,,,'noint'] - total.suppression[PRE.BASELINE.YEAR,,,'noint']) / total.suppression[PRE.BASELINE.YEAR,,,'noint']


#parameter.means = apply(all.parameters[,,,'noint'], c('parameter','location'), mean, na.rm=T)



#-- The city-level scatterplots --#

PLOT.DIR = file.path('../../results/ryan_white/figure_city_variation/')
PLOT.HEIGHT = 3
PLOT.WIDTH = 3
PLOT.DPI = 600
PLOT.DEVICE = 'png'


baseline.total.results = apply(full.results[BASELINE.YEAR,,,,,,,,'noint'], c('sim','outcome','location'), sum, na.rm=T)
baseline.total.results[,'oahs.suppression',] = baseline.total.results[,'oahs.suppression',] /  baseline.total.results[,'oahs.clients',]
baseline.total.results[,'adap.suppression',] = baseline.total.results[,'adap.suppression',] /  baseline.total.results[,'adap.clients',]
for (outcome.to.div in c('suppression','rw.clients','non.adap.clients','oahs.clients','adap.clients'))
    baseline.total.results[,outcome.to.div,] = baseline.total.results[,outcome.to.div,] / baseline.total.results[,'diagnosed.prevalence',]
baseline.totals.by.city = apply(baseline.total.results, c('outcome','location'), mean, na.rm=T)


total.prevalence = apply(full.results[BASELINE.YEAR,,,,,,'diagnosed.prevalence',,'noint'], c('sim','location'), sum)

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

PALETTE = ggsci::pal_nejm()(10)
CITY.SIZE.SCALE = scale_size(range = c(1,4))
CITY.COLOR = scale_fill_manual(values = c("Non-Expansion State" = PALETTE[4],
                                "Expansion State" = PALETTE[7]))
THEME =  theme_bw(base_size = 6)
SEGMENT.SIZE = 0.25
LABEL.SIZE = 2

HOFFSET = -.1
VOFFSET = 0.1
df.label = df.city.summ[c(HOUSTON.MSA,BALTIMORE.MSA, RIVERSIDE.MSA),]
df.label$city.name = c("Houston, TX", "Baltimore, MD", "Riverside, CA")


# Ryan White Clients
df.label$x = df.label$rw.clients - 0.05
df.label$y = df.label$excess + 0.05
df.label$name = paste0(df.label$city.name, " ")

plot = ggplot(df.city.summ, aes(rw.clients, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of HIV+ Residents Receiving Ryan White Services in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=rw.clients, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_rw_clients.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# Total Suppression
df.label$x = df.label$suppression - 0.015
df.label$y = df.label$excess + 0.05
df.label$x[3] = df.label$suppression[3] + 0.015
df.label$y[3] = df.label$excess[3] - 0.03
df.label$name = paste0(df.label$city.name, " ")

plot = ggplot(df.city.summ, aes(suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of All HIV+ Residents Virally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.65,0.9)) +
  geom_text(data=df.label[-3,], 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F) +
  geom_text(data=df.label[3,], 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 1, hjust=0, show.legend = F); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# ADAP Suppression
df.label$x = df.label$adap.suppression - 0.015
df.label$y = df.label$excess + 0.03
df.label$name = paste0(df.label$city.name, " ")

plot = ggplot(df.city.summ, aes(adap.suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of AIDS Drug Assistance Clients Virally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=adap.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.72,0.95)) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_adap_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)




# OAHS Suppression
df.label$x = df.label$oahs.suppression - 0.005
df.label$y = df.label$excess + 0.05
df.label$name = paste0(df.label$city.name, " ")

plot = ggplot(df.city.summ, aes(oahs.suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylab("Relative Excess HIV Infections") +
  xlab("Proportion of Outpatient Ambulatory Clients Virally Suppressed in 2025") + 
  THEME + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=oahs.suppression, yend=excess), size=SEGMENT.SIZE, show.legend = F) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_continuous(labels = scales::percent, limits = c(0.84,0.94)) +
  geom_text(data=df.label, 
            aes(x, y, label=name), size=LABEL.SIZE,
            vjust = 0, hjust=1, show.legend = F); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, "excess_vs_oahs_suppression.png"),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)









df.label$x = df.label$suppression - 0.01
df.label$y = df.label$excess + VOFFSET
df.label$name = paste0(df.label$city.name, " ")

ggplot(df.city.summ, aes(suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + #xlim(0,1) +
  ylab("Relative Excess HIV Infections") +
  xlab("Baseline Viral Suppression") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=suppression, yend=excess), size=1, show.legend = F) + 
  geom_text(data=df.label, 
            aes(x, y, label=name), size=5,
            vjust = 0, hjust=1, show.legend = F)

# OAHS Suppression
df.label$x = df.label$oahs.suppression - .01
df.label$y = df.label$excess + VOFFSET
df.label$name = paste0(df.label$name, " ")
ggplot(df.city.summ, aes(oahs.suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + #xlim(0,1) +
  ylab("Relative Excess HIV Infections") +
  xlab("Baseline Viral Suppression If Receiving Outpatient Ambulatory Health Services") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR + 
  geom_segment(data = df.label, aes(x, y, xend=oahs.suppression, yend=excess), size=1, show.legend = F) + 
  geom_text(data=df.label, 
            aes(x, y, label=name), size=5,
            vjust = 0, hjust=1, show.legend = F)



# OTHERS

df.label$x = df.label$adap.clients + HOFFSET
df.label$y = df.label$excess + VOFFSET
df.label$name = paste0(df.label$name, " ")
ggplot(df.city.summ, aes(adap.clients, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  xlim(0,.5) + ylim(0,1) + ylab("Relative Excess HIV Infections") +
  xlab("Proportion of HIV+ Residents Receiving AIDS Drug Assistance") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=adap.clients, yend=excess), size=1, show.legend = F) + 
geom_text(data=df.label, 
          aes(x, y, label=name), size=5,
          vjust = 0, hjust=1, show.legend = F)

df.label$x = df.label$oahs.clients + HOFFSET
df.label$y = df.label$excess + VOFFSET
df.label$name = paste0(df.label$name, " ")
ggplot(df.city.summ, aes(oahs.clients, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  xlim(0,.6) + ylim(0,1) + ylab("Relative Excess HIV Infections") +
  xlab("Proportion of HIV+ Residents Receiving Outpatient Ambulatory Health Services") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR +
  geom_segment(data = df.label, aes(x, y, xend=oahs.clients, yend=excess), size=1, show.legend = F) + 
  geom_text(data=df.label, 
            aes(x, y, label=name), size=5,
            vjust = 0, hjust=1, show.legend = F)


ggplot(df.city.summ, aes(delta.suppression, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + #xlim(0,1) +
  ylab("Relative Excess HIV Infections") +
  xlab("Change in Baseline Viral Suppression from 2016 to 2025") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR



ggplot(df.city.summ, aes(new, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + #xlim(0,1) +
  ylab("Relative Excess HIV Infections") +
  xlab("New Diagnoses in 2025") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR

ggplot(df.city.summ, aes(delta.new, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + #xlim(0,1) +
  ylab("Relative Excess HIV Infections") +
  xlab("Change in New Diagnoses from 2016 to 2025") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR

ggplot(df.city.summ, aes(sexual.transmission, excess, size=new, fill=medicaid)) + geom_point(shape=21, show.legend = F) + 
  ylim(0,1) + xlim(0,0.7) +
  ylab("Relative Excess HIV Infections") +
  xlab("Average Sexual Transmission Rates") + 
  theme_bw() + CITY.SIZE.SCALE + CITY.COLOR

ranked.df.city.summ = as.data.frame(apply(df.city.summ, 2, order))
ranked.df.city.summ$medicaid = as.numeric(df.city.summ$medicaid=='Expansion State')
lm(excess ~ rw.clients + medicaid + suppression, data=ranked.df.city.summ)
fit = lm(excess ~ rw.clients + adap.clients + oahs.clients + suppression + oahs.suppression + adap.suppression + new + sexual.transmission, data=ranked.df.city.summ); fit$coefficients; summary(fit)$r.squared
fit = lm(excess ~ rw.clients + adap.clients + oahs.clients + suppression + oahs.suppression + adap.suppression + new + delta.new + sexual.transmission, data=ranked.df.city.summ); fit$coefficients; summary(fit)$r.squared
fit = lm(excess ~ rw.clients + adap.clients + oahs.clients + suppression + oahs.suppression + adap.suppression + new + delta.new + sexual.transmission + medicaid, data=df.city.summ); fit$coefficients; summary(fit)$r.squared

fit = lm(excess ~ rw.clients + adap.clients + oahs.clients + suppression + oahs.suppression + adap.suppression + new + delta.new + sexual.transmission + medicaid, data=df.city.summ); fit$coefficients; summary(fit)$r.squared

pcc.df = df.city.summ[,c('rw.clients','adap.clients','oahs.clients','suppression','oahs.suppression','adap.suppression','new','sexual.transmission','medicaid','excess')]
pcc.df = df.city.summ[,c('rw.clients','suppression','oahs.suppression','adap.suppression','new','sexual.transmission','medicaid','excess')]
prccs = epiR::epi.prcc(pcc.df); prccs[order(abs(prccs$est), decreasing = T),]

df.city.summ$numeric.medicaid = as.numeric(df.city.summ$medicaid=='Expansion State')
vars.of.interest = c('rw.clients','adap.clients','oahs.clients','suppression','oahs.suppression','adap.suppression','new','delta.new','sexual.transmission','numeric.medicaid')
pccs = sapply(vars.of.interest, function(var){
  
  other.vars = setdiff(vars.of.interest, var)
  fit1 = lm(as.formula(paste0("excess ~ ", paste0(other.vars, collapse='+'))), data=df.city.summ)
  fit2 = lm(as.formula(paste0(var, " ~ ", paste0(other.vars, collapse='+'))), data=df.city.summ)
  
  cor(fit1$residuals, fit2$residuals)
  
}); names(pccs) = vars.of.interest; pccs[order(abs(pccs), decreasing = T)]

