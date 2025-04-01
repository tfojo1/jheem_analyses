
source('commoncode/locations_of_interest.R')
source('applications/ryan_white/ryan_white_main.R')
library(ggplot2)

loc = HOUSTON.MSA
simset.baseline = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM)
simset.noint = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'noint')
simset.end = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.end')
simset.b.intr = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.b.intr')
simset.p.intr = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.p.intr')

PLOT.YEARS = 2010:2030

PLOT.DIR = file.path('../../results/ryan_white/figure_city_projections/', loc)
PLOT.HEIGHT = 2
PLOT.WIDTH = 3
PLOT.DPI = 600
PLOT.DEVICE = 'png'

PLOT.STYLE.MANAGER = create.style.manager(
  color.sim.by = 'simset',
  linetype.sim.by = 'stratum',
  sim.palette = RW.PALETTE[-1],
  data.palette = RW.PALETTE[1]
)

PLOT.THEME = theme_bw(base_size = 6)
PLOT.GUIDES = guides(fill = "none", color = "none", linetype = "none", shape = "none")
REMOVE.FACET.STRIP = theme(strip.text = element_blank())
SCALE.SIZE = scale_size_manual(values=c(size=1))

if (!dir.exists(PLOT.DIR))
    dir.create(PLOT.DIR)

plot = simplot(simset.baseline, c('non.adap.clients'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Clients (n)") + xlab("Year") +
  ggtitle(NULL); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_non.adap.clients.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.clients'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Clients (n)") + xlab("Year") +
  ggtitle(NULL); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_oahs.clients.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('adap.proportion'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Ratio (AIDS Drug Assistance/\nNon-AIDS Drug Assistance)") + xlab("Year") +
  ggtitle(NULL); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_adap.ratio.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.suppression'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Proportion Suppressed (%)") + xlab("Year") +
  ggtitle(NULL); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_oahs.suppression.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('adap.suppression'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Proportion Suppressed (%)") + xlab("Year") +
  ggtitle(NULL); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_adap.suppression.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.baseline, c('non.adap.clients'), split.by='race', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + 
  ylab("Clients (n)") + xlab("Year") +
  ggtitle(NULL); plot





plot = simplot(simset.baseline, c('non.adap.clients','oahs.clients','adap.proportion'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc))); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_n.rw.total.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.suppression','adap.suppression'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc))); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "rw.supp.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('non.adap.clients'), facet.by='age', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc))); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_n.non.adap.age")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('non.adap.clients'), facet.by='race', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc))); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_n.non.adap.race")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('non.adap.clients'), facet.by='sex', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc))); plot
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_n.non.adap.sex")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.suppression'), facet.by='race', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc)))
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_oahs.supp.race")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.suppression'), facet.by='age', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc)))
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_oahs.supp.age")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.baseline, c('oahs.suppression'), facet.by='sex', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Calibration: ", get.location.name(loc)))
ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_oahs.supp.sex")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


PLOT.YEARS.INT = 2015:2035
max.inc = max(simset.end$get('incidence'))
ceiling.power = 100
#y.max.inc = ceiling(max.inc/(10^floor(log10(max.inc)))) * 10^floor(log10(max.inc))
y.max.inc = ceiling(max.inc/ceiling.power) * ceiling.power
max.new = max(simset.end$get('new'))
y.max.new = ceiling(max.new/ceiling.power) * ceiling.power


PLOT.DATA.MANAGER = create.data.manager('Slim.Data', "slimmed down data for plots")
PLOT.DATA.MANAGER$register.parent.source('cdc', "CDC", 'cdc')
PLOT.DATA.MANAGER$register.source('cdc', parent.source = 'cdc', full.name='CDC', short.name='cdc')
PLOT.DATA.MANAGER$register.ontology('cdc', SURVEILLANCE.MANAGER$ontologies$cdc)
PLOT.DATA.MANAGER$register.outcome('diagnoses',
                                   metadata = SURVEILLANCE.MANAGER$outcome.info$diagnoses$metadata)
PLOT.DATA.MANAGER$put(data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location,
                      outcome = 'diagnoses',
                      source = 'cdc', 
                      ontology.name = 'cdc',
                      url = 'dummy.org',
                      details = 'details')
PLOT.DATA.MANAGER$put(data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location,
                      outcome = 'diagnoses',
                      source = 'cdc', 
                      ontology.name = 'cdc',
                      url = 'dummy.org',
                      details = 'details')


# CESSATION: INCIDENCE
end.style.manager = create.style.manager(
  color.sim.by = 'simset',
  linetype.sim.by = 'stratum',
  sim.palette = c(RW.BASELINE.COLOR, RW.BASELINE.COLOR),
  data.palette = RW.DATA.COLOR
)

plot = simplot(simset.noint, simset.end, c('incidence'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval',
               data.manager=PLOT.DATA.MANAGER, style.manager = end.style.manager) +
  ylim(0, y.max.inc) + ylab("Infections") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_inc_cessation.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# CESSATION: NEW
plot = simplot(simset.noint, simset.end, c('new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval', 
               data.manager = PLOT.DATA.MANAGER, style.manager = PLOT.STYLE.MANAGER) +
  ylim(0, y.max.new) + ylab("Diagnoses") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_new_cessation.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)


# BRIEF INTR: Incidence
plot = simplot(simset.noint, simset.b.intr, c('incidence'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval', data.manager=PLOT.DATA.MANAGER) +
  ylim(0, y.max.inc) + ylab("Infections") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_inc_b.intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# BRIEF INTR: NEW
plot = simplot(simset.noint, simset.b.intr, c('new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval', data.manager=PLOT.DATA.MANAGER) +
  ylim(0, y.max.new) + ylab("Diagnoses") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_new_b.intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)



# PROLONGED INTR: Incidence
plot = simplot(simset.noint, simset.p.intr, c('incidence'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval', data.manager=PLOT.DATA.MANAGER) +
  ylim(0, y.max.inc) + ylab("Infections") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_inc_p.intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

# BRIEF INTR: NEW
plot = simplot(simset.noint, simset.p.intr, c('new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval', data.manager=PLOT.DATA.MANAGER) +
  ylim(0, y.max.new) + ylab("Diagnoses") + xlab("Year") +
  PLOT.THEME + PLOT.GUIDES + REMOVE.FACET.STRIP + SCALE.SIZE + ggtitle(NULL) + 
  geom_vline(xintercept = 2024.5, linetype='dotted'); plot

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_new_p.intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)











simplot(simset.noint, simset.b.intr, c('incidence','new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval') +
  ylim(0, y.max.inc) + PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Brief Interruption: ", get.location.name(loc))) + 
  geom_vline(xintercept = c(2024, 2028), linetype='dashed')

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_brief_intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)

simplot(simset.noint, simset.p.intr, c('incidence','new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval') +
  ylim(0, y.max.inc) + PLOT.THEME + PLOT.GUIDES +
  ggtitle(paste0("Cessation: ", get.location.name(loc))) + 
  geom_vline(xintercept = c(2024, 2028), linetype='dashed')

ggsave(plot = plot, 
       filename=file.path(PLOT.DIR, paste0(loc, "_prolonged_intr.png")),
       height = PLOT.HEIGHT, width = PLOT.WIDTH, dpi = PLOT.DPI, device = PLOT.DEVICE)
