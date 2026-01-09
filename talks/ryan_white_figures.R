
source('applications/ryan_white/ryan_white_main.R')
source('talks/figure_settings.R')
RW.PLOT.DIR = file.path(PLOT.DIR, 'rw')

if (!dir.exists(RW.PLOT.DIR))
    dir.create(RW.PLOT.DIR)

VERSION = 'rw'
SUB.VERSION = 'w'
LOCATION = 'C.12580'
NSIM = 80

if (locations::get.location.type(LOCATION)=='STATE')
    CALIBRATION.CODE = 'final.ehe.state'

if (locations::get.location.type(LOCATION)!='STATE')
    CALIBRATION.CODE = 'final.ehe'


if (!exists('simset.end'))
{
    simset.noint = retrieve.simulation.set(VERSION, 
                                           LOCATION, 
                                           CALIBRATION.CODE, 
                                           n.sim = NSIM, 
                                           sub.version = SUB.VERSION,
                                           intervention.code = 'noint')
    
    simset.end = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'rw.end')
    
    simset.bintr = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'rw.b.intr')
    
    simset.pintr = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'rw.p.intr')
}

if (!exists(total.results))
{
    load("Q:results/ryan_white/ryan_white_results_city_2025-07-01.Rdata")
    city.totals = total.results
    
    load("Q:results/ryan_white/ryan_white_results_state_2025-09-09.Rdata")
    state.totals = total.results
    
    load("Q:results/ryan_white/ryan_white_results_state_nanb_2026_2025-10-14.Rdata")
    nanb.totals = total.results
}


RW.CALIB.YEARS = 2015:2024
RW.CALIB.HEIGHT = FOUR.PANEL.HEIGHT
RW.CALIB.WIDTH = 4
RW.CALIB.SCALE = scale_x_continuous(breaks = c(2015, 2018, 2021, 2024))

RW.YEARS = 2015:2035

RW.STYLE.MANAGER.1 = create.style.manager(color.sim.by = 'simset',
                                          linetype.sim.by = 'stratum',
                                          linewidth.baseline = 0.25,
                                          alpha.line = 0.1,
                                          sim.palette = RW.BASELINE.COLOR,
                                          data.palette = RW.DATA.COLOR)

RW.STYLE.MANAGER.2 = create.style.manager(color.sim.by = 'stratum',
                                        linetype.sim.by = 'simset',
                                        linewidth.baseline = 0.25,
                                        alpha.line = 0.1,
                                        sim.palette = PALETTE.2)


RW.STYLE.MANAGER.3 = create.style.manager(color.sim.by = 'simset',
                                          linetype.sim.by = 'stratum',
                                          linewidth.baseline = 5,
                                          #alpha.line = 0.1,
                                          sim.palette = RW.BASELINE.COLOR,
                                          data.palette = RW.DATA.COLOR)

SIMSET.COLORS = c(
    simset.noint = RW.BASELINE.COLOR,
    simset.end = RW.END.COLOR,
    simset.bintr = RW.B.INTR.COLOR,
    simset.pintr = RW.P.INTR.COLOR,
    simset.end.cons =  rgb(59/256,77/256,84/256),
    simset.intr = RW.P.INTR.COLOR
)

SIMSET.NAMES = c(
    simset.noint = 'Continuation',
    simset.end = "Cessation",
    simset.bintr = "Brief Interruption",
    simset.pintr = "Prolonged Interruption",
    simset.end.cons = "Cessation (Conservative)",
    simset.intr = "Interruption"
)



#-- CALIBRATION --#

plot = simplot(simset.noint,
               data.manager = RW.DATA.MANAGER,
               'non.adap.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a +
    RW.CALIB.SCALE +
    ylab("Clients (n)") + ggtitle("Non-ADAP Clients"); plot


ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients.png'),
       height = FOUR.PANEL.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               data.manager = RW.DATA.MANAGER,
               'oahs.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a +
    RW.CALIB.SCALE +
    ylab("Clients (n)") + ggtitle("Outpatient Health Services Clients"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_clients.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               data.manager = RW.DATA.MANAGER,
               'adap.proportion', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a +
    RW.CALIB.SCALE +
    ylab("Proportion (%)") + ggtitle("Ryan White Clients Receiving ADAP"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'adap_proportion.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               data.manager = RW.DATA.MANAGER,
               'oahs.suppression', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a +
    RW.CALIB.SCALE +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression among\nOutpatient Health Services Clients"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_suppression.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               data.manager = RW.DATA.MANAGER,
               'adap.suppression', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a +
    RW.CALIB.SCALE +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression among\nADAP Clients"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'adap_suppression.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- CALIBRATION by STRATA --#

# OAHS Clients
plot = simplot(simset.noint,
               split.by='age',
               label.function = AGE.LABEL.FUNCTION,
               data.manager = RW.DATA.MANAGER,
               'oahs.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    RW.CALIB.SCALE +
    ylab("Clients (n)") + ggtitle("Outpatient Health Services Clients\nby Age"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_clients_by_age.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               split.by='race',
               data.manager = RW.DATA.MANAGER,
               'oahs.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    ylab("Clients (n)") + ggtitle("Outpatient Health Services Clients\nby Race"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_clients_by_race.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

# OAHS Suppression
plot = simplot(simset.noint,
               split.by='age',
               label.function = AGE.LABEL.FUNCTION,
               data.manager = RW.DATA.MANAGER,
               'oahs.suppression', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    RW.CALIB.SCALE +
    ylab("Clients (n)") + ggtitle("Suppression Among Outpatient\nHealth Services Clients by Age"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_suppression_by_age.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               split.by='race',
               data.manager = RW.DATA.MANAGER,
               'oahs.suppression', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    ylab("Clients (n)") + ggtitle("Suppression Among Outpatient\nHealth Services Clientsby Race"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'oahs_suppression_by_race.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

# Non-ADAP Clients
plot = simplot(simset.noint,
               split.by='age',
               label.function = AGE.LABEL.FUNCTION,
               data.manager = RW.DATA.MANAGER,
               'non.adap.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    RW.CALIB.SCALE +
    ylab("Clients (n)") + ggtitle("Non-ADAP Clients by Age"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients_by_age.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               split.by='race',
               data.manager = RW.DATA.MANAGER,
               'non.adap.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    ylab("Clients (n)") + ggtitle("Non-ADAP Clients by Race"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients_by_race.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               split.by='sex',
               data.manager = RW.DATA.MANAGER,
               dimension.values.post.mapping = list(sex=c('male','female')),
               'non.adap.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    ylab("Clients (n)") + ggtitle("Non-ADAP Clients by Sex"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients_by_sex.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               split.by='risk', 
               data.manager = RW.DATA.MANAGER,
               'non.adap.clients', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    ylab("Clients (n)") + ggtitle("Non-ADAP Clients by Risk Factor"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients_by_sex.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

# ADAP Ratio

plot = simplot(simset.noint,
               split.by='age',
               label.function = AGE.LABEL.FUNCTION,
               data.manager = RW.DATA.MANAGER,
               'adap.proportion', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    RW.CALIB.SCALE +
    ylab("Proportion (%)") + ggtitle("Ryan White Clients Receiving ADAP"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'adap_ratio_clients_by_age.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               split.by='race',
               label.function = AGE.LABEL.FUNCTION,
               data.manager = RW.DATA.MANAGER,
               'adap.proportion', dimension.values = list(year=RW.CALIB.YEARS),
               style.manager = RW.STYLE.MANAGER.2) + theme_bw() + THEME.2b +
    guides(shape = "none",
           linetype = "none",
           fill = "none") +
    RW.CALIB.SCALE +
    ylab("Proportion (%)") + ggtitle("Ryan White Clients Receiving ADAP"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'non_adap_clients_by_race.png'),
       height = RW.CALIB.HEIGHT, width = RW.CALIB.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


#-- PROJECTIONS --#

PROJ.YUPPER = 1000

# Indiv sims
plot = simplot(simset.noint,
               simset.end,
               'incidence', dimension.values = list(year=RW.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_end_individual_sims.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.pintr,
               'incidence', dimension.values = list(year=RW.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("42-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_pintr_individual_sims.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.bintr,
               'incidence', dimension.values = list(year=RW.YEARS),
               style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("18-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_bintr_individual_sims.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

# Line and Ribbon

plot = simplot(simset.noint,
               simset.end,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_end.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.pintr,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("42-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_pintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.bintr,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("18-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_bintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)



#-- PROJECTION - TOTAL --#

PROJ.YUPPER = 65000

city.total.incidence = apply(city.totals[as.character(RW.YEARS),,'incidence',,],
                             c('year','sim','intervention'), sum)

city.totals.df = reshape2::melt(apply(city.total.incidence, c('year','intervention'), mean))
city.totals.df$intervention = as.character(city.totals.df$intervention)
city.totals.df$intervention = paste0("simset.",city.totals.df$intervention)
city.totals.df$intervention = gsub("(rw.)","",city.totals.df$intervention)
city.totals.df$intervention = gsub("b\\.intr","bintr",city.totals.df$intervention)
city.totals.df$intervention = gsub("p\\.intr","pintr",city.totals.df$intervention)
city.totals.df$lower = as.numeric(apply(city.total.incidence,c('year','intervention'), quantile, probs=.025, na.rm=T))
city.totals.df$upper = as.numeric(apply(city.total.incidence,c('year','intervention'), quantile, probs=.975, na.rm=T))

replace.into.mask = city.totals.df$intervention=='simset.end.cons' & city.totals.df$year=='2024'
replace.from.mask = city.totals.df$intervention=='simset.end' & city.totals.df$year=='2024'
city.totals.df$value[replace.into.mask] = city.totals.df$value[replace.from.mask]
city.totals.df$lower[replace.into.mask] = city.totals.df$lower[replace.from.mask]
city.totals.df$upper[replace.into.mask] = city.totals.df$upper[replace.from.mask]

plot = ggplot(city.totals.df[city.totals.df$intervention=='simset.noint' | city.totals.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_city_total_inc_vs_end.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(city.totals.df[city.totals.df$intervention=='simset.noint' | city.totals.df$intervention=='simset.bintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("18-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_city_total_inc_vs_bintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(city.totals.df[city.totals.df$intervention=='simset.noint' | city.totals.df$intervention=='simset.pintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("42-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_city_total_inc_vs_pintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


#-- For comparisons to conservative analysis --#


plot = ggplot(city.totals.df[city.totals.df$intervention=='simset.noint' | city.totals.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("31 Cities - Survey-Based"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'orig_aggregated_rw_city_total_inc_vs_end.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = ggplot(city.totals.df[city.totals.df$intervention=='simset.noint' | city.totals.df$intervention=='simset.end.cons',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("31 Cities - Conservative"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'cons_aggregated_rw_city_total_inc_vs_end.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)



state.total.incidence = apply(state.totals[as.character(RW.YEARS),,'incidence',RW.11.STATES,],
                             c('year','sim','intervention'), sum)

state.totals.df = reshape2::melt(apply(state.total.incidence, c('year','intervention'), mean))
state.totals.df$intervention = as.character(state.totals.df$intervention)
state.totals.df$intervention = paste0("simset.",state.totals.df$intervention)
state.totals.df$intervention = gsub("(rw.)","",state.totals.df$intervention)
state.totals.df$intervention = gsub("b\\.intr","bintr",state.totals.df$intervention)
state.totals.df$intervention = gsub("p\\.intr","pintr",state.totals.df$intervention)
state.totals.df$intervention = gsub("\\.26$","",state.totals.df$intervention)
state.totals.df$lower = as.numeric(apply(state.total.incidence,c('year','intervention'), quantile, probs=.025, na.rm=T))
state.totals.df$upper = as.numeric(apply(state.total.incidence,c('year','intervention'), quantile, probs=.975, na.rm=T))


PROJ.YUPPER = 45000

# For stand-alone


plot = ggplot(state.totals.df[state.totals.df$intervention=='simset.noint' | state.totals.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_state_total_inc_vs_end.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(state.totals.df[state.totals.df$intervention=='simset.noint' | state.totals.df$intervention=='simset.bintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("18-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_state_total_inc_vs_bintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(state.totals.df[state.totals.df$intervention=='simset.noint' | state.totals.df$intervention=='simset.pintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("42-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_state_total_inc_vs_pintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

# For comparison to conservative

plot = ggplot(state.totals.df[state.totals.df$intervention=='simset.noint' | state.totals.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("11 States - Survey-Based"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'orig_aggregated_rw_state_total_inc_vs_end.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)



plot = ggplot(state.totals.df[state.totals.df$intervention=='simset.noint' | state.totals.df$intervention=='simset.end.cons',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("11 States - Conservative"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'cons_aggregated_rw_state_total_inc_vs_end.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- State Fraction of Costs --#

cost.locations = RW.STATES
rw.total.budget = state.rw.costs[,'total']
names(rw.total.buget) = rowNames(state.rw.costs)
RW.COSTS.DF = data.frame(
    state = get.location.name(cost.locations),
    budget = rw.total.budget[cost.locations],
    fraction.nanb = state.rw.fraction.c.d.ehe.minority.of.non.adap[cost.locations]
)
RW.COSTS.DF$ehe = "nonehe"
RW.COSTS.DF$ehe[sapply(cost.locations, function(loc){any(loc==c('AL','SC','KY','MO','AR','OK','MS'))})] = "ehe"
RW.COSTS.DF$state = factor(RW.COSTS.DF$state, levels = RW.COSTS.DF$state[order(RW.COSTS.DF$fraction.nanb)])

#ggplot(RW.COSTS.DF, aes(x=budget, y=fraction.nanb)) + geom_point()


plot = ggplot(RW.COSTS.DF, aes(fraction.nanb, state, size=budget, fill=ehe)) + geom_point(shape=22) + 
    scale_x_continuous("Fraction of Non-ADAP Budget\nfrom Parts C/D, EHE, MAI", labels = scales::percent, limits=c(0,.6)) +
    scale_size_continuous(labels = function(x){paste0("$", round(x/1000000), "M")}) +
    scale_fill_manual(
        values = c(ehe=RW.NONEXP.COLOR, nonehe=RW.EXP.COLOR),
        labels = c(ehe='EHE Priority State', nonehe = 'Non-Priority State')
    ) +
    ylab(NULL) + 
    theme_bw() + THEME.4; plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_costs.png'),
       height = SINGLE.PANEL.HEIGHT, width = SINGLE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


#-- NANB --#
nanb.total.incidence = apply(nanb.totals[as.character(RW.YEARS),,'incidence',,],
                              c('year','sim','intervention'), sum)

nanb.totals.df = reshape2::melt(apply(nanb.total.incidence, c('year','intervention'), mean))
nanb.totals.df$intervention = as.character(nanb.totals.df$intervention)
nanb.totals.df$intervention = paste0("simset.",nanb.totals.df$intervention)
nanb.totals.df$intervention = gsub("(rw.)","",nanb.totals.df$intervention)
nanb.totals.df$intervention = gsub("(nanb.)","",nanb.totals.df$intervention)
nanb.totals.df$intervention = gsub("b\\.intr","intr",nanb.totals.df$intervention)
nanb.totals.df$intervention = gsub("\\.26$","",nanb.totals.df$intervention)
nanb.totals.df$lower = as.numeric(apply(nanb.total.incidence,c('year','intervention'), quantile, probs=.025, na.rm=T))
nanb.totals.df$upper = as.numeric(apply(nanb.total.incidence,c('year','intervention'), quantile, probs=.975, na.rm=T))

omit.mask = nanb.totals.df$intervention != 'simset.noint' & nanb.totals.df$year==2024
nanb.totals.df$value[omit.mask] = nanb.totals.df$lower[omit.mask] = nanb.totals.df$upper[omit.mask] = NA

PROJ.YUPPER = 40000

plot = ggplot(nanb.totals.df[nanb.totals.df$intervention=='simset.noint' | nanb.totals.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_nanb_total_inc_vs_end.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(nanb.totals.df[nanb.totals.df$intervention=='simset.noint' | nanb.totals.df$intervention=='simset.intr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("3-Year Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_nanb_total_inc_vs_intr.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)
