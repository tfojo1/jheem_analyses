
source('applications/ryan_white/ryan_white_main.R')
source('talks/croi_2026/figure_settings_croi.R')
RW.PLOT.DIR = file.path(PLOT.DIR, 'rw')

if (!dir.exists(RW.PLOT.DIR))
    dir.create(RW.PLOT.DIR)

VERSION = 'rw'
SUB.VERSION = NULL
LOCATION = 'NY'
NSIM = 1000

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
                                         intervention.code = 'rw.end.26')
    
    simset.pintr = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'rw.p.intr.26')
}

if (!exists("total.results"))
{
    load("Q:results/ryan_white/ryan_white_results_city_2025-07-01.Rdata")
    city.totals = total.results
    
    #load("Q:results/ryan_white/ryan_white_results_state_2025-09-09.Rdata") # this is the old timeframe
    load("Q:results/ryan_white/ryan_white_results_state_2026_2025-09-11.Rdata")
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
    simset.pintr = RW.P.INTR.COLOR,
    simset.end.cons =  rgb(59/256,77/256,84/256),
    simset.intr = RW.P.INTR.COLOR
)

SIMSET.NAMES = c(
    simset.noint = 'Continuation',
    simset.end = "Cessation",
    simset.pintr = "2.5-year Interruption",
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





#-- PROJECTIONS: SINGLE LOCATION --#

PROJ.YUPPER = 6000

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
    ylab("Infections (n)") + ggtitle("2.5-year Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_pintr_individual_sims.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


# Line and Ribbon
plot = simplot(simset.noint,
               #simset.end,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_continuation.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH,
       dpi = PLOT.DPI, device = PLOT.DEVICE)


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
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH,
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset.noint,
               simset.pintr,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("2.5-year Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'rw_total_inc_vs_pintr.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH,
       dpi = PLOT.DPI, device = PLOT.DEVICE)






#-- PROJECTION: ALL LOCATIONS --#
PROJ.YUPPER = 80000

state.total.incidence = apply(state.totals[as.character(RW.YEARS),,'incidence',,],
                             c('year','sim','intervention'), sum)

state.totals.df = reshape2::melt(apply(state.total.incidence, c('year','intervention'), mean))
state.totals.df$intervention = as.character(state.totals.df$intervention)
state.totals.df$intervention = paste0("simset.",state.totals.df$intervention)
state.totals.df$intervention = gsub("(rw.)","",state.totals.df$intervention)
state.totals.df$intervention = gsub("\\.26$","",state.totals.df$intervention) # MS added this to fix colors below 
state.totals.df$intervention = gsub("b\\.intr","bintr",state.totals.df$intervention)
state.totals.df$intervention = gsub("p\\.intr","pintr",state.totals.df$intervention)
state.totals.df$lower = as.numeric(apply(state.total.incidence,c('year','intervention'), quantile, probs=.025, na.rm=T))
state.totals.df$upper = as.numeric(apply(state.total.incidence,c('year','intervention'), quantile, probs=.975, na.rm=T))

# not sure what this is doing
replace.into.mask = state.totals.df$intervention=='simset.end.cons' & state.totals.df$year=='2024'
replace.from.mask = state.totals.df$intervention=='simset.end' & state.totals.df$year=='2024'
state.totals.df$value[replace.into.mask] = state.totals.df$value[replace.from.mask]
state.totals.df$lower[replace.into.mask] = state.totals.df$lower[replace.from.mask]
state.totals.df$upper[replace.into.mask] = state.totals.df$upper[replace.from.mask]

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
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
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
    ylab("Infections (n)") + ggtitle("2.5-year Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(RW.PLOT.DIR, 'aggregated_rw_state_total_inc_vs_pintr.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)





