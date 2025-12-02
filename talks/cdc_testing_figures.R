
LOCATION = 'TX'

source('applications/cdc_testing/cdc_testing_main.R')
source('talks/figure_settings.R')
CDCT.PLOT.DIR = file.path(PLOT.DIR, 'cdct')

if (!dir.exists(CDCT.PLOT.DIR))
    dir.create(CDCT.PLOT.DIR)

VERSION = 'cdct'
SUB.VERSION = NULL
LOCATION = 'C.12580'
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
    simset.noint = simset.noint$thin(keep=100)
    
    simset.end = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'cdct.end')
    simset.end = simset.end$thin(keep=100)
    
    
    simset.bintr = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'cdct.bintr')
    simset.bintr = simset.bintr$thin(keep=100)
    
    
    simset.pintr = retrieve.simulation.set(VERSION, 
                                         LOCATION, 
                                         CALIBRATION.CODE, 
                                         n.sim = NSIM, 
                                         sub.version = SUB.VERSION,
                                         intervention.code = 'cdct.pintr')
    simset.pintr = simset.pintr$thin(keep=100)
    
}
if (!exists(total.results))
{
    source('applications/cdc_testing/for_manuscript/load_cdct_results.R')
}

CDCT.PALETTE.FULL = c(
    continuation = rgb(58/256,118/256,62/256),
    cessation = rgb(59/256,77/256,84/256),
    pintr = rgb(212/256,148/256,84/256),
    bintr = rgb(70/256,59/256,209/256)
)
CDCT.DATA.COLOR = ggsci::pal_jama()(7)[1]

CDCT.CALIB.YEARS = 2010:2025
CDCT.CALIB.HEIGHT = FOUR.PANEL.HEIGHT
CDCT.CALIB.WIDTH = 4
#CDCT.CALIB.SCALE = scale_x_continuous(breaks = c(2015, 2018, 2021, 2024))

CDCT.YEARS = 2015:2035


CDCT.STYLE.MANAGER.1 = create.style.manager(color.sim.by = 'simset',
                                          linetype.sim.by = 'stratum',
                                          linewidth.baseline = 0.25,
                                          alpha.line = 0.1,
                                          sim.palette = CDCT.PALETTE.FULL['continuation'],
                                          data.palette = CDCT.DATA.COLOR)

CDCT.STYLE.MANAGER.PROJ = create.style.manager(color.sim.by = 'simset',
                                          linetype.sim.by = 'stratum',
                                          linewidth.baseline = 5,
                                          sim.palette = CDCT.PALETTE.FULL[c('cessation','continuation')],
                                          data.palette = CDCT.DATA.COLOR)

# for simsets
SIMSET.COLORS = c(
    simset.noint = as.character(CDCT.PALETTE.FULL['continuation']),
    simset.end = as.character(CDCT.PALETTE.FULL['cessation']),
    simset.bintr = as.character(CDCT.PALETTE.FULL['bintr']),
    simset.pintr = as.character(CDCT.PALETTE.FULL['pintr'])
)

SIMSET.NAMES = c(
    simset.noint = 'Continuation',
    simset.end = "Cessation",
    simset.bintr = "Brief Interruption",
    simset.pintr = "Prolonged Interruption"
)

#-- CALIBRATION --#

plot = simplot(simset.noint,
               'cdc.funded.tests', dimension.values = list(year=CDCT.CALIB.YEARS),
               style.manager = CDCT.STYLE.MANAGER.1) + theme_bw() + THEME.3 +
    ylab("Tests (n)") + ggtitle("Number of CDC-Funded HIV Tests"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'tests.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               'total.cdc.hiv.test.positivity', dimension.values = list(year=CDCT.CALIB.YEARS),
               style.manager = CDCT.STYLE.MANAGER.1) + theme_bw() + THEME.3 +
    ylab("Positivity Rate (%)") + ggtitle("Positivity, CDC-Funded HIV Tests"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'positivity.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- PROJECTION --#

PROJ.YUPPER = 6500

plot = simplot(simset.noint,
               simset.end,
               summary.type = 'mean.and.interval',
               'incidence', dimension.values = list(year=CDCT.YEARS),
               style.manager = CDCT.STYLE.MANAGER.PROJ) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'cdct_total_inc_vs_end.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.bintr,
               summary.type = 'mean.and.interval',
               'incidence', dimension.values = list(year=CDCT.YEARS),
               style.manager = CDCT.STYLE.MANAGER.PROJ) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    guides(linetype='none') +
    ylab("Infections (n)") + ggtitle("15-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'cdct_total_inc_vs_bintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset.noint,
               simset.pintr,
               summary.type = 'mean.and.interval',
               'incidence', dimension.values = list(year=CDCT.YEARS),
               style.manager = CDCT.STYLE.MANAGER.PROJ) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    ylab("Infections (n)") + ggtitle("39-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'cdct_total_inc_vs_pintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- PROJECTION - TOTAL --#

cdct.total.incidence = apply(total.results[as.character(CDCT.YEARS),,'incidence',CDC.TESTING.LOCATIONS,],
                             c('year','sim','intervention'), sum)

cdct.total.inc.df = reshape2::melt(apply(cdct.total.incidence, c('year','intervention'), mean))
cdct.total.inc.df$intervention = as.character(cdct.total.inc.df$intervention)
cdct.total.inc.df$intervention = paste0("simset.",cdct.total.inc.df$intervention)
cdct.total.inc.df$intervention = gsub("(cdct.)","",cdct.total.inc.df$intervention)
cdct.total.inc.df$intervention = gsub("(\\.26)","",cdct.total.inc.df$intervention)
cdct.total.inc.df$lower = as.numeric(apply(cdct.total.incidence,c('year','intervention'), quantile, probs=.025, na.rm=T))
cdct.total.inc.df$upper = as.numeric(apply(cdct.total.incidence,c('year','intervention'), quantile, probs=.975, na.rm=T))

plot = ggplot(cdct.total.inc.df[cdct.total.inc.df$intervention=='simset.noint' | cdct.total.inc.df$intervention=='simset.end',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,NA)) +
    ylab("Infections (n)") + ggtitle("Cessation"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'aggregated_cdct_total_inc_vs_end.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(cdct.total.inc.df[cdct.total.inc.df$intervention=='simset.noint' | cdct.total.inc.df$intervention=='simset.bintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,NA)) +
    ylab("Infections (n)") + ggtitle("15-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'aggregated_cdct_total_inc_vs_bintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = ggplot(cdct.total.inc.df[cdct.total.inc.df$intervention=='simset.noint' | cdct.total.inc.df$intervention=='simset.pintr',],
              aes(x=year, y=value, ymin=lower, ymax=upper, fill=intervention)) +
    geom_ribbon(alpha=0.2, size=.2) +
    geom_line(aes(color=intervention), size=1) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    guides(linetype='none') +
    scale_y_continuous(labels = function(y){format(y, big.mark=',')}, limits = c(0,NA)) +
    ylab("Infections (n)") + ggtitle("39-Month Interruption"); plot

ggsave(plot = plot, 
       filename = file.path(CDCT.PLOT.DIR, 'aggregated_cdct_total_inc_vs_pintr.png'),
       height = THREE.PANEL.HEIGHT, width = THREE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)
