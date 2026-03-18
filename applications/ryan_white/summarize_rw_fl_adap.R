
FL.PLOT.DIR = '../../results/ryan_whit_fl_adap'

INTERVENTION.CODE = 'rw.bik.end.26' # 'fl.adap.end.26', 'fl.adap.end.cons.26'
INTERVENTION.DESCRIPTION =  "Lose Biktarvy Access" # "50% Cut to ADAP"
PLOT.TITLE = "Lose Biktarvy Access"  # "Cessation"
PLOT.FILE.NAME = 'fl_no_biktarvy' #' fl_adap_cut'

simset.end = retrieve.simulation.set('rw','FL','final.ehe.state',1000,INTERVENTION.CODE)

simset.noint = retrieve.simulation.set('rw','FL','final.ehe.state',1000,'noint')


delta = colSums(simset.end$get('incidence', year=2026:2030) -
    simset.noint$get('incidence', year=2026:2030))

rel.delta = delta / colSums(simset.noint$get('incidence', year=2026:2030))

paste0(
    format(round(mean(delta)), big.mark=','),
    " [",
    format(round(quantile(delta, 0.025)), big.mark=','),
    " to ",
    format(round(quantile(delta, 0.975)), big.mark=','),
    "]"
)

paste0(
    round(100*mean(rel.delta)),
    "% [",
    round(100*quantile(rel.delta, 0.025)),
    " to ",
    round(100*quantile(rel.delta, 0.975)),
    "%]"
)


RW.YEARS = 2015:2035


PLOT.DEVICE = 'png'
PLOT.DPI = 600
TWO.PANEL.WIDTH = 6
TWO.PANEL.HEIGHT = 4


THEME.4 = theme(text = element_text(size = 14),
                legend.position = 'bottom',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.title = element_text(hjust=0.5),
                legend.title = element_blank(),
                axis.title.x = element_blank(),
                legend.background = element_blank(),
                legend.margin = margin(t=-10,b=-5,r=0,l=0)
)

RW.STYLE.MANAGER.3 = create.style.manager(color.sim.by = 'simset',
                                          linetype.sim.by = 'stratum',
                                          linewidth.baseline = 5,
                                          #alpha.line = 0.1,
                                          sim.palette = RW.BASELINE.COLOR,
                                          data.palette = RW.DATA.COLOR)

SIMSET.COLORS = c(
    simset.noint = RW.BASELINE.COLOR,
    simset.end = RW.END.COLOR
)

SIMSET.NAMES = c(
    simset.noint = 'Continuation',
    simset.end = INTERVENTION.DESCRIPTION
)

PROJ.YUPPER = NA

plot = simplot(simset.noint,
               simset.end,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    scale_linetype_discrete(guide='none') +
    ylab("Infections (n)") + ggtitle(PLOT.TITLE); plot

ggsave(plot = plot, 
       filename = file.path(FL.PLOT.DIR, paste0(PLOT.FILE.NAME,'.png')),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH,
       dpi = PLOT.DPI, device = PLOT.DEVICE)

means.df = data.frame(
    year = 2025:2030,
    simset.noint = simset.noint$get('incidence', year=2025:2030, summary.type = 'mean.and.interval')['mean',,],
    simset.end = simset.end$get('incidence', year=2025:2030, summary.type = 'mean.and.interval')['mean',,]
)

plot = simplot(simset.noint,
               simset.end,
               'incidence', dimension.values = list(year=RW.YEARS),
               summary.type = 'mean.and.interval',
               style.manager = RW.STYLE.MANAGER.3) + 
    geom_ribbon(data = means.df, aes(x=year, ymin=simset.noint, ymax=simset.end),
                fill=SIMSET.COLORS['simset.end'], color=SIMSET.COLORS['simset.end'],
                alpha = 0.2) +
    theme_bw() + THEME.4 +
    scale_color_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_fill_manual(values = SIMSET.COLORS, labels=SIMSET.NAMES) + 
    scale_y_continuous(labels=function(y){format(y, big.mark=',')}, limits=c(0, PROJ.YUPPER)) +
    scale_linetype_discrete(guide='none') +
    ylab("Infections (n)") + ggtitle(PLOT.TITLE); plot 

ggsave(plot = plot, 
       filename = file.path(FL.PLOT.DIR, paste0(PLOT.FILE.NAME,'_shaded.png')),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH,
       dpi = PLOT.DPI, device = PLOT.DEVICE)


