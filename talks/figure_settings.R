PLOT.DIR = '../../../../Talks/JHEEM/2025.11 CFAR Talk/figures'
if (!dir.exists(PLOT.DIR))
    dir.create(PLOT.DIR, recursive = T)
PLOT.DEVICE = 'png'
PLOT.DPI = 600

library(ggplot2)


YEARS = 2008:2022
YEARS.PLUS.FUTURE = 2008:2035

SINGLE.PANEL.WIDTH = 8.5
SINGLE.PANEL.HEIGHT = 5.5
FOUR.PANEL.WIDTH = 4.6
FOUR.PANEL.HEIGHT = 2.7
TWO.PANEL.WIDTH = 6
TWO.PANEL.HEIGHT = 4
THREE.PANEL.WIDTH = 4.2
THREE.PANEL.HEIGHT = 3.6


THEME.1 = theme(text = element_text(size = 24),
              legend.position = 'none',
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              plot.title = element_blank(),
              axis.title.x = element_blank()
)


THEME.2a = theme(text = element_text(size = 12),
                 legend.position = 'none',
                 strip.background = element_blank(),
                 strip.text.x = element_blank(),
                 plot.title = element_text(hjust=0.5),
                 legend.title = element_blank(),
                 axis.title.x = element_blank()
)

THEME.2b = theme(text = element_text(size = 12),
                 legend.position = 'bottom',
                 strip.background = element_blank(),
                 strip.text.x = element_blank(),
                 plot.title = element_text(hjust=0.5),
                 legend.title = element_blank(),
                 axis.title.x = element_blank(),
                 legend.background = element_blank(),
                 legend.margin = margin(t=-10,b=-5,r=0,l=0)
)

THEME.3 = theme(text = element_text(size = 16),
                legend.position = 'none',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.title = element_text(hjust=0.5),
                legend.title = element_blank(),
                axis.title.x = element_blank()
)

# For different scenarios
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

PALETTE.1 = ggsci::pal_jama()(7)[-1]
PALETTE.2 = ggsci::pal_jama()(7)[-(1:2)]

STYLE.MANAGER.SINGLE.SIM = create.style.manager(color.sim.by = 'simset', 
                                                linewidth.baseline = 4,
                                                sim.palette = PALETTE.1)
STYLE.MANAGER.1 = create.style.manager(color.sim.by = 'simset', 
                                       linewidth.baseline = 0.25,
                                       alpha.line = 0.1,
                                       sim.palette = PALETTE.1)
STYLE.MANAGER.2a = create.style.manager(color.sim.by = 'simset', 
                                        linewidth.baseline = 0.25,
                                        alpha.line = 0.1,
                                        shade.data.by = NULL,
                                        sim.palette = PALETTE.1)
STYLE.MANAGER.2b = create.style.manager(color.sim.by = 'stratum', 
                                        linewidth.baseline = 0.25,
                                        alpha.line = 0.1,
                                        shade.data.by = NULL,
                                        sim.palette = PALETTE.2)
STYLE.MANAGER.3 = create.style.manager(color.sim.by = 'simset', 
                                       linewidth.baseline = 0.25,
                                       alpha.line = 0.1,
                                       shade.data.by = NULL,
                                       sim.palette = PALETTE.1)

GUIDES.2 = guides(shape = "none",
                  linetype = "none",
                  color = "none")



AGE.LABELS = c(
    '13-24 years' = '13-24',
    '25-34 years' = '25-34',
    '35-44 years' = '35-44',
    '45-54 years' = '45-54',
    '55+ years' = '55+'
)
AGE.LABEL.FUNCTION = function(to.label){AGE.LABELS[to.label]}
