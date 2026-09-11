

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


# 
# AGE.LABELS = c(
#     '13-24 years' = '13-24',
#     '25-34 years' = '25-34',
#     '35-44 years' = '35-44',
#     '45-54 years' = '45-54',
#     '55+ years' = '55+'
# )
# AGE.LABEL.FUNCTION = function(to.label){AGE.LABELS[to.label]}

RW.CALIB.YEARS = 2010:2024
RW.CALIB.HEIGHT = FOUR.PANEL.HEIGHT
RW.CALIB.WIDTH = 4
RW.CALIB.SCALE = scale_x_continuous(breaks = c(2010, 2018, 2021, 2024))

shade.darker <- function(color, delta)
{
    rgb.val = pmax(0, pmin(255, col2rgb(color) + delta))
    dim(rgb.val) = c(3, length(color))
    rgb(rgb.val[1,], rgb.val[2,], rgb.val[3,], maxColorValue = 255)
}

RW.PALETTE.RAW = ggsci::pal_jama()(7)
RW.DATA.COLOR = RW.PALETTE.RAW[1]
RW.BASELINE.COLOR = RW.PALETTE.RAW[3]
RW.END.COLOR = RW.PALETTE.RAW[4]
RW.B.INTR.COLOR = RW.PALETTE.RAW[5]
RW.P.INTR.COLOR = RW.PALETTE.RAW[2]
RW.NONEXP.COLOR = RW.PALETTE.RAW[2]
RW.NONEXP.LABEL.COLOR = shade.darker(RW.NONEXP.COLOR,-80)
RW.EXP.COLOR = RW.PALETTE.RAW[6]
RW.EXP.LABEL.COLOR = shade.darker(RW.EXP.COLOR,-40)
RW.TOTAL.LABEL.COLOR = "#000000"
RW.TOTAL.TEXT.COLOR = "#FFFFFF"

RW.PALETTE = c(
    data = RW.DATA.COLOR,
    baseline = RW.BASELINE.COLOR,
    end = RW.END.COLOR,
    b.intr = RW.B.INTR.COLOR,
    p.intr = RW.P.INTR.COLOR,
    nonexp = RW.NONEXP.COLOR,
    exp = RW.EXP.COLOR,
    nonexp_label = RW.NONEXP.LABEL.COLOR,
    exp_label = RW.EXP.LABEL.COLOR,
    total_label = RW.TOTAL.LABEL.COLOR
)


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

simplot(calib.simsets$`Atlanta – calib.8.21.stage3.az`$last_sim,
               outcomes="diagnosis.ps", style.manager = RW.STYLE.MANAGER.1) + theme_bw() + THEME.2a
    # # RW.CALIB.SCALE +
    # ylab("diagnosis ps") + ggtitle("NDiagnosis PS"); plot


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
