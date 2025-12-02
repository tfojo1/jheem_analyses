
source('talks/figure_settings.R')
source('applications/EHE/ehe_specification.R')

VERSION = 'ehe'
LOCATION = 'C.12580'
NSIM = 1000

if (locations::get.location.type(LOCATION)=='STATE')
    CALIBRATION.CODE = 'final.ehe.state'

if (locations::get.location.type(LOCATION)!='STATE')
    CALIBRATION.CODE = 'final.ehe'


if (!exists('simset'))
{
    simset = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE, n.sim = NSIM)
    simset = simset$thin(keep=100)
}

#-- Total New Diagnoses --#

plot = simplot(simset$last.sim(), 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.SINGLE.SIM) + 
    theme_bw() + THEME.1 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'single_sim_total_new.png'),
       height = SINGLE.PANEL.HEIGHT, width = SINGLE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


#-- Total x 4 outcomes --#

plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.1) + theme_bw() + THEME.1 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'simset_total_new.png'),
       height = SINGLE.PANEL.HEIGHT, width = SINGLE.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_new.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
    ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_prevalence.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'suppression', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_suppression.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'awareness', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
    ylab("Proportion Aware (%)") + ggtitle("Knowledge of Status"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_awareness.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


#-- New one-way strat --#


plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'age',
               label.function = AGE.LABEL.FUNCTION) + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses by Age"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_by_age.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'sex') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses by Sex"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_by_sex.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'risk') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses by Risk"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_by_risk.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)



#-- New risk x race --#

plot = simplot(simset, 'new', 
               dimension.values = list(year=YEARS),
               dimension.values.post.mapping = list(risk='msm'),
               facet.by='risk',
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses - MSM by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_msm_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'new', 
               dimension.values = list(year=YEARS),
               dimension.values.post.mapping = list(risk='heterosexual'),
               facet.by='risk',
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses - Heterosexual by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_het_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'new', 
               dimension.values = list(year=YEARS),
               dimension.values.post.mapping = list(risk='idu'),
               facet.by='risk',
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses - PWID by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_idu_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'new', 
               dimension.values = list(year=YEARS),
               dimension.values.post.mapping = list(risk='msm_idu'),
               facet.by='risk',
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses - MSM/PWID by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'new_msm_idu_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- Total New and Incidence into the Future --#
plot = simplot(simset, 'new', dimension.values = list(year=YEARS.PLUS.FUTURE),
               style.manager = STYLE.MANAGER.3) + theme_bw() + THEME.3 +
    ylab("New Diagnoses (n)") + ggtitle("New Diagnoses"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_new_future.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'incidence', dimension.values = list(year=YEARS.PLUS.FUTURE),
               style.manager = STYLE.MANAGER.3) + theme_bw() + THEME.3 +
    ylab("Infections (n)") + ggtitle("Incidence"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_inc_future.png'),
       height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)
