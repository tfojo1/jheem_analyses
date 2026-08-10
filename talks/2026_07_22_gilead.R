
source('applications/EHE/ehe_specification.R')
source('talks/croi_2026/figure_settings_croi.R')

PLOT.DIR = "../../../figures/gilead"

if (!dir.exists(PLOT.DIR))
    dir.create(PLOT.DIR)

VERSION = 'ehe'
LOCATION = NYC.MSA

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



#-- Total x 4 outcomes --#
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

plot = simplot(simset, 'prep.uptake', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
    ylab("Number Prescribed PrEP (n)") + ggtitle("PrEP Uptake"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'total_prep.png'),
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


#-- Prev one-way strat --#
plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'age',
               label.function = AGE.LABEL.FUNCTION) + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence by Age"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prev_by_age.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prev_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'sex') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence by Sex"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prev_by_sex.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'risk') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence by Risk"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prev_by_risk.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- Suppression one-way strat --#
plot = simplot(simset, 'suppression', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'age',
               label.function = AGE.LABEL.FUNCTION) + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression by Age"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'supp_by_age.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'suppression', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'race') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression by Race"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'supp_by_race.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'suppression', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'sex') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression by Sex"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'supp_by_sex.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


plot = simplot(simset, 'suppression', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'risk') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Proportion Suppressed (%)") + ggtitle("Viral Suppression by Risk"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'supp_by_risk.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

#-- PrEP one-way strat --#
plot = simplot(simset, 'prep.uptake', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'age',
               label.function = AGE.LABEL.FUNCTION) + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Number Prescribed PrEP (n)") + ggtitle("PrEP Uptake by Age"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prep_by_age.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)

plot = simplot(simset, 'prep.uptake', dimension.values = list(year=YEARS),
               style.manager = STYLE.MANAGER.2b, split.by = 'sex') + theme_bw() + THEME.2b + GUIDES.2 +
    ylab("Number Prescribed PrEP (n)") + ggtitle("PrEP Uptake by Sex"); plot

ggsave(plot = plot, 
       filename = file.path(PLOT.DIR, 'prep_by_sex.png'),
       height = FOUR.PANEL.HEIGHT, width = FOUR.PANEL.WIDTH, 
       dpi = PLOT.DPI, device = PLOT.DEVICE)


