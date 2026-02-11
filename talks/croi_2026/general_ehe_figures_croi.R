
source('applications/EHE/ehe_specification.R')
source('talks/croi_2026/figure_settings_croi.R')

VERSION = 'ehe'
LOCATION = 'NY'
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



#-- Total x 2 outcomes (NEW using 4-panel dimensions) --#
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


# Old dimensions
if(1==2){
    
    #-- Total x 2 outcomes --#
    plot = simplot(simset, 'new', dimension.values = list(year=YEARS),
                   style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
        ylab("New Diagnoses (n)") + ggtitle("New Diagnoses"); plot
    
    ggsave(plot = plot, 
           filename = file.path(PLOT.DIR, 'total_new_old.png'),
           height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
           dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    
    plot = simplot(simset, 'diagnosed.prevalence', dimension.values = list(year=YEARS),
                   style.manager = STYLE.MANAGER.2a) + theme_bw() + THEME.2a +
        ylab("Prevalent Cases (n)") + ggtitle("Estimated Prevalence"); plot
    
    ggsave(plot = plot, 
           filename = file.path(PLOT.DIR, 'total_prevalence_old.png'),
           height = TWO.PANEL.HEIGHT, width = TWO.PANEL.WIDTH, 
           dpi = PLOT.DPI, device = PLOT.DEVICE)
    
    
}