# ****************************************************************************************************
# 06 PRETTY PLOTS: CALIBRATION FIGURES FOR THE MANUSCRIPT ----
# ****************************************************************************************************
# Plots the calibration fit (model against surveillance data) from the baseline calibration
# simsets, and saves the figures in the same figures/ folder as the analysis.
#
# 1. It does not use 00_settings.R or the result arrays. It reads the calibration simsets
#    directly and has its own settings below.
# 2. It can run at any point; it does not depend on steps 01-05.
# 3. The figures go to <root>/shield/outputs/<CALIB.NAME>/figures/. Keep CALIB.NAME the same as
#    in 00_settings.R, and the calibration figures land next to the analysis figures.
#
# HELPERS
#   pretty_plots_helper_functions.R          (this folder) shield.plot() and its data layer
#   pretty_plots_style.R                     (this folder) years, colours, theme, labels, sizes
#                                            and save.shield.fig(). Sourced by the helper file.
#   ../calibration/calibration_helper_functions.R   load.calib.simsets()
# ****************************************************************************************************

# ---- DEPENDENCIES ----
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source('../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/pretty_plots_helper_functions.R')


# ---- SETTINGS ----
# 1. CALIB.NAME: the calibration to plot. It also names the output folder.
# 2. N.SIM: how many sims the calibration run kept (the "-800" in the simset folder name).
CALIB.NAME <- "calib.10.20.stage3"
N.SIM      <- 800

# ---- OUTPUT FOLDER ----
print(paste("Root directory is set to: ", ROOT.DIR))
FIG.DIR <- shield.fig.path(CALIB.NAME, create = TRUE)
print(paste("Figures will be written to: ", FIG.DIR))


# READ DATA ----
calib.simsets <- load.calib.simsets(
    locations         = SHIELD.TEN.MSAS,
    calibration.codes = CALIB.NAME,
    n.sim             = N.SIM
)

# PLOT CALIBRATIONS----
{
    CITIES<- c("Atlanta","Houston")

p1<-shield.plot(calib.simsets, "diagnosis.ps", locations = CITIES,overlay.locations = F,
                log.y = TRUE,
                show.sources = T,
                title = "PS Syphilis Diagnoses",
                titles = c(Atlanta = "A. Atlanta–Sandy Springs",
                           Chicago = "B. Chicago–Naperville"));p1
p2<-shield.plot(calib.simsets, "diagnosis.ps", locations = CITIES,split.by = "sex",overlay.locations = F,
                log.y = TRUE,show.sources = T,
                title = "PS Syphilis Diagnoses, by Sex",
                titles = c(Atlanta = "C. Atlanta–Sandy Springs",
                           Chicago = "D. Chicago–Naperville"));p2
p3<-shield.plot(calib.simsets, "prop.male.ps.diag.among.msm", locations = CITIES,
                log.y = TRUE, show.sources = F,
                shape.by = "both",show.data = F,
                title = "E. Proportion of Male Diagnosis among MSM");p3
p4<-shield.plot(calib.simsets, "incidence", locations = CITIES,
                log.y = TRUE,show.sources = F,
                title = "F. Syphilis Incidence");p4


#putting them together
plt<- (p1 /p2)/(p3+p4) ;plt
# +plot_layout(guides = "collect") & theme(legend.position = "bottom");plt

ggsave(file.path(FIG.DIR, "fig2-Epi_impact1.png"), plt,
       width = 12, height = 9, dpi = 300, bg = "white")
}
