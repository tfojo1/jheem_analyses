if (1==2){
    source('../jheem_analyses/commoncode/locations_of_interest.R')
    source("../jheem_analyses/applications/SHIELD/shield_specification.R")
    source('../jheem_analyses/applications/SHIELD/analysis/calibration/pretty_plots_helper_functions.R')
    
    print(paste("Root directory is set to: ",ROOT.DIR))
    BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/calib.8.21.stage3.az")
    FIG.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/figures/") else "figures/"
    TABLE.DIR <- if (exists("BASE.PATH")) paste0(BASE.PATH, "/tables/") else "tables/"
    # for (d in c(TABLE.DIR, FIG.DIR)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    
}
# READ DATA ----
calibration.codes <- "calib.8.21.stage3.az"
calib.simsets <- load.calib.simsets(
    locations         =  SHIELD.TEN.MSAS,
    calibration.codes = calibration.codes,
    n.sim = 400
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
