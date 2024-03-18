




# Houston -----

load("../../files/simulations/ehe/init.transmission.ehe-100/C.26420/ehe_init.transmission.ehe-100_C.26420_baseline.Rdata")

# pdf("calibration_plots.pdf",width = 17,height = 10)
# simplot(simset, outcomes = "new")
# simplot(simset, outcomes = "diagnosed.prevalence")
# simplot(simset, outcomes = "new", facet.by = "race", split.by = "risk")
houston_risk.race <- simplot(simset, outcomes = "new", facet.by = "risk", split.by = "race")+ theme(legend.position = "none")
houston_risk <- simplot(simset, outcomes = "new", split.by = "risk") + theme(legend.position = "none")
houston_race <- simplot(simset, outcomes = "new", split.by = "race") + theme(legend.position = "none")
# simplot(simset, outcomes = "new", facet.by = "risk") 
# simplot(simset, outcomes = "new", facet.by = "race")
# dev.off()

# simplot(simset, outcomes = "new", facet.by = "sex")
# 
# SURVEILLANCE.MANAGER$pull(outcome = "diagnoses", dimension.values = list(location = 'C.16980'))
# SURVEILLANCE.MANAGER$pull(outcome = "diagnoses", dimension.values = list(location = 'C.16980'),
#                           keep.dimensions = c('year','race'))

# Miami -----

load("../../files/simulations/ehe/init.transmission.ehe-100/C.33100/ehe_init.transmission.ehe-100_C.33100_baseline.Rdata")

miami_risk.race <- simplot(simset, outcomes = "new", facet.by = "risk", split.by = "race")
miami_risk <- simplot(simset, outcomes = "new", split.by = "risk") 
miami_race <- simplot(simset, outcomes = "new", split.by = "race")


# Chicago -----

load("../../files/simulations/ehe/init.transmission.ehe-100/C.16980/ehe_init.transmission.ehe-100_C.16980_baseline.Rdata")

chicago_risk.race <- simplot(simset, outcomes = "new", facet.by = "risk", split.by = "race")+ theme(legend.position = "none")
chicago_risk <- simplot(simset, outcomes = "new", split.by = "risk") + theme(legend.position = "none")
chicago_race <- simplot(simset, outcomes = "new", split.by = "race") + theme(legend.position = "none")

if (!require("cowplot")) {
  install.packages("cowplot")
}

cowplot::plot_grid(houston_risk.race,
                   miami_risk.race,
                   chicago_risk.race,
                   labels = c("Houston", "Miami", "Chicago"), 
                   ncol = 1)


cowplot::plot_grid(houston_risk, miami_risk, chicago_risk,
                   nrow = 3, 
                   labels = c("Houston", "Miami", "Chicago"))

cowplot::plot_grid(houston_race, miami_race, chicago_race,
                   nrow = 3, 
                   labels = c("Houston", "Miami", "Chicago"))
