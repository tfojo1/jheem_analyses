# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( color.data.by = "source",shade.data.by =  "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.12580'  # Baltimore MSA

get.jheem.root.directory() #"/Volumes/jheem$"

load(paste0(get.jheem.root.directory(),"/shield/","calib.diagnosis.07.01.pk1","Rdata")) #good fit
# Quick checkpoint ----
simset1=simset
simset$n.sim
# Extract first and last simulations and their parameters 
sim.first1    <- simset$first.sim()
sim.last1     <- simset$last.sim()
params.first1 <- simset$first.sim()$params
params.last1  <- simset$last.sim()$params

# MANUAL run to compare
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
params.manual <- params.last1
sim.manual <- engine$run(params.manual)


# PLOT -----
simplot(
    sim.last1,
    sim.manual,
    # split.by = "race", facet.by = "sex",
    # split.by = "race", facet.by = "age",
    # outcomes = c("population"),    split.by = "race", facet.by = "age",
    outcomes = c("diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing"),
    # outcomes = c("prevalence"),
    dimension.values = list(year = 1970:2023),
    style.manager = source.style.manager
)

