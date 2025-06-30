# detach("package:deSolve", unload = TRUE)
# detach("package:deSolve", character.only = TRUE)
# unloadNamespace("jheem2")
# unloadNamespace("deSolve")


# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
VERSION='shield'
LOCATION="C.12580"

load("~/Downloads/params.set.Rdata")
#includes 2 parameter sets. params.manual and params.last

engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)

params.manual <- params.last
{
    #remove
    params.manual <- params.manual[!names(params.manual) %in% c("initial.infection.multiplier.1970.late","initial.infection.multiplier.1970.early")]
    
    #add calib params that have been added since then:
    params.manual["syphilis.screening.multiplier.1980"] <- 1
    params.manual["sti.screening.multiplier.ps"] <- 0
    params.manual["sti.screening.multiplier.el"] <- 1
    params.manual["sti.screening.multiplier.ll"] <- 1
    params.manual["sti.screening.multiplier.tertiary"] <- 1
    params.manual["sti.screening.multiplier.cns"] <- 1
    
    #  
    params.manual["ps.diagnoses.multiplier.1970"] <- 0
    params.manual["el.diagnoses.multiplier.1970"] <- 0
    params.manual["lu.diagnoses.multiplier.1970"] <- 0
    sim.manual0 <- engine$run(params.manual)
}

# # {
#     params.manual<- params.last
#     params.manual["syphilis.screening.multiplier.1980"] <- 1
#     params.manual["sti.screening.multiplier.ps"] <- 0
#     params.manual["sti.screening.multiplier.el"] <- 1
#     params.manual["sti.screening.multiplier.ll"] <- 1
#     params.manual["sti.screening.multiplier.tertiary"] <- 1
#     params.manual["sti.screening.multiplier.cns"] <- 1
#     # 
#     # params.manual["prp.symptomatic.primary.msm"] <- params.manual["prp.symptomatic.primary.msm"] *0
#     # params.manual["prp.symptomatic.primary.heterosexual_male"] <- params.manual["prp.symptomatic.primary.heterosexual_male"] *0
#     # params.manual["prp.symptomatic.primary.female"] <- params.manual["prp.symptomatic.primary.female"] *0
#     # params.manual["prp.symptomatic.secondary.msm"] <- params.manual["prp.symptomatic.secondary.msm"] *0
#     # params.manual["prp.symptomatic.secondary.heterosexual_male"] <- params.manual["prp.symptomatic.secondary.heterosexual_male"] *0
#     # params.manual["prp.symptomatic.secondary.female"] <- params.manual["prp.symptomatic.secondary.female"] *0
#     #
#     # params.manual["hiv.testing.or"] <- params.manual["hiv.testing.or"] *0
#     # #
#     # params.manual["syphilis.screening.multiplier.1980"] <- params.manual["syphilis.screening.multiplier.1980"] *0
#     # params.manual["syphilis.screening.multiplier.1990"] <- params.manual["syphilis.screening.multiplier.1990"] *0
#     # params.manual["syphilis.screening.multiplier.2000"] <- params.manual["syphilis.screening.multiplier.2000"] *0
#     # params.manual["syphilis.screening.multiplier.2010"] <- params.manual["syphilis.screening.multiplier.2010"] *0
#     # params.manual["syphilis.screening.multiplier.2020"] <- params.manual["syphilis.screening.multiplier.2020"] *0
#     # # #
#     # params.manual["sti.screening.multiplier.ps"] <- params.manual["sti.screening.multiplier.ps"] *0
#     # params.manual["sti.screening.multiplier.el"] <- params.manual["sti.screening.multiplier.el"] *0
#     # params.manual["sti.screening.multiplier.ll"] <- params.manual["sti.screening.multiplier.ll"] *0
#     # params.manual["sti.screening.multiplier.tertiary"] <- params.manual["sti.screening.multiplier.tertiary"] *0
#     # params.manual["sti.screening.multiplier.cns"]<- params.manual["sti.screening.multiplier.cns"] *0
#     
#     sim.manual1 <- engine$run(params.manual)
# }
# comment out remission: no treatment 

simplot(
    # sim.first,
    # sim.last,
    sim.manual0,
    # sim.manual1,
    # outcomes = c("population"),
    # split.by = "profile",# facet.by = "age",
    outcomes = c("incidence",
                 "diagnosis.total",  "diagnosis.ps",
                "diagnosis.el.misclassified", "diagnosis.ll.misclassified"
                 
                 # "diagnosis.el.true" ,"diagnosis.ll.true",
                 # "diagnosis.delayed.treatment","diagnosis.immediate.treatment",
                 # "diagnosis.tertiary","diagnosis.cns",
                 # "diagnosis.congenital",
                 # "hiv.testing","sti.testing"
    ),
    # dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)


#tests=
# kill transmission: Kills incidence ----> PASS
# kill prp.symptomatic Testing: increase transmission ----> PASS
# kill HIV Testing: increase transmission ---->  FAIL
# kill STI SCREENING by KNOTS: increase transmission ---->  FAIL
# the two are the same, 
# the peak is before 1980...why?
    # kill STI SCREENING by STAGE: no change! ---->  FAIL
    # is this correctly applied on the infected poppulation?