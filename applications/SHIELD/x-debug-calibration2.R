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
{
    params.manual <- params.last
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
    params.manual["ps.diagnoses.multiplier.1970"] <- 1
    params.manual["el.diagnoses.multiplier.1970"] <- 1
    params.manual["lu.diagnoses.multiplier.1970"] <- 1
    params.manual0<-params.manual
    sim.manual0 <- engine$run(params.manual)
}
#looking at local variables 
q=engine$extract.quantity.values() #returns the input values to the model
q$n.initial.population.infected
sapply(q$n.initial.population.infected,sum)
unlist(q$ps.diagnoses.multiplier.1970) *unlist(q$prp.ps.diag.1997)*sapply(q$n.initial.population,sum)+
unlist(q$el.diagnoses.multiplier.1970) *unlist(q$prp.el.diag.1997)*sapply(q$n.initial.population,sum)+
unlist(q$lu.diagnoses.multiplier.1970) *unlist(q$prp.lu.diag.1997)*sapply(q$n.initial.population,sum)

simplot(
    sim.manual0,
    # sim.manual1,
    # outcomes = c("population"),
    # split.by = "profile",# facet.by = "age",
    outcomes = c(
        # "incidence",
                 "diagnosis.total",  "diagnosis.ps",
                "diagnosis.el.misclassified", "diagnosis.ll.misclassified"
                 
                 # "diagnosis.el.true" ,"diagnosis.ll.true",
                 # "diagnosis.delayed.treatment","diagnosis.immediate.treatment",
                 # "diagnosis.tertiary","diagnosis.cns",
                 # "diagnosis.congenital",
                 # "hiv.testing","sti.testing"
    ),
    dimension.values = list(year = 1970:2030),
    style.manager = source.style.manager
)

{
    params.manual<- params.manual0
    params.manual["transmission.rate.multiplier.heterosexual0"] <- 0
    # params.manual["transmission.rate.multiplier.heterosexual1"] <- 1
    # params.manual["transmission.rate.multiplier.heterosexual2"] <- 1
    # params.manual["transmission.rate.multiplier.heterosexual3"] <- 1
    # params.manual["transmission.rate.multiplier.heterosexual4"] <- 1
    
    # transmission.rate.multiplier.heterosexual0       1.0386505
    # transmission.rate.multiplier.heterosexual1       1.5688099
    # transmission.rate.multiplier.heterosexual2       1.1142859
    # transmission.rate.multiplier.heterosexual3       1.3936504
    # transmission.rate.multiplier.heterosexual4       1.3756808
    params.manual["transmission.rate.multiplier.msm0"] <- 0
    # params.manual["transmission.rate.multiplier.msm1"] <- 1
    # params.manual["transmission.rate.multiplier.msm2"] <- 1
    # params.manual["transmission.rate.multiplier.msm3"] <- 1
    # params.manual["transmission.rate.multiplier.msm4"] <- 1
    # # transmission.rate.multiplier.msm0                1.4459724
    # # transmission.rate.multiplier.msm1                1.3638819
    # # transmission.rate.multiplier.msm2                1.5587803
    # # transmission.rate.multiplier.msm3                1.2001844
    # transmission.rate.multiplier.msm4                0.6623852
    sim.manual1 <- engine$run(params.manual)
}
# comment out remission: no treatment 

#tests=
# kill transmission: Kills incidence ----> PASS
# kill prp.symptomatic Testing: increase transmission ----> PASS
# kill HIV Testing: increase transmission ---->  FAIL
# kill STI SCREENING by KNOTS: increase transmission ---->  FAIL
# the two are the same, 
# the peak is before 1980...why?
    # kill STI SCREENING by STAGE: no change! ---->  FAIL
    # is this correctly applied on the infected poppulation?