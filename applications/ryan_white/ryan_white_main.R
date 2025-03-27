


source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('../jheem_analyses/applications/ryan_white/ryan_white_mcmc.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_interventions.R')

RW.LOCATIONS = setdiff(MSAS.OF.INTEREST, c(ST.LOUIS.MSA, CINCINATTI.MSA))
# St Louis is not an EHE city, Cincinatti does not have RW data

# Run settings
VERBOSE = T
CALIBRATION.CODE = 'full.ehe'
N.SIM = 100
FORCE.REDO = T

RW.INTERVENTION.CODES = c('noint', 'rw.end', 'rw.b.intr', 'rw.p.intr')

RW.INT.RUN.TO.YEAR = 2035
RW.INT.RUN.FROM.YEAR = 2025

RW.INT.KEEP.TO.YEAR = RW.INT.RUN.TO.YEAR
RW.NOINT.KEEP.FROM.YEAR = 2010
RW.INT.KEEP.FROM.YEAR = 2024

# Other medicaid stuff

city.main.state = sapply(RW.LOCATIONS, function(loc){
  
  name = locations::get.location.name(loc)
  
  split = strsplit(name, ', ')
  states.str = split[[1]][length(split[[1]])]
  states = strsplit(states.str, "-")[[1]]
  
  states[1]
})

city.main.state.medicaid.expansion = unlist(sapply(city.main.state, function(st){
  
  state_info$medicaid_expansion[state_info$state==gsub(" ", "_", locations::get.location.name(st))]=='Yes'
  
}))

RW.MEDICAID.EXPANSION.CITIES = names(city.main.state.medicaid.expansion)[city.main.state.medicaid.expansion]
RW.MEDICAID.NONEXPANSION.CITIES = names(city.main.state.medicaid.expansion)[!city.main.state.medicaid.expansion]


RW.PALETTE = ggsci::pal_jama()(7)

ggplot2::qplot(1:length(RW.PALETTE), 1:length(RW.PALETTE), geom='point', size=20, color=RW.PALETTE) + ggplot2::geom_point(size=20)
