


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
CALIBRATION.CODE = 'final.ehe'
N.SIM = 1000
FORCE.REDO = T
N.CHUNKS = 20

RW.INTERVENTION.CODES = c('noint', 'rw.end', 'rw.b.intr', 'rw.p.intr')

RW.INT.RUN.TO.YEAR = 2035
RW.INT.RUN.FROM.YEAR = 2025

RW.INT.KEEP.TO.YEAR = RW.INT.RUN.TO.YEAR
RW.NOINT.KEEP.FROM.YEAR = 2010
RW.INT.KEEP.FROM.YEAR = 2024

RW.N.SIM.FOR.WEB = 80
RW.WEB.FROM.YEAR = 2010
RW.WEB.TO.YEAR = 2035

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
RW.DATA.COLOR = RW.PALETTE[1]
RW.BASELINE.COLOR = RW.PALETTE[2]
RW.END.COLOR = RW.PALETTE[4]
RW.B.INTR.COLOR = RW.PALETTE[5]
RW.P.INTR.COLOR = RW.PALETTE[3]


RW.PALETTE = ggsci::pal_aaas()(7)
RW.DATA.COLOR = RW.PALETTE[4]
RW.BASELINE.COLOR = RW.PALETTE[1]
RW.END.COLOR = RW.PALETTE[6]
RW.B.INTR.COLOR = RW.PALETTE[3]
RW.P.INTR.COLOR = RW.PALETTE[5]
RW.NONEXP.COLOR = RW.PALETTE[4]
RW.EXP.COLOR = RW.PALETTE[5]

color = RW.PALETTE; names(color) = RW.PALETTE
ggplot2::qplot(1:length(RW.PALETTE), 1:length(RW.PALETTE), geom='point', size=20, color=RW.PALETTE) + ggplot2::geom_point(size=20) +
  ggplot2::scale_color_manual(values = color)


RW.CITY.SHORT.NAMES = c(
  C.35620='New York, NY',
  C.33100='Miami/Fort Lauderdale, FL',
  C.31080='Los Angeles, CA',
  C.12060='Atlanta, GA',
  C.26420='Houston, TX',
  C.19100='Dallas/Fort Worth, TX',
  C.16980='Chicago, IL',
  C.47900='Washington, DC',
  C.37980='Philadelphia, PA',
  C.36740='Orlando, FL',
  C.41860='San Francisco, CA',
  C.38060='Phoenix, AZ',
  C.45300='Tampa, FL',
  C.40140='Riverside, CA',
  C.19820='Detroit, MI',
  C.12580='Baltimore, MD',
  C.29820='Las Vegas, NV',
  C.14460='Boston, MA',
  C.41740='San Diego, CA',
  C.16740='Charlotte, NC',
  C.41700='San Antonio, TX',
  C.27260='Jacksonville, FL',
  C.35380='New Orleans, LA',
  C.32820='Memphis, TN',
  C.42660='Seattle, WA',
  C.12420='Austin, TX',
  C.26900='Indianapolis, IN',
  C.18140='Columbus, OH',
  C.12940='Baton Rouge, LA',
  C.40900='Sacramento, CA',
  C.17460='Cleveland, OH'
)