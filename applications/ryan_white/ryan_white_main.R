


source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('../jheem_analyses/applications/ryan_white/ryan_white_mcmc.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_interventions.R')

RW.IS.STATE.LEVEL = T

if (RW.IS.STATE.LEVEL)
    RW.LOCATIONS = sort(c("CA", "NY", "FL", "GA", "TX", "AL", "MS", "LA", "IL", "MO", "WI"))
if (!RW.IS.STATE.LEVEL)
    RW.LOCATIONS = setdiff(MSAS.OF.INTEREST, c(ST.LOUIS.MSA, CINCINATTI.MSA))
    # St Louis is not an EHE city, Cincinatti does not have RW data

RW.LOCATION.DESCRIPTOR = ifelse(RW.IS.STATE.LEVEL, 'State', "City")
RW.LOCATION.DESCRIPTOR.PLURAL = ifelse(RW.IS.STATE.LEVEL, 'States', "Cities")

# Run settings
VERBOSE = T
CALIBRATION.CODE = "final.ehe.state" 
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
RW.WEB.FROM.YEAR = 2015
RW.WEB.TO.YEAR = 2035
RW.WEB.SEED.FROM.YEAR = RW.WEB.FROM.YEAR
RW.WEB.SEED.TO.YEAR = 2025

# Other medicaid stuff

city.main.state = sapply(RW.LOCATIONS, function(loc){
  
  name = locations::get.location.name(loc)
  
  split = strsplit(name, ', ')
  states.str = split[[1]][length(split[[1]])]
  states = strsplit(states.str, "-")[[1]]
  
  states[1]
})


if (RW.IS.STATE.LEVEL)
{
    RW.MEDICAID.EXPANSION.LOCATIONS = intersect(RW.LOCATIONS, MEDICAID.EXPANSION.STATES)
    RW.MEDICAID.NONEXPANSION.LOCATIONS = intersect(RW.LOCATIONS, MEDICAID.NONEXPANSION.STATES)
    
    city.main.state.medicaid.expansion = rep(T, length(RW.LOCATIONS))
    names(city.main.state.medicaid.expansion) = RW.LOCATIONS
    
    city.main.state.medicaid.expansion[intersect(MEDICAID.NONEXPANSION.STATES, RW.LOCATIONS)] = F
}

if (!RW.IS.STATE.LEVEL)
{
    RW.MEDICAID.EXPANSION.CITIES = RW.MEDICAID.EXPANSION.LOCATIONS = names(city.main.state.medicaid.expansion)[city.main.state.medicaid.expansion]
    RW.MEDICAID.NONEXPANSION.CITIES = RW.MEDICAID.NONEXPANSION.LOCATIONS = names(city.main.state.medicaid.expansion)[!city.main.state.medicaid.expansion]
    
    
    city.main.state.medicaid.expansion = unlist(sapply(city.main.state, function(st){
        
        state_info$medicaid_expansion[state_info$state==gsub(" ", "_", locations::get.location.name(st))]=='Yes'
        
    }))
}

shade.darker <- function(color, delta)
{
    rgb.val = pmax(0, pmin(255, col2rgb(color) + delta))
    dim(rgb.val) = c(3, length(color))
    rgb(rgb.val[1,], rgb.val[2,], rgb.val[3,], maxColorValue = 255)
}

RW.PALETTE.RAW = ggsci::pal_jama()(7)
RW.DATA.COLOR = RW.PALETTE.RAW[1]
RW.BASELINE.COLOR = RW.PALETTE.RAW[3]
RW.END.COLOR = RW.PALETTE.RAW[4]
RW.B.INTR.COLOR = RW.PALETTE.RAW[5]
RW.P.INTR.COLOR = RW.PALETTE.RAW[2]
RW.NONEXP.COLOR = RW.PALETTE.RAW[2]
RW.NONEXP.LABEL.COLOR = shade.darker(RW.NONEXP.COLOR,-80)
RW.EXP.COLOR = RW.PALETTE.RAW[6]
RW.EXP.LABEL.COLOR = shade.darker(RW.EXP.COLOR,-40)
RW.TOTAL.LABEL.COLOR = "#000000"
RW.TOTAL.TEXT.COLOR = "#FFFFFF"

RW.PALETTE = c(
    data = RW.DATA.COLOR,
    baseline = RW.BASELINE.COLOR,
    end = RW.END.COLOR,
    b.intr = RW.B.INTR.COLOR,
    p.intr = RW.P.INTR.COLOR,
    nonexp = RW.NONEXP.COLOR,
    exp = RW.EXP.COLOR,
    nonexp_label = RW.NONEXP.LABEL.COLOR,
    exp_label = RW.EXP.LABEL.COLOR,
    total_label = RW.TOTAL.LABEL.COLOR
)


# RW.PALETTE = ggsci::pal_aaas()(10)
# RW.DATA.COLOR = RW.PALETTE[4]
# RW.BASELINE.COLOR = RW.PALETTE[1]
# RW.END.COLOR = RW.PALETTE[6]
# RW.B.INTR.COLOR = RW.PALETTE[3]
# RW.P.INTR.COLOR = RW.PALETTE[5]
# RW.NONEXP.COLOR = RW.PALETTE[4]
# RW.EXP.COLOR = RW.PALETTE[5]


# RW.PALETTE = ggsci::pal_nejm()(8)
# RW.PALETTE = ggsci::pal_bmj()(9)
# RW.PALETTE = ggsci::pal_lancet()(9)
# RW.PALETTE = ggsci::pal_frontiers()(10)
# RW.PALETTE = ggsci::pal_jco()(10)
# RW.PALETTE = ggsci::pal_uchicago()(9)

color = RW.PALETTE.RAW; names(color) = RW.PALETTE.RAW
ggplot2::qplot(1:length(RW.PALETTE.RAW), 1:length(RW.PALETTE.RAW), geom='point', size=20, color=RW.PALETTE.RAW) + ggplot2::geom_point(size=20) +
  ggplot2::scale_color_manual(values = color)

ggplot2::qplot(1:length(RW.PALETTE), 1:length(RW.PALETTE), geom='point', size=20, color=names(RW.PALETTE)) + ggplot2::geom_point(size=20) +
    ggplot2::scale_color_manual(values = RW.PALETTE)



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