
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
CALIBRATION.CODE = 'full.with.covid2'
N.SIM = 100
FORCE.REDO = T