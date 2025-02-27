
# First, load up the sim I sent you

source('applications/ryan_white/ryan_white_specification.R')
source('applications/ryan_white/ryan_white_likelihoods.R')


adap.lk = rw.adap.likelihood.instructions$instantiate.likelihood("rw",sim$location, data.manager = RW.DATA.MANAGER)
adap.suppression.lk = rw.adap.suppression.likelihood.instructions$instantiate.likelihood("rw",sim$location, data.manager = RW.DATA.MANAGER)