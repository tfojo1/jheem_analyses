source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

## Full simset 
load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-15_C.12580.Rdata')
simset.full = simset
sim.full.last = simset.full$last.sim()
params.full.last = sim.full.last$params
sim.full.last = engine$run(parameters = params.full.last) 

## Trans simset
load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-12_C.12580.Rdata")
simset.trans = simset
sim.trans.last = simset.trans$last.sim()
params.trans.last = sim.trans.last$params
sim.trans.last = engine$run(parameters = params.trans.last) 

x = exp(full.lik$compute.piecewise(sim.full.last) - full.lik$compute.piecewise(sim.trans.last))
cbind(sort(x,decreasing = T))

simplot(sim.trans.last,
        sim.full.last,
        outcomes = c("testing"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans.last,
        sim.full.last,
        style.manager = source.style.manager,
        facet.by = "race", 
        outcomes = c("testing"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.trans.last,
        sim.full.last,
        facet.by = "race", split.by = "risk", 
        outcomes = c("testing"), 
        dimension.values = list(year = 2000:2030)) 

# parameters included in each calibration 
params.for.pop = c(par.names.pop,par.names.basic.trans)
params.for.trans = par.names.transmission
params.for.full = EHE.PARAMETERS.PRIOR@var.names

# Visualization of parameters: 

#------- POP PARAMS -------#
#--(A)--#
        #-------- TRANS PARAMS --------------#
                             #------(B)------#
#------------------------------ FULL PARAMS ---------------------------#
#-(C.1)-#                                    #----------(C.2)----------#
                                                                 #-(D)-#   


# (A) parameters in pop but not trans 
params.pop.only = setdiff(params.for.pop,params.for.trans) # setdiff: in the first but not the second 

# (B) parameters in trans but not pop 
params.added.in.trans = setdiff(params.for.trans,params.for.pop)

# (C.1 + C.2) parameters in full but not trans  
params.added.in.full = setdiff(params.for.full,params.for.trans)

# (C.2 only) parameters in full but not trans or pop
params.added.in.full.non.pop = setdiff(params.added.in.full,params.for.pop)

testing.related = EHE.PARAMETERS.PRIOR@var.names[grepl("test",EHE.PARAMETERS.PRIOR@var.names)]

# (D) testing-related parameters in full and not trans or pop 
params.added.in.full.non.pop.testing = intersect(testing.related,params.added.in.full.non.pop)

round(cbind(params.trans.last[params.added.in.full.non.pop.testing],
            params.full.last[params.added.in.full.non.pop.testing]),4)


