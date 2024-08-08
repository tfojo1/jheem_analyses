source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load('../jheem_analyses/prelim_results/base.5_simset_2024-08-02_C.12580.Rdata')
simset.base.5 = simset
sim.base.5.last = simset.base.5$last.sim()
sim.base.5.first = simset.base.5$first.sim()

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-02_C.12580.Rdata')
simset.full.with.aids = simset
sim.full.with.aids = simset.full.with.aids$last.sim()

load('../jheem_analyses/prelim_results/minus.pos_simset_2024-08-02_C.12580.Rdata')
simset.minus.pos = simset
sim.minus.pos = simset.minus.pos$last.sim()

load('../jheem_analyses/prelim_results/minus.tst_simset_2024-08-02_C.12580.Rdata')
simset.minus.tst = simset
sim.minus.tst = simset.minus.tst$last.sim()

load('../jheem_analyses/prelim_results/minus.aids_simset_2024-08-02_C.12580.Rdata')
simset.minus.aids = simset
sim.minus.aids = simset.minus.aids$last.sim()

full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')
exp(full.lik$compute.piecewise(sim.base.5.last) - full.lik$compute.piecewise(sim.base.5.first))

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
sim = engine$run(sim.base.5.last$params)


simplot(sim.base.5,
        #sim.full.with.aids,
        #sim.minus.pos,
        #sim.minus.tst,
        #sim.minus.aids,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.base.5,
        #sim.full.with.aids,
        #sim.minus.pos,
        #sim.minus.tst,
        #sim.minus.aids,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.base.5.first,
        sim.base.5.last,
        #sim.full.with.aids,
        #sim.minus.pos,
        #sim.minus.tst,
        #sim.minus.aids,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        #style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.base.5,
        #sim.full.with.aids,
        #sim.minus.pos,
        #sim.minus.tst,
        #sim.minus.aids,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
