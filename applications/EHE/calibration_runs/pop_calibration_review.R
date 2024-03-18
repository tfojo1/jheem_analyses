source('../jheem_analyses/applications/EHE/ehe_specification.R')

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-07_C.12580.Rdata")
sim.baltimore.1 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-08_C.12580.Rdata")
sim.baltimore.2 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-09_C.12580.Rdata")
sim.baltimore.3 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-12_C.12580.Rdata")
sim.baltimore.4 = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-08_C.12060.Rdata")
sim.atlanta.old = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-09_C.12060.Rdata")
sim.atlanta.new = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-08_C.26420.Rdata")
sim.houston.old = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-09_C.26420.Rdata")
sim.houston.new = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-08_C.33100.Rdata")
sim.miami.old = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-09_C.33100.Rdata")
sim.miami.new = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-07_C.42660.Rdata")
sim.seattle.1 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-08_C.42660.Rdata")
sim.seattle.2 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-09_C.42660.Rdata")
sim.seattle.3 = sim
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-12_C.42660.Rdata")
sim.seattle.4 = sim

rm(sim)

# 3/9: 35-44 was worse
simplot(#sim.baltimore.1, # old better (3/7)
        #sim.baltimore.2, # old worse (3/8)
        sim.baltimore.3, # (3/9)
        sim.baltimore.4, # (3/12)
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Baltimore")

# maybe slightly less bad? but still bad
simplot(sim.atlanta.old,
        sim.atlanta.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Atlanta")

# not much qualitative difference
simplot(sim.houston.old,
        sim.houston.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Houston")

# not much qualitative difference
simplot(sim.miami.old,
        sim.miami.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Miami")

# 3/9 still bad; 35-44 got worse
simplot(#sim.seattle.1, # old better (3/7)
        #sim.seattle.2, # old worse (3/8)
        sim.seattle.3, # (3/9)
        sim.seattle.4, # (3/12)
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Seattle")


# MIGRATION PLOTS # 
# at least 35-44 now has the same trend as 25-34 and 45-54
simplot(sim.baltimore.3,
        sim.baltimore.4,
        facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Baltimore")

# no real difference
simplot(sim.atlanta.old,
        sim.atlanta.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Atlanta")

# no real difference
simplot(sim.houston.old,
        sim.houston.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Houston")

# no real difference
simplot(sim.miami.old,
        sim.miami.new,
        facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Miami")

# 35-44 now has same trend as others
simplot(sim.seattle.3,
        sim.seattle.4,
        #facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Seattle")
