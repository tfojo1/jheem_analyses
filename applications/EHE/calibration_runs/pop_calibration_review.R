source('../jheem_analyses/applications/EHE/ehe_specification.R')


load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-04-02_C.12060.Rdata")
sim.atlanta = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-04-02_C.33100.Rdata")
sim.miami = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-04-02_C.37980.Rdata")
sim.philadelphia = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-04-02_C.42660.Rdata")
sim.seattle = sim

rm(sim)

simplot(sim.atlanta,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Atlanta")

simplot(sim.miami,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Miami")

simplot(sim.philadelphia, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Philadelphia")

simplot(sim.seattle, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Seattle")


# MIGRATION PLOTS # 
simplot(sim.atlanta,
        #facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Atlanta")

simplot(sim.miami,
        #facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Miami")

simplot(sim.philadelphia,
        #facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Philadelphia")

simplot(sim.seattle,
        #facet.by = "age", split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) + ggtitle("Seattle")
