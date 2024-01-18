
source('../jheem_analyses/source_code.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(stringr)

files = list.files("../jheem_analyses/prelim_results/")
files = files[grepl("init.pop.migration.sim_",files)]
locations.run = str_sub(files,-13,-7)
names(locations.run) = names(LOCATIONS.OF.INTEREST[match(locations.run,unname(LOCATIONS.OF.INTEREST))])

for(i in 1:length(locations.run)){
  
  location = locations.run[i]
  file = files[i]
  load(paste0("../jheem_analyses/prelim_results/",file))
  
  plot = simplot(sim, "population",
                 facet.by = "age",split.by = "race",
                 dimension.values = list(year = as.character(2000:2020))) + 
    ggtitle(names(location)) +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  ggsave(filename = 
           paste0("prelim_results/2024_01_15_figs/",names(location),".jpeg"),
         plot,width = 10,height = 7,dpi = 350)

}



# simplot(sim.pop.only, sim.migration, "population",
#         facet.by = "age", split.by = "race",
#         dimension.values = list(year = as.character(2000:2020)))
# 
# simplot(sim.pop.only, sim.migration, "immigration",
#         split.by = "race",
#         dimension.values = list(year = as.character(2000:2020)))
# 
# simplot(sim.pop.only, sim.migration, "emigration",
#         split.by = "race",
#         dimension.values = list(year = as.character(2000:2020)))
# cbind(par.pop.only,par.migration)
# simplot(sim, 'population')
simplot(sim, "population",facet.by = "age",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
sim$parameters[par.names,]
# 


jpeg(file=paste0("prelim_results/age.race_",sim$location,".jpeg"), width = 2500,height = 1500,res=300)
simplot(sim.migration, "population",facet.by = "age",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
dev.off()
