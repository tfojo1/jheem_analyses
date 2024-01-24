print("sourcing files")
source('../jheem_analyses/source_code.R')
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library(stringr)

files = list.files("../jheem_analyses/prelim_results/")
files.old.1 = files[grepl("init.pop.migration.sim_2024-01-12",files)]
files.old.2 = files[grepl("init.pop.migration.sim_2024-01-13",files)]
files.old.3 = files[grepl("init.pop.migration.sim_2024-01-14",files)]
files.old.4 = files[grepl("init.pop.migration.sim_2024-01-15",files)]
files.old = c(files.old.1,files.old.2,files.old.3,files.old.4)

files.1 = files[grepl("init.pop.migration.sim_2024-01-22",files)]
files.2 = files[grepl("init.pop.migration.sim_2024-01-23",files)]
# files.1 = files[grepl("init.pop.migration.sim_2024-01-19",files)]
# files.2 = files[grepl("init.pop.migration.sim_2024-01-20",files)]
files = c(files.1,files.2)
locations.run = str_sub(files,-13,-7)
names(locations.run) = names(LOCATIONS.OF.INTEREST[match(locations.run,unname(LOCATIONS.OF.INTEREST))])

print("starting for loop")
for(i in 1:length(locations.run)){
  
  location = locations.run[i]
  print(paste0("loading files for location ",location))
  new.file = files[i]
  old.file = files.old[grepl(location,files.old)]
  load(paste0("../jheem_analyses/prelim_results/",new.file))
  sim.new = sim
  
  load(paste0("../jheem_analyses/prelim_results/",old.file))
  sim.old = sim
  
  lik = joint.pop.migration.likelihood.instructions$instantiate.likelihood('ehe', location = location,
                                                                           data.manager = SURVEILLANCE.MANAGER)
  likelihood = exp(lik$compute(sim.new,check.consistency=F) - lik$compute(sim.old,check.consistency=F)) 
  
  plot = simplot(sim.old, sim.new, "population",
                 facet.by = "age",split.by = "race",
                 dimension.values = list(year = as.character(2000:2020))) + 
    labs(title = names(location), 
            subtitle = paste0("Likelihood, new (dashed) - old (solid) = ",likelihood)) + 
    theme(plot.title = element_text(hjust = 0.5,size = 25),plot.subtitle = element_text(hjust = 0.5))
  
  ggsave(filename = 
           paste0("prelim_results/2024_01_23_figs/",names(location),".jpeg"),
         plot,width = 10,height = 7,dpi = 350)
  
}

# old for loop
if(1==2){
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
simplot(sim, "population",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, "immigration",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
sim$parameters[par.names,]
# # 
# 
# 
# jpeg(file=paste0("prelim_results/age.race_",sim$location,".jpeg"), width = 2500,height = 1500,res=300)
# simplot(sim.migration, "population",facet.by = "age",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
# dev.off()
