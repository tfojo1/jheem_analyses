source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library(stringr)

files = list.files("../jheem_analyses/prelim_results/")
files.new = files[grepl("init.pop.migration.sim_2024-03-19",files)]
locations = str_sub(files.new,-13,-7)
names(locations) = names(LOCATIONS.OF.INTEREST[match(locations,unname(LOCATIONS.OF.INTEREST))])

files.old = files[grepl("init.pop.migration.sim_2024-03-13",files)]

print("starting for loop")
for(i in 1:length(locations)){
  
  location = locations[i]
  print(paste0("loading files for location ",location))
  file.new = files.new[i]
  file.old = files.old[grepl(location,files.old)]
  
  load(paste0("../jheem_analyses/prelim_results/",file.new))
  sim.new = sim
  
  load(paste0("../jheem_analyses/prelim_results/",file.old))
  sim.old = sim

    plot = simplot(sim.old, sim.new, "population",
                 facet.by = "age",split.by = "race",
                 dimension.values = list(year = as.character(2000:2030))) + 
    ggtitle(names(location)) +
    theme(plot.title = element_text(hjust = 0.5,size = 25))

  ggsave(filename = 
           paste0("prelim_results/2024_03_19_figs/",names(location),"_comparison.jpeg"),
         plot,width = 10,height = 7,dpi = 350)
  
}
