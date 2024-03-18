print("sourcing files")
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

library(stringr)

files = list.files("../jheem_analyses/prelim_results/")
files = files[grepl("init.transmission.sim_2024-02-01",files)]
locations = str_sub(files,-13,-7)
names(locations) = names(LOCATIONS.OF.INTEREST[match(locations,unname(LOCATIONS.OF.INTEREST))])


print("starting for loop")
for(i in 1:length(locations)){
  
  location = locations[i]
  print(paste0("loading file for location ",location))
  file = files[i]
  load(paste0("../jheem_analyses/prelim_results/",file))
  
  plot.total = simplot(sim,
                       outcomes = c("new","diagnosed.prevalence"),
                       dimension.values = list(year = 2000:2020)) + 
    ggtitle(names(location)) +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  plot.risk = simplot(sim,
                      facet.by = "risk",
                      outcomes = c("new","diagnosed.prevalence"),
                      dimension.values = list(year = 2000:2020)) + 
    ggtitle(names(location)) +
    theme(plot.title = element_text(hjust = 0.5,size = 25))
  
  ggsave(filename = 
           paste0("prelim_results/2024_02_01_figs_transmission/",names(location),"_total.jpeg"),
         plot.total,width = 10,height = 7,dpi = 350)
  
  ggsave(filename = 
           paste0("prelim_results/2024_02_01_figs_transmission/",names(location),"_risk.jpeg"),
         plot.risk,width = 10,height = 7,dpi = 350)
  
}

