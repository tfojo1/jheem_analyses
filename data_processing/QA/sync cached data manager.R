
library(jheem2)

source('commoncode/cache_manager.R')

#Push the data_manager_cache_metadata.Rdata once you've updated it with the sync function so that everyone else will have it


#Zoe's_new_data_manager = copy.data.manager("Zoe's_old_data_manager", name="the_same_thing_probably", description="also_probably_the_same_as_the_old_description"
#you can check the dates it made with get.data.manager.cache.metadata() first



# SURVEILLANCE MANAGER ----------------------------------------------------

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")

sync.cached.data.manager("surveillance.manager.rdata",
                         allow.flag = T)
get.data.manager.cache.metadata("surveillance.manager.rdata")



# CENSUS MANAGER ----------------------------------------------------------

census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

sync.cached.data.manager("census.manager.rdata",
                         allow.flag = T)

get.data.manager.cache.metadata("census.manager.rdata")


# SYPHILIS MANAGER --------------------------------------------------------

syphilis.manager = load.data.manager(name="syphilis.manager", file="../../cached/syphilis.manager.rdata")

sync.cached.data.manager("syphilis.manager.rdata",
                         allow.flag = T)

get.data.manager.cache.metadata("syphilis.manager.rdata")
