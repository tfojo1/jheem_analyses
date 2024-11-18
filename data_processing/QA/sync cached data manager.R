
library(jheem2)

source('commoncode/cache_manager.R')

#Push the data_manager_cache_metadata.Rdata once you've updated it with the sync function so that everyone else will have it


#Zoe's_new_data_manager = copy.data.manager("Zoe's_old_data_manager", name="the_same_thing_probably", description="also_probably_the_same_as_the_old_description"
#you can check the dates it made with get.data.manager.cache.metadata() first



# SURVEILLANCE MANAGER ----------------------------------------------------

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")

#You have to change the link everytime
sync.cached.data.manager("surveillance.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EQS1u_TFlkpCrsv5vhMlufoB7UmfiE2TYW04-CXSdAG4Xw?e=4rzT2e",
                         allow.flag = T)
get.data.manager.cache.metadata()



# CENSUS MANAGER ----------------------------------------------------------

census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

#You have to change the link everytime
sync.cached.data.manager("census.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EajriZ0H1v9Nrh6yTjt177ABl43ytsqv9mfgRVLX2aDPBA?e=rGjTWR",
                         allow.flag = T)

get.data.manager.cache.metadata()


# SYPHILIS MANAGER --------------------------------------------------------

syphilis.manager = load.data.manager(name="syphilis.manager", file="../../cached/syphilis.manager.rdata")

#UPDATE LINK EACH TIME
sync.cached.data.manager("syphilis.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/Ea6-pTjU9VZOqh4BFkEZjcEBr092GiFiU2KmBk5D3b6AvA?e=SqWoHP",
                         allow.flag = T)

get.data.manager.cache.metadata()
