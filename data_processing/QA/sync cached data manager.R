
library(jheem2)

source('commoncode/cache_manager.R')

#Push the data_manager_cache_metadata.Rdata once you've updated it with the sync function so that everyone else will have it


#Zoe's_new_data_manager = copy.data.manager("Zoe's_old_data_manager", name="the_same_thing_probably", description="also_probably_the_same_as_the_old_description"
#you can check the dates it made with get.data.manager.cache.metadata() first



# SURVEILLANCE MANAGER ----------------------------------------------------

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")

surveillance.manager = load.data.manager(name="surveillance.manager", file="C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/First Week/Data Manager Archives/surveillance.manager_10.24.24.rdata")


#You have to change the link everytime
sync.cached.data.manager("surveillance.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EfzhTwGkOwFEmdzwTI1zOksBbkmkx7j6axHy4_kuK9bOzQ?e=vyxOVm",
                         allow.flag = T)
get.data.manager.cache.metadata()



# CENSUS MANAGER ----------------------------------------------------------

census.manager = load.data.manager(name="census.manager", file="Q:/data_managers/census.manager.rdata")

#You have to change the link everytime
sync.cached.data.manager("census.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EajriZ0H1v9Nrh6yTjt177ABl43ytsqv9mfgRVLX2aDPBA?e=rGjTWR",
                         allow.flag = T)

get.data.manager.cache.metadata()


# SYPHILIS MANAGER --------------------------------------------------------

syphilis.manager = load.data.manager(name="syphilis.manager", file="Q:/data_managers/syphilis.manager.rdata")

#UPDATE LINK EACH TIME
sync.cached.data.manager("syphilis.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/IQCuvqU41PVWTqoeARZBGY3BAa9PdhohYlNipgZOQ92-gLw?e=rMybjs",
                         allow.flag = T)

get.data.manager.cache.metadata()

# ryan.white MANAGER --------------------------------------------------------

ryan.white.data.manager = load.data.manager("Q:/data_managers/ryan.white.data.manager.rdata")

#UPDATE LINK EACH TIME
sync.cached.data.manager("ryan.white.data.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EUO4ZLEl6MVPrwq6FcDWyDcB3rNqnAAr_L3Nu3lpAa0T9Q?e=bQ1HkK",
                         allow.flag = T)

get.data.manager.cache.metadata()


# ryan white webtool data manager -----------------------------------------


sync.cached.data.manager("ryan.white.web.data.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EcCBfRFYvqZIhJCzNDtsOB8B8Ew48XRToSBmfb4SfW1ZBg?e=Gexrn9",
                         allow.flag = T)

get.data.manager.cache.metadata()

#medicaid data manager -----------------------------------------


sync.cached.data.manager("medicaid.data.manager.rdata",
                         "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EV5Cu3Imh1tGhswKWLwlvQEBQMYolFTH9IjAtxdrdwOfPg?e=TvYvHj",
                         allow.flag = T)

get.data.manager.cache.metadata()