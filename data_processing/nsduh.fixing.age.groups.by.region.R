##Fixing NSDUH Regions for Younger Age Groups##

################################################################################
#Starting with 13-17
################################################################################
substate.regions = locations::get.all.for.type("NSDUH") #start processing of figuring out which counties are in which regions
counties.in.substate.regions = locations::get.contained.locations(substate.regions, "COUNTY", return.list = T)
names(counties.in.substate.regions)=substate.regions #converted it from a list to a named list

xx = sapply(counties.in.substate.regions, function(x){!(all(is.na(x)))}) #shows which substate regions do not have an associated county
counties.in.substate.regions = counties.in.substate.regions[xx] #use vector created above to tell how to subset list; R keeps the ones that say true in the vector and removes those that are false

substate.regions = substate.regions[xx] #good practice to subset this the same way for consistency

xy = census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex #going to aggregate this into year, location, age (bc we don't need race/eth)
xyz = apply(xy, c('year', 'location', 'age'), function(x){sum(x)}) #using applying bc xy is an array; function tells us how to aggregate the data.  Use dim(xyz) to check 

all.regions.younger.populations =lapply(counties.in.substate.regions, function(my.counties){
  my.county.young.population = xyz[ , my.counties, ages.of.interest.13.17] #subsetting for what we want
  region.younger.population = apply(my.county.young.population, 'year', function(x){sum(x)})#now need to aggregate the ages; margin is what you want  
})

names(all.regions.younger.populations) = substate.regions #need to give this names of what the substate region is


#Set up into data frame for pivoting
younger.age.by.region.df= data.frame(all.regions.younger.populations)

younger.age.by.region.df$year <- row.names(younger.age.by.region.df)
rownames(younger.age.by.region.df) <- NULL

younger.age.by.region.df <- younger.age.by.region.df %>%
  pivot_longer(cols = -c("year"), 
               names_to= c("location"), 
               #names_pattern = ".", 
               values_to = "value")

#reformat to match nsduh data for join
younger.age.by.region.df <- younger.age.by.region.df%>%
  mutate(age.group = "13 - 17 years")%>%
  rename(population.13.17 = value)%>%
  rename(year.original = year)%>%
  filter(year.original  != "2006" & year.original  != "2008" & year.original  != "2010" & year.original != "2012" & year.original  != "2014" & year.original  != "2016")%>%
  mutate(year = case_when(year.original  == "2005" ~ "2004-2006",
                          year.original  == "2007" ~"2006-2008",
                          year.original  == "2009" ~"2008-2010",
                          year.original  == "2011" ~"2010-2012",
                          year.original  == "2013" ~"2012-2014",
                          year.original  == "2015" ~"2014-2016",
                          year.original  == "2017" ~"2016-2018"))


#save- bc you are pulling this from census manager you can just read it in for nsduh processing
#save(younger.age.by.region.df , file="C:/Users/zthomps5/Desktop/current/younger.age.by.region.df.RData")


#This will be the process for the nsduh code to join
# zoe.attempt = lapply(data.list.nsduh.region.clean, function(file){
#   
#   data=file[[2]]
#   filename = file[[1]]
#   
#   data =left_join(data, younger.age.by.region.df, by=join_by("location", "year"), relationship = "many-to-many")
# 
#   data= as.data.frame(data)
#   
#   list(filename, data)
# })


################################################################################
#Now do the same things for 18-25
################################################################################
all.regions.older.populations =lapply(counties.in.substate.regions, function(my.counties){
  my.county.older.population = xyz[ , my.counties, ages.of.interest.18.25] #subsetting for what we want
  region.older.population = apply(my.county.older.population, 'year', function(x){sum(x)})#now need to aggregate the ages; margin is what you want  
})

names(all.regions.older.populations) = substate.regions #need to give this names of what the substate region is

#Set up into data frame for pivoting
older.age.by.region.df= data.frame(all.regions.older.populations)

older.age.by.region.df$year <- row.names(older.age.by.region.df)
rownames(older.age.by.region.df) <- NULL

older.age.by.region.df <- older.age.by.region.df %>%
  pivot_longer(cols = -c("year"), 
               names_to= c("location"), 
               #names_pattern = ".", 
               values_to = "value")

#reformat to match nsduh data for join
older.age.by.region.df <- older.age.by.region.df%>%
  mutate(age.group = "18-25 years")%>%
  rename(year.original = year)%>%
  rename(population.18.25 = value)%>%
  filter(year.original  != "2006" & year.original  != "2008" & year.original  != "2010" & year.original != "2012" & year.original  != "2014" & year.original  != "2016")%>%
  mutate(year = case_when(year.original  == "2005" ~ "2004-2006",
                          year.original  == "2007" ~"2006-2008",
                          year.original  == "2009" ~"2008-2010",
                          year.original  == "2011" ~"2010-2012",
                          year.original  == "2013" ~"2012-2014",
                          year.original  == "2015" ~"2014-2016",
                          year.original  == "2017" ~"2016-2018"))

################################################################################
#Combine and save
################################################################################

age_regroup_substate_region = full_join(younger.age.by.region.df, older.age.by.region.df, by=join_by("location", "year")) 

age_regroup_substate_region <- age_regroup_substate_region %>%
  select(-year.original.x)%>%
  rename(year.original = year.original.y)%>%
  select(-age.group.x, -age.group.y)

save(age_regroup_substate_region , file="C:/Users/zthomps5/Desktop/current/age_regroup_substate_region.RData")
