##Fixing NSDUH Regions for Younger Age Groups##

################################################################################
#Starting with 13-17
################################################################################
states = locations::get.all.for.type("state") #start processing of figuring out which counties are in which regions
counties.in.states = locations::get.contained.locations(states, "COUNTY", return.list = T)
names(counties.in.states)=states #converted it from a list to a named list

xx = sapply(counties.in.states, function(x){!(all(is.na(x)))}) #removing states that do not have an associated county
counties.in.states = counties.in.states[xx] 

xy = census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex #going to aggregate this into year, location, age (bc we don't need race/eth)
xyz = apply(xy, c('year', 'location', 'age'), function(x){sum(x)}) #using applying bc xy is an array; function tells us how to aggregate the data.  Use dim(xyz) to check 

all.states.younger.populations =lapply(counties.in.states, function(my.counties){
  my.county.young.population = xyz[, my.counties, ages.of.interest.13.17]  #I cannot figure out why this is giving subscript out of bounds bc it has 3 dimensions
  state.younger.population = apply(my.county.young.population, 'year', function(x){sum(x)})
})

names(all.states.younger.populations) = states #need to give this names of what the substate region is


#Set up into data frame for pivoting
younger.age.by.state.df= data.frame(all.states.younger.populations)

younger.age.by.state.df$year <- row.names(younger.age.by.state.df)
rownames(younger.age.by.state.df) <- NULL

younger.age.by.state.df <- younger.age.by.state.df %>%
  pivot_longer(cols = -c("year"), 
               names_to= c("location"), 
               #names_pattern = ".", 
               values_to = "value")

#reformat to match nsduh data for join
younger.age.by.state.df <- younger.age.by.state.df%>%
  mutate(age.group = "13 - 17 years")%>%
  rename(value.13.17 = value)%>%
  rename(year.original = year)%>%
  filter(year.original  != "2006" & year.original  != "2008" & year.original  != "2010" & year.original != "2012" & year.original  != "2014" & year.original  != "2016")%>%
  mutate(year = case_when(year.original  == "2005" ~ "2004-2006",
                          year.original  == "2007" ~"2006-2008",
                          year.original  == "2009" ~"2008-2010",
                          year.original  == "2011" ~"2010-2012",
                          year.original  == "2013" ~"2012-2014",
                          year.original  == "2015" ~"2014-2016",
                          year.original  == "2017" ~"2016-2018"))
