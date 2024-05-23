
# Set up ------------------------------------------------------------------

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')


# Example 1: HIV Deaths ---------------------------------------------------
##First I adjusted the values here to get to 3 outlier for deaths:
hiv.deaths.adjusted <- run.outlier.process(outcome= 'hiv.deaths',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           phi = 0.3,
                                           theta = 0.1,
                                           locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")))

#Then applied same values across strata- one example is 'sex' where there are 110 outliers
#These are mostly within surveillance reports (the other outliers are mainly within cdc.hiv)
hiv.deaths.adjusted.stratified <- run.outlier.process(outcome= 'hiv.deaths',
                                           stratifications= list(c('sex')), 
                                           data.manager= surveillance.manager,
                                           phi = 0.3,
                                           theta = 0.1,
                                           locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")))

#Are these 110 sex outliers really outliers?
#This needs to be mapped with more recent data bc otherwise it's hard to tell
#My concern is that after getting through lots of iterations as we correct them we'll have hundreds more

hiv.deaths.by.sex = as.data.frame.table(surveillance.manager$data$hiv.deaths$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex)

plot.one <- hiv.deaths.by.sex %>% filter(location == "C.41700")%>% filter(sex == 'male')
ggplot(data = plot.one)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)

#This does seem like outlier
plot.one <- hiv.deaths.by.sex %>% filter(location == "C.16980")%>% filter(sex == 'female')
ggplot(data = plot.one)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)

# Example 2: Gonorrhea ----------------------------------------------------

#One gc outlier with these values:
#But then for race or age you end up with 110 or 18 outliers
gonorrhea.adjusted <- run.outlier.process(outcome= 'gonorrhea',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.5,
                                          theta = 0.1,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")))

gonorrhea.adjusted.stratified <- run.outlier.process(outcome= 'gonorrhea',
                                                     stratifications= list(c('race')), 
                                                     data.manager= surveillance.manager,
                                                     phi = 0.5,
                                                     theta = 0.1,
                                                     locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")))

gc.all.data = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location__race)

#EXAMPLE: This identifies every year but the reference year and covid as an outlier
#which maybe that's fine?
#but then as I correct these it's going to ripple out again
plot.one <- gc.all.data %>% filter(location == "AK")%>% filter(race == 'Multiracial')
ggplot(data = plot.one)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)
