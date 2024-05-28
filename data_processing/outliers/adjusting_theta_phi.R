#Adjust values of theta and phi for each outcome to determine best set up for finding true outliers

#outcomes = diagnosed.prevalence; diagnoses; hiv.deaths
#stratification = total
#location = EHE.Counties

#phi: a percent change from one year to another (default value is 0.15)
#theta: a multiplier that produces a percent change based on how many years apart the two samples are (it has a maximum value) (default value is 0.05)

#True = it is an outlier
# False = the data point is fine as is


# Source outlier finder ---------------------------------------------------

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')

#will Need to fix these after talking to Andrew
national = "US"
states = c(state.abb)
MSAs = MSAS.OF.INTEREST
counties = unlist(COUNTIES.OF.INTEREST) 
substate.region = locations::get.all.for.type("NSDUH")
  
# diagnoses ---------------------------------------------------------------

diagnoses.adjusted <- run.outlier.process(outcome= 'diagnoses',
                                              stratifications= list(c()), #but there's more strata
                                              data.manager= surveillance.manager,
                                              phi = 0.3,
                                              theta = 0.1,
                                              locations= c(national, states, counties, MSAs))

#Use diagnoses.adjusted; removing 2 outliers (DC + Baltimore)
# diagnoses.adjusted$adjudication <- c('TRUE', 'TRUE')
# 
# run.outlier.process(outcome= 'diagnoses',
#                     stratifications= list(c()),
#                     data.manager= surveillance.manager,
#                     locations= EHE.MSAS,
#                     adjudication.data.frame = diagnoses.adjusted)


# diagnosed.prevalence ----------------------------------------------------

diagnosed.prevalence.adjusted <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.6, #this just seems like a huge value
                                          theta = 0.4,
                                          locations= c(national, states, counties, MSAs))

#using the adjusted dataframe- this removes 10 values
# diagnosed.prevalence.adjusted$adjudication <- replace(diagnosed.prevalence.adjusted$adjudication, 1:10, 'TRUE')
# 
# run.outlier.process(outcome= 'diagnosed.prevalence',
#                     stratifications= list(c()),
#                     data.manager= surveillance.manager,
#                     locations= EHE.MSAS,
#                     adjudication.data.frame = diagnosed.prevalence.adjusted)


# hiv.deaths --------------------------------------------------------------
hiv.deaths.adjusted <- run.outlier.process(outcome= 'hiv.deaths',
                                                     stratifications= list(c()), 
                                                     data.manager= surveillance.manager,
                                                     phi = 0.3,
                                                     theta = 0.1,
                                                     locations= c(states, MSAs))

#This is another example- hiv.deaths 'sex'
hiv.deaths.adjusted <- run.outlier.process(outcome= 'hiv.deaths',
                                           stratifications= list(c('sex')), 
                                           data.manager= surveillance.manager,
                                           phi = 0.3,
                                           theta = 0.1,
                                           locations= c(states, MSAs))


# Gonorrhea -----------------------------------------------------------------

#This is an example - i started with a lot of GC outliers, adjusted values to get 1 at total level, but then at strata that becomes 110 for race, 18 for age
#Like how do I do this without making the values absurd?

gonorrhea.adjusted <- run.outlier.process(outcome= 'gonorrhea',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           phi = 0.5,
                                           theta = 0.1,
                                           locations= c(states))

#EXAMPLE: This identifies every year but the reference year and covid as an outlier
#which maybe that's fine?
#but then as I correct these it's going to ripple out again
gonorrhea.adjusted.stratified <- run.outlier.process(outcome= 'gonorrhea',
                                          stratifications= list(c('race')), 
                                          data.manager= surveillance.manager,
                                          phi = 0.5,
                                          theta = 0.1,
                                          locations= c(states))

gc.all.data = as.data.frame.table(surveillance.manager$data$gonorrhea$estimate$cdc.sti$cdc.sti$year__location__race)


plot.one <- gc.all.data %>% filter(location == "AK")%>% filter(race == 'Multiracial')
ggplot(data = plot.one)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)


