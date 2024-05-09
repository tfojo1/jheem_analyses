#May 9th- Adjust values of theta and phi for each outcome to determine best set up for finding true outliers

#outcomes = diagnosed.prevalence; diagnoses; hiv.deaths
#stratification = total
#location = EHE.Counties

#phi: a percent change from one year to another (default value is 0.15)
#theta: a multiplier that produces a percent change based on how many years apart the two samples are (it has a maximum value) (default value is 0.05)

#Questions: how do I plan this across sources, strata, locations?
#Do I need to review tens of obs?

#True = it is an outlier
# False = the data point is fine as is


# diagnoses ---------------------------------------------------------------

diagnoses.original <- run.outlier.process(outcome= 'diagnoses',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          locations= c(EHE.MSAS))

diagnoses.adjusted <- run.outlier.process(outcome= 'diagnoses',
                                              stratifications= list(c()), 
                                              data.manager= surveillance.manager,
                                              phi = 0.3,
                                              theta = 0.1,
                                              locations= c(EHE.MSAS))

#Use diagnoses.adjusted; removing 2 outliers (DC + Baltimore)
diagnoses.adjusted$adjudication <- c('TRUE', 'TRUE')

run.outlier.process(outcome= 'diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    locations= EHE.MSAS,
                    adjudication.data.frame = diagnoses.adjusted)


# diagnosed.prevalence ----------------------------------------------------


diagnosed.prevalence.original <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                    stratifications= list(c()), 
                                    data.manager= surveillance.manager,
                                    locations= c(EHE.MSAS))

diagnosed.prevalence.adjusted <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.2,
                                          theta = 0.05,
                                          locations= c(EHE.MSAS))

#using the adjusted dataframe- this removes 10 values
diagnosed.prevalence.adjusted$adjudication <- replace(diagnosed.prevalence.adjusted$adjudication, 1:10, 'TRUE')

run.outlier.process(outcome= 'diagnosed.prevalence',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    locations= EHE.MSAS,
                    adjudication.data.frame = diagnosed.prevalence.adjusted)


# hiv.deaths --------------------------------------------------------------
#I don't know how to assess these- it's like there's data for 2019 and 20212
hiv.deaths.original <- run.outlier.process(outcome= 'hiv.deaths',
                                                     stratifications= list(c()), 
                                                     data.manager= surveillance.manager,
                                                     locations= c(EHE.MSAS))

hiv.deaths.adjusted <- run.outlier.process(outcome= 'hiv.deaths',
                                                     stratifications= list(c()), 
                                                     data.manager= surveillance.manager,
                                                     phi = 0.2,
                                                     theta = 0.05,
                                                     locations= c(EHE.MSAS))


#Adding this to compare to other hiv death data:
# hiv.deaths.to.graph.one = as.data.frame.table(surveillance.manager$data$hiv.deaths$estimate$cdc.hiv$cdc$year__location)
# hiv.deaths.to.graph.two = as.data.frame.table(surveillance.manager$data$hiv.deaths$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex)
# 
# 
# new <- hiv.deaths.to.graph.two%>%
#   group_by(year, location)%>%
#   mutate(sum = sum(Freq))%>%
#   select(-sex, -Freq)%>%
#   rename(Freq= sum)
# 
# combo = rbind(new, hiv.deaths.to.graph.one)
# 
# 
# graph <- combo %>% filter(location == "C.29820")
# ggplot(data = graph)+                   
#   geom_point(                             
#     mapping = aes(x = year, y = Freq),    
#     color = "purple")+                       
#   labs()+                                 
#   theme()+
  ylim(0, NA)
