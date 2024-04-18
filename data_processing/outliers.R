
# Set up  -----------------------------------------------------------------
options(error=NULL)

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')

states = c(state.abb)
counties = locations::get.all.for.type("COUNTY")#do you want all counties? 

# Call outlier function ---------------------------------------------------

#Note from 4-17-24: We are going to start examining outliers at the total level
#first for outcomes diagnoses, prevalence, and hiv.mortality.  Todd said to 
#manually check 12-24 obs.  Then eventually we will move to the more 
#stratified data.  This will not work for proportion outcomes or on counts with
#n values of under 50.

hiv.deaths.outliers <- run.outlier.process(outcome= 'hiv.deaths',
                                  stratifications= list(c()), #this indicates total level
                                  data.manager= surveillance.manager,
                                  locations= c(MSAS.OF.INTEREST, states)) #28 outliers

#it's confusing what locations to pick because the locations available really
#depend on the source. But you can't differentiate the source
diagnoses.outliers <- run.outlier.process(outcome= 'diagnoses',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           locations= c(MSAS.OF.INTEREST, states, counties))

diagnosed.prevalence.outliers <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          locations= c(MSAS.OF.INTEREST, states, counties)) 


# This is how you will put back in the adjudicated data frame -------------

# run.outlier.process(outcome= 'diagnosed.prevalence',
#                     stratifications= list(c()),
#                     data.manager= surveillance.manager,
#                     locations= MSAS.OF.INTEREST,
#                     adjudication.data.frame = fixed.outliers)


