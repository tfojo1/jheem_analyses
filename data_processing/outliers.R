
# Set up  -----------------------------------------------------------------
options(error=NULL)

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')

states = c(state.abb)
counties = locations::get.all.for.type("COUNTY")#do you want all counties? 


# Re-do 4-29-24 EHE Only -----------------------------------------
diagnoses.outliers.ehe <- run.outlier.process(outcome= 'diagnoses',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          locations= c(EHE.MSAS))

#write.csv(diagnoses.outliers.ehe, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/outliers/outliers.ehe.counties/diagnoses.outliers.ehe.csv")

all.diagnoses = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location) #only for source = cdc.hiv

#1
dx.1 <- all.diagnoses %>% filter(location == "C.35620")
ggplot(data = dx.1)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()
#2
dx.2 <- all.diagnoses %>% filter(location == "C.33100")
ggplot(data = dx.2)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme() 

#3
dx.3 <- all.diagnoses %>% filter(location == "C.12420")
ggplot(data = dx.3)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme() 

all.diagnoses.two = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc$year__location) #only for source = cdc.hiv

#4
dx.4 <- all.diagnoses.two %>% filter(location == "C.33100")
ggplot(data = dx.4)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme() 

#5
dx.5 <- all.diagnoses.two %>% filter(location == "C.12060")
ggplot(data = dx.5)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

#6
dx.6 <- all.diagnoses.two %>% filter(location == "C.12580")
ggplot(data = dx.6)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

#Examples to review in meeting:
  #Outliers are 2008, 2013, 2014 (if 2008 is an outlier is 2015 also an outlier; is this an unreal fluctuation)
ex.1 <- all.diagnoses.two %>% filter(location == "C.12060")
ggplot(data = ex.1)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

  #outliers are 2008 and 2009 (should it actually be 2008 and 2010?)
ex.2 <- all.diagnoses.two %>% filter(location == "C.16980")
ggplot(data = ex.2)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

  #outliers are 2008, 2010, 2011, 2012, 2013 (I think 2008 is clearly an outlier but the others I dont feel certain about)
ex.3 <- all.diagnoses.two %>% filter(location == "C.47900")
ggplot(data = ex.3)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

  #oulier is 2011, but then should 2008-2010 also be outliers? But that could be where the amendement comes in
ex.4 <- all.diagnoses.two %>% filter(location == "C.40140")
ggplot(data = ex.4)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

  #outlier is 2011, 2012, 2013 - how do we know what an outlier is here?
ex.5 <- all.diagnoses.two %>% filter(location == "C.18140")
ggplot(data = ex.5)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()

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

# Saving ----------------------------------------
write.csv(diagnoses.outliers, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/outliers/diagnoses_outliers.csv")
write.csv(diagnosed.prevalence.outliers, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/outliers/diagnosed.prevalence.outliers.csv")


# This is how you will put back in the adjudicated data frame -------------

# run.outlier.process(outcome= 'diagnosed.prevalence',
#                     stratifications= list(c()),
#                     data.manager= surveillance.manager,
#                     locations= MSAS.OF.INTEREST,
#                     adjudication.data.frame = fixed.outliers)


# Create Plots ------------------------------------------------------------


# HIV Deaths Charts --------------------------------------------------------


hiv.deaths = as.data.frame.table(surveillance.manager$data$hiv.deaths$estimate$cdc.hiv$cdc$year__location)

#Arizona: COVID?
hiv.deaths.az <- hiv.deaths %>% filter(location == "AZ")
ggplot(data = hiv.deaths.az)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "red")+                       
  labs()+                                 
  theme() 

#Michigan: change scale?
hiv.deaths.mi <- hiv.deaths %>% filter(location == "MI")
ggplot(data = hiv.deaths.mi)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "red")+                       
  labs()+                                 
  theme() 


# Diagnoses Charts --------------------------------------------------------


diagnoses.outliers = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location)

#C.47900- Washington, Arlington, Alexandria- I think it's flagging NaN
diagnoses.outliers.dc <- diagnoses.outliers %>% filter(location == "C.47900")
ggplot(data = diagnoses.outliers.dc)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "blue")+                       
  labs()+                                 
  theme() 

#CT
diagnoses.outliers.ct <- diagnoses.outliers %>% filter(location == "CT")
ggplot(data = diagnoses.outliers.ct)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "blue")+                       
  labs()+                                 
  theme() 

#WV- I don't think these are outliers.  It looks like recent post-covid years
#are being used as the reference point when they are the outliers
diagnoses.outliers.wv <- diagnoses.outliers %>% filter(location == "WV")
ggplot(data = diagnoses.outliers.wv)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "blue")+                       
  labs()+                                 
  theme() 

#54011- Pierce County Wisconsin- another good example
diagnoses.outliers.wi <- diagnoses.outliers %>% filter(location == "54011")
ggplot(data = diagnoses.outliers.wi)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "blue")+                       
  labs()+                                 
  theme()

#Charlotte MSA- only has 3 years of data- is this enough to know what constitutes an outlier?
diagnoses.outliers.char <- diagnoses.outliers %>% filter(location == "C.16740")
ggplot(data = diagnoses.outliers.char)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "blue")+                       
  labs()+                                 
  theme() 


# Diagnosed.Prevalence Charts ---------------------------------------------
diagnosed.prevalence.outliers = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location)

#New Mexico
dx.prev.nm <- diagnosed.prevalence.outliers %>% filter(location == "NM")
ggplot(data = dx.prev.nm)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "purple")+                       
  labs()+                                 
  theme()

#01003 Hampshire County, MA- This graph is interesting, I thik 2010 is the actual outlier
dx.prev.ma <- diagnosed.prevalence.outliers %>% filter(location == "01003")
ggplot(data = dx.prev.ma)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "purple")+                       
  labs()+                                 
  theme()

#01083 Worcester County, MA- This graph is interesting, I think 2010 is the actual outlier
dx.prev.wor <- diagnosed.prevalence.outliers %>% filter(location == "01083")
ggplot(data = dx.prev.wor)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "purple")+                       
  labs()+                                 
  theme()

#06005 - Amador County, CA
dx.prev.amador <- diagnosed.prevalence.outliers %>% filter(location == "06005")
ggplot(data = dx.prev.amador)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "purple")+                       
  labs()+                                 
  theme()

#12121 - Suwanee County, FL
dx.prev.suwanee <- diagnosed.prevalence.outliers %>% filter(location == "12121")
ggplot(data = dx.prev.suwanee)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "purple")+                       
  labs()+                                 
  theme()


