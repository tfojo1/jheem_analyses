# Set up  -----------------------------------------------------------------
library(jheem2)
library(tidyverse)
options(error=NULL)

surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')


# Outlier Function --------------------------------------------------------

dx.prev.ehe <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                              stratifications= list(c()), 
                                              data.manager= surveillance.manager,
                                              locations= c(EHE.MSAS))



#write.csv(dx.prev.ehe, file = "C:/Users/zthomps5/OneDrive - Johns Hopkins/Desktop/outliers/diagnosed.prevalence.5.1.24/dx.prev.ehe.csv")

# Plots -------------------------------------------------------------------

  #Source = cdc.surveillance.reports
all.dx.prev.source1 = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location)

#Plot1- Washington, DC
plot.one <- all.dx.prevsource1 %>% filter(location == "C.47900")
ggplot(data = plot.one)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)

#Plot2- Riverside-San Bernardino, CA
plot.two <- all.dx.prevsource1 %>% filter(location == "C.40140")
ggplot(data = plot.two)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)


#Source = cdc.aggregated.county
all.dx.prev.source2 = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location)

#Plot3- Atlanta
plot.three <- all.dx.prev.source2 %>% filter(location == "C.12060")
ggplot(data = plot.three)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)

#Plot4- NOLA
plot.three <- all.dx.prev.source2 %>% filter(location == "C.35380")
ggplot(data = plot.three)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)


# Adjust Outlier Values ---------------------------------------------------

dx.prev.ehe <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                   stratifications= list(c()), 
                                   data.manager= surveillance.manager,
                                   phi=0.2, #default is phi=0.15 (percent change from year to year)
                                   theta=0.08, #default is theta=0.05 (a multiplier that produces a percent change)
                                   locations= c(EHE.MSAS))

#Here I adjusted the values with gives 3 total outliers- but looking at them I don't think they
#are outliers and I think we miss the actual outliers  ( iused 0.3 for phi and 0.1 for theta)

plot.three <- all.dx.prev.source2 %>% filter(location == "C.16980")
ggplot(data = plot.three)+                   
  geom_point(                             
    mapping = aes(x = year, y = Freq),    
    color = "turquoise4")+                       
  labs()+                                 
  theme()+
  ylim(0, NA)
