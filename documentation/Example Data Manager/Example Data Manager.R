#Creating a data manager#

library(jheem2)
library(tidyverse)

data.manager = create.data.manager('example.data.manager', description='example.data.manager')

data.manager$register.outcome(
    'tb.cases',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Tuberculosis Cases',
        axis.name = 'Tuberculosis Cases',
        units = 'cases',
        description = "New Tuberculosis Diagnoses"))

data.manager$register.outcome(
    'my.proportion.outcome', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion Outcome',
        axis.name = 'Proportion Outcome',
        units = '%',
        description = "This Outcome is a Proportion"), denominator.outcome = 'tb.cases')


# Register your Data Sources ----------------------------------------------

data.manager$register.parent.source('my.parent.source', full.name = 'Parent Source for Your Data', short.name= "my.parent.source")
data.manager$register.source('child.source', parent.source= "my.parent.source", full.name = "Full Name of the Source", short.name='child.source') 

# Register Your Ontologies ------------------------------------------------

data.manager$register.ontology(
    'ontology.one',
    ont = ontology(
        year= NULL, #Keep year and location NULL as they are incomplete dimensions (the exception is if you have data that uses specific year ranges)
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
        race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

# Process your Data -------------------------------------------------------
#I usually make a list of dataframes because this is easy to add to as we have more data in the future

TB.DATA.DIRECTORY ="documentation/Example Data Manager"

tb.files <- Sys.glob(paste0(TB.DATA.DIRECTORY, '/*.csv'))

list.of.all.tb.files <- lapply(tb.files, function(x){  
    list(filename=x, data=read.csv(x, skip=7))
})

#This is total level data so we just need Year, Location, Outcome, Value:

cleaned.tb.files = lapply(list.of.all.tb.files, function(file){ 
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data <- data %>%
        mutate(value = as.numeric(gsub(",", "", Cases)))%>%
        mutate(outcome = 'tb.cases')%>%    
        mutate(location = state.abb[match(`Geography`, state.name)])%>%
        mutate(year = as.character(Year))
        
    data= as.data.frame(data)
    list(filename, data)
})


# Put your Data into the Data Manager -------------------------------------
put.tb.data = lapply(cleaned.tb.files, `[[`, 2)

for (data in put.tb.data) {
data.manager$put.long.form(
    data = data,
    ontology.name = 'ontology.one',
    source = 'child.source',
    url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
    details = 'This is Example Data from Atlas Plus')
}


# Save your data manager (use the Q drive typically) --------------------------------

save(data.manager, file="documentation/Example Data Manager/example.data.manager.rdata")


# Accessing your data manager ---------------------------------------------

my.new.data.manager = load.data.manager(name="example.data.manager", file="documentation/Example Data Manager/example.data.manager.rdata")

#Pulling Data: Approach #1 (can use to aggregate data but Andrew might remove this...)
states.in.my.data = dimnames(my.new.data.manager$data$tb.cases$estimate$child.source$ontology.one$year__location)$location

data.i.want.to.pull = my.new.data.manager$pull(
    outcome = "tb.cases",
    source = "child.source",
    from.ontology.names = "ontology.one",
    dimension.values = list(location = states.in.my.data),
    keep.dimensions = c("year")
)

#Pulling Data: Approach #2
my.new.data.manager$data$tb.cases$estimate$child.source$ontology.one$year__location #See all data for this outcome, source, ontology

dimnames(my.new.data.manager$data$tb.cases$estimate$child.source$ontology.one$year__location) #see the dimnames for this outcome

my.new.data.manager$data$tb.cases$estimate$child.source$ontology.one$year__location[, "CA"] #pull a specific location from this outcome

