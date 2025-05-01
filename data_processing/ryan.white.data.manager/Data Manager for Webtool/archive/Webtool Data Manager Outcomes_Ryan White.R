#This code pulls data from the Ryan White Data Manager to create the Ryan White Webtool Data Manager

#Outcomes include:

# FROM RYAN WHITE MANAGER
# non.adap.clients
# adap.proportion
# oahs.clients
# oahs.suppression
# adap.suppression

ryan.white.data.manager = load.data.manager("Q:/data_managers/ryan.white.data.manager.rdata")

# Only want data for 31 MSAs of Interest + all states: --------------------

#source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors
#states = locations::get.all.for.type("STATE") #This has states that are not in the Ryan White data and are impacting the pull function

#states = dimnames(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location)$location


# Ryan White Data Manager -------------------------------------------------

#NON.ADAP.CLIENTS
all.non.adap.data = list(
non.adap.clients.total = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location, stringsAsFactors = F),
non.adap.clients.age = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age, stringsAsFactors = F),
non.adap.clients.fpl = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__fpl, stringsAsFactors = F),
non.adap.clients.race = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race, stringsAsFactors = F),
non.adap.clients.sex = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex, stringsAsFactors = F),
non.adap.clients.sex.race = as.data.frame.table(ryan.white.data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex__risk, stringsAsFactors = F)
)

all.non.adap.data = lapply(all.non.adap.data, function(file){
    
    data=file

    data$outcome = "non.adap.clients"
    data$value = data$Freq

    list(data) 
})

all.non.adap.data.put = lapply(all.non.adap.data, `[[`, 1)

for (data in all.non.adap.data.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')
}

#ADAP.PROPORTION
all.adap.proportion = list(
adap.proportion = as.data.frame.table(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location, stringsAsFactors = F),
adap.proportion.sex = as.data.frame.table(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex, stringsAsFactors = F),
adap.proportion.race = as.data.frame.table(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location__race, stringsAsFactors = F),
adap.proportion.age = as.data.frame.table(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location__age, stringsAsFactors = F),
adap.proportion.fpl = as.data.frame.table(ryan.white.data.manager$data$adap.proportion$estimate$ryan.white.program$ryan.white.pdfs$year__location__fpl, stringsAsFactors = F))

all.adap.proportion = lapply(all.adap.proportion, function(file){
    
    data=file
    
    data$outcome = "adap.proportion"
    data$value = data$Freq
    
    list(data) 
})

all.adap.proportion.put = lapply(all.adap.proportion, `[[`, 1)
for (data in all.adap.proportion.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')
}

#OAHS.CLIENTS
all.oahs.clients = list(
oahs.clients.total = as.data.frame.table(ryan.white.data.manager$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location, stringsAsFactors = F),
oahs.clients.age = as.data.frame.table(ryan.white.data.manager$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age, stringsAsFactors = F),
oahs.clients.race = as.data.frame.table(ryan.white.data.manager$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race, stringsAsFactors = F)
)
all.oahs.clients = lapply(all.oahs.clients, function(file){
    
    data=file
    
    data$outcome = "oahs.clients"
    data$value = data$Freq
    
    list(data) 
})
all.oahs.clients.put = lapply(all.oahs.clients, `[[`, 1)
for (data in all.oahs.clients.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')
}

#OAHS.SUPPRESSION
all.oahs.suppression = list(
    oahs.suppression.total = as.data.frame.table(ryan.white.data.manager$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location, stringsAsFactors = F),
    oahs.suppression.age = as.data.frame.table(ryan.white.data.manager$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location__age, stringsAsFactors = F),
    oahs.suppression.race = as.data.frame.table(ryan.white.data.manager$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location__race, stringsAsFactors = F)
)
all.oahs.suppression = lapply(all.oahs.suppression, function(file){
    
    data=file
    
    data$outcome = "oahs.suppression"
    data$value = data$Freq
    
    list(data) 
})

all.oahs.suppression.put = lapply(all.oahs.suppression, `[[`, 1)
for (data in all.oahs.suppression.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')
}

#ADAP.SUPPRESSION
    adap.suppression.total = as.data.frame.table(ryan.white.data.manager$data$adap.suppression$estimate$nastad.adap$ryan.white.pdfs$year__location, stringsAsFactors = F)
    
    adap.suppression.total <- adap.suppression.total %>%
        mutate(outcome = "adap.suppression")%>%
        rename(value = Freq)
    
    data.manager$put.long.form(
        data = adap.suppression.total,
        ontology.name = 'ryan.white.pdfs',
        source = 'nastad.adap',
        url = 'https://nastad.org/adap-monitoring-project',
        details = 'NASTAD PDF Reports')




