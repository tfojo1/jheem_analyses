#This code pulls data from the HIV Surveillance Manager (JHEEM) to create the Ryan White Webtool Data Manager

#Outcomes include:

# FROM HIV SURVEILLANCE MANAGER
# diagnoses
# diagnosed.prevalence
# prep
# suppression
# total.prevalence
# awareness
# proportion.tested
# proportion.tested.n


surveillance.manager = load.data.manager("Q:/data_managers/surveillance.manager.rdata")


# HIV DATA MANAGER --------------------------------------------------------

#SUPPRESSION - ONE (3 SOURCES)
all.suppression.source.one = list(
suppression.source.one.total = as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F),
suppression.source.one.age= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__age, stringsAsFactors = F),
suppression.source.one.race = as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__race, stringsAsFactors = F),
suppression.source.one.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__risk, stringsAsFactors = F),
suppression.source.one.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__sex, stringsAsFactors = F),
suppression.source.one.age.race= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__age__race, stringsAsFactors = F),
suppression.source.one.age.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__age__risk, stringsAsFactors = F),
suppression.source.one.age.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__age__sex, stringsAsFactors = F),
suppression.source.one.race.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__race__sex, stringsAsFactors = F),
suppression.source.one.race.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__race__risk, stringsAsFactors = F),
suppression.source.one.sex.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location__sex__risk, stringsAsFactors = F),
suppression.source.one.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F)
)

all.suppression.source.one = lapply(all.suppression.source.one, function(file){
    
    data=file
    
    data$outcome = "suppression"
    data$value = data$Freq
    
    list(data) 
})

all.suppression.source.one.put = lapply(all.suppression.source.one, `[[`, 1)

for (data in all.suppression.source.one.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

#SUPPRESSION - 2
all.suppression.source.two = list(
    suppression.source.two.total = as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location, stringsAsFactors = F),
    suppression.source.two.age= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__age, stringsAsFactors = F),
    suppression.source.two.race = as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__race, stringsAsFactors = F),
    suppression.source.two.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__risk, stringsAsFactors = F),
    suppression.source.two.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__sex, stringsAsFactors = F),
    suppression.source.two.age.race= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__age__race, stringsAsFactors = F),
    suppression.source.two.age.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__age__risk, stringsAsFactors = F),
    suppression.source.two.age.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__age__sex, stringsAsFactors = F),
    suppression.source.two.race.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__race__sex, stringsAsFactors = F),
    suppression.source.two.race.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__race__risk, stringsAsFactors = F),
    suppression.source.two.sex.risk= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location__sex__risk, stringsAsFactors = F),
    suppression.source.two.sex= as.data.frame.table(surveillance.manager$data$suppression$estimate$cdc.aggregated.proportion$cdc$year__location, stringsAsFactors = F)
)

all.suppression.source.two = lapply(all.suppression.source.two, function(file){
    
    data=file
    
    data$outcome = "suppression"
    data$value = data$Freq
    
    list(data) 
})

all.suppression.source.two.put = lapply(all.suppression.source.two, `[[`, 1)

for (data in all.suppression.source.two.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.aggregated.proportion',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

#SUPPRESSION - THREE 
all.suppression.source.three = list(
total.suppression.source.three = as.data.frame.table(surveillance.manager$data$suppression$estimate$lhd$lhd$year__location, stringsAsFactors = F),
total.suppression.source.three.race = as.data.frame.table(surveillance.manager$data$suppression$estimate$lhd$lhd$year__location__race, stringsAsFactors = F),
total.suppression.source.three.age = as.data.frame.table(surveillance.manager$data$suppression$estimate$lhd$lhd$year__location__age, stringsAsFactors = F),
total.suppression.source.three.risk = as.data.frame.table(surveillance.manager$data$suppression$estimate$lhd$lhd$year__location__risk, stringsAsFactors = F),
total.suppression.source.three.risk = as.data.frame.table(surveillance.manager$data$suppression$estimate$lhd$lhd$year__location__sex, stringsAsFactors = F)
)

all.suppression.source.three = lapply(all.suppression.source.three, function(file){
    
    data=file
    
    data$outcome = "suppression"
    data$value = data$Freq
    
    list(data) 
})

all.suppression.source.three.put = lapply(all.suppression.source.three, `[[`, 1)

for (data in all.suppression.source.three.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'lhd',
        source = 'lhd',
        url = 'https://www.michigan.gov/mdhhs/keep-mi-healthy/chronicdiseases/hivsti/data-and-statistics/HIV-STI-Statewide-Annual-Analyses#current',
        details = 'Local Health Department Reports')
}

#TOTAL.PREVALENCE (2 SOURCES)
total.prevalence.source.one = as.data.frame.table(surveillance.manager$data$total.prevalence$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F)
total.prevalence.source.one <- total.prevalence.source.one %>%
    mutate(outcome = 'total.prevalence')%>%
    rename(value = Freq)%>%
    mutate(type = locations::get.location.type(location))%>%
    filter(type == "STATE")

total.prevalence.source.two = as.data.frame.table(surveillance.manager$data$total.prevalence$estimate$cdc.aggregated.county$cdc$year__location, stringsAsFactors = F)
total.prevalence.source.two <- total.prevalence.source.two %>%
    mutate(outcome = 'total.prevalence')%>%
    rename(value = Freq)

data.manager$put.long.form(
    data = total.prevalence.source.one,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    metric = 'estimate',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
    details = 'CDC Atlas Plus')

data.manager$put.long.form(
    data = total.prevalence.source.two,
    ontology.name = 'cdc',
    source = 'cdc.aggregated.county',
    metric = 'estimate',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
    details = 'CDC Atlas Plus')

#AWARENESS (TWO SOURCES)
awareness.source.one = as.data.frame.table(surveillance.manager$data$awareness$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F)
awareness.source.one <- awareness.source.one %>%
    mutate(outcome = 'awareness')%>%
    rename(value = Freq)%>%
    mutate(type = locations::get.location.type(location))%>%
    filter(type == "STATE")

awareness.source.two = as.data.frame.table(surveillance.manager$data$awareness$estimate$cdc.aggregated.proportion$cdc$year__location, stringsAsFactors = F)
awareness.source.two <- awareness.source.two %>%
    mutate(outcome = 'awareness')%>%
    rename(value = Freq)

data.manager$put.long.form(
    data = awareness.source.one,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    metric = 'estimate',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
    details = 'CDC Atlas Plus')

data.manager$put.long.form(
    data = awareness.source.two,
    ontology.name = 'cdc',
    source = 'cdc.aggregated.proportion',
    metric = 'estimate',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
    details = 'CDC Atlas Plus')


#PROPOTION.TESTED 
all.proportion.tested.data = list(
proportion.tested.total = as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location, stringsAsFactors = F),
proportion.tested.sex = as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__sex, stringsAsFactors = F),
proportion.tested.age = as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__age, stringsAsFactors = F),
proportion.tested.race = as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__race, stringsAsFactors = F),
proportion.tested.risk = as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__risk, stringsAsFactors = F)
)

all.proportion.tested.data = lapply(all.proportion.tested.data, function(file){
    
    data=file
    
    data$outcome = "proportion.tested"
    data$value = data$Freq
    
    list(data) 
})

all.proportion.tested.data.put= lapply(all.proportion.tested.data, `[[`, 1)

for (data in all.proportion.tested.data.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'estimate',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#PROPOTION.TESTED.N
all.proportion.tested.n.data = list(
    proportion.tested.n.total = as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location, stringsAsFactors = F),
    proportion.tested.n.sex = as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__sex, stringsAsFactors = F),
    proportion.tested.n.age = as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__age, stringsAsFactors = F),
    proportion.tested.n.race = as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__race, stringsAsFactors = F),
    proportion.tested.n.risk = as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__risk, stringsAsFactors = F)
)

all.proportion.tested.n.data = lapply(all.proportion.tested.n.data, function(file){
    
    data=file
    
    data$outcome = "proportion.tested.n"
    data$value = data$Freq
    
    list(data) 
})

all.proportion.tested.n.data.put= lapply(all.proportion.tested.n.data, `[[`, 1)

for (data in all.proportion.tested.n.data.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'estimate',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


# PREP - 4 sources --------------------------------------------------------
#PREP-SOURCE 1
prep.source.one = list(
    prep.source.one.total = as.data.frame.table(surveillance.manager$data$prep$estimate$aidsvu$aidsvu$year__location, stringsAsFactors = F),
    prep.source.one.sex = as.data.frame.table(surveillance.manager$data$prep$estimate$aidsvu$aidsvu$year__location__sex, stringsAsFactors = F),
    prep.source.one.age = as.data.frame.table(surveillance.manager$data$prep$estimate$aidsvu$aidsvu$year__location__age, stringsAsFactors = F),
    prep.source.one.race = as.data.frame.table(surveillance.manager$data$prep$estimate$aidsvu$aidsvu$year__location__race, stringsAsFactors = F)
    
)

prep.source.one = lapply(prep.source.one, function(file){
    
    data=file
    
    data$outcome = "prep"
    data$value = data$Freq
    data$location.check = locations::get.location.type(data$location)
    data = subset(data, data$location.check == "STATE")
    
    list(data) 
})

prep.source.one.put = lapply(prep.source.one, `[[`, 1)  

for (data in prep.source.one.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'aidsvu',
        source = 'aidsvu',
        dimension.values = list(),
        url = 'https://aidsvu.org/',
        details = 'AIDS Vu Reporting')
}

#PREP SOURCE 2
prep.source.two = list(
    prep.source.two.total = as.data.frame.table(surveillance.manager$data$prep$estimate$cdc.prep$cdc$year__location, stringsAsFactors = F),
    prep.source.two.age = as.data.frame.table(surveillance.manager$data$prep$estimate$cdc.prep$cdc$year__location__age, stringsAsFactors = F),
    prep.source.two.sex = as.data.frame.table(surveillance.manager$data$prep$estimate$cdc.prep$cdc$year__location__sex, stringsAsFactors = F),
    prep.source.two.age.sex = as.data.frame.table(surveillance.manager$data$prep$estimate$cdc.prep$cdc$year__location__age__sex, stringsAsFactors = F)
    
)

prep.source.two = lapply(prep.source.two, function(file){
    
    data=file
    
    data$outcome = "prep"
    data$value = data$Freq
    data$location.check = locations::get.location.type(data$location)
    data = subset(data, data$location.check == "STATE")
    
    list(data) 
})

prep.source.two.put = lapply(prep.source.two, `[[`, 1)  

for (data in prep.source.two.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.prep',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

#PREP SOURCE 3

prep.source.three = list(
    prep.source.three.total = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.cdc.aggregated.county$cdc$year__location, stringsAsFactors = F),
    prep.source.three.age = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.cdc.aggregated.county$cdc$year__location__age, stringsAsFactors = F),
    prep.source.three.sex = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.cdc.aggregated.county$cdc$year__location__sex, stringsAsFactors = F)
)

prep.source.three = lapply(prep.source.three, function(file){
    
    data=file
    
    data$outcome = "prep"
    data$value = data$Freq
    
    list(data) 
})

prep.source.three.put = lapply(prep.source.three, `[[`, 1)  

for (data in prep.source.three.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'prep.cdc.aggregated.county',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

#PREP SOURCE 4
prep.source.four = list(
    prep.source.four.total = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.aidsvu.aggregated.county$aidsvu$year__location, stringsAsFactors = F),
    prep.source.four.sex = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.aidsvu.aggregated.county$aidsvu$year__location__sex, stringsAsFactors = F),
    prep.source.four.age = as.data.frame.table(surveillance.manager$data$prep$estimate$prep.aidsvu.aggregated.county$aidsvu$year__location__age, stringsAsFactors = F)
    
)

prep.source.four = lapply(prep.source.four, function(file){
    
    data=file
    
    data$outcome = "prep"
    data$value = data$Freq
    
    list(data) 
})

prep.source.four.put = lapply(prep.source.four, `[[`, 1)  

for (data in prep.source.four.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'aidsvu',
        source = 'prep.aidsvu.aggregated.county',
        dimension.values = list(),
        url = 'https://aidsvu.org/',
        details = 'AIDS Vu Reporting')
}

#DIAGNOSED.PREVALNCE (SOURCE 1)
dx.prev.source.one = list(
    dx.prev.source.one.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F),
    
    dx.prev.source.one.race = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__race, stringsAsFactors = F),
    dx.prev.source.one.age = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age, stringsAsFactors = F),
    dx.prev.source.one.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__risk, stringsAsFactors = F),
    dx.prev.source.one.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__sex, stringsAsFactors = F),
    
    dx.prev.source.one.age.race = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__race, stringsAsFactors = F),
    dx.prev.source.one.age.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__risk, stringsAsFactors = F),
    dx.prev.source.one.age.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__sex, stringsAsFactors = F),
    dx.prev.source.one.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__sex__risk, stringsAsFactors = F),
    dx.prev.source.one.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__race__risk, stringsAsFactors = F),
    dx.prev.source.one.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__race__sex, stringsAsFactors = F),
    
    
    dx.prev.source.one.age.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__sex__risk, stringsAsFactors = F),
    dx.prev.source.one.age.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__race__risk, stringsAsFactors = F),
    dx.prev.source.one.age.race.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__race__sex, stringsAsFactors = F),
    dx.prev.source.one.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__race__sex__risk, stringsAsFactors = F),
    dx.prev.source.one.age.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk, stringsAsFactors = F)
)

dx.prev.source.one = lapply(dx.prev.source.one, function(file){
    
    data=file
    
    data$outcome = "diagnosed.prevalence"
    data$value = data$Freq
    
    data$location.check = locations::get.location.type(data$location)
    
    data <- data [(data$location.check == "STATE") | (data$location.check == "CBSA"),]
    
    
    list(data) 
})

dx.prev.source.one.put = lapply(dx.prev.source.one, `[[`, 1)

for (data in dx.prev.source.one.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

dx.prev.source.two = list(
    dx.prev.source.two.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location, stringsAsFactors = F),
    
    dx.prev.source.two.race = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__race, stringsAsFactors = F),
    dx.prev.source.two.age = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age, stringsAsFactors = F),
    dx.prev.source.two.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__risk, stringsAsFactors = F),
    dx.prev.source.two.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__sex, stringsAsFactors = F),
    
    dx.prev.source.two.age.race = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__race, stringsAsFactors = F),
    dx.prev.source.two.age.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__risk, stringsAsFactors = F),
    dx.prev.source.two.age.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__sex, stringsAsFactors = F),
    dx.prev.source.two.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__sex__risk, stringsAsFactors = F),
    dx.prev.source.two.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__race__risk, stringsAsFactors = F),
    dx.prev.source.two.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__race__sex, stringsAsFactors = F),
    
    
    dx.prev.source.two.age.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__sex__risk, stringsAsFactors = F),
    dx.prev.source.two.age.race.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__race__risk, stringsAsFactors = F),
    dx.prev.source.two.age.race.sex = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__age__race__sex, stringsAsFactors = F),
    dx.prev.source.two.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location__race__sex__risk, stringsAsFactors = F)
)

dx.prev.source.two = lapply(dx.prev.source.two, function(file){
    
    data=file
    
    data$outcome = "diagnosed.prevalence"
    data$value = data$Freq
    
    
    list(data) 
})

dx.prev.source.two.put = lapply(dx.prev.source.two, `[[`, 1)

for (data in dx.prev.source.two.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.aggregated.county',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

dx.prev.source.three = list(
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location, stringsAsFactors = F),
    
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__risk, stringsAsFactors = F),
    
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age__sex, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__sex, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__risk, stringsAsFactors = F),
    dx.prev.source.three.total = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex__risk, stringsAsFactors = F)
    
    
)

dx.prev.source.three = lapply(dx.prev.source.three, function(file){
    
    data=file
    
    data$outcome = "diagnosed.prevalence"
    data$value = data$Freq
    
    
    list(data) 
})

dx.prev.source.three.put = lapply(dx.prev.source.three, `[[`, 1)

for (data in dx.prev.source.three.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.msa.reports',
        source = 'cdc.surveillance.reports',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC MSA Reports')
}

#DIAGNOSES- SOURCE 1
diagnoses.source.one = list(
    diagnoses.source.one.total = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location, stringsAsFactors = F),
    diagnoses.source.one.race = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__race, stringsAsFactors = F),
    diagnoses.source.one.age = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age, stringsAsFactors = F),
    diagnoses.source.one.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__risk, stringsAsFactors = F),
    diagnoses.source.one.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__sex, stringsAsFactors = F),
    diagnoses.source.one.age.race = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race, stringsAsFactors = F),
    diagnoses.source.one.age.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__risk, stringsAsFactors = F),
    diagnoses.source.one.age.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__sex, stringsAsFactors = F),
    diagnoses.source.one.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__sex__risk, stringsAsFactors = F),
    diagnoses.source.one.race.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__race__risk, stringsAsFactors = F),
    diagnoses.source.one.race.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__race__sex, stringsAsFactors = F),
    diagnoses.source.one.age.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__sex__risk, stringsAsFactors = F),
    diagnoses.source.one.age.race.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__risk, stringsAsFactors = F),
    diagnoses.source.one.age.race.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex, stringsAsFactors = F),
    diagnoses.source.one.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__race__sex__risk, stringsAsFactors = F),
    diagnoses.source.one.age.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk, stringsAsFactors = F)
)


diagnoses.source.one = lapply(diagnoses.source.one, function(file){
    
    data=file
    
    data$outcome = "diagnoses"
    data$value = data$Freq
    
    data$location.check = locations::get.location.type(data$location)
    
    data <- data [(data$location.check == "STATE") | (data$location.check == "CBSA"),]
    
    list(data) 
})


diagnoses.source.one.put = lapply(diagnoses.source.one, `[[`, 1)

for (data in diagnoses.source.one.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

diagnoses.source.two = list(
    diagnoses.source.two.total = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location, stringsAsFactors = F),
    diagnoses.source.two.race = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__race, stringsAsFactors = F),
    diagnoses.source.two.age = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age, stringsAsFactors = F),
    diagnoses.source.two.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__risk, stringsAsFactors = F),
    diagnoses.source.two.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__sex, stringsAsFactors = F),
    diagnoses.source.two.age.race = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__race, stringsAsFactors = F),
    diagnoses.source.two.age.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__risk, stringsAsFactors = F),
    diagnoses.source.two.age.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__sex, stringsAsFactors = F),
    diagnoses.source.two.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__sex__risk, stringsAsFactors = F),
    diagnoses.source.two.race.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__race__risk, stringsAsFactors = F),
    diagnoses.source.two.race.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__race__sex, stringsAsFactors = F),
    diagnoses.source.two.age.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__sex__risk, stringsAsFactors = F),
    diagnoses.source.two.age.race.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__race__risk, stringsAsFactors = F),
    diagnoses.source.two.age.race.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__age__race__sex, stringsAsFactors = F),
    diagnoses.source.two.race.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.aggregated.county$cdc$year__location__race__sex__risk, stringsAsFactors = F)
)


diagnoses.source.two = lapply(diagnoses.source.two, function(file){
    
    data=file
    
    data$outcome = "diagnoses"
    data$value = data$Freq
    
    list(data) 
})


diagnoses.source.two.put = lapply(diagnoses.source.two, `[[`, 1)

for (data in diagnoses.source.two.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.aggregated.county',
        url = 'https://www.cdc.gov/nchhstp/about/atlasplus.html',
        details = 'CDC Atlas Plus')
}

#DIAGNOSES SOURCE 3
diagnoses.source.three = list(
    diagnoses.source.three.total = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location, stringsAsFactors = F),
    
    diagnoses.source.three.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex, stringsAsFactors = F),
    diagnoses.source.three.age = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age, stringsAsFactors = F),
    diagnoses.source.three.race = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race, stringsAsFactors = F),
    diagnoses.source.three.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__risk, stringsAsFactors = F),
    
    diagnoses.source.three.age.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__age__sex, stringsAsFactors = F),
    diagnoses.source.three.race.sex = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__sex, stringsAsFactors = F),
    diagnoses.source.three.race.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__risk, stringsAsFactors = F),
    diagnoses.source.three.sex.risk = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__sex__risk, stringsAsFactors = F)
    
)


diagnoses.source.three = lapply(diagnoses.source.three, function(file){
    
    data=file
    
    data$outcome = "diagnoses"
    data$value = data$Freq
    
    list(data) 
})


diagnoses.source.three.put = lapply(diagnoses.source.three, `[[`, 1)

for (data in diagnoses.source.three.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.msa.reports',
        source = 'cdc.surveillance.reports',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC MSA Reports')
}
