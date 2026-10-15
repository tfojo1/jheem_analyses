#===============================================================================

#This code processes 1, 2, and 3 way stratified population data from the
#Census.  This code pulls from the list 'four.way.stratified' created in the code:
#'four.way.stratified.census.county.population.data' and aggregates the 4 way stratified 
#data into various other stratifications.

#Data is put with multiple statements to differentiate intercensal estimates vs.
#vintage data in the 'Details' argument

#===============================================================================


#===============================================================================
#ONE WAY (4)
#===============================================================================
#Age:
stratified.age.alone = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, age, value)%>%
        group_by(outcome, year, location, age)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Sex:
stratified.sex.alone = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, value)%>%
        group_by(outcome, year, location, sex)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Race:
stratified.race.alone = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, race, value)%>%
        group_by(outcome, year, location, race)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Ethnicity:
stratified.ethnicity.alone = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, ethnicity, value)%>%
        group_by(outcome, year, location, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})



#===============================================================================
#TWO WAY (6)
#===============================================================================
#Sex+Age:
stratified.sex.age = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, age, value)%>%
        group_by(outcome, year, location, sex, age)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Sex+Race:
stratified.sex.race = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, race, value)%>%
        group_by(outcome, year, location, sex, race)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Sex+Eth:
stratified.sex.ethnicity = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, ethnicity, value)%>%
        group_by(outcome, year, location, sex, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Age+Race:
stratified.age.race = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, age, race, value)%>%
        group_by(outcome, year, location, age, race)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Age+Eth:
stratified.age.ethnicity = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, age, ethnicity, value)%>%
        group_by(outcome, year, location, age, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Race+Eth:
stratified.race.ethnicity = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, race, ethnicity, value)%>%
        group_by(outcome, year, location, race, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})


#===============================================================================
#THREE WAY (4)
#===============================================================================
#Sex+Race+Ethniticy:
stratified.sex.race.eth = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, race, ethnicity, value)%>%
        group_by(outcome, year, location, sex, race, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Sex+Age+Eth:
stratified.sex.age.eth = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, age, ethnicity, value)%>%
        group_by(outcome, year, location, sex, age, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})

#Sex+Race+Age:
stratified.sex.age.race = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, sex, age, race, value)%>%
        group_by(outcome, year, location, sex, age, race)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})
#Age+Race+Eth:
stratified.age.race.eth = lapply(four.way.stratified, function(file){
    
    data=file[[2]]
    filename = file[[1]]
    
    data <- data %>%
        select(outcome, year, location, age, race, ethnicity, value)%>%
        group_by(outcome, year, location, age, race, ethnicity)%>%
        mutate(new.value = sum(value)) %>%
        select(-value)%>%
        rename(value = new.value)
    
    data<- data[!duplicated(data), ]
    
    
    data= as.data.frame(data)
    
    list(filename, data)
})


#===============================================================================

#Organize Dataframes and Put

#===============================================================================

one.two.three.way.strata <- do.call(
    c,
    mget(c(
        "stratified.age.alone",
        "stratified.sex.alone",
        "stratified.race.alone",
        "stratified.ethnicity.alone",
        "stratified.sex.age",
        "stratified.sex.race",
        "stratified.sex.ethnicity",
        "stratified.age.race",
        "stratified.age.ethnicity",
        "stratified.race.ethnicity",
        "stratified.sex.race.eth",
        "stratified.sex.age.eth",
        "stratified.sex.age.race",
        "stratified.age.race.eth"
    ))
)


one.two.three.strata.intercensal <- one.two.three.way.strata[
    grepl("intercensal", names(one.two.three.way.strata))
]

one.two.three.strata.vintage <- one.two.three.way.strata[
    grepl("vintage", names(one.two.three.way.strata))
]

#Put:
one.two.three.strata.intercensal.put = lapply(one.two.three.strata.intercensal, `[[`, 2)

for (data in one.two.three.strata.intercensal.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values.to.distribute = list(race=('multiracial')),
        url = 'www.census.gov',
        details = 'Census Intercensal Data')
    
}

one.two.three.strata.vintage.put = lapply(one.two.three.strata.vintage, `[[`, 2)

for (data in one.two.three.strata.vintage.put) {
    
    census.manager$put.long.form(
        data = data,
        ontology.name = 'census',
        source = 'census.population',
        dimension.values.to.distribute = list(race=('multiracial')),
        url = 'www.census.gov',
        details = 'Census Vintage Data')
}
