
# Add in Variance Values --------------------------------------------------

#This is for data from BRFSS
#Outcomes = medicaid.total and uninsured.total


# Uninsured Variance ------------------------------------------------------


#Variance: uninsured.total; TOTAL-----------------------------------------------------
variance.total.uninsured = lapply(total.proportion.tested.uninsured, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = proportion_tested*(1-proportion_tested)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance)%>%
        rename(value = variance) #rename the old value to now be variance.  This now represents the variance metric for the proportion tested outcome
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})
#Variance: uninsured.total; SEX-----------------------------------------------------
variance.sex.uninsured = lapply(sex.proportion.tested.uninsured, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, sex)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, sex)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})
#Variance: uninsured.total; AGE-----------------------------------------------------
variance.age.uninsured = lapply(age.proportion.tested.uninsured, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, age)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, age)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})

#Variance: uninsured.total; RACE-----------------------------------------------------
variance.race.uninsured = lapply(race.proportion.tested.uninsured, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, race)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, race)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})


#Variance: uninsured.total; RISK-----------------------------------------------------

variance.risk.uninsured = lapply(risk.proportion.tested.uninsured, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, risk)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, risk)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})

# PUT---------------------------------------------------
variance.total.uninsured.put= lapply(variance.total.uninsured, `[[`, 2)

for (data in variance.total.uninsured.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

variance.sex.uninsured.put= lapply(variance.sex.uninsured, `[[`, 2)

for (data in variance.sex.uninsured.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

variance.age.uninsured.put= lapply(variance.age.uninsured, `[[`, 2)

for (data in variance.age.uninsured.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

variance.race.uninsured.put= lapply(variance.race.uninsured, `[[`, 2)

for (data in variance.race.uninsured.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

variance.risk.uninsured.put= lapply(variance.risk.uninsured, `[[`, 2)

for (data in variance.risk.uninsured.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


# Medicaid Variance -------------------------------------------------------


#Variance: medicaid.total; TOTAL-----------------------------------------------------
variance.total.medicaid = lapply(total.proportion.tested.medicaid, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = proportion_tested*(1-proportion_tested)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance)%>%
        rename(value = variance) #rename the old value to now be variance.  This now represents the variance metric for the proportion tested outcome
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})
#Variance: medicaid.total; SEX-----------------------------------------------------
variance.sex.medicaid = lapply(sex.proportion.tested.medicaid, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, sex)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, sex)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})
#Variance: medicaid.total; AGE-----------------------------------------------------
variance.age.medicaid = lapply(age.proportion.tested.medicaid, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, age)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, age)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})

#Variance: medicaid.total; RACE-----------------------------------------------------
variance.race.medicaid = lapply(race.proportion.tested.medicaid, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, race)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, race)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})


#Variance: medicaid.total; RISK-----------------------------------------------------

variance.risk.medicaid = lapply(risk.proportion.tested.medicaid, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, risk)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, risk)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})

# PUT---------------------------------------------------
total.proportion.tested.medicaid.put= lapply(total.proportion.tested.medicaid, `[[`, 2)

for (data in total.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

sex.proportion.tested.medicaid.put= lapply(sex.proportion.tested.medicaid, `[[`, 2)

for (data in sex.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

age.proportion.tested.medicaid.put= lapply(age.proportion.tested.medicaid, `[[`, 2)

for (data in age.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

race.proportion.tested.medicaid.put= lapply(race.proportion.tested.medicaid, `[[`, 2)

for (data in race.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

risk.proportion.tested.medicaid.put= lapply(risk.proportion.tested.medicaid, `[[`, 2)

for (data in risk.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}
