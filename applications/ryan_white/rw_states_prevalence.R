source('../jheem_analyses/applications/EHE/ehe_specification.R')

RW.STATES = c("CA","NY","FL","GA","TX","AL","MS","LA","IL","MO","WI") # "WI"


rw.total.prevalence = 
    sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location["2021",RW.STATES])

national.prevalence = 
    sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk["2021",,,,,],na.rm = T)

rw.total.prevalence/national.prevalence

all.states.prevalence = 
    SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location["2021",]

all.states.prevalence = sort(all.states.prevalence[(sapply(names(all.states.prevalence),nchar)==2)],decreasing = T)
ALL.STATES = names(all.states.prevalence)

REMAINING.STATES = setdiff(ALL.STATES,RW.STATES)

all.states.prevalence[REMAINING.STATES]
