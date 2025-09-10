source('../jheem_analyses/applications/EHE/ehe_specification.R')

RW.STATES = c("AL","AR","AZ","CA","CO","FL","GA","IL",
              "KY","LA","MD","MI","MN","MO","MS","NC","NV",
              "NY","OH","OK","SC","TN","TX","VA","WA","WI")


rw.total.prevalence = 
    sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location["2021",RW.STATES])

national.prevalence = 
    sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk["2021",,,,,],na.rm = T)

rw.total.prevalence/national.prevalence

all.states.prevalence = 
    SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location["2021",]
all.states.diagnoses = 
    SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location["2021",]

all.states.prevalence = sort(all.states.prevalence[(sapply(names(all.states.prevalence),nchar)==2)],decreasing = T)
all.states.diagnoses = sort(all.states.diagnoses[(sapply(names(all.states.diagnoses),nchar)==2)],decreasing = T)

ALL.STATES = names(all.states.prevalence)

REMAINING.STATES = setdiff(ALL.STATES,RW.STATES)

all.states.prevalence[REMAINING.STATES]
all.states.prevalence[RW.STATES]

all.states.diagnoses[REMAINING.STATES]
all.states.diagnoses[RW.STATES]



