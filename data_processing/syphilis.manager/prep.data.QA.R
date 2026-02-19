#AIDS VU PREP DATA
#Some states race was not available for about 1/3 of users
#We are creating a prep proportion to estimate:


# Create Outcome: ---------------------------------------------------------

syphilis.manager$register.outcome(
    'prep.proportion',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'PrEP Proportion',
        axis.name = 'PrEP Proportion',
        units = '%',
        description = "PrEP Proportion"), denominator.outcome = 'prep') 

# Pull Total Prep: --------------------------------------------------------

prep = as.data.frame.table(syphilis.manager$data$prep$estimate$aidsvu$aidsvu$year__location)

states.without.race <- c("CA", "HI", "IL", "NY", "TX", "VA")

prep.problem.states <- prep%>%
    filter(location %in% states.without.race)%>%
    rename(total = Freq)


# Pull Prep by Race: ------------------------------------------------------

prep.race = as.data.frame.table(syphilis.manager$data$prep$estimate$aidsvu$aidsvu$year__location__race)
prep.problem.states.race <- prep.race%>%
    filter(location %in% states.without.race)%>%
    rename(race.count = Freq)


# Combine and Calculate Proportion: ---------------------------------------

prep.proportion.df = left_join(prep.problem.states.race, prep.problem.states, by = c("year", "location"))


prep.proportion.df <- prep.proportion.df %>%
    mutate(prep.proportion = (round(race.count / total, digits = 2)))%>%
    mutate(Keep.indicator = case_when(
        year == "2012" & location == "CA" ~ "keep",
        year == "2013" & location == "CA" ~ "keep",
        year == "2016" & location == "HI" ~ "keep",
        year == "2017" & location == "HI" ~ "keep",
        
        year == "2018" & location == "HI" ~ "keep",
        year == "2019" & location == "HI" ~ "keep",
        year == "2020" & location == "HI" ~ "keep",
        year == "2021" & location == "HI" ~ "keep",
        
        year == "2012" & location == "IL" ~ "keep",
        year == "2013" & location == "IL" ~ "keep",
        
        year == "2012" & location == "NY" ~ "keep",
        
        year == "2012" & location == "TX" ~ "keep",
        
        year == "2012" & location == "VA" ~ "keep",
        
        TRUE ~ "drop"
        
    ))%>%
    filter(Keep.indicator == "keep")%>%
    mutate(outcome = "prep.proportion")%>%
    rename(value = prep.proportion)%>%
    mutate(across(c(year, location, outcome, race), as.character))%>%
    select(year, location, outcome, race, value)


# Put: --------------------------------------------------------------------

syphilis.manager$put.long.form(
    data = prep.proportion.df,
    ontology.name = 'aidsvu', 
    source = 'aidsvu',
    dimension.values = list(),
    url = 'https://aidsvu.org/',
    details = 'Estimated prep proportion data')



# Remove the prep data by race for these states ---------------------------

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="CA", year=c("2012", "2013")),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="HI", year=c("2016", "2017", "2018", "2019", "2020", "2021")),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="IL", year=c("2012", "2013")),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="NY", year="2012"),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="TX", year="2012"),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)

remove.data(
    data.manager=syphilis.manager,
    outcome = "prep",
    source= "aidsvu",
    ontology.name = "aidsvu",
    dimension.values = list(race=c('black', 'hispanic', 'white'), location="VA", year="2012"),
    metric = "estimate",
    details.for.removal = "removed",
    url.for.removal = "url"
)
