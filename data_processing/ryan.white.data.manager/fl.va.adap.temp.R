#This code is pulling data from an excel file that Melissa put together with only FL and VA data.  This will eventually be replaced
#When we scrape the PDFs to have data for all states.

# * = new outcome

#==========================================================================
#Outcome = adap.income.distribution*
#Stratification= FPL (Proportion, denominator = adap.clients)
#Note: Needs 2 ontologies 2015-2020 (nastad.fpl.1) and 2021-2023 (nastad.fpl.2)
# **Redistribute 'unknown' FPL in the Put
#==========================================================================

NASTAD_income <- read_excel("Q:/data_raw/ryan.white.pdf.tables/fl.va.temp/RW_income_data.xlsx", 
                             sheet = "NASTAD_income")

#2015-2020
nastad.income.1 <- NASTAD_income[c(8:20), ]

colnames(nastad.income.1) <- as.character(nastad.income.1[1, ])
nastad.income.1 <- nastad.income.1[-1, ]
nastad.income.1 <- nastad.income.1[, !is.na(names(nastad.income.1))]

nastad.income.1.clean <-nastad.income.1%>%
    select(-`Report Year`, -`ADAP Clients Served`)%>%
    mutate(outcome = "adap.income.distribution")%>%
    rename(location =`State/Territory`)%>%
    rename(year = Year)%>%
    pivot_longer(cols = contains('FPL'),
                 names_to= "fpl",
                 values_to = "value")%>%
    mutate(fpl = case_when(
        fpl == "At or below 100% of FPL" ~"0-100",
        fpl == "101%-138% of FPL" ~"101-138",
        fpl == "139%-200% of FPL" ~"139-200",
        fpl == "201%-300% of FPL" ~"201-300",
        fpl == "301%-400% of FPL" ~"301-400",
        fpl == "Above 400%  FPL" ~">400",
        fpl == "Unknown FPL" ~"Unknown"))%>%
    mutate(value = readr::parse_number(value))

nastad.income.1.clean$value <- as.numeric(format(nastad.income.1.clean$value, scientific = FALSE))

nastad.income.1.clean <- as.data.frame(nastad.income.1.clean)

#Put 2015-2020
data.manager$put.long.form(
    data = nastad.income.1.clean,
    ontology.name = 'nastad.fpl.1',
    source = 'nastad.adap',
    dimension.values.to.distribute = list(fpl=('Unknown')),
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')
    
    
#2021-2023
nastad.income.2 <- NASTAD_income[c(1:6), ]

nastad.income.2.clean <- nastad.income.2%>%
    select(-`Report Year`, -`ADAP Clients Served`)%>%
    mutate(outcome = "adap.income.distribution")%>%
    rename(location =`State/Territory`)%>%
    rename(year = Year)%>%
    pivot_longer(cols = contains('FPL'),
                 names_to= "fpl",
                 values_to = "value")%>%
    mutate(fpl = case_when(
        fpl == "At or below 100% of FPL" ~"0-100",
        fpl == "101%-138% of FPL" ~"101-138",
        fpl == "139%-200% of FPL" ~"139-200",
        fpl == "201%-300% of FPL" ~"201-300",
        fpl == "301%-400% of FPL" ~"301-400",
        fpl == "401%-500% of FPL" ~"401-500",
        fpl == "Above 500% of FPL" ~">500",
        fpl == "Unknown FPL" ~"Unknown"))%>%
    mutate(value = readr::parse_number(value))

nastad.income.2.clean$value <- as.numeric(format(nastad.income.2.clean$value, scientific = FALSE))

nastad.income.2.clean <- as.data.frame(nastad.income.2.clean)

#Put 2021-2023
data.manager$put.long.form(
    data = nastad.income.2.clean,
    ontology.name = 'nastad.fpl.2',
    source = 'nastad.adap',
    dimension.values.to.distribute = list(fpl=('Unknown')),
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')


#==========================================================================
#Outcome = adap.suppression
#Stratification= service type (proportion)

#Outcome = adap.clients.service.distribution*
#stratification = service type (proportion, denominator = adap.clients)
#==========================================================================
NASTAD_VL_and_service_type <- read_excel("Q:/data_raw/ryan.white.pdf.tables/fl.va.temp/RW_income_data.xlsx", 
                                         sheet = "NASTAD_VL_and_service_type", 
                                         skip = 1, n_max = 18)

#Outcome = adap.suppression by service type
suppression.by.service.type.df <- NASTAD_VL_and_service_type%>%
    select(Year, `State/Territory`, `Full-Pay Medications (WITH OR WITHOUT INSURANCE) VL <200`, `ADAP-Funded Insurance Only VL <200`)%>%
    rename(year = Year)%>%
    mutate(year = as.character(year))%>%
    rename(location = `State/Territory`)%>%
    mutate(outcome = "adap.suppression")%>%
    pivot_longer(cols = contains('VL'),
                 names_to= "service.type",
                 values_to = "value")%>%
    mutate(service.type = case_when(
        service.type == "Full-Pay Medications (WITH OR WITHOUT INSURANCE) VL <200" ~ "full.pay",
        service.type == "ADAP-Funded Insurance Only VL <200" ~ "insurance.only"
    ))%>%
    mutate(value = readr::parse_number(value))

suppression.by.service.type.df <- as.data.frame(suppression.by.service.type.df)

data.manager$put.long.form(
    data = suppression.by.service.type.df,
    ontology.name = 'nastad.fpl.2',
    source = 'nastad.adap',
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')

#outcome = adap.clients.service.distribution
#(how many clients are getting full pay vs. insurance)

service.distribution <- NASTAD_VL_and_service_type%>%
    select(Year, `State/Territory`, `ADAP Clients Served Total`, `Subset of ADAP Clients Served by Full-Pay Medications (WITH OR WITHOUT INSURANCE) Total Count`, `Subset of ADAP Clients Served by ADAP-Funded Insurance Only Count`)%>%
    rename(year = Year)%>%
    mutate(year = as.character(year))%>%
    rename(location = `State/Territory`)%>%
    mutate(total = readr::parse_number(`ADAP Clients Served Total`))%>%
    mutate(full.pay = readr::parse_number(`Subset of ADAP Clients Served by Full-Pay Medications (WITH OR WITHOUT INSURANCE) Total Count`))%>%
    mutate(insurance.only = readr::parse_number(`Subset of ADAP Clients Served by ADAP-Funded Insurance Only Count`))%>%
    select(year, location, total, full.pay, insurance.only)%>%
    mutate(outcome = "adap.clients.service.distribution")
    
full.pay <- service.distribution%>%
    mutate(value =  round(full.pay / total, 2),
           service.type = "full.pay")%>%
    select(year, location, outcome, service.type, value)

insurance.only <- service.distribution%>%
    mutate(value =  round(insurance.only / total, 2),
           service.type = "insurance.only")%>%
    select(year, location, outcome, service.type, value)

full.pay <- as.data.frame(full.pay)

insurance.only <- as.data.frame(insurance.only)

#Put adap.clients.service.distribution:
data.manager$put.long.form(
    data = full.pay,
    ontology.name = 'nastad.fpl.2',
    source = 'nastad.adap',
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')

data.manager$put.long.form(
    data = insurance.only,
    ontology.name = 'nastad.fpl.2',
    source = 'nastad.adap',
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')



#==========================================================================
# Outcome = adap.clients (Ryan White Report, adding 2023)
#Stratification = total, FPL (Count)
#==========================================================================

RW_ADAP <- read_excel("Q:/data_raw/ryan.white.pdf.tables/fl.va.temp/RW_income_data.xlsx", 
                      sheet = "RW_ADAP")

rw.adap.clean <- RW_ADAP%>%
    select(Year, `State/Territory`, contains('Count'), `Total (N)`)%>%
    rename(year = Year)%>%
    mutate(year = as.character(year))%>%
    rename(location = `State/Territory`)%>%
    mutate(location = state.abb[match(location, state.name)])%>%
    mutate(outcome = "adap.clients")%>%
    filter(year == "2023") #Other years are already in the data manager

#Put Total 2023
rw.adap.total <- rw.adap.clean%>%
    rename(value = `Total (N)`)%>%
    select(year, location, value, outcome)

    data.manager$put.long.form(
        data = rw.adap.total,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')
    
#Put FPL 2023
    rw.adap.fpl <- rw.adap.clean%>%
        select(-`Total (N)`)%>%
        pivot_longer(cols = contains('Count'),
                     names_to= "fpl",
                     values_to = "value")%>%
        mutate(fpl = case_when(
            fpl == "0-100% FPL Count" ~"0-100",
            fpl == "101%-138% FPL Count" ~"101-138",
            fpl == "139%-250% FPL Count" ~"139-250",
            fpl == "251%-400% FPL Count" ~"251-400",
            fpl == ">400% FPL Count" ~">400"))
    
    
    data.manager$put.long.form(
        data = rw.adap.fpl,
        ontology.name = 'ryan.white.pdfs',
        source = 'ryan.white.program',
        url = 'https://ryanwhite.hrsa.gov/data/reports',
        details = 'Ryan White Downloaded PDF Reports')


