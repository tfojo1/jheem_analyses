#source('../jheem2/R/tests/source_jheem2_package.R')
#SURVEILLANCE.MANAGER = load.data.manager("../../cached/surveillance.manager.rdata")

SURVEILLANCE.MANAGER$register.outcome("number.eligible",
                                      metadata = create.outcome.metadata(display.name = "Number of Eligible PrEP Users", description = "Number of people eligible for PrEP",scale = "non.negative.number", axis.name = "PrEP Referrals", units = "People", singular.unit = "Person"))


SURVEILLANCE.MANAGER$register.outcome("non.healthcare.hiv.tests",
                                      metadata = create.outcome.metadata(display.name = "Number Number of Non-Healthcare CDC HIV Tests", description = "Number Number of Non-Healthcare CDC HIV Tests",scale = "non.negative.number", axis.name = "HIV Tests", units = "Tests", singular.unit = "Test"))


SURVEILLANCE.MANAGER$register.outcome("proportion.referred",
                                      metadata = create.outcome.metadata(display.name = "Proportion Referred to PrEP", description = "Proportion of individuals referred to PrEP", scale = "proportion", axis.name = "Proportion", units = "Proportion", singular.unit = "Proportion"), denominator.outcome = "adult.population")




#Number Eligible 
SURVEILLANCE.MANAGER$put(data = 17146, outcome = "number.eligible", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2025", location = "LA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 3327, outcome = "number.eligible", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2025", location = "AL"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports") #to update


#Number non-healthcare tests

SURVEILLANCE.MANAGER$put(data = 0, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2011", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 13889, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2011", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 23636, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2012", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 31733, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2012", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 10754, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2013", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 15496, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2013", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 14991, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2014", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 19311, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2014", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 12920, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2015", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 24303, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2015", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 10805, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2016", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 18761, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2016", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 3225, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2017", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 18729, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2017", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 50778, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2018", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 17738, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2018", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 53640, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2019", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 17947, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2019", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 47689, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2020", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 11797, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2020", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

SURVEILLANCE.MANAGER$put(data = 59886, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2021", location = "AL"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 17882, outcome = "non.healthcare.hiv.tests", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2021", location = "LA"), url = "www.cdc.gov", details = "CDC State Prevention Reports") #to update

#Proportion Referred

SURVEILLANCE.MANAGER$put(data = 0.54, outcome = "proportion.referred", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2022", location = "AL"), url = "www.cdc.gov", details = "Integrated HIV Surveillance and Prevention Programs for Health Departments") 
SURVEILLANCE.MANAGER$put(data = 0.54, outcome = "proportion.referred", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2022", location = "LA"), url = "www.cdc.gov", details = "Integrated HIV Surveillance and Prevention Programs for Health Departments") 

set.default.data.manager(SURVEILLANCE.MANAGER)
