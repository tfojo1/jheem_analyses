#source('../jheem2/R/tests/source_jheem2_package.R')
SURVEILLANCE.MANAGER = load.data.manager("../../cached/surveillance.manager.rdata")

SURVEILLANCE.MANAGER$register.outcome("number.eligible",
                                      metadata = create.outcome.metadata(display.name = "Number of Eligible PrEP Users", description = "Number of people eligible for PrEP",scale = "non.negative.number", axis.name = "PrEP Referrals", units = "People", singular.unit = "Person"))


SURVEILLANCE.MANAGER$register.outcome("non.healthcare.hiv.tests",
                                      metadata = create.outcome.metadata(display.name = "Number Number of Non-Healthcare CDC HIV Tests", description = "Number Number of Non-Healthcare CDC HIV Tests",scale = "non.negative.number", axis.name = "HIV Tests", units = "Tests", singular.unit = "Test"))


SURVEILLANCE.MANAGER$register.outcome("proportion.referred",
                                      metadata = create.outcome.metadata(display.name = "Proportion Referred to PrEP", description = "Proportion of individuals referred to PrEP", scale = "proportion", axis.name = "Proportion", units = "Proportion", singular.unit = "Proportion"), denominator.outcome = "adult.population")




#Number Eligible 
SURVEILLANCE.MANAGER$put(data = 17146, outcome = "number.eligible", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2022", location = "LA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports") #to update
SURVEILLANCE.MANAGER$put(data = 3327, outcome = "number.eligible", source = "cdc.hiv", ontology.name = "cdc",dimension.values = list(year = "2022", location = "AL"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports") #to update


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


#Update CDC HIV Tests for 2022 

SURVEILLANCE.MANAGER$put(data =  72704, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "AL"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  624, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "AK"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  69762, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "AZ"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  22274, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "AR"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  140472, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "CA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  9284, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "CO"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  84997, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "CT"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  5366, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "DE"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  226001, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "FL"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  97988, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "GA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  2140, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "HI"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  6239, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "ID"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  35357, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "IL"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  16540, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "IN"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  10066, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "IA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  14806, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "KS"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  30477, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "KY"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  99102, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "LA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  1379, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "ME"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  48195, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MD"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  40442, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  30685, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MI"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  4464, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MN"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  8379, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MS"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  27138, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MO"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  2130, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "MT"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  6573, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NE"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  28048, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NV"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  870, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NH"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  33695, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NJ"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  8095, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NM"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  63137, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NY"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  26603, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "NC"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  4144, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "ND"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  7812, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "OH"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  2900, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "OK"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  9703, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "OR"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  86931, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "PA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  1480, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "RI"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  57187, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "SC"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  1370, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "SD"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  69330, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "TN"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  331043, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "TX"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  2495, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "UT"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  0, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "VT"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  34855, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "VA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  11724, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "WA"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  3721, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "WV"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  8319, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "WI"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")
SURVEILLANCE.MANAGER$put(data =  3038, outcome = "hiv.tests", source = "cdc.hiv", ontology.name = "cdc", dimension.values = list(year = "2022", location = "WY"), url = "https://www.cdc.gov/hivpartners/php/state-hiv-profiles/index.html", details = "CDC State Prevention Reports")





set.default.data.manager(SURVEILLANCE.MANAGER)
