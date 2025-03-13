#syphilis.manager = load.data.manager(name="syphilis.manager", file="../../cached/syphilis.manager.rdata")
#source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors


# This code creates a 'completeness' variable which tells how much data we have available to make up the entire MSA for prenatal care
#screening variables

#For example- do we have 100% of counties that make up the MSA?

# -------------------------------------------------------------------------

#Need to know:
# 1. counties in each MSA
# 2. what counties we have prenatal care data for (x) (assign them an MSA)
# 3. population of the MSA
# 4. county populations (to sum)


# FIRST TRIMESTER - TOTAL---------------------------------------------------------


#1
counties.in.each.msa = locations::get.contained.locations(MSAS.OF.INTEREST, "COUNTY", return.list = T)
counties.in.each.msa.df = stack(counties.in.each.msa)%>% rename(county.code = values) %>% rename(msa = ind)

# #unlist
unlisted.counties.in.msas = as.data.frame(unlist(counties.in.each.msa))
unlisted.counties.in.msas <- unlisted.counties.in.msas %>%
  rename(county = `unlist(counties.in.each.msa)`)

#2 
prenatal.counties = as.data.frame.table(data.manager$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location)

prenatal.counties <- prenatal.counties %>%
  filter(location != "US")%>% #Remove US
  mutate(outcome = "counties we have prenatal data for")%>%
  select(-Freq)%>%
  mutate(county.we.have.prenatal.for = "1")

#3
total.msa.population = as.data.frame.table(data.manager$data$population$estimate$census.aggregated.population$census$year__location)

total.msa.population$msa.indicator = (total.msa.population$location %in% MSAS.OF.INTEREST) 

total.msa.population <- total.msa.population %>%
  filter(msa.indicator == T)%>%
  filter(year == "2016" | year == "2017" | year == "2018" | year == "2019" | year== "2020" | year == "2021" | year == "2022" |year == "2023")%>%
  rename(total.msa.population = Freq)

#4
county.population = as.data.frame.table(data.manager$data$population$estimate$census.population$census$year__location)
county.population <- county.population %>%
  filter(location != "US")%>%
  rename(total.county.population = Freq)%>%
  filter(year == "2016" | year == "2017" | year == "2018" | year == "2019" | year== "2020" | year == "2021" | year == "2022" |year == "2023")


#Match the counties we have prenatal data for to their population
everything_v1 = left_join(prenatal.counties, county.population, by=c("year", "location"))

everything_v1 <- everything_v1%>%
  rename(county.code = location)%>%
  mutate(county.code = as.character(county.code))

#Remove any counties that are NOT in an MSA
everything_v1$is.county.in.an.msa = (everything_v1$county.code %in% unlisted.counties.in.msas$county)


everything_v1 <- everything_v1 %>%
  filter(is.county.in.an.msa == T) #select only counties that are in an MSA

#mutate(what.msa.is.this.county.in = locations::get.overlapping.locations(county.code, "CBSA")) #this wasn't working


#Merge everything_v1 with counties.in.each.msa.df
everything_v2 = left_join(everything_v1, counties.in.each.msa.df, by = 'county.code')

everything_v2$msa = as.character(everything_v2$msa)

total.msa.population <- total.msa.population%>%
  rename(msa = location)%>%
  select(year, msa, total.msa.population)%>%
  mutate(msa = as.character(msa))

everything_v3 = left_join(everything_v2, total.msa.population, by = c("year", "msa"))

#I think now you could just save everything_v3 somewhere and reference it
#group by year and MSA and then sum total.county.population

everything_v4 <- everything_v3 %>%
  group_by(year, msa)%>%
  mutate(numerator = sum(total.county.population))%>%
  mutate(completeness = numerator/total.msa.population)

#Format for put:
final.completeness.first.tri <- everything_v4%>%
  rename(location = msa)%>%
  rename(value = completeness)%>%
  select(year, location, value)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "completeness.prenatal.care.initiation.first.trimester")

final.completeness.first.tri <- as.data.frame(final.completeness.first.tri)

#there's 33 MSAs here which matches the MSAs of interest
# check.how.many <- everything_v5%>%
#   ungroup()%>%
#   select(location)%>%
#   count(location)
  
  data.manager$put.long.form(
    data = final.completeness.first.tri,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')

  

# NO.PRENATAL.CARE - TOTAL--------------------------------------------------------

  #2 
  prenatal.counties = as.data.frame.table(data.manager$data$no.prenatal.care$estimate$cdc.wonder.natality$cdc.fertility$year__location)
  
  prenatal.counties <- prenatal.counties %>%
    filter(location != "US")%>% #Remove US
    mutate(outcome = "counties we have prenatal data for")%>%
    select(-Freq)%>%
    mutate(county.we.have.prenatal.for = "1")
  
  
  #Match the counties we have prenatal data for to their population
  everything_v1 = left_join(prenatal.counties, county.population, by=c("year", "location"))
  
  everything_v1 <- everything_v1%>%
    rename(county.code = location)%>%
    mutate(county.code = as.character(county.code))
  
  #Remove any counties that are NOT in an MSA
  everything_v1$is.county.in.an.msa = (everything_v1$county.code %in% unlisted.counties.in.msas$county)
  
  
  everything_v1 <- everything_v1 %>%
    filter(is.county.in.an.msa == T) #select only counties that are in an MSA
  
  #mutate(what.msa.is.this.county.in = locations::get.overlapping.locations(county.code, "CBSA")) #this wasn't working
  
  
  #Merge everything_v1 with counties.in.each.msa.df
  everything_v2 = left_join(everything_v1, counties.in.each.msa.df, by = 'county.code')
  
  everything_v2$msa = as.character(everything_v2$msa)

  
  everything_v3 = left_join(everything_v2, total.msa.population, by = c("year", "msa"))
  
  #I think now you could just save everything_v3 somewhere and reference it
  #group by year and MSA and then sum total.county.population
  
  everything_v4 <- everything_v3 %>%
    group_by(year, msa)%>%
    mutate(numerator = sum(total.county.population))%>%
    mutate(completeness = numerator/total.msa.population)
  
  #Format for put:
  final.completeness.no.prenatal.care <- everything_v4%>%
    rename(location = msa)%>%
    rename(value = completeness)%>%
    select(year, location, value)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(value))%>%
    mutate(outcome = "completeness.no.prenatal.care")
  
  final.completeness.no.prenatal.care <- as.data.frame(final.completeness.no.prenatal.care)
  
  data.manager$put.long.form(
    data = final.completeness.no.prenatal.care,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
  
  
