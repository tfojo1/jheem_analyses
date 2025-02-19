##Calculation of adap.proportion##
#adap.proportion = (# adap clients in a state) / (non.adap.clients in a state)


# UPDATE this as we have more years of data -------------------------------
years.of.adap.data <- c("2022") #adjust this as we add more years
locations.of.adap.data <- locations::get.all.for.type("state")

# -------------------------------------------------------------------------

# Total -------------------------------------------------------------------
total.adap = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location)%>%
  mutate(outcome = "adap.clients.numerator")%>%
  rename(numerator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

total.non = as.data.frame.table(data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location)%>%
  mutate(outcome = "non.adap.clients.denominator")%>%
  filter(location %in% locations.of.adap.data)%>%
  filter(year %in% years.of.adap.data)%>%
  rename(denominator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

total.proportion = merge(total.adap, total.non, by = c("location","year"))%>%
  mutate(value = numerator/denominator)%>%
  select(year, location, value)%>%
  mutate(outcome = "adap.proportion")


# Sex ---------------------------------------------------------------------
sex.adap = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex)%>%
  mutate(outcome = "adap.clients.numerator")%>%
  rename(numerator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

sex.non = as.data.frame.table(data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__sex)%>%
  mutate(outcome = "non.adap.clients.denominator")%>%
  filter(location %in% locations.of.adap.data)%>%
  filter(year %in% years.of.adap.data)%>%
  rename(denominator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

sex.proportion = merge(sex.adap, sex.non, by = c("location","year", "sex"))%>%
  mutate(value = numerator/denominator)%>%
  select(year, location, sex, value)%>%
  mutate(outcome = "adap.proportion")%>%
  mutate(sex = as.character(sex))


# Race --------------------------------------------------------------------
race.adap = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race)%>%
  mutate(outcome = "adap.clients.numerator")%>%
  rename(numerator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

race.non = as.data.frame.table(data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race)%>%
  mutate(outcome = "non.adap.clients.denominator")%>%
  filter(location %in% locations.of.adap.data)%>%
  filter(year %in% years.of.adap.data)%>%
  rename(denominator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

race.proportion = merge(race.adap, race.non, by = c("location","year", "race"))%>%
  mutate(value = numerator/denominator)%>%
  select(year, location, race, value)%>%
  mutate(outcome = "adap.proportion")%>%
  filter(value != 'Inf')%>%
  mutate(race = as.character(race))

# Age Group ---------------------------------------------------------------
age.adap = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age)%>%
  mutate(outcome = "adap.clients.numerator")%>%
  rename(numerator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

age.non = as.data.frame.table(data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age)%>%
  mutate(outcome = "non.adap.clients.denominator")%>%
  filter(location %in% locations.of.adap.data)%>%
  filter(year %in% years.of.adap.data)%>%
  rename(denominator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

age.proportion = merge(age.adap, age.non, by = c("location","year", "age"))%>%
  mutate(value = numerator/denominator)%>%
  select(year, location, age, value)%>%
  mutate(outcome = "adap.proportion")%>%
  mutate(age = as.character(age))


# FPL ---------------------------------------------------------------------
fpl.adap = as.data.frame.table(data.manager$data$adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__fpl)%>%
  mutate(outcome = "adap.clients.numerator")%>%
  rename(numerator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

fpl.non = as.data.frame.table(data.manager$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__fpl)%>%
  mutate(outcome = "non.adap.clients.denominator")%>%
  filter(location %in% locations.of.adap.data)%>%
  filter(year %in% years.of.adap.data)%>%
  rename(denominator = Freq)%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year))

fpl.proportion = merge(fpl.adap, fpl.non, by = c("location","year", "fpl"))%>%
  mutate(value = numerator/denominator)%>%
  select(year, location, fpl, value)%>%
  mutate(outcome = "adap.proportion")%>%
  mutate(fpl = as.character(fpl))

# Put ---------------------------------------------------------------------
adap.proportion.data.list = list(
  total.proportion,
  sex.proportion,
  race.proportion,
  age.proportion,
  fpl.proportion) 

for (data in adap.proportion.data.list) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}

