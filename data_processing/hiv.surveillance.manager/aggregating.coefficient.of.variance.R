#Update for 2-11-25: Andrew/Todd requested the Coefficient of Variance for Awareness to be aggregated from County to MSA for Riverside C.40140 only since we have data for both counties:
#update for 2-18-25: Adding the same for Miami


# RIVERSIDE: C.40140 -----------------------------------------------------
Riverside_Counties <- locations::get.contained.locations("C.40140", "COUNTY") #06065, 06071

riverside.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                                  metric = "coefficient.of.variance",
                                                                  dimension.values=list(location=Riverside_Counties),
                                                                  keep.dimensions='year')

riverside.msa.coefficient.of.variance <- as.data.frame.table(riverside.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.40140")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = riverside.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')

#  MIAMI: C.33100 --------------------------------------------------------
miami.counties <- locations::get.contained.locations("C.33100", "COUNTY") #12011, 12086, 12099

miami.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                              metric = "coefficient.of.variance",
                                                              dimension.values=list(location=miami.counties),
                                                              keep.dimensions='year')

miami.msa.coefficient.of.variance <- as.data.frame.table(miami.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.33100")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = miami.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')


# SAN DIEGO ---------------------------------------------------------------
san.diego.counties <- locations::get.contained.locations("C.41740", "COUNTY") 

san.diego.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                              metric = "coefficient.of.variance",
                                                              dimension.values=list(location=san.diego.counties),
                                                              keep.dimensions='year')

san.diego.msa.coefficient.of.variance <- as.data.frame.table(san.diego.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.41740")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = san.diego.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')

# LOS ANGELES -------------------------------------------------------------
los.angeles.counties <- locations::get.contained.locations("C.31080", "COUNTY") 

los.angeles.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                                  metric = "coefficient.of.variance",
                                                                  dimension.values=list(location=los.angeles.counties),
                                                                  keep.dimensions='year')

los.angeles.msa.coefficient.of.variance <- as.data.frame.table(los.angeles.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.31080")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = los.angeles.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')

# LAS VEGAS ---------------------------------------------------------------

las.vegas.counties <- locations::get.contained.locations("C.29820", "COUNTY") 

las.vegas.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                                    metric = "coefficient.of.variance",
                                                                    dimension.values=list(location=las.vegas.counties),
                                                                    keep.dimensions='year')

las.vegas.msa.coefficient.of.variance <- as.data.frame.table(las.vegas.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.29820")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = las.vegas.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')
