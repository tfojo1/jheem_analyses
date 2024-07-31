#This is stratified data, county level, census data, 2020-2023

#Estimate the adult.population from age grouped data
#Then find adult.population from the single year ages

#census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

# Source Helper Code Todd Wrote -------------------------------------------


##This is the function that Todd wrote; More code included to run the functions that his function references
##This is from the HELPERS_age_year_helpers.R file (this is in JHEEM2 dev branch I think)

source('data_processing/HELPERS_age_year_helpers_COPY.R')

#'@title Re-distribute counts according to one set of age brackets into a different set of age brackets
#'
#'@param counts Either (a) a dimensionless numeric vector, or (b) a numeric array, with  one dimension named "age". The "given" age brackets according to which these counts are distributed must be continuous - with no gaps between them
#'@param desired.age.brackets,given.age.brackets The age brackets according to which the ages in counts are given or desired to be restratified to. Either (1) a character vector giving names of brackets, with one name for each age bracket value or (b) a numeric vector of age endpoints, with one more element than the number of age bracket values. given.age.brackets should be present only if names(counts) is absent (if counts is a vector) or if dimnames(counts)$age is null (if counts is an array)
#'@param smooth.infinite.age.to In building a spline over counts, what value should an infinite bound in an age range be replaced with
#'@param allow.extrapolation A logical value indicating whether extrapolation should be allowed in restratifying. Extrapolating is required when one of the desired age brackets includes ages not present in any of the given age brackets
#'@param na.rm Whether NAs should be ignored in counts
#'@param method The method to be passed to \code{\link{splinefun}}. Must be a method that can produce a monotone spline (ie, either 'monoH.FC' or 'hyman')
#'@param error.prefix A character value to be prepended to any error messages
#'
#'@export
restratify.age.counts <- function(counts,
                                  desired.age.brackets,
                                  given.age.brackets = NULL,
                                  smooth.infinite.age.to = Inf,
                                  allow.extrapolation = F,
                                  na.rm = F,
                                  method=c('monoH.FC','hyman')[1],
                                  error.prefix = '')
{
  #-- Validate error.prefix --#
  if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
    stop("Error in restratify.age.counts: 'error.prefix' must be a single, non-NA character value")
  error.prefix = paste0(error.prefix, "Error in restratify.age.counts: ")
  #-- Validate counts --#
  if (!is.numeric(counts))
    stop(paste0(error.prefix, "'counts' must be either a dimensionless numeric vector or numeric array"))
  given.brackets.from = "'given.age.brackets'"
  n.brackets = length(counts)
  n.brackets.from = "length(counts)"
  if (is.array(counts))
  {
    if (is.null(names(dim(counts))))
      stop(paste0(error.prefix, "If 'counts' is an array, it must have named dimensions"))
    if (sum(names(dim(counts))=='age')!=1)
      stop(paste0(error.prefix, "If 'counts' is an array, it must have one (and only one) dimension named 'age'"))
    if (is.null(dimnames(counts)$age))
    {
      if (is.null(given.age.brackets))
        stop(paste0(error.prefix, "If the dimnames for the age dimension of 'counts' are  not set, then given.age.brackets must be specified (ie, cannot be NULL)"))
    }
    else
    {
      if (!is.null(given.age.brackets))
        stop(paste0(error.prefix, "If the dimnames for the age dimension of 'counts' are set, then given.age.brackets cannot be specified (the given age brackets will be derived from the dimnames of 'counts')"))
      given.age.brackets = dimnames(counts)$age
      given.brackets.from = "the dimnames for the age dimension of 'counts'"
    }
    n.brackets = dim(counts)['age']
    n.brackets.from = "the length of the 'age' dimension of 'counts'"
    if (n.brackets < 2)
      stop(paste0(error.prefix, "The 'age' dimension of 'counts' must have at least two values"))
  }
  else
  {
    if (is.null(names(counts)))
    {
      if (is.null(given.age.brackets))
        stop(paste0(error.prefix, "If the names of 'counts' is not set, then given.age.brackets must be specified (ie, cannot be NULL)"))
    }
    else
    {
      if (!is.null(given.age.brackets))
        stop(paste0(error.prefix, "If the names of 'counts' are set, then given.age.brackets cannot be specified (the given age brackets will be derived from the names of 'counts')"))
      given.age.brackets = names(counts)
      given.brackets.from = "the names of 'counts'"
    }
    if (n.brackets < 2)
      stop(paste0(error.prefix, "'counts' must have at least two values"))
  }
  #-- Parse/Validate given.age.brackets --#
  parsed.given.brackets = parse.age.brackets(age.brackets = given.age.brackets,
                                             n.brackets = n.brackets,
                                             require.contiguous = T,
                                             smooth.infinite.age.to = smooth.infinite.age.to,
                                             require.non.infinite.ages = T,
                                             error.prefix = error.prefix,
                                             required.length.name.for.error = n.brackets.from,
                                             age.brackets.name.for.error = given.brackets.from)
  #-- Parse/Validate desired.age.brackets --#
  parsed.desired.brackets = parse.age.brackets(age.brackets = desired.age.brackets,
                                               n.brackets = NA,
                                               require.contiguous = F,
                                               smooth.infinite.age.to = smooth.infinite.age.to,
                                               require.non.infinite.ages = T,
                                               error.prefix = error.prefix,
                                               required.length.name.for.error = NA,
                                               age.brackets.name.for.error = 'desired.age.brackets')
  if (!allow.extrapolation &&
      (any(parsed.desired.brackets$lower < parsed.given.brackets$lower[1]) ||
       any(parsed.desired.brackets$upper > parsed.given.brackets$upper[n.brackets])))
    stop(paste0(error.prefix, "Cannot extrapolate counts for ages beyond the given ages (ie, less than ",
                parsed.given.brackets$lower[1], " or greater than ", parsed.given.brackets$upper[n.brackets], ")"))
  #-- Make the new values --#
  if (is.array(counts))
  {
    orig.dim = dim(counts)
    orig.dim.names = dimnames(counts)
    if (is.null(orig.dim.names))
      orig.dim.names = lapply(orig.dim, function(d){NULL})
    non.age.dimensions = setdiff(names(orig.dim), 'age')
    raw = apply(counts, non.age.dimensions, function(val){
      smoother = do.get.cumulative.age.counts.smoother(counts = val[parsed.given.brackets$order],
                                                       endpoints = parsed.given.brackets$endpoints,
                                                       method = method)
      smoother(parsed.desired.brackets$upper) - smoother(parsed.desired.brackets$lower)
    })
    raw.dim = c(age = length(parsed.desired.brackets$names),
                orig.dim[non.age.dimensions])
    raw.dim.names = c(list(age = parsed.desired.brackets$names),
                      orig.dim.names[non.age.dimensions])
    dim(raw) = raw.dim
    dimnames(raw) = raw.dim.names
    if (names(orig.dim)[1] != 'age')
    {
      new.dim.names = raw.dim.names[names(orig.dim)]
      new.dim = raw.dim[names(orig.dim)]
      rv = apply(raw, names(orig.dim), function(x){x})
      dim(rv) = new.dim
      dimnames(rv) = new.dim.names
      rv
    }
    else
      raw
  }
  else
  {
    smoother = do.get.cumulative.age.counts.smoother(counts = counts[parsed.given.brackets$order],
                                                     endpoints = parsed.given.brackets$endpoints,
                                                     method = method)
    rv = smoother(parsed.desired.brackets$upper) - smoother(parsed.desired.brackets$lower)
    names(rv) = parsed.desired.brackets$names
    rv
  }
}


# ESTIMATED DATA ----------------------------------------------------------
#ESTIMATED DATA: adult.population 2020-2023 by TOTAL
desired.ages.for.census <- c('0-4 years', '5-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
                             '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
                             '75-79 years', '80-84 years', '85+ years')

#Update for 7-23-24: Creating a racial mapping to align this race data with the census ontology.
race.mappings.to.census = c('White' = 'white',
                            'Black' = 'black',
                            'American Indian and Alaska Native' = 'american indian or alaska native',
                            'Native Hawaiian and Other Pacific Islander' = 'asian or pacific islander',
                            'Asian' = 'asian or pacific islander') 

#ESTIMATED DATA: adult.population 2020-2023 by RACE
population.by.race.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race

restratify.age.race <- restratify.age.counts(population.by.race.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.race.20.23 = restratify.age.race[ , ,3:17,] #subset by only adult age groups
adult.pop.race.20.23 = apply(restratify.adult.pop.race.20.23, MARGIN = c("year","location", "race"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.race.20.23 <- as.data.frame.table(adult.pop.race.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
adult.pop.race.20.23$race = race.mappings.to.census[adult.pop.race.20.23$race]
adult.pop.race.20.23<- adult.pop.race.20.23 %>%
  group_by(year, location, race)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)
adult.pop.race.20.23<- as.data.frame(adult.pop.race.20.23[!duplicated(adult.pop.race.20.23), ])


#ESTIMATED DATA: adult.population 2020-2023 by ETHNICITY
population.by.ethnicity.array = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity

restratify.age.ethnicity <- restratify.age.counts(population.by.ethnicity.array, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restratify.adult.pop.ethnicity.20.23 = restratify.age.ethnicity[ , , 3:17,] #subset by only adult age groups
adult.pop.ethnicity.20.23 = apply(restratify.adult.pop.ethnicity.20.23, MARGIN = c("year","location", "ethnicity"), sum) #sum the adult age groups to get adult.population for 2020-2023

adult.pop.ethnicity.20.23 <- as.data.frame.table(adult.pop.ethnicity.20.23)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(ethnicity = as.character(tolower(ethnicity)))%>%
  select(-Freq)

#ESTIMATED DATA: adult.population 2020-2023 by RACE+ETHNICITY

array.race.eth = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity

restratify.race.eth <- restratify.age.counts(array.race.eth, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restrat.race.eth = restratify.race.eth[ , ,3:17, ,] #subset by only adult age groups
fixed.race.eth = apply(restrat.race.eth, MARGIN = c("year","location", "race", 'ethnicity'), sum) #sum the adult age groups to get adult.population for 2020-2023


fixed.race.eth <- as.data.frame.table(fixed.race.eth)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
fixed.race.eth$race = race.mappings.to.census[fixed.race.eth$race]
fixed.race.eth<- fixed.race.eth %>%
  mutate(ethnicity = tolower(ethnicity))%>%
  group_by(year, location, race, ethnicity)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)

fixed.race.eth<- as.data.frame(fixed.race.eth[!duplicated(fixed.race.eth), ])

#ESTIMATED DATA: adult.population 2020-2023 by RACE+ETHNICITY+AGE
array.race.eth.age = census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity

restratify.race.eth.age <- restratify.age.counts(array.race.eth.age, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

restrat.race.eth.age = restratify.race.eth.age[ , ,3:17, ,] #subset by only adult age groups
fixed.race.eth.age = apply(restrat.race.eth.age, MARGIN = c("year","location", "race", 'ethnicity', 'age'), sum) #sum the adult age groups to get adult.population for 2020-2023


fixed.race.eth.age <- as.data.frame.table(fixed.race.eth.age)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq)

#Update for 7-23-24: To align this race data with the current census ontology:
fixed.race.eth.age$race = race.mappings.to.census[fixed.race.eth.age$race]

fixed.race.eth.age<- fixed.race.eth.age %>%
  mutate(ethnicity = tolower(ethnicity))%>%
  group_by(year, location, race, ethnicity, age)%>%
  mutate(value.new = sum(value))%>%
  select(-value)%>%
  rename(value = value.new)

fixed.race.eth.age<- as.data.frame(fixed.race.eth.age[!duplicated(fixed.race.eth.age), ])

##################################################################################

# PUT for all stratified, estimated data ----------------------------------

estimated.adult.pop.stratified.put = list(
  #adult.pop.total.20.23,
  #adult.pop.sex.20.23,
  #adult.pop.age.20.23,
  adult.pop.race.20.23, #race only
  adult.pop.ethnicity.20.23, #eth only
  fixed.race.eth, #race+eth
  fixed.race.eth.age) #this is race +eth+age


for (data in estimated.adult.pop.stratified.put) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
##################################################################################


# SINGLE YEAR AGE GROUP DATA ----------------------------------------------

#adult.pop by SINGLE YEAR AGE (for TOTAL, SEX, and AGE)
single.year.age = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age)
single.year.age.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age__sex)

#age
single.year.age <- single.year.age %>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(age = as.character(age))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(Freq))%>%
  select(-Freq)%>%
  mutate(outcome = "adult.population")

single.year.age<- single.year.age[!duplicated(single.year.age), ]


#total
single.year.total <- single.year.age%>%
  group_by(year, location)%>%
  mutate(value.fixed = sum(value))%>%
  select(-age, -value)%>%
  rename(value = value.fixed)

single.year.total<- single.year.total[!duplicated(single.year.total), ]

#sex
single.year.sex <- single.year.age.sex%>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(age = as.character(age))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(value = as.numeric(Freq))%>%
  select(-Freq)%>%
  mutate(outcome = "adult.population")%>%
  group_by(year, location, sex)%>%
  mutate(new.value = sum(value))%>%
  select(outcome, year, location, sex, new.value)%>%
  rename(value = new.value)

single.year.sex<- single.year.sex[!duplicated(single.year.sex), ]

#Age + Sex
single.year.age.sex <- single.year.age.sex%>%
  rename(value = Freq)%>%
  filter(age != "< 1 year" & age != "1 year" & age != "2 years" &  age != "3 years" &  age != "4 years" &  age != "5 years" & 
           age != "6 years" &  age != "7 years" &  age != "8 years" &  age != "9 years" &  age != "10 years" &  age != "11 years" & 
           age != "12 years") %>%
  filter(year != "remove")%>%
  mutate(outcome = 'adult.population')%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(sex = as.character(sex))%>%
  mutate(age = as.character(age))


##
single.year.total= as.data.frame(single.year.total)
single.year.age= as.data.frame(single.year.age)
single.year.sex= as.data.frame(single.year.sex)
single.year.age.sex = as.data.frame(single.year.age.sex)

#Put Single Year Age Group Data
adult.pop.by.single.year.age = list(
  single.year.total, #by total
  single.year.age, #by age
  single.year.sex, #by sex
  single.year.age.sex #by age+sex
)

for (data in adult.pop.by.single.year.age) {
  
  surveillance.manager$put.long.form(
    data = data,
    ontology.name = 'census', 
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


# ADDING NEW STRATIFICATIONS ----------------------------------------------
#Need: race+eth
#   race+eth+sex
#   age+race+eth

