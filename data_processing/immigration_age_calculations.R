##This is the function that Todd wrote; More code included to run the functions that his function references
##This is from the HELPERS_age_year_helpers.R file (this is in JHEEM2 dev branch I think)

source('data_processing/HELPERS_age_year_helpers_COPY.R')
################################################################################
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
################################################################################
##The immigration/emigration data has a 5-17 age bracket- need to split this out
##And then need to create adult.immigration/emigration outcome to just show 13+
################################################################################

#CREATE VECTOR OF WHAT YOU WANT AGES TO BE
desired.ages <- c("1-12 years", "13-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                  "65-69 years", "70-74 years", "75+ years")

#AGE RE-CALCULATION: EMIGRATION
age.em.df <- data.list.move.clean[grep("emigration_age", names(data.list.move.clean))] 
age.em.df <- as.data.frame(age.em.df$msa_emigration_age_11.15) 

age.em.df <- age.em.df%>%
  mutate(age = factor(age.em.df$age, levels =c("1-4 years", 
                                               "5-17 years", "18-19 years", "20-24 years", "25-29 years",
                                               "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                                               "50-54 years", "55-59 years", "60-64 years","65-69 years", 
                                               "70-74 years", "75+ years")))%>% 
  select(year, location, age, value)%>% 
  arrange(age,location, year)

agearray.dimnames.em <- list(year = unique(age.em.df$year), location = unique(age.em.df$location), age = unique(age.em.df$age))
agearray.emm<- array(data = age.em.df$value, 
                     dim= sapply(agearray.dimnames.em, length),
                     dimnames= agearray.dimnames.em)

#APPLY RESTRATIFY AGE FUNCTION: EMIGRATION
restratify.age.array.em <- restratify.age.counts(agearray.emm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#AGE Re-CALCULATION: IMMIGRATION
age.df.imm <- data.list.move.clean[grep("immigration_age", names(data.list.move.clean))] 
age.df.imm <- as.data.frame(age.df.imm$msa_immigration_age_11.15) 
age.df.imm <- age.df.imm%>%
  mutate(age = factor(age.df.imm$age, levels =c("1-4 years", 
                                                "5-17 years", "18-19 years", "20-24 years", "25-29 years", 
                                                "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                                                "50-54 years", "55-59 years", "60-64 years","65-69 years", 
                                                "70-74 years", "75+ years")))%>% 
  select(year, location, age, value)%>% 
  arrange(age,location, year)

agearray.dimnames.imm <- list(year = unique(age.df.imm$year), location = unique(age.df.imm$location), age = unique(age.df.imm$age))
agearray.imm<- array(data = age.df.imm$value, 
                     dim= sapply(agearray.dimnames.imm, length),
                     dimnames= agearray.dimnames.imm)

#APPLY RESTRATIFY AGE FUNCTION: IMMIGRATION
restratify.age.array.imm <- restratify.age.counts(agearray.imm, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

#PROPORTION ARRAY: EMIGRATION
total.array.em = apply(restratify.age.array.em, MARGIN = c("year", "location"), sum)
total.array.em #Denominator for proportion

adult.array.em = restratify.age.array.em[ , , 2:15]
adult.array.new.em = apply(adult.array.em, MARGIN = c("location"), sum) #Numerator for proportion- only people ages 13+

proportion.adult.array.em = (adult.array.new.em/total.array.em) #Proportion array
adult.prop.df.em = as.data.frame.table(proportion.adult.array.em)

#PROPORTION ARRAY: IMMIGRATION
total.array.imm = apply(restratify.age.array.imm, MARGIN = c("year", "location"), sum)
total.array.imm #Denominator for proportion

adult.array.imm = restratify.age.array.imm[ , , 2:15]
adult.array.new.imm = apply(adult.array.imm, MARGIN = c("location"), sum) #Numerator for proportion

proportion.adult.array.imm = (adult.array.new.imm/total.array.imm) #Proportion array
adult.prop.df.imm = as.data.frame.table(proportion.adult.array.imm)

#TOTAL: EMIGRATION
total.adults.em = as.data.frame.table(restratify.age.array.em)
total.adults.df.em <-total.adults.em %>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(age.group.sum = as.numeric(round(Freq)))%>%
  filter(age != "1-12 years")%>%
  select(-Freq, -age)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  group_by(location)%>%
  mutate(value = sum(age.group.sum))%>%
  select(-age.group.sum)

total.adults.df.em  = as.data.frame(total.adults.df.em)

#TOTAL: IMMIGRATION (+Proportion array)
total.adults.imm = as.data.frame.table(restratify.age.array.imm)
total.adults.df.imm <-total.adults.imm%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(age.group.sum = as.numeric(round(Freq)))%>%
  filter(age != "1-12 years")%>%
  select(-Freq, -age)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  group_by(location)%>%
  mutate(value = sum(age.group.sum))%>%
  select(-age.group.sum)

total.adults.df.imm  = as.data.frame(total.adults.df.imm)

#SEX: EMIGRATION
sex.df.em <- data.list.move.clean[grep("emigration_sex", names(data.list.move.clean))] 
sex.df.em <- as.data.frame(sex.df.em$msa_emigration_sex_11.15) 
sex.adult.prop.em = left_join(sex.df.em, adult.prop.df.em, by="location")
sex.adult.prop.em <- sex.adult.prop.em %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, sex, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)

#SEX: IMMIGRATION
sex.df.imm <- data.list.move.clean[grep("immigration_sex", names(data.list.move.clean))] 
sex.df.imm <- as.data.frame(sex.df.imm$msa_immigration_sex_11.15) 
sex.adult.prop.imm = left_join(sex.df.imm, adult.prop.df.imm, by="location")
sex.adult.prop.imm <- sex.adult.prop.imm %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, sex, value_new)%>%
  mutate(value = as.numeric(value_new))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select(-value_new, -year.x)

#RACE: EMIGRATION
race.df.em <- data.list.move.clean[grep("emigration_race", names(data.list.move.clean))] 
race.df.em <- as.data.frame(race.df.em$msa_emigration_race_11.15) 
race.adult.prop.em = left_join(race.df.em, adult.prop.df.em, by="location")
race.adult.prop.em <- race.adult.prop.em %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, race, value_new)%>%
  rename(value = value_new)%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))

#RACE: IMMIGRATION
race.df.imm <- data.list.move.clean[grep("immigration_race", names(data.list.move.clean))] 
race.df.imm <- as.data.frame(race.df.imm$msa_immigration_race_11.15) 
race.adult.prop.imm = left_join(race.df.imm, adult.prop.df.imm, by="location")
race.adult.prop.imm <- race.adult.prop.imm %>%
  mutate(value_new = round(value * Freq))%>%
  select(outcome, year.x, location, race, value_new)%>%
  rename(value = value_new)%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))

#ETHNICITY: IMMIGRATION
eth.df.imm <- data.list.move.clean[grep("immigration_eth", names(data.list.move.clean))] 
eth.df.imm <- as.data.frame(eth.df.imm$msa_immigration_eth_11.15) 
eth.adult.prop.imm = left_join(eth.df.imm, adult.prop.df.imm, by="location")

eth.adult.prop.imm <- eth.adult.prop.imm %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.immigration')%>%
  mutate(location = as.character(location))%>%
  select (-year.x, -year.y, -Freq, -X.msa_immigration_eth_11.15.)


#ETHNICITY: EMIGRATION
eth.df.em <- data.list.move.clean[grep("emigration_eth", names(data.list.move.clean))] 
eth.df.em <- as.data.frame(eth.df.em$msa_emigration_eth_11.15) 
eth.adult.prop.em = left_join(eth.df.em, adult.prop.df.em, by="location")

eth.adult.prop.em <- eth.adult.prop.em %>%
  mutate(value = round(Freq*value))%>%
  mutate(year = as.character(year.x))%>%
  mutate(outcome = 'adult.emigration')%>%
  mutate(location = as.character(location))%>%
  select(-year.x, -year.y, -Freq, -X.msa_emigration_eth_11.15.)


#AGE: IMMIGRATION
new.age.df.imm = as.data.frame.table(restratify.age.array.imm)

new.age.prop.1.imm <- new.age.df.imm  %>%
  filter(age != "1-12 years")%>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

#AGE: EMIGRATION
new.age.df.em = as.data.frame.table(restratify.age.array.em)

new.age.prop.1.em <- new.age.df.em %>%
  filter(age != "1-12 years")%>%
  mutate(outcome = "adult.emigration")%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(age = as.character(age))%>%
  select(-Freq)

#OTHER RACE: EMIGRATION (Note this pulls dataset from immigration code)
other.emigration <- other_race[grep("emigration", names(other_race))] 
other.emigration <- as.data.frame(other.emigration$other.race.emigration)
other.emigration.adults = left_join(other.emigration, adult.prop.df.em, by = "location")

other.emigration.adults <- other.emigration.adults %>%
  mutate(outcome = "adult.emigration")%>%
  mutate(value = round(value*Freq))%>% #this is what i keep changing
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)

#OTHER RACE: IMMIGRATION (Note this pulls dataset from immigration code)
other.immigration <- other_race[grep("immigration", names(other_race))] 
other.immigration <- as.data.frame(other.immigration$other.race.immigration)
other.immigration.adults = left_join(other.immigration, adult.prop.df.imm, by = "location")

other.immigration.adults <- other.immigration.adults %>%
  mutate(outcome = "adult.immigration")%>%
  mutate(value = round(value*Freq))%>%
  mutate(location = as.character(location))%>%
  mutate(year = as.character(year.y))%>%
  select (-year.x, -year.y, -Freq)

#CREATE LISTS FOR IMMIGRATION/EMIGRATION TO PUT INTO MANAGER
adult.imm.em.list = list(
  "df.total.imm" = total.adults.df.imm, 
  "df.age.imm" = new.age.prop.1.imm,
  "df.eth.imm" = eth.adult.prop.imm, 
  "df.race.imm" = race.adult.prop.imm, 
  "df.sex.imm" = sex.adult.prop.imm,
  "df.other.race.imm"= other.immigration.adults,
  
  "df.total.em" = total.adults.df.em, 
  "df.age.em" = new.age.prop.1.em,
  "df.eth.em" = eth.adult.prop.em, 
  "df.race.em" = race.adult.prop.em, 
  "df.sex.em" = sex.adult.prop.em ,
  "df.other.em"= other.emigration.adults)


#PUT INTO DATA MANAGER
for (data in adult.imm.em.list) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    #dimension.values = list(),
    dimension.values.to.distribute = list(race=c('Other')),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}

###############################################################################
##Update March 11 2024: Using proportion of adults calculated above to apply to 
#new years of data that are not available with age strata
#you don't need to do this for 11-15 bc it's already done
#Uses these from above: adult.prop.df.em; adult.prop.df.imm
###############################################################################
proportion.df.immigration <- adult.prop.df.imm %>%
  select(location, Freq)%>%
  rename(adult.proportion = Freq)

proportion.df.emigration <- adult.prop.df.em %>%
  select(location, Freq)%>%
  rename(adult.proportion = Freq)

#immigration for 12.16, 13.17, 14.18, 15.19, 16.29
select.total.years <- data.list.move.clean[grep("_total", names(data.list.move.clean))]
select.years.of.interest <- select.total.years[grep("12.16|13.17|14.18|15.19|16.20", names(select.total.years))]

adult.movement.data.unstratified.years = lapply(select.years.of.interest, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  if(grepl("immigration", filename)) {
    data <- data%>%
      full_join(proportion.df.immigration, by = "location")%>%
      select(-outcome)%>%
      mutate(outcome = "adult.immigration")
  }
  
  if(grepl("emigration", filename)) {
    data <- data%>%
      full_join(  proportion.df.emigration, by = "location")%>%
      select(-outcome)%>%
      mutate(outcome = "adult.emigration")
  }
  
data <- data %>%
  mutate(new.value = value * adult.proportion)%>%
  rename(old.value = value)%>%
  rename(value = new.value) %>%
  filter(!is.na(adult.proportion))%>%
  filter(!is.na(old.value))
  

data= as.data.frame(data)
  
list(filename, data)

})

#PUT INTO DATA MANAGER

updated.adult.movement = lapply(adult.movement.data.unstratified.years, `[[`, 2)  

for (data in updated.adult.movement) {
  
  data.manager$put.long.form( #Not redistributing race bc none of these have race
    data = data,
    ontology.name = 'census.immigration.adults',
    source = 'census.population',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}
