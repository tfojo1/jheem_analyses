#This code is to restructure age groups from recent census data (2020-2022)
#So that we can calculate an adult.population for these recent years
#This will pull data from the census manager and then put data into the
#surveillance manager with outcome = adult.population

#YOU CAN REMOVE THIS AS LONG AS YOU SOURCE IT AFTER THE IMMIGRATION AGE RESTRATIFICATION IN THE SURVEILLANCE MANAGER

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


# Structure Data to Restratify Ages ---------------------------------------

desired.ages.for.census <- c('0-4 years', '5-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
          '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
          '75-79 years', '80-84 years', '85+ years')

age.array.from.census.manager = census.manager$data$population$estimate$census.population$stratified.census$year__location__age

restratify.age.array.for.census <- restratify.age.counts(age.array.from.census.manager, desired.age.brackets= desired.ages.for.census, smooth.infinite.age.to =100)

adult.age.groups.census.20.22 = restratify.age.array.for.census[ , , 3:17] #subset by only adult age groups
adult.population.census.20.22 = apply(adult.age.groups.census.20.22, MARGIN = c("year","location"), sum) #sum the adult age groups to get adult.population for 2020-2022


# Put adult.population into data manager ----------------------------------

fixed.adult.population.census.20.22 <- as.data.frame.table(adult.population.census.20.22)%>%
  mutate(value = round(Freq))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(outcome = "adult.population")%>%
  select(-Freq)

fixed.adult.population.census.20.22 = list(fixed.adult.population.census.20.22)

for (data in fixed.adult.population.census.20.22) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census', #the ontology here should really be stratified census but that doesn't exist at the surveillance manager level
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
