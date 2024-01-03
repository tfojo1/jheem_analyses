##This is the function that Todd wrote; More code included to run the functions that his function references
##This is from the HELPERS_age_year_helpers.R file (this is in JHEEM2 dev branch I think)

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
##And then need to create adult.immigration outcome to just show 13+
################################################################################
#Create vector of what you want the ages to be; Todd's function needs this
#Specify anything in his function that does not have a default; it needs an array and returns an array

desired.ages <- c("1-12 years", "13+ years")

#Pull the data frame you made and turn it into an array
                                          ##USE NAMES TO IMPROVE THIS SECTION##
age.df = data.list.move.clean[[1]]
age.df=age.df[[2]]

age.df <- age.df%>%
  mutate(age = factor(age.df$age, levels =c("1-4 years", "5-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                            "65-69 years", "70-74 years", "75+ years")))

#Must make sure the structure matches the array- need to sort in this order for it to be correct
age.df <- age.df %>% 
  select(year, location, age, value)%>% 
  arrange(age,location, year)

#Now make df into an array; but first set dimnames. Dim is the number of categories there are
#You can use 'length' to determine the number easily
agearray.dimnames <- list(year = unique(age.df$year), location = unique(age.df$location), age = unique(age.df$age))

agearray<- array(data = age.df$value, 
                 dim= sapply(agearray.dimnames, length),
                 dimnames= agearray.dimnames)

##RIGHT NOW IN ORDER TO RUN THIS YOU HAVE TO SOURCE 'HELPERS_age_year_helpers.R" that Andrew emailed 1.3
restratify.age.array <- restratify.age.counts(agearray, desired.age.brackets= desired.ages, smooth.infinite.age.to =100)

##Now use to calculate the proportion of immigration that is only 13+ (adults)

total.array = apply(restratify.age.array, MARGIN = c("year", "location"), sum )
total.array #Denominator for proportion

adult.array = restratify.age.array[,,"13+ years"]
adult.array #numerator for proportion

proportion.adult.array = (adult.array/total.array)
proportion.adult.array

##Create arrays for sex/race/eth (do age groups later)
sex.df = data.list.move.clean[[4]]
sex.df=sex.df[[2]]

sex.array.dimnames <- list(year = unique(sex.df$year), location = unique(sex.df$location), sex = unique(sex.df$sex))

sex.array<- array(data = sex.df$value, 
                 dim= sapply(sex.array.dimnames, length),
                 dimnames= sex.array.dimnames)

test.array = (c(proportion.adult.array)*sex.array)

#testing to see if it worked#
baltimore.test = proportion.adult.array[,"C.10180"]
baltimore.test
balitmore.sex = sex.array[,"C.10180",]
baltimore.test = test.array[,"C.10180",]

#can i ust make the arrya back into a df?
adult.prop.df = as.data.frame.table(proportion.adult.array)

sex.df = data.list.move.clean[[9]]
sex.df=sex.df[[2]]
test = merge()