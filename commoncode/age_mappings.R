
#'@param values A numeric vector of values to map
#'@param given.age.lowers,given.age.uppers The upper and lower bounds for each of the ages for the age bracket for each element in values
#'@param desired.ages A character vector giving the ages we want to map to. Must be in the format of "x-y years" (can alternatively use the desired.age.lowers and desired.age.uppers arguments instead)
#'@param desired.age.lowers,desired.age.uppers The bounds of the age brackets we want to map to
#'@param population.sizes An optional argument. If known, the population sizes by age. Must be a named vector
#'@param population.ages A character vector giving the names of ages for population.sizes. Must be in the format of "x-y years" (can alternatively use the population.age.lowers and population.age.uppers arguments instead)
#'@param population.age.lowers,population.age.uppers The upper and lower bounds for each of the ages for the age bracket for each element in population.sizes
#'@param max.age The maximum age (if an age bracket upper bound is Infinite, we use this value instead)
#'@param error.prefix Text to prepend to any errors that are thrown from the function. Optional
#'
#'@details Note: age.lowers is inclusive and age.uppers is exclusive. So the age bracket containing anyone who is 25, 26, 27, 28, or 29 years old should have lower=25 and upper = 30

map.age.values <- function(values,
                           given.age.lowers,
                           given.age.uppers,
                           desired.ages = NULL,
                           desired.age.lowers=NULL,
                           desired.age.uppers=NULL,
                           population.sizes = NULL,
                           population.ages = NULL,
                           population.age.lowers=NULL,
                           population.age.uppers=NULL,
                           max.age = 100,
                           error.prefix = '')
{
    #-- Check arguments --#
    if (!is.numeric(values) || length(values)==0 || any(is.na(values)))
        stop(paste0(error.prefix, "'values' must be a non-empty, numeric vector with no NA values"))
  
    n.given = length(values)
    
    if (!is.numeric(given.age.lowers) || length(given.age.lowers)!=n.given || any(is.na(given.age.lowers)))
        stop(paste0(error.prefix, "'given.age.lowers' must be a numeric vector of length ",
                    n.given, "(the same length as 'values'), with no NA values"))
    
    if (!is.numeric(given.age.uppers) || length(given.age.uppers)!=n.given || any(is.na(given.age.uppers)))
        stop(paste0(error.prefix, "'given.age.uppers' must be a numeric vector of length ",
                    n.given, "(the same length as 'values'), with no NA values"))
    
    if (any(given.age.lowers>=given.age.uppers))
        stop(paste0(error.prefix, "The values of 'given.age.lowers' must each be lower than the corresponding value in 'given.age.uppers'"))
    
    # Check desired age bounds
    if (is.null(desired.age.lowers) || is.null(desired.age.uppers))
    {
        if (!is.null(desired.age.lowers) || !is.null(desired.age.uppers))
            stop(paste0(error.prefix,
                        "'desired.age.lowers' and 'desired.age.uppers' must either BOTH be NULL or NEITHER be NULL"))
      
        if (is.null(desired.ages))
            stop(paste0(error.prefix, "Either 'desired.ages' must be specified OR both 'desired.age.lowers' and 'desired.age.uppers' must be specified"))
        if (!is.character(desired.ages) || length(desired.ages)==0 || any(is.na(desired.ages)))
            stop(paste0(error.prefix, "'desired.ages' must be a non-empty character vector with no NA values"))
      
        parsed.ages = parse.age.strata.names(desired.ages)
        desired.age.lowers = parsed.ages$lower
        desired.age.uppers = parsed.ages$upper
    }
    else
    {
        if (!is.null(desired.ages))
            stop(paste0(error.prefix, "'desired.ages' cannot be specified if 'desired.age.lowers' and 'desired.age.uppers' are also specified"))
      
        if (!is.numeric(desired.age.lowers) || length(desired.age.lowers)==0 || any(is.na(desired.age.lowers)))
            stop(paste0(error.prefix, "'desired.age.lowers' must be a non-empty numeric vector with no NA values"))
      
        if (!is.numeric(desired.age.uppers) || length(desired.age.uppers)!=length(desired.age.lowers) || any(is.na(desired.age.uppers)))
            stop(paste0(error.prefix, "'desired.age.uppers' must be a numeric vector of length ",
                        length(desired.age.lowers), "(the same length as 'desired.age.lowers'), with no NA values"))
    }
    
    n.desired = length(desired.age.lowers)
    
    if (any(desired.age.lowers>=desired.age.uppers))
        stop(paste0(error.prefix, "The values of 'desired.age.lowers' must each be lower than the corresponding value in 'desired.age.uppers'"))
    
    #-- Parse population --#
    if (is.null(population.sizes))
    {
        rv = sapply(1:n.desired, function(i){
            weight = pmax(0, (pmin(max.age, desired.age.uppers[i], given.age.uppers) -
                        pmax(desired.age.lowers[i], given.age.lowers)) / 
                (min(max.age, desired.age.uppers[i]) - desired.age.lowers[i]))
            
            sum(weight * values)
        })
    }
    else
    {
        stop('need to implement this')
      
        if (!is.numeric(max.age) || length(max.age) != 1 || is.na(max.age) || is.infinite(max.age))
          stop(paste0(error.prefix,
                      "'max.age' must be a single, non-NA, non-infinite numeric value"))
        
        # There is some error checking we could do here to make sure we have 
        # a given that overlaps every desired
        # a population that overlaps every desired
        
        #-- Create 'from' intervals --#
        # such that each from interval falls into
        # - exactly one desired interval
        # - exactly one given interval
        # - exactly one population interval
        
        all.lowers = sort(union(desired.age.lowers,
                                union(given.age.lowers, population.age.lowers)))
        all.uppers = sort(union(desired.age.uppers,
                                union(given.age.uppers, population.age.uppers)))
        all.age.cut.points = sort(union(all.lowers, all.uppers))
        
        max.age.cut.point = max(all.age.cut.points[!is.infinite(all.age.cut.points)])
        if (max.age.cut.point > max.age)
          stop(paste0(error.prefix,
                      "'max.age' was given as ", max.age,
                      ", but some age upper bounds (",
                      max.age.cut.point, ") are greater than ", max.age))
        
        if (any(is.infinite(all.age.cut.points)) && max.age.cut.point==max.age)
          stop(paste0(error.prefix,
                      "Since some age upper bounds are infinite, 'max.age' (",
                      max.age, ") must be GREATER than all other age upper bounds, but it is equal to at least one upper bound"))
        
        from.lowers = all.age.cut.points[-length(all.age.cut.points)]
        from.uppers = all.age.cut.points[-1]
        
        n.from = length(from.lowers)
        
        keep.mask = sapply(1:n.from, function(j){
          j.is.in.desired = from.lowers[j] >= desired.age.lowers & from.uppers[j] <= desired.age.uppers
          any(j.is.in.desired)
        })
        from.lowers = from.lowers[keep.mask]
        from.uppers = from.uppers[keep.mask]
        n.from = sum(keep.mask)
        
        
        from.values = sapply(1:n.from, function(j){
          j.is.in.given = from.lowers[j] >= given.age.lowers & from.uppers[j] <= given.age.uppers
          if (sum(j.is.in.given)>1)
            stop(paste0(error.prefix, "Uh oh - there is an internal error in the map.age.values() function. Contact Todd or Andrew"))
          if (!any(j.is.in.given))
            NA
          else
            values[j.is.in.given]
        })
        
        keep.mask = !is.na(from.values)
        from.values = from.values[keep.mask]
        from.lowers = from.lowers[keep.mask]
        from.uppers = from.uppers[keep.mask]
        n.from = sum(keep.mask)
        
        from.population.sizes = sapply(1:n.from, function(j){
          j.is.in.population = population.age.lowers <= from.lowers[j] & population.age.uppers >= from.uppers[j]
          if (!any(j.is.in.population))
            stop(paste0(error.prefix, "No populations map to a given range"))
          if (sum(j.is.in.population)>1)
            stop(paste0(error.prefix, "Uh oh - there is an internal error in the map.age.values() function. Contact Todd or Andrew"))
          
          i = (1:length(j.is.in.population))[j.is.in.population]
          
          population.sizes[i] * (min(max.age, from.uppers[j])-from.lowers[j]) / (min(max.age, population.age.uppers[i])-population.age.lowers[i])
        })
        
        #-- Calculate --#
        rv = sapply(1:n.desired, function(i){
          from.is.in.i = from.lowers >= desired.age.lowers[i] & from.uppers <= desired.age.uppers[i]
          pop.in.i = sum(from.population.sizes[from.is.in.i])
          
          sum(from.values[from.is.in.i] * from.population.sizes[from.is.in.i] / pop.in.i)
        })
    }
    
    
    if (!is.null(desired.ages))
      names(rv) = desired.ages
    
    rv
}


# Test code
if (1==2)
{
    map.age.values(values=c(0.25, 0.5, 0.75, 1),
                   given.age.lowers = c(0,25,30,40),
                   given.age.uppers = c(25,30,40,Inf),
                   desired.ages = c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'))

}
