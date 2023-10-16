
# To test your code

if (1==2)
{
    source('applications/EHE/ehe_specification.R')
    specification.metadata = get.specification.metadata('ehe', 
                                                        location = 'c.12580')
}

# The functions we actually need to implement

# Returns a 3-d array, with dimensions: 'age', 'race', 'sex'
get.idu.incidence.rates <- function(specification.metadata)
{
    # Set up our return array
    dim.names = specification.metadata$dim.names[c('age','race','sex')]
    rv = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    
    # The code for how you can parse the age strata we need
    age.info = parse.age.strata.names(dim.names$age)
    age.spans = age.info$upper - age.info$lower
    
    # Do the work
    
    # Return our calculated value
    rv
}

get.idu.remission.rates <- function(specification.metadata)
{
    
}

get.idu.relapse.rates <- function(specification.metadata)
{
    
}