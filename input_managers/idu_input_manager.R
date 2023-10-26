
# Remission/relapse from Shah et al: https://pubmed.ncbi.nlm.nih.gov/16364568/ 

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
  # Set up our return array
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  rv = array(0, dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  # Do the work
  overall.remission = (936/5553) # 936 remission events/5553 py --> 16.8/100py, Shah et al. 
  
  remission.age = 1/(c(0.69,0.87,0.98,1)) # inverted time ratio
  names(remission.age) = c("0-29 years","30-34 years","35-39 years","40+ years")
  
  remission.race = 1/c(1.34)
  names(remission.race) = c("black")
  
  remission.sex = 1/c(0.65)
  names(remission.sex) = c("msm")
  
  age.info.remission = parse.age.strata.names(names(remission.age))
  age.spans.remission = age.info.remission$upper - age.info.remission$lower


  remission.age.mapped = map.age.values(values = overall.remission*remission.age,
                                        given.age.lowers = age.info.remission$lower,
                                        given.age.uppers = age.info.remission$upper, 
                                        desired.ages = dim.names$age)
  
  rv = array(rep(remission.age.mapped,9), # this is going to fill in assuming that age is the first dimension 
             dim=sapply(dim.names, length),
             dimnames = dim.names)

  rv[,"black",] = rv[,"black",]*remission.race
  rv[,,"msm"] = rv[,,"msm"]*remission.sex
  
  # Return our calculated value
  rv 
}

get.idu.relapse.rates <- function(specification.metadata)
{
  # Set up our return array
  dim.names = specification.metadata$dim.names[c('age','race','sex')]
  rv = array(0, dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  # Do the work
  overall.relapse = (678/1727) # 678 relapse events/1727 py --> 39.2/100py, Shah et al. 

  relapse.race = 1/c(1.40)
  names(relapse.race) = c("black")
  
  relapse.sex = 1/c(1.22)
  names(relapse.sex) = c("female")
  
  rv = array(overall.relapse, # this is going to fill in assuming that age is the first dimension 
             dim=sapply(dim.names, length),
             dimnames = dim.names)
  
  rv[,"black",] = rv[,"black",]*relapse.race
  rv[,,"female"] = rv[,,"female"]*relapse.sex
  
  # Return our calculated value
  rv 
}


## old code

# The code for how you can parse the age strata we need
# age.info = parse.age.strata.names(dim.names$age)
# age.spans = age.info$upper - age.info$lower

# dim.names.age.mapping = list("model.ages" = dim.names$age,
#                              "data.ages" = names(remission.age))
# 
# age.mapping.remission = rbind(c(1,0,0,0), # 13-24 = 0-30
#                               c(0.5,0.5,0,0), # 25-34 = 0.5(0-30) + 0.5(30-34)
#                               c(0,0,0.5,0.5), # 35-44 = 0.5(35-39) + 0.5(40+)
#                               c(0,0,0,1), # 45-54 = 40+
#                               c(0,0,0,1)) # 55+ = 40+
# 
# age.mapping.remission = array(age.mapping.remission,
#                               dim=sapply(dim.names.age.mapping,length),
#                               dimnames=dim.names.age.mapping)
# 
# remission.age.rates = remission.age*overall.remission
# remission.age.mapped = rep(remission.age.rates,each=5)*age.mapping.remission
# remission.age.mapped = rowSums(remission.age.mapped)


