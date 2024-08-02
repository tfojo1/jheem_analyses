#0801
# Error with starting year in 1950 
# Error when we exclude location 
# Error in dim(val) = c(length(val)/length(private$i.compartment.value.function.aliases),  : 
                        # the dims contain missing values

##############################
# Source supporting files
source('../jheem_analyses/source_code.R') # a file that contains all the necessary functions for the JHEEM

##--------------------##
##--------------------##
##-- INITIAL SET-UP --##
##--------------------##
##--------------------##   
#'@title Create a Model Specification for Running the JHEEM
# should we inherit any existing model? 
#@TODD: should we start in 1970 or earlier?

SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield', #A single character value denoting the version
                                                  iteration=1,
                                                  # subversion = # runs the same model but can report seperate outcomes
                                                  description = "The initial SHIELD version, set up to model national epidemic", #A short text description of this version
                                                  
                                                  start.year = 1950,## @Todd: this is not set up to accept 1950
                                                  
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #similar to atlas [0-15][16-20]
                                                  
                                                  compartments.for.infected.only = list( #The names of the lists represent dimensions, and the values the compartments for each dimension.
                                                    continuum = c('undiagnosed', 'diagnosed.untreated','recovered'),
                                                    stage = c('primarySecondary', 'earlyLatent','lateLatent','tertiary')
                                                  ),
                                                  compartments.for.uninfected.only = list(),
                                                  compartments.for.infected.and.uninfected = list(
                                                    age = 'all.ages',#alias for the agegroups defined above 
                                                    race=c('black','hispanic','other'),
                                                    sex= c('heterosexual_male', 'msm', 'female')
                                                  ),
                                                 
                                                  compartment.value.aliases = list( 
                                                    #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                    # helps to define specifications for groups of compartments later on 
                                                    # named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities)
                                                    diagnosed.states='diagnosed',
                                                    undiagnosed.states=c('diagnosed.untreated','recovered'),
                                                    
                                                    early.stages=c('primarySecondary','earlyLatent'),
                                                    late.stages=c('lateLatent','tertiary')
                                                    # 
                                                    # primarySecondary.stages = 'primarySecondary',
                                                    # earlyLatent.stages = 'earlyLatent',
                                                    # lateLatent.stages = 'lateLatent',
                                                    # tertiary.stages = 'tertiary',
                                                  )
)

##----------------------##
##-- Fix Strata Sizes --##
##----------------------##
#'@title Set Whether to Fix Strata Sizes During a Time Period
# a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data
# Set Whether to Fix Strata Sizes During a Time Period
# Todd: what's this? 
register.fixed.model.strata(SHIELD.SPECIFICATION, #The jheem.specification object
                            applies.after.time = -Inf,
                            applies.before.time = 2007, #single numeric values giving the time frame over whether this setting applies
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex') #A character vector denoting which dimensions should be fixed. These should be dimensions common to both infected and uninfected groups. These are only used if fix.strata==TRUE
                            
)

# register an initial popualtion with 100 people in each box
# get to a place that we can source 
##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(SHIELD.SPECIFICATION)

# source('../jheem_analyses/applications/EHE/ehe_parameters_helpers.R')
# source('../jheem_analyses/applications/EHE/ehe_parameters.R')
# source('../jheem_analyses/applications/EHE/ehe_parameter_mapping.R')
# 
# register.calibrated.parameters.for.version('shield',
#                                            distribution = EHE.PARAMETERS.PRIOR,
#                                            apply.function = EHE.APPLY.PARAMETERS.FN,
#                                            sampling.blocks = EHE.PARAMETER.SAMPLING.BLOCKS,
#                                            calibrate.to.year = 2025,
#                                            join.with.previous.version = F)


#Quantity vs element?
# Element indivisible 
# Quantity is combination of didfferent element or quantities
