# think about the statistics that we want to record
# proportionof people aware of diagnosis 
# proportion loving with diag and not track.integrated.outcome(prevalece of syphilis 
#                                                              )
# we want to take the average of two timesteps i
# 
# data on testing/screening
# screening based on HIV --- 
# break down diagnosis to screening to sphmptamatoc testing rate   

#@Todd: how to remove all the warnings 

# > specification.metadata=get.specification.metadata("shield","US") 
##############################
# Source supporting files
source('../jheem_analyses/source_code.R') # a file that contains all the necessary functions for the JHEEM

##--------------------##
##-- INITIAL SET-UP --##
##--------------------##   
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield',
                                                  iteration=1,
                                                  description = "The initial SHIELD version, set up to model national epidemic",
                                                  start.year = 1940,
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #similar to atlas [0-15][16-20]
                                                  
                                                  compartments.for.infected.only = list( 
                                                    continuum = c('undiagnosed', 'diagnosed.untreated'),
                                                    stage = c('ps', 'el','ll','ter')
                                                  ),
                                                  compartments.for.uninfected.only = list(
                                                    profile=c('susceptible','diagnosed.treated')),
                                                  compartments.for.infected.and.uninfected = list(
                                                    location = "US", 
                                                    age = 'all.ages',
                                                    race=c('black','hispanic','other'),
                                                    sex= c('heterosexual_male', 'msm', 'female')
                                                  ),
                                                  
                                                  compartment.value.aliases = list( 
                                                    #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                    # helps to define specifications for groups of compartments later on 
                                                    diagnosed.treated.states=c('diagnosed.treated'),
                                                    
                                                    ps.stages=c('ps'),
                                                    el.stages=c('el'),
                                                    late.stages=c('ll','ter')
                                                  )
)

##----------------------##
##-- Fix Strata Sizes --##
##----------------------##
#'@title Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data
register.fixed.model.strata(SHIELD.SPECIFICATION, 
                            applies.after.time = -Inf,
                            applies.before.time = 2007,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex') 
)


##------------------------##
##-- INITIAL POPULATION --##
##------------------------##
# Specify the initial compartment sizes in 1940
# e.g., model a dummy population: 900 uninfected, 100 infected 
# we want to define a function to evaluate the population size at the end, after we know the location 

#function
get.initial.population.infected<-function(location,specification.metadata){
  dim.names<-specification.metadata$dim.names[c('age','race','sex','continuum','stage')]
  array(100,dim=sapply(dim.names,length),dimnames=dim.names) #dummy value for now
}
# add as an element
register.model.element(SHIELD.SPECIFICATION,
                       name = 'initial.population.infected',
                       scale="non.negative.number",
                       get.value.function = get.initial.population.infected)
# define the value for initial population
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'initial.population.infected')

##
get.initial.population.uninfected<-function(location,specification.metadata){
  dim.names<-specification.metadata$dim.names[c('age','race','sex','profile')]
  array(900,dim=sapply(dim.names,length),dimnames=dim.names)
}
register.model.element(SHIELD.SPECIFICATION,
                       name = 'initial.population.uninfected',
                       scale="non.negative.number",
                       get.value.function = get.initial.population.uninfected)
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'initial.population.uninfected')



##------------------##
##-- TRANSMISSION --##
##------------------##
# Transmission has 4 elements: 1.susceptibility, 2.transmissibility, 3.contact, 4.new infection proportion (where does new infection go to? e.g., infected PrEP, infected not on PrEP)
# e.g., model a flat transmission rate that applies to all groups
register.transmission(SHIELD.SPECIFICATION,
                      contact.value = 'sexual.contact',
                      susceptibility.value = 'sexual.susceptibility',
                      transmissibility.value = 'sexual.transmissibility',
                      new.infection.proportions.value = 'new.infection.proportions',
                      tag = 'sexual.transmission',
                      new.infections.applies.to = list(continuum='undiagnosed',stage='ps'))
# all.new.infections.into.compartments #@Todd what is this option ????

# rate of contact between infected and uninfected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact',
                        value = 1)

# Susceptibility of uninfected persons
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.susceptibility',
                        value = 1)

# Infectiousness of infected persons
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.transmissibility',
                        value = 1)

# where do nw infections go to?
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'new.infection.proportions',
                        value = 1)


##------------------##
##--   MORTALITY  --##
##------------------##
# different sources of mortality (general, syphilis related, etc.)

register.mortality(SHIELD.SPECIFICATION, 
                   tag = 'general.mortality',
                   groups = c('infected','uninfected'),
                   mortality.rate.value = 'general.mortality.rate')

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'general.mortality.rate',
                        value = 0.01)


##------------------##
##--   NATALITY   --##
##------------------##
# Fertility and birth rate 
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = c('infected', 'uninfected'),
                  applies.to = list(sex='female'), #only women give birth
                  child.groups = 'uninfected',
                  fertility.rate.value = 'general.fertility.rate',
                  birth.proportions.value = 'general.birth.proportions', # when they are born, where do they go? (e.g., what age, what race, what sex will they have)
                  parent.child.concordant.dimensions = c('race'),# the race of the cild will be the same 
                  all.births.into.compartments = list(profile='susceptible',age= 1),
                  tag = 'births')

# e.g., assuming 50% of births are female and 10% of male are msm
# required elements to decide where new births should go
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.births.female',
                       scale = 'proportion', 
                       value = 0.5)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.male.msm',
                       scale = 'proportion', 
                       value = 0.1)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'general.birth.proportions',
                        value = 'proportion.births.female')# this will apply to all unless we define the subsets below:

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='heterosexual_male'), # there is a sex.from option as well that is used to differntiate proportions based on who the parent is
                               name = 'general.birth.proportions',
                               value = expression( (1- proportion.births.female)*(1-proportion.male.msm) ))

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='msm'),
                               name = 'general.birth.proportions',
                               value = expression( (1- proportion.births.female)*(proportion.male.msm) ))

register.model.element(SHIELD.SPECIFICATION,
                       name = 'general.fertility.rate',
                       scale = 'rate', 
                       value = 0.024 #this will come from census data by race for women in each county
)


##---------------------------##
##-- Continuum Transitions --##
##---------------------------##
# e.g., fix screening rate of 10% for all infected groups, assuming 90% get treated immediately and 10% remain untreated 
#assuming those who are diag.untrt will seek trt after a year 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'screening.rate',
                       scale = 'rate',
                       value = 0.1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.immediately.treated',
                       scale = 'proportion',
                       value = 0.9)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'screening.immediate.treatment.rate',
                        value = expression(screening.rate * proportion.immediately.treated))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'screening.delayed.treatment.rate',
                        value = expression(screening.rate * (1-proportion.immediately.treated)))

#screening followed by immediate trt
register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'undiagnosed'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'screening.immediate.treatment.rate',
                   remission.proportions.value = 'remission.prp',
                   tag = 'screening.immediate.trt'
                   )
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'remission.prp',
                        value = 1)

#screening followed by delayed trt
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments =  'undiagnosed',
                    to.compartments = 'diagnosed.untreated',
                    value = 'screening.delayed.treatment.rate')

# delayed treatment option
register.model.element(SHIELD.SPECIFICATION,
                       name = 'delayed.treatment.rate',
                       scale = 'rate',
                       value = 1)

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'diagnosed.untreated'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'delayed.treatment.rate',
                   remission.proportions.value='remission.prp',
                   tag = 'delayed.trt' )

##---------------------------##
##-- Stage Transitions --##
##---------------------------###
# e.g., assuming a fix duration for each state: ps= 3months, earlyLatent=9months, LateLatent=10years, Teritiary=infinit
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.ps',
                       scale = 'time', 
                       value = 0.25) #3/12

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'ps',
                    to.compartments = 'el',
                    value = expression(1/duration.ps))
##
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.el',
                       scale = 'time',
                       value = 0.75) #9/12

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'el',
                    to.compartments = 'll',
                    value = expression(1/duration.el))
##
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.ll',
                       scale = 'time',
                       value = 10)

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'll',
                    to.compartments = 'ter',
                    value = expression(1/duration.ll))

##---------------------------##
##--         OUTPUTS.      --##
##---------------------------##
# The model reports 2 categories of outcomes: 
# 1-cumulative outcomes, reported between jan1 to dec31) 
# 2-point estimates of compartment sizes reported in Jan 1st 

# The model projects the size of infected.uninfected population by default in Jan 1st of each year

# For calibration purposes, we fit these estiamtes agains CDC, but it's unclear if CDC data represents Jan 1st or another time during the year (e.g., HIV prevalence, prop suppressed)
# Because of this, we generally calculate the average value between Jan 1st and Dec 31st of each year and report that as the annual output 

# !!!for dynamic transitions that change over time (e.g., testing), the anchor points are coded at the begginign of the year (e.g., if transmission changes from 2000 to 2020, these dates represent jan 1st of those years)

#Dynamic outputs: (in addition to compartment size)
# track.dynamic.outcome: a more general definition, calculated at each step of solver (mortality birth etc)
# track.transition: people move from one compartment to another #tracking transitions along one dimension (e.g., continuum)


## All Diagnosis (at all stages ): has 2 components
 ### 1st component:new diagnosis that dont receive treatment immediately (transition)
track.transition(SHIELD.SPECIFICATION, 
                 name = 'diag.untreated',
                 #display name on the graph
                 outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Delayed Treatment',
                                                            description = "Number of Individuals with a New Diagnosis of Syphilis that dont Start Treatment Immediately in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Cases',
                                                            units = 'cases',
                                                            singular.unit = 'case'),
                 dimension = 'continuum',#transition along the continuum
                 from.compartments = 'undiagnosed',
                 to.compartments = 'diagnosed.untreated',
                 keep.dimensions = c('location','age','race','sex','stage')
)
#2 components: new diagnosis that receive treatment immediately (remission)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'diag.treated',
                      outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Immediate Treatment',
                                                                 description = "Number of Individuals with a New Diagnosis of Syphilis that Start Treatment Immediately in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = "remission.from", #where they come from 
                      subset.dimension.values = list(continuum='undiagnosed'), #only counting those who receive immediata trt
                      keep.dimensions = c('location','age','race','sex','stage')
                      )

#Total diagnosis
#We usually try to outline the outcomes here with the final outcomes we need for calibrations: so we break them by stage
## primarySecondary
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.ps",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='ps.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses PS',
                                                                    description = "Number of Individuals with a Diagnosis of Primary-Secondary Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'ps.syphilis' #corresponding to the name in data manager
                         )
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.el",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='el.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses EL',
                                                                    description = "Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'el.syphilis' #corresponding to the name in data manager
)
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.late",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='late.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses Late',
                                                                    description = "Number of Individuals with a Diagnosis of Late Latent & Teritiary Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis' #corresponding to the name in data manager
)
##########################
# Incidence (new infections + reinfections)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'incidence',
                      outcome.metadata = create.outcome.metadata(display.name = 'Incidence',
                                                                 description = "Number of Individuals Infected with Syphilis in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = 'incidence.from', # use of ".from" helps us track where individuals are coming from (differentiate new vs re-infections)
                      keep.dimensions = c('location','age','race','sex','profile')
)
##########################
# New Treatment Initiations: Immediate and Delayed
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'trt.initiation',
                      outcome.metadata = create.outcome.metadata(display.name = 'Treatment Initiation',
                                                                 description = "Number of Individuals Starting Treatment in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = "remission.from", #where they come from 
                      keep.dimensions = c('location','age','race','sex','stage')
)



##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(SHIELD.SPECIFICATION)

#@Melissa: 
#set this up for shield, and place the global transmission rate to play around 
# source('../jheem_analyses/applications/EHE/ehe_parameters_helpers.R')
# source('../jheem_analyses/applications/EHE/ehe_parameters.R')
# source('../jheem_analyses/applications/EHE/ehe_parameter_mapping.R')
# 
# register.calibrated.parameters.for.version('ehe',
#                                            distribution = EHE.PARAMETERS.PRIOR,
#                                            apply.function = EHE.APPLY.PARAMETERS.FN,
#                                            sampling.blocks = EHE.PARAMETER.SAMPLING.BLOCKS,
#                                            calibrate.to.year = 2025,
#                                            join.with.previous.version = F)


