#0807
# Engine error when we exclude location 

##############################
#DEfault definitions:
# Groups: infected and uninfected 
# list(sex='xx',age='xx',race='xx') or list(conitnuum='xx',stage='xx')

##############################
# Source supporting files
source('../jheem_analyses/source_code.R') # a file that contains all the necessary functions for the JHEEM

##--------------------##
##-- INITIAL SET-UP --##
##--------------------##   
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield', #A single character value denoting the version
                                                  iteration=1,
                                                  # subversion = # runs the same model but can report seperate outcomes
                                                  description = "The initial SHIELD version, set up to model national epidemic", #A short text description of this version
                                                  
                                                  start.year = 1940,
                                                  
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #similar to atlas [0-15][16-20]
                                                  #The names of the lists represent dimensions, and the values the compartments for each dimension.
                                                  #Todd: the EHE continuum has the stage in it, but we have defined stage as a seperate dim for now. Should we combine them, in which case we will have ~12 values for the continuum
                                                  compartments.for.infected.only = list( 
                                                    # continuum = c('undiagnosed', 'diagnosed.untreated','diagnosed.treated'), 
                                                    # stage = c('primarySecondary', 'earlyLatent','lateLatent','tertiary')
                                                    # 
                                                    
                                                    continuum=c('ps.undiagnosed','el.undiagnosed','ll.undiagnosed','ter.undiagnosed',
                                                                'ps.diagnosed.untreated','el.diagnosed.untreated','ll.diagnosed.untreated','ter.diagnosed.untreated')  
                                                  ),
                                                  
                                                  #@Todd: I think that we should define the suscptible and diagnosed.treated states seperatly here? 
                                                  compartments.for.uninfected.only = list(
                                                    profile=c('susceptible','diagnosed.treated')),
                                                  
                                                  compartments.for.infected.and.uninfected = list(
                                                    location = "C.12580", #@Todd: if I remove location, the specification works but engine fails
                                                    
                                                    age = 'all.ages',#alias for the agegroups defined above 
                                                    race=c('black','hispanic','other'),
                                                    sex= c('heterosexual_male', 'msm', 'female')
                                                  ),
                                                  
                                                  compartment.value.aliases = list( 
                                                    #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                    # helps to define specifications for groups of compartments later on 
                                                    # named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities)
                                                    undiagnosed.states=c('ps.undiagnosed','el.undiagnosed','ll.undiagnosed','ter.undiagnosed'),
                                                    diagnosed.states=c('ps.diagnosed.untreated','el.diagnosed.untreated','ll.diagnosed.untreated','ter.diagnosed.untreated','diagnosed.treated'),
                                                    
                                                    diagnosed.untreated.states=c('ps.diagnosed.untreated','el.diagnosed.untreated','ll.diagnosed.untreated','ter.diagnosed.untreated'),
                                                    diagnosed.treated.states=c('diagnosed.treated'),
                                                    
                                                    ps.states=c('ps.undiagnosed','ps.diagnosed.untreated'),
                                                    el.states=c('el.undiagnosed','el.diagnosed.untreated'),
                                                    ll.states=c('ll.undiagnosed','ll.diagnosed.untreated'),
                                                    ter.states=c('ter.undiagnosed','ter.diagnosed.untreated')
                                                    
                                                    
                                                  )
)

##----------------------##
##-- Fix Strata Sizes --##
##----------------------##
#'@title Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data
register.fixed.model.strata(SHIELD.SPECIFICATION, #The jheem.specification object
                            applies.after.time = -Inf,
                            applies.before.time = 2007, #single numeric values giving the time frame over whether this setting applies
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex') #A character vector denoting which dimensions should be fixed. These should be dimensions common to both infected and uninfected groups. These are only used if fix.strata==TRUE
)


##------------------------##
##-- INITIAL POPULATION --##
##------------------------##
# Specify the initial compartment sizes in 1940
# e.g., model a dummy population: 900 uninfected, 100 infected 

register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'initial.population.infected')

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.infected',
                        value = 100)

register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'initial.population.uninfected')

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.uninfected',
                        value = 900)

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
                      new.infections.applies.to = list(continuum='ps.undiagnosed'))

# all.new.infections.into.compartments #@Todd what is this ????

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
                  all.births.into.compartments = list(age= 1),
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


#Todd: can we define congenital syphilis as such? 
# register.natality(specification = SHIELD.SPECIFICATION,
#                   parent.groups = 'infected',
#                   applies.to = list(sex='female'), #only women give birth
#                   child.groups = 'infected',
#                   fertility.rate.value = 'general.fertility.rate',
#                   birth.proportions.value = 'general.birth.proportions', # when they are born, where do they go? (e.g., what age, what race, what sex will they have)
#                   parent.child.concordant.dimensions = c('race'),# the race of the cild will be the same 
#                   all.births.from.compartments=list(continuum=c('undiagnosed','diagnosed.untreated') ),
#                   all.births.into.compartments=character(age=1,continuum='undiagnosed',stage='primarySecondary'),
#                   tag = 'congenital.infections')

##---------------------------##
##-- Continuum Transitions --##
##---------------------------##
####### SCREENING/TREATMENT ######
# e.g., fix screening rate of 1% for all infected groups, assuming 90% get treated immediately and 10% remain untreated 
#assuming those who are diag.untrt will seek trt after a year 

register.model.element(SHIELD.SPECIFICATION,
                       name = 'screening.rate',
                       scale = 'rate',
                       value = 0.01)
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
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected', # @TODD: I'm confused about the group role here. from.compartment/to.compartments specified who will go from where to where, what's the role of group? 
                    from.compartments = 'undiagnosed.states',
                    to.compartments = 'diagnosed.untreated.states',
                    value = 'screening.delayed.treatment.rate')
#screening followed by delayed trt
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'undiagnosed.states',
                    to.compartments = 'diagnosed.treated.states',
                    value = 'screening.immediate.treatment.rate')
#delayed treatment option 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'delayed.treatment.rate',
                       scale = 'rate',
                       value = 1)

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'diagnosed.untreated.states',
                    to.compartments = 'diagnosed.treated.states',
                    value = 'delayed.treatment.rate')

###### DISEASE PROGRESSION ######
#e.g., assuming a fix duration for each state: ps= 3months, earlyLatent=9months, LateLatent=10years, Teritiary=infinit
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.ps',
                       scale = 'time',#TODD: is this in years? 
                       value = 0.25) #3/12

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'ps.states',
                    to.compartments = 'el.states',
                    value = expression(1/'duration.ps'))
##
# register.model.element(SHIELD.SPECIFICATION,
#                        name = 'duration.el',
#                        scale = 'time', 
#                        value = 0.75) #9/12
# 
# register.transition(SHIELD.SPECIFICATION,
#                     dimension = 'continuum',
#                     groups = 'infected',
#                     from.compartments = list(continuum=c('el.undiagnosed','el.diagnosed.untreated')),
#                     to.compartments = list(continuum=c('ll.undiagnosed','ll.diagnosed.untreated')),
#                     value = expression(1/'duration.el'))
# ##
# register.model.element(SHIELD.SPECIFICATION,
#                        name = 'duration.ll',
#                        scale = 'time', 
#                        value = 10) 
# 
# register.transition(SHIELD.SPECIFICATION,
#                     dimension = 'continuum',
#                     groups = 'infected',
#                     from.compartments = list(continuum=c('ll.undiagnosed','ll.diagnosed.untreated')),
#                     to.compartments = list(continuum=c('ter.undiagnosed','ter.diagnosed.untreated')),
#                     value = expression(1/'duration.ll'))

##---------------------------##
##-- Dynamic outputs --##
##---------------------------##
#Dynamic outputs: (in addition to compartment size)
# track.dynamic.outcome: a more general definition, calculated at each step of solver (mortality birth etc)
# track.transition: people move from one compartment to another 

track.transition(SHIELD.SPECIFICATION, #tracking transitions along one dimension (e.g., continuum)
                 name = 'new.diagnoses.ps',
                 #display name on the graph
                 outcome.metadata = create.outcome.metadata(display.name = 'New Diagnoses',
                                                            description = "Number of Individuals with a New Diagnosis of Primary-Secondary Syphilis in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Cases',
                                                            units = 'cases',
                                                            singular.unit = 'case'),
                 dimension = 'continuum',
                 subset.dimension.values = list(continuum='ps.undiagnosed'),
                 from.compartments = 'undiagnosed.states',
                 to.compartments = 'diagnosed.states',
                 keep.dimensions = c('age','race','sex'),
                 corresponding.data.outcome = 'diagnoses'# a helper for simplot to add survillance data 
)
 


##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(SHIELD.SPECIFICATION)






#Quantity vs element?
# Element indivisible 
# Quantity is combination of didfferent element or quantities
