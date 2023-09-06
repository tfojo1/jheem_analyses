source('applications/EHE/ehe_specification.R')
source('applications/extended_continuum/extended_continuum_specification_helpers.R')

# Melissa to do:
# redefine suppression quantity
# write out continuum transitions
# add outcomes to track: retention, linkage, suppression - he'll write this 
# this should be it? 
# add the register statement at the end - this may have bugs

EXT.SPECIFICATION = create.jheem.specification(version = 'ext',
                                               iteration = 1,
                                               description = "The expanded JHEEM continuum, with additional compartments for engagement and suppression", 
                                               
                                               parent.version = 'ehe',
                                               do.not.inherit.transitions.for.dimension = 'continuum', 
                                               
                                               compartments.for.infected.only = list(
                                                 continuum = c('undiagnosed', 'undiagnosed_from_prep', 'unengaged',
                                                               'engaged_unsuppressed_naive','engaged_unsuppressed_failing',
                                                               'engaged_recently_suppressed','engaged_durably_suppressed',
                                                               'disengaged_naive','disengaged_failing') 
                                               ),
                                               compartments.for.uninfected.only = list(),
                                               compartments.for.infected.and.uninfected = list(
                                                 location = 'location',
                                                 age = 'all.ages',
                                                 race=c('black','hispanic','other'),
                                                 sex= c('heterosexual_male', 'msm', 'female'),
                                                 risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
                                               ),
                                               compartment.value.aliases = list(
                                                 location = function(location){location},
                                                 
                                                 first.diagnosed.states='unengaged', 
                                                 diagnosed.states=c('unengaged',
                                                                    'engaged_unsuppressed_naive','engaged_unsuppressed_failing',
                                                                    'engaged_recently_suppressed','engaged_durably_suppressed',
                                                                    'disengaged_naive','disengaged_failing'),
                                                 undiagnosed.states=c('undiagnosed','undiagnosed_from_prep'),
                                                 undiagnosed.from.prep.states='undiagnosed_from_prep',
                                                 undiagnosed.no.prep.states='undiagnosed',
                                                 
                                                 engaged.states=c('engaged_unsuppressed_naive','engaged_unsuppressed_failing',
                                                                  'engaged_recently_suppressed','engaged_durably_suppressed'),
                                                 disengaged.states=c('disengaged_naive','disengaged_failing'),
                                                 
                                                 suppressed.states=c('engaged_recently_suppressed','engaged_durably_suppressed')
                                               )
)

##--------------------------------------##
##--------------------------------------##
##--        GENERAL QUANTITIES        --##
##--  (for multiple core components)  --##
##--------------------------------------##
##--------------------------------------##

## Staying the same: 
# PrEP, testing, needle exchange 

## Updating: 
# Suppression (0/1 value for unsuppressed/suppressed; not the same as gain of suppression rate)

## Adding: 
# Linkage, time to suppression/failing, disengagement/re-engagement, gain of suppression, loss of suppression

##-------------##
##-- Linkage --##                            
##-------------##

register.model.element(EXT.SPECIFICATION,
                       name = 'linkage',
                       scale = 'proportion', # leave this as a proportion (later, multiply (proportion who link)*(3 months))
                       get.functional.form.function= get.linkage.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010 # prior to 2010, flat (if I want to ramp up, set ramp time/value)
                       )

register.model.element(EXT.SPECIFICATION,
                       name = 'time.to.linkage',
                       scale = 'time',
                       value = 1/4) # 3 months

##---------------------------------##
##-- Time to suppression/failing --##                            
##---------------------------------##

register.model.element(EXT.SPECIFICATION,
                       name='naive.to.suppressed.proportion',
                       scale='proportion',
                       get.functional.form.function = get.naive.to.suppressed.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.quantity(EXT.SPECIFICATION,
                       name = 'time.to.failing.or.suppression',
                       scale = 'time',
                       value = expression(time.to.start.art + time.to.suppression)) 

register.model.element(EXT.SPECIFICATION,
                       name = 'time.to.start.art',
                       scale = 'time',
                       get.functional.form.function = get.time.to.start.art.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2000,
                       functional.form.to.time = 2012, # when start ART time should *theoretically* be 0 (but wasn't in practice)
                       taper.times = 2014, # will sample this 
                       taper.values = 0) 

register.model.element(EXT.SPECIFICATION,
                       name = 'time.to.suppression',
                       scale = 'time',
                       value = 1/4) # 3 months for now

##---------------------------------##
##-- Disengagement/Re-engagement --##                            
##---------------------------------##

register.model.element(EXT.SPECIFICATION,
                       name = 'disengagement.naive',
                       scale = 'rate',
                       get.functional.form.function = get.disengagement.naive.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'disengagement.failing',
                       scale = 'rate',
                       get.functional.form.function = get.disengagement.failing.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'disengagement.recently.suppressed',
                       scale = 'rate',
                       get.functional.form.function = get.disengagement.recently.suppressed.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'disengagement.durably.suppressed',
                       scale = 'rate',
                       get.functional.form.function = get.disengagement.durably.suppressed.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'reengagement',
                       scale = 'rate',
                       get.functional.form.function = get.reengagement.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

##------------------------------##
##-- Gain/Loss of Suppression --##                            
##------------------------------##

register.model.element(EXT.SPECIFICATION,
                       name = 'gain.of.suppression',
                       scale = 'rate',
                       get.functional.form.function = get.gain.of.suppression.model,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'loss.of.suppression.recent',
                       scale = 'rate',
                       get.functional.form.function = get.loss.of.suppression.recent.model, 
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)

register.model.element(EXT.SPECIFICATION,
                       name = 'loss.of.suppression.durable',
                       scale = 'rate',
                       get.functional.form.function = get.loss.of.suppression.durable.model, 
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010)


##---------------------------------##
##-- Time to durable suppression --##                            
##---------------------------------##

register.model.element(EXT.SPECIFICATION,
                       name = 'time.to.durable.suppression',
                       scale = 'time',
                       value = 2) # spend two years in recently suppressed

##-----------------##
##-- Suppression --##                            
##-----------------##
## This quantity is used later on, for mortality, for example; this is not the same as the gain of suppression rate 

register.model.quantity(EXT.SPECIFICATION,
                        name = 'suppression',
                        value = 0) 

# set suppressed states to 1 
register.model.quantity.subset(EXT.SPECIFICATION,
                               name = 'suppression',
                               value = 1, 
                               applies.to = list(continuum='suppressed.states'))


##--------------------------------##
##--------------------------------##
##--        TRANSITIONS         --##
##--------------------------------##
##--------------------------------##

## Staying the same: 
# IDU transitions, Stage transitions 

## Updating: 
# Continuum transitions (didn't inherit any of these, so add back in all of them even if they are the exact same)

##---------------------------##
##-- Continuum Transitions --##
##---------------------------##

register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed_from_prep',
                    to.compartments = 'unengaged',
                    groups = 'infected',
                    value = expression( 1 / prep.screening.frequency))

register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed_from_prep',
                    to.compartments = 'undiagnosed',
                    groups = 'infected',
                    value = 'all.prep.discontinuation')

register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed',
                    to.compartments = 'unengaged', 
                    groups = 'infected',
                    value = 'testing') 

# linkage (success)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'unengaged',
                    to.compartments = 'engaged_unsuppressed_naive', 
                    groups = 'infected',
                    value = expression(linkage*(1/time.to.linkage))) 

# linkage (failure)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'unengaged',
                    to.compartments = 'disengaged_naive', 
                    groups = 'infected',
                    value = expression((1-linkage)*(1/time.to.linkage))) 

# leave naive (success, suppressed)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_unsuppressed_naive',
                    to.compartments = 'engaged_recently_suppressed', 
                    groups = 'infected',
                    value = expression((naive.to.suppressed.proportion)*(1/time.to.failing.or.suppression))) 

# leave naive (failure)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_unsuppressed_naive',
                    to.compartments = 'engaged_unsuppressed_failing', 
                    groups = 'infected',
                    value = expression((1-naive.to.suppressed.proportion)*(1/time.to.failing.or.suppression))) 

# disengagement (naive)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_unsuppressed_naive', 
                    to.compartments = 'disengaged_naive', 
                    groups = 'infected',
                    value = 'disengagement.naive')

# disengagement (failing)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_unsuppressed_failing', 
                    to.compartments = 'disengaged_failing', 
                    groups = 'infected',
                    value = 'disengagement.failing')

# disengagement (recently suppressed)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_recently_suppressed', 
                    to.compartments = 'disengaged_failing', 
                    groups = 'infected',
                    value = 'disengagement.recently.suppressed')

# disengagement (durably suppressed)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_durably_suppressed', 
                    to.compartments = 'disengaged_failing', 
                    groups = 'infected',
                    value = 'disengagement.durably.suppressed')

# reengagement (naive)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'disengaged_naive', 
                    to.compartments = 'engaged_unsuppressed_naive', 
                    groups = 'infected',
                    value = 'reengagement')

# reengagement (failing)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'disengaged_failing', 
                    to.compartments = 'engaged_unsuppressed_failing', 
                    groups = 'infected',
                    value = 'reengagement')

# gain of suppression (from failing only)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_unsuppressed_failing', 
                    to.compartments = 'engaged_recently_suppressed', 
                    groups = 'infected',
                    value = 'gain.of.suppression')

# loss of suppression (recent)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_recently_suppressed', 
                    to.compartments = 'engaged_unsuppressed_failing', 
                    groups = 'infected',
                    value = 'loss.of.suppression.recent') 

# loss of suppression (durable)
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_durably_suppressed', 
                    to.compartments = 'engaged_unsuppressed_failing', 
                    groups = 'infected',
                    value = 'loss.of.suppression.durable') 

# recent to durable suppression
register.transition(EXT.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'engaged_recently_suppressed',
                    to.compartments = 'engaged_durably_suppressed', 
                    groups = 'infected',
                    value = expression(1/time.to.durable.suppression))


##----------------------------##
##----------------------------##
##--        NATALITY        --##
##----------------------------##
##----------------------------##

## Staying the same 

##--------------------------------##
##--------------------------------##
##--        MORTALITY           --##
##--------------------------------##
##--------------------------------##

## Staying the same 
## Because we changed suppression definition above, this still works: 
# mortality.rate.value = expression((1-suppression) * unsuppressed.hiv.mortality.rate))

##------------------------##
##------------------------##
##--        AGING       --##
##------------------------##
##------------------------##

## Staying the same 

##--------------------------------##
##--------------------------------##
##--        TRANSMISSION        --##
##--------------------------------##
##--------------------------------##

## Staying the same
## Right now, inefficient to multiply all the 0's in suppression; will later update to only apply to unsuppressed states
## (Right now: value = expression((1-suppression) * diagnosed.vs.undiagnosed.sexual.transmissibility * acute.vs.chronic.transmissibility)))

##--------------------------##
##--------------------------##
##-- SET TRACKED OUTCOMES --##
##--------------------------##
##--------------------------##

## Adding: linkage, disengagement/reengagement, suppression - didn't do suppression yet 

# probably won't use this - want the proportion and run into an issue with the denominator
track.transition(EXT.SPECIFICATION,
                 name = 'number.linked',
                 outcome.metadata = create.outcome.metadata(display.name = 'Number Linked to Care',
                                                            description = "Number of Individuals Linked to Care in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Individuals',
                                                            units = 'individuals'),
                 dimension = 'continuum',
                 from.compartments = 'unengaged',
                 to.compartments = 'engaged_unsuppressed_naive',
                 keep.dimensions = c('age','race','sex','risk'))

# old version - doesn't work 
# track.quantity.outcome(EXT.SPECIFICATION,
#                        name = 'linkage',
#                        outcome.metadata = create.outcome.metadata(display.name = 'Linkage to Care',
#                                                                   description = "Proportion of Newly Diagnosed Individuals Linked to Care within 3 months",
#                                                                   scale = 'proportion',
#                                                                   axis.name = 'Proportion Linked',
#                                                                   units = '%'),
#                        value='linkage',
#                        denominator.outcome = 'new',
#                        keep.dimensions = c('age','race','sex','risk'))

track.integrated.outcome(EXT.SPECIFICATION,
                         name = 'cumulative.linkage',
                         outcome.metadata = create.outcome.metadata(display.name = 'Cumulative Number Linked to Care',
                                                                    description = "Cumulative Number of Individuals Linked to Care",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Individuals',
                                                                    units = 'individuals'),
                         outcome.name.to.integrate = 'number.linked',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         save = F)

# may want to reevaluate how we track retention
track.transition(EXT.SPECIFICATION,
                 name = 'disengagement',
                 outcome.metadata = create.outcome.metadata(display.name = 'Disengagement from Care',
                                                            description = "Number of Individuals Disengaging from Care in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Individuals',
                                                            units = 'individuals'),
                 dimension = 'continuum',
                 from.compartments = 'engaged.states',
                 to.compartments = 'disengaged.states',
                 keep.dimensions = c('age','race','sex','risk','continuum.from')) # might remove continuum if we don't need it

# might not need to track
track.transition(EXT.SPECIFICATION,
                 name = 'reengagement',
                 outcome.metadata = create.outcome.metadata(display.name = 'Re-Engagement in Care',
                                                            description = "Number of Individuals Re-Engaging in Care in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Individuals',
                                                            units = 'individuals'),
                 dimension = 'continuum',
                 from.compartments = 'disengaged.states',
                 to.compartments = 'engaged.states',
                 keep.dimensions = c('age','race','sex','risk')) 

## Didn't do suppression yet

##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(EXT.SPECIFICATION)


