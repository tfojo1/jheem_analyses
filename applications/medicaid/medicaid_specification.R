source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_prep/cdc_prep_parameters.R') 
source('../jheem_analyses/applications/cdc_prep/odds_ratio_model.R') 


MEDICAID.SPECIFICATION = create.jheem.specification(version='medicaid',
                                                    iteration = '1',
                                                    description='Model to study the impacts of cuts to Medicaid',
                                                    parent.version = 'ehe')

####------------####
####------------####
####-- INPUTS --####
####------------####
####------------####

##-----------------------------##
##-- INTERVENTION PARAMETERS --##
##-----------------------------##

# proportion.suppressed.pwh.who.lose.medicaid

register.model.element.values(MEDICAID.SPECIFICATION,
                              scale = 'proportion',
                              medicaid.affects.suppression.switch = 1,
                              medicaid.affects.testing.switch = 1,
                              medicaid.affects.prep.switch = 1
                              )

# To represent the scenario where some people gain RW to offset medicaid cuts
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.gain.rw.among.pwh.who.lose.medicaid',
                       scale = 'proportion',
                       value = 0)

##-------------------------------------##
##-- INPUT PARMETERS for CALIBRATION --##
##-------------------------------------##

#-- Medicaid Coverage --#

# This will have to be a functional form
# The proportion of people who have medicaid
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.on.medicaid.of.uninfected',
                       scale = 'proportion',
                       value = 0.1) # @Melissa - need to set a functional form

# We will connect this with p.on.medicaid.of.uninfected to make the proportion diagnosed PWH on medicaid
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'OR.diagnosed.pwh.on.medicaid',
                       scale = 'ratio',
                       value = 1) # @Melissa - need to get a value. Maybe functional form, maybe just a scalar value

# We will connect this with p.on.medicaid.of.uninfected to make the proportion UNdiagnosed PWH on medicaid
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'OR.undiagnosed.pwh.on.medicaid',
                       scale = 'ratio',
                       value = 1) # @Melissa - need to get a value. Maybe functional form, maybe just a scalar value


#-- Uninsured Population --#
# For calibrating testing rates among uninsured
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.uninsured.of.uninfected',
                       scale = 'proportion',
                       value = 0.1) # @Melissa - need to set a functional form



#-- Ryan White Services --#

# Probability of ADAP, with OR without other non-ADAP services
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.adap.of.pwh.with.medicaid',
                       scale = 'proportion',
                       value = 0.25) # @Melissa - need to set a functional form

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.adap.of.pwh.without.medicaid',
                       scale = 'proportion',
                       value = 0.25) # @Melissa - need to set a functional form

# Given that you have ADAP, what is the probability you also have non-ADAP services
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.non.adap.of.adap.with.medicaid',
                       scale = 'proportion',
                       value = 4/5) # @Melissa - will probably just have this scalar value

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.non.adap.of.adap.without.medicaid',
                       scale = 'proportion',
                       value = 4/5) # @Melissa - will probably just have this scalar value

# Probability of non-ADAP services withOUT adap services
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.rw.without.adap.of.pwh.with.medicaid',
                       scale = 'proportion',
                       value = 0.25) # @Melissa - need to set a functional form

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.rw.without.adap.of.pwh.without.medicaid',
                       scale = 'proportion',
                       value = 0.25) # @Melissa - need to set a functional form




#-- Testing --#

# Medicaid and uninured testing rates will be a multiplier of the previously calibrated overall testing rate
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'medicaid.testing.rate.ratio',
                       scale = 'ratio',
                       value = 1)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'uninsured.testing.rate.ratio',
                       scale = 'ratio',
                       value = 1)

# Let the RR for testing if you have HIV vary by medicaid/uninsured
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'RR.medicaid.undiagnosed.testing.increase',
                       scale = 'ratio',
                       value = 1)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'RR.uninsured.undiagnosed.testing.increase',
                       scale = 'ratio',
                       value = 1)

#-- PrEP --#

# Put together with calibrated p on prep to get p on prep for medicaid and uninsured
register.model.element(MEDICAID.SPECIFICATION,
                       name = 'OR.prep.medicaid',
                       scale = 'ratio',
                       value = 1)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'OR.prep.uninsured',
                       scale = 'ratio',
                       value = 0.1)

##---------------------##
##-- MEDICAID LOSSES --##
##---------------------##

# Not sure we need these here

# register.model.element(MEDICAID.SPECIFICATION,
#                        name = 'logit.mean.medicaid.loss',
#                        scale = 'proportion',
#                        value = log(0.1)-log(0.9))
# 
# register.model.element(MEDICAID.SPECIFICATION,
#                        name = 'logit.sd.medicaid.loss',
#                        scale = 'rate',
#                        value = 0.025)
# 
# register.model.element(MEDICAID.SPECIFICATION,
#                        name = 'z.medicaid.loss',
#                        scale = 'number',
#                        value = 0.1)
# 
# register.model.quantity(MEDICAID.SPECIFICATION,
#                         name = 'medicaid.loss',
#                         scale = 'proportion',
#                         value = expression(1 / (1 + exp(-(logit.mean.medicaid.loss + z.medicaid.loss * logit.sd.medicaid.loss)))))


# Not sure here - do we just want to set these?
# Do we want to calibrate them instead to the total loss?

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.medicaid.loss.of.uninfected',
                       scale = 'proportion',
                       value = 0)

register.model.element.values(MEDICAID.SPECIFICATION,
                              scale = 'ratio',
                              OR.medicaid.loss.for.undiagnosed.pwh = 1,
                              OR.medicaid.loss.for.diagnosed.pwh.with.ryan.white = 1,
                              OR.medicaid.loss.for.diagnosed.pwh.without.ryan.white = 1)


register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.OR.medicaid.loss.for.diagnosed.pwh.with.ryan.white',
                       scale = 'ratio',
                       value = 1)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.OR.medicaid.loss.for.diagnosed.pwh.without.ryan.white',
                       scale = 'ratio',
                       value = 1)


####------------------------------####
####------------------------------####
####-- CALCULATIONS from INPUTS --####
####------------------------------####
####------------------------------####


##-----------------------##
##-- MEDICAID COVERAGE --##
##-----------------------##

# Our inputs are p.on.medicaid if uninfected
#  plus ORs for p.on.medicaid if diagnosed/undiagnosed PWH
# Need to combine these to get p.on.medicaid if diagnosed/undiagnosed PWH

#-- Uninfected --#

# help us for calculation
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'odds.on.medicaid.of.uninfected',
                        value = expression( p.on.medicaid.of.uninfected / (1-p.on.medicaid.of.uninfected) ))

#-- Diagnosed PWH --#

# Multiply odds by OR and convert back to probability
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.diagnosed.pwh.on.medicaid',
                        value = expression( 1 / (1 + 1/(odds.on.medicaid.of.uninfected * OR.diagnosed.pwh.on.medicaid)) ))


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.never.medicaid.of.diagnosed.pwh',
                        value = expression(1-p.diagnosed.pwh.on.medicaid))


#-- Undiagnosed PWH --#

# Multiply odds by OR and convert back to probability
register.model.quantity(MEDICAID.SPECIFICATION,
                       name = 'p.undiagnosed.pwh.on.medicaid',
                       value = expression( 1 / (1 + 1/(odds.on.medicaid.of.uninfected * OR.undiagnosed.pwh.on.medicaid)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.never.medicaid.of.undiagnosed.pwh',
                        value = expression(1-p.undiagnosed.pwh.on.medicaid))
           
##-------------------##
##-- MEDICAID LOSS --##
##-------------------##

# same thing - take a p.lose.medicaid for uninfected, combine with ORs for diagnosed/undiagnosed PWH to get p.lose.medicaid for those groups

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'odds.medicaid.loss.of.uninfected',
                        value = expression( p.medicaid.loss.of.uninfected / (1-p.medicaid.loss.of.uninfected) ))

# We are anchoring our projections of medicaid loss assuming that the given projections are for the uninfected, which approximately equals total medicaid population
# We are calculating the p medicaid loss among undiagnosed pwh and diagnosed pwh (with or without Ryan White) as odds ratios multiplied into the odds
#       of losing medicaid if uninfected
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.loss.undiagnosed.pwh',
                        value = expression( 1 / (1 + 1/(odds.medicaid.loss.of.uninfected * OR.medicaid.loss.for.undiagnosed.pwh)) )) # take odds if uninfected, multiply by OR, convert back to probability

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.loss.diagnosed.pwh.with.ryan.white',
                        value = expression( 1 / (1 + 1/(odds.medicaid.loss.of.uninfected * OR.medicaid.loss.for.diagnosed.pwh.with.ryan.white)) )) # take odds if uninfected, multiply by OR, convert back to probability

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.loss.diagnosed.pwh.without.ryan.white',
                        value = expression( 1 / (1 + 1/(odds.medicaid.loss.of.uninfected * OR.medicaid.loss.for.diagnosed.pwh.without.ryan.white)) )) # take odds if uninfected, multiply by OR, convert back to probability


# Split out who keeps/loses medicaid among undiagnosed
# We'll do the split for diagnosed below, with Ryan White coverage

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.keep.medicaid.of.undiagnosed.pwh',
                        value = expression(p.undiagnosed.pwh.on.medicaid * (1-p.medicaid.loss.undiagnosed.pwh)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.lose.medicaid.of.undiagnosed.pwh',
                        value = expression(p.undiagnosed.pwh.on.medicaid * p.medicaid.loss.undiagnosed.pwh))

##-------------------------##
##-- RYAN WHITE COVERAGE --##
##-------------------------##


# Split diagnosed PWH into 2x2 medicaid yes/no, rw yes/no
#  We will use this to split out for suppression into: lose medicaid with RW, keep medicaid with RW, lose medicaid without RW, keep medicaid without RW
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = "p.rw.with.medicaid.of.pwh",
                        value = expression(p.diagnosed.pwh.on.medicaid * 
                                               (p.adap.of.pwh.with.medicaid + p.rw.without.adap.of.pwh.with.medicaid) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.no.rw.with.medicaid.of.pwh',
                        value = expression(p.diagnosed.pwh.on.medicaid *
                                               (1 - p.adap.of.pwh.with.medicaid - p.rw.without.adap.of.pwh.with.medicaid)))


# We recalculated the below in a separate bit of code
# leaving commented here in case we decide we want to refactor

# We called this one p.no.medicaid.no.rw.of.diagnosed.pwh below
# register.model.quantity(MEDICAID.SPECIFICATION,
#                         name = 'p.no.rw.without.medicaid.of.pwh',
#                         value = expression((1-p.diagnosed.pwh.on.medicaid) *
#                                                (1 - p.adap.of.pwh.without.medicaid - p.rw.without.adap.of.pwh.without.medicaid)))


# Split out medicaid loss
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.keep.medicaid.with.rw.of.diagnosed.pwh',
                        value = expression(p.rw.with.medicaid.of.pwh * (1-p.medicaid.loss.diagnosed.pwh.with.ryan.white)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.lose.medicaid.with.rw.of.diagnosed.pwh',
                        value = expression(p.rw.with.medicaid.of.pwh * p.medicaid.loss.diagnosed.pwh.with.ryan.white))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.keep.medicaid.without.rw.of.diagnosed.pwh',
                        value = expression(p.no.rw.with.medicaid.of.pwh * (1-p.medicaid.loss.diagnosed.pwh.without.ryan.white)))

# we are going to split the people without RW who lose medicaid into: lose medicaid but GAIN RW vs lose medicaid and still don't have RW
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.lose.medicaid.without.rw.of.diagnosed.pwh',
                        value = expression(p.no.rw.with.medicaid.of.pwh * p.medicaid.loss.diagnosed.pwh.without.ryan.white * (1-p.gain.rw.among.pwh.who.lose.medicaid)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.lose.medicaid.gain.rw.of.diagnosed.pwh',
                        value = expression(p.no.rw.with.medicaid.of.pwh * p.medicaid.loss.diagnosed.pwh.without.ryan.white * p.gain.rw.among.pwh.who.lose.medicaid))

##-----------------------##
##-- VIRAL SUPPRESSION --##
##-----------------------##

# This is where we plug it in to the suppression rate
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.of.diagnosed.after.medicaid.loss',
                        value = expression(suppression.never.medicaid * p.never.medicaid.of.diagnosed.pwh +
                                               suppression.keep.medicaid.with.rw * p.keep.medicaid.with.rw.of.diagnosed.pwh +
                                               suppression.keep.medicaid.without.rw * p.keep.medicaid.without.rw.of.diagnosed.pwh +
                                               suppression.lose.medicaid.with.rw * p.lose.medicaid.with.rw.of.diagnosed.pwh +
                                               suppression.lose.medicaid.without.rw * p.lose.medicaid.without.rw.of.diagnosed.pwh +
                                               suppression.lose.medicaid.gain.rw * p.lose.medicaid.gain.rw.of.diagnosed.pwh
                                           ))


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.of.diagnosed',
                        value = expression(suppression.of.diagnosed.after.medicaid.loss * medicaid.affects.suppression.switch +
                                               super.suppression.of.diagnosed * (1-medicaid.affects.suppression.switch)))
# If medicaid affects testing (ie, medicaid.affects.suppression.switch==1), then this will evaluate to equal suppression.of.diagnosed.after.medicaid.loss
# If medicaid does NOT affect testing(medicaid.affects.suppression.switch==0), then this will evaluate to super.suppression.of.diagnosed


register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.or.medicaid',
                       scale = 'proportion',
                       value = 1)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.or.adap.only',
                       scale = 'proportion',
                       value = 1) # @Melissa - we're going to need a functional form for this

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.or.non.adap.only',
                       scale = 'proportion',
                       value = 1) # @Melissa - we're going to need a functional form for this

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'suppression.or.adap.and.non.adap',
                       scale = 'proportion',
                       value = 1) # @Melissa - we're going to need a functional form for this


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'odds.suppression.of.diagnosed',
                        value = expression(super.suppression.of.diagnosed / (1 - super.suppression.of.diagnosed)))

# get new probabilities by:
# a) get new odds: old odds * ORs
# b) convert back to probability: 1 / (1 + 1/odds)
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.medicaid.without.rw',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.medicaid)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.medicaid.with.adap.only',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.medicaid * suppression.or.adap.only)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.medicaid.with.non.adap.only',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.medicaid * suppression.or.non.adap.only)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.medicaid.with.adap.and.non.adap',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.medicaid * suppression.or.adap.and.non.adap)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.no.medicaid.with.adap.only',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.adap.only)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.no.medicaid.with.non.adap.only',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.non.adap.only)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.no.medicaid.with.adap.and.non.adap',
                        value = expression( 1 / (1 + 1/(odds.suppression.of.diagnosed * suppression.or.adap.and.non.adap)) ))

# Going to use:
# total suppression among diagnosed pwh = 
#   suppression among medicaid + adap only * p with medicaid and adap only +
#   suppression among medicaid + non-adap only * p with medicaid and non-adap only +
#   suppression among medicaid + adap and non-adap * p with medicaid and adap only and non-adap +
#   suppression among medicaid without RW * p with medicaid without RW +
#   suppression among no medicaid + adap only * p with no medicaid and adap only +
#   suppression among no medicaid + non-adap only * p with no medicaid and non-adap only +
#   suppression among no medicaid + adap and non-adap * p with no medicaid and adap only and non-adap +
#   suppression among no medicaid without RW * p with no medicaid without RW
# And solve for suppression among no medicaid without TW
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.no.medicaid.without.rw',
                        value = expression(
                            ( super.suppression.of.diagnosed -
                                  suppression.medicaid.with.adap.only * p.medicaid.adap.only.of.diagnosed.pwh -
                                  suppression.medicaid.with.non.adap.only * p.medicaid.non.adap.only.of.diagnosed.pwh -
                                  suppression.medicaid.with.adap.and.non.adap * p.medicaid.adap.and.non.adap.of.diagnosed.pwh -
                                  suppression.medicaid.without.rw * p.medicaid.no.rw.of.diagnosed.pwh -
                                  suppression.no.medicaid.with.adap.only * p.no.medicaid.adap.only.of.diagnosed.pwh -
                                  suppression.no.medicaid.with.non.adap.only * p.no.medicaid.non.adap.only.of.diagnosed.pwh -
                                  suppression.no.medicaid.with.adap.and.non.adap * p.no.medicaid.adap.and.non.adap.of.diagnosed.pwh) / 
                                p.no.medicaid.no.rw.of.diagnosed.pwh
                                
                        )) # the remainder to make sure they sum

# Split up the proportions of people with HIV into each of the 8 combos of
#   medicaid/no medicaid
#   adap only/non-adap only/both/no rw

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.adap.only.of.diagnosed.pwh',
                        value = expression(p.diagnosed.pwh.on.medicaid * 
                                               p.adap.of.pwh.with.medicaid *
                                               (1-p.non.adap.of.adap.with.medicaid)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.adap.and.non.adap.of.diagnosed.pwh',
                        value = expression(p.diagnosed.pwh.on.medicaid * 
                                               p.adap.of.pwh.with.medicaid *
                                               p.non.adap.of.adap.with.medicaid))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.non.adap.only.of.diagnosed.pwh',
                        value = expression(p.diagnosed.pwh.on.medicaid * 
                                               p.rw.without.adap.of.pwh.with.medicaid))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.medicaid.no.rw.of.diagnosed.pwh',
                        value = expression(p.diagnosed.pwh.on.medicaid * 
                                               (1-p.adap.of.pwh.with.medicaid-p.rw.without.adap.of.pwh.with.medicaid)))


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.no.medicaid.adap.only.of.diagnosed.pwh',
                        value = expression((1-p.diagnosed.pwh.on.medicaid) * 
                                               p.adap.of.pwh.without.medicaid *
                                               (1-p.non.adap.of.adap.without.medicaid)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.no.medicaid.adap.and.non.adap.of.diagnosed.pwh',
                        value = expression((1-p.diagnosed.pwh.on.medicaid) * 
                                               p.adap.of.pwh.without.medicaid *
                                               p.non.adap.of.adap.without.medicaid))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.no.medicaid.non.adap.only.of.diagnosed.pwh',
                        value = expression((1-p.diagnosed.pwh.on.medicaid) * 
                                               p.rw.without.adap.of.pwh.without.medicaid))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.no.medicaid.no.rw.of.diagnosed.pwh',
                        value = expression((1-p.diagnosed.pwh.on.medicaid) * 
                                               (1-p.adap.of.pwh.without.medicaid-p.rw.without.adap.of.pwh.without.medicaid)))

# We need to estimate suppression at baseline among
# - PWH who don't have medicaid but do have RW
#   - split ADAP only, non-ADAP only, both
# - PWH who don't have medicaid and don't have RW
# - PWH who have medicaid + RW
#   - split ADAP only, non-ADAP only, both
# - PWH who have medicaid without RW
#
# Then we'll need to apply an OR for loss to the two medicaid groups
#  to get new p suppressed after losing medicaid

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.never.medicaid',
                        value = expression(
                            (suppression.no.medicaid.with.adap.only * p.no.medicaid.adap.only.of.diagnosed.pwh +
                                suppression.no.medicaid.with.non.adap.only * p.no.medicaid.non.adap.only.of.diagnosed.pwh +
                                suppression.no.medicaid.with.adap.and.non.adap * p.no.medicaid.adap.and.non.adap.of.diagnosed.pwh +
                                suppression.no.medicaid.without.rw * p.no.medicaid.no.rw.of.diagnosed.pwh) / 
                            p.never.medicaid.of.diagnosed.pwh
                        ))

# This actually calculates the BASELINE suppression among people with medicaid AND RW
# In stating it is the suppression among those with RW who KEEP medicaid, we are assuming that there are no
#   systematic baseline differences in suppression between those who will lose medicaid and those who will keep it
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.keep.medicaid.with.rw',
                        value = expression(
                            (suppression.medicaid.with.adap.only * p.medicaid.adap.only.of.diagnosed.pwh +
                                suppression.medicaid.with.non.adap.only * p.medicaid.non.adap.only.of.diagnosed.pwh +
                                suppression.medicaid.with.adap.and.non.adap * p.medicaid.adap.and.non.adap.of.diagnosed.pwh) /
                            (p.medicaid.adap.only.of.diagnosed.pwh + 
                                 p.medicaid.non.adap.only.of.diagnosed.pwh +
                                 p.medicaid.adap.and.non.adap.of.diagnosed.pwh)
                        ))
     
# Again, in saying this is suppression among those who keep medicaid, we assumed
#   there are no baseline differences between those who will keep and those who will lose
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.keep.medicaid.without.rw',
                        value = 'suppression.medicaid.without.rw')
    
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'odds.suppression.medicaid.with.rw',
                        value = expression(suppression.keep.medicaid.with.rw / (1-suppression.keep.medicaid.with.rw)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.lose.medicaid.with.rw',
                        value = expression( 1 / (1 + 1/(odds.suppression.medicaid.with.rw * 
                                                  suppression.OR.medicaid.loss.for.diagnosed.pwh.with.ryan.white)) ))
    
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'odds.suppression.medicaid.without.rw',
                        value = expression(suppression.medicaid.without.rw / (1-suppression.medicaid.without.rw)))
    
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.lose.medicaid.without.rw',
                        value = expression( 1 / (1 + 1/(odds.suppression.medicaid.without.rw * 
                                                  suppression.OR.medicaid.loss.for.diagnosed.pwh.without.ryan.white)) ))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.lose.medicaid.gain.rw',
                        value = 'suppression.no.medicaid.with.rw') # implies that their suppression changes to match people with RW but without medicaid
                        # value = 'suppression.medicaid.without.rw') # implies that their suppression is unchanged
    
    
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'suppression.no.medicaid.with.rw',
                        value = expression(
                            (suppression.no.medicaid.with.adap.only * p.no.medicaid.adap.only.of.diagnosed.pwh +
                                 suppression.no.medicaid.with.non.adap.only * p.no.medicaid.non.adap.only.of.diagnosed.pwh +
                                 suppression.no.medicaid.with.adap.and.non.adap * p.no.medicaid.adap.and.non.adap.of.diagnosed.pwh) /
                                (p.no.medicaid.adap.only.of.diagnosed.pwh + 
                                     p.no.medicaid.non.adap.only.of.diagnosed.pwh +
                                     p.no.medicaid.adap.and.non.adap.of.diagnosed.pwh)
                        ))

# Outcomes
#
# - n ADAP clients
# - n non-ADAP clients
#
# - OAHS suppression (assume it's equal to non-ADAP suppression)
# - ADAP Suppression
#
# - p of ADAP with Medicaid
# - p of non-ADAP with Medicaid

##-------------##
##-- TESTING --##
##-------------##

#-- Diagnosis Rate --#
# Since we are calibrating to observed testing among medicaid/uninsured
# we are going to formulate the DIAGNOSIS rates in those populations as TESTING rate times a RR for being more likely to get tested if you actually have HIV
# same thing we do for testing in general in the parent specification
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'diagnosis.rate.medicaid',
                        value = expression(medicaid.general.population.testing*(1+medicaid.undiagnosed.testing.increase)))
                    #    value = expression(super.testing.of.undiagnosed * medicaid.testing.rate.ratio))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'diagnosis.rate.keep.medicaid',
                        value = 'diagnosis.rate.medicaid') # we assume that there are no systematic differences between those who lose vs keep medicaid - so testing rate among those who keep medicaid = testing rate among everyone with medicaid at baseline

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'diagnosis.rate.lose.medicaid',
                        value = expression(uninsured.general.population.testing*(1+uninsured.undiagnosed.testing.increase)))
                     #   value = expression(super.testing.of.undiagnosed * uninsured.testing.rate.ratio))

# Fundamentally, we assume that if you have HIV, you are more likely to get tested than someone in the general population
# Here, we are not going to assume that HOW much more likely you are to get tested is the same if you are on medicaid/uninsured as for everyone
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'medicaid.undiagnosed.testing.increase',
                        value = expression(undiagnosed.testing.increase * RR.medicaid.undiagnosed.testing.increase))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'uninsured.undiagnosed.testing.increase',
                        value = expression(undiagnosed.testing.increase * RR.uninsured.undiagnosed.testing.increase))

# Since we will have previously calibrated overall diagnosis rate, 
# we assume that the diagnosis rate not on medicaid is whatever is needed to sum with diagnosis rate on medicaid
# start with:  (**overall testing rate = super.testing.of.undiagnosed)
# overall.testing.rate * 1 = diagnosis.rate.medicaid * p.on.medicaid + diagnosis.rate.NO.medicaid * p.not.on.medicaid
#   and solve for diagnosis.rate.NO.medicaid
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'diagnosis.rate.never.medicaid',
                        value = expression( (super.testing.of.undiagnosed - diagnosis.rate.medicaid * (1-p.never.medicaid.of.undiagnosed.pwh) ) /
                                                p.never.medicaid.of.undiagnosed.pwh) )


# Put it together
# Split into groups: never medicaid, had medicaid and keep it, had medicaid and lose it
# But unlike PrEP, we can calibrate to observed testing rates for uninsured, medicaid
register.model.quantity(MEDICAID.SPECIFICATION, 
                        name = "testing.of.undiagnosed.after.medicaid.loss",
                        value = expression(diagnosis.rate.never.medicaid * p.never.medicaid.of.undiagnosed.pwh +
                                               diagnosis.rate.keep.medicaid * p.keep.medicaid.of.undiagnosed.pwh +
                                               diagnosis.rate.lose.medicaid * p.lose.medicaid.of.undiagnosed.pwh)
)

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'testing.of.undiagnosed',
                        value = expression(testing.of.undiagnosed.after.medicaid.loss * medicaid.affects.testing.switch +
                                               super.testing.of.undiagnosed * (1-medicaid.affects.testing.switch)))
# If medicaid affects testing (ie, medicaid.affects.testing.switch==1), then this will evaluate to equal testing.of.undiagnosed.after.medicaid.loss
# If medicaid does NOT affect testing(medicaid.affects.testing.switch==0), then this will evaluate to super.testing.of.undiagnosed


#-- Testing Rate --#
# Define testing rates for medicaid/uninsured as a rate-ratio times general population testing rate
# These rate ratios are input parameters, that will be calibated to observed testing in these groups

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'medicaid.general.population.testing',
                        value = expression(super.general.population.testing * medicaid.testing.rate.ratio))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'uninsured.general.population.testing',
                        value = expression(super.general.population.testing * uninsured.testing.rate.ratio))

## NB, we are NOT calculating total testing rates that reflect medicaid cuts

#-- Helpers --#

# This code below helps us massage the testing rates in the model (which are for 13+)
# into rates that align with observed data (which are for 18+)

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'medicaid.fraction.population.over.18',
                        value = 'fraction.population.over.18')

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'medicaid.fraction.tests.over.18',
                        value = 'fraction.tests.over.18')

register.model.quantity(MEDICAID.SPECIFICATION,
                        'medicaid.general.population.testing.over.18',
                        value = expression(medicaid.general.population.testing * medicaid.fraction.tests.over.18 / medicaid.fraction.population.over.18))


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'uninsured.fraction.population.over.18',
                        value = 'fraction.population.over.18')

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'uninsured.fraction.tests.over.18',
                        value = 'fraction.tests.over.18')

register.model.quantity(MEDICAID.SPECIFICATION,
                        'uninsured.general.population.testing.over.18',
                        value = expression(uninsured.general.population.testing * uninsured.fraction.tests.over.18 / uninsured.fraction.population.over.18))

##----------##
##-- PREP --##
##----------##

# Tie in to PrEP Quantities
# Divide into three groups: never had medicaid, had medicaid and did not lose it, had medicaid and lost it
# This is the big picture - will define the input quantities below
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.after.medicaid.loss',
                        value = expression(oral.prep.coverage.never.medicaid * p.never.medicaid.of.uninfected +
                                               oral.prep.coverage.keep.medicaid * p.keep.medicaid.of.uninfected +
                                               oral.prep.coverage.lose.medicaid * p.lose.medicaid.of.uninfected)
)

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage',
                        value = expression(oral.prep.coverage.after.medicaid.loss * medicaid.affects.prep.switch +
                                               super.oral.prep.coverage * (1-medicaid.affects.prep.switch)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.after.medicaid.loss',
                        value = expression(lai.prep.coverage.never.medicaid * p.never.medicaid.of.uninfected +
                                               lai.prep.coverage.keep.medicaid * p.keep.medicaid.of.uninfected +
                                               lai.prep.coverage.lose.medicaid * p.lose.medicaid.of.uninfected)
)


register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage',
                        value = expression(lai.prep.coverage.after.medicaid.loss * medicaid.affects.prep.switch +
                                               super.lai.prep.coverage * (1-medicaid.affects.prep.switch)))

# Proportions never/keeping/losing medicaid
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.never.medicaid.of.uninfected',
                        value = expression(1-p.on.medicaid.of.uninfected))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.lose.medicaid.of.uninfected',
                        value = expression(p.on.medicaid.of.uninfected * p.medicaid.loss.of.uninfected))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'p.keep.medicaid.of.uninfected',
                        value = expression(p.on.medicaid.of.uninfected * (1-p.medicaid.loss.of.uninfected)))


# Proportions on PrEP by medicaid status

# Convert to odds so we can apply an ODDS ratio for uninsured and on mediaid
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.odds',
                        value = expression(super.oral.prep.coverage / (1-super.oral.prep.coverage)))

# We'll assume that if you lose medicaid, your coverage goes to what it is for uninsured people
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.lose.medicaid',
                        value = 'oral.prep.coverage.uninsured')

# Fundamentally, this OR is just going to be an input - we don't have anything to calibrate it to
#  We'll estimate the OR for taking PrEP if uninsured nationally, and assume that is a causal effect that holds locally
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.uninsured',
                        value = expression(1 / (1 + 1/(oral.prep.coverage.odds * OR.prep.uninsured) )))

# For medicaid, same as for uninsured
# Again, we don't have anything to calibrate to, so we'll just calculate a national OR for PrEP if on medicaid and assume that holds locally
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.keep.medicaid',
                        value = 'oral.prep.coverage.medicaid')

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.medicaid',
                        value = expression(1 / (1 + 1/(oral.prep.coverage.odds * OR.prep.medicaid) )))

# We now have total prep coverage, and coverage among medicaid
# So we calculate coverage among not-on-medicaid as whatever is necessary to sum to total coverage (ie, weighted total coverage - coverage on medicaid)
# Start with
# total.coverage * 1 = medicaid.coverage * p.on.medicaid + non-medicaid.coverage * (1-p.on.medicaid)
#   and solve for non-medicaid.coverage
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'oral.prep.coverage.never.medicaid',
                        value = expression( (super.oral.prep.coverage - p.on.medicaid.of.uninfected * oral.prep.coverage.medicaid) / (1 - p.on.medicaid.of.uninfected) ))

# Ditto for long-acting injectable
register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.odds',
                        value = expression(super.lai.prep.coverage / (1-super.lai.prep.coverage)))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.lose.medicaid',
                        value = 'lai.prep.coverage.uninsured')

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.uninsured',
                        value = expression(1 / (1 + 1/(lai.prep.coverage.odds * OR.prep.uninsured) )))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.keep.medicaid',
                        value = 'lai.prep.coverage.medicaid')

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.medicaid',
                        value = expression(1 / (1 + 1/(lai.prep.coverage.odds * OR.prep.medicaid) )))

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'lai.prep.coverage.never.medicaid',
                        value = expression( (super.lai.prep.coverage - p.on.medicaid.of.uninfected * lai.prep.coverage.medicaid) / (1 - p.on.medicaid.of.uninfected) ))

####--------------####
####--------------####
####-- OUTCOMES --####
####--------------####
####--------------####


##----------------------------------##
##-- NUMBER of PEOPLE on MEDICAID --##
##----------------------------------##

# Number of all people on Medicaid
# Sum of number uninfected, diagnosed PWH, undiagnosed PWH
track.cumulative.outcome(MEDICAID.SPECIFICATION, 
                         name = 'medicaid.recipients',
                         outcome.metadata = create.outcome.metadata(display.name = 'Medicaid Recipients',
                                                                    description = "Number of Individuals Receiving Medicaid",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = "People on Medicaid",
                                                                    units = "people",
                                                                    singular.unit = 'person'),
                         value = expression(uninfected.medicaid.recipients +
                                                undiagnosed.medicaid.recipients + 
                                                pwh.on.medicaid),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'medicaid.recipients')

# Number Diagnosed PWH on Medicaid
# I don't think this will be a calibration target, but I think we'll want to know it
track.integrated.outcome(MEDICAID.SPECIFICATION,
                         name = 'pwh.on.medicaid',
                         outcome.metadata = create.outcome.metadata(display.name = 'PWH on Medicaid',
                                                                    description = "Number of Diagnosed People with HIV Receiving Medicaid",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'PWH on Medicaid',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'p.diagnosed.pwh.on.medicaid',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'))

# Number of undiagnosed on Medicaid
# Input to n on medicaid
track.integrated.outcome(MEDICAID.SPECIFICATION,
                         name = 'undiagnosed.medicaid.recipients',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         save = F,
                         value.to.integrate = 'infected',
                         multiply.by = 'p.undiagnosed.pwh.on.medicaid',
                         subset.dimension.values = list(continuum='undiagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'))

# Number of uninfected on Medicaid
# input to n on medicaid and used to calibrate testing among medicaid
track.integrated.outcome(MEDICAID.SPECIFICATION,
                         name = 'uninfected.medicaid.recipients',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         save = F,
                         value.to.integrate = 'uninfected',
                         multiply.by = 'p.on.medicaid.of.uninfected',
                         keep.dimensions = c('location','age','race','sex','risk'))


# Number of uninfected who are uninsured
# Used to calibrate testing among uninsured
# For the sake of simplifying the parameter space, we will assume that n uninsured is approximately = n uninfected uninsured
track.integrated.outcome(MEDICAID.SPECIFICATION,
                         name = 'uninfected.uninsured',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Uninsured',
                                                                    description = "Number of Uninsured People without HIV",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Uninsured People',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'uninfected',
                         multiply.by = 'p.uninsured.of.uninfected',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'n.uninsured')

##---------------------------------------##
##-- PROBABILITY of RECEIVING HIV TEST --##
##---------------------------------------##

# P tested if on Medicaid - calibrate to BRFSS

track.cumulative.proportion.from.rate(MEDICAID.SPECIFICATION,
                                      name = 'testing.medicaid',
                                      outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested on Medicaid',
                                                                                 description = "The Proportion of Medicaid Recipients who Received an HIV Test in the Past Year",
                                                                                 scale = 'proportion',
                                                                                 axis.name = 'Proportion Tested',
                                                                                 units = '%'),
                                      rate.value = 'medicaid.general.population.testing.over.18',
                                      denominator.outcome = 'medicaid.cumulative.uninfected.over.18',
                                      keep.dimensions = c('location','age','race','sex','risk'),
                                      corresponding.data.outcome = 'proportion.tested',
                                      rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                                      save = T)

track.cumulative.outcome(MEDICAID.SPECIFICATION,
                         name = 'medicaid.cumulative.uninfected.over.18',
                         value = expression(uninfected.medicaid.recipients * medicaid.fraction.population.over.18),
                         scale = 'non.negative.number',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                         save = F,
                         outcome.metadata = NULL)

# P tested if uninsured - calibrate to BRFSS

track.cumulative.proportion.from.rate(MEDICAID.SPECIFICATION,
                                      name = 'testing.uninsured',
                                      outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested who are Uninsured',
                                                                                 description = "The Proportion of Uninsured Population who Received an HIV Test in the Past Year",
                                                                                 scale = 'proportion',
                                                                                 axis.name = 'Proportion Tested',
                                                                                 units = '%'),
                                      rate.value = 'uninsured.general.population.testing.over.18',
                                      denominator.outcome = 'uninsured.cumulative.uninfected.over.18',
                                      keep.dimensions = c('location','age','race','sex','risk'),
                                      corresponding.data.outcome = 'proportion.tested',
                                      rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                                      save = T)

track.cumulative.outcome(MEDICAID.SPECIFICATION,
                         name = 'uninsured.cumulative.uninfected.over.18',
                         value = expression(uninfected.uninsured * uninsured.fraction.population.over.18),
                         scale = 'non.negative.number',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                         save = F,
                         outcome.metadata = NULL)

##-----------------##
##-- REGISTER IT --##
##-----------------##

register.model.specification(MEDICAID.SPECIFICATION)

