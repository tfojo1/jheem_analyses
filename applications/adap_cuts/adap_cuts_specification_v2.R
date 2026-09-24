source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('applications/adap_cuts/adap_cuts_specification_helpers.R')
source('applications/ryan_white/ryan_white_specification_helpers.R')

INSURANCE.CATEGORIES = c('medicaid.only', 
                         'medicare.only', 
                         'dual.medicare.medicaid.only', 
                         'private.only', 
                         'uninsured.only',
                         'medicaid.and.uninsured',
                         'medicare.and.uninsured',
                         'dual.medicare.medicaid.and.uninsured',
                         'private.and.uninsured'
                         )
ADAP.SERVICE.CATEGORIES = c('full.pay.only', # F 
                            'premium.only', # P 
                            'premium.and.cost.sharing', # PCs 
                            'cost.sharing.only', # Cs  
                            'full.pay.and.premium', # FP 
                            'full.pay.and.premium.and.cost.sharing', # FPCs
                            'full.pay.and.cost.sharing' # FCs 
                           
                             # Cost-sharing subcategories: 
                            # D: deductible
                            # Cp: co-pay assistance 
)

INCOME.BRACKETS = c('0-100', '101-138', '139-200', '201-250', '251-300','301-400','401-500','>500')

ADAP.SPECIFICATION = create.jheem.specification(version='adap',
                                              iteration = '1',
                                              description='Model to study the impacts of cuts to ADAP on HIV transmission',
                                              parent.version = 'ehe',
                                              compartments.for.outcomes = list(
                                                  income = INCOME.BRACKETS,
                                                  insurance = INSURANCE.CATEGORIES,
                                                  service.type = ADAP.SERVICE.CATEGORIES)
                                              )



####-----------------------------####
####-----------------------------####
####-- INPUTS for INTERVENTION --####
####-----------------------------####
####-----------------------------####


##-- NEW INCOME THRESHOLDS --##
register.model.quantity(ADAP.SPECIFICATION,
                        name = "adap.full.pay.fpl.threshold",
                        value = 'baseline.adap.full.pay.fpl.threshold',
                        scale = 'non.negative.number')

register.model.quantity(ADAP.SPECIFICATION,
                        name = "adap.premium.fpl.threshold",
                        value = 'baseline.adap.premium.fpl.threshold',
                        scale = 'non.negative.number')

register.model.quantity(ADAP.SPECIFICATION,
                        name = "adap.cost.sharing.fpl.threshold",
                        value = 'baseline.adap.cost.sharing.fpl.threshold',
                        scale = 'non.negative.number')


##-- INPUTS: EFFECTS of ELIGIBILITY RESTRICTIONS --##
# Loss: 4 effects
register.model.element(ADAP.SPECIFICATION,
                       name = "lose.F.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.P.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.PCs.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.Cs.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

# Change: 3 effects
register.model.element(ADAP.SPECIFICATION,
                       name = "change.P.to.F.suppression.rr",
                       value = 1,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCs.to.F.suppression.rr", 
                       value = 1,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCs.to.P.suppression.rr",
                       value = 0.5,
                       scale = 'proportion')


##-- INPUTS: EFFECTS of FORMULARY RESTRICTIONS --##
register.model.element(ADAP.SPECIFICATION,
                       name = "change.F.formulary.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCp.formulary.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.Cp.formulary.suppression.rr", 
                       value = 0.5,
                       scale = 'proportion')

##-- INPUTS: HOW MANY EXPERIENCE FORMULARY RESTRICTIONS --##
register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.F.clients.with.formulary.change", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.PCp.clients.with.formulary.change", 
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.Cp.clients.with.formulary.change", 
                       value = 0.5,
                       scale = 'proportion')

## among those who change from anything to full pay, what proportion have a formulary change (proportion who change will be dependent on income eligibility)
register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.change.to.F.with.formulary.change", 
                       value = 0.5,
                       scale = 'proportion')


####----------------------------------####
####----------------------------------####
####-- INPUTS for SETUP/CALIBRATION --####
####----------------------------------####
####----------------------------------####

##-----------------------------##
##-- INPUTS: ADAP THRESHOLDS --##
##-----------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.full.pay.fpl.threshold",
                       get.value.function = get.state.baseline.adap.full.pay.threshold,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.premium.fpl.threshold",
                       get.value.function = get.state.baseline.adap.premium.threshold,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.cost.sharing.fpl.threshold",
                       get.value.function = get.state.baseline.adap.cost.sharing.threshold,
                       scale = 'non.negative.number')

##------------------------------------##
##-- INPUTS: STATE LEVEL PARAMETERS --##
##------------------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "adap.covers.copay",
                       get.value.function = state.adap.cost.sharing.covers.copays,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "adap.covers.deductible",
                       get.value.function = state.adap.cost.sharing.covers.deductibles,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "adap.allows.medicaid.only",
                       get.value.function = state.adap.allows.medicaid.only,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "adap.allows.medicare.and.medicaid",
                       get.value.function = get.state.adap.allows.medicare.and.medicaid,
                       scale = 'non.negative.number')

##--------------------##
##-- INPUTS: INCOME --##
##--------------------##

# @todo - placeholder functional form for now
register.model.element(ADAP.SPECIFICATION,
                       name = 'adap.fpl.median',
                       functional.form = create.static.functional.form(
                           value = array(200,
                                         dim = c(age=5, race=3, sex=3, risk=3),
                                         dimnames = list(age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                         race=c('black','hispanic','other'),
                                                         sex=c('heterosexual_male','msm','female'),
                                                         risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'))),
                           link = 'identity',
                           value.is.on.transformed.scale = F), 
                       scale = 'number')

# @todo placeholder functional for for now
register.model.element(ADAP.SPECIFICATION,
                       name = 'adap.fpl.cv',
                       functional.form = create.static.functional.form(
                           value = array(0.5,
                                         dim = c(age=5, race=3, sex=3, risk=3),
                                         dimnames = list(age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                         race=c('black','hispanic','other'),
                                                         sex=c('heterosexual_male','msm','female'),
                                                         risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'))),
                           link = 'log',
                           value.is.on.transformed.scale = F),
                       scale = 'non.negative.number')

# @todo placeholder functional for for now
register.model.element(ADAP.SPECIFICATION,
                       name = 'general.over.adap.fpl.median.multiplier',
                       functional.form = create.static.functional.form(value = 2,
                                                                       link = 'log',
                                                                       value.is.on.transformed.scale = T), 
                       scale = 'ratio')

# @todo placeholder functional for for now
register.model.element(ADAP.SPECIFICATION,
                       name = 'general.over.adap.fpl.cv.multiplier',
                       functional.form = create.static.functional.form(value = 1,
                                                                       link = 'log',
                                                                       value.is.on.transformed.scale = F),
                       scale = 'ratio')


##-------------------------------------##
##-- INPUTS: SSI. MEDICAID, MEDICARE --##
##-------------------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = 'medicaid.fpl.threshold',
                       get.value.function = get.state.medicaid.threshold,
                       scale = 'non.negative.number')

##-- BASELINE ELIGIBILITY --##
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.baseline.benefits.eligible',
                       get.functional.form.function = get.p.baseline.benefits.eligible.functional.form,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'rr.ssi.medicaid.if.benefits.eligible',
                       value = 1,
                       scale = 'proportion')



##-- MEDICARE --##
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.medicare.by.age.if.benefits.eligible',
                       get.functional.form.function = get.p.medicare.by.age.eligibility.functional.form,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.medicare.if.not.by.age.if.benefits.eligible',
                       get.functional.form.function = get.p.medicare.if.not.by.age.if.benefits.eligible.functional.form,
                       scale = 'proportion')


##-- SSI --##
register.model.element(ADAP.SPECIFICATION,
                       name = 'ssi.benefit.fpl',
                       get.value.function = get.state.ssi.benefit.fpl,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'ssi.breakeven.fpl',
                       get.value.function = get.state.ssi.breakeven.fpl,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.ssi.if.income.and.benefits.eligible',
                       get.functional.form.function = get.p.ssi.if.income.and.benefits.eligible.functional.form,
                       scale = 'proportion')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.base.ssi.and.medicaid.eligible',
                        value = expression(p.baseline.benefits.eligible * rr.ssi.medicaid.if.benefits.eligible))

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.ssi.without.medicaid',
                       functional.form = get.p.ssi.without.medicaid(),
                       scale = "proportion")

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.ssi.if.income.eligible', # probability that you have SSI if your income is below the threshold
                        value = expression(p.base.ssi.and.medicaid.eligible * p.ssi.if.income.and.benefits.eligible *
                                               (p.ssi.without.medicaid + (1-p.ssi.without.medicaid) * adap.allows.medicaid.only)))

##-- MEDICAID --##
# Given that you have medicare and do not have SSI/are not under the Medicaid income threshold, what is the probability you are on medicaid
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.medicaid.not.by.income.or.ssi.if.medicare',
                       get.functional.form.function = get.p.medicaid.not.by.income.or.ssi.if.medicare.functional.form, #@todo - need to fill in
                       scale = 'proportion')

# Given that you do NOT have medicare and do not have SSI/are not under the Medicaid income threshold, what is the probability you are on medicaid
register.model.element(ADAP.SPECIFICATION, 
                       name = 'p.medicaid.not.by.income.or.ssi.if.no.medicare.and.benefits.eligible',
                       get.functional.form.function = get.p.medicaid.not.by.income.or.ssi.if.no.medicare.and.benefits.eligible.functional.form,
                       scale = 'proportion')

#-- UNINSURED --#
# The probability that you are uninsured given that you don't have public insurance is a function of your income

#@todo - need to fill in real estimates from data
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.midpoint',
                       value = 250, # the income at which the p of uninsurance is halfway between min and max p
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.min', # min p uninsured
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.max', # max p uninsured
                       value = 0.95,
                       scale = 'proportion')


##-----------------------##
##-- INPUTS: P on ADAP --##
##-----------------------##
register.model.element(ADAP.SPECIFICATION,
                       name = 'baseline.proportion.pwh.with.adap',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.adap.functional.form", version = 'rw'),
                       functional.form.from.time = 2010)


##---------------------------------##
##-- INPUTS: SUPPRESSION on ADAP --##
##---------------------------------##

# Given what combination of ADAP services you receive, what is your probability of being suppressed

# Full-pay only
register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.F.only.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.only.suppression.functional.form(),
                       functional.form.from.time = 2010)

# Insurance assistance only
register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.P.only.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.Cs.only.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.PCs.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

# Full-pay PLUS Insurance assistance
register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FP.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FCs.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FPCs.suppressed', 
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)


##------------------------------------------------##
##-- INPUTS: FRACTION OF TIME SPENT if A CLIENT --##
##------------------------------------------------##

## ADAP CATEGORIES ##  (repeat below for insurance category overlaps)
# Individual categories: 
# F: full pay only 
# P: premium only 
# Cs: cost-sharing only 

# At any given time: 
# F, P, Cs, or PCs
    # Cannot have full-pay at the same time as either premium or cost-sharing, but can have premium and cost-sharing at the same time 

# Over the course of a year, can have: 
#   Only one category during that year (3): 
#       F, P, or Cs 
#   Two categories during that year (3): 
#       FP: full pay and premium
#       FCs: full pay and cost-sharing
#       PCs: premium and cost-sharing
#   All three categories during that year (1)
#       FPCs: full pay, premium, and cost-sharing

# Later on, cost-sharing split into: deductible and/or co-pay assistance
# D: deductible
# Cp: co-pay assistance 


### ONLY ONE CATEGORY DURING THAT YEAR (3): F, P, or Cs ###
# Among those who had only one service during a year, what fraction of the year were they covered (assumes not the whole year)
    # We will use the same value for all categories, but keep them separate for now for max flexibility 
    # maybe replace with 0.84 based on 2023 NASTAD report, but look at other years too

# FULL PAY ONLY (F)
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.F',  
                       value = 0.875, 
                       scale = 'proportion')

# PREMIUM ONLY (P)
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.P',  
                       value = 0.875, 
                       scale = 'proportion')

# COST-SHARE ONLY (Cs) 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.Cs', 
                       value = 0.875, 
                       scale = 'proportion')


### TWO CATEGORIES DURING THAT YEAR (3): FP, FCs, PCs ###
    # People in this category could have spent their time in one of three states: (a) no ADAP, (b) category 1, (c) category 2
    # which are (usually) mutually exclusive and must always add to one

# FULL PAY AND PREMIUM (FP)
    # Mutually exclusive 
# First, what time covered at all (for now, same as above among full-pay only)
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.FP', 
                       value = 0.875,
                       scale = 'proportion')

# Next, what time spent on full-pay out of that coverage time 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.F.of.time.covered.among.FP',  
                       value = 0.25/0.875, # setting this so that it is ~ 3 months of the year on full-pay
                       scale = 'proportion')

# Finally, multiply these two to get fraction time spent on full-pay out of the full calendar year 
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'fraction.time.F.among.FP', # fraction.time.on.adap.full.pay.from.full.pay.and.premium.without.cost.sharing
                       value = expression(fraction.time.covered.among.FP*fraction.time.F.of.time.covered.among.FP),
                       scale = 'proportion')

# and the same thing for premium (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'fraction.time.P.among.FP', # fraction.time.on.adap.premium.without.cost.sharing.from.with.full.pay
                       value = expression(fraction.time.covered.among.FP*(1-fraction.time.F.of.time.covered.among.FP)),
                       scale = 'proportion')

# FULL PAY AND COST-SHARING (FCs)
    # Mutually exclusive; same steps as above 
# First, what time covered at all 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.FCs', 
                       value = 0.875,
                       scale = 'proportion')

# Next, what time spent on full-pay out of that coverage time 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.F.of.time.covered.among.FCs',  
                       value = 0.25/0.875, 
                       scale = 'proportion')

# Finally, multiply these two to get fraction time spent on full-pay out of the full calendar year 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.F.among.FCs', # fraction.time.on.adap.full.pay.from.full.pay.and.cost.sharing.without.premium
                        value = expression(fraction.time.covered.among.FCs*fraction.time.F.of.time.covered.among.FCs),
                        scale = 'proportion')

# and the same thing for cost-sharing (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.Cs.among.FCs', # fraction.time.on.adap.cost.sharing.without.premium.from.with.full.pay
                        value = expression(fraction.time.covered.among.FCs*(1-fraction.time.F.of.time.covered.among.FCs)),
                        scale = 'proportion')

# PREMIUM AND COST-SHARING (PCs)
    # NOT mutually exclusive: could in theory spend time in: neither, each individually, or both
    # For simplicity, assume full coverage period was both (not either individually)
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.PCs', # fraction.time.on.adap.premium.and.cost.sharing.from.without.full.pay
                       value = 0.875,
                       scale = 'proportion')


### ALL THREE CATEGORIES DURING THAT YEAR (1): FPCs ###
    # Again, PCs is not mutually exclusive: could spend time in P, Cs, or both 
    # For simplicity, assume people were either in F or PCs combined (not P or Cs individually)
# First, what time covered at all 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.FPCs', 
                       value = 0.875,
                       scale = 'proportion')

# Next, what time spent on full-pay out of that coverage time 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.F.of.time.covered.among.FPCs',  
                       value = 0.25/0.875, 
                       scale = 'proportion')

# Finally, multiply these two to get fraction time spent on full-pay out of the full calendar year 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.F.among.FPCs', # fraction.time.on.adap.full.pay.from.full.pay.and.premium.and.cost.sharing
                        value = expression(fraction.time.covered.among.FPCs*fraction.time.F.of.time.covered.among.FPCs),
                        scale = 'proportion')

# and the same thing for premium + cost-sharing (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.PCs.among.FPCs', # fraction.time.on.adap.premium.and.cost.sharing.from.with.full.pay
                        value = expression(fraction.time.covered.among.FPCs*(1-fraction.time.F.of.time.covered.among.FPCs)),
                        scale = 'proportion')


## INSURANCE CATEGORIES ## 
# Individual categories: 
# uninsured, medicaid, medicare, medicare + medicaid, private insurance

# Over the course of a year, can have: 
# unin/medicaid, unin/medicare, unin/medicare+medicaid, unin/private

# Melissa - we will use these for outcome tracking 

# 50/50 Medicaid/uninsured 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.uninsured.among.uninsured.and.medicaid',  
                       value = 0.5, 
                       scale = 'proportion')

# 50/50 Medicare/uninsured 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.uninsured.among.uninsured.and.medicare',  
                       value = 0.5, 
                       scale = 'proportion')

# 50/50 Medicare+Medicaid/uninsured 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.uninsured.among.uninsured.and.medicare.and.medicaid',  
                       value = 0.5, 
                       scale = 'proportion')

# 50/50 Private/uninsured 
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.uninsured.among.uninsured.and.private',  
                       value = 0.5, 
                       scale = 'proportion')


##---------------------------------------------------------------------------##
##-- INPUTS: PROPORTION OF COST-SHARING CLIENTS WHO RECEIVE COPAY SERVICES --##
##---------------------------------------------------------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.Cs.clients.with.Cp.if.allowed", 
                       functional.form = get.proportion.Cs.clients.with.Cp.functional.form(), 
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.PCs.clients.with.Cp.if.allowed", 
                       functional.form = get.proportion.Cs.clients.with.Cp.functional.form(), # we are PURPOSEFULLY repeating the same function here (assuming same proportion with Cp regardless of F/P)
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.FCs.clients.with.Cp.if.allowed", 
                       functional.form = get.proportion.Cs.clients.with.Cp.functional.form(), # we are PURPOSEFULLY repeating the same function here
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.FPCs.clients.with.Cp.if.allowed", 
                       functional.form = get.proportion.Cs.clients.with.Cp.functional.form(), # we are PURPOSEFULLY repeating the same function here
                       scale = 'proportion')


#-- Quickly, put these together with the state-level inputs --#
#   The three quantities below evaluate to:
#   - p.adap.<x>.clients.with.copay.if.allowed  IF  both copay and deductible are covered by state ADAP
#   - 0                                         IF  state ADAP does not cover copays
#   - 1                                         IF  state ADAP does not cover deductibles
register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.PCs.clients.with.Cp", 
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.PCs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))

register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.FPCs.clients.with.Cp", 
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.FPCs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))

register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.Cs.clients.with.Cp", 
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.Cs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))

register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.FCs.clients.with.Cp", 
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.FCs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))


##----------------------------------------------##
##-- INPUTS: P ADAP SERVICE TYPE GIVEN INCOME --##
##----------------------------------------------##

# we presume that: p_full_pay = min + (max-min) / (1 + exp(slope * (income - midpoint)))
# *NB that slope here is constrained to be positive - ie, a strictly decreasing p with increasing income

# These are the inputs to P1-6 in the section "DISTRIBUTE P by INCOME on ADAP INTO SERVICE CATEGORIES"

# P1: F.only
# P2: Fplus.among.not.F.only
# P3: P.among.Fplus
# P4: Cs.among.FP
# P5: P.among.no.F
# P6: Cs.among.P


#-- P1: Probability of having ONLY full-pay services if on ADAP (F.only) --#
# Given ADAP, there is a probability of having full-pay; defined by these 4 parameters 
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.F.only.midpoint', # midpoint of probability that you have full pay only, given that we know you have ADAP 
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.F.only.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.F.only.min', 
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.F.only.max', 
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.F.only.medicaid', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.F.only.medicare', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.F.only.medicare.and.medicaid', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.F.only.private', 
                       value = log(0.1),
                       scale = 'number') # ratio



#-- P2: Probablity of having full-pay plus other services if on ADAP not full pay only (Fplus.among.not.F.only) --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Fplus.among.not.F.only.midpoint', 
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Fplus.among.not.F.only.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Fplus.among.not.F.only.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Fplus.among.not.F.only.max',
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Fplus.among.not.F.only.medicaid', 
                       value = log(0.2),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Fplus.among.not.F.only.medicare', 
                       value = log(0.2),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Fplus.among.not.F.only.medicare.and.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Fplus.among.not.F.only.private',
                       value = log(1),
                       scale = 'number') # ratio



#-- P3: Probability of receiving premium assistance if also on full pay (P.among.Fplus) --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.Fplus.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.Fplus.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.Fplus.min', 
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.Fplus.max', 
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.Fplus.medicaid', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.Fplus.medicare', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.Fplus.medicare.and.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.Fplus.private',
                       value = log(1),
                       scale = 'number') # ratio


#--P4: Probability of receiving cost-sharing assistance if on full.pay and premium assistance (Cs.among.FP) --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.FP.midpoint', 
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.FP.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.FP.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.FP.max',
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.FP.medicaid',
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.FP.medicare',
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.FP.medicare.and.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.FP.private', 
                       value = log(1),
                       scale = 'number') # ratio


#-- P5: Probability of receiving premium assistance if not receiving any full pay services (P.among.no.F) --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.no.F.midpoint', 
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.no.F.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.no.F.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.P.among.no.F.max',
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.no.F.medicaid', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.no.F.medicare', 
                       value = log(0.1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.no.F.medicare.and.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.P.among.no.F.private', 
                       value = log(1),
                       scale = 'number') # ratio


#-- P6: Probability of receiving cost-sharing assistance if on premium assistance but not full pay (Cs.among.P)--#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.P.midpoint', 
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.P.slope', 
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.P.min', 
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.Cs.among.P.max', 
                       value = 0.95,
                       scale = 'proportion')

# Then, apply an odds ratio to that probability, based on insurance (medicaid, medicare, medicare and medicaid, private)
register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.P.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.P.medicare', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.P.medicare.and.medicaid', 
                       value = log(1),
                       scale = 'number') # ratio

register.model.element(ADAP.SPECIFICATION,
                       name = 'log.OR.Cs.among.P.private', 
                       value = log(1),
                       scale = 'number') # ratio


####---------------------------####
####---------------------------####
####-- CALCULATED QUANTITIES --####
####---------------------------####
####---------------------------####

# need to make quantities for
# - max.income

##-----------------------------------------------------##
##-- CALCULATED: INCOME DISTRIBUTION of ADAP CLIENTS --##
##-----------------------------------------------------##

# Max ADAP income threshold - just defined by the state - gives us the upper bound for how high we need to break up the % FPL 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'max.adap.baseline.income',
                        value = calculate.max.adap.baseline.income)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'adap.incomes',
                        value = distribute.adap.incomes)

# Calculate income distribution, stratified by SSI, for all ADAP clients 
# This is among ADAP, probability of being in a certain income bracket AND having SSI (or not having SSI, below)
# numerator: have ADAP, have SSI, in a certain income bracket; denominator: everyone with ADAP
# bounded by 75% FPL (SSI max benefit), and SSI breakeven (~150%; benefit is 0 because no longer qualifying for it)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.ssi.among.adap', 
                        value = calculate.baseline.p.of.income.with.ssi.among.adap)  

# numerator: have ADAP, no SSI, in a certain income bracket; denominator: everyone with ADAP 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.without.ssi.among.adap', 
                        value = calculate.baseline.p.of.income.without.ssi.among.adap) 



# Fold in Medicare
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.medicare.and.ssi.among.adap', 
                        value = expression(baseline.p.of.income.with.ssi.among.adap *
                                               p.baseline.benefits.eligible *
                                               (p.medicare.by.age.if.benefits.eligible +
                                                    (1-p.medicare.by.age.if.benefits.eligible) * p.medicare.if.not.by.age.if.benefits.eligible))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.medicare.without.ssi.among.adap', 
                        value = expression(baseline.p.of.income.without.ssi.among.adap *
                                               p.baseline.benefits.eligible *
                                               (p.medicare.by.age.if.benefits.eligible +
                                                    (1-p.medicare.by.age.if.benefits.eligible) * p.medicare.if.not.by.age.if.benefits.eligible))
)


# Fold in Medicaid
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'income.is.below.medicaid.threshold', # needs dimension of income; either 0 or 1
                        value = calculate.income.is.below.medicaid.threshold)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.medicare.and.medicaid.among.adap', 
                        value = expression(adap.allows.medicare.and.medicaid * (
                            baseline.p.of.income.with.medicare.and.ssi.among.adap +
                                baseline.p.of.income.with.medicare.without.ssi.among.adap * 
                                (income.is.below.medicaid.threshold +
                                     (1-income.is.below.medicaid.threshold) *
                                     p.medicaid.not.by.income.or.ssi.if.medicare * p.base.ssi.and.medicaid.eligible)
                        )))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.medicaid.among.adap',
                        value = expression(adap.allows.medicaid.only * (
                            (baseline.p.of.income.with.ssi.among.adap - baseline.p.of.income.with.medicare.and.ssi.among.adap) +
                                (baseline.p.of.income.without.ssi.among.adap - baseline.p.of.income.with.medicare.without.ssi.among.adap) *
                                (income.is.below.medicaid.threshold +
                                     (1-income.is.below.medicaid.threshold) *
                                     p.medicaid.not.by.income.or.ssi.if.no.medicare.and.benefits.eligible * p.base.ssi.and.medicaid.eligible)
                        )))
                      


# Tally up the differences to make five categories that sum to 1:
#   1) medicare + medicaid (already calculated)
#   2) medicare without medicaid
#   3) medicaid without medicare (already calculated)
#   4) private insurance
#   5) uninsured

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.with.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.ssi.among.adap + 
                                               baseline.p.of.income.with.medicare.without.ssi.among.adap -
                                               baseline.p.of.income.with.medicare.and.medicaid.among.adap))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.without.medicare.or.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.ssi.among.adap + 
                                               baseline.p.of.income.without.ssi.among.adap -
                                               baseline.p.of.income.with.medicare.and.medicaid.among.adap -
                                               baseline.p.of.income.with.medicare.among.adap -
                                               baseline.p.of.income.with.medicaid.among.adap))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.uninsured.given.income.and.no.public.insurance',
                        value = calculate.p.uninsured.given.income.and.no.public.insurance)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.uninsured.among.adap',
                        value = expression(baseline.p.of.income.without.medicare.or.medicaid.among.adap * p.uninsured.given.income.and.no.public.insurance))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.income.private.among.adap', 
                        value = expression(baseline.p.of.income.without.medicare.or.medicaid.among.adap - baseline.p.of.income.uninsured.among.adap))



##------------------------------------------------------------------------##
##-- CALCULATED: DISTRIBUTE P by INCOME on ADAP INTO SERVICE CATEGORIES --##
##------------------------------------------------------------------------##

# Given ADAP and income and Medicare/Medicaid, we need to split them into the following 7 categories, and they need to sum to 1
# Safest way to do this is through nesting 

# Over the course of a year, can have: 
#   Only one category during that year (3): 
#       F, P, or Cs
#   Two categories during that year (3): 
#       FP: full pay and premium
#       FCs: full pay and cost-sharing
#       PCs: premium and cost-sharing
#   All three categories during that year (1)
#       FPCs: full pay, premium, and cost-sharing

# Nested proportions (to ensure they sum to 1):
# P1: First, F only
# P2: Given not F only; probability of any F? (F+)
# P3: Given F+, probability of any P? (FP, FPCs)
# P4: Given FP, probability of FPCs (vs FP only)? 
# P5: Given no F, probability of any P? (P, PCs) 
# P6: Given P, probability of PCs (vs P only)? 

# --> 6 probabilities to split into 7 categories 
#           P3 and P5 might be the same (probability of P)
#           P4 and P6 might be the same (probability of Cs)


# Baseline implies before any threshold changes 

# Repeated for: 
# medicaid, medicare, medicare + medicaid, private insurance, uninsured (uninsured: all are on F only - can't have premiums or cost-share without insurance)
    
# For the below section, we mean they had Medicaid (or other insurance type) AT THAT GIVEN MOMENT; not over the course of the year 

#-- Distribute for Medicaid --#
# Inputs: 
# P1: First, F only 
# P2: Given not F only; probability of any F? (F+)
# P3: Given F+, probability of any P? (FP, FPCs)
# P4: Given FP, probability of FPCs (vs FP only)? 
# P5: Given no F, probability of any P? (P, PCs) 
# P6: Given P, probability of PCs (vs P only)? 

# P3 and P5 might be the same (probability of P)
# P4 and P6 might be the same (probability of Cs)

# Outputs (need to get to 7 proportions that sum to 1): 
    # F, P, Cs, 
    # FP, FCs, PCs
    # FPCs


# INPUTS - these are our priors, we will use them below; calculated from the 4 parameters of the logistic functions (defined in INPUTS: P ADAP SERVICE TYPE GIVEN INCOME)
# P1 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicaid', 
                        value = expression(p.F.only.min + (p.F.only.max - p.F.only.min) /
                                                   (1 + exp(p.F.only.slope * (adap.incomes - p.F.only.midpoint) + 
                                                                log.OR.F.only.medicaid)))) # this last term will be the only thing that changes for other insurance types

# P2
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Fplus.among.not.F.only.income.medicaid', 
                        value = expression(p.Fplus.among.not.F.only.min + (p.Fplus.among.not.F.only.max - p.Fplus.among.not.F.only.min) /
                                               (1 + exp(p.Fplus.among.not.F.only.slope * (adap.incomes - p.Fplus.among.not.F.only.midpoint) + 
                                                            log.OR.Fplus.among.not.F.only.medicaid))))

# P3
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.Fplus.income.medicaid',
                        value = expression(p.P.among.Fplus.min + (p.P.among.Fplus.max - p.P.among.Fplus.min) /
                                               (1 + exp(p.P.among.Fplus.slope * (adap.incomes - p.P.among.Fplus.midpoint) + 
                                                            log.OR.P.among.Fplus.medicaid))))

# P4
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.FP.income.medicaid',
                        value = expression(p.Cs.among.FP.min + (p.Cs.among.FP.max - p.Cs.among.FP.min) /
                                               (1 + exp(p.Cs.among.FP.slope * (adap.incomes - p.Cs.among.FP.midpoint) + 
                                                            log.OR.Cs.among.FP.medicaid))))

# P5
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.no.F.income.medicaid', 
                        value = expression(p.P.among.no.F.min + (p.P.among.no.F.max - p.P.among.no.F.min) /
                                               (1 + exp(p.P.among.no.F.slope * (adap.incomes - p.P.among.no.F.midpoint) + 
                                                            log.OR.P.among.no.F.medicaid))))

# P6
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.P.income.medicaid',
                        value = expression(p.Cs.among.P.min + (p.Cs.among.P.max - p.Cs.among.P.min) /
                                               (1 + exp(p.Cs.among.P.slope * (adap.incomes - p.Cs.among.P.midpoint) + 
                                                            log.OR.Cs.among.P.medicaid))))


# OUTPUTS 
# OUTPUT 1: F only
# Basically just set to input p1, but need to multiply in the "among adap" part 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicaid.among.adap', # probability of F, and a given income, and medicaid; denominator: all adap #
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # just have to multiply in the "among adap" 
                                               baseline.p.of.F.only.income.medicaid)) 

# OUTPUT 2: FP  
# (FP)/all = (FP/FPx) * (FPx/Fx) * (Fx/all)
#          = (1-p4) * p3 * (Fx/all)
#       (Fx/all) = (Fx/(Fx + no F))*(1-F/all)
#                = p2 * (1 - p1)
#                 first quantity: out of the people who are not getting full pay only, what proportion get any full pay? (p2)
#                 second quantity: all the people who are not getting full pay only (all except p1)
# so, full equation: 
# (FP)/all = (1-p4) * p3 * p2 * (1 - p1)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.income.medicaid.among.adap', # probability of FP, and a given income, and medicaid; denominator: all adap 
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # always have to include "among adap"
                                               (1-baseline.p.of.Cs.among.FP.income.medicaid) * # (1-p4)
                                               baseline.p.of.P.among.Fplus.income.medicaid * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicaid) # (1-p1)
                                           )) 

# OUTPUT 3: FPCs
# FPCs/all = (FPCs/FPx) * (FPx/Fx) * (Fx/all)
#          = p4 * p3 * (p2 * (1-p1))
#           (See FP for Fx/all explanation)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.income.medicaid.among.adap', # probability of FPCs, and a given income, and medicaid; denominator: all adap 
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # always have to include "among adap"
                                               (baseline.p.of.Cs.among.FP.income.medicaid) * # p4
                                               baseline.p.of.P.among.Fplus.income.medicaid * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicaid) # (1-p1)
                        )) 


# OUTPUT 4: PCs
# PCs/all = (PCs/Px) * (Px/all)
#         = p6 * (Px/all)
#        (Px/all) = (Px/(Px + no P))*(1 - no F)
#                 = p5 * (1-p1 - ((1-p1)*p2))
#                 first quantity: out of all the people who are not getting F, what proportion get P (p5)
#                 second quantity: everyone who is not getting F, i.e., all except:
#                       F only (p1)
#                       F+ : (1-p1)*p2 
# so, full equation: 
# (PCs/all) = p6 * p5 * (1-p1 - ((1-p1)*p2))
    # this can be simplified more but leaving as is 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.income.medicaid.among.adap',
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # always have to include "among adap"
                                               baseline.p.of.Cs.among.P.income.medicaid * # p6 
                                               baseline.p.of.P.among.no.F.income.medicaid * # p5 
                                               (1-baseline.p.of.F.only.income.medicaid - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicaid) * baseline.p.of.Fplus.among.not.F.only.income.medicaid))  # ((1-p1)*p2))
                        ))

# OUTPUT 5: P only
# (P/all) = (1-(PCs/Px)) * (Px/all)
#       (using math from PCs, just 1-p6 instead of p6 [probability of cost-share]): 
# (P/all) = (1-p6) * p5 * (1-p1 - ((1-p1)*p2))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.income.medicaid.among.adap',
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # always have to include "among adap"
                                               (1-baseline.p.of.Cs.among.P.income.medicaid) * # (1-p6) 
                                               baseline.p.of.P.among.no.F.income.medicaid * # p5 
                                               (1-baseline.p.of.F.only.income.medicaid - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicaid) * baseline.p.of.Fplus.among.not.F.only.income.medicaid))  # ((1-p1)*p2))
                        ))

# OUTPUT 6: FCs 
# (FCs/all) = (FCs/Fx)*(Fx/all)
#          = (1-p3) * (p2 * (1-p1))
#           (See FP for Fx/all explanation)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.income.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicaid.among.adap * # always have to include "among adap"
                                               (1-baseline.p.of.P.among.Fplus.income.medicaid) * # (1-p3)
                                               baseline.p.of.Fplus.among.not.F.only.income.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicaid) # (1-p1)
                        )) 

# OUTPUT 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.income.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicaid.among.adap -  
                                               (baseline.p.of.F.only.income.medicaid.among.adap + # F only 
                                                    baseline.p.of.P.only.income.medicaid.among.adap + # P only 
                                                    baseline.p.of.FP.income.medicaid.among.adap + # FP 
                                                    baseline.p.of.FCs.income.medicaid.among.adap + # FCs 
                                                    baseline.p.of.PCs.income.medicaid.among.adap + # PCs 
                                                    baseline.p.of.FPCs.income.medicaid.among.adap # FPCs 
                                               )
                        )) 




#-- Distribute for Medicare --#
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations # 

# INPUTS 
# P1 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicare', 
                        value = expression(p.F.only.min + (p.F.only.max - p.F.only.min) /
                                               (1 + exp(p.F.only.slope * (adap.incomes - p.F.only.midpoint) + 
                                                            log.OR.F.only.medicare)))) 

# P2
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Fplus.among.not.F.only.income.medicare', 
                        value = expression(p.Fplus.among.not.F.only.min + (p.Fplus.among.not.F.only.max - p.Fplus.among.not.F.only.min) /
                                               (1 + exp(p.Fplus.among.not.F.only.slope * (adap.incomes - p.Fplus.among.not.F.only.midpoint) + 
                                                            log.OR.Fplus.among.not.F.only.medicare))))

# P3
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.Fplus.income.medicare',
                        value = expression(p.P.among.Fplus.min + (p.P.among.Fplus.max - p.P.among.Fplus.min) /
                                               (1 + exp(p.P.among.Fplus.slope * (adap.incomes - p.P.among.Fplus.midpoint) + 
                                                            log.OR.P.among.Fplus.medicare))))

# P4
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.FP.income.medicare',
                        value = expression(p.Cs.among.FP.min + (p.Cs.among.FP.max - p.Cs.among.FP.min) /
                                               (1 + exp(p.Cs.among.FP.slope * (adap.incomes - p.Cs.among.FP.midpoint) + 
                                                            log.OR.Cs.among.FP.medicare))))

# P5
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.no.F.income.medicare', 
                        value = expression(p.P.among.no.F.min + (p.P.among.no.F.max - p.P.among.no.F.min) /
                                               (1 + exp(p.P.among.no.F.slope * (adap.incomes - p.P.among.no.F.midpoint) + 
                                                            log.OR.P.among.no.F.medicare))))

# P6
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.P.income.medicare',
                        value = expression(p.Cs.among.P.min + (p.Cs.among.P.max - p.Cs.among.P.min) /
                                               (1 + exp(p.Cs.among.P.slope * (adap.incomes - p.Cs.among.P.midpoint) + 
                                                            log.OR.Cs.among.P.medicare))))



# OUTPUTS 
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations 
# OUTPUT 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               baseline.p.of.F.only.income.medicare)) 

# OUTPUT 2: FP  
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.income.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               (1-baseline.p.of.Cs.among.FP.income.medicare) * # (1-p4)
                                               baseline.p.of.P.among.Fplus.income.medicare * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare * # p2
                                               (1-baseline.p.of.F.only.income.medicare) # (1-p1)
                        )) 

# OUTPUT 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.income.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               (baseline.p.of.Cs.among.FP.income.medicare) * # p4
                                               baseline.p.of.P.among.Fplus.income.medicare * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare * # p2
                                               (1-baseline.p.of.F.only.income.medicare) # (1-p1)
                        )) 

# OUTPUT 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.income.medicare.among.adap',
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               baseline.p.of.Cs.among.P.income.medicare * # p6 
                                               baseline.p.of.P.among.no.F.income.medicare * # p5 
                                               (1-baseline.p.of.F.only.income.medicare - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicare) * baseline.p.of.Fplus.among.not.F.only.income.medicare))  # ((1-p1)*p2))
                        ))

# OUTPUT 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.income.medicare.among.adap',
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               (1-baseline.p.of.Cs.among.P.income.medicare) * # (1-p6) 
                                               baseline.p.of.P.among.no.F.income.medicare * # p5 
                                               (1-baseline.p.of.F.only.income.medicare - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicare) * baseline.p.of.Fplus.among.not.F.only.income.medicare))  # ((1-p1)*p2))
                        ))

# OUTPUT 6: FCs 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.income.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.among.adap * 
                                               (1-baseline.p.of.P.among.Fplus.income.medicare) * # (1-p3)
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare * # p2
                                               (1-baseline.p.of.F.only.income.medicare) # (1-p1)
                        )) 

# OUTPUT 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.income.medicare.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.among.adap -  
                                               (baseline.p.of.F.only.income.medicare.among.adap + # F only 
                                                    baseline.p.of.P.only.income.medicare.among.adap + # P only 
                                                    baseline.p.of.FP.income.medicare.among.adap + # FP 
                                                    baseline.p.of.FCs.income.medicare.among.adap + # FCs 
                                                    baseline.p.of.PCs.income.medicare.among.adap + # PCs 
                                                    baseline.p.of.FPCs.income.medicare.among.adap # FPCs 
                                               )
                        )) 



#-- Distribute for Medicare + Medicaid --#
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations # 

# INPUTS 
# P1 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicare.and.medicaid', 
                        value = expression(p.F.only.min + (p.F.only.max - p.F.only.min) /
                                               (1 + exp(p.F.only.slope * (adap.incomes - p.F.only.midpoint) + 
                                                            log.OR.F.only.medicare.and.medicaid)))) 

# P2
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid', 
                        value = expression(p.Fplus.among.not.F.only.min + (p.Fplus.among.not.F.only.max - p.Fplus.among.not.F.only.min) /
                                               (1 + exp(p.Fplus.among.not.F.only.slope * (adap.incomes - p.Fplus.among.not.F.only.midpoint) + 
                                                            log.OR.Fplus.among.not.F.only.medicare.and.medicaid))))

# P3
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.Fplus.income.medicare.and.medicaid',
                        value = expression(p.P.among.Fplus.min + (p.P.among.Fplus.max - p.P.among.Fplus.min) /
                                               (1 + exp(p.P.among.Fplus.slope * (adap.incomes - p.P.among.Fplus.midpoint) + 
                                                            log.OR.P.among.Fplus.medicare.and.medicaid))))

# P4
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.FP.income.medicare.and.medicaid',
                        value = expression(p.Cs.among.FP.min + (p.Cs.among.FP.max - p.Cs.among.FP.min) /
                                               (1 + exp(p.Cs.among.FP.slope * (adap.incomes - p.Cs.among.FP.midpoint) + 
                                                            log.OR.Cs.among.FP.medicare.and.medicaid))))

# P5
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.no.F.income.medicare.and.medicaid', 
                        value = expression(p.P.among.no.F.min + (p.P.among.no.F.max - p.P.among.no.F.min) /
                                               (1 + exp(p.P.among.no.F.slope * (adap.incomes - p.P.among.no.F.midpoint) + 
                                                            log.OR.P.among.no.F.medicare.and.medicaid))))

# P6
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.P.income.medicare.and.medicaid',
                        value = expression(p.Cs.among.P.min + (p.Cs.among.P.max - p.Cs.among.P.min) /
                                               (1 + exp(p.Cs.among.P.slope * (adap.incomes - p.Cs.among.P.midpoint) + 
                                                            log.OR.Cs.among.P.medicare.and.medicaid))))


# OUTPUTS 
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations 
# OUTPUT 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.medicare.and.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               baseline.p.of.F.only.income.medicare.and.medicaid)) 

# OUTPUT 2: FP  
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.income.medicare.and.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               (1-baseline.p.of.Cs.among.FP.income.medicare.and.medicaid) * # (1-p4)
                                               baseline.p.of.P.among.Fplus.income.medicare.and.medicaid * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicare.and.medicaid) # (1-p1)
                        )) 

# OUTPUT 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.income.medicare.and.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               (baseline.p.of.Cs.among.FP.income.medicare.and.medicaid) * # p4
                                               baseline.p.of.P.among.Fplus.income.medicare.and.medicaid * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicare.and.medicaid) # (1-p1)
                        )) 

# OUTPUT 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.income.medicare.and.medicaid.among.adap',
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               baseline.p.of.Cs.among.P.income.medicare.and.medicaid * # p6 
                                               baseline.p.of.P.among.no.F.income.medicare.and.medicaid * # p5 
                                               (1-baseline.p.of.F.only.income.medicare.and.medicaid - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicare.and.medicaid) * baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid))  # ((1-p1)*p2))
                        ))

# OUTPUT 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.income.medicare.and.medicaid.among.adap',
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               (1-baseline.p.of.Cs.among.P.income.medicare.and.medicaid) * # (1-p6) 
                                               baseline.p.of.P.among.no.F.income.medicare.and.medicaid * # p5 
                                               (1-baseline.p.of.F.only.income.medicare.and.medicaid - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.medicare.and.medicaid) * baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid))  # ((1-p1)*p2))
                        ))

# OUTPUT 6: FCs 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.income.medicare.and.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap * 
                                               (1-baseline.p.of.P.among.Fplus.income.medicare.and.medicaid) * # (1-p3)
                                               baseline.p.of.Fplus.among.not.F.only.income.medicare.and.medicaid * # p2
                                               (1-baseline.p.of.F.only.income.medicare.and.medicaid) # (1-p1)
                        )) 

# OUTPUT 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.income.medicare.and.medicaid.among.adap', 
                        value = expression(baseline.p.of.income.with.medicare.and.medicaid.among.adap -  
                                               (baseline.p.of.F.only.income.medicare.and.medicaid.among.adap + # F only 
                                                    baseline.p.of.P.only.income.medicare.and.medicaid.among.adap + # P only 
                                                    baseline.p.of.FP.income.medicare.and.medicaid.among.adap + # FP 
                                                    baseline.p.of.FCs.income.medicare.and.medicaid.among.adap + # FCs 
                                                    baseline.p.of.PCs.income.medicare.and.medicaid.among.adap + # PCs 
                                                    baseline.p.of.FPCs.income.medicare.and.medicaid.among.adap # FPCs 
                                               )
                        )) 



#-- Distribute for Private Insurance --#
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations # 

# INPUTS 
# P1 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.private', 
                        value = expression(p.F.only.min + (p.F.only.max - p.F.only.min) /
                                               (1 + exp(p.F.only.slope * (adap.incomes - p.F.only.midpoint) + 
                                                            log.OR.F.only.private)))) 

# P2
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Fplus.among.not.F.only.income.private', 
                        value = expression(p.Fplus.among.not.F.only.min + (p.Fplus.among.not.F.only.max - p.Fplus.among.not.F.only.min) /
                                               (1 + exp(p.Fplus.among.not.F.only.slope * (adap.incomes - p.Fplus.among.not.F.only.midpoint) + 
                                                            log.OR.Fplus.among.not.F.only.private))))

# P3
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.Fplus.income.private',
                        value = expression(p.P.among.Fplus.min + (p.P.among.Fplus.max - p.P.among.Fplus.min) /
                                               (1 + exp(p.P.among.Fplus.slope * (adap.incomes - p.P.among.Fplus.midpoint) + 
                                                            log.OR.P.among.Fplus.private))))

# P4
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.FP.income.private',
                        value = expression(p.Cs.among.FP.min + (p.Cs.among.FP.max - p.Cs.among.FP.min) /
                                               (1 + exp(p.Cs.among.FP.slope * (adap.incomes - p.Cs.among.FP.midpoint) + 
                                                            log.OR.Cs.among.FP.private))))

# P5
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.among.no.F.income.private', 
                        value = expression(p.P.among.no.F.min + (p.P.among.no.F.max - p.P.among.no.F.min) /
                                               (1 + exp(p.P.among.no.F.slope * (adap.incomes - p.P.among.no.F.midpoint) + 
                                                            log.OR.P.among.no.F.private))))

# P6
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.among.P.income.private',
                        value = expression(p.Cs.among.P.min + (p.Cs.among.P.max - p.Cs.among.P.min) /
                                               (1 + exp(p.Cs.among.P.slope * (adap.incomes - p.Cs.among.P.midpoint) + 
                                                            log.OR.Cs.among.P.private))))


# OUTPUTS 
# COMPLETELY ANALOGOUS TO MEDICAID - see that section for explanations 
# OUTPUT 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.private.among.adap', 
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               baseline.p.of.F.only.income.private)) 

# OUTPUT 2: FP  
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.income.private.among.adap', 
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               (1-baseline.p.of.Cs.among.FP.income.private) * # (1-p4)
                                               baseline.p.of.P.among.Fplus.income.private * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.private * # p2
                                               (1-baseline.p.of.F.only.income.private) # (1-p1)
                        )) 

# OUTPUT 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.income.private.among.adap', 
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               (baseline.p.of.Cs.among.FP.income.private) * # p4
                                               baseline.p.of.P.among.Fplus.income.private * # p3
                                               baseline.p.of.Fplus.among.not.F.only.income.private * # p2
                                               (1-baseline.p.of.F.only.income.private) # (1-p1)
                        )) 

# OUTPUT 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.income.private.among.adap',
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               baseline.p.of.Cs.among.P.income.private * # p6 
                                               baseline.p.of.P.among.no.F.income.private * # p5 
                                               (1-baseline.p.of.F.only.income.private - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.private) * baseline.p.of.Fplus.among.not.F.only.income.private))  # ((1-p1)*p2))
                        ))

# OUTPUT 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.income.private.among.adap',
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               (1-baseline.p.of.Cs.among.P.income.private) * # (1-p6) 
                                               baseline.p.of.P.among.no.F.income.private * # p5 
                                               (1-baseline.p.of.F.only.income.private - # (1-p1 -
                                                    ((1-baseline.p.of.F.only.income.private) * baseline.p.of.Fplus.among.not.F.only.income.private))  # ((1-p1)*p2))
                        ))

# OUTPUT 6: FCs 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.income.private.among.adap', 
                        value = expression(baseline.p.of.income.private.among.adap * 
                                               (1-baseline.p.of.P.among.Fplus.income.private) * # (1-p3)
                                               baseline.p.of.Fplus.among.not.F.only.income.private * # p2
                                               (1-baseline.p.of.F.only.income.private) # (1-p1)
                        )) 

# OUTPUT 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.income.private.among.adap', 
                        value = expression(baseline.p.of.income.private.among.adap -  
                                               (baseline.p.of.F.only.income.private.among.adap + # F only 
                                                    baseline.p.of.P.only.income.private.among.adap + # P only 
                                                    baseline.p.of.FP.income.private.among.adap + # FP 
                                                    baseline.p.of.FCs.income.private.among.adap + # FCs 
                                                    baseline.p.of.PCs.income.private.among.adap + # PCs 
                                                    baseline.p.of.FPCs.income.private.among.adap # FPCs 
                                               )
                        )) 



#-- Distribute for Uninsured --#
# Everyone who's uninsured must be on full-pay (can't have premium assistance or cost-sharing if you're uninsured!)

# OUTPUT 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.income.uninsured.among.adap', 
                        value = 'baseline.p.of.income.uninsured.among.adap') 

# OUTPUT 2: FP  
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.income.uninsured.among.adap', 
                        value = 0) 

# OUTPUT 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.income.uninsured.among.adap', 
                        value = 0) 

# OUTPUT 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.income.uninsured.among.adap',
                        value = 0)

# OUTPUT 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.income.uninsured.among.adap',
                        value = 0)

# OUTPUT 6: FCs 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.income.uninsured.among.adap', 
                        value = 0) 

# OUTPUT 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.income.uninsured.among.adap', 
                        value = 0) 



# Sum over insurance types to get proportion in each service category by income, among ADAP 
# 1: F only
# 2: FP  
# 3: FPCs
# 4: PCs
# 5: P only
# 6: FCs 
# 7: Cs only 

# 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.F.only.income.among.adap', 
                       value = expression(baseline.p.of.F.only.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid (proportion F only Medicaid * proportion Medicaid)
                                              baseline.p.of.F.only.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.F.only.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.F.only.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.F.only.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 2: FP 
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.FP.income.among.adap', 
                       value = expression(baseline.p.of.FP.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.FP.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.FP.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.FP.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.FP.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.FPCs.income.among.adap', 
                       value = expression(baseline.p.of.FPCs.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.FPCs.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.FPCs.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.FPCs.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.FPCs.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.PCs.income.among.adap', 
                       value = expression(baseline.p.of.PCs.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.PCs.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.PCs.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.PCs.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.PCs.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.P.only.income.among.adap', 
                       value = expression(baseline.p.of.P.only.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.P.only.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.P.only.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.P.only.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.P.only.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 6: FCs
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.FCs.income.among.adap', 
                       value = expression(baseline.p.of.FCs.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.FCs.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.FCs.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.FCs.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.FCs.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))

# 7: Cs only
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'baseline.p.of.Cs.only.income.among.adap', 
                       value = expression(baseline.p.of.Cs.only.income.medicaid.among.adap*baseline.p.of.income.with.medicaid.among.adap + # Medicaid
                                              baseline.p.of.Cs.only.income.medicare.among.adap*baseline.p.of.income.with.medicare.among.adap + # Medicare
                                              baseline.p.of.Cs.only.income.medicare.and.medicaid.among.adap*baseline.p.of.income.with.medicare.and.medicaid.among.adap + # Medicare + Medicaid 
                                              baseline.p.of.Cs.only.income.private.among.adap*baseline.p.of.income.private.among.adap + # Private
                                              baseline.p.of.Cs.only.income.uninsured.among.adap*baseline.p.of.income.uninsured.among.adap # Uninsured
                       ))



##--------------------------------------------------------------##
##-- CALCULATED: DISTRIBUTE P ON ADAP INTO SERVICE CATEGORIES --##
##   Sum over income to get proportion in each service category ##
##--------------------------------------------------------------##

# Categories: F only, FP, FPCs, PCs, P only, FCs, Cs only 

# 1: F only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.F.only.among.adap', 
                        value = calculate.baseline.p.of.F.only.among.adap)

# 2: FP  
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FP.among.adap', 
                        value = calculate.baseline.p.of.FP.among.adap)

# 3: FPCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FPCs.among.adap',
                        value = calculate.baseline.p.of.FPCs.among.adap)

# 4: PCs
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.PCs.among.adap', 
                        value = calculate.baseline.p.of.PCs.among.adap)

# 5: P only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.P.only.among.adap', 
                        value = calculate.baseline.p.of.P.only.among.adap)

# 6: FCs 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.FCs.among.adap',
                        value = calculate.baseline.p.of.FCs.among.adap)

# 7: Cs only 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.Cs.only.among.adap', 
                        value = calculate.baseline.p.of.Cs.only.among.adap)


##---------------------------------------------------------------------##
##-- CALCULATED: PROPORTION ADAP CLIENTS WHO LOSE or CHANGE SERVICES --##
##---------------------------------------------------------------------##

# Ways people can change based on income threshold: 

# 1: F only
#   1a: Lose
#   1b: Keep 

# 2: FP
#   2a: Lose both
#   2b: Lose premium (F only)
#   2c: Lose full-pay (P only)
#   2d: Keep both 

# 3: FPCs
#   3a: Lose all
#   3b: Lose F (--> PCs)
#   3c: Lose Cs (--> FP)
#   3d: Lose PCs (--> F)
#   3e: Lose FCs (--> P)
#   3f: Keep all 
#   No way to lose premium only (and keep cost-sharing); if you lose premium, presumably lost insurance so lose cost-sharing as well 

# 4: PCs
#   4a: Lose both 
#   4b: Lose Cs (--> P)
#   4c: Lose both, but gain full-pay (above premium but below full-pay )
#   4d: Keep both 

# 5: P only
#   5a: Lose
#   5b: Lose, but gain full-pay (above premium but below full-pay)
#   5c: Keep 

# 6: FCs 
#   6a: Lose both 
#   6b: Lose F (--> Cs) 
#   6c: Lose Cs (--> F)
#   6d: Keep both 

# 7: Cs only 
#   7a: Lose 
#   7b: Keep 

# Whatever they lose --> lower threshold; keep --> upper threshold

#-- 1: F only --#
# 1a: Lose
calculate.proportion.F.only.lose.F <- function(baseline.p.of.F.only.income.among.adap, 
                                               adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.F.only.income.among.adap, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = Inf)
    
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.F.only.lose.F', 
                        value = calculate.proportion.F.only.lose.F)

# 1b: Keep (don't need to calculate; will be (1 - lose)) 


#-- 2: FP --#
# 2a: Lose both
calculate.proportion.FP.lose.FP <- function(baseline.p.of.FP.income.among.adap, 
                                            adap.full.pay.fpl.threshold,
                                            adap.premium.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FP.income.among.adap, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold,adap.premium.fpl.threshold),
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FP.lose.FP', 
                        value = calculate.proportion.FP.lose.FP)

# 2b: Lose premium (F only)
calculate.proportion.FP.lose.P <- function(baseline.p.of.FP.income.among.adap, 
                                           adap.full.pay.fpl.threshold,
                                           adap.premium.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FP.income.among.adap, 
                                   lower.threshold = adap.premium.fpl.threshold,
                                   upper.threshold = adap.full.pay.fpl.threshold) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FP.lose.P', 
                        value = calculate.proportion.FP.lose.P)


# 2c: Lose full-pay (P only)
calculate.proportion.FP.lose.F <- function(baseline.p.of.FP.income.among.adap, 
                                            adap.full.pay.fpl.threshold,
                                            adap.premium.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FP.income.among.adap, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = adap.premium.fpl.threshold) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FP.lose.F', 
                        value = calculate.proportion.FP.lose.F)

# 2d: Keep both 
# (1- the rest)


#-- 3: FPCs --#
# 3a: Lose all
calculate.proportion.FPCs.lose.FPCs <- function(baseline.p.of.FPCs.income.among.adap, 
                                           adap.full.pay.fpl.threshold,
                                           adap.premium.fpl.threshold,
                                           adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FPCs.income.among.adap, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold, max(adap.premium.fpl.threshold, adap.cost.sharing.fpl.threshold)),
                                   upper.threshold = Inf) 
}

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FPCs.lose.FPCs', 
                        value = calculate.proportion.FPCs.lose.FPCs)

# 3b: Lose F (--> PCs)
calculate.proportion.FPCs.lose.F <- function(baseline.p.of.FPCs.income.among.adap, 
                                                adap.full.pay.fpl.threshold,
                                             adap.premium.fpl.threshold,
                                             adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FPCs.income.among.adap, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = min(adap.premium.fpl.threshold,adap.cost.sharing.fpl.threshold)) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FPCs.lose.F', 
                        value = calculate.proportion.FPCs.lose.F)

# 3c: Lose Cs (--> FP)
calculate.proportion.FPCs.lose.Cs <- function(baseline.p.of.FPCs.income.among.adap, 
                                              adap.cost.sharing.fpl.threshold,
                                              adap.premium.fpl.threshold,
                                              adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FPCs.income.among.adap, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold,
                                   upper.threshold = min(adap.premium.fpl.threshold,adap.full.pay.fpl.threshold)) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FPCs.lose.Cs', 
                        value = calculate.proportion.FPCs.lose.Cs)

# 3d: Lose PCs (--> F)
calculate.proportion.FPCs.lose.PCs <- function(baseline.p.of.FPCs.income.among.adap, 
                                             adap.full.pay.fpl.threshold,
                                             adap.premium.fpl.threshold,
                                             adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FPCs.income.among.adap, 
                                   lower.threshold = adap.premium.fpl.threshold, # just premium here because you can't lose only premium (if they are above premium but below cost-sharing, they'll lose cost-sharing)
                                   upper.threshold = adap.full.pay.fpl.threshold) # but below full-pay threshold (keep it)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FPCs.lose.PCs', 
                        value = calculate.proportion.FPCs.lose.PCs)


# 3e: Lose FCs (--> P)
calculate.proportion.FPCs.lose.FCs <- function(baseline.p.of.FPCs.income.among.adap, 
                                               adap.full.pay.fpl.threshold,
                                               adap.premium.fpl.threshold,
                                               adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FPCs.income.among.adap, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold,adap.cost.sharing.fpl.threshold), 
                                   upper.threshold = adap.premium.fpl.threshold) 
}

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FPCs.lose.FCs', 
                        value = calculate.proportion.FPCs.lose.FCs)


# 3f: Keep all 
# (1- the rest)


#-- 4: PCs --#
# 4a: Lose both 
calculate.proportion.PCs.lose.PCs <- function(baseline.p.of.PCs.income.among.adap, 
                                              adap.premium.fpl.threshold,
                                              adap.cost.sharing.fpl.threshold,
                                              adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.PCs.income.among.adap, 
                                   lower.threshold = max(adap.premium.fpl.threshold,adap.full.pay.fpl.threshold), # no Cs here because you can't lose only premium (if above premium but below cost-sharing, they'll lose cost-sharing)
                                                        # also, full-pay included because you have to be above both to fully lose coverage (if you are in between, as in 4c, switch to full-pay)
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.PCs.lose.PCs', 
                        value = calculate.proportion.PCs.lose.PCs)

# 4b: Lose Cs (--> P)
calculate.proportion.PCs.lose.Cs <- function(baseline.p.of.PCs.income.among.adap, 
                                              adap.premium.fpl.threshold,
                                              adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.PCs.income.among.adap, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold,
                                   upper.threshold = adap.premium.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.PCs.lose.Cs', 
                        value = calculate.proportion.PCs.lose.Cs)

# 4c: Lose both, but gain full-pay 
calculate.proportion.PCs.lose.PCs.gain.F <- function(baseline.p.of.PCs.income.among.adap, 
                                              adap.premium.fpl.threshold,
                                              adap.cost.sharing.fpl.threshold,
                                              adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.PCs.income.among.adap, 
                                   lower.threshold = adap.premium.fpl.threshold, # no Cs here because you can't lose only premium (if above premium but below cost-sharing, they'll lose cost-sharing)
                                   upper.threshold = adap.full.pay.fpl.threshold) # if you are below full-pay threshold, gain full-pay 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.PCs.lose.PCs.gain.F', 
                        value = calculate.proportion.PCs.lose.PCs.gain.F)

# 4d: Keep both 
# (1- the rest)


#-- 5: P only --#
# 5a: Lose
calculate.proportion.P.only.lose.P <- function(baseline.p.of.P.only.income.among.adap, 
                                               adap.premium.fpl.threshold,
                                               adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.P.only.income.among.adap, 
                                   lower.threshold = max(adap.premium.fpl.threshold, adap.full.pay.fpl.threshold), # because you have to be above both to fully lose it (if you are in between, as in 5b, switch to F)
                                   upper.threshold = Inf)
    
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.P.only.lose.P', 
                        value = calculate.proportion.P.only.lose.P)

# 5b: Lose but gain full-pay 
calculate.proportion.P.only.lose.P.gain.F <- function(baseline.p.of.P.only.income.among.adap, 
                                                      adap.premium.fpl.threshold,
                                                      adap.full.pay.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.P.only.income.among.adap, 
                                   lower.threshold = adap.premium.fpl.threshold, 
                                   upper.threshold = adap.full.pay.fpl.threshold) # if you are below full-pay threshold, gain full-pay 
    
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.P.only.lose.P.gain.F', 
                        value = calculate.proportion.P.only.lose.P.gain.F)


# 5c: Keep 
# (1- the rest)


#-- 6: FCs --#
# 6a: Lose both 
calculate.proportion.FCs.lose.FCs <- function(baseline.p.of.FCs.income.among.adap, 
                                              adap.full.pay.fpl.threshold,
                                              adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FCs.income.among.adap, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold,adap.cost.sharing.fpl.threshold), 
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FCs.lose.FCs', 
                        value = calculate.proportion.FCs.lose.FCs)

# 6b: Lose F (--> Cs) 
calculate.proportion.FCs.lose.F <- function(baseline.p.of.FCs.income.among.adap, 
                                              adap.full.pay.fpl.threshold,
                                              adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FCs.income.among.adap, 
                                   lower.threshold = adap.full.pay.fpl.threshold, 
                                   upper.threshold = adap.cost.sharing.fpl.threshold) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FCs.lose.F', 
                        value = calculate.proportion.FCs.lose.F)


# 6c: Lose Cs (--> F)
calculate.proportion.FCs.lose.Cs <- function(baseline.p.of.FCs.income.among.adap, 
                                            adap.full.pay.fpl.threshold,
                                            adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.FCs.income.among.adap, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold, 
                                   upper.threshold = adap.full.pay.fpl.threshold) 
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.FCs.lose.Cs', 
                        value = calculate.proportion.FCs.lose.Cs)

# 6d: Keep both 
# (1- the rest)


#-- 7: Cs only --#
# 7a: Lose 
calculate.proportion.Cs.only.lose.Cs <- function(baseline.p.of.Cs.only.income.among.adap, 
                                                 adap.cost.sharing.fpl.threshold)
{
    calculate.p.between.thresholds(income.distribution = baseline.p.of.Cs.only.income.among.adap, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold, 
                                   upper.threshold = Inf)
    
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.Cs.only.lose.Cs', 
                        value = calculate.proportion.Cs.only.lose.Cs)

# 7b: Keep 
# (1 - lose)



##-----------------------------##
##-- CALCULATED: SUPPRESSION --##
##-----------------------------##

# Ways people can change (or not change): 

# ADAP Unchanged: 
# F
# P
# PCs
# Cs

# Lose ADAP: 
# lose F 
# lose P
# lose PCs
# lose Cs 

# Change ADAP: 
# P to F
# PCs to F 
# PCs to P 
# There is no world where you can go from full pay to premiums only (because ADAP is last resort; if you could have been getting premium support before, never should have been getting full-pay)
# Can't lose just premium (so can't go from PCs to Cs) - if you lose your insurance, lose both

# No world you can change Cs to F; if you have Cs alone, you have insurance; if you have insurance, you shouldn't get full pay 

# Formulary change: 
# keep F, change formulary 
# keep PCp, change formulary 
# keep Cp, change formulary 
# P to F, change formulary 
# PCs to F, change formulary 
# Only full pay and Cp are affected by formulary changes (i.e., premium support and deductible assistance aren't impacted by formularly restrictions)


# This is for a single point in time (i.e., can't have full pay and premium at the same time)
# These are all proportions WHO ARE SUPPRESSED (up to now, we've calculated proportions who have the services - will have to multiply in suppression among each group)

##-- TIE-IN TO MAIN SUPPRESSION --##
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'suppression.of.diagnosed',
                        value = expression(
                                # Never had ADAP
                                proportion.pwh.who.are.suppressed.without.adap +
                                
                                # ADAP Unchanged
                                proportion.pwh.who.are.suppressed.F.unchanged + 
                                proportion.pwh.who.are.suppressed.P.unchanged + 
                                proportion.pwh.who.are.suppressed.PCs.unchanged + 
                                proportion.pwh.who.are.suppressed.Cs.unchanged + 
                                
                                # Lose ADAP
                                proportion.pwh.who.are.suppressed.lose.F * lose.F.suppression.rr + 
                                proportion.pwh.who.are.suppressed.lose.P * lose.P.suppression.rr + 
                                proportion.pwh.who.are.suppressed.lose.PCs * lose.PCs.suppression.rr + 
                                proportion.pwh.who.are.suppressed.lose.Cs * lose.Cs.suppression.rr + 

                                # Change ADAP Service
                                proportion.pwh.who.are.suppressed.change.P.to.F * change.P.to.F.suppression.rr + 
                                proportion.pwh.who.are.suppressed.change.PCs.to.F * change.PCs.to.F.suppression.rr + 
                                proportion.pwh.who.are.suppressed.change.PCs.to.P * change.PCs.to.P.suppression.rr + 
                                                                    
                                # Formulary Change
                                proportion.pwh.who.are.suppressed.F.change.formulary * change.F.formulary.suppression.rr + 
                                proportion.pwh.who.are.suppressed.PCp.change.formulary * change.PCp.formulary.suppression.rr + 
                                proportion.pwh.who.are.suppressed.Cp.change.formulary * change.Cp.formulary.suppression.rr + 
                                
                                proportion.pwh.who.are.suppressed.change.P.to.F.change.formulary * change.F.formulary.suppression.rr + # using the same RR as above: practically, these formulary suppression RRs are probably all going to be the same 
                                proportion.pwh.who.are.suppressed.change.PCs.to.F.change.formulary * change.F.formulary.suppression.rr 
                                )
)

# Each section does two things: (1) folding in suppression; (2) parsing out from "over the course of the year," to "at a single point in time" (e.g., from FP to F vs P)

#-- Suppression Among Full Pay (F) --#
# 3 things they can do: 
# F1: Lose F
# F2: Keep F with formulary change
# F3: Keep F without formulary change

# F1: Lose F (from F only, from FP, from FPCs, or from FCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.lose.F',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (
                                                   # over the course of the year, only ever had F 
                                                   proportion.of.adap.who.are.suppressed.F.only * 
                                                       proportion.F.only.lose.F +
                                                       
                                                       # over the course of the year, they have FP, but at this point in time, how many have F and are suppressed
                                                       proportion.of.adap.who.are.suppressed.F.among.FP * 
                                                       proportion.FP.lose.F +
                                                       
                                                       # over the course of the year, they have FPCs, but at this point in time, how many have F and are suppressed
                                                       proportion.of.adap.who.are.suppressed.F.among.FPCs * 
                                                       proportion.FPCs.lose.F  *
                                                       
                                                       # over the course of the year, they have FCs, but at this point in time, how many have F and are suppressed
                                                       proportion.of.adap.who.are.suppressed.F.among.FCs * 
                                                       proportion.FCs.lose.F
                                               ))
)

# intermediary step
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.keep.F', 
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.F.only *
                                                    (1-proportion.F.only.lose.F) +
                                                proportion.of.adap.who.are.suppressed.F.among.FP *
                                                    (1-proportion.FP.lose.F) +
                                                proportion.of.adap.who.are.suppressed.F.among.FPCs *
                                                    (1-proportion.FPCs.lose.F) *
                                                proportion.of.adap.who.are.suppressed.F.among.FCs *
                                                    (1-proportion.FCs.lose.F)
                                               ))
)

# F2: Keep F with formulary change 
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'proportion.pwh.who.are.suppressed.F.change.formulary',
                       value = expression(proportion.pwh.who.are.suppressed.keep.F * 
                                              proportion.F.clients.with.formulary.change)
)

# F3: Keep F without formulary change
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.F.unchanged', 
                        value = expression(proportion.pwh.who.are.suppressed.keep.F * 
                                               (1-proportion.F.clients.with.formulary.change))
)


# Full-pay (F) suppression components
# F only suppression 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.F.only',
                        value = expression(baseline.p.of.F.only.among.adap * 
                                               fraction.time.covered.among.F * 
                                               proportion.F.only.suppressed
                        ))

# F among FP suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.F.among.FP',
                        value = expression(baseline.p.of.FP.among.adap *
                                               fraction.time.F.among.FP *
                                               proportion.FP.suppressed
                        ))

# F among FPCs suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.F.among.FPCs',
                        value = expression(baseline.p.of.FPCs.among.adap * 
                                               fraction.time.F.among.FPCs *
                                               proportion.FPCs.suppressed
                        ))

# F among FCs suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.F.among.FCs',
                        value = expression(baseline.p.of.FCs.among.adap * 
                                               fraction.time.F.among.FCs *
                                               proportion.FCs.suppressed
                        ))



#-- Suppression Among Premium (P) --#
# 4 things they can do: 
# P1: Lose P
# P2: Change to F with formulary change
# P3: Change to F without formulary change
# P4: Keep P 

# P1: Lose P (either from P only or from FP)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.lose.P',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (
                                                   # over the course of the year, only ever had P
                                                   proportion.of.adap.who.are.suppressed.P.only * 
                                                       proportion.P.only.lose.P +
                                                       
                                                       # over the course of the year, they have FP, but at this point in time, how many have P and are suppressed 
                                                       # (implicitly lose F - if they didn't lose F, they'd switch to F)
                                                       proportion.of.adap.who.are.suppressed.P.among.FP *
                                                       proportion.FP.lose.FP 
                                               ))
)

# P2: Change P to F, WITH formulary change (either from P only or from FP)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.change.P.to.F.change.formulary', 
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.P.only * # P only: lose P, gain F, formulary change 
                                                    proportion.P.only.lose.P.gain.F *
                                                    proportion.change.to.F.with.formulary.change +
                                                    
                                                    proportion.of.adap.who.are.suppressed.P.among.FP * # P among FP: lose P, formulary change 
                                                    proportion.FP.lose.P *
                                                    proportion.change.to.F.with.formulary.change
                                               ))
)

# P3: Change P to F, WITHOUT formulary change (either from P only or from FP)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.change.P.to.F', # (no formulary change)
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.P.only * # P only: lose P, gain F
                                                    proportion.P.only.lose.P.gain.F *
                                                    (1-proportion.change.to.F.with.formulary.change) +
                                                    
                                                proportion.of.adap.who.are.suppressed.P.among.FP * # P among FP: lose P 
                                                    proportion.FP.lose.P * 
                                                    (1-proportion.change.to.F.with.formulary.change)
                                               ))
)

# P4: Keep P 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.P.unchanged',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.P.only * # P only: unchanged 
                                                    (1-proportion.P.only.lose.P - 
                                                         proportion.P.only.lose.P.gain.F) +
                                                    
                                                proportion.of.adap.who.are.suppressed.P.among.FP * # P among FP: unchanged 
                                                    (1-proportion.FP.lose.P - 
                                                         proportion.FP.lose.FP)
                                               ))
)



# Premium (P) suppression components
# P only suppression 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.P.only',
                        value = expression(baseline.p.of.P.only.among.adap * 
                                               fraction.time.covered.among.P *
                                               proportion.P.only.suppressed
                        ))

# P among FP suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.P.among.FP',
                        value = expression(baseline.p.of.FP.among.adap *
                                               fraction.time.P.among.FP *
                                               proportion.FP.suppressed
                        ))




#-- Suppression Among Premium AND Cost-Sharing (PCs) --#
# 6 things they can do: 
# PCs1: Lose PCs
# PCs2: Change to F with formulary change
# PCs3: Change to F without formulary change
# PCs4: Lose Cs 
# PCs5: Keep PCs with formulary change (only applies to Cp)
# PCs6: Keep PCs without formulary change  (only applies to Cp)

# PCs1: Lose PCs (either from PCs or from FPCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.lose.PCs',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               
                                               # over the course of the year, only ever had PCs
                                               (proportion.of.adap.who.are.suppressed.PCs * 
                                                    proportion.PCs.lose.PCs +
                                               
                                                    # over the course of the year, they have FPCs, but at this point in time, how many have PCs and are suppressed 
                                                    # (implicitly lose F - if they didn't lose F, they'd switch to F)
                                                    proportion.of.adap.who.are.suppressed.PCs.among.FPCs * 
                                                    proportion.FPCs.lose.FPCs 
                                               ))
)

# PCs2: Change to F with formulary change (either from PCs or from FPCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.change.PCs.to.F.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.PCs * # PCs: lose PCs, gain F, formulary change
                                                    proportion.PCs.lose.PCs.gain.F *
                                                    proportion.change.to.F.with.formulary.change +
                                                    
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: lose PCs, formulary change 
                                                    proportion.FPCs.lose.PCs * 
                                                    proportion.change.to.F.with.formulary.change
                                               ))
)

# PCs3: Change to F without formulary change (either from PCs or from FPCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.change.PCs.to.F',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.PCs * # PCs: lose PCs, gain F
                                                    proportion.PCs.lose.PCs.gain.F *
                                                    (1 - proportion.change.to.F.with.formulary.change) +
                                                    
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: lose PCs
                                                    proportion.FPCs.lose.PCs * 
                                                    (1 - proportion.change.to.F.with.formulary.change)
                                               ))
)

# PCs4: Lose Cs (PCs --> P, FPCs --> FP, FPCs --> P)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.change.PCs.to.P',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.PCs * # PCs: lose Cs
                                                    proportion.PCs.lose.Cs +
                                                    
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: lose Cs 
                                                    proportion.FPCs.lose.Cs + 
                                                
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: lose FCs 
                                                    proportion.FPCs.lose.FCs
                                               ))
)

# PCs5: Keep PCs with formulary change; only applies to Cp (either from PCs or from FPCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.PCp.change.formulary', # formulary only impacts if they're cost-sharing is specifically copay
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.PCs * # PCs: keep PCs, formulary 
                                                    (1 - proportion.PCs.lose.PCs -
                                                         proportion.PCs.lose.PCs.gain.F -
                                                         proportion.PCs.lose.Cs) *
                                                    proportion.PCs.clients.with.Cp * # only applies to copay - need to multiply in this proportion 
                                                    proportion.PCp.clients.with.formulary.change +
                                                    
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: keep PCs, formulary 
                                                    (1 - proportion.FPCs.lose.FPCs  - 
                                                         proportion.FPCs.lose.PCs -
                                                         proportion.FPCs.lose.FCs - 
                                                         proportion.FPCs.lose.Cs) *
                                                    proportion.FPCs.clients.with.Cp *
                                                    proportion.PCp.clients.with.formulary.change
                                               ))
)
# PCs6: Keep PCs without formulary change; only applies to Cp (either from PCs or from FPCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.PCs.unchanged',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.PCs * # PCs: keep PCs (unchanged) 
                                                    (1 - proportion.PCs.lose.PCs -
                                                         proportion.PCs.lose.PCs.gain.F -
                                                         proportion.PCs.lose.Cs) *
                                                    (1 - proportion.PCs.clients.with.Cp * proportion.PCp.clients.with.formulary.change) +
                                                    
                                                proportion.of.adap.who.are.suppressed.PCs.among.FPCs * # PCs among FPCs: keep PCs (unchanged) 
                                                    (1 - proportion.FPCs.lose.FPCs  - 
                                                         proportion.FPCs.lose.PCs - 
                                                         proportion.FPCs.lose.FCs- 
                                                         proportion.FPCs.lose.Cs) *
                                                    (1 - proportion.FPCs.clients.with.Cp * proportion.PCp.clients.with.formulary.change)
                                               ))
)



# Premium and cost-sharing (PCs) suppression components
# PCs suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.PCs',
                        value = expression(baseline.p.of.PCs.among.adap * 
                                               fraction.time.covered.among.PCs *
                                               proportion.PCs.suppressed
                        ))

# PCs among FPCs suppression
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.PCs.among.FPCs',
                        value = expression(baseline.p.of.FPCs.among.adap *
                                               fraction.time.PCs.among.FPCs *
                                               proportion.FPCs.suppressed
                        ))



#-- Suppression Among Cost-Sharing (Cs) --#
# 3 things they can do: 
# Cs1: Lose Cs
# Cs2: Keep Cs with formulary change
# Cs3: Keep Cs without formulary change

# Cs1: Lose Cs (either from Cs only or from FCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.lose.Cs',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               
                                               # over the course of the year, only ever had Cs 
                                               (proportion.of.adap.who.are.suppressed.Cs.only *
                                                    proportion.Cs.only.lose.Cs +
                                                    
                                                    # over the course of the year, they had FCs, but at this point in time, how many have Cs and are suppressed
                                                    
                                                    proportion.of.adap.who.are.suppressed.Cs.among.FCs *
                                                    (proportion.FCs.lose.FCs + # implicitly lose F 
                                                         proportion.FCs.lose.Cs)  # truly only lost Cs - special case for Cs because you can't switch to F from Cs 
                                               ))
)

# Cs2: Keep Cs with formulary change (either from Cs or from FCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.Cp.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.Cs.only * # Cs only: formulary 
                                                    (1-proportion.Cs.only.lose.Cs) *
                                                    proportion.Cs.clients.with.Cp * # among co-pay only 
                                                    proportion.Cp.clients.with.formulary.change +
                                                    
                                                proportion.of.adap.who.are.suppressed.Cs.among.FCs * # Cs among FCs: formulary 
                                                    (1-proportion.FCs.lose.FCs - # implicitly lose F 
                                                         proportion.FCs.lose.Cs) * # truly only lost Cs - special case for Cs because you can't switch to F from Cs 
                                                    proportion.FCs.clients.with.Cp * # among co-pay only 
                                                    proportion.Cp.clients.with.formulary.change # for now, assuming that the proportion with formulary change is the same for all Cp (not factoring in the F component here)
                                               ))
)

#Cs3: Keep Cs without formulary change (either from Cs or from FCs)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.Cs.unchanged', 
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.Cs.only * # Cs only
                                                    (1-proportion.Cs.only.lose.Cs) *
                                                    (1 - proportion.Cs.clients.with.Cp * 
                                                         proportion.Cp.clients.with.formulary.change) +
                                                    
                                                proportion.of.adap.who.are.suppressed.Cs.among.FCs * # Cs among FCs
                                                    (1-proportion.FCs.lose.FCs - # implicitly lose F 
                                                         proportion.FCs.lose.Cs) * # truly only lost Cs - special case for Cs because you can't switch to F from Cs 
                                                    (1 - proportion.FCs.clients.with.Cp * 
                                                         proportion.Cp.clients.with.formulary.change) # for now, assuming that the proportion with formulary change is the same for all Cp (not factoring in the F component here)
                                               ))
)


# Cost-sharing (Cs) suppression components
# Cs suppression 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.Cs.only',
                        value = expression(baseline.p.of.Cs.only.among.adap * 
                                               fraction.time.covered.among.Cs *
                                               proportion.Cs.only.suppressed
                        ))

# Cs among FCs suppression 
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.Cs.among.FCs',
                        value = expression(baseline.p.of.FCs.among.adap *
                                               fraction.time.Cs.among.FCs *
                                               proportion.FCs.suppressed
                        ))


#-- Suppression without any ADAP --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.without.adap',
                        value = expression(
                            super.suppression.of.diagnosed - 
                                
                                # ADAP Unchanged
                                proportion.pwh.who.are.suppressed.F.unchanged -
                                proportion.pwh.who.are.suppressed.P.unchanged -
                                proportion.pwh.who.are.suppressed.PCs.unchanged -
                                proportion.pwh.who.are.suppressed.Cs.unchanged -
                                
                                # Lose ADAP
                                proportion.pwh.who.are.suppressed.lose.F -
                                proportion.pwh.who.are.suppressed.lose.P -
                                proportion.pwh.who.are.suppressed.lose.PCs - 
                                proportion.pwh.who.are.suppressed.lose.Cs -
                                
                                # Change ADAP
                                proportion.pwh.who.are.suppressed.change.P.to.F -
                                proportion.pwh.who.are.suppressed.change.PCs.to.F -
                                proportion.pwh.who.are.suppressed.change.PCs.to.P -
                                
                                # Formulary Change
                                proportion.pwh.who.are.suppressed.F.change.formulary -
                                proportion.pwh.who.are.suppressed.PCp.change.formulary - 
                                proportion.pwh.who.are.suppressed.Cp.change.formulary -
                                
                                proportion.pwh.who.are.suppressed.change.P.to.F.change.formulary -
                                proportion.pwh.who.are.suppressed.change.PCs.to.F.change.formulary
                            
                        ))






##--------------##
##-- REGISTER --##
##--------------##

register.model.specification(ADAP.SPECIFICATION)
# register.set.parameters.for.version('rw',
#                                     parameter.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
#                                     apply.function = ryan.white.apply.set.parameters,
#                                     join.with.previous.version = T)
