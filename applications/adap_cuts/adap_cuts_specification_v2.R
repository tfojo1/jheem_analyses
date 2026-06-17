source('../jheem_analyses/applications/EHE/ehe_specification.R')


INSURANCE.CATEGORIES = c('medicaid.only',
                         'medicare.only',
                         'dual.medicare.medicaid.only',
                         'private.only',
                         'uninsured.only',
                         'medicaid.and.uninsured',
                         'medicare.and.uninsured',
                         'dual.medicare.medicaid.and.uninsured',
                         'private.and.uninsured',
                         'dual.medicare.medicaid.and.medicaid',
                         'dual.medicare.medicaid.and.medicare'
                         )
ADAP.SERVICE.CATEGORIES = c('full.pay.only', # F 
                            'premium.only', # P renamed from: premium.without.cost.sharing
                            'premium.and.cost.sharing', # PCs 
                            'cost.sharing.only', # Cs # renamed from: cost.sharing.without.premium
                            'full.pay.and.premium', # FP # renamed from: full.pay.and.premium.without.cost.sharing
                            'full.pay.and.premium.and.cost.sharing', # FPCs
                            'full.pay.and.cost.sharing' # FCs # renamed from: full.pay.and.cost.sharing.without.premium
)

# Cost-sharing subcategories: 
# D: deductible
# Cp: co-pay assistance 

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

####----------------------------------####
####----------------------------------####
####-- STATE-SPECIFIC INPUT HELPERS --####
####----------------------------------####
####----------------------------------####

is.state.medicaid.expansion <- function(location)
{
    if (any(location==MEDICAID.NONEXPANSION.STATES))
        0
    else
        1
}

get.state.medicaid.threshold <- function(location)
{
    if (is.state.medicaid.expansion(location))
        139
    else
        0
}

state.adap.allows.medicaid.only <- function(location)
{
    # From https://nastad.org/sites/default/files/2026-02/2026-adap-report-table-10.pdf
    # States with 0% on medicaid only
    DISALLOW.MEDICAID.ONLY.STATES = c('AL','CA','CT','DC','IN','IA','MD','NJ','NM','NC','RI','SC','WA','WI')
    
    if (any(location==DISALLOW.MEDICAID.ONLY.STATES))
        0
    else
        1
}

state.adap.allows.medicare.plus.medicaid <- function(location)
{
    # From https://nastad.org/sites/default/files/2026-02/2026-adap-report-table-10.pdf
    # States with 0% on medicaid and medicare
    DISALLOW.MEDICARE.MEDICAID.STATES = c('AL','AK', 'CT','GA',)
    
    if (any(location==DISALLOW.MEDICARE.MEDICAID.STATES))
        0
    else
        1
}

get.state.ssi.benefit.fpl <- function(location)
{
    # @todo need to make state-specific
    # for now this is just the federal ssi benefit / federal poverty level for single adult
    995*12 / 15960
}

get.state.ssi.breakeven.fpl <- function(location)
{
    # @todo need to make state-specific
    # for now this is just the federal ssi breakeven / federal poverty level for single adult
    2073*12 / 15960
}

# is.state.209b <- function(location)
# {
#     STATES.209B = c('CT','HI','IL','MN','MO','NH','ND','OH','OK','VA')    
#     if (any(location==STATES.209B))
#         1
#     else
#         0
# }

# From https://nastad.org/sites/default/files/2026-02/2026-adap-report-table-2.pdf
get.state.baseline.adap.full.pay.threshold <- function(location)
{
    STATE.ADAP.INCOME.THRESHOLD = c(
        AL = 400,
        AK = 400,
        AZ = 400,
        AR = 500,
        CA = 500,
        CO = 500,
        CT = 500,
        DE = 500,
        DC = 500,
        FL = 400,
        GA = 400,
        HI = 400,
        ID = 500,
        IL = 500,
        IN = 300,
        KS = 400,
        KY = 500,
        LA = 500,
        ME = 500,
        MD = 500,
        MA = 500,
        MI = 500,
        MN = 500,
        MS = 400,
        MO = 400,
        MT = 500,
        NE = 500,
        NV = 400,
        NH = 500,
        NJ = 500,
        NM = 500,
        NY = 500,
        NC = 300,
        ND = 500,
        OH = 500,
        OK = 500,
        OR = 550,
        PA = 500,
        PR = 500,
        RI = 500,
        SC = 550,
        SD = 300,
        TN = 400,
        TX = 200, 
        UT = 250,
        VT = 500,
        VA = 500,
        WA = 500,
        WI = 400,
        WY = 550
    )
    
    rv = STATE.ADAP.INCOME.THRESHOLD[location]
    
    if (is.na(rv))
        stop(paste0("We don't have a state ADAP threshold for '", location, "'"))
    
    rv
    
}

get.state.baseline.adap.premium.assistance.threshold <- function(location)
{
    get.state.baseline.adap.full.pay.threshold(location)
}

get.state.baseline.adap.cost.sharing.threshold <- function(location)
{
    get.state.baseline.adap.full.pay.threshold(location)
}

state.has.adap.premium.assistance <- function(location)
{
    # @todo need to make state-specific
    1
}

state.has.adap.cost.sharing <- function(location)
{
    # @todo need to make state-specific
    1
}

state.adap.cost.sharing.covers.copays <- function(location)
{
    # @todo need to make state-specific
    1
}

state.adap.cost.sharing.covers.deductibles <- function(location)
{
    # @todo need to make state-specific
    1
}


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
                        name = "adap.premium.assistance.threshold",
                        value = 'baseline.adap.premium.assistance.fpl.threshold',
                        scale = 'non.negative.number')

register.model.quantity(ADAP.SPECIFICATION,
                        name = "adap.cost.sharing.threshold",
                        value = 'baseline.adap.cost.sharing.fpl.threshold',
                        scale = 'non.negative.number')


##-- INPUTS: EFFECTS of ELIGIBILITY RESTRICTIONS --##
# Loss: 4 effects
register.model.element(ADAP.SPECIFICATION,
                       name = "lose.F.suppression.rr", # lose.adap.full.pay.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.P.suppression.rr", # lose.adap.premium.without.cost.sharing.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.PCs.suppression.rr", # lose.adap.premium.and.cost.sharing.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.Cs.suppression.rr", # lose.adap.cost.sharing.without.premium.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

# Change: 3 effects
register.model.element(ADAP.SPECIFICATION,
                       name = "change.P.to.F.suppression.rr",# change.adap.premium.without.cost.sharing.to.full.pay.suppression.rr
                       value = 1,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCs.to.F.suppression.rr", # change.adap.premium.and.cost.sharing.to.full.pay.suppression.rr
                       value = 1,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCs.to.P.suppression.rr", # change.adap.premium.and.cost.sharing.to.premium.without.cost.sharing.suppression.rr
                       value = 0.5,
                       scale = 'proportion')


##-- INPUTS: EFFECTS of FORMULARY RESTRICTIONS --##
register.model.element(ADAP.SPECIFICATION,
                       name = "change.F.formulary.suppression.rr", # change.adap.full.pay.formulary.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.PCp.formulary.suppression.rr", # change.adap.copay.and.premium.formulary.suppression.rr - swapped order
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.Cp.formulary.suppression.rr", #change.adap.copay.without.premium.formulary.suppression.rr
                       value = 0.5,
                       scale = 'proportion')

##-- INPUTS: HOW MANY EXPERIENCE FORMULARY RESTRICTIONS --##
register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.F.clients.with.formulary.change", #proportion.adap.full.pay.clients.with.formulary.change
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.PCp.clients.with.formulary.change", #proportion.adap.premium.and.copay.clients.with.formulary.change
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.Cp.clients.with.formulary.change", # proportion.adap.copay.without.premium.clients.with.formulary.change
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.adap.change.to.full.pay.clients.with.formulary.change", ## melissa question: WHAT DOES THIS MEAN? 
                       value = 0.5,
                       scale = 'proportion')


####----------------------------------####
####----------------------------------####
####-- INPUTS for SETUP/CALIBRATION --####
####----------------------------------####
####----------------------------------####

##-------------------##
##-- INPUT HELPERS --##
##-------------------##

get.adap.full.pay.only.suppression.functional.form <- function()
{
    get.adjusted.oahs.functional.form(
        adap.p.suppressed = 0.84 # 2023, from https://nastad.org/sites/default/files/2025-02/pdf-2025-adap-table-12.pdf
    )
}

get.adap.insurance.assistance.only.functional.form <- function()
{
    get.adjusted.oahs.functional.form(
        adap.p.suppressed = 0.89 # 2023, from https://nastad.org/sites/default/files/2025-02/pdf-2025-adap-table-12.pdf
    )
}

get.adap.full.pay.and.insurance.assistance.functional.form <- function()
{
    get.adjusted.oahs.functional.form(
        adap.p.suppressed = 0.92 # 2023, from https://nastad.org/sites/default/files/2025-02/pdf-2025-adap-table-12.pdf
    ) 
}

get.adjusted.oahs.functional.form <- function(adap.p.suppressed,
                                              oahs.p.suppressed = 0.906 #2023, from https://www.google.com/url?sa=t&source=web&rct=j&opi=89978449&url=https://ryanwhite.hrsa.gov/sites/default/files/ryanwhite/data/2023-ryan-white-annual-data-report.pdf&ved=2ahUKEwi7ptnkua2TAxUCKlkFHVcSJpQQFnoECBsQAQ&usg=AOvVaw33rl0vRWjxgb15zIJtsskb
                                              )
{
    log.or.adap.to.oahs = log(adap.p.suppressed) - log(1-adap.p.suppressed) + log(1-oahs.p.suppressed) - log(oahs.p.suppressed)
    
    oahs.ff = get.cached.object.for.version(name = "p.suppression.oahs.functional.form", version = 'rw')
    
    create.logistic.linear.functional.form(
        intercept = oahs.ff$betas$intercept + log.or.adap.to.oahs,
        slope = oahs.ff$betas$slope,
        anchor.year = oahs.ff$anchor.year,
        parameters.are.on.logit.scale = T
    )
}


#@todo - need to fold in race
get.p.baseline.benefits.eligible.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(c('13-24 years' = 0.01,
                        '25-34 years' = 0.10,
                        '35-44 years' = 0.26,
                        '45-54 years' = 0.64,
                        '55+ years' = 0.80),
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    )
}


#@todo - need to fold in race
get.p.ssi.if.income.and.benefits.eligible.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(c('13-24 years' = 0.01,
                        '25-34 years' = 0.10,
                        '35-44 years' = 0.26,
                        '45-54 years' = 0.64,
                        '55+ years' = 0.80),
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    )
}

#@todo
get.p.medicaid.if.income.eligible.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(0.5,
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    ) 
}

#@todo
get.p.medicaid.if.not.by.income.or.ssi.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(0.5,
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    ) 
}

#@todo
get.p.medicare.by.age.eligibility.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(0.5,
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    ) 
}

#@todo
get.p.medicare.if.medicaid.eligible.not.by.income.or.ssi.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(0.5,
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    ) 
}

#@todo
get.p.medicare.if.not.medicaid.eligible.functional.form <- function()
{
    dim.names = list(
        age = c('13-24 years',
                '25-34 years',
                '35-44 years',
                '45-54 years',
                '55+ years'),
        race = c('black', 'hispanic','other'),
        sex = c('heterosexual_male','msm','female'),
        risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
    )
    
    create.static.functional.form(
        value = array(0.5,
                      dim = sapply(dim.names, length),
                      dimnames = dim.names), # from jhhcc_code project, exploring_fpl.R
        link = 'logit',
        value.is.on.transformed.scale = F
    ) 
}

##-----------------------------##
##-- INPUTS: ADAP THRESHOLDS --##
##-----------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.full.pay.fpl.threshold",
                       get.value.function = get.state.baseline.adap.full.pay.threshold,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.premium.fpl.threshold",
                       get.value.function = get.state.baseline.adap.premium.assistance.threshold,
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
                       name = "adap.allows.medicare.plus.medicaid",
                       get.value.function = state.adap.allows.medicare.plus.medicaid,
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
                       value = get.state.medicaid.threshold,
                       sale = 'non.negative.number')

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

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.ssi.if.income.eligible', # probability that you have SSI if your income is below the threshold
                        value = expression(p.base.ssi.and.medicaid.eligible * p.ssi.if.income.and.benefits.eligible *
                                               (p.ssi.without.medicaid + (1-p.ssi.without.medicaid) * adap.allows.medicaid.only)))

##-- MEDICAID --##

register.model.element(ADAP.SPECIFICATION, # given that you have medicare and do not have SSI/are not under the Medicaid income threshold, what is the probability you are on medicaid
                       name = 'p.medicaid.not.by.income.or.ssi.if.medicare',
                       get.functional.form.function = x, #@todo - need to fill in
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION, # given that you do NOT have medicare and do not have SSI/are not under the Medicaid income threshold, what is the probability you are on medicaid
                       name = 'p.medicaid.not.by.income.or.ssi.if.no.medicare.and.benefits.eligible',
                       get.functional.form.function = x,
                       scale = 'proportion')

#-- UNINSURED --#
# The probability that you are uninsured given that you don't have public insurance is a function of your income

#@todo - need to fill in real estimates from data
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.midpoint',
                       value = 250, # the income at which the p of uninsurance is halfway between min and max p
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.uninsured.given.income.and.no.public.insurance.logistic.slope',
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
                       name = 'proportion.F.only.suppressed', # proportion.adap.full.pay.only.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.only.suppression.functional.form(),
                       functional.form.from.time = 2010)

# Insurance assistance only
register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.P.only.suppressed', # proportion.adap.premium.without.cost.sharing.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.Cs.only.suppressed', # proportion.adap.cost.sharing.without.premium.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.PCs.suppressed', # proportion.adap.premium.and.cost.sharing.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.insurance.assistance.only.functional.form(),
                       functional.form.from.time = 2010)

# Full-pay PLUS Insurance assistance
register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FP.suppressed', # proportion.adap.full.pay.and.premium.without.cost.sharing.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FCs.suppressed', # proportion.adap.full.pay.and.cost.sharing.without.premium.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.FPCs.suppressed', # proportion.adap.full.pay.and.premium.and.cost.sharing.suppressed
                       scale = 'proportion',
                       functional.form = get.adap.full.pay.and.insurance.assistance.functional.form(),
                       functional.form.from.time = 2010)


##------------------------------------------------##
##-- INPUTS: FRACTION OF TIME SPENT if A CLIENT --##
##------------------------------------------------##

# Individual categories: 
# F: full pay only 
# P: premium only 
# Cs: cost-sharing only 

# At any given time: 
# F, P, Cs, or PCs
    # Cannot have full-pay at the same time as either premium or cost-sharing, but can have premium and cost-sharing at the same time 

# Over the course of a year, can have: 
    # Only one category during that year (3): 
        # F, P, or Cs
    # Two categories during that year (3): 
        # FP: full pay and premium
        # FCs: full pay and cost-sharing
        # PCs: premium and cost-sharing 
    # All three categories during that year (1)
        # FPCs: full pay, premium, and cost-sharing 

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
                       name = 'fraction.time.F.among.FP', 
                       value = expression(fraction.time.covered.among.FP*fraction.time.F.of.time.covered.among.FP),
                       scale = 'proportion')

# and the same thing for premium (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                       name = 'fraction.time.P.among.FP', 
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
                        name = 'fraction.time.F.among.FCs', 
                        value = expression(fraction.time.covered.among.FCs*fraction.time.F.of.time.covered.among.FCs),
                        scale = 'proportion')

# and the same thing for cost-sharing (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.Cs.among.FCs', 
                        value = expression(fraction.time.covered.among.FCs*(1-fraction.time.F.of.time.covered.among.FCs)),
                        scale = 'proportion')

# PREMIUM AND COST-SHARING (PCs)
    # NOT mutually exclusive: could in theory spend time in: neither, each individually, or both
    # For simplicity, assume full coverage period was both (not either individually)
register.model.element(ADAP.SPECIFICATION,
                       name = 'fraction.time.covered.among.PCs', 
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
                        name = 'fraction.time.F.among.FPCs', 
                        value = expression(fraction.time.covered.among.FPCs*fraction.time.F.of.time.covered.among.FPCs),
                        scale = 'proportion')

# and the same thing for premium + cost-sharing (1-full pay)
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'fraction.time.PCs.among.FPCs', 
                        value = expression(fraction.time.covered.among.FPCs*(1-fraction.time.F.of.time.covered.among.FPCs)),
                        scale = 'proportion')

## Todd's old code, with some comments from me to compare
if(1==2){
    # F among FP
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.full.pay.among.full.pay.and.premium.without.cost.sharing',
                           value = 0.25,
                           scale = 'proportion')
    
    
    # quantity used to calculate P among FP (below)
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.non.full.pay.time.on.adap.premium.without.cost.sharing.from.with.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    
    
    # F among FPCs
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.full.pay.from.full.pay.and.premium.and.cost.sharing',
                           value = 0.25,
                           scale = 'proportion')
    
    # F among FCs
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.full.pay.from.full.pay.and.cost.sharing.without.premium',
                           value = 0.25,
                           scale = 'proportion')
    
    # Time on premium and/or cost-sharing from no full-pay
    # # P alone 
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.premium.without.cost.sharing.from.without.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    # PCs alone
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.premium.and.cost.sharing.from.without.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    # P alone - SAME AS ABOVE, I think he meant to do Cs alone 
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.time.on.adap.premium.without.cost.sharing.from.without.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    # Time on premium and/or cost-sharing as fraction of time not on full-pay when both
    # quantity used to calculate PCs among FPCs (below)
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.non.full.pay.time.on.adap.premium.and.cost.sharing.from.with.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    # quantity used to calculate Cs among FCs (below)
    register.model.element(ADAP.SPECIFICATION,
                           name = 'fraction.non.full.pay.time.on.adap.cost.sharing.without.premium.from.with.full.pay',
                           value = 0.875,
                           scale = 'proportion')
    
    # Quick calculation for the non-full pay services when combined with full-pay
    # P among FP
    register.model.quantity(ADAP.SPECIFICATION,
                            name = 'fraction.time.on.adap.premium.without.cost.sharing.from.with.full.pay',
                            value = expression( (1 - fraction.time.on.adap.premium.without.cost.sharing.from.without.full.pay) *
                                                    fraction.non.full.pay.time.on.adap.premium.without.cost.sharing.from.with.full.pay))
    # PCs among FPCs
    register.model.quantity(ADAP.SPECIFICATION,
                            name = 'fraction.time.on.adap.premium.and.cost.sharing.from.with.full.pay',
                            value = expression( (1 - fraction.time.on.adap.premium.and.cost.sharing.from.without.full.pay) *
                                                    fraction.non.full.pay.time.on.adap.premium.and.cost.sharing.from.with.full.pay))
    # Cs among FCs
    register.model.quantity(ADAP.SPECIFICATION,
                            name = 'fraction.time.on.adap.cost.sharing.without.premium.from.with.full.pay',
                            value = expression( (1 - fraction.time.on.adap.cost.sharing.without.premium.from.without.full.pay) *
                                                    fraction.non.full.pay.time.on.adap.cost.sharing.without.premium.from.with.full.pay))
    
}


##---------------------------------------------------------------------------##
##-- INPUTS: PROPORTION OF COST-SHARING CLIENTS WHO RECEIVE COPAY SERVICES --##
##---------------------------------------------------------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.Cs.clients.with.Cp.if.allowed", # p.adap.cost.sharing.without.premium.clients.with.copay.if.allowed
                       functional.form = proportion.adap.cost.sharing.without.premium.clients.with.copay(), # melissa question: should this be a functional form?
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.PCs.clients.with.Cp.if.allowed", # p.adap.premium.and.cost.sharing.clients.with.copay.if.allowed
                       functional.form = get.p.adap.cost.sharing.clients.with.copay.functional.form(),
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.FPCs.clients.with.Cp.if.allowed", # p.adap.full.pay.premium.and.cost.sharing.clients.with.copay.if.allowed
                       functional.form = proportion.adap.full.pay.premium.and.cost.sharing.clients.with.copay(), # melissa question: should this be a functional form?
                       scale = 'proportion')


#-- Quickly, put these together with the state-level inputs --#
#   The three quantities below evaluate to:
#   - p.adap.<x>.clients.with.copay.if.allowed  IF  both copay and deductible are covered by state ADAP
#   - 0                                         IF  state ADAP does not cover copays
#   - 1                                         IF  state ADAP does not cover deductibles
register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.PCs.clients.with.Cp", # proportion.adap.premium.and.cost.sharing.clients.with.copay
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.PCs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))

register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.FPCs.clients.with.Cp", # proportion.adap.full.pay.premium.and.cost.sharing.clients.with.copay
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.FPCs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))

register.model.quantity(ADAP.SPECIFICATION,
                        name = "proportion.Cs.clients.with.Cp", # proportion.adap.cost.sharing.without.premium.clients.with.copay
                        value = expression(adap.covers.copay * adap.covers.deductible * proportion.Cs.clients.with.Cp.if.allowed + 
                                               (1-adap.covers.deductible)*adap.covers.copay))


##----------------------------------------------##
##-- INPUTS: P ADAP SERVICE TYPE GIVEN INCOME --##
##----------------------------------------------##

# we presume that: p_full_pay = min + (max-min) / (1 + exp(slope * (income - midpoint)))
# *NB that slope here is constrained to be positive - ie, a strictly decreasing p with increasing income


# @redo - these are all just placeholder parameters

#-- Probability of having ONLY full-pay services if on ADAP --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.logistic.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.medicaid.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.medicare.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.medicare.and.medicaid.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.private.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.only.if.adap.uninsured.or',
                       value = 1,
                       scale = 'ratio')

#-- Probablity of having full-pay plus other services if on ADAP not full pay only --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.logistic.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.adap.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.medicaid.or',
                       value = 0.2,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.medicare.or',
                       value = 0.2,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.medicare.and.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.private.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.plus.if.not.full.pay.only.uninsured.or',
                       value = 0.1,
                       scale = 'ratio')


#-- Probability of receiving premium assistance if also on full pay --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.logistic.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.medicaid.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.medicare.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.medicare.and.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.private.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.full.pay.uninsured.or',
                       value = 0.01,
                       scale = 'ratio')

#--Probability of receiving cost-sharing assistance if on full.pay and premium assistance --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.medicare.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.medicare.and.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.private.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.full.pay.and.premium.uninsured.or',
                       value = 1,
                       scale = 'ratio')

#-- Probability of receiving premium assistance if not receiving any full pay services --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.logistic.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.medicaid.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.medicare.or',
                       value = 0.1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.medicare.and.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.private.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.premium.if.no.full.pay.uninsured.or',
                       value = 0.01,
                       scale = 'ratio')

#-- Probability of receiving cost-sharing assistance if on premium assistance but not full pay --#
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.premium.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.premium.max',
                       value = 0.95,
                       scale = 'proportion')

# odds ratios based on insurance
register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.medicare.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.medicare.and.medicaid.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.private.or',
                       value = 1,
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.cost.sharing.if.premium.without.full.pay.uninsured.or',
                       value = 1,
                       scale = 'ratio')

####---------------------------####
####---------------------------####
####-- CALCULATED QUANTITIES --####
####---------------------------####
####---------------------------####


# need to make quantities for
# - max.income

##-------------------------##
##-- CALCULATED: HELPERS --##
##-------------------------##

# Take the income distribution parameters plus SSI parameters
# and project the probability of being in each single percentage-point multiple of FPL
# calculate.adap.income.proportions.single.fpl <- function(adap.fpl.median,
#                                                          adap.fpl.cv,
#                                                          p.ssi.if.income.eligible,
#                                                          ssi.benefit.fpl,
#                                                          ssi.breakeven.fpl,
#                                                          max.income)
# {
#     income = 0:max.income
#     
#     adap.fpl.sd = adap.fpl.median * adap.fpl.cv
#     multiply.p.by = 1 / pnorm(max.income, adap.fpl.median, adap.fpl.sd)
#         
#     p = sapply(income, function(inc){
#         
#         if (inc >= ssi.breakeven.fpl)
#             pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
#         else if (inc < ssi.benefit.fpl)
#             (1-p.ssi.if.income.eligible) * pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
#         else
#         {
#             what.inc.would.be.without.ssi = (inc - ssi.benefit.fpl) * ssi.breakeven.fpl  / (ssi.breakeven.fpl - ssi.benefit.fpl)
#             
#             p.ssi.if.income.eligible * pnorm(what.inc.would.be.without.ssi, adap.fpl.median, adap.fpl.sd) * multiply.p.by +
#             (1-p.ssi.if.income.eligible) * pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
#         }
#         
#     })
#     dim(p) = c(length(p)/length(income), length(income))
#         # dimensions of p are [stratum, income]
#     
#     p[,-1] = p[,-1] - p[,-dim(p)[2]]
#     
#     dim(p) = c(dim(adap.fpl.median), income=length(income))
#     dimnames(p) = c(dimnames(adap.fpl.median), list(income = income))
#     
#     p
# }

calculate.baseline.p.of.adap.by.income.with.ssi <- function(adap.fpl.median,
                                                            adap.fpl.cv,
                                                            p.ssi.if.income.eligible,
                                                            ssi.benefit.fpl,
                                                            ssi.breakeven.fpl,
                                                            max.baseline.adap.income)
{
    income = 0:max.baseline.adap.income

    adap.fpl.sd = adap.fpl.median * adap.fpl.cv
    multiply.p.by = 1 / pnorm(max.baseline.adap.income, adap.fpl.median, adap.fpl.sd)
    zero = array(0, dim=dim(adap.fpl.median))
    
    p = sapply(income, function(inc){
        
        if (inc < ssi.breakeven.fpl & inc >= ssi.benefit.fpl)
        {
            what.inc.would.be.without.ssi = (inc - ssi.benefit.fpl) * ssi.breakeven.fpl  / (ssi.breakeven.fpl - ssi.benefit.fpl)
            p.ssi.if.income.eligible * pnorm(what.inc.would.be.without.ssi, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        }
        else
            zero
    })
    dim(p) = c(length(p)/length(income), length(income))
        # dimensions of p are [stratum, income]

    p[,-1] = p[,-1] - p[,-dim(p)[2]]

    dim(p) = c(dim(adap.fpl.median), income=length(income))
    dimnames(p) = c(dimnames(adap.fpl.median), list(income = income))

    p
}

calculate.baseline.p.of.adap.by.income.without.ssi <- function(adap.fpl.median,
                                                               adap.fpl.cv,
                                                               p.ssi.if.income.eligible,
                                                               ssi.benefit.fpl,
                                                               ssi.breakeven.fpl,
                                                               max.baseline.adap.income)
{
    income = 0:max.baseline.adap.income
    
    adap.fpl.sd = adap.fpl.median * adap.fpl.cv
    multiply.p.by = 1 / pnorm(max.baseline.adap.income, adap.fpl.median, adap.fpl.sd)
    
    p = sapply(income, function(inc){
        
        if (inc >= ssi.breakeven.fpl)
            pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        else
            (1-p.ssi.if.income.eligible) * pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        
    })
    dim(p) = c(length(p)/length(income), length(income))
    # dimensions of p are [stratum, income]
    
    p[,-1] = p[,-1] - p[,-dim(p)[2]]
    
    dim(p) = c(dim(adap.fpl.median), income=length(income))
    dimnames(p) = c(dimnames(adap.fpl.median), list(income = income))
    
    p
}
    
calculate.max.baseline.adap.income <- function(baseline.adap.full.pay.fpl.threshold,
                                               baseline.adap.premium.fpl.threshold,
                                               baseline.adap.cost.sharing.fpl.threshold,
                                               ssi.breakeven.fpl)
{
    max(baseline.adap.full.pay.fpl.threshold, 
        baseline.adap.premium.fpl.threshold,
        baseline.adap.cost.sharing.fpl.threshold,
        ssi.breakeven.fpl)
}
    

# A helper to generate a set of logistic probabilities
calculate.logistic.p <- function(logistic.midpoint,
                                 logistic.slope,
                                 min.p,
                                 max.p,
                                 max.baseline.adap.income,
                                 additional.or = 1)
{
    income = 0:max.baseline.adap.income
    p = min.p + (max.p - min.p) /
        (1 + exp(logistic.slope * (income - logistic.midpoint) + log(additional.or)))
    
    dim(p) = c(income=length(income))
    dimnames(p) = list(income=income)
    p
}


##-----------------------------------------------------##
##-- CALCULATED: INCOME DISTRIBUTION of ADAP CLIENTS --##
##-----------------------------------------------------##


register.model.quantity(ADAP.SPECIFICATION,
                        name = 'max.adap.baseline.income',
                        value = calculate.max.baseline.adap.income)

# Calculate income distribution, stratified by SSI, for all ADAP clients
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.with.ssi',
                        value = calculate.baseline.p.of.adap.by.income.with.ssi)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.without.ssi',
                        value = calculate.baseline.p.of.adap.by.income.without.ssi)

# Fold in Medicare
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.with.medicare.and.ssi',
                        value = expression(baseline.p.of.adap.by.income.with.ssi *
                                               p.baseline.benefits.eligible *
                                               (p.medicare.by.age.if.benefits.eligible +
                                                    (1-p.medicare.by.age.if.benefits.eligible) * p.medicare.if.not.by.age.if.benefits.eligible))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.with.medicare.without.ssi',
                        value = expression(baseline.p.of.adap.by.income.without.ssi *
                                               p.baseline.benefits.eligible *
                                               (p.medicare.by.age.if.benefits.eligible +
                                                    (1-p.medicare.by.age.if.benefits.eligible) * p.medicare.if.not.by.age.if.benefits.eligible))
)


# Fold in Medicaid

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'income.is.below.medicaid.threshold',
                        value = calculate.income.is.below.medicaid.threshold)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.with.medicare.and.medicaid',
                        value = expression(adap.allows.medicare.and.medicaid * (
                            baseline.p.of.adap.by.income.with.medicare.and.ssi +
                                baseline.p.of.adap.by.income.with.medicare.without.ssi * 
                                (income.is.below.medicaid.threshold +
                                     (1-income.is.below.medicaid.threshold) *
                                     p.medicaid.not.by.income.or.ssi.if.medicare * p.base.ssi.and.medicaid.eligible)
                        )))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.with.medicaid.without.medicare',
                        value = expression(adap.allows.medicaid.only * (
                            (baseline.p.of.adap.by.income.with.ssi - baseline.p.of.adap.by.income.with.medicare.and.ssi) +
                                (baseline.p.of.adap.by.income.without.ssi - baseline.p.of.adap.by.income.with.medicare.without.ssi) *
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
                        name = 'baseline.p.of.adap.by.income.with.medicare.without.medicaid',
                        value = expression(baseline.p.of.adap.by.income.with.medicare.and.ssi + 
                                               baseline.p.of.adap.by.income.with.medicare.without.ssi -
                                               baseline.p.of.adap.by.income.with.medicare.and.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.without.medicare.or.medicaid',
                        value = expression(baseline.p.of.adap.by.income.with.ssi + 
                                               baseline.p.of.adap.by.income.without.ssi -
                                               baseline.p.of.adap.by.income.with.medicare.and.medicaid -
                                               baseline.p.of.adap.by.income.with.medicare.without.medicaid -
                                               baseline.p.of.adap.by.income.with.medicaid.without.medicare))

calculate.p.uninsured.given.income.and.no.public.insurance <- function(p.uninsured.given.income.and.no.public.insurance.midpoint,
                                                                       p.uninsured.given.income.and.no.public.insurance.slope,
                                                                       p.uninsured.given.income.and.no.public.insurance.min,
                                                                       p.uninsured.given.income.and.no.public.insurance.max,
                                                                       max.baseline.adap.income)
{
    calculate.logistic.p(logistic.midpoint = p.uninsured.given.income.and.no.public.insurance.midpoint,
                         logistic.slope = p.uninsured.given.income.and.no.public.insurance.slope,
                         min.p = p.uninsured.given.income.and.no.public.insurance.min,
                         max.p = p.uninsured.given.income.and.no.public.insurance.max,
                         max.baseline.adap.income = max.baseline.adap.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.uninsured.given.income.and.no.public.insurance',
                        value = calculate.p.uninsured.given.income.and.no.public.insurance)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.uninsured',
                        value = expression(baseline.p.of.adap.by.income.without.medicare.or.medicaid * p.uninsured.given.income.and.no.public.insurance))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.by.income.private.insurance',
                        value = expression(baseline.p.of.adap.by.income.without.medicare.or.medicaid - baseline.p.of.adap.by.income.uninsured))



##------------------------------------------------------------------------##
##-- CALCULATED: DISTRIBUTE P by INCOME on ADAP INTO SERVICE CATEGORIES --##
##------------------------------------------------------------------------##



#-- Distribute for Uninsured --#

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.uninsured',
                        value = 'baseline.p.of.adap.by.income.uninsured')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income.uninsured',
                        value = 0)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income.uninsured',
                        value = 0)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium.by.income.uninsured',
                        value = 0)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.without.cost.sharing.by.income.uninsured',
                        value = 0)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.and.cost.sharing.by.income.uninsured',
                        value = 0)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.cost.sharing.without.premium.by.income.uninsured',
                        value = 0)

#-- Distribute for Medicaid --#

# Full-Pay Only
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.medicaid',
                        value = expression(baseline.p.of.adap.by.income.uninsured * baseline.p.full.pay.only.by.income.medicaid))

# Full-Pay Plus
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.plus.by.income.medicaid',
                        value = expression(baseline.p.of.adap.by.income.uninsured * 
                                               (1-baseline.p.full.pay.only.by.income.medicaid) *
                                               baseline.p.full.pay.plus.if.not.full.pay.only.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.full.pay.plus.by.income.medicaid * baseline.p.premium.if.full.pay.plus.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.by.income.medicaid * baseline.p.cost.sharing.if.full.pay.and.premium.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.without.cost.sharing.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.by.income.medicaid - baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.cost.sharing.without.premium.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.full.pay.plus.by.income.medicaid - baseline.p.of.adap.with.full.pay.and.premium.by.income.medicaid))

# No Full-Pay
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.without.full.pay.by.income.medicaid',
                        value = expression(baseline.p.of.adap.by.income.uninsured * 
                                               (1-baseline.p.full.pay.only.by.income.medicaid) *
                                               (1-baseline.p.full.pay.plus.if.not.full.pay.only.by.income.medicaid)))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.by.income.medicaid',
                        value = expression(baseline.p.of.adap.without.full.pay.by.income.medicaid * baseline.p.premium.if.not.full.pay.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.and.cost.sharing.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.premium.by.income.medicaid * baseline.p.cost.sharing.if.premium.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.without.cost.sharing.by.income.medicaid',
                        value = expression(baseline.p.of.adap.with.premium.by.income.medicaid - baseline.p.of.adap.with.premium.and.cost.sharing.by.income.medicaid))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.cost.sharing.without.premium.by.income.medicaid',
                        value = expression(baseline.p.of.adap.without.full.pay.by.income.medicaid - baseline.p.of.adap.with.premium.by.income.medicaid))



# Need to add
# baseline.p.full.pay.only.by.income.medicaid
# baseline.p.full.pay.plus.if.not.full.pay.only.by.income.medicaid
# baseline.p.premium.if.full.pay.plus.by.income.medicaid
# baseline.p.cost.sharing.if.full.pay.and.premium.by.income.medicaid
# baseline.p.cost.sharing.if.premium.by.income.medicaid
# baseline.p.premium.if.not.full.pay.by.income.medicaid

# UP TO HERE !!!!


#-- Distribute for Medicare + Medicaid --#

#-- Distribute for Medicare --#

#-- Distribute for Private Insurance --#

# Also need to add:
#   income.

# going to need to define
# rr.full.pay.with.medicaid.only
# rr.full.pay.with.medicare.only
# rr.full.pay.with.medicare.and.medicaid

##-- Distribute by Income across service categories --##


#-- Full Pay Only --##
calculate.p.full.pay.only.by.income <- function(p.full.pay.only.if.adap.midpoint,
                                                p.full.pay.only.if.adap.logistic.slope,
                                                p.full.pay.only.if.adap.min,
                                                p.full.pay.only.if.adap.max,
                                                max.baseline.adap.income)
{
    calculate.logistic.p(logistic.midpoint = p.full.pay.only.if.adap.midpoint,
                         logistic.slope = p.full.pay.only.if.adap.logistic.slope,
                         min.p = p.full.pay.only.if.adap.min,
                         max.p = p.full.pay.only.if.adap.max,
                         max.baseline.adap.income = max.baseline.adap.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.full.pay.only.by.income',
                        value = calculate.p.full.pay.by.income)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.and.uninsured',
                        value = expression(baseline.p.of.adap.by.income.uninsured * baseline.p.full.pay.only.by.income))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.and.private.insurance',
                        value = expression(baseline.p.of.adap.by.income.private.insurance * baseline.p.full.pay.only.by.income * rr.full.pay.only.if.private.insurance))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.with.medicare.without.medicaid',
                        value = expression(baseline.p.full.pay.only.by.income * baseline.p.of.adap.by.income.with.medicare.without.medicaid * rr.full.pay.with.medicare.only))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.with.medicaid.without.medicare',
                        value = expression(baseline.p.full.pay.only.by.income * baseline.p.of.adap.by.income.with.medicaid.without.medicare * rr.full.pay.with.medicaid.only))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income.with.medicare.and.medicaid',
                        value = expression(baseline.p.full.pay.only.by.income * baseline.p.of.adap.by.income.with.medicare.and.medicaid * rr.full.pay.with.medicare.and.medicaid))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only.by.income',
                        value = expression(p.of.adap.with.full.pay.only.by.income.without.medicare.or.medicaid +
                                               p.of.adap.with.full.pay.only.by.income.with.medicare.without.medicaid +
                                               p.of.adap.with.full.pay.only.by.income.with.medicaid.without.medicare +
                                               p.of.adap.with.full.pay.only.by.income.with.medicare.and.medicaid))


#-- Any Premium Assistance --#
calculate.p.premium.if.not.full.pay.only.by.income <- function(p.premium.if.not.full.pay.only.if.adap.midpoint,
                                                               p.premium.if.not.full.pay.only.if.adap.logistic.slope,
                                                               p.premium.if.not.full.pay.only.if.adap.min,
                                                               p.premium.if.not.full.pay.only.if.adap.max,
                                                               max.baseline.adap.income)
{
    calculate.logistic.p(logistic.midpoint = p.premium.if.not.full.pay.only.if.adap.midpoint,
                         logistic.slope = p.premium.if.not.full.pay.only.if.adap.logistic.slope,
                         min.p = p.premium.if.not.full.pay.only.if.adap.min,
                         max.p = p.premium.if.not.full.pay.only.if.adap.max,
                         max.baseline.adap.income = max.baseline.adap.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.premium.if.not.full.pay.only.by.income',
                        value = calculate.p.premium.if.not.full.pay.only.by.income)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.if.not.full.pay.only.by.income.without.medicare.or.medicaid',
                        value = expression(baseline.p.premium.if.not.full.pay.only.by.income * 
                                               (baseline.p.of.adap.by.income.without.medicare.or.medicaid - p.of.adap.with.full.pay.only.by.income.without.medicare.or.medicaid)))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.if.not.full.pay.only.by.income.with.medicare.without.medicaid',
                        value = expression(baseline.p.premium.if.not.full.pay.only.by.income * rr.premium.with.medicare.only *
                                               (baseline.p.of.adap.by.income.with.medicare.without.medicaid - baseline.p.of.adap.with.full.pay.only.by.income.with.medicare.without.medicaid)))
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.if.not.full.pay.only.by.income.with.medicaid.without.medicare',
                        value = expression(baseline.p.premium.if.not.full.pay.only.by.income * rr.full.pay.with.medicaid.only *
                                               (baseline.p.of.adap.by.income.with.medicaid.without.medicare - baseline.p.of.adap.with.full.pay.only.by.income.with.medicaid.without.medicare)))e
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.if.not.full.pay.only.by.income.with.medicare.and.medicaid',
                        value = expression(baseline.p.premium.if.not.full.pay.only.by.income * baseline.p.of.adap.by.income.with.medicare.and.medicaid * rr.full.pay.with.medicare.and.medicaid))



## <-- UP TO HERE --> ##
#-- Full Pay Plus --#
calculate.p.full.pay.plus.if.not.full.pay.only.by.income <- function(p.full.pay.plus.if.not.full.pay.only.midpoint,
                                                p.full.pay.plus.only.if.adap.logistic.slope,
                                                p.full.pay.plus.only.if.adap.min,
                                                p.full.pay.plus.only.if.adap.max,
                                                max.baseline.adap.income)
{
    calculate.logistic.p(logistic.midpoint = p.full.pay.plus.only.if.adap.midpoint,
                         logistic.slope = p.full.pay.plus.only.if.adap.logistic.slope,
                         min.p = p.full.pay.plus.only.if.adap.min,
                         max.p = p.full.pay.plus.only.if.adap.max,
                         max.baseline.adap.income = max.baseline.adap.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.full.pay.plus.if.not.full.pay.only.by.income',
                        value = calculate.p.full.pay.plus.if.not.full.pay.only.by.income)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.full.pay.plus.by.income',
                        value = expression(p.full.pay.plus.if.not.full.pay.only.by.income * (
                            baseline.p.of.adap.by.income.without.medicare.or.medicaid +
                                baseline.p.of.adap.by.income.with.medicaid.without.medicare * rr.full.pay.with.medicaid.only +
                                baseline.p.of.adap.by.income.with.medicare.without.medicaid * rr.full.pay.with.medicare.only +
                                baseline.p.of.adap.by.income.with.medicare.and.medicaid * rr.full.pay.with.medicare.and.medicaid
                        )))


register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.premium.without.cost.sharing.by.income',
                        value = 0.5)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.premium.and.cost.sharing.by.income',
                        value = 0.5)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.cost.sharing.without.premium.by.income',
                        value = 0.5)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income',
                        value = 0.5)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income',
                        value = 0.5)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'p.of.adap.with.full.pay.and.cost.sharing.without.premium.by.income',
                        value = 0.5)


##--------------------------------------------------------------##
##-- CALCULATED: DISTRIBUTE P ON ADAP INTO SERVICE CATEGORIES --##
##   Sum over income to get proportion in each service category ##
##--------------------------------------------------------------##

#-- Helper Function --#
sum.p.across.income <- function(income.distribution)
{
    non.income.dimensions = setdiff(names(dim(income.distribution)), 'income')
    apply(income.distribution, non.income.dimensions, sum)
}

# Full-Pay Only
calculate.baseline.p.of.adap.with.full.pay.only <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.only',
                        value = calculate.baseline.p.of.adap.with.full.pay.only)

# Full-Pay Plus another service
calculate.baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing',
                        value = calculate.baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing)

calculate.baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing',
                        value = calculate.baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing)

calculate.baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium',
                        value = calculate.baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium)

# No full-pay
calculate.baseline.p.of.adap.with.premium.without.cost.sharing <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.without.cost.sharing',
                        value = calculate.baseline.p.of.adap.with.premium.without.cost.sharing)

calculate.baseline.p.of.adap.with.premium.and.cost.sharing <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.premium.and.cost.sharing',
                        value = calculate.baseline.p.of.adap.with.premium.and.cost.sharing)

calculate.baseline.p.of.adap.with.cost.sharing.without.premium <- function(p.of.adap.with.full.pay.only.by.income) {
    sum.p.across.income(p.of.adap.with.full.pay.only.by.income)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.p.of.adap.with.cost.sharing.without.premium',
                        value = calculate.baseline.p.of.adap.with.cost.sharing.without.premium)

##---------------------------------------------------------------------##
##-- CALCULATED: PROPORTION ADAP CLIENTS WHO LOSE or CHANGE SERVICES --##
##---------------------------------------------------------------------##

#-- A helper --#
calculate.p.between.thresholds <- function(income.distribution,
                                           lower.threshold,
                                           upper.threshold)
{
    if (lower.threshold>=upper.threshold)
        income.indices = integer(0)
    else
    {
        max.index = dim(income.distribution)['income']
        lower.index = min(max.index, max(0, floor(lower.threshold)))
        upper.index = min(max.index, max(0, ceiling(upper.threshold)))
        income.indices = lower.index:upper.index
    }
    
    non.income.dimensions = setdiff(names(dim(income.distribution)), 'income')
    
    apply(income.distribution, non.income.dimensions, function(inc){
        sum(inc[income.indices]) / sum(inc)
    })
}


#-- From full-pay only --#
calculate.proportion.adap.full.pay.only.who.lose.eligibility < - function(p.of.adap.with.full.pay.only.by.income, 
                                                                          adap.full.pay.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.only.by.income, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.only.who.lose.eligibility',
                        value = calculate.proportion.adap.full.pay.only.who.lose.eligibility)

#-- From full-pay + premium without cost-sharing --#
calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.full.pay.eligibility < - function(p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                                                                                               adap.full.pay.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.full.pay.eligibility',
                        value = calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.full.pay.eligibility)

calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.premium.eligibility < - function(p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                                                                                              adap.full.pay.fpl.threshold, 
                                                                                                              adap.premium.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold, adap.premium.fpl.threshold),
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.premium.eligibility',
                        value = calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.premium.eligibility)

calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay < - function(p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                                                                                                    adap.full.pay.fpl.threshold, 
                                                                                                                    adap.premium.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.without.cost.sharing.by.income, 
                                   lower.threshold = adap.premium.fpl.threshold,
                                   upper.threshold = adap.full.pay.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay',
                        value = calculate.proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay)




#-- From full-pay + premium and cost-sharing --#
calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.full.pay.eligibility < - function(p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                                                                                           adap.full.pay.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.full.pay.eligibility',
                        value = calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.full.pay.eligibility)

calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility < - function(p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                                                                                           adap.full.pay.fpl.threshold,
                                                                                                           adap.premium.fpl.threshold,
                                                                                                           adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold, max(adap.premium.fpl.threshold, adap.cost.sharing.fpl.threshold)),
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility',
                        value = calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility)


calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay < - function(p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                                                                                                                    adap.full.pay.fpl.threshold,
                                                                                                                                    adap.premium.fpl.threshold,
                                                                                                                                    adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                   lower.threshold = max(adap.premium.fpl.threshold, adap.cost.sharing.fpl.threshold),
                                   upper.threshold = adap.full.pay.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay',
                        value = calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay)

calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing < - function(p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                                                                                                                      adap.full.pay.fpl.threshold,
                                                                                                                                      adap.premium.fpl.threshold,
                                                                                                                                      adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.and.premium.and.cost.sharing.by.income, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold,
                                   upper.threshold = adap.premium.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing',
                        value = calculate.proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing)


#-- From full-pay + cost-sharing without premium --#
calculate.proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.full.pay.eligibility < - function(p.of.adap.with.full.pay.and.cost.sharing.without.premium.by.income, 
                                                                                                               adap.full.pay.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.only.by.income, 
                                   lower.threshold = adap.full.pay.fpl.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.full.pay.eligibility',
                        value = calculate.proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.full.pay.eligibility)

calculate.proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility < - function(p.of.adap.with.full.pay.and.cost.sharing.without.premium.by.income, 
                                                                                                                   adap.cost.sharing.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.full.pay.only.by.income, 
                                   lower.threshold = adap.cost.sharing.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility',
                        value = calculate.proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility)


#-- From premium without cost-sharing --#
calculate.proportion.adap.premium.without.cost.sharing.who.lose.eligibility < - function(p.of.adap.with.premium.without.cost.sharing.by.income, 
                                                                                         adap.full.pay.fpl.threshold, 
                                                                                         adap.premium.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.premium.without.cost.sharing.by.income, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold, adap.premium.fpl.threshold),
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.premium.without.cost.sharing.who.lose.eligibility',
                        value = calculate.proportion.adap.premium.without.cost.sharing.who.lose.eligibility)

calculate.proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay < - function(p.of.adap.with.premium.without.cost.sharing.by.income, 
                                                                                                       adap.full.pay.fpl.threshold, 
                                                                                                       adap.premium.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.premium.without.cost.sharing.by.income, 
                                   lower.threshold = adap.premium.fpl.threshold,
                                   upper.threshold = adap.full.pay.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay',
                        value = calculate.proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay)



#-- From premium and cost-sharing --#


proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing

calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility < - function(p.of.adap.with.premium.and.cost.sharing.by.income, 
                                                                                     adap.full.pay.fpl.threshold,
                                                                                     adap.premium.fpl.threshold,
                                                                                     adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility, 
                                   lower.threshold = max(adap.full.pay.fpl.threshold, max(adap.premium.fpl.threshold, adap.cost.sharing.fpl.threshold)),
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.premium.and.cost.sharing.who.lose.eligibility',
                        value = calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility)


calculate.proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay < - function(calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility, 
                                                                                                   adap.full.pay.fpl.threshold,
                                                                                                   adap.premium.fpl.threshold,
                                                                                                   adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility, 
                                   lower.threshold = max(adap.premium.fpl.threshold, adap.cost.sharing.fpl.threshold),
                                   upper.threshold = adap.full.pay.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay',
                        value = calculate.proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay)

calculate.proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing < - function(calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility, 
                                                                                                                                    adap.full.pay.fpl.threshold,
                                                                                                                                    adap.premium.fpl.threshold,
                                                                                                                                    adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = calculate.proportion.adap.premium.and.cost.sharing.who.lose.eligibility, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold,
                                   upper.threshold = adap.premium.fpl.threshold)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing',
                        value = calculate.proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing)



#-- From cost-sharing without premium --#
calculate.proportion.adap.cost.sharing.without.premium.who.lose.eligibility < - function(p.of.adap.with.cost.sharing.without.premium.by.income, 
                                                                                         adap.cost.sharing.fpl.threshold) 
{
    calculate.p.between.thresholds(income.distribution = p.of.adap.with.cost.sharing.without.premium.by.income, 
                                   lower.threshold = adap.cost.sharing.fpl.threshold,
                                   upper.threshold = Inf)
}
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.cost.sharing.without.premium.who.lose.eligibility',
                        value = calculate.proportion.adap.cost.sharing.without.premium.who.lose.eligibility)

##-----------------------------##
##-- CALCULATED: SUPPRESSION --##
##-----------------------------##

##-- TIE-IN TO MAIN SUPPRESSION --##
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'suppression.of.diagnosed',
                        value = expression(
                                # Never had ADAP
                                proportion.pwh.who.are.suppressed.without.adap +
                                
                                # ADAP Unchanged
                                proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged +
                                proportion.pwh.who.are.suppressed.with.adap.premium.without.cost.sharing.unchanged +
                                proportion.pwh.who.are.suppressed.with.adap.premium.and.cost.sharing.unchanged +
                                proportion.pwh.who.are.suppressed.with.adap.cost.sharing.without.premium.unchanged +
                                
                                # Lose ADAP
                                proportion.pwh.who.are.suppressed.and.lose.adap.full.pay * lose.F.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.lose.adap.premium.without.cost.sharing * lose.P.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.lose.adap.premium.and.cost.sharing * lose.PCs.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.lose.adap.cost.sharing.without.premium * lose.Cs.suppression.rr +

                                # Change ADAP Service
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.without.cost.sharing.to.full.pay.without.formulary.change * change.P.to.F.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.without.formulary.change * change.PCs.to.F.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.premium.without.cost.sharing * change.PCs.to.P.suppression.rr +
                                                                    
                                # Formulary Change
                                proportion.pwh.who.are.suppressed.and.keep.adap.full.pay.but.change.formulary * change.F.formulary.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.keep.adap.premium.and.copay.but.change.formulary * change.PCp.formulary.suppression.rr +
                                proportion.pwh.who.are.suppressed.and.keep.adap.copay.without.premium.but.change.formulary * change.Cp.formulary.suppression.rr +
                                
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.without.copay.to.full.pay.and.change.formulary * change.adap.full.pay.formulary.suppression.rr + #change.F.formulary.suppression.rr? I think this was a copy paste error
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.and.change.formulary * change.adap.full.pay.formulary.suppression.rr #change.F.formulary.suppression.rr? I think this was a copy paste error
                                )
)

#-- Suppression Among Full Pay --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.full.pay',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.only *
                                                    proportion.adap.full.pay.only.who.lose.eligibility +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.without.cost.sharing *
                                                    proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.full.pay.eligibility +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.and.cost.sharing *
                                                    proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.full.pay.eligibility  *
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.cost.sharing.without.premium *
                                                    proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.full.pay.eligibility
                                                ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.full.pay',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.only *
                                                    (1-proportion.adap.full.pay.only.who.lose.eligibility) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.without.cost.sharing *
                                                    (1-proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.full.pay.eligibility) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.and.cost.sharing *
                                                    (1-proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.full.pay.eligibility) *
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.cost.sharing.without.premium *
                                                    (1-proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.full.pay.eligibility)
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                       name = 'proportion.pwh.who.are.suppressed.and.keep.adap.full.pay.but.change.formulary',
                       value = expression(.who.are.suppressed.and.keep.adap.full.pay * 
                                              proportion.F.clients.with.formulary.change)
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.full.pay.unchanged',
                        value = expression(.who.are.suppressed.and.keep.adap.full.pay * 
                                               (1-proportion.F.clients.with.formulary.change))
)


# Full-pay suppression components
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.only',
                        value = expression(baseline.p.of.adap.with.full.pay.only * 
                                               fraction.time.on.adap.full.pay.from.full.pay.only * 
                                               proportion.F.only.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.without.cost.sharing',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing *
                                               fraction.time.on.adap.full.pay.from.full.pay.and.premium.without.cost.sharing *
                                               proportion.FP.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.premium.and.cost.sharing',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing *
                                               fraction.time.on.adap.full.pay.from.full.pay.and.premium.and.cost.sharing *
                                               proportion.FPCs.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.full.pay.from.full.pay.and.cost.sharing.without.premium',
                        value = expression(baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium *
                                               fraction.time.on.adap.full.pay.from.full.pay.and.cost.sharing.without.premium *
                                               proportion.adap.full.pay.and.cost.sharing.without.premium.suppressed
                        ))

#-- Suppression Among Premium WithOUT Cost-Sharing --#

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.premium.without.cost.sharing',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.without.cost.sharing.who.lose.eligibility +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.premium.eligibility
                                               ))
)


register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.change.adap.premium.without.cost.sharing.to.full.pay.without.formulary.change',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay *
                                                    (1-proportion.adap.change.to.full.pay.clients.with.formulary.change) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay *
                                                    (1-proportion.adap.change.to.full.pay.clients.with.formulary.change)
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.change.adap.premium.without.copay.to.full.pay.and.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay *
                                                    proportion.adap.change.to.full.pay.clients.with.formulary.change +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay *
                                                    proportion.adap.change.to.full.pay.clients.with.formulary.change
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.premium.without.cost.sharing.unchanged',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.without.full.pay *
                                                    (1-proportion.adap.premium.without.cost.sharing.who.lose.eligibility-proportion.adap.premium.without.cost.sharing.who.change.eligibility.to.full.pay) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.with.full.pay *
                                                    (1-proportion.adap.full.pay.and.premium.without.cost.sharing.who.lose.premium.eligibility-proportion.adap.full.pay.and.premium.without.cost.sharing.who.change.eligibility.to.full.pay)
                                               ))
)



# Premium without cost-sharing suppression components
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.without.full.pay',
                        value = expression(baseline.p.of.adap.with.premium.without.cost.sharing *
                                               fraction.time.on.adap.premium.without.cost.sharing.from.without.full.pay *
                                               proportion.P.only.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.without.cost.sharing.from.with.full.pay',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.without.cost.sharing *
                                               fraction.time.on.adap.premium.without.cost.sharing.from.with.full.pay *
                                               proportion.FP.suppressed
                        ))


#-- Suppression Among Premium AND Cost-Sharing --#

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.premium.and.cost.sharing',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.and.cost.sharing.who.lose.eligibility +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.and.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay *
                                                    proportion.adap.change.to.full.pay.clients.with.formulary.change +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay *
                                                    proportion.adap.change.to.full.pay.clients.with.formulary.change
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.without.formulary.change',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay *
                                                    (1 - proportion.adap.change.to.full.pay.clients.with.formulary.change) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay *
                                                    (1 - proportion.adap.change.to.full.pay.clients.with.formulary.change)
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.premium.without.cost.sharing',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.premium.and.copay.but.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    (1 - proportion.adap.premium.and.cost.sharing.who.lose.eligibility -
                                                         proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay -
                                                         proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing) *
                                                    proportion.PCs.clients.with.Cp *
                                                    proportion.PCp.clients.with.formulary.change +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    (1 - proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility -
                                                         proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay -
                                                         proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing) *
                                                    proportion.FPCs.clients.with.Cp *
                                                    proportion.PCp.clients.with.formulary.change
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.premium.and.cost.sharing.unchanged',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay *
                                                    (1 - proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.eligibility -
                                                         proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.full.pay -
                                                         proportion.adap.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing) *
                                                    (1 - proportion.PCs.clients.with.Cp * proportion.PCp.clients.with.formulary.change) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay *
                                                    (1 - proportion.adap.full.pay.and.premium.and.cost.sharing.who.lose.premium.and.cost.sharing.eligibility -
                                                         proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.full.pay -
                                                         proportion.adap.full.pay.and.premium.and.cost.sharing.who.change.eligibility.to.premium.without.cost.sharing) *
                                                    (1 - proportion.FPCs.clients.with.Cp * proportion.PCp.clients.with.formulary.change)
                                               ))
)



 


# Premium and cost-sharing suppression components
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.without.full.pay',
                        value = expression(baseline.p.of.adap.with.premium.and.cost.sharing *
                                               fraction.time.on.adap.premium.and.cost.sharing.from.without.full.pay *
                                               proportion.PCs.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.premium.and.cost.sharing.from.with.full.pay',
                        value = expression(baseline.p.of.adap.with.full.pay.and.premium.and.cost.sharing *
                                               fraction.time.on.adap.premium.and.cost.sharing.from.with.full.pay *
                                               proportion.FPCs.suppressed
                        ))


#-- Suppression Among Cost-Sharing withOUT Premium --#

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.cost.sharing.without.premium',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.without.full.pay *
                                                    proportion.adap.cost.sharing.without.premium.who.lose.eligibility +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.with.full.pay *
                                                    proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.copay.without.premium.but.change.formulary',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.without.full.pay *
                                                    (1-proportion.adap.cost.sharing.without.premium.who.lose.eligibility) *
                                                    proportion.Cs.clients.with.Cp *
                                                    proportion.Cp.clients.with.formulary.change +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.with.full.pay *
                                                    (1-proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility) *
                                                    proportion.FPCs.clients.with.Cp *
                                                    proportion.adap.copay.and.premium.clients.with.formulary.change
                                               ))
)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.cost.sharing.without.premium.unchanged',
                        value = expression(baseline.proportion.pwh.with.adap * 
                                               (proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.without.full.pay *
                                                    (1-proportion.adap.cost.sharing.without.premium.who.lose.eligibility) *
                                                    (1 - proportion.Cs.clients.with.Cp * proportion.Cp.clients.with.formulary.change) +
                                                proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.with.full.pay *
                                                    (1-proportion.adap.full.pay.and.cost.sharing.without.premium.who.lose.cost.sharing.eligibility) *
                                                    (1 - proportion.FPCs.clients.with.Cp * proportion.adap.copay.and.premium.clients.with.formulary.change)
                                               ))
)


# Cost-sharing without premium suppression components
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.without.full.pay',
                        value = expression(baseline.p.of.adap.with.cost.sharing.without.premium *
                                               fraction.time.on.adap.cost.sharing.without.premium.from.without.full.pay *
                                               proportion.Cs.only.suppressed
                        ))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.of.adap.who.are.suppressed.with.baseline.adap.cost.sharing.without.premium.from.with.full.pay',
                        value = expression(baseline.p.of.adap.with.full.pay.and.cost.sharing.without.premium *
                                               fraction.time.on.adap.cost.sharing.without.premium.from.with.full.pay *
                                               proportion.adap.full.pay.and.cost.sharing.without.premium.suppressed
                        ))


#-- Suppression without any ADAP --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.without.adap',
                        value = expression(
                                super.suppression.of.diagnosed - 
                                
                                proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged -
                                proportion.pwh.who.are.suppressed.with.adap.premium.without.cost.sharing.unchanged -
                                proportion.pwh.who.are.suppressed.with.adap.premium.and.cost.sharing.unchanged -
                                proportion.pwh.who.are.suppressed.with.adap.cost.sharing.without.premium.unchanged -
                                
                                # Lose ADAP
                                proportion.pwh.who.are.suppressed.and.lose.adap.full.pay -
                                proportion.pwh.who.are.suppressed.and.lose.adap.premium.without.cost.sharing -
                                proportion.pwh.who.are.suppressed.and.lose.adap.premium.and.cost.sharing - 
                                proportion.pwh.who.are.suppressed.and.lose.adap.cost.sharing.without.premium -

                                # Change ADAP
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.without.cost.sharing.to.full.pay.without.formulary.change -
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.without.formulary.change -
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.premium.without.cost.sharing -

                                # Formulary Change
                                proportion.pwh.who.are.suppressed.and.keep.adap.full.pay.but.change.formulary -
                                proportion.pwh.who.are.suppressed.and.keep.adap.premium.and.copay.but.change.formulary - 
                                proportion.pwh.who.are.suppressed.and.keep.adap.copay.without.premium.but.change.formulary -
                                
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.without.copay.to.full.pay.and.change.formulary -
                                proportion.pwh.who.are.suppressed.and.change.adap.premium.and.cost.sharing.to.full.pay.and.change.formulary
                                
                        ))







## UP TO HERE




calculate.baseline.adap.full.pay.income.proportions.single.fpl <- function(adap.income.proportions.single.fpl,
                                                                           p.full.pay.given.income.midpoint,
                                                                           p.full.pay.given.income.logistic.slope,
                                                                           p.full.pay.given.income.min,
                                                                           p.full.pay.given.income.max,
                                                                           baseline.adap.full.pay.fpl.threshold,
                                                                           baseline.adap.insurance.fpl.threshold)
{
    max.income = max(baseline.adap.full.pay.fpl.threshold, baseline.adap.insurance.fpl.threshold)
    income = 0:max.income
    
    logistic.p = p.full.pay.given.income.min + 
        (p.full.pay.given.income.max - p.full.pay.given.income.min) /
        (1 + exp(p.full.pay.given.income.logistic.slope * (income - p.full.pay.given.income.midpoint)))
    
    non.income.dimensions = setdiff(names(dim(adap.income.proportions.single.fpl)), 'income')
    p = apply(adap.income.proportions.single.fpl, non.income.dimensions, function(prop){
        prop * logistic.p
    })
    
    dim(p) = dim(adap.income.proportions.single.fpl)
    dimnames(p) = dimnames(adap.income.proportions.single.fpl)
    
    p
}

calculate.baseline.proportion.pwh.with.adap.full.pay <- function(baseline.proportion.pwh.with.adap,
                                                                 adap.income.proportions.single.fpl)
{
    non.income.dimensions = setdiff(names(dim(adap.income.proportions.single.fpl)), 'income')
    apply(adap.income.proportions.single.fpl, non.income.dimensions, sum) * baseline.proportion.pwh.with.adap
}

calculate.baseline.proportion.above.new.fpl.threshold <- function(baseline.income.proportions.single.fpl,
                                                                  fpl.threshold)
{
    fpl.threshold = floor(fpl.threshold)
    n.income.values = dim(baseline.income.proportions.single.fpl)['income']
    
    non.income.dimensions = setdiff(names(dim(baseline.income.proportions.single.fpl)), 'income')
    if (fpl.threshold >= n.income.values)
    {
        array(1, dim=dim(baseline.income.proportions.single.fpl)[non.income.dimensions],
              dimnames = dimnames(baseline.adap.full.pay.income.proportions.single.fpl)[non.income.dimensions])
    }         
    else
    {
        above.threshold.indices = (fpl.threshold+1):n.income.values
        
        rv = apply(baseline.income.proportions.single.fpl, non.income.dimensions, function(x){
            sum(x[above.threshold.indices])
        }) / apply(baseline.income.proportions.single.fpl, non.income.dimensions, sum)    
    }
}

calculate.baseline.proportion.adap.full.pay.clients.above.new.fpl.threshold <- function(baseline.adap.full.pay.income.proportions.single.fpl,
                                                                                        adap.full.pay.fpl.threshold)
{
    calculate.baseline.proportion.above.new.fpl.threshold(
        baseline.income.proportions.single.fpl = baseline.adap.full.pay.income.proportions.single.fpl,
        fpl.threshold = adap.full.pay.fpl.threshold)
}

calculate.baseline.proportion.adap.insurance.clients.above.new.fpl.threshold <- function(baseline.adap.insurance.income.proportions.single.fpl,
                                                                                         adap.insurance.fpl.threshold)
{
    calculate.baseline.proportion.above.new.fpl.threshold(
        baseline.income.proportions.single.fpl = baseline.adap.insurance.income.proportions.single.fpl,
        fpl.threshold = adap.insurance.fpl.threshold)
}

##---------------------------##
##-- CALCULATED: P on ADAP --##
##---------------------------##

#-- Get proportions of ADAP clients in each stratum of FPL (single percentage point) and full pay vs insurance
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'adap.income.proportions.single.fpl',
                        value = calculate.adap.income.proportions.single.fpl)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.adap.full.pay.income.proportions.single.fpl',
                        value = calculate.baseline.adap.full.pay.income.proportions.single.fpl)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.adap.insurance.income.proportions.single.fpl',
                        value = expression(adap.income.proportions.single.fpl - baseline.adap.full.pay.income.proportions.single.fpl))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.adap.copay.assistance.income.proportions.single.fpl',
                        value = 0.5) #@todo

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.adap.insurance.income.proportions.single.fpl',
                        value = expression(baseline.adap.insurance.income.proportions.single.fpl - baseline.adap.copay.assistance.income.proportions.single.fpl))


#-- Get the above proportions, but of all PWH --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.proportion.pwh.with.adap.full.pay',
                        value = calculate.baseline.proportion.pwh.with.adap.full.pay)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'baseline.proportion.pwh.with.adap.insurance',
                        value = expression(baseline.proportion.pwh.with.adap - baseline.proportion.pwh.with.adap.full.pay))

#-- Figure out what proportion of adap clients are above new threshold --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.full.pay.clients.above.new.fpl.threshold',
                        value = calculate.baseline.proportion.adap.full.pay.clients.above.new.fpl.threshold)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.insurance.clients.above.new.fpl.threshold',
                        value = calculate.baseline.proportion.adap.insurance.clients.above.new.fpl.threshold)



#-- Apply to calculate the time-updates proportion on adap full pay --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.with.adap.full.pay',
                        value = expression(baseline.proportion.pwh.with.adap.full.pay * (1-proportion.adap.full.pay.clients.above.new.fpl.threshold)),
                        scale = 'proportion')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.with.adap.insurance',
                        value = expression(baseline.proportion.pwh.with.adap.insurance * (1-proportion.adap.insurance.clients.above.new.fpl.threshold)),
                        scale = 'proportion')

#-- The proportion with copay --#
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.adap.insurance.clients.with.copay.assistance',
                        value = 'p.copay.assistance.given.adap.insurance', # for now, this is a 1:1 mapping of the parameter. But we may want to change this to be a function of income
                        scale = 'proportion') 



##-- PARTITION OUT ADAP --##















##-- ADAP SUPPRESSION CALCULATED QUANTITIES --##

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged',
                        value = expression(proportion.pwh.with.adap.full.pay * # This is calculated as baseline.proportion.pwh.with.adap.full.pay * (1-proportion.adap.full.pay.clients.above.new.fpl.threshold)
                                               (1-proportion.adap.full.pay.or.copay.assistance.clients.with.formulary.change) * 
                                               proportion.adap.full.pay.suppressed))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged',
                        value = expression(proportion.pwh.with.adap.insurance * # This is calculated as baseline.proportion.pwh.with.adap.insurance * (1-proportion.adap.insurance.clients.above.new.fpl.threshold)
                                               (1 - proportion.adap.insurance.clients.with.copay.assistance * proportion.adap.full.pay.or.copay.assistance.clients.with.formulary.change) * 
                                               proportion.adap.insurance.suppressed))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.full.pay',
                        value = expression(baseline.proportion.pwh.with.adap.full.pay * proportion.adap.full.pay.clients.above.new.fpl.threshold),
                        scale = 'proportion')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.lose.adap.insurance',
                        value = expression(baseline.proportion.pwh.with.adap.insurance * proportion.adap.insurance.clients.above.new.fpl.threshold),
                        scale = 'proportion')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.and.keep.adap.but.change.formulary',
                        value = expression(proportion.adap.full.pay.or.copay.assistance.clients.with.formulary.change *
                                               (proportion.pwh.with.adap.full.pay * proportion.adap.full.pay.suppressed +
                                                    proportion.pwh.with.adap.insurance * proportion.adap.insurance.clients.with.copay.assistance * proportion.adap.insurance.suppressed)),
                        scale = 'proportion')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.without.adap',
                        value = expression(super.suppression.of.diagnosed - 
                                               
                                               proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged -
                                               proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged -
                                               
                                               proportion.pwh.who.are.suppressed.and.lose.adap.full.pay - 
                                               proportion.pwh.who.are.suppressed.and.lose.adap.insurance - 
                                               
                                               proportion.pwh.who.are.suppressed.and.keep.adap.but.change.formulary
                        ))


##-------------------------------------------##
##-- CALCULATED: TO SUPPORT INCOME OUTPUTS --##
##-------------------------------------------##

calculate.adap.full.pay.income.distribution <- function(baseline.adap.full.pay.income.proportions.single.fpl,
                                                        adap.full.pay.fpl.threshold,
                                                        cutpoints = c(0, 100, 138, 200, 250, 300, 400, 500, Inf))
{
    calculate.income.distribution(income.proportions.single.fpl = baseline.adap.full.pay.income.proportions.single.fpl,
                                  cutpoints = cutpoints,
                                  fpl.threshold = adap.full.pay.fpl.threshold)
}

calculate.income.distribution <- function(income.proportions.single.fpl,
                                          fpl.threshold,
                                          cutpoints = c(0, 100, 138, 200, 250, 300, 400, 500, Inf))
{
    n.incomes = dim(income.proportions.single.fpl)['income']
    fpl.threshold = floor(fpl.threshold)
    fpl.threshold.index = min(fpl.threshold+1, n.incomes)
    non.income.dimensions = setdiff(names(dim(income.proportions.single.fpl)), 'income')
    
    lower.indices = cutpoints[-length(cutpoints)] + 1
    lower.indices[-1] = lower.indices[-1]+1
    lower.indices = pmin(fpl.threshold.index, lower.indices)
    
    upper.indices = pmin(fpl.threshold.index, cutpoints[-1]+1)
    
    rv = apply(income.proportions.single.fpl, non.income.dimensions, function(income.proportions){
        
        total.below.threshold = sum(income.proportions[1:fpl.threshold.index])
        
        sapply(1:length(lower.indices), function(i){
            
            if (lower.indices[i]==upper.indices[i])
                0
            else
                sum(income.proportions[lower.indices[i]:upper.indices[i]]) / total.below.threshold
        })
    })
    
    income.names = paste0(lower.indices-1, "-", upper.indices-1)
    if (cutpoints[length(cutpoints)]==Inf)
        income.names[length(income.names)] = paste0(">", lower.indices[length(lower.indices)]-1)
    
    dim.names = c(
        list(income=income.names),
        dimnames(income.proportions.single.fpl)[non.income.dimensions])
    
    dim(rv) = vapply(dim.names, length, FUN.VALUE=integer(1))
    dimnames(rv) = dim.names
    
    rv
}

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'general.fpl.median',
                        value = expression(adap.fpl.median * general.over.adap.fpl.median.multiplier), 
                        scale = 'non.negative.number')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'general.fpl.cv',
                        value = expression(adap.fpl.cv * general.over.adap.fpl.cv.multiplier), 
                        scale = 'non.negative.number')

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'adap.income.and.service.type.distribution',
                        value = 0)

register.model.quantity.subset(ADAP.SPECIFICATION,
                               name = 'adap.income.and.service.type.distribution',
                               applies.to = list(service.type='any.full.pay'),
                               value = 0.5) #@todo



register.model.quantity(ADAP.SPECIFICATION,
                        name = 'adap.full.pay.income.distribution',
                        value = calculate.adap.full.pay.income.distribution)

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.with.adap.full.pay.by.income',
                        value = expression(adap.full.pay.income.distribution * proportion.pwh.with.adap.full.pay))


##--------------##
##--------------##
##-- OUTCOMES --##
##--------------##
##--------------##


# to calibrate:
# adap clients
#   - by demographics
#   - by income
# adap income distribution
#   - total
# adap suppression
#   - total
#   - by service type

##-------------------##
##-- ADAP OUTCOMES --##
##-------------------##

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Clients',
                                                                    description = "Number of Individuals Receiving ADAP Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap.by.service.and.income',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         allow.expand.dimensions = c('income','service.type'),
                         keep.dimensions = c('location','age','race','sex','risk','service.type','income'),
                         corresponding.data.outcome = 'adap.full.pay.clients',
                         save = T)


track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'adap.clients.all.incomes',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value = 'adap.clients',
                         exclude.dimensions = 'income',
                         save = F)

track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'adap.income.distribution',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Income Distribution',
                                                                    description = "Proportions of ADAP Clients by Income",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'adap.clients',
                         value.is.numerator = T,
                         denominator.outcome = 'adap.clients.all.incomes',
                         allow.expand.denominator.dimensions = 'income',
                         keep.dimensions = c('location','age','race','sex','risk','service.type','income'),
                         corresponding.data.outcome = 'adap.income.distribution',
                         save = T)

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among ADAP Clients',
                                                                    description = "Proportion of ADAP Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged.by.service',
                         denominator.outcome = 'adap.insurance.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk','service.type'),
                         corresponding.data.outcome = 'adap.suppression',
                         save = T)



##-- INCOME OUTCOMES --##

# track.point.outcome(ADAP.SPECIFICATION,
#                     name = 'point.general.fpl.100', 
#                     value = expression(plnorm(100, general.fpl.log.mean, general.fpl.log.sd)), 
#                     outcome.metadata = NULL,
#                     save = F,
#                     scale = 'non.negative.number',
#                     keep.dimensions = c('location','age','race','sex'))
# 
# 
# track.integrated.outcome(ADAP.SPECIFICATION,
#                          name = 'general.fpl.100',
#                          outcome.metadata = create.outcome.metadata(display.name = 'Proportion Population <100% FPL',
#                                                                     description = "Proportion of the General Population Living Under 100% of Federal Poverty Level",
#                                                                     scale = 'proportion',
#                                                                     axis.name = 'Proportion <100% FPL',
#                                                                     units = '%',
#                                                                     singular.unit = '%'),
#                          value.to.integrate = 'point.general.fpl.100', 
#                          denominator.outcome = 'population',
#                          keep.dimensions = c('location','age','race','sex'))
# 
# 
# track.point.outcome(ADAP.SPECIFICATION,
#                     name = 'point.adap.fpl.100', 
#                     value = expression(plnorm(100, adap.fpl.log.mean, adap.fpl.log.sd)), 
#                     outcome.metadata = NULL,
#                     save = F,
#                     scale = 'non.negative.number',
#                     keep.dimensions = c('location','age','race','sex'))
# 
# 
# track.integrated.outcome(ADAP.SPECIFICATION,
#                          name = 'adap.fpl.100',
#                          outcome.metadata = create.outcome.metadata(display.name = 'Proportion of ADAP Clients <100% FPL',
#                                                                     description = "Proportion of ADAP Clients Living Under 100% of Federal Poverty Level",
#                                                                     scale = 'proportion',
#                                                                     axis.name = 'Proportion <100% FPL',
#                                                                     units = '%',
#                                                                     singular.unit = '%'),
#                          value.to.integrate = 'point.adap.fpl.100', 
#                          denominator.outcome = 'adap.clients',
#                          keep.dimensions = c('location','age','race','sex'))

##--------------##
##-- REGISTER --##
##--------------##

register.model.specification(ADAP.SPECIFICATION)
# register.set.parameters.for.version('rw',
#                                     parameter.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
#                                     apply.function = ryan.white.apply.set.parameters,
#                                     join.with.previous.version = T)
