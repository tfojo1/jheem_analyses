

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

get.state.adap.allows.medicare.and.medicaid <- function(location)
{
    # From https://nastad.org/sites/default/files/2026-02/2026-adap-report-table-10.pdf
    # States with 0% on medicaid and medicare
    DISALLOW.MEDICARE.MEDICAID.STATES = c('AL','AK', 'CT','GA')
    
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

get.state.baseline.adap.premium.threshold <- function(location)
{
    get.state.baseline.adap.full.pay.threshold(location) # practically, there are no states (right now) that have different thresholds
}

get.state.baseline.adap.cost.sharing.threshold <- function(location)
{
    get.state.baseline.adap.full.pay.threshold(location) # practically, there are no states (right now) that have different thresholds
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

# placeholder for now 
get.p.ssi.without.medicaid = function(){
    create.static.functional.form(value = .05)
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
get.p.medicare.if.not.by.age.if.benefits.eligible.functional.form <- function()
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

# placeholder for now
get.p.medicaid.not.by.income.or.ssi.if.medicare.functional.form <- function()
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

# placeholder for now
get.p.medicaid.not.by.income.or.ssi.if.no.medicare.and.benefits.eligible.functional.form <- function()
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

# Melissa: we will eventually fill this in 
get.proportion.Cs.clients.with.Cp.functional.form = function(){
    create.static.functional.form(value = 0.5)
}


####---------------------------####
####---------------------------####
####-- CALCULATED QUANTITIES --####
####---------------------------####
####---------------------------####

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

calculate.baseline.p.of.income.with.ssi.among.adap <- function(adap.fpl.median,
                                                               adap.fpl.cv,
                                                               p.ssi.if.income.eligible,
                                                               ssi.benefit.fpl,
                                                               ssi.breakeven.fpl,
                                                               max.adap.baseline.income)
{
    income = 0:max.adap.baseline.income
    
    adap.fpl.sd = adap.fpl.median * adap.fpl.cv
    multiply.p.by = 1 / pnorm(max.adap.baseline.income, adap.fpl.median, adap.fpl.sd)
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

calculate.baseline.p.of.income.without.ssi.among.adap <- function(adap.fpl.median,
                                                                  adap.fpl.cv,
                                                                  p.ssi.if.income.eligible,
                                                                  ssi.benefit.fpl,
                                                                  ssi.breakeven.fpl,
                                                                  max.adap.baseline.income)
{
    income = 0:max.adap.baseline.income
    
    adap.fpl.sd = adap.fpl.median * adap.fpl.cv
    multiply.p.by = 1 / pnorm(max.adap.baseline.income, adap.fpl.median, adap.fpl.sd)
    
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

calculate.income.is.below.medicaid.threshold = function(adap.incomes,
                                                        medicaid.fpl.threshold){
    
    array(as.numeric(adap.incomes)<=medicaid.fpl.threshold,
          dim = c(income = length(adap.incomes)),
          dimnames = list(income = adap.incomes))
}

calculate.p.uninsured.given.income.and.no.public.insurance <- function(p.uninsured.given.income.and.no.public.insurance.midpoint,
                                                                       p.uninsured.given.income.and.no.public.insurance.slope,
                                                                       p.uninsured.given.income.and.no.public.insurance.min,
                                                                       p.uninsured.given.income.and.no.public.insurance.max,
                                                                       max.adap.baseline.income)
{
    calculate.logistic.p(logistic.midpoint = p.uninsured.given.income.and.no.public.insurance.midpoint,
                         logistic.slope = p.uninsured.given.income.and.no.public.insurance.slope,
                         min.p = p.uninsured.given.income.and.no.public.insurance.min,
                         max.p = p.uninsured.given.income.and.no.public.insurance.max,
                         max.adap.baseline.income = max.adap.baseline.income)
}

calculate.max.adap.baseline.income <- function(baseline.adap.full.pay.fpl.threshold,
                                               baseline.adap.premium.fpl.threshold,
                                               baseline.adap.cost.sharing.fpl.threshold,
                                               ssi.breakeven.fpl)
{
    max(baseline.adap.full.pay.fpl.threshold, 
        baseline.adap.premium.fpl.threshold,
        baseline.adap.cost.sharing.fpl.threshold,
        ssi.breakeven.fpl)
}

distribute.adap.incomes <- function(max.adap.baseline.income){
    rv = array(0:max.adap.baseline.income,
               dim = c(income = max.adap.baseline.income + 1),
               dimnames = list(income = 0:max.adap.baseline.income))
    
    rv
}


# A helper to generate a set of logistic probabilities
calculate.logistic.p <- function(logistic.midpoint,
                                 logistic.slope,
                                 min.p,
                                 max.p,
                                 max.adap.baseline.income,
                                 additional.or = 1)
{
    income = 0:max.adap.baseline.income
    p = min.p + (max.p - min.p) /
        (1 + exp(logistic.slope * (income - logistic.midpoint) + log(additional.or)))
    
    dim(p) = c(income=length(income))
    dimnames(p) = list(income=income)
    p
}

sum.p.across.income <- function(income.distribution)
{
    non.income.dimensions = setdiff(names(dim(income.distribution)), 'income')
    apply(income.distribution, non.income.dimensions, sum)
}

## Helpers to sum over income 
# 1: F only
calculate.baseline.p.of.F.only.among.adap <- function(baseline.p.of.F.only.income.among.adap) { 
    sum.p.across.income(baseline.p.of.F.only.income.among.adap)
}

# 2: FP  
calculate.baseline.p.of.FP.among.adap <- function(baseline.p.of.FP.income.among.adap) { 
    sum.p.across.income(baseline.p.of.FP.income.among.adap)
}

# 3: FPCs
calculate.baseline.p.of.FPCs.among.adap <- function(baseline.p.of.FPCs.income.among.adap) { 
    sum.p.across.income(baseline.p.of.FPCs.income.among.adap)
}

# 4: PCs
calculate.baseline.p.of.PCs.among.adap <- function(baseline.p.of.PCs.income.among.adap) { 
    sum.p.across.income(baseline.p.of.PCs.income.among.adap)
}

# 5: P only
calculate.baseline.p.of.P.only.among.adap <- function(baseline.p.of.P.only.income.among.adap) { 
    sum.p.across.income(baseline.p.of.P.only.income.among.adap)
}

# 6: FCs 
calculate.baseline.p.of.FCs.among.adap <- function(baseline.p.of.FCs.income.among.adap) { 
    sum.p.across.income(baseline.p.of.FCs.income.among.adap)
}

# 7: Cs only 
calculate.baseline.p.of.Cs.only.among.adap <- function(baseline.p.of.Cs.only.income.among.adap) { 
    sum.p.across.income(baseline.p.of.Cs.only.income.among.adap)
}


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



