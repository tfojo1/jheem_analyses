source('../jheem_analyses/applications/EHE/ehe_specification.R')

ADAP.SPECIFICATION = create.jheem.specification(version='adap',
                                              iteration = '1',
                                              description='Model to study the impacts of cuts to ADAP on HIV transmission',
                                              parent.version = 'ehe',
                                              compartments.for.outcomes = list(income=c('0-100', '101-138', '139-200', '201-250', '251-300','301-400','401-500','>500')))





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
                        name = "adap.insurance.fpl.threshold",
                        value = 'baseline.adap.insurance.fpl.threshold',
                        scale = 'non.negative.number')


##-- INPUTS: EFFECTS of CUTS--##
register.model.element(ADAP.SPECIFICATION,
                       name = "lose.adap.full.pay.suppression.rr",
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "lose.adap.insurance.suppression.rr",
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "change.adap.formulary.suppression.rr",
                       value = 0.5,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = "proportion.adap.clients.with.formulary.change",
                       value = 0,
                       scale = 'proportion')

####----------------------------------####
####----------------------------------####
####-- INPUTS for SETUP/CALIBRATION --####
####----------------------------------####
####----------------------------------####

##-------------------##
##-- INPUT HELPERS --##
##-------------------##


get.proportion.with.adap.functional.form <- function()
{
     get.cached.object.for.version(name = "p.adap.functional.form", version = 'rw')
}

get.proportion.adap.full.pay.suppressed.functional.form <- function()
{
    get.cached.object.for.version(name = "p.suppression.oahs.functional.form", version = 'rw')
}

get.proportion.adap.insurance.suppressed.functional.form <- function()
{
    get.cached.object.for.version(name = "p.suppression.oahs.functional.form", version = 'rw')
}


##-----------------------------##
##-- INPUTS: ADAP THRESHOLDS --##
##-----------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.full.pay.fpl.threshold",
                       value = 500,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = "baseline.adap.insurance.fpl.threshold",
                       value = 500,
                       scale = 'non.negative.number')


##--------------------##
##-- INPUTS: INCOME --##
##--------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = 'adap.fpl.median',
                       functional.form = create.static.functional.form(value = array(200,
                                                                                     dim = c(age=5, race=3, sex=3, risk=3),
                                                                                     dimnames = list(age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                                                                     race=c('black','hispanic','other'),
                                                                                                     sex=c('heterosexual_male','msm','female'),
                                                                                                     risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'))),
                                                                       link = 'identity',
                                                                       value.is.on.transformed.scale = F), 
                       scale = 'number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'adap.fpl.cv',
                       functional.form = create.static.functional.form(value = array(0.5,
                                                                                     dim = c(age=5, race=3, sex=3, risk=3),
                                                                                     dimnames = list(age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
                                                                                                     race=c('black','hispanic','other'),
                                                                                                     sex=c('heterosexual_male','msm','female'),
                                                                                                     risk=c('never_IDU', 'active_IDU', 'IDU_in_remission'))),
                                                                       link = 'log',
                                                                       value.is.on.transformed.scale = F),
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'general.over.adap.fpl.median.multiplier',
                       functional.form = create.static.functional.form(value = 1,
                                                                       link = 'log',
                                                                       value.is.on.transformed.scale = T), 
                       scale = 'ratio')

register.model.element(ADAP.SPECIFICATION,
                       name = 'general.over.adap.fpl.cv.multiplier',
                       functional.form = create.static.functional.form(value = 1,
                                                                       link = 'log',
                                                                       value.is.on.transformed.scale = F),
                       scale = 'ratio')



register.model.element(ADAP.SPECIFICATION,
                       name = 'ssi.benefit.fpl',
                       value = 995*12 / 15960,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'ssi.breakeven.fpl',
                       value = 2073*12 / 15960,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.ssi.if.income.eligible',
                       value = 587 / (1106+449),
                       scale = 'non.negative.number')


##-----------------------##
##-- INPUTS: P on ADAP --##
##-----------------------##


register.model.element(ADAP.SPECIFICATION,
                       name = 'baseline.proportion.pwh.with.adap',
                       scale = 'proportion',
                       functional.form = get.proportion.with.adap.functional.form(),
                       functional.form.from.time = 2010)


##---------------------------------##
##-- INPUTS: SUPPRESSION on ADAP --##
##---------------------------------##

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.adap.full.pay.suppressed',
                       scale = 'proportion',
                       functional.form = get.proportion.adap.full.pay.suppressed.functional.form(),
                       functional.form.from.time = 2010)

register.model.element(ADAP.SPECIFICATION,
                       name = 'proportion.adap.insurance.suppressed',
                       scale = 'proportion',
                       functional.form = get.proportion.adap.insurance.suppressed.functional.form(),
                       functional.form.from.time = 2010)


##-------------------------------------##
##-- INPUTS: P FULL-PAY given INCOME --##
##-------------------------------------##

# we presume that: p_full_pay = min + (max-min) / (1 + exp(slope * (income - midpoint)))
# *NB that slope here is constrained to be positive - ie, a strictly decreasing p with increasing income

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.given.income.midpoint',
                       value = 250,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.given.income.logistic.slope',
                       value = 0.05,
                       scale = 'non.negative.number')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.given.income.min',
                       value = 0.2,
                       scale = 'proportion')

register.model.element(ADAP.SPECIFICATION,
                       name = 'p.full.pay.given.income.max',
                       value = 0.95,
                       scale = 'proportion')


####---------------------------####
####---------------------------####
####-- CALCULATED QUANTITIES --####
####---------------------------####
####---------------------------####



##------------------------##
##-- CALCULATED: INCOME --##
##------------------------##

calculate.adap.income.proportions.single.fpl <- function(adap.fpl.median,
                                                         adap.fpl.cv,
                                                         p.ssi.if.income.eligible,
                                                         ssi.benefit.fpl,
                                                         ssi.breakeven.fpl,
                                                         baseline.adap.full.pay.fpl.threshold,
                                                         baseline.adap.insurance.fpl.threshold)
{
    max.income = max(baseline.adap.full.pay.fpl.threshold, baseline.adap.insurance.fpl.threshold)
    income = 0:max.income
    
    adap.fpl.sd = adap.fpl.median * adap.fpl.cv
    multiply.p.by = 1 / pnorm(max.income, adap.fpl.median, adap.fpl.sd)
        
    p = sapply(income, function(inc){
        
        if (inc >= ssi.breakeven.fpl)
            pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        else if (inc < ssi.benefit.fpl)
            (1-p.ssi.if.income.eligible) * pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        else
        {
            what.inc.would.be.without.ssi = (inc - ssi.benefit.fpl) * ssi.breakeven.fpl  / (ssi.breakeven.fpl - ssi.benefit.fpl)
            
            p.ssi.if.income.eligible * pnorm(what.inc.would.be.without.ssi, adap.fpl.median, adap.fpl.sd) * multiply.p.by +
            (1-p.ssi.if.income.eligible) * pnorm(inc, adap.fpl.median, adap.fpl.sd) * multiply.p.by
        }
        
    })
    dim(p) = c(length(p)/length(income), length(income))
        # dimensions of p are [stratum, income]
    
    p[,-1] = p[,-1] - p[,-dim(p)[2]]
    
    dim(p) = c(dim(adap.fpl.median), income=length(income))
    dimnames(p) = c(dimnames(adap.fpl.median), list(income = income))
    
    p
}

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


##-----------------------------##
##-- CALCULATED: SUPPRESSION --##
##-----------------------------##


##-- TIE-IN TO MAIN SUPPRESSION --##
register.model.quantity(ADAP.SPECIFICATION,
                        name = 'suppression.of.diagnosed',
                        value = expression(proportion.pwh.who.are.suppressed.without.adap +
                                               
                                               proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged +
                                               proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged +
                                               
                                               proportion.pwh.who.are.suppressed.and.lose.adap.full.pay * lose.adap.full.pay.suppression.rr +
                                               proportion.pwh.who.are.suppressed.and.lose.adap.insurance * lose.adap.insurance.suppression.rr +
                                               
                                               proportion.pwh.who.are.suppressed.and.keep.adap.but.change.formulary * change.adap.formulary.suppression.rr)
)


##-- ADAP SUPPRESSION CALCULATED QUANTITIES --##

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged',
                        value = expression(proportion.pwh.with.adap.full.pay * # This is calculated as baseline.proportion.pwh.with.adap.full.pay * (1-proportion.adap.full.pay.clients.above.new.fpl.threshold)
                                               (1-proportion.adap.clients.with.formulary.change) * 
                                               proportion.adap.full.pay.suppressed))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged',
                        value = expression(proportion.pwh.with.adap.insurance * # This is calculated as baseline.proportion.pwh.with.adap.insurance * (1-proportion.adap.insurance.clients.above.new.fpl.threshold)
                                               (1-proportion.adap.clients.with.formulary.change) * 
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
                        value = expression(proportion.adap.clients.with.formulary.change *
                                               (proportion.pwh.with.adap.full.pay * proportion.adap.full.pay.suppressed +
                                                    proportion.pwh.with.adap.insurance * proportion.adap.insurance.suppressed)),
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

##-- ADAP OUTCOMES --##

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.full.pay.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Full-Pay Clients',
                                                                    description = "Number of Individuals Receiving ADAP Full-Pay Medication Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap.full.pay.by.income',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         allow.expand.dimensions = 'income',
                         keep.dimensions = c('location','age','race','sex','risk','income'),
                         corresponding.data.outcome = 'adap.full.pay.clients',
                         save = T)

track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'total.adap.full.pay.clients',
                         scale = 'non.negative.number',
                         value = 'adap.full.pay.clients',
                         exclude.dimensions = 'income',
                         save = F)

track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'adap.full.pay.income.distribution',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Full-Pay Income Distribution',
                                                                    description = "Number of Individuals Receiving ADAP Full-Pay Medication Services",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value = 'adap.full.pay.clients',
                         value.is.numerator = T,
                         denominator.outcome = 'total.adap.full.pay.clients',
                         allow.expand.denominator.dimensions = 'income',
                         keep.dimensions = c('location','age','race','sex','risk','income'),
                         corresponding.data.outcome = 'adap.full.pay.clients',
                         save = T)

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.insurance.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP-Funded Insurance Clients',
                                                                    description = "Number of Individuals Receiving ADAP-Funded Insurance",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap.insurance',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.insurance.clients',
                         save = T)


track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'adap.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Clients',
                                                                    description = "Number of Individuals Receiving any AIDS Drug Assistance Program through Ryan White",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value = expression(adap.full.pay.clients + adap.insurance.clients),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.clients',
                         save = T)

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.full.pay.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among ADAP Full-Pay Clients',
                                                                    description = "Proportion of ADAP Full-Pay Medication Program Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.adap.full.pay.unchanged',
                         denominator.outcome = 'adap.full.pay.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.full.pay.suppression',
                         save = T)

track.integrated.outcome(ADAP.SPECIFICATION,
                         name = 'adap.insurance.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among ADAP-Funded Insurance Clients',
                                                                    description = "Proportion of ADAP-Funded Insurance Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged',
                         denominator.outcome = 'adap.insurance.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.insurance.suppression',
                         save = T)


track.cumulative.outcome(ADAP.SPECIFICATION,
                         name = 'adap.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among ADAP Clients',
                                                                    description = "Proportion of Ryan White AIDS Drug Assistance Program Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(adap.full.pay.suppression * adap.full.pay.clients + adap.insurance.suppression * adap.insurance.clients),
                         denominator.outcome = 'adap.clients',
                         value.is.numerator = T,
                         keep.dimensions = c('location','age','race','sex','risk'),
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
