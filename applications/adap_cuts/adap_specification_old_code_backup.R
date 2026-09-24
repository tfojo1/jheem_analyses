

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
                        name = 'proportion.pwh.who.are.suppressed.F.unchanged',
                        value = expression(proportion.pwh.with.adap.full.pay * # This is calculated as baseline.proportion.pwh.with.adap.full.pay * (1-proportion.adap.full.pay.clients.above.new.fpl.threshold)
                                               (1-proportion.adap.full.pay.or.copay.assistance.clients.with.formulary.change) * 
                                               proportion.adap.full.pay.suppressed))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged',
                        value = expression(proportion.pwh.with.adap.insurance * # This is calculated as baseline.proportion.pwh.with.adap.insurance * (1-proportion.adap.insurance.clients.above.new.fpl.threshold)
                                               (1 - proportion.adap.insurance.clients.with.copay.assistance * proportion.adap.full.pay.or.copay.assistance.clients.with.formulary.change) * 
                                               proportion.adap.insurance.suppressed))

register.model.quantity(ADAP.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.lose.F',
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
                                               
                                               proportion.pwh.who.are.suppressed.F.unchanged -
                                               proportion.pwh.who.are.suppressed.with.adap.insurance.unchanged -
                                               
                                               proportion.pwh.who.are.suppressed.lose.F - 
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
