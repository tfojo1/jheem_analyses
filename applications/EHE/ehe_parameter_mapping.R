
##------------------------##
##-- THE MAIN FUNCTIONS --##
##------------------------##

EHE.APPLY.PARAMETERS.FN = function(jheem.engine, parameters,
                                   check.consistency = !jheem.engine$has.been.crunched())
{
    specification.metadata = jheem.engine$specification.metadata
    
    if (check.consistency)
        used.parameter.names = character()
    
    #-- Elements with Single Values --#
    used.names = set.element.values.from.parameters(jheem.engine, 
                                                    element.names = NULL,
                                                    parameters = parameters,
                                                    check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
            
    #-- Suppression --#
    used.names = set.ehe.alphas.from.parameters(jheem.engine,
                                                element.name = 'suppression.of.diagnosed',
                                                parameters = parameters,
                                                parameter.suffixes = c(intercept='.suppressed.or', slope='.suppressed.slope.or'),
                                                idu.applies.to.in.remission = F,
                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- Testing --#
    used.names = set.ehe.alphas.from.parameters(jheem.engine,
                                                element.name = 'testing',
                                                parameters = parameters,
                                                parameter.suffixes = c(intercept='.proportion.tested.or', slope='.proportion.tested.slope.or'),
                                                idu.applies.to.in.remission = F,
                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    
    jheem.engine$set.element.ramp.values(element.name = 'testing',
                                         values = parameters['testing.ramp.up.vs.current.rr'] * c(TESTING.FIRST.YEAR.FRACTION.OF.RAMP,1),
                                         indices = 2:3,
                                         check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 'testing.ramp.up.vs.current.rr')
    
    #-- PrEP --#
    
    # parameters for PrEP x age and race - are used for both interecept and slope
    used.names = set.element.functional.form.alphas.from.parameters(jheem.engine = jheem.engine,
                                                                    element.name = 'oral.prep',
                                                                    alpha.name = 'intercept',
                                                                    parameters = parameters,
                                                                    parameter.name.prefix = '',
                                                                    parameter.name.suffix = '.prep.or',
                                                                    dimensions.with.values.referred.to.by.name = 'race',
                                                                    dimensions.with.values.referred.to.by.index = 'age',
                                                                    check.consistency = check.consistency)
    
    used.names = set.element.functional.form.alphas.from.parameters(jheem.engine = jheem.engine,
                                                                    element.name = 'oral.prep',
                                                                    alpha.name = 'slope',
                                                                    parameters = parameters,
                                                                    parameter.name.prefix = '',
                                                                    parameter.name.suffix = '.prep.or',
                                                                    dimensions.with.values.referred.to.by.name = 'race',
                                                                    dimensions.with.values.referred.to.by.index = 'age',
                                                                    check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # parameters for PrEP x sex/risk
    idu.states = specification.metadata$compartment.aliases$active.idu.states
    names(idu.states) = rep("risk", length(idu.states))
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    names(non.idu.states) = rep("risk", length(non.idu.states))
    
    
    # Intercepts (only for msm vs non-msm)
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'intercept',
                                                                values = parameters['msm.prep.intercept.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex',
                                                                check.consistency = check.consistency)
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'intercept',
                                                                values = parameters['non.msm.prep.intercept.or'],
                                                                applies.to.dimension.values = c('heterosexual_male','female'),
                                                                dimensions = 'sex',
                                                                check.consistency = check.consistency)
    

    # Slopes x3 - msm slope applies to all msm regardless of IDU status (a main effect)
    #             wherease heterosexual and idu slopes are an interaction between sex and risk
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                values = parameters['msm.prep.slope.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                value = parameters['idu.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', idu.states),
                                                                check.consistency = check.consistency)
    jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                value = parameters['heterosexual.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', non.idu.states),
                                                                check.consistency = check.consistency)
    
    # PrEP Efficacy
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.msm.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.MSM.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.heterosexual.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.HETEROSEXUAL.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.idu.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.IDU.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 
                                 'msm.prep.intercept.or',
                                 'non.msm.prep.intercept.or',
                                 'msm.prep.slope.or',
                                 'idu.prep.slope.or',
                                 'heterosexual.prep.slope.or',
                                 'prep.efficacy.z')
    
    #-- Proportion MSM of Male --#
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'proportion.msm.of.male',
                                                                alpha.name = 'value',
                                                                values = parameters['proportion.msm.of.male.mult'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 
                                 'proportion.msm.of.male.mult')
    #-- Transmission Rates --#
    
    trate.times = 0:2
    
    # MSM
    used.names = set.ehe.trate.alphas.from.parameters(jheem.engine,
                                                      parameters = parameters,
                                                      category = 'msm',
                                                      age.multiplier.infix = 'msm.susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = T,
                                                      check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # Heterosexual
    used.names = set.ehe.trate.alphas.from.parameters(jheem.engine,
                                                      parameters = parameters,
                                                      category = 'heterosexual',
                                                      age.multiplier.infix = 'susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = T,
                                                      check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # IDU
    used.names = set.ehe.trate.alphas.from.parameters(jheem.engine,
                                                      parameters = parameters,
                                                      category = 'idu',
                                                      age.multiplier.infix = 'susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = F,
                                                      check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    # Add in the IDU msm vs het male
    for (time in trate.times)
    {
        jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters[paste0('msm.vs.heterosexual.male.idu.susceptibility.rr.', time)],
                                                                    applies.to.dimension.values = 'msm',
                                                                    dimensions = 'sex.to',
                                                                    check.consistency = check.consistency)
        
        jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters['female.vs.heterosexual.male.idu.susceptibility.rr'],
                                                                    applies.to.dimension.values = 'female',
                                                                    dimensions = 'sex.to',
                                                                    check.consistency = check.consistency)
    }
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 
                                 paste0('msm.vs.heterosexual.male.idu.susceptibility.rr.', trate.times),
                                 'female.vs.heterosexual.male.idu.susceptibility.rr')
    
    # Add in the IDU Peak
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.start',
                                                                values = parameters['idu.peak.trate.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.end',
                                                                values = parameters['idu.peak.trate.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.start',
                                                                values = parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.peak'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex.to',
                                                                check.consistency = check.consistency)
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.end',
                                                                values = parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.peak'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex.to',
                                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 'idu.peak.trate.multiplier', 'msm.vs.heterosexual.male.idu.susceptibility.rr.peak')
    
    
    #-- Aging --#
    used.names = set.ehe.aging.from.parameters(jheem.engine,
                                               parameters = parameters,
                                               times = c('.pre.spike', 0:3),
                                               check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- IDU --#
    used.names = set.ehe.idu.from.parameters(jheem.engine,
                                             parameters = parameters,
                                             check.consistency = check.consistency)
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- HIV Mortality --#
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate0',
                                                                values = parameters['hiv.mortality.0'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate2',
                                                                values = parameters['hiv.mortality.2'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.ramp.values(element.name = 'unsuppressed.hiv.mortality.rate',
                                         values = c(parameters['hiv.mortality.0'], rep(parameters['peak.hiv.mortality'], 2)),
                                         indices = c('pre.peak', 'peak.start', 'peak.end'),
                                         check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 'hiv.mortality.0', 'hiv.mortality.2', 'peak.hiv.mortality')
    
    
    #-- Diagnosed Transmissibility --#
    
    jheem.engine$set.element.value(element.name = 'diagnosed.needle.sharing.rr',
                                   value = parameters['diagnosed.transmission.rr'],
                                   check.consistency = check.consistency)
    
    jheem.engine$set.element.value(element.name = 'diagnosed.sexual.transmission.rr',
                                   value = parameters['diagnosed.transmission.rr'],
                                   check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 'diagnosed.transmission.rr')
    
    
    
    #-- Return --#
    if (check.consistency)
        invisible(unique(used.parameter.names))
    else
        invisible(used.parameter.names)
}

##---------------##
##-- CONSTANTS --##
##---------------##

ORAL.PREP.MSM.RR.LOG.SD = (log(EHE_BASE_PARAMETERS$ci.upper['prep.rr.msm']) - log(EHE_BASE_PARAMETERS$ci.lower['prep.rr.msm'])) / qnorm(.975)
ORAL.PREP.HETEROSEXUAL.RR.LOG.SD = (log(EHE_BASE_PARAMETERS$ci.upper['prep.rr.heterosexual']) - log(EHE_BASE_PARAMETERS$ci.lower['prep.rr.heterosexual'])) / qnorm(.975)
ORAL.PREP.IDU.RR.LOG.SD = (log(EHE_BASE_PARAMETERS$ci.upper['prep.rr.idu']) - log(EHE_BASE_PARAMETERS$ci.lower['prep.rr.idu'])) / qnorm(.975)


##----------------------##
##-- HELPER FUNCTIONS --##
##----------------------##

# Returns the names of parameters used if check.consistency == T
# sex.risk.multiplier gets applied (multiplied in) BEFORE applying transformation
set.ehe.alphas.from.parameters <- function(jheem.engine,
                                           element.name,
                                           parameters,
                                           parameter.suffixes,
                                           alpha.names = names(parameter.suffixes),
                                           idu.applies.to.in.remission,
                                           transformation = 'identity',
                                           sex.risk.multiplier=1,
                                           dimensions.with.values.referred.to.by.name = c('race'),
                                           dimensions.with.values.referred.to.by.index = c('age'),
                                           check.consistency = !jheem.engine$has.been.crunched())
{
    #-- Check Arguments --#
    if (check.consistency)
    {
        if (length(sex.risk.multiplier)!=1 || is.na(sex.risk.multiplier))
            stop("sex.risk.multiplier must be a single value that is not NA")
        
        # @need to do
    }
    
    specification.metadata = jheem.engine$specification.metadata
    
    #-- Invert Parameter values if requested --#
    if (transformation=='reciprocal')
    {
        parameters = 1/parameters
        sex.risk.multiplier = 1/sex.risk.multiplier
    }
    
    #-- Set up idu/non-idu states --#
    if (idu.applies.to.in.remission)
        idu.states = specification.metadata$compartment.aliases$idu.states
    else
        idu.states = specification.metadata$compartment.aliases$active.idu.states
    
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    
    names(idu.states) = rep('risk', length(idu.states))
    names(non.idu.states) = rep('risk', length(non.idu.states))
    
    #-- Iterate through each alpha name --#
    used.parameter.names = character()
    for (i in 1:length(alpha.names))
    {
        alpha.name = alpha.names[i]
        parameter.suffix = parameter.suffixes[i]
        
        #-- Main Effects --#
        used.names = set.element.functional.form.alphas.from.parameters(jheem.engine = jheem.engine,
                                                                        element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        parameters = parameters,
                                                                        parameter.name.prefix = '',
                                                                        parameter.name.suffix = parameter.suffix,
                                                                        dimensions.with.values.referred.to.by.name = dimensions.with.values.referred.to.by.name,
                                                                        dimensions.with.values.referred.to.by.index = dimensions.with.values.referred.to.by.index,
                                                                        check.consistency = check.consistency,
                                                                        throw.error.if.no.parameters = alpha.name != 'slope')
        
        if (check.consistency)
            used.parameter.names = c(used.parameter.names, used.names)
        
        #-- Sex/Risk Interaction Effects --#
        
        # MSM
        param.name = paste0('msm', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            jheem.engine$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='msm', non.idu.states),
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # Heterosexual
        param.name = paste0('heterosexual', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            jheem.engine$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='heterosexual_male', sex='female', non.idu.states),
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # IDU
        param.name = paste0('idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            jheem.engine$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='heterosexual_male', sex='female', idu.states),
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # MSM-IDU
        param.name = paste0('msm.idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            jheem.engine$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='msm', idu.states),
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
    }
    
    # Return
    used.parameter.names
}


set.ehe.trate.alphas.from.parameters <- function(jheem.engine,
                                                 parameters,
                                                 category,
                                                 age.multiplier.infix,
                                                 times,
                                                 do.ramp,
                                                 check.consistency)
{
    elem.name = paste0(category, '.trates')
    used.parameter.names = character()
    
    specification.metadata = jheem.engine$specification.metadata
    for (time in times)
    {
        alpha.name = paste0('rate', time)
        
        #-- The race effects --#
        for (race in specification.metadata$dim.names$race)
        {
            param.name = paste0(race, '.', category, '.trate.', time)
            jheem.engine$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                        alpha.name = alpha.name,
                                                                        values = parameters[param.name],
                                                                        applies.to.dimension.values = race,
                                                                        dimensions = 'race.to',
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        
        #-- The age effects --#
        for (age.index in 1:specification.metadata$n.ages)
        {
            # First check for one time-specific parameter for age
            param.name = paste0('age',age.index, '.', age.multiplier.infix, '.', time)
            param.value = parameters[param.name]
            
            # Next check for a time 1/2 specific parameter for age
            if (is.na(param.value) && (time==1 || time==2))
            {
                param.name = paste0('age',age.index, '.', age.multiplier.infix, '.12')
                param.value = parameters[param.name]
            }
                
            # Last check for a general time-specific parameter
            if (is.na(param.value))
            {
                param.name = paste0('age',age.index, '.', age.multiplier.infix)
                param.value = parameters[param.name]
            }
            
            # Set if the parameter exists
            if (!is.na(param.value))
            {
                jheem.engine$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                            alpha.name = alpha.name,
                                                                            values = param.value,
                                                                            applies.to.dimension.values = age.index,
                                                                            dimensions = 'age.to',
                                                                            check.consistency = check.consistency)
                
                
                if (check.consistency)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
        }
    }

    # Ramp
    if (do.ramp)
    {
        param.name = paste0(category, ".peak.trate.multiplier")
        jheem.engine$set.element.ramp.values(element.name = elem.name,
                                             values = rep(parameters[param.name], 2),
                                             indices = c('peak.start','peak.end'),
                                             check.consistency = check.consistency)
        
        if (check.consistency)
            used.parameter.names = c(used.parameter.names, param.name)
    }
    
    # After Modifier
    param.name = paste0(category, '.fraction.trate.change.after.t2')
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                alpha.name = 'after.modifier',
                                                                values = parameters[param.name],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, param.name)
    
    
    # Return
    used.parameter.names
}

set.ehe.aging.from.parameters <- function(jheem.engine,
                                          parameters,
                                          times,
                                          idu.applies.to.in.remission = T,
                                          check.consistency)
{
    #-- Some set-up --#
    used.parameter.names = character()
    specification.metadata = jheem.engine$specification.metadata
    
    if (idu.applies.to.in.remission)
        idu.states = specification.metadata$compartment.aliases$idu.states
    else
        idu.states = specification.metadata$compartment.aliases$active.idu.states
    
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    
    names(idu.states) = rep('risk', length(idu.states))
    names(non.idu.states) = rep('risk', length(non.idu.states))
       
    for (time in times)
    {
        alpha.name = paste0('rate',time)
        jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['msm.age1.aging.base'],
                                                                    applies.to.dimension.values=list(sex='msm', age=1, risk = non.idu.states),
                                                           #         applies.to.dimension.values=list(sex='msm', age=1),
                                                                    check.consistency = check.consistency)
        
        jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['heterosexual.age1.aging.base'],
                                                                    applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=1, risk=non.idu.states),
                                                                    check.consistency = check.consistency)
        
        jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['idu.age1.aging.base'],
                                                                    applies.to.dimension.values=list(age=1, risk=idu.states),
                                                               #     applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=1, risk=idu.states),
                                                                    check.consistency = check.consistency)
        
        if (check.consistency)
            used.parameter.names = c(used.parameter.names, 
                                     'msm.age1.aging.base',
                                     'heterosexual.age1.aging.base',
                                     'idu.age1.aging.base')
        
        for (age.index in 2:specification.metadata$n.ages)
        {
            param.name = paste0('msm.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(sex='msm', age=age.index, risk=non.idu.states),
                                                                    #        applies.to.dimension.values=list(sex='msm', age=age.index),
                                                                            check.consistency = check.consistency)
                
                if (check.consistency)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
            
            
            param.name = paste0('heterosexual.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=age.index, risk=non.idu.states),
                                                                            check.consistency = check.consistency)
                
                if (check.consistency)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
            
            param.name = paste0('idu.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                jheem.engine$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(age=age.index, risk=idu.states),
                                                                       #     applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=age.index, risk=idu.states),
                                                                            check.consistency = check.consistency)
                
                if (check.consistency)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
        }
    }
    
    
    # Return
    used.parameter.names
}

set.ehe.idu.from.parameters = function(jheem.engine,
                                       parameters,
                                       times = c(0,2),
                                       check.consistency)
{
    used.parameter.names = character()
    specification.metadata = jheem.engine$specification.metadata
    
    for (time in times)
    {
        alpha.name = paste0('time',time)
        for (race in specification.metadata$dim.names$race)
        {
            param.name = paste0(race, '.incident.idu.multiplier.', time)
            jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                        alpha.name = alpha.name,
                                                                        values = parameters[param.name],
                                                                        applies.to.dimension.values = race,
                                                                        dimensions = 'race',
                                                                        check.consistency = check.consistency)
            
            if (check.consistency)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        param.name = paste0('msm.incident.idu.multiplier.', time)
        jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                    alpha.name = alpha.name,
                                                                    values = parameters[param.name],
                                                                    applies.to.dimension.values = 'msm',
                                                                    dimensions = 'sex',
                                                                    check.consistency = check.consistency)
        
        if (check.consistency)
            used.parameter.names = c(used.parameter.names, param.name)
    }
    
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.remission',
                                                                alpha.name = 'value',
                                                                values = parameters['idu.remission.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    jheem.engine$set.element.functional.form.main.effect.alphas(element.name = 'idu.relapse',
                                                                alpha.name = 'value',
                                                                values = parameters['idu.relapse.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all',
                                                                check.consistency = check.consistency)
    
    if (check.consistency)
        used.parameter.names = c(used.parameter.names, 'idu.remission.multiplier', 'idu.relapse.multiplier')
    
    # Return
    used.parameter.names
    
}