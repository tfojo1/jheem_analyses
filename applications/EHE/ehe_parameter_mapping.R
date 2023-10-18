
##------------------------##
##-- THE MAIN FUNCTIONS --##
##------------------------##

EHE.APPLY.PARAMETERS.FN = function(model.settings, parameters,
                                   track.used.parameters)
{
    specification.metadata = model.settings$specification.metadata
    
    if (track.used.parameters)
        used.parameter.names = character()
    
          
    #-- Suppression --#
    used.names = set.ehe.alphas.from.parameters(model.settings,
                                                element.name = 'suppression.of.diagnosed',
                                                parameters = parameters,
                                                parameter.suffixes = c(intercept='.suppressed.or', slope='.suppressed.slope.or'),
                                                idu.applies.to.in.remission = F,
                                                track.used.parameters = track.used.parameters)
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- Testing --#
    used.names = set.ehe.alphas.from.parameters(model.settings,
                                                element.name = 'testing',
                                                parameters = parameters,
                                                parameter.suffixes = c(intercept='.proportion.tested.or', slope='.proportion.tested.slope.or'),
                                                idu.applies.to.in.remission = F,
                                                track.used.parameters = track.used.parameters,
                                                throw.error.if.no.parameters = F)
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    
    model.settings$set.element.ramp.values(element.name = 'testing',
                                         values = parameters['testing.ramp.up.vs.current.rr'] * c(TESTING.FIRST.YEAR.FRACTION.OF.RAMP,1),
                                         indices = 2:3)
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 'testing.ramp.up.vs.current.rr')
    
    #-- PrEP --#
    
    # parameters for PrEP x age and race - are used for both interecept and slope
    used.names = set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                                    element.name = 'oral.prep',
                                                                    alpha.name = 'intercept',
                                                                    parameters = parameters,
                                                                    parameter.name.prefix = '',
                                                                    parameter.name.suffix = '.prep.or',
                                                                    dimensions.with.values.referred.to.by.name = 'race',
                                                                    dimensions.with.values.referred.to.by.index = 'age')
    
    used.names = set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                                    element.name = 'oral.prep',
                                                                    alpha.name = 'slope',
                                                                    parameters = parameters,
                                                                    parameter.name.prefix = '',
                                                                    parameter.name.suffix = '.prep.or',
                                                                    dimensions.with.values.referred.to.by.name = 'race',
                                                                    dimensions.with.values.referred.to.by.index = 'age')
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # parameters for PrEP x sex/risk
    idu.states = specification.metadata$compartment.aliases$active.idu.states
    names(idu.states) = rep("risk", length(idu.states))
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    names(non.idu.states) = rep("risk", length(non.idu.states))
    
    
    # Intercepts (only for msm vs non-msm)
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'intercept',
                                                                values = parameters['msm.prep.intercept.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'intercept',
                                                                values = parameters['non.msm.prep.intercept.or'],
                                                                applies.to.dimension.values = c('heterosexual_male','female'),
                                                                dimensions = 'sex')
    

    # Slopes x3 - msm slope applies to all msm regardless of IDU status (a main effect)
    #             wherease heterosexual and idu slopes are an interaction between sex and risk
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                values = parameters['msm.prep.slope.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                value = parameters['idu.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', idu.states))
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep',
                                                                alpha.name = 'slope',
                                                                value = parameters['heterosexual.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', non.idu.states))
    
    # PrEP Efficacy
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.msm.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.MSM.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.heterosexual.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.HETEROSEXUAL.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.idu.rr',
                                                                alpha.name = 'value',
                                                                values = exp(ORAL.PREP.IDU.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 
                                 'msm.prep.intercept.or',
                                 'non.msm.prep.intercept.or',
                                 'msm.prep.slope.or',
                                 'idu.prep.slope.or',
                                 'heterosexual.prep.slope.or',
                                 'prep.efficacy.z')
    
    #-- Proportion MSM of Male --#
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'proportion.msm.of.male',
                                                                alpha.name = 'value',
                                                                values = parameters['proportion.msm.of.male.mult'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 
                                 'proportion.msm.of.male.mult')
    #-- Transmission Rates --#
    
    trate.times = 0:2
    
    # MSM
    used.names = set.ehe.trate.alphas.from.parameters(model.settings,
                                                      parameters = parameters,
                                                      category = 'msm',
                                                      age.multiplier.infix = 'msm.susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = T,
                                                      track.used.parameters = track.used.parameters)
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # Heterosexual
    used.names = set.ehe.trate.alphas.from.parameters(model.settings,
                                                      parameters = parameters,
                                                      category = 'heterosexual',
                                                      age.multiplier.infix = 'susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = T,
                                                      track.used.parameters = track.used.parameters)
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    # IDU
    used.names = set.ehe.trate.alphas.from.parameters(model.settings,
                                                      parameters = parameters,
                                                      category = 'idu',
                                                      age.multiplier.infix = 'susceptibility.rr.mult',
                                                      times=trate.times,
                                                      do.ramp = F,
                                                      track.used.parameters = track.used.parameters)
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    # Add in the IDU msm vs het male
    for (time in trate.times)
    {
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters[paste0('msm.vs.heterosexual.male.idu.susceptibility.rr.', time)],
                                                                    applies.to.dimension.values = 'msm',
                                                                    dimensions = 'sex.to')
        
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters['female.vs.heterosexual.male.idu.susceptibility.rr'],
                                                                    applies.to.dimension.values = 'female',
                                                                    dimensions = 'sex.to')
    }
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 
                                 paste0('msm.vs.heterosexual.male.idu.susceptibility.rr.', trate.times),
                                 'female.vs.heterosexual.male.idu.susceptibility.rr')
    
    # Add in the IDU Peak
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.start',
                                                                values = parameters['idu.peak.trate.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.end',
                                                                values = parameters['idu.peak.trate.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.start',
                                                                values = parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.peak'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex.to')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'extra.idu.peak.multiplier',
                                                                alpha.name = 'peak.end',
                                                                values = parameters['msm.vs.heterosexual.male.idu.susceptibility.rr.peak'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex.to')
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 'idu.peak.trate.multiplier', 'msm.vs.heterosexual.male.idu.susceptibility.rr.peak')
    
    
    #-- Aging --#
    used.names = set.ehe.aging.from.parameters(model.settings,
                                               parameters = parameters,
                                               times = c('.pre.spike', 0:3),
                                               track.used.parameters = track.used.parameters)
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- IDU --#
    used.names = set.ehe.idu.from.parameters(model.settings,
                                             parameters = parameters,
                                             track.used.parameters = track.used.parameters)
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, used.names)
    
    
    #-- HIV Mortality --#
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate0',
                                                                values = parameters['hiv.mortality.0'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate2',
                                                                values = parameters['hiv.mortality.2'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.ramp.values(element.name = 'unsuppressed.hiv.mortality.rate',
                                         values = c(parameters['hiv.mortality.0'], rep(parameters['peak.hiv.mortality'], 2)),
                                         indices = c('pre.peak', 'peak.start', 'peak.end'))
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 'hiv.mortality.0', 'hiv.mortality.2', 'peak.hiv.mortality')
    
    
    #-- Diagnosed Transmissibility --#
    
    model.settings$set.element.value(element.name = 'diagnosed.needle.sharing.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
    model.settings$set.element.value(element.name = 'diagnosed.sexual.transmission.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 'diagnosed.transmission.rr')
    
    
    
    #-- Return --#
    if (track.used.parameters)
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

# Returns the names of parameters used if track.used.parameters == T
# sex.risk.multiplier gets applied (multiplied in) BEFORE applying transformation
set.ehe.alphas.from.parameters <- function(model.settings,
                                           element.name,
                                           parameters,
                                           parameter.suffixes,
                                           alpha.names = names(parameter.suffixes),
                                           idu.applies.to.in.remission,
                                           transformation = 'identity',
                                           sex.risk.multiplier=1,
                                           dimensions.with.values.referred.to.by.name = c('race'),
                                           dimensions.with.values.referred.to.by.index = c('age'),
                                           track.used.parameters,
                                           throw.error.if.no.parameters = T)
{
    #-- Check Arguments --#
    if (track.used.parameters)
    {
        if (length(sex.risk.multiplier)!=1 || is.na(sex.risk.multiplier))
            stop("sex.risk.multiplier must be a single value that is not NA")
        
        # @need to do
    }
    
    specification.metadata = model.settings$specification.metadata
    
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
        used.names = set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                                        element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        parameters = parameters,
                                                                        parameter.name.prefix = '',
                                                                        parameter.name.suffix = parameter.suffix,
                                                                        dimensions.with.values.referred.to.by.name = dimensions.with.values.referred.to.by.name,
                                                                        dimensions.with.values.referred.to.by.index = dimensions.with.values.referred.to.by.index,
                                                                        throw.error.if.no.parameters = throw.error.if.no.parameters)
        
        if (track.used.parameters)
            used.parameter.names = c(used.parameter.names, used.names)
        
        #-- Sex/Risk Interaction Effects --#
        
        # MSM
        param.name = paste0('msm', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='msm', non.idu.states))
            
            if (track.used.parameters)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # Heterosexual
        param.name = paste0('heterosexual', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='heterosexual_male', sex='female', non.idu.states))
            
            if (track.used.parameters)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # IDU
        param.name = paste0('idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='heterosexual_male', sex='female', idu.states))
            
            if (track.used.parameters)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        # MSM-IDU
        param.name = paste0('msm.idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = c(sex='msm', idu.states))
            
            if (track.used.parameters)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
    }
    
    # Return
    used.parameter.names
}


set.ehe.trate.alphas.from.parameters <- function(model.settings,
                                                 parameters,
                                                 category,
                                                 age.multiplier.infix,
                                                 times,
                                                 do.ramp,
                                                 track.used.parameters)
{
    elem.name = paste0(category, '.trates')
    used.parameter.names = character()
    
    specification.metadata = model.settings$specification.metadata
    for (time in times)
    {
        alpha.name = paste0('rate', time)
        
        #-- The race effects --#
        for (race in specification.metadata$dim.names$race)
        {
            param.name = paste0(race, '.', category, '.trate.', time)
            model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                        alpha.name = alpha.name,
                                                                        values = parameters[param.name],
                                                                        applies.to.dimension.values = race,
                                                                        dimensions = 'race.to')
            
            if (track.used.parameters)
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
                model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                            alpha.name = alpha.name,
                                                                            values = param.value,
                                                                            applies.to.dimension.values = age.index,
                                                                            dimensions = 'age.to')
                
                
                if (track.used.parameters)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
        }
    }

    # Ramp
    if (do.ramp)
    {
        param.name = paste0(category, ".peak.trate.multiplier")
        model.settings$set.element.ramp.values(element.name = elem.name,
                                             values = rep(parameters[param.name], 2),
                                             indices = c('peak.start','peak.end'))
        
        if (track.used.parameters)
            used.parameter.names = c(used.parameter.names, param.name)
    }
    
    # After Modifier
    param.name = paste0(category, '.fraction.trate.change.after.t2')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                alpha.name = 'after.modifier',
                                                                values = parameters[param.name],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, param.name)
    
    
    # Return
    used.parameter.names
}

set.ehe.aging.from.parameters <- function(model.settings,
                                          parameters,
                                          times,
                                          idu.applies.to.in.remission = T,
                                          track.used.parameters)
{
    #-- Some set-up --#
    used.parameter.names = character()
    specification.metadata = model.settings$specification.metadata
    
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
        model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['msm.age1.aging.base'],
                                                                    applies.to.dimension.values=list(sex='msm', age=1, risk = non.idu.states))
        
        model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['heterosexual.age1.aging.base'],
                                                                    applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=1, risk=non.idu.states))
        
        model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                    alpha.name = alpha.name,
                                                                    value = parameters['idu.age1.aging.base'],
                                                                    applies.to.dimension.values=list(age=1, risk=idu.states))
        
        if (track.used.parameters)
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
                model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(sex='msm', age=age.index, risk=non.idu.states))
                
                if (track.used.parameters)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
            
            
            param.name = paste0('heterosexual.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=age.index, risk=non.idu.states))
                
                if (track.used.parameters)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
            
            param.name = paste0('idu.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(age=age.index, risk=idu.states))
                
                if (track.used.parameters)
                    used.parameter.names = c(used.parameter.names, param.name)
            }
        }
    }
    
    
    # Return
    used.parameter.names
}

set.ehe.idu.from.parameters = function(model.settings,
                                       parameters,
                                       times = c(0,2),
                                       track.used.parameters)
{
    used.parameter.names = character()
    specification.metadata = model.settings$specification.metadata
    
    for (time in times)
    {
        alpha.name = paste0('time',time)
        for (race in specification.metadata$dim.names$race)
        {
            param.name = paste0(race, '.incident.idu.multiplier.', time)
            model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                        alpha.name = alpha.name,
                                                                        values = parameters[param.name],
                                                                        applies.to.dimension.values = race,
                                                                        dimensions = 'race')
            
            if (track.used.parameters)
                used.parameter.names = c(used.parameter.names, param.name)
        }
        
        param.name = paste0('msm.incident.idu.multiplier.', time)
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                    alpha.name = alpha.name,
                                                                    values = parameters[param.name],
                                                                    applies.to.dimension.values = 'msm',
                                                                    dimensions = 'sex')
        
        if (track.used.parameters)
            used.parameter.names = c(used.parameter.names, param.name)
    }
    
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.remission',
                                                                alpha.name = 'value',
                                                                values = parameters['idu.remission.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.relapse',
                                                                alpha.name = 'value',
                                                                values = parameters['idu.relapse.multiplier'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
    
    if (track.used.parameters)
        used.parameter.names = c(used.parameter.names, 'idu.remission.multiplier', 'idu.relapse.multiplier')
    
    # Return
    used.parameter.names
    
}