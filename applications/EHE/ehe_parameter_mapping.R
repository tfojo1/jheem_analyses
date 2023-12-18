
##------------------------##
##-- THE MAIN FUNCTIONS --##
##------------------------##

EHE.APPLY.PARAMETERS.FN = function(model.settings, parameters)
{
    specification.metadata = model.settings$specification.metadata
    
    #-- Birth rates --#
    for(race in specification.metadata$dim.names$race){
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "fertility",
                                                     alpha.name = 'intercept',
                                                     values = parameters[paste0(race,'.birth.rate.multiplier')],
                                                     applies.to.dimension.values=list(race=race))
     
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "fertility",
                                                     alpha.name = 'slope',
                                                     values = parameters[paste0(race,'.birth.rate.slope.multiplier')],
                                                     applies.to.dimension.values=list(race=race))
    }
    
    #-- Mortality rates --#
    # Race
    for(race in specification.metadata$dim.names$race){
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "non.idu.general.mortality",
                                                     alpha.name = 'value',
                                                     values = parameters[paste0(race,'.non.idu.general.mortality.rate.multiplier')],
                                                     applies.to.dimension.values=list(race=race))
    }
    
    # Age
    for(age in 1:length(specification.metadata$dim.names$age)){
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "non.idu.general.mortality",
                                                     alpha.name = 'value',
                                                     values = parameters[paste0('age',age,'.non.idu.general.mortality.rate.multiplier')],
                                                     applies.to.dimension.values=list(age=age))
    }
    
    # Sex
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters['male.non.idu.general.mortality.rate.multiplier'],
                                                   applies.to.dimension.values=list(sex=c('heterosexual_male','msm')))
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters['female.non.idu.general.mortality.rate.multiplier'],
                                                   applies.to.dimension.values=list(sex='female'))
    if(1==2){
      #-- Immigration rates --#
      # Race
      for(race in specification.metadata$dim.names$race){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "immigration",
                                                       alpha.name = 'value',
                                                       values = parameters[paste0(race,'.immigration.rate.multiplier')],
                                                       applies.to.dimension.values=list(race=race))
      }
      
      # Age
      for(age in 1:length(specification.metadata$dim.names$age)){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "immigration",
                                                       alpha.name = 'value',
                                                       values = parameters[paste0('age',age,'.immigration.rate.multiplier')],
                                                       applies.to.dimension.values=list(age=age))
      }
      
      #-- Emigration rates --#
      # Race
      for(race in specification.metadata$dim.names$race){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "emigration",
                                                       alpha.name = 'value',
                                                       values = parameters[paste0(race,'.emigration.rate.multiplier')],
                                                       applies.to.dimension.values=list(race=race))
      }
      
      # Age
      for(age in 1:length(specification.metadata$dim.names$age)){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "emigration",
                                                       alpha.name = 'value',
                                                       values = parameters[paste0('age',age,'.emigration.rate.multiplier')],
                                                       applies.to.dimension.values=list(age=age))
      }
    }
    

    #-- Suppression --#
    set.ehe.alphas.from.parameters(model.settings,
                                   element.name = 'suppression.of.diagnosed',
                                   parameters = parameters,
                                   parameter.suffixes = c(intercept='.suppressed.or', slope='.suppressed.slope.or'),
                                   idu.applies.to.in.remission = F)
    
    #-- Testing --#
    set.ehe.alphas.from.parameters(model.settings,
                                   element.name = 'testing',
                                   parameters = parameters,
                                   parameter.suffixes = c(intercept='.proportion.tested.or', slope='.proportion.tested.slope.or'),
                                   idu.applies.to.in.remission = F,
                                   throw.error.if.no.parameters = F)

    model.settings$set.element.ramp.values(element.name = 'testing',
                                         values = parameters['testing.ramp.up.vs.current.rr'] * c(TESTING.FIRST.YEAR.FRACTION.OF.RAMP,1),
                                         indices = 2:3)

    #-- PrEP --#
    
    # parameters for PrEP x age and race - are used for both interecept and slope
    set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                       element.name = 'oral.prep.uptake',
                                                       alpha.name = 'intercept',
                                                       parameters = parameters,
                                                       parameter.name.prefix = '',
                                                       parameter.name.suffix = '.prep.or',
                                                       dimensions.with.values.referred.to.by.name = 'race',
                                                       dimensions.with.values.referred.to.by.index = 'age')
    
    set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                       element.name = 'oral.prep.uptake',
                                                       alpha.name = 'slope',
                                                       parameters = parameters,
                                                       parameter.name.prefix = '',
                                                       parameter.name.suffix = '.prep.or',
                                                       dimensions.with.values.referred.to.by.name = 'race',
                                                       dimensions.with.values.referred.to.by.index = 'age')
    
    
    # parameters for PrEP x sex/risk
    idu.states = specification.metadata$compartment.aliases$active.idu.states
    names(idu.states) = rep("risk", length(idu.states))
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    names(non.idu.states) = rep("risk", length(non.idu.states))
    
    
    # Intercepts (only for msm vs non-msm)
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake',
                                                                alpha.name = 'intercept',
                                                                values = parameters['msm.prep.intercept.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake',
                                                                alpha.name = 'intercept',
                                                                values = parameters['non.msm.prep.intercept.or'],
                                                                applies.to.dimension.values = c('heterosexual_male','female'),
                                                                dimensions = 'sex')
    

    # Slopes x3 - msm slope applies to all msm regardless of IDU status (a main effect)
    #             wherease heterosexual and idu slopes are an interaction between sex and risk
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake',
                                                                alpha.name = 'slope',
                                                                values = parameters['msm.prep.slope.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake',
                                                                alpha.name = 'slope',
                                                                value = parameters['idu.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', idu.states))
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake',
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


    #-- Proportion MSM of Male --#
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'proportion.msm.of.male',
                                                                alpha.name = 'value',
                                                                values = parameters['proportion.msm.of.male.mult'],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')

    #-- Transmission Rates --#
    
    trate.times = 0:2
    
    # MSM
    set.ehe.trate.alphas.from.parameters(model.settings,
                                         parameters = parameters,
                                         category = 'msm',
                                         age.multiplier.infix = 'msm.susceptibility.rr.mult',
                                         times=trate.times,
                                         do.ramp = T)

    # Heterosexual
    set.ehe.trate.alphas.from.parameters(model.settings,
                                         parameters = parameters,
                                         category = 'heterosexual',
                                         age.multiplier.infix = 'susceptibility.rr.mult',
                                         times=trate.times,
                                         do.ramp = T)

    # IDU
    set.ehe.trate.alphas.from.parameters(model.settings,
                                         parameters = parameters,
                                         category = 'idu',
                                         age.multiplier.infix = 'susceptibility.rr.mult',
                                         times=trate.times,
                                         do.ramp = F)

    
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
    
    
    #-- Aging --#
    for(age in 1:(length(specification.metadata$dim.names$age)-1)){
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "default.aging",
                                                     alpha.name = '2007',
                                                     values = parameters[paste0('age',age,'.aging.multiplier')],
                                                     applies.to.dimension.values=list(age=age))

      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "default.aging",
                                                     alpha.name = '2019',
                                                     values = parameters[paste0('age',age,'.aging.multiplier')],
                                                     applies.to.dimension.values=list(age=age))
    }
    
    set.ehe.aging.from.parameters(model.settings,
                                  parameters = parameters,
                                  times = c('.pre.spike', 0:3))
    
    #-- IDU --#
    set.ehe.idu.from.parameters(model.settings,
                                parameters = parameters)
    

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

        
    #-- Diagnosed Transmissibility --#
    
    model.settings$set.element.value(element.name = 'diagnosed.needle.sharing.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
    model.settings$set.element.value(element.name = 'diagnosed.sexual.transmission.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
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
                                           throw.error.if.no.parameters = T)
{
    #-- Check Arguments --#
    
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
    for (i in 1:length(alpha.names))
    {
        alpha.name = alpha.names[i]
        parameter.suffix = parameter.suffixes[i]
        
        #-- Main Effects --#
        set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                           element.name = element.name,
                                                           alpha.name = alpha.name,
                                                           parameters = parameters,
                                                           parameter.name.prefix = '',
                                                           parameter.name.suffix = parameter.suffix,
                                                           dimensions.with.values.referred.to.by.name = dimensions.with.values.referred.to.by.name,
                                                           dimensions.with.values.referred.to.by.index = dimensions.with.values.referred.to.by.index,
                                                           throw.error.if.no.parameters = throw.error.if.no.parameters)
        
        
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
        }
        
    }
}


set.ehe.trate.alphas.from.parameters <- function(model.settings,
                                                 parameters,
                                                 category,
                                                 age.multiplier.infix,
                                                 times,
                                                 do.ramp)
{
    elem.name = paste0(category, '.trates')
    
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
    }
    
    # After Modifier
    param.name = paste0(category, '.fraction.trate.change.after.t2')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                alpha.name = 'after.modifier',
                                                                values = parameters[param.name],
                                                                applies.to.dimension.values = 'all',
                                                                dimensions = 'all')
}

set.ehe.aging.from.parameters <- function(model.settings,
                                          parameters,
                                          times,
                                          idu.applies.to.in.remission = T)
{
    #-- Some set-up --#
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
            }
            
            
            param.name = paste0('heterosexual.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=age.index, risk=non.idu.states))
            }
            
            param.name = paste0('idu.age',age.index,'.aging.',time)
            param.value = parameters[param.name]
            
            if (!is.na(param.value))
            {
                model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
                                                                            alpha.name = alpha.name,
                                                                            value = param.value,
                                                                            applies.to.dimension.values=list(age=age.index, risk=idu.states))
            }
        }
    }
}

set.ehe.idu.from.parameters = function(model.settings,
                                       parameters,
                                       times = c(0,2))
{
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
        }
        
        param.name = paste0('msm.incident.idu.multiplier.', time)
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                    alpha.name = alpha.name,
                                                                    values = parameters[param.name],
                                                                    applies.to.dimension.values = 'msm',
                                                                    dimensions = 'sex')
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
}