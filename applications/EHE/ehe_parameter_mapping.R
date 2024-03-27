
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
                                                     alpha.name = 'value',
                                                     values = parameters[paste0(race,'.birth.rate.multiplier')],
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
    
    #-- Migration rates --#
    migration.times = c("time.1","time.2")
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    
    for(time in migration.times){
      # Immigration
      # Total 
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "immigration",
                                                     alpha.name = time,
                                                     values = parameters[paste0('immigration.multiplier.',time)],
                                                     dimensions = "all") 
      # Race
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "immigration",
                                                     alpha.name = time,
                                                     values = parameters[paste0(races,'.migration.multiplier.',time)],
                                                     dimensions = "race",
                                                     applies.to.dimension.values=races)  
      # Age
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "immigration",
                                                     alpha.name = time,
                                                     values = parameters[paste0("age",(1:length(ages)),'.migration.multiplier.',time)],
                                                     dimensions = "age",
                                                     applies.to.dimension.values=ages) 
      # Emigration
      # Total 
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "emigration",
                                                     alpha.name = time,
                                                     values = parameters[paste0('emigration.multiplier.',time)],
                                                     dimensions = "all") 
      # Race
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "emigration",
                                                     alpha.name = time,
                                                     values = 1/parameters[paste0(races,'.migration.multiplier.',time)],
                                                     dimensions = "race",
                                                     applies.to.dimension.values=races)  
      # Age
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "emigration",
                                                     alpha.name = time,
                                                     values = 1/parameters[paste0("age",(1:length(ages)),'.migration.multiplier.',time)],
                                                     dimensions = "age",
                                                     applies.to.dimension.values=ages) 
      
    }

    #-- Assortativity --#
    
    for (race in races)
    {
        model.settings$set.element.functional.form.interaction.alphas(element.name = 'race.sexual.oes', 
                                                                      alpha.name = 'value', 
                                                                      value = parameters[paste0(race,'.sexual.assortativity.multiplier')],
                                                                      applies.to.dimension.values=c(race.from=race, race.to=race))
      
        model.settings$set.element.functional.form.interaction.alphas(element.name = 'race.idu.oes', 
                                                                      alpha.name = 'value', 
                                                                      value = parameters['race.needle.sharing.assortativity.multiplier'],
                                                                      applies.to.dimension.values=c(race.from=race, race.to=race))
    }
    
    #-- Suppression --#
    set.ehe.alphas.from.parameters(model.settings,
                                   element.name = 'suppression.of.diagnosed.without.covid',
                                   parameters = parameters,
                                   parameter.suffixes = c(intercept='.suppressed.or', slope='.suppressed.slope.or'),
                                   idu.applies.to.in.remission = F)
    
    #-- Testing --#
    set.ehe.alphas.from.parameters(model.settings,
                                   element.name = 'general.population.testing.without.covid',
                                   parameters = parameters,
                                   parameter.suffixes = c(intercept='.proportion.tested.or', slope='.proportion.tested.slope.or'),
                                   idu.applies.to.in.remission = F,
                                   throw.error.if.no.parameters = F)

    model.settings$set.element.ramp.values(element.name = 'general.population.testing.without.covid',
                                         values = parameters['testing.ramp.up.vs.current.rr'] * c(TESTING.FIRST.YEAR.FRACTION.OF.RAMP,1),
                                         indices = 2:3)
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase",
                                                   alpha.name = "value",
                                                   value = parameters["msm.undiagnosed.testing.increase.rr"],
                                                   applies.to.dimension.values=list(sex="msm"))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase",
                                                   alpha.name = "value",
                                                   value = parameters["heterosexual.undiagnosed.testing.increase.rr"],
                                                   applies.to.dimension.values=list(sex=c("female","heterosexual_male"),
                                                                                    risk = non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase",
                                                   alpha.name = "value",
                                                   value = parameters["idu.undiagnosed.testing.increase.rr"],
                                                   applies.to.dimension.values=list(sex=c("female","heterosexual_male"),
                                                                                    risk = idu.states))
    

    #-- PrEP --#
    
    # parameters for PrEP x age and race - are used for both interecept and slope
    set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                       element.name = 'oral.prep.uptake.without.covid',
                                                       alpha.name = 'intercept',
                                                       parameters = parameters,
                                                       parameter.name.prefix = '',
                                                       parameter.name.suffix = '.prep.or',
                                                       dimensions.with.values.referred.to.by.name = 'race',
                                                       dimensions.with.values.referred.to.by.index = 'age')
    
    set.element.functional.form.alphas.from.parameters(model.settings = model.settings,
                                                       element.name = 'oral.prep.uptake.without.covid',
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
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                alpha.name = 'intercept',
                                                                values = parameters['msm.prep.intercept.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                alpha.name = 'intercept',
                                                                values = parameters['non.msm.prep.intercept.or'],
                                                                applies.to.dimension.values = c('heterosexual_male','female'),
                                                                dimensions = 'sex')
    

    # Slopes x3 - msm slope applies to all msm regardless of IDU status (a main effect)
    #             wherease heterosexual and idu slopes are an interaction between sex and risk
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                alpha.name = 'slope',
                                                                values = parameters['msm.prep.slope.or'],
                                                                applies.to.dimension.values = 'msm',
                                                                dimensions = 'sex')
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                alpha.name = 'slope',
                                                                value = parameters['idu.prep.slope.or'],
                                                                applies.to.dimension.values=c(sex='heterosexual_male', sex='female', idu.states))
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake.without.covid',
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
    spline.times = c(2010,2020,2030,2040)
    age.indices = 1:(length(specification.metadata$dim.names$age)-1)
    
    age.race.time.ages = c(1,2)
    age.race.ages = 3
    non.interacted.ages = setdiff(age.indices, c(age.race.time.ages, age.race.ages))
    
    for(spline.i in 1:length(spline.times))
    {
        model.settings$set.element.functional.form.main.effect.alphas(element.name = "uninfected.aging",
                                                                      alpha.name = as.character(spline.times[spline.i]),
                                                                      value = parameters[paste0('age',non.interacted.ages,'.aging.multiplier')],
                                                                      applies.to.dimension.values = non.interacted.ages,
                                                                      dimensions = 'age')
        for (race in specification.metadata$dim.names$race)
        {
            for (age in age.race.ages)
            {
                set.element.functional.form.interaction.alphas(model.settings,
                                                               element.name = "uninfected.aging",
                                                               alpha.name = as.character(spline.times[spline.i]),
                                                               value = parameters[paste0(race, '.age', age, '.aging.multiplier')],
                                                               applies.to.dimension.values=list(age=age,
                                                                                                race=race))
            }
          
            for (age in age.race.time.ages)
            {
                if (spline.i<=1)
                    aging.multiplier = parameters[paste0(race, '.age', age, '.aging.multiplier.1')]
                else
                    aging.multiplier = parameters[paste0(race, '.age', age, '.aging.multiplier.2')]
                
                set.element.functional.form.interaction.alphas(model.settings,
                                                               element.name = "uninfected.aging",
                                                               alpha.name = as.character(spline.times[spline.i]),
                                                               value = aging.multiplier,
                                                               applies.to.dimension.values=list(age=age,
                                                                                                race=race))
            }
        }
        
    }
    
    # age.race.interaction.ages = c(1,2,3)
    # non.interacted.ages = setdiff(age.indices, age.race.interaction.ages)
    # 
    # for(spline.i in 1:length(spline.times)){
    #   
    #     model.settings$set.element.functional.form.main.effect.alphas(element.name = "uninfected.aging",
    #                                                                   alpha.name = as.character(spline.times[spline.i]),
    #                                                                   value = parameters[paste0('age',non.interacted.ages,'.aging.multiplier')],
    #                                                                   applies.to.dimension.values = non.interacted.ages,
    #                                                                   dimensions = 'age')
    #     
    #     for (race in specification.metadata$dim.names$race)
    #     {
    #         for (age in age.race.interaction.ages)
    #         {
    #             aging.multiplier = parameters[paste0(race, '.age', age, '.aging.multiplier')]
    #           
    #             if (spline.i == age){
    #               if(age==1){
    #                 aging.multiplier = aging.multiplier * parameters[paste0(race,'.age1.domino.aging.multiplier')]
    #               } else{
    #                 aging.multiplier = aging.multiplier * parameters[paste0(race,'.domino.aging.multiplier')]
    #               }
    #             }
    #                 
    #             
    #             set.element.functional.form.interaction.alphas(model.settings,
    #                                                            element.name = "uninfected.aging",
    #                                                            alpha.name = as.character(spline.times[spline.i]),
    #                                                            value = aging.multiplier,
    #                                                            applies.to.dimension.values=list(age=age,
    #                                                                                             race=race))
    #         }
    #     }
    # }
    
    #-- HIV Aging --#
    hiv.aging.spline.times = c("pre.spike","time0","time1","time2") 
    
    for(spline.i in 1:length(hiv.aging.spline.times)){
    
      spline.time = hiv.aging.spline.times[spline.i]
      time.suffix = max(0,spline.i-2)
      
    # Race
      for(race in specification.metadata$dim.names$race){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "hiv.positive.aging.rates",
                                                       alpha.name = spline.time, 
                                                       values = parameters[paste0(race,'.hiv.aging.multiplier.',time.suffix)],
                                                       applies.to.dimension.values=list(race=race))
      }
      
      # Age
      for(age in 1:(length(specification.metadata$dim.names$age)-1)){
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "hiv.positive.aging.rates",
                                                       alpha.name = spline.time,
                                                       values = parameters[paste0('age',age,'.hiv.aging.multiplier.',time.suffix)],
                                                       applies.to.dimension.values=list(age=age))
      }
      
      # Sex/risk 
      set.element.functional.form.main.effect.alphas(model.settings,
                                                     element.name = "hiv.positive.aging.rates",
                                                     alpha.name = spline.time,
                                                     values = parameters[paste0('msm.hiv.aging.multiplier.',time.suffix)],
                                                     applies.to.dimension.values=c(sex='msm')) 
      
      # when cutting across two dimensions, have to use interaction alphas 
      set.element.functional.form.interaction.alphas(model.settings,
                                                     element.name = "hiv.positive.aging.rates",
                                                     alpha.name = spline.time,
                                                     value = parameters[paste0('idu.hiv.aging.multiplier.',time.suffix)],
                                                     applies.to.dimension.values=c(sex='heterosexual_male', sex='female', idu.states))
      
      set.element.functional.form.interaction.alphas(model.settings,
                                                     element.name = "hiv.positive.aging.rates",
                                                     alpha.name = spline.time,
                                                     value = parameters[paste0('heterosexual.hiv.aging.multiplier.',time.suffix)],
                                                     applies.to.dimension.values=c(sex='heterosexual_male', sex='female', non.idu.states))
      
    
    }
    

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

# set.ehe.aging.from.parameters <- function(model.settings,
#                                           parameters,
#                                           times,
#                                           idu.applies.to.in.remission = T)
# {
#     #-- Some set-up --#
#     specification.metadata = model.settings$specification.metadata
#     
#     if (idu.applies.to.in.remission)
#         idu.states = specification.metadata$compartment.aliases$idu.states
#     else
#         idu.states = specification.metadata$compartment.aliases$active.idu.states
#     
#     non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
#     
#     names(idu.states) = rep('risk', length(idu.states))
#     names(non.idu.states) = rep('risk', length(non.idu.states))
#        
#     for (time in times)
#     {
#         alpha.name = paste0('rate',time)
#         model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                     alpha.name = alpha.name,
#                                                                     value = parameters['msm.age1.aging.base'],
#                                                                     applies.to.dimension.values=list(sex='msm', age=1, risk = non.idu.states))
#         
#         model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                     alpha.name = alpha.name,
#                                                                     value = parameters['heterosexual.age1.aging.base'],
#                                                                     applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=1, risk=non.idu.states))
#         
#         model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                     alpha.name = alpha.name,
#                                                                     value = parameters['idu.age1.aging.base'],
#                                                                     applies.to.dimension.values=list(age=1, risk=idu.states))
# 
#         
#         for (age.index in 2:specification.metadata$n.ages)
#         {
#             param.name = paste0('msm.age',age.index,'.aging.',time)
#             param.value = parameters[param.name]
#             
#             if (!is.na(param.value))
#             {
#                 model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                             alpha.name = alpha.name,
#                                                                             value = param.value,
#                                                                             applies.to.dimension.values=list(sex='msm', age=age.index, risk=non.idu.states))
#             }
#             
#             
#             param.name = paste0('heterosexual.age',age.index,'.aging.',time)
#             param.value = parameters[param.name]
#             
#             if (!is.na(param.value))
#             {
#                 model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                             alpha.name = alpha.name,
#                                                                             value = param.value,
#                                                                             applies.to.dimension.values=list(sex=c('heterosexual_male', 'female'), age=age.index, risk=non.idu.states))
#             }
#             
#             param.name = paste0('idu.age',age.index,'.aging.',time)
#             param.value = parameters[param.name]
#             
#             if (!is.na(param.value))
#             {
#                 model.settings$set.element.functional.form.interaction.alphas(element.name = 'hiv.positive.aging.rates',
#                                                                             alpha.name = alpha.name,
#                                                                             value = param.value,
#                                                                             applies.to.dimension.values=list(age=age.index, risk=idu.states))
#             }
#         }
#     }
# }

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