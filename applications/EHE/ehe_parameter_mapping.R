
##------------------------##
##-- THE MAIN FUNCTIONS --##
##------------------------##

EHE.APPLY.PARAMETERS.FN = function(model.settings, parameters)
{
    specification.metadata = model.settings$specification.metadata
    
    idu.states = specification.metadata$compartment.aliases$active.idu.states
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    age.indices = 1:length(ages)
    n.ages = length(ages)
    
    #-- Birth rates --#

    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "fertility",
                                                   alpha.name = 'value',
                                                   values = parameters[paste0(races,'.birth.rate.multiplier')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values=races)

    #-- Mortality rates --#
    # Race
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters[paste0(races,'.non.idu.general.mortality.rate.multiplier')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values=races)
    
    # Age
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters[paste0('age', age.indices, '.non.idu.general.mortality.rate.multiplier')],
                                                   dimension = 'age',
                                                   applies.to.dimension.values=ages)
    
    # Sex
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters['male.non.idu.general.mortality.rate.multiplier'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values=c('heterosexual_male','msm'))
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "non.idu.general.mortality",
                                                   alpha.name = 'value',
                                                   values = parameters['female.non.idu.general.mortality.rate.multiplier'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values='female')
    
    #-- Migration rates --#
    migration.times = c("time.1","time.2")
    
    for(time in migration.times)
    {   
        # Immigration - by Race 
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "immigration",
                                                       alpha.name = time,
                                                       values = parameters[paste0(races, '.immigration.multiplier.',time)],
                                                       dimension = "race",
                                                       applies.to.dimension.values = races) 
  
        
        # Emigration - by Race 
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = "emigration",
                                                       alpha.name = time,
                                                       values = parameters[paste0(races, '.emigration.multiplier.',time)],
                                                       dimension = "race",
                                                       applies.to.dimension.values = races) 
        
        # Age - immigration + emigration
        migration.multipliers = parameters[paste0("age", age.indices, '.migration.multiplier.',time)]
        race.interacted.ages = (1:length(age.indices))[is.na(migration.multipliers)]
        non.interacted.ages = (1:length(age.indices))[!is.na(migration.multipliers)]
        
        if (length(non.interacted.ages)>0)
        {
            multipliers = parameters[paste0("age", non.interacted.ages, '.migration.multiplier.',time)]
            
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = "immigration",
                                                           alpha.name = time,
                                                           values = multipliers,
                                                           dimension = "age",
                                                           applies.to.dimension.values = ages[non.interacted.ages]) 
            
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = "emigration",
                                                           alpha.name = time,
                                                           values = 1/multipliers,
                                                           dimension = "age",
                                                           applies.to.dimension.values = ages[non.interacted.ages]) 
        }
        
        if (length(race.interacted.ages)>0)
        {
            for (age.index in race.interacted.ages)
            {
                for (race in races)
                {
                    multiplier = parameters[paste0(race, ".age", age.index, '.migration.multiplier.',time)]
                    
                    set.element.functional.form.interaction.alphas(model.settings,
                                                                   element.name = 'immigration', 
                                                                   alpha.name = time, 
                                                                   value = multiplier,
                                                                   applies.to.dimension.values = list(age = ages[age.index],
                                                                                                      race = race))
                    
                    set.element.functional.form.interaction.alphas(model.settings,
                                                                   element.name = 'emigration', 
                                                                   alpha.name = time, 
                                                                   value = 1/multiplier,
                                                                   applies.to.dimension.values = list(age = ages[age.index],
                                                                                                      race = race))
                }
            }
        }
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
    
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'general.population.testing.without.covid',
                                                   alpha.name = 'intercept',
                                                   values = parameters['proportion.tested.or'],
                                                   applies.to.dimension.values = 'all',
                                                   dimension = 'all')
    
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'general.population.testing.without.covid',
                                                   alpha.name = 'slope',
                                                   values = parameters['proportion.tested.slope.or'],
                                                   applies.to.dimension.values = 'all',
                                                   dimension = 'all')

    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = 'testing.ramp.rr',
                                                   alpha.name = 'ramp.1',
                                                   value = parameters['msm.testing.ramp'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = 'msm')
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'testing.ramp.rr',
                                                   alpha.name = 'ramp.1',
                                                   value = parameters['heterosexual.testing.ramp'],
                                                   applies.to.dimension.values = list(sex=c("female","heterosexual_male"),
                                                                                      risk = non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'testing.ramp.rr',
                                                   alpha.name = 'ramp.1',
                                                   value = parameters['idu.testing.ramp'],
                                                   applies.to.dimension.values = list(sex=c("female","heterosexual_male"),
                                                                                      risk = idu.states))

    
    # model.settings$set.element.ramp.values(element.name = 'general.population.testing.without.covid',
    #                                      values = parameters['testing.ramp.up.vs.current.rr'] * c(TESTING.FIRST.YEAR.FRACTION.OF.RAMP,1),
    #                                      indices = 2:3)

    # model.settings$set.element.ramp.values(element.name = 'general.population.testing.without.covid',
    #                                        values = parameters['testing.ramp.up.vs.current.rr'],
    #                                        indices = 2)
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase.without.covid",
                                                   alpha.name = "value",
                                                   value = parameters["msm.undiagnosed.testing.increase.rr"],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = 'msm')
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase.without.covid",
                                                   alpha.name = "value",
                                                   value = parameters["heterosexual.undiagnosed.testing.increase.rr"],
                                                   applies.to.dimension.values=list(sex=c("female","heterosexual_male"),
                                                                                    risk = non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "undiagnosed.testing.increase.without.covid",
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
    
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'oral.prep.persistence',
                                                   alpha.name = 'value',
                                                   values = parameters['oral.prep.persistence.or'],
                                                   applies.to.dimension.values = 'all',
                                                   dimension = 'all')

    # Intercepts (only for msm vs non-msm)
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                  alpha.name = 'intercept',
                                                                  values = parameters['msm.prep.intercept.or'],
                                                                  applies.to.dimension.values = 'msm',
                                                                  dimension = 'sex')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                  alpha.name = 'intercept',
                                                                  values = parameters['non.msm.prep.intercept.or'],
                                                                  applies.to.dimension.values = c('heterosexual_male','female'),
                                                                  dimension = 'sex')
    

    # Slopes x3 - msm slope applies to all msm regardless of IDU status (a main effect)
    #             wherease heterosexual and idu slopes are an interaction between sex and risk
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                  alpha.name = 'slope',
                                                                  values = parameters['msm.prep.slope.or'],
                                                                  applies.to.dimension.values = 'msm',
                                                                  dimension = 'sex')
    
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                  alpha.name = 'slope',
                                                                  value = parameters['idu.prep.slope.or'],
                                                                  applies.to.dimension.values = list(sex = c('heterosexual_male', 'female'),
                                                                                                     risk = idu.states))
    model.settings$set.element.functional.form.interaction.alphas(element.name = 'oral.prep.uptake.without.covid',
                                                                  alpha.name = 'slope',
                                                                  value = parameters['heterosexual.prep.slope.or'],
                                                                  applies.to.dimension.values = list(sex = c('heterosexual_male', 'female'),
                                                                                                     risk = non.idu.states))
    
    # PrEP Efficacy
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.msm.rr',
                                                                  alpha.name = 'value',
                                                                  values = exp(ORAL.PREP.MSM.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.heterosexual.rr',
                                                                  alpha.name = 'value',
                                                                  values = exp(ORAL.PREP.HETEROSEXUAL.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'oral.prep.idu.rr',
                                                                  alpha.name = 'value',
                                                                  values = exp(ORAL.PREP.IDU.RR.LOG.SD*parameters['prep.efficacy.z']),
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')


    # PrEP Indications
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'prep.indication.without.covid',
                                                   alpha.name = 'intercept',
                                                   values = parameters['msm.prep.indications.or'],
                                                   applies.to.dimension.values = 'msm',
                                                   dimension = 'sex')
    
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'prep.indication.without.covid',
                                                   alpha.name = 'intercept',
                                                   values = parameters['non.msm.prep.indications.or'],
                                                   applies.to.dimension.values = c('heterosexual_male','female'),
                                                   dimension = 'sex')
    
    set.element.functional.form.main.effect.alphas(model.settings = model.settings,
                                                   element.name = 'prep.indication.without.covid',
                                                   alpha.name = 'slope',
                                                   values = parameters['prep.indications.slope.or'],
                                                   applies.to.dimension.values = 'all',
                                                   dimension = 'all')
    
    
    #-- Proportion MSM of Male --#
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'proportion.msm.of.male',
                                                                alpha.name = 'value',
                                                                values = parameters['proportion.msm.of.male.mult'],
                                                                applies.to.dimension.values = 'all',
                                                                dimension = 'all')

    #-- Transmission Rates --#
    
    trate.times = 0:2
    
    # MSM
    set.ehe.age.race.stratified.trate.alphas.from.parameters(model.settings,
                                                        parameters = parameters,
                                                        category = 'msm',
                                                        stratified.age.indices = 1:2,
                                                        times=trate.times)
      
    # Heterosexual
    set.ehe.trate.alphas.from.parameters(model.settings,
                                         parameters = parameters,
                                         category = 'heterosexual',
                                         times=trate.times)

    # IDU
    set.ehe.trate.alphas.from.parameters(model.settings,
                                         parameters = parameters,
                                         category = 'idu',
                                         times=trate.times)

    
    # Add in the MSM-IDU susceptibility multipliers
    # And female-IDU susceptibility multipliers
    
    for (time in trate.times)
    {
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                      alpha.name = paste0('rate', time),
                                                                      values = parameters[paste0('msm.idu.susceptibility.rr.', time)],
                                                                      applies.to.dimension.values = 'msm',
                                                                      dimension = 'sex.to')
      
      model.settings$set.element.functional.form.main.effect.alphas(element.name = 'msm.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters[paste0('msm.idu.susceptibility.rr.', time)],
                                                                    applies.to.dimension.values = idu.states,
                                                                    dimension = 'risk.to')
      
      model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.trates',
                                                                    alpha.name = paste0('rate', time),
                                                                    values = parameters['female.vs.heterosexual.male.idu.susceptibility.rr'],
                                                                    applies.to.dimension.values = 'female',
                                                                    dimension = 'sex.to')
    }
    
    # Add in the IDU Peak
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.peak.multiplier',
                                                                  alpha.name = 'peak.start',
                                                                  values = parameters['idu.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.peak.multiplier',
                                                                  alpha.name = 'peak.end',
                                                                  values = parameters['idu.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.peak.multiplier',
                                                                  alpha.name = 'peak.start',
                                                                  values = parameters['msm.idu.susceptibility.rr.peak'],
                                                                  applies.to.dimension.values = 'msm',
                                                                  dimension = 'sex.to')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.peak.multiplier',
                                                                  alpha.name = 'peak.end',
                                                                  values = parameters['msm.idu.susceptibility.rr.peak'],
                                                                  applies.to.dimension.values = 'msm',
                                                                  dimension = 'sex.to')
    
    # Add in the MSM Peak
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'msm.peak.multiplier',
                                                                  alpha.name = 'peak.start',
                                                                  values = parameters['msm.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'msm.peak.multiplier',
                                                                  alpha.name = 'peak.end',
                                                                  values = parameters['msm.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'msm.peak.multiplier',
                                                                  alpha.name = 'peak.start',
                                                                  values = parameters['msm.idu.susceptibility.rr.peak'],
                                                                  applies.to.dimension.values = idu.states,
                                                                  dimension = 'risk.to')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'msm.peak.multiplier',
                                                                  alpha.name = 'peak.end',
                                                                  values = parameters['msm.idu.susceptibility.rr.peak'],
                                                                  applies.to.dimension.values = idu.states,
                                                                  dimension = 'risk.to')
    
    # Add in the Heterosexual Peak
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'heterosexual.peak.multiplier',
                                                                  alpha.name = 'peak.start',
                                                                  values = parameters['heterosexual.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'heterosexual.peak.multiplier',
                                                                  alpha.name = 'peak.end',
                                                                  values = parameters['heterosexual.peak.trate.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    # 
    # # Add in the Peak Multipliers for Age
    # for (category in c('msm','heterosexual','idu'))
    # {
    #     model.settings$set.element.functional.form.main.effect.alphas(element.name = paste0(category, '.peak.multiplier'),
    #                                                                   alpha.name = 'peak.start',
    #                                                                   values = parameters[paste0('age', 1:specification.metadata$n.ages,
    #                                                                                              '.peak.susceptibility.rr.mult')],
    #                                                                   applies.to.dimension.values = specification.metadata$dim.names$age,
    #                                                                   dimension = 'age.to')
    #     model.settings$set.element.functional.form.main.effect.alphas(element.name = paste0(category, '.peak.multiplier'),
    #                                                                   alpha.name = 'peak.end',
    #                                                                   values = parameters[paste0('age', 1:specification.metadata$n.ages,
    #                                                                                              '.peak.susceptibility.rr.mult')],
    #                                                                   applies.to.dimension.values = specification.metadata$dim.names$age,
    #                                                                   dimension = 'age.to')
    # }
    
    #-- Non-HIV Aging --#
    spline.times = c(2010,2020,2030,2040)
    age.indices = 1:(length(specification.metadata$dim.names$age)-1)
    
    age.race.time.ages = c(1,2,3,4)
    age.race.ages = numeric()
    non.interacted.ages = setdiff(age.indices, c(age.race.time.ages, age.race.ages))
    
    for(spline.i in 1:length(spline.times))
    {
        if (length(non.interacted.ages)>0)
        {
          model.settings$set.element.functional.form.main.effect.alphas(element.name = "uninfected.aging",
                                                                        alpha.name = as.character(spline.times[spline.i]),
                                                                        value = parameters[paste0('age',non.interacted.ages,'.aging.multiplier')],
                                                                        applies.to.dimension.values = ages[non.interacted.ages],
                                                                        dimension = 'age')
        }
      
        for (race in specification.metadata$dim.names$race)
        {
            for (age in age.race.ages)
            {
                set.element.functional.form.interaction.alphas(model.settings,
                                                               element.name = "uninfected.aging",
                                                               alpha.name = as.character(spline.times[spline.i]),
                                                               value = parameters[paste0(race, '.age', age, '.aging.multiplier')],
                                                               applies.to.dimension.values = list(age = ages[age],
                                                                                                  race = race))
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
                                                               applies.to.dimension.values = list(age = ages[age],
                                                                                                  race = race))
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
    
    race.interacted.time.suffixes = numeric()
    race.interacted.age.indices = 1:2
    race.interacted.ages = specification.metadata$dim.names$age[race.interacted.age.indices]
    non.race.interacted.ages = specification.metadata$dim.names$age[-specification.metadata$n.ages][-race.interacted.age.indices]
    
    for(spline.i in 1:length(hiv.aging.spline.times)){
    
      spline.time = hiv.aging.spline.times[spline.i]
      time.suffix = max(0,spline.i-2)
      
      if (any(race.interacted.time.suffixes==time.suffix))
      {
          for (race in specification.metadata$dim.names$race)
          {
              value = parameters[paste0(race,'.hiv.aging.multiplier.',time.suffix)]
              
              set.element.functional.form.interaction.alphas(model.settings,
                                                             element.name = "hiv.positive.aging.rates",
                                                             alpha.name = spline.time,
                                                             value = value,
                                                             applies.to.dimension.values=list(sex = c('heterosexual_male','female'),
                                                                                              race = race))

              set.element.functional.form.interaction.alphas(model.settings,
                                                             element.name = "hiv.positive.aging.rates",
                                                             alpha.name = spline.time,
                                                             value = value,
                                                             applies.to.dimension.values=list(age = non.race.interacted.ages,
                                                                                              sex = 'msm',
                                                                                              race = race))
          }
          
          # Age*Risk by msm or hetersexual (IDU is heterosexual * IDU multiplier)
          # Age*race*risk for some MSM
          for(age in 1:(length(specification.metadata$dim.names$age)-1)){
            
            age.value = specification.metadata$dim.names$age[age]
            
            
            if (any(age==race.interacted.age.indices))
            {
                for (race in specification.metadata$dim.names$race)
                {
                    set.element.functional.form.interaction.alphas(model.settings,
                                                                   element.name = "hiv.positive.aging.rates",
                                                                   alpha.name = spline.time,
                                                                   value = parameters[paste0(race, '.age',age,'.msm.hiv.aging.multiplier.',time.suffix)],
                                                                   applies.to.dimension.values=list(age = age.value, 
                                                                                                    sex = 'msm',
                                                                                                    race = race))
                }
            }
            else
            {
              tmp = parameters[paste0('age',age,'.msm.hiv.aging.multiplier.',time.suffix)]
                set.element.functional.form.interaction.alphas(model.settings,
                                                               element.name = "hiv.positive.aging.rates",
                                                               alpha.name = spline.time,
                                                               value = parameters[paste0('age',age,'.msm.hiv.aging.multiplier.',time.suffix)],
                                                               applies.to.dimension.values = list(age = age.value,
                                                                                                  sex = 'msm'))
            }
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = "hiv.positive.aging.rates",
                                                           alpha.name = spline.time,
                                                           value = parameters[paste0('age',age,'.heterosexual.hiv.aging.multiplier.',time.suffix)],
                                                           applies.to.dimension.values = list(age = age.value,
                                                                                              sex = c('heterosexual_male','female'), 
                                                                                              risk = non.idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = "hiv.positive.aging.rates",
                                                           alpha.name = spline.time,
                                                           value = parameters[paste0('age',age,'.idu.hiv.aging.multiplier.',time.suffix)],
                                                           applies.to.dimension.values = list(age = age.value,
                                                                                              sex = c('heterosexual_male','female'), 
                                                                                              risk = idu.states))
            # set.element.functional.form.interaction.alphas(model.settings,
            #                                                element.name = "hiv.positive.aging.rates",
            #                                                alpha.name = spline.time,
            #                                                value = parameters[paste0('age',age,'.heterosexual.hiv.aging.multiplier.',time.suffix)]*
            #                                                  parameters[paste0('idu.hiv.aging.multiplier.',time.suffix)],
            #                                                applies.to.dimension.values = list(age = age.value,
            #                                                                                   sex = c('heterosexual_male','female'), 
            #                                                                                   risk = idu.states))
          }
      }
      else
      {
          # Race
          set.element.functional.form.main.effect.alphas(model.settings,
                                                         element.name = "hiv.positive.aging.rates",
                                                         alpha.name = spline.time, 
                                                         values = parameters[paste0(races,'.hiv.aging.multiplier.',time.suffix)],
                                                         dimension = 'race',
                                                         applies.to.dimension.values=races)
          
          for(age in 1:(length(specification.metadata$dim.names$age)-1)){
            age.value = specification.metadata$dim.names$age[age]
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = "hiv.positive.aging.rates",
                                                           alpha.name = spline.time,
                                                           value = parameters[paste0('age',age,'.msm.hiv.aging.multiplier.',time.suffix)],
                                                           applies.to.dimension.values=list(age = age.value, 
                                                                                            sex = 'msm'))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = "hiv.positive.aging.rates",
                                                           alpha.name = spline.time,
                                                           value = parameters[paste0('age',age,'.heterosexual.hiv.aging.multiplier.',time.suffix)],
                                                           applies.to.dimension.values = list(age = age.value,
                                                                                              sex = c('heterosexual_male','female'), 
                                                                                              risk = non.idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = "hiv.positive.aging.rates",
                                                           alpha.name = spline.time,
                                                           value = parameters[paste0('age',age,'.idu.hiv.aging.multiplier.',time.suffix)],
                                                           applies.to.dimension.values = list(age = age.value,
                                                                                              sex = c('heterosexual_male','female'), 
                                                                                              risk = idu.states))
            # set.element.functional.form.interaction.alphas(model.settings,
            #                                                element.name = "hiv.positive.aging.rates",
            #                                                alpha.name = spline.time,
            #                                                value = parameters[paste0('age',age,'.heterosexual.hiv.aging.multiplier.',time.suffix)]*
            #                                                  parameters[paste0('idu.hiv.aging.multiplier.',time.suffix)],
            #                                                applies.to.dimension.values = list(age = age.value,
            #                                                                                   sex = c('heterosexual_male','female'), 
            #                                                                                   risk = idu.states))
          }
      }

   
    }
    

    #-- IDU --#
    set.ehe.idu.from.parameters(model.settings,
                                parameters = parameters)
    

    #-- HIV Mortality --#
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate0',
                                                                values = parameters['hiv.mortality.0'],
                                                                applies.to.dimension.values = 'all',
                                                                dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'unsuppressed.hiv.mortality.rate',
                                                                alpha.name = 'rate1',
                                                                values = parameters['hiv.mortality.1'],
                                                                applies.to.dimension.values = 'all',
                                                                dimension = 'all')
    
    model.settings$set.element.ramp.values(element.name = 'unsuppressed.hiv.mortality.rate',
                                         values = c(parameters['hiv.mortality.0'], rep(parameters['peak.hiv.mortality'], 2)),
                                         indices = c('pre.peak', 'peak.start', 'peak.end'))

        
    #-- Diagnosed Transmissibility --#
    
    model.settings$set.element.value(element.name = 'diagnosed.needle.sharing.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
    model.settings$set.element.value(element.name = 'diagnosed.sexual.transmission.rr',
                                   value = parameters['diagnosed.transmission.rr'])
    
    #-- COVID Multipliers --#
    # Sexual transmission
    # Race
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters[paste0(races,'.sexual.transmission.covid.multiplier')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values=races)
    
    # Age
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age12.sexual.transmission.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values=ages[1:2]) # can just do 1:2
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age34.sexual.transmission.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values=ages[3:4])
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age5.sexual.transmission.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values=ages[5:n.ages])
    
    # Sex/risk
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['heterosexual.sexual.transmission.covid.multiplier'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = c('heterosexual_male','female'))
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.sexual.transmission.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['msm.sexual.transmission.covid.multiplier'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = 'msm')
    
    
    # Testing
    # Race
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   values = parameters[paste0(races,'.testing.covid.multiplier')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values = races)
    
    # Age
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age12.testing.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values = ages[1:2])
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age34.testing.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values = ages[3:4])
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['age5.testing.covid.multiplier'],
                                                   dimension = 'age',
                                                   applies.to.dimension.values = ages[5:n.ages])
    
    # Sex/risk 
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   value = parameters['heterosexual.testing.covid.multiplier'],
                                                   applies.to.dimension.values=list(sex = c("female","heterosexual_male"),
                                                                                    risk = non.idu.states))
    # have to change to main effect alpha because it's only applying to one stratum
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   values = parameters['msm.testing.covid.multiplier'],
                                                   dimension = 'sex',
                                                   applies.to.dimension.values = 'msm')
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = "max.covid.effect.testing.reduction",
                                                   alpha.name = "value",
                                                   value = parameters['idu.testing.covid.multiplier'],
                                                   applies.to.dimension.values=list(sex = c("female","heterosexual_male"),
                                                                                    risk = idu.states))
    
    # Undiagnosed testing RR increase
    # Even though this is named the exact same as in the specification, have to map it because using a functional form
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "max.covid.effect.undiagnosed.testing.rr.increase",
                                                   alpha.name = "value",
                                                   values = parameters['max.covid.effect.undiagnosed.testing.rr.increase'],
                                                   applies.to.dimension.values = "all",
                                                   dimension = 'all')
    
    
    # AIDS Diagnoses
    for (time.suffix in c('peak','0','1'))
    {
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = 'aids.to.new.diagnoses.ratio',
                                                       alpha.name = paste0('time.', time.suffix),
                                                       values = parameters[paste0('aids.to.new.diagnoses.ratio.', time.suffix)],
                                                       applies.to.dimension.values = 'all',
                                                       dimension = 'all')
    }
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
                                                                        applies.to.dimension.values = list(sex = 'msm', 
                                                                                                           risk = non.idu.states))
        }
        
        # Heterosexual
        param.name = paste0('heterosexual', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = list(sex = c('heterosexual_male','female'),
                                                                                                           risk = non.idu.states))
        }
        
        # IDU
        param.name = paste0('idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = list(sex = c('heterosexual_male','female'),
                                                                                                           risk = idu.states))
        }
        
        # MSM-IDU
        param.name = paste0('msm.idu', parameter.suffix)
        value = parameters[param.name]
        if (!is.na(value))
        {
            model.settings$set.element.functional.form.interaction.alphas(element.name = element.name,
                                                                        alpha.name = alpha.name,
                                                                        value = value * sex.risk.multiplier,
                                                                        applies.to.dimension.values = list(sex = 'msm', 
                                                                                                           risk = idu.states))
        }
        
    }
}


set.ehe.trate.alphas.from.parameters <- function(model.settings,
                                                 parameters,
                                                 category,
                                                 times)
{
    specification.metadata = model.settings$specification.metadata
    elem.name = paste0(category, '.trates')
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    
    for (time in times)
    {
        alpha.name = paste0('rate', time)
        
        #-- The race effects --#
        model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[paste0(races, '.', category, '.trate.', time)],
                                                                      dimension = 'race.to',
                                                                      applies.to.dimension.values = races)

        #-- The age effects --#
        age.multipliers = parameters[paste0('age', 1:specification.metadata$n.ages, '.susceptibility.rr.mult.', time)]
        age.multipliers[is.na(age.multipliers)] = 1
        
        model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[paste0('age', 1:specification.metadata$n.ages,
                                                                                                 '.', category, '.susceptibility.rr')] *
                                                                                age.multipliers,
                                                                      dimension = 'age.to',
                                                                      applies.to.dimension.values = ages)
    }

    # After Modifier
    param.name = paste0(category, '.fraction.trate.change.after.t2')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                alpha.name = 'after.modifier',
                                                                values = parameters[param.name],
                                                                applies.to.dimension.values = 'all',
                                                                dimension = 'all')
}

set.ehe.age.race.stratified.trate.alphas.from.parameters <- function(model.settings,
                                                                     parameters,
                                                                     category,
                                                                     stratified.age.indices,
                                                                     times)
{
    specification.metadata = model.settings$specification.metadata
    elem.name = paste0(category, '.trates')
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    non.stratified.age.indices = setdiff(1:specification.metadata$n.ages, stratified.age.indices)
    
    for (time in times)
    {
        alpha.name = paste0('rate', time)
        
        
        #-- The race effects --#
        model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[paste0(races, '.', category, '.trate.', time)],
                                                                      dimension = 'race.to',
                                                                      applies.to.dimension.values = races)
        #-- The age effects --#
        age.multipliers = parameters[paste0('age', 1:specification.metadata$n.ages, '.susceptibility.rr.mult.', time)]
        age.multipliers[is.na(age.multipliers)] = 1
        
        # The non-stratified age effects
        model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[paste0('age', non.stratified.age.indices,
                                                                                                 '.', category, '.susceptibility.rr')] *
                                                                                age.multipliers[non.stratified.age.indices],
                                                                      dimension = 'age.to',
                                                                      applies.to.dimension.values = ages[non.stratified.age.indices])
        
        # The race-stratified age effects
        for (age.index in stratified.age.indices)
        { 
            for (race in races)
            {
                if (time==2)
                    value = parameters[paste0('age', age.index, '.', race, '.', category, '.susceptibility.rr.', time)]
                else
                    value = parameters[paste0('age', age.index, '.', race, '.', category, '.susceptibility.rr.01')]
                
                model.settings$set.element.functional.form.interaction.alphas(element.name = elem.name,
                                                                              alpha.name = alpha.name,
                                                                              value = value * age.multipliers[age.index],
                                                                              applies.to.dimension.values = list(race.to = race,
                                                                                                                 age.to = ages[age.index]))
            }
        }

    }
    
    # After Modifier
    param.name = paste0(category, '.fraction.trate.change.after.t2')
    model.settings$set.element.functional.form.main.effect.alphas(element.name = elem.name,
                                                                  alpha.name = 'after.modifier',
                                                                  values = parameters[param.name],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
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
                                       times = c(1,2)) # must align with idu_input_manager
{
    specification.metadata = model.settings$specification.metadata
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    
    for (time in times)
    {
        alpha.name = paste0('time',time)
        
        param.names = paste0(races, '.incident.idu.multiplier.', time)
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[param.names],
                                                                      applies.to.dimension.values = races,
                                                                      dimension = 'race')
        
        param.names = paste0('age', 1:length(ages), '.incident.idu.multiplier') #no time varying component
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[param.names],
                                                                      applies.to.dimension.values = ages,
                                                                      dimension = 'age')
        
        # param.name = paste0('young.incident.idu.multiplier.', time)
        # model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
        #                                                               alpha.name = alpha.name,
        #                                                               values = parameters[param.name],
        #                                                               applies.to.dimension.values = 1, # first age only 
        #                                                               dimensions = 'age')
        
        param.name = paste0('msm.incident.idu.multiplier.', time)
        model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.incidence',
                                                                      alpha.name = alpha.name,
                                                                      values = parameters[param.name],
                                                                      applies.to.dimension.values = 'msm',
                                                                      dimension = 'sex')
    }
    
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.remission',
                                                                  alpha.name = 'value',
                                                                  values = parameters['idu.remission.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
    
    model.settings$set.element.functional.form.main.effect.alphas(element.name = 'idu.relapse',
                                                                  alpha.name = 'value',
                                                                  values = parameters['idu.relapse.multiplier'],
                                                                  applies.to.dimension.values = 'all',
                                                                  dimension = 'all')
}

