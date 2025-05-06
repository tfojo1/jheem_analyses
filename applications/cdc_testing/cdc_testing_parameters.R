
CDC.FUNDED.DIAGNOSES.SD = log(1.5)/2
CDC.FUNDED.TESTS.SD = log(1.5)/2

CDC.TESTING.PARAMETERS.PRIOR = join.distributions(
    
    cdc.funded.diagnoses.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    
    cdc.funded.diagnoses.msm.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.msm.idu.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.idu.male.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.idu.female.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.heterosexual.male.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.heterosexual.female.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    
    cdc.funded.diagnoses.black.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.hispanic.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.other.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    
    cdc.funded.diagnoses.age1.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.age2.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.age3.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.age4.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    cdc.funded.diagnoses.age5.or = Lognormal.Distribution(0, CDC.FUNDED.DIAGNOSES.SD),
    
    cdc.funded.tests.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    
    cdc.funded.tests.msm.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.msm.idu.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.idu.male.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.idu.female.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.heterosexual.male.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.heterosexual.female.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    
    cdc.funded.tests.black.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.hispanic.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.other.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    
    cdc.funded.tests.age1.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.age2.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.age3.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.age4.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD),
    cdc.funded.tests.age5.or = Lognormal.Distribution(0, CDC.FUNDED.TESTS.SD)
)





cdc.testing.apply.set.parameters <- function(model.settings, parameters)
{
    specification.metadata = model.settings$specification.metadata
    
    non.idu.states = specification.metadata$compartment.aliases$never.idu.states
    idu.states = setdiff(specification.metadata$dim.names$risk, non.idu.states)
    
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    age.indices = 1:length(ages)
    n.ages = length(ages)
    
    
    parameter.prefixes = c(
        fraction.diagnoses.from.cdc = 'cdc.funded.diagnoses.'
        
    )
    element.names = names(parameter.prefixes)
    
    for (i in 1:length(element.names))
    {
        element.name = element.names[i]
        parameter.prefix = parameter.prefixes[i]
        
        # All
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       value = parameters[paste0(parameter.prefix, 'or')],
                                                       dimension = 'all',
                                                       applies.to.dimension.values='all')
        
        # Sex/Risk Terms
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'msm.or')],
                                                       applies.to.dimension.values = list(sex='msm', risk=non.idu.states))
        
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'msm.idu.or')],
                                                       applies.to.dimension.values = list(sex='msm', risk=idu.states))
        
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'heterosexual.male.or')],
                                                       applies.to.dimension.values = list(sex='heterosexual_male', risk=non.idu.states))
        
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'idu.male.or')],
                                                       applies.to.dimension.values = list(sex='heterosexual_male', risk=idu.states))
        
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'heterosexual.female.or')],
                                                       applies.to.dimension.values = list(sex='female', risk=non.idu.states))
        
        set.element.functional.form.interaction.alphas(model.settings,
                                                       element.name = element.name, 
                                                       alpha.name = 'intercept', 
                                                       value = parameters[paste0(parameter.prefix, 'idu.female.or')],
                                                       applies.to.dimension.values = list(sex='female', risk=idu.states))
        
        # Race Terms
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       values = parameters[paste0(parameter.prefix, races,'.or')],
                                                       dimension = 'race',
                                                       applies.to.dimension.values=races)
        
        if (!is.na(parameters[paste0(parameter.prefix,'slope.or')]))
        {
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = element.name,
                                                           alpha.name = 'slope',
                                                           values = parameters[paste0(parameter.prefix,'slope.or')],
                                                           dimension = 'all',
                                                           applies.to.dimension.values='all')
            
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = element.name,
                                                           alpha.name = 'slope',
                                                           values = parameters[paste0(parameter.prefix, races,'.slope.or')],
                                                           dimension = 'race',
                                                           applies.to.dimension.values=races)
        }
        
        # Age Terms
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
                                                       dimension = 'age',
                                                       applies.to.dimension.values=ages)
        
    }
    
   
}

CDC.TESTING.PARAMETER.SAMPLING.BLOCKS = list(
    
)