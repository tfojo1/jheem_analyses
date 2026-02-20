

CDC.FUNDED.DIAGNOSES.SD = log(1.5)/2
CDC.DIAGNOSES.TOTAL.SD = log(10)/2
CDC.TESTS.TOTAL.SD = log(10)/2
CDC.FUNDED.TESTS.SD = log(1.5)/2

CDC.TESTING.PARAMETERS.PRIOR = join.distributions(
    
    cdc.funded.diagnoses.or0 = Lognormal.Distribution(0, CDC.DIAGNOSES.TOTAL.SD),
    cdc.funded.diagnoses.or1 = Lognormal.Distribution(0, CDC.DIAGNOSES.TOTAL.SD),
    cdc.funded.diagnoses.or2 = Lognormal.Distribution(0, CDC.DIAGNOSES.TOTAL.SD),
    cdc.funded.diagnoses.change.after.2 = Normal.Distribution(0.5, 0.1, lower = 0),
    
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
    
    cdc.funded.tests.or = Lognormal.Distribution(0, CDC.TESTS.TOTAL.SD),
    cdc.funded.tests.slope.or = Lognormal.Distribution(0, CDC.TESTS.TOTAL.SD),
    
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



PREP.ELIGIBLE.SD = log(1.5)/2
PREP.ELIGIBLE.TOTAL.SD = log(10)/2
PREP.CDC.SD = log(1.5)/2
PREP.CDC.TOTAL.SD = log(10)/2
#PREP.REFERRED.TOTAL.SD = log(10)/2
#PREP.REFERRED.SD = log(1.5)/2

CDC.PREP.PARAMETERS.PRIOR = join.distributions(
    
    CDC.TESTING.PARAMETERS.PRIOR,
    
    prep.eligible.or = Lognormal.Distribution(0, PREP.ELIGIBLE.TOTAL.SD),
    #prep.eligible.slope.or = Lognormal.Distribution(0, PREP.ELIGIBLE.TOTAL.SD),
    
    prep.eligible.msm.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.msm.idu.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.idu.male.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.idu.female.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.heterosexual.male.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.heterosexual.female.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    
    prep.eligible.black.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.hispanic.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.other.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    
    prep.eligible.age1.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.age2.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.age3.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.age4.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    prep.eligible.age5.or = Lognormal.Distribution(0, PREP.ELIGIBLE.SD),
    
    prep.cdc.or = Lognormal.Distribution(0, PREP.CDC.TOTAL.SD),
    #prep.eligible.slope.or = Lognormal.Distribution(0, PREP.ELIGIBLE.TOTAL.SD),
    
    prep.cdc.msm.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.msm.idu.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.idu.male.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.idu.female.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.heterosexual.male.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.heterosexual.female.or = Lognormal.Distribution(0, PREP.CDC.SD),
    
    prep.cdc.black.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.hispanic.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.other.or = Lognormal.Distribution(0, PREP.CDC.SD),
    
    prep.cdc.age1.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.age2.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.age3.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.age4.or = Lognormal.Distribution(0, PREP.CDC.SD),
    prep.cdc.age5.or = Lognormal.Distribution(0, PREP.CDC.SD),
    
    
    # prep.referred.or = Lognormal.Distribution(0, PREP.REFERRED.TOTAL.SD),
    # prep.referred.slope.or = Lognormal.Distribution(0, PREP.REFERRED.TOTAL.SD),
    # 
    # prep.referred.msm.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.msm.idu.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.idu.male.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.idu.female.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.heterosexual.male.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.heterosexual.female.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # 
    # prep.referred.black.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.hispanic.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.other.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # 
    # prep.referred.age1.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.age2.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.age3.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.age4.or = Lognormal.Distribution(0, PREP.REFERRED.SD),
    # prep.referred.age5.or = Lognormal.Distribution(0, PREP.REFERRED.SD),

    fraction.cdc.tests.unique = Beta.Distribution(7.2,7.2),
    
    rr.fraction.index.diagnoses.that.yield.a.positive.contact = Lognormal.Distribution(0, log(2)/2)
    
    #fraction.cdc.funded.tests.in.non.healthcare.settings = Beta.Distribution(1,1)
        
)


cdc.prep.apply.set.parameters <- function(model.settings, parameters)
{
    
    specification.metadata = model.settings$specification.metadata

    non.idu.states = specification.metadata$compartment.aliases$never.idu.states
    idu.states = setdiff(specification.metadata$dim.names$risk, non.idu.states)

    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    age.indices = 1:length(ages)
    n.ages = length(ages)


# parameter.prefixes = c(
#     #   fraction.diagnoses.from.cdc = 'cdc.funded.diagnoses.',
#     fraction.cdc.referred.to.prep = 'prep.referred.'
# 
# )
# element.names = names(parameter.prefixes)
# 
# for (i in 1:length(element.names))
# {
#     element.name = element.names[i]
#     parameter.prefix = parameter.prefixes[i]
# 
#     # All
#     set.element.functional.form.main.effect.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'or')],
#                                                    dimension = 'all',
#                                                    applies.to.dimension.values='all')
# 
#     # Sex/Risk Terms
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'msm.or')],
#                                                    applies.to.dimension.values = list(sex='msm', risk=non.idu.states))
# 
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'msm.idu.or')],
#                                                    applies.to.dimension.values = list(sex='msm', risk=idu.states))
# 
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'heterosexual.male.or')],
#                                                    applies.to.dimension.values = list(sex='heterosexual_male', risk=non.idu.states))
# 
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'idu.male.or')],
#                                                    applies.to.dimension.values = list(sex='heterosexual_male', risk=idu.states))
# 
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'heterosexual.female.or')],
#                                                    applies.to.dimension.values = list(sex='female', risk=non.idu.states))
# 
#     set.element.functional.form.interaction.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    value = parameters[paste0(parameter.prefix, 'idu.female.or')],
#                                                    applies.to.dimension.values = list(sex='female', risk=idu.states))
# 
#     # Race Terms
# 
#     set.element.functional.form.main.effect.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    values = parameters[paste0(parameter.prefix, races,'.or')],
#                                                    dimension = 'race',
#                                                    applies.to.dimension.values=races)
# 
#     if (!is.na(parameters[paste0(parameter.prefix,'slope.or')]))
#     {
#         set.element.functional.form.main.effect.alphas(model.settings,
#                                                        element.name = element.name,
#                                                        alpha.name = 'slope',
#                                                        values = parameters[paste0(parameter.prefix,'slope.or')],
#                                                        dimension = 'all',
#                                                        applies.to.dimension.values='all')
# 
#         #set.element.functional.form.main.effect.alphas(model.settings,
#         # element.name = element.name,
#         # alpha.name = 'slope',
#         # values = parameters[paste0(parameter.prefix, races,'.slope.or')],
#         # dimension = 'race',
#         # applies.to.dimension.values=races)
#     }
# 
#     # Age Terms
# 
#     set.element.functional.form.main.effect.alphas(model.settings,
#                                                    element.name = element.name,
#                                                    alpha.name = 'intercept',
#                                                    values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
#                                                    dimension = 'age',
#                                                    applies.to.dimension.values=ages)
# 
# }
    
    parameter.prefixes = c(
        fraction.prep.from.cdc = 'prep.cdc.'
        
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
            
            #set.element.functional.form.main.effect.alphas(model.settings,
            # element.name = element.name,
            # alpha.name = 'slope',
            # values = parameters[paste0(parameter.prefix, races,'.slope.or')],
            # dimension = 'race',
            # applies.to.dimension.values=races)
        }
        
        # Age Terms
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
                                                       dimension = 'age',
                                                       applies.to.dimension.values=ages)
        
    }
    

    parameter.prefixes = c(
        fraction.cdc.tested.prep.eligible = 'prep.eligible.'
        #fraction.tests.from.cdc = 'cdc.funded.tests.'

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
            
            #set.element.functional.form.main.effect.alphas(model.settings,
            # element.name = element.name,
            # alpha.name = 'slope',
            # values = parameters[paste0(parameter.prefix, races,'.slope.or')],
            # dimension = 'race',
            # applies.to.dimension.values=races)
        }
        
        # Age Terms
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
                                                       dimension = 'age',
                                                       applies.to.dimension.values=ages)
        
    }
    
    
    parameter.prefixes = c(
        fraction.diagnoses.from.cdc = 'cdc.funded.diagnoses.'
        #fraction.tests.from.cdc = 'cdc.funded.tests.'
        
    )
    element.names = names(parameter.prefixes)
    
    
    
    for (i in 1:length(element.names))
    {
        element.name = element.names[i]
        parameter.prefix = parameter.prefixes[i]
        
        for(knot in 0:2){
            
            alpha.name = paste0("time",knot)
            # All
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = element.name,
                                                           alpha.name = alpha.name,
                                                           value = parameters[paste0(parameter.prefix, 'or',knot)],
                                                           dimension = 'all',
                                                           applies.to.dimension.values='all')
            
            
      
            
            # Sex/Risk Terms
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'msm.or')],
                                                           applies.to.dimension.values = list(sex='msm', risk=non.idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'msm.idu.or')],
                                                           applies.to.dimension.values = list(sex='msm', risk=idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'heterosexual.male.or')],
                                                           applies.to.dimension.values = list(sex='heterosexual_male', risk=non.idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'idu.male.or')],
                                                           applies.to.dimension.values = list(sex='heterosexual_male', risk=idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'heterosexual.female.or')],
                                                           applies.to.dimension.values = list(sex='female', risk=non.idu.states))
            
            set.element.functional.form.interaction.alphas(model.settings,
                                                           element.name = element.name, 
                                                           alpha.name = alpha.name, 
                                                           value = parameters[paste0(parameter.prefix, 'idu.female.or')],
                                                           applies.to.dimension.values = list(sex='female', risk=idu.states))
            
            # Race Terms
            
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = element.name,
                                                           alpha.name = alpha.name,
                                                           values = parameters[paste0(parameter.prefix, races,'.or')],
                                                           dimension = 'race',
                                                           applies.to.dimension.values=races)
            
            # Age Terms
            
            set.element.functional.form.main.effect.alphas(model.settings,
                                                           element.name = element.name,
                                                           alpha.name = alpha.name,
                                                           values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
                                                           dimension = 'age',
                                                           applies.to.dimension.values=ages)
            
            model.settings$set.element.functional.form.main.effect.alphas(element.name = element.name,
                                                                          alpha.name = 'after.modifier',
                                                                          values = parameters["cdc.funded.diagnoses.change.after.2"],
                                                                          applies.to.dimension.values = "all",
                                                                          dimension = 'all')
            
            
            
            
            
        }
    }
        
    parameter.prefixes = c(
        #   fraction.diagnoses.from.cdc = 'cdc.funded.diagnoses.',
        fraction.tests.from.cdc = 'cdc.funded.tests.'
        
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
            
            #set.element.functional.form.main.effect.alphas(model.settings,
            # element.name = element.name,
            # alpha.name = 'slope',
            # values = parameters[paste0(parameter.prefix, races,'.slope.or')],
            # dimension = 'race',
            # applies.to.dimension.values=races)
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
    cdc.funded.diagnoses = c("cdc.funded.diagnoses.or0","cdc.funded.diagnoses.or1","cdc.funded.diagnoses.or2","cdc.funded.diagnoses.change.after.2"),
    cdc.funded.diagnoses.by.race = c("cdc.funded.diagnoses.black.or","cdc.funded.diagnoses.hispanic.or","cdc.funded.diagnoses.other.or"),
    cdc.funded.diagnoses.by.risk = c("cdc.funded.diagnoses.msm.or","cdc.funded.diagnoses.msm.idu.or","cdc.funded.diagnoses.idu.male.or","cdc.funded.diagnoses.idu.female.or","cdc.funded.diagnoses.heterosexual.male.or","cdc.funded.diagnoses.heterosexual.female.or"),
    cdc.funded.diagnoses.by.age = c("cdc.funded.diagnoses.age1.or","cdc.funded.diagnoses.age2.or","cdc.funded.diagnoses.age3.or","cdc.funded.diagnoses.age4.or","cdc.funded.diagnoses.age5.or"),
    cdc.funded.tests.by.race = c("cdc.funded.tests.black.or","cdc.funded.tests.hispanic.or","cdc.funded.tests.other.or"),
    cdc.funded.tests.by.risk = c("cdc.funded.tests.msm.or","cdc.funded.tests.msm.idu.or","cdc.funded.tests.idu.male.or","cdc.funded.tests.idu.female.or","cdc.funded.tests.heterosexual.male.or","cdc.funded.tests.heterosexual.female.or"),
    cdc.funded.tests.by.age = c("cdc.funded.tests.age1.or","cdc.funded.tests.age2.or","cdc.funded.tests.age3.or","cdc.funded.tests.age4.or","cdc.funded.tests.age5.or"),
    cdc.funded.tests = c("cdc.funded.tests.or","cdc.funded.tests.slope.or")
    
)


CDC.PREP.PARAMETER.SAMPLING.BLOCKS = c(
    CDC.TESTING.PARAMETER.SAMPLING.BLOCKS,
    list(
        prep.eligible.by.risk = c("prep.eligible.msm.or","prep.eligible.msm.idu.or","prep.eligible.idu.male.or","prep.eligible.idu.female.or","prep.eligible.heterosexual.male.or","prep.eligible.heterosexual.female.or"),
        prep.eligible.by.age = c("prep.eligible.age1.or","prep.eligible.age2.or","prep.eligible.age3.or","prep.eligible.age4.or","prep.eligible.age5.or"),
        prep.eligible.by.race =  c("prep.eligible.black.or","prep.eligible.hispanic.or","prep.eligible.other.or"),
        prep.eligible = c("prep.eligible.or"),
        prep.cdc.by.race = c("prep.cdc.black.or","prep.cdc.hispanic.or","prep.cdc.other.or"),
        prep.cdc.by.risk = c("prep.cdc.msm.or","prep.cdc.msm.idu.or","prep.cdc.idu.male.or","prep.cdc.idu.female.or","prep.cdc.heterosexual.male.or","prep.cdc.heterosexual.female.or"),
        prep.cdc.by.rage = c("prep.cdc.age1.or","prep.cdc.age2.or","prep.cdc.age3.or","prep.cdc.age4.or","prep.cdc.age5.or"),
        prep.cdc = c("prep.cdc.or"),
        prep.unique.total = 'fraction.cdc.tests.unique',
        rr.positive.contact = 'rr.fraction.index.diagnoses.that.yield.a.positive.contact'
        #cdc.tests.non.healthcare ='fraction.cdc.funded.tests.in.non.healthcare.settings'
))

