
RYAN.WHITE.PARAMETERS.PRIOR = join.distributions(
  
    non.adap.msm.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.msm.idu.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.idu.male.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.idu.female.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.heterosexual.male.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.heterosexual.female.or = Lognormal.Distribution(0, log(1.5)/2),
  
    non.adap.black.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.hispanic.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.other.or = Lognormal.Distribution(0, log(1.5)/2),
    
    non.adap.age1.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.age2.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.age3.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.age4.or = Lognormal.Distribution(0, log(1.5)/2),
    non.adap.age5.or = Lognormal.Distribution(0, log(1.5)/2)
  
  )

ryan.white.apply.set.parameters <- function(model.settings, parameters)
{
    specification.metadata = model.settings$specification.metadata
    
    idu.states = specification.metadata$compartment.aliases$active.idu.states
    non.idu.states = setdiff(specification.metadata$dim.names$risk, idu.states)
    
    races = specification.metadata$dim.names$race
    ages = specification.metadata$dim.names$age
    age.indices = 1:length(ages)
    n.ages = length(ages)
    
    
    ##---------------------------------##
    ##-- PROPORTION WITH NON-ADAP RW --##
    ##---------------------------------##
    
    # Sex/Risk Terms
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.msm.or'],
                                                   applies.to.dimension.values = list(sex='msm', risk=non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.msm.idu.or'],
                                                   applies.to.dimension.values = list(sex='msm', risk=idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.heterosexual.male.or'],
                                                   applies.to.dimension.values = list(sex='heterosexual_male', risk=non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.idu.male.or'],
                                                   applies.to.dimension.values = list(sex='heterosexual_male', risk=idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.heterosexual.female.or'],
                                                   applies.to.dimension.values = list(sex='female', risk=non.idu.states))
    
    set.element.functional.form.interaction.alphas(model.settings,
                                                   element.name = 'proportion.pwh.with.non.adap.rw', 
                                                   alpha.name = 'intercept', 
                                                   value = parameters['non.adap.idu.female.or'],
                                                   applies.to.dimension.values = list(sex='female', risk=idu.states))
    
    # Race Terms
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "proportion.pwh.with.non.adap.rw",
                                                   alpha.name = 'intercept',
                                                   values = parameters[paste0('non.adap.', races,'.or')],
                                                   dimension = 'race',
                                                   applies.to.dimension.values=races)
    
    # Age Terms
    
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "proportion.pwh.with.non.adap.rw",
                                                   alpha.name = 'intercept',
                                                   values = parameters[paste0('non.adap.age', age.indices,'.or')],
                                                   dimension = 'age',
                                                   applies.to.dimension.values=ages)
    
    ##-----------------------------------##
    ##-- PROPORTION NON-ADAP WITH OAHS --##
    ##-----------------------------------##
    
    ##-----------------------------------##
    ##-- PROPORTION NON-ADAP WITH ADAP --##
    ##-----------------------------------##
    
    ##---------------------------##
    ##-- PROPORTION SUPPRESSED --##
    ##---------------------------##
    
   
}