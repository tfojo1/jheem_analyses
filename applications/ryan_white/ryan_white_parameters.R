
RW.PARAM.SD = log(1.5)/2

RYAN.WHITE.PARAMETERS.PRIOR = join.distributions(
  
    #-- Non-ADAP --#
    non.adap.msm.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.msm.idu.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.idu.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.idu.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.heterosexual.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.heterosexual.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
  
    non.adap.black.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.hispanic.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.other.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    non.adap.age1.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.age2.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.age3.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.age4.or = Lognormal.Distribution(0, RW.PARAM.SD),
    non.adap.age5.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    
    #-- OAHS given non-ADAP --#
    
    oahs.msm.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.msm.idu.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.idu.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.idu.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.heterosexual.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.heterosexual.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    oahs.black.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.hispanic.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.other.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    oahs.age1.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.age2.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.age3.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.age4.or = Lognormal.Distribution(0, RW.PARAM.SD),
    oahs.age5.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    
    #-- ADAP given non-ADAP --#
    
    adap.msm.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.msm.idu.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.idu.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.idu.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.heterosexual.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.heterosexual.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    adap.black.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.hispanic.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.other.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    adap.age1.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.age2.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.age3.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.age4.or = Lognormal.Distribution(0, RW.PARAM.SD),
    adap.age5.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    proportion.adap.without.non.adap.rw = Logitnormal.Distribution(),
    
    #-- SUPPRESSION --#
    
    rw.suppression.msm.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.msm.idu.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.idu.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.idu.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.heterosexual.male.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.heterosexual.female.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    rw.suppression.black.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.hispanic.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.other.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    rw.suppression.age1.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.age2.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.age3.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.age4.or = Lognormal.Distribution(0, RW.PARAM.SD),
    rw.suppression.age5.or = Lognormal.Distribution(0, RW.PARAM.SD),
    
    adap.vs.oahs.suppression.or = Lognormal.Distribution(-0.6664765, 0.7653844),
    non.oahs.vs.oahs.suppression.or = Lognormal.Distribution(log(0.8/1.5)) #https://pmc.ncbi.nlm.nih.gov/articles/PMC5848228/, figure 2, panel B, 'support' vs 'core'
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
    
    
    parameter.prefixes = c(
        proportion.pwh.with.non.adap.rw = 'non.adap.',
        proportion.non.adap.rw.with.oahs = 'oahs',
        proportion.non.adap.rw.with.adap = 'adap.',
        proportion.adap.suppressed = 'rw.suppression.',
        proportion.oahs.without.adap.suppressed = 'rw.suppression.',
        proportion.rw.without.adap.or.oahs.suppressed = 'rw.suppression.'
    )
    element.names = names(parameter.prefixes)
    
    for (i in 1:length(element.names))
    {
        element.name = element.names[i]
        parameter.prefix = parameter.prefixes[i]
      
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
        
        # Age Terms
        
        set.element.functional.form.main.effect.alphas(model.settings,
                                                       element.name = element.name,
                                                       alpha.name = 'intercept',
                                                       values = parameters[paste0(parameter.prefix, 'age', age.indices,'.or')],
                                                       dimension = 'age',
                                                       applies.to.dimension.values=ages)
        
    }

    # Suppression: ADAP vs OAHS
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "proportion.adap.suppressed",
                                                   alpha.name = 'intercept',
                                                   values = parameters['adap.vs.oahs.suppression.or'],
                                                   dimension = 'all',
                                                   applies.to.dimension.values='all')
    
    # Suppression: non-OAHS RW services vs OAHS
    set.element.functional.form.main.effect.alphas(model.settings,
                                                   element.name = "proportion.rw.without.adap.or.oahs.suppressed",
                                                   alpha.name = 'intercept',
                                                   values = parameters['non.oahs.vs.oahs.suppression.or'],
                                                   dimension = 'all',
                                                   applies.to.dimension.values='all')
}


if (1==2)
{
    # The OR for adap vs oahs
    adap.supp = RW.DATA.MANAGER$data$adap.suppression$estimate$nastad.adap$ryan.white.pdfs$year__location
    oahs.supp = RW.DATA.MANAGER$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location
    
    supp.locs = intersect(dimnames(adap.supp)$location, dimnames(oahs.supp)$location)
    supp.years = intersect(dimnames(adap.supp)$year, dimnames(oahs.supp)$year)
    
    adap.supp = adap.supp[supp.years, supp.locs]
    oahs.supp = oahs.supp[supp.years, supp.locs]
    
    adap.supp.log.odds = log(adap.supp) - log(1-adap.supp)
    oahs.supp.log.odds = log(oahs.supp) - log(1-oahs.supp)
    
    supp.log.ors = adap.supp.log.odds - oahs.supp.log.odds
    # chop off outliers (from plot)
    supp.log.ors = supp.log.ors[!is.na(supp.log.ors) & supp.log.ors>-3]
    
    mean(supp.log.ors) #-0.6664765
    sd(supp.log.ors) #0.7653844
    
    # Number with ADAP with no other RW services
    # https://pmc.ncbi.nlm.nih.gov/articles/PMC5848228/
    # Table 1
    n.adap.only = 1098
    n.any.rw = 855+298+1098+1536+1353+178+2228
    n.any.adap = 1098 + 1353 + 178 + 2228
    
    p.adap.only.of.adap = n.adap.only / n.any.adap
}