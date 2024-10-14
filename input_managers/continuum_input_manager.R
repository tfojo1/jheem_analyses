

get.suppression.functional.form <- function(specification.metadata,
                                                max.suppressed.proportion=0.9,
                                                anchor.year = 2008,
                                            include.slope = T)
{
    covariates = c('age','race','sex')
    df.col.names = c('year', covariates,'risk','p','weight')
  
    #-- Pull Prevalence --#
    arr.prev = SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk[,'US',,,,]
    prev.years = dimnames(arr.prev)$year
    
    #-- Prep data frame --#
    
    suppression.stratifications = names(SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.hiv$cdc)
    suppression.stratifications = suppression.stratifications[sapply(strsplit(suppression.stratifications, "__"), length)==3]
    
    raw.df = NULL
    for (stratification in suppression.stratifications)
    {
      
        if (length(strsplit(stratification, "__")[[1]])==3)
        {
            d = strsplit(stratification, "__")[[1]][3]
            #suppression.arr = national.surveillance[[paste0("suppression.", d)]][,'national',]
            suppression.arr = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.supplemental.reports$cdc[[stratification]][,'US',]
            suppression.years = intersect(dimnames(suppression.arr)$year[!apply(is.na(suppression.arr), 'year', all)],
                                          prev.years)
            suppression.arr = suppression.arr[suppression.years,,drop=F]
        }
        else
        {
            suppression.arr = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.hiv$cdc[[stratification]][,'US',,]
            suppression.years = intersect(dimnames(suppression.arr)$year[!apply(is.na(suppression.arr), 'year', all)],
                                          prev.years)
            suppression.arr = suppression.arr[suppression.years,,,drop=F]
        }
        
        aggregated.prev.arr = apply(arr.prev[suppression.years,,,,,drop=F], names(dim(suppression.arr)), sum, na.rm=T)  
        
        mapping = get.ontology.mapping(from.ontology = dimnames(aggregated.prev.arr),
                                       to.ontology = dimnames(suppression.arr))
        if (is.null(mapping))
            stop(paste0("Error get.suppression.functional.form() - cannot map prevalence to suppression for stratification '", stratification, "'"))
        aggregated.prev.arr = mapping$apply(aggregated.prev.arr, na.rm = T)
        
        sub.df = reshape2::melt(suppression.arr, value.name = 'p')
        sub.df$weight = as.numeric(aggregated.prev.arr)
        
        missing.columns = setdiff(df.col.names, names(sub.df))
        for (col in missing.columns)
            sub.df[,col] = 'all'
        
        sub.df = sub.df[!is.na(sub.df$p),df.col.names]
        
        raw.df = rbind(raw.df, sub.df)
        
        # mappings = get.mappings.to.align.ontologies(ontology.1 = specification.metadata$dim.names[setdiff(names(dim(arr.prev)), 'year')],
        #                                 ontology.2 = dimnames(suppression.arr)[setdiff(names(dim(suppression.arr)), 'year')])
    }
    
    raw.df = raw.df[raw.df$sex!='male' | raw.df$risk != 'all',]
    raw.df$sex[raw.df$risk=='msm' | raw.df$risk=='msm_idu'] = 'male'
    
    df = restratify.data.to.specification(data = raw.df,
                                          dim.names = specification.metadata$dim.names,
                                          max.age = 85)
    
    if (any(df$p > max.suppressed.proportion))
        stop(paste0("Error in get.suppression.functional.form() - cannot use ", max.suppressed.proportion, " as max.suppressed.proportion, because the data contain suppression up to ", max(df$p)))
    df$p = df$p/max.suppressed.proportion
    
    
    df$year = df$year - anchor.year
    for (covariate in covariates)
    {
        df[,covariate] = factor(df[,covariate], levels = c('all', setdiff(unique(df[,covariate]), 'all')))
    }
    
    ff = as.formula(paste0("p ~ ", paste0(covariates, "*year", collapse=' + ')))
    fit = suppressWarnings(glm(formula = ff,
                               data = df,
                               family = 'binomial',
                               weights = df$weight))
    
    #-- Use predict() to get the logistic intercept and slope --#
    
    dim.names = specification.metadata$dim.names[covariates]
    fake.data.array = array(0, dim = sapply(dim.names, length), dimnames = dim.names)
    intercept.data.values = reshape2::melt(fake.data.array, value.name = 'year')
    slope.data.values = intercept.data.values
    slope.data.values$year = 1
    
    intercept = suppressWarnings(predict(fit, newdata=intercept.data.values, type='link'))
    slope = suppressWarnings(predict(fit, newdata=slope.data.values, type='link')) - intercept
    
    dim(intercept) = dim(slope) = sapply(dim.names, length)
    dimnames(intercept) = dimnames(slope) = dim.names
 
    if (1==2)
    {
        ggplot(data=df, aes(year, p)) + geom_point() + geom_smooth()
    }
    #-- Return --#
    
    if (!include.slope)
        slope = 0
    
    create.logistic.linear.functional.form(intercept = intercept,
                                           slope = slope,
                                           anchor.year = anchor.year,
                                           max = max.suppressed.proportion,
                                           min = 0,
                                           parameters.are.on.logit.scale = T)
}

OLD.get.suppression.functional.form <- function(specification.metadata,
                                            national.surveillance,
                                            max.suppressed.proportion=0.9,
                                            anchor.year=2020)
{   
    #-- Fit Sex x Risk --#
    
    df = reshape2::melt(national.surveillance$suppression.sex.risk)
    counts = reshape2::melt(national.surveillance$prevalence.for.continuum.sex.risk)
    df$n = counts$value
    df = df[df$sex != 'female' | (df$risk!='msm' & df$risk!='msm_idu'),]
    
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df$year = df$year - anchor.year
    
    df$msm = as.numeric(df$risk=='msm')
    df$msm_idu = as.numeric(df$risk=='msm_idu')
    df$heterosexual_female = as.numeric(df$risk=='heterosexual' & df$sex=='female')
    df$heterosexual_male = as.numeric(df$risk=='heterosexual' & df$sex=='male')
    df$idu_female = as.numeric(df$risk=='idu' & df$sex=='female')
    df$idu_male = as.numeric(df$risk=='idu' & df$sex=='male')
    
    fit.sex.risk = suppressWarnings(glm(value ~ msm + year:msm +
                                          msm_idu + year:msm_idu +
                                          heterosexual_female + year:heterosexual_female +
                                          heterosexual_male + year:heterosexual_male +
                                          idu_female + year:idu_female +
                                          idu_male + year:idu_male +
                                          0,
                                        data=df, family='binomial'))
    
    #-- The 'All' Data Frame --#
    
    df.all = reshape2::melt(national.surveillance$suppression.all)
    df.all$n = national.surveillance$prevalence.for.continuum.all[,1]
    
    df.all$value = pmin(df.all$value / max.suppressed.proportion, .9999)
    
    #-- Fit Race --#
    
    df = reshape2::melt(national.surveillance$suppression.race)
    df$n = reshape2::melt(national.surveillance$prevalence.for.continuum.all)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          race='all',
                          value=df.all$value,
                          n=df.all$n))
    
    df$black = as.numeric(df$race=='black')
    df$hispanic = as.numeric(df$race=='hispanic')
    df$other = as.numeric(df$race=='other')
    
    df$year = df$year - anchor.year
    
    fit.race = suppressWarnings(glm(value ~ black + hispanic + other +
                                      black:year + hispanic:year + other:year + year,
                                    data=df, family='binomial', weight=n))
    
    #-- Fit Age --#
    
    df = reshape2::melt(national.surveillance$suppression.age)
    df$n = reshape2::melt(national.surveillance$prevalence.for.continuum.age)$value
    df$value = pmin(df$value / max.suppressed.proportion, .9999)
    
    ages = sort(unique(as.character(df$age)))
    
    df = rbind(df,
               data.frame(year=df.all$year,
                          location='national',
                          age='all',
                          value=df.all$value,
                          n=df.all$n))
    
    
    for (age in 1:length(ages))
        df[,paste0('age',age)] = as.numeric(df$age==ages[age])
    
    df$year = df$year - anchor.year
    
    ff = as.formula(paste0('value ~ ',
                           paste0('age', 1:length(ages), collapse=' + '),
                           " + ",
                           paste0('year:age', 1:length(ages), collapse=' + '),
                           ' + year'))
    
    fit.age = suppressWarnings(glm(ff, data=df, family='binomial'))
    
    
    
    #-- Unpack the intercepts and slopes into arrays indexed [age,race,sex,risk] --#
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    stratified.log.odds.intercept = stratified.log.odds.slope =
      array(0, dim=sapply(dim.names, length), dimnames=dim.names)
    
    n.ages = length(dim.names[['age']])
    for (age in 1:n.ages)
    {
      stratified.log.odds.intercept[age,,,] = stratified.log.odds.intercept[age,,,] + 
        fit.age$coefficients[paste0('age',age)]
      stratified.log.odds.slope[age,,,] = stratified.log.odds.slope[age,,,] + 
        fit.age$coefficients[paste0('age',age,':year')]
    }

    for (race in specification.metadata$dim.names$race)
    {
      stratified.log.odds.intercept[,race,,] = stratified.log.odds.intercept[,race,,] + 
        fit.race$coefficients[race]
      stratified.log.odds.slope[,race,,] = stratified.log.odds.slope[,race,,] + 
        fit.race$coefficients[paste0(race,":year")]
    }

    idu.strata = setdiff(specification.metadata$dim.names$risk, 'never_IDU')
    non.idu.strata = 'never_IDU'
    
    
    # Non-IDU, heterosexual female
    stratified.log.odds.intercept[,,'female',non.idu.strata] = 
      stratified.log.odds.intercept[,,'female',non.idu.strata] + 
      fit.sex.risk$coefficients['heterosexual_female']
    stratified.log.odds.slope[,,'female',non.idu.strata] = 
      stratified.log.odds.slope[,,'female',non.idu.strata] + 
      fit.sex.risk$coefficients['year:heterosexual_female']
    
    # Non-IDU, heterosexual male
    stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] = 
      stratified.log.odds.intercept[,,'heterosexual_male',non.idu.strata] + 
      fit.sex.risk$coefficients['heterosexual_male']
    stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] = 
      stratified.log.odds.slope[,,'heterosexual_male',non.idu.strata] + 
      fit.sex.risk$coefficients['year:heterosexual_male']
    
    # IDU female
    stratified.log.odds.intercept[,,'female',idu.strata] = 
      stratified.log.odds.intercept[,,'female',idu.strata] + 
      fit.sex.risk$coefficients['idu_female']
    stratified.log.odds.slope[,,'female',idu.strata] = 
      stratified.log.odds.slope[,,'female',idu.strata] + 
      fit.sex.risk$coefficients['year:idu_female']
    
    # IDU, heterosexual male
    stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] = 
      stratified.log.odds.intercept[,,'heterosexual_male',idu.strata] + 
      fit.sex.risk$coefficients['idu_male']
    stratified.log.odds.slope[,,'heterosexual_male',idu.strata] = 
      stratified.log.odds.slope[,,'heterosexual_male',idu.strata] + 
      fit.sex.risk$coefficients['year:idu_male']
     
    # Non-IDU MSM
    stratified.log.odds.intercept[,,'msm',non.idu.strata] = 
      stratified.log.odds.intercept[,,'msm',non.idu.strata] + 
      fit.sex.risk$coefficients['msm']
    stratified.log.odds.slope[,,'msm',non.idu.strata] = 
      stratified.log.odds.slope[,,'msm',non.idu.strata] + 
      fit.sex.risk$coefficients['msm:year']
    
    # IDU MSM
    stratified.log.odds.intercept[,,'msm',idu.strata] = 
      stratified.log.odds.intercept[,,'msm',idu.strata] + 
      fit.sex.risk$coefficients['msm_idu']
    stratified.log.odds.slope[,,'msm',idu.strata] = 
      stratified.log.odds.slope[,,'msm',idu.strata] + 
      fit.sex.risk$coefficients['year:msm_idu']
    
    #-- Return --#
    create.logistic.linear.functional.form(intercept = stratified.log.odds.intercept,
                                           slope = stratified.log.odds.slope,
                                           anchor.year=anchor.year,
                                           max=max.suppressed.proportion,
                                           min=0)
}