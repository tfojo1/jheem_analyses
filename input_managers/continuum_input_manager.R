

get.suppression.functional.form <- function(specification.metadata,
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