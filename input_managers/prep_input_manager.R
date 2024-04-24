

if (1==2)
{
    source('applications/EHE/ehe_ontology_mappings.R')
  
  library(ggplot2)
  
  specification.metadata = get.specification.metadata('ehe', 'C.12580')
  ff = get.prep.use.functional.form(specification.metadata)
  ff = get.prep.indication.functional.form(specification.metadata)
  ff = get.prep.persistence.functional.form(specification.metadata)
    years = 2014:2030
    projected.list = ff$project(years)
    
    projected = t(sapply(projected.list, function(x){x}))
    dim.names = c(list(year=as.character(years)),
                  dimnames(projected.list[[1]]))
    dim(projected) = sapply(dim.names, length)
    dimnames(projected) = dim.names
    
    df.race.msm = reshape2::melt(apply(projected[,,,'msm',], c('year','race'), mean))
    ggplot(df.race.msm, aes(year, value, color=race)) + geom_line() + geom_point() + ylim(0,1)
    
    df.age.msm = reshape2::melt(apply(projected[,,,'msm',], c('year','age'), mean))
    ggplot(df.age.msm, aes(year, value, color=age)) + geom_line() + geom_point() + ylim(0,1)
    
    
    df.race.idu = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'active_IDU'], c('year','race'), mean))
    ggplot(df.race.idu, aes(year, value, color=race)) + geom_line() + geom_point() + ylim(0,1)
    
    df.age.idu = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'active_IDU'], c('year','age'), mean))
    ggplot(df.age.idu, aes(year, value, color=age)) + geom_line() + geom_point() + ylim(0,1)
    
    df.sex.idu = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'active_IDU'], c('year','sex'), mean))
    ggplot(df.sex.idu, aes(year, value, color=sex)) + geom_line() + geom_point() + ylim(0,1)
    
    
    df.sex = reshape2::melt(apply(projected, c('year','sex'), mean))
    ggplot(df.sex, aes(year, value, color=sex)) + geom_line() + geom_point() + ylim(0,1)
    
    df.race.het = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'never_IDU'], c('year','race'), mean))
    ggplot(df.race.het, aes(year, value, color=race)) + geom_line() + geom_point() + ylim(0,1)
    
    df.age.het = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'never_IDU'], c('year','age'), mean))
    ggplot(df.age.het, aes(year, value, color=age)) + geom_line() + geom_point() + ylim(0,1)
    
    df.sex.het = reshape2::melt(apply(projected[,,,c('heterosexual_male','female'),'never_IDU'], c('year','sex'), mean))
    ggplot(df.sex.het, aes(year, value, color=sex)) + geom_line() + geom_point() + ylim(0,1)
    
    
    
    ff2 = OLD.get.prep.use.functional.form(specification.metadata)
    ff2 = OLD.get.prep.indication.functional.form(specification.metadata)
    
    projected.list2 = ff2$project(years)
    
    projected2 = t(sapply(projected.list2, function(x){x}))
    dim.names = c(list(year=as.character(years)),
                  dimnames(projected.list2[[1]]))
    dim(projected2) = sapply(dim.names, length)
    dimnames(projected2) = dim.names
    
    df2.race = reshape2::melt(apply(projected2[,,,'msm',], c('year','race'), mean))
    ggplot(df2.race, aes(year, value, color=race)) + geom_line() + geom_point() + ylim(0,1)
    
    df2.age = reshape2::melt(apply(projected2[,,,'msm',], c('year','age'), mean))
    ggplot(df2.age, aes(year, value, color=age)) + geom_line() + geom_point() + ylim(0,1)
    
    
    df3.race = p.msm[p.msm$age=='all',]
    ggplot(df3.race, aes(year, p, color=race)) + geom_line() + geom_point() + ylim(0,1)
    
    df3.age = p.msm[p.msm$race=='all',]
    ggplot(df3.age, aes(year, p, color=age)) + geom_line() + geom_point() + ylim(0,1)
    
    
    
    df2.age = reshape2::melt(apply(projected2[,,,'msm',], c('year','age'), mean))
    ggplot(df2.age, aes(year, value, color=age)) + geom_line() + geom_point() + ylim(0,1)
}

get.prep.use.functional.form <- function(specification.metadata,
                                         anchor.year = 2020,
                                         max.prep.coverage = 0.5)
{
    #-- MSM --#
    # Set up data for MSM
  
    p.msm.raw = rbind(
      
        # ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2015-United-States-tables-REV_20160818.pdf
        # data.frame(
        #     year = 2016,
        #     p = c(4.9, 2.9, 6.5, 8.3, 4.5, 4.9, 4.4, 5.4, 4.9)/100,
        #     race = c('all','all','all','all','all','black','hispanic','other','white'),
        #     age = c('all','18-24 years','25-29 years','30-39 years','40+ years','all','all','all','all'),
        #     weight = c(100, 27., 15.5, 14.6, 42.3, 6.6, 13.6, 71.4, 8.5) / 100
        # ),
        
        ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2016-United-States-tables-REV_20170215.pdf
        data.frame(
            year = 2016,
            p = c(8.6, 4.0, 13.0, 13.6, 8.4, 8.4, 9.3, 8.8, 8.5)/100,
            race = c('all','all','all','all','all','black','hispanic','other','white'),
            age = c('all','18-24 years','25-29 years','30-39 years','40+ years','all','all','all','all'),
            weight = c(100, 26.7, 16.7, 13.9, 42.7, 8.6, 12.9, 69.6, 8.9) / 100
        ),
        
        ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2017-United-States-tables-REV_20171204.pdf
        data.frame(
            year = 2017,
            p = c(12.7, 5.5, 17.6, 21.5, 13.0, 14.0, 10.9, 13.6, 13.0)/100,
            race = c('all','all','all','all','all','black','hispanic','other','white'),
            age = c('all','18-24 years','25-29 years','30-39 years','40+ years','all','all','all','all'),
            weight = c(100, 27.1, 12.4, 15.8, 44.6, 6.5, 15.3, 69.2, 6.8) / 100
        ),
        
        ### PrEP Use in 2018 among MSM (American Men's Internet Survey)
        ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2018-United-States-Report.pdf 
        data.frame(
            year = 2018,
            p = c(13.8, 6.3, 16.2, 22.3, 19.9, 11.6, 12.0, 12.6, 14.6)/100,
            race = c('all','all','all','all','all','black','hispanic','other','white'),
            age = c('all','18-24 years','25-29 years','30-39 years','40+ years','all','all','all','all'),
            weight = c(100, 41.8, 12.9, 14.8, 30.5, 5.5, 16.1, 7.4, 69.2)/100
        ),
        
        ### PrEP Use in 2019 (AMIS) ======
        ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2019-United-States-Report.pdf 
        ## not used - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8488229/ 
        data.frame(
            year = 2019,
            p = c(15.2, 7.8, 21.3, 25.4, 18.9, NA, 12.8, 15.7, 14.5)/100,
            race = c('all','all','all','all','all','black','hispanic','other','white'),
            age = c('all','18-24 years','25-29 years','30-39 years','40+ years','all','all','all','all'),
            weight = c(100, 41.5, 17.9, 14.6, 25.9, 14.9, 15.4, 8.3, 59.4)/100
        )
        
        # PrEP Use in 2021 among MSM (CDC)
        ## https://www.cdc.gov/hiv/pdf/library/reports/cdc-hiv-surveillance-special-report-number-31.pdf
        # data.frame(
        #     year = 2021,
        #     p = c(41.6, 39.7, 40.6, 46.2, 42.9, 33.5, 24.3, 44.1, 54.8, 56.7)/100,
        #     race = c('all','all','all','all','all','all','black','hispanic','other','white'),
        #     age = c('all','18-24 years','25-29 years','30-39 years','40-49 years', '50+ years','all','all','all','all')
        # )
    )
    
    p.msm = restratify.data.to.specification(data = p.msm.raw,
                                             dim.names = specification.metadata$dim.names)
    p.msm$year = p.msm$year - anchor.year
 
    # Fit data for MSM
    fit.msm <- lm(p ~ year + race + age, data = p.msm, weights = p.msm$weight)  
    
    
    # Set up data for non-MSM
    
    p.non.msm.raw = rbind(
      
        ### PrEP Use in 2015 among PWID (NHBS) ----
        ## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf
        # numerator: # took PrEP
        # denominator: # any receptive sharing
        data.frame(
            year = 2015,
            p = c(33/5867, 24/4169, 8/1677, 3/440, 6/837, 13/1588, 6/1285, 5/1717, 7/1586, 9/1320, 17/2620, 0/(64+19+10+240) ),
            race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', 'all', 'all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
            sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
            risk = 'active.idu',
            weight = c(5867, 4169, 1677, 440, 837, 1588, 1285, 1717, 1586, 1320, 2620, (64+19+10+240) )/5867
        ),
        
        ### PrEP Use in 2017 among PWID (NHBS) -------
        ## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
        # numerator: # took PrEP
        # denominator: # any receptive sharing
        data.frame(
          year = 2017,
          p = c(120/6350, 65/4287, 49/2015, 7/290, 13/812, 37/1922, 33/1448, 30/1878, 25/1698, 30/1255, 53/2927, (3+1+8)/(77+20+5+364) ),
          race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
          age = c('all', 'all', 'all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
          sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
          risk = 'active.idu',
          weight = c(6350, 4287, 2015, 290, 812, 1922, 1448, 1878, 1698, 1255, 2927, (77+20+5+364))/6350
        ),
        
        ### PrEP Use in 2016 ----
        ## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-19.pdf
        # denominator: any STI
        data.frame(
          year = 2016,
          p = c(11/(195+70), 2/(77+20), 9/(110+42), 2/(59+8), 2/(46+7), 2/(38+9), 2/(25+19), 3/(19+19), 9/(161+54), 1/(14+8), 0/(3+0), 1/(2+0+1+0+0+0+6+0)),
          race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
          age = c('all', 'all', 'all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
          sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
          risk = 'non.active.idu',
          weight = c((195+70), (77+20), (110+42), (59+8), (46+7), (38+9), (25+19), (19+19), (161+54), (14+8), (3+0), (2+0+1+0+0+0+6+0))/(195+70)
        ),
        
        ### PrEP Use in 2019 ----
        ## https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-26.pdf
        # denominator: any STI
        data.frame(
          year = 2019,
          p = c(42/(283+84), 18/(99+22), 24/(174+55), 5/(74+9), 6/(55+12), 14/(62+14), 12/(40+17), 5/(42+25), 32/(224+65), 4/(31+10), 2/(4+0), (1+3)/(1+1+0+0+0+0+13+1)),
          race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
          age = c('all', 'all', 'all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
          sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
          risk = 'non.active.idu',
          weight = c((283+84), (99+22), (174+55), (74+9), (55+12), (62+14), (40+17), (42+25), (224+65), (31+10), (4+0), (1+1+0+0+0+0+13+1))/(283+84)
        )
    )
    
    non.msm.dim.names = specification.metadata$dim.names
    non.msm.dim.names$sex = setdiff(non.msm.dim.names$sex, 'msm')
    non.msm.dim.names$sex[non.msm.dim.names$sex=='heterosexual_male'] = 'male'
    p.non.msm = restratify.data.to.specification(data = p.non.msm.raw,
                                                 dim.names = non.msm.dim.names)
    p.non.msm$year = p.non.msm$year - anchor.year
    p.non.msm$sex[p.non.msm$sex=='male'] = 'heterosexual_male'
    
    # Fit data for non-MSM
  
    fit.non.msm <- lm(p ~ year + race + age + sex + risk,
                       data = p.non.msm, weights = p.non.msm$weight)
    
    #-- Plug in to Unified Intercept and Slope Arrays --#
    # Set up our intercept/slope arrays
    
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    int = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
    slope = array(0, dim=sapply(dim.names, length),
                  dimnames = dim.names)
    
    # Plug in from MSM fit
    msm.subset = reshape2::melt(int[,,'msm',])
    msm.subset$year = 0
    int.msm = predict(fit.msm, msm.subset)
    msm.subset$year = 1
    slope.msm = predict(fit.msm, msm.subset) - int.msm
    
    int[,,'msm',] = int.msm
    slope[,,'msm',] = slope.msm
    
    # Plug in from non-MSM fit
    non.msm.sex = setdiff(dim.names$sex, 'msm')
    non.msm.subset = reshape2::melt(int[,,non.msm.sex,])
    non.msm.subset$year = 0
    int.non.msm = predict(fit.non.msm, non.msm.subset)
    non.msm.subset$year = 1
    slope.non.msm = predict(fit.non.msm, non.msm.subset) - int.non.msm
    
    int[,,non.msm.sex,] = int.non.msm
    slope[,,non.msm.sex,] = slope.non.msm
    
    # create.linear.functional.form(
    #   intercept = int,
    #   slope = slope,
    #   anchor.year = anchor.year,
    #   min = 0,
    #   max = max.prep.coverage,
    # )
    
    create.logistic.tail.functional.form(
        intercept = int,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.prep.coverage,
        intercept.link = 'logit',
        intercept.robust.to.negative = F,
        slope.link = 'logit',
        slope.robust.to.negative = T
    )
}

get.prep.indication.functional.form <- function(specification.metadata,
                                                anchor.year = 2010,
                                                max.p.indicated = 0.85)
{
    #-- MSM --#
    
    # Set up data for MSM
    p.msm.raw = rbind(
        
        # #https://www.cdc.gov/hiv/library/reports/hiv-surveillance-special-reports/no-31/index.html
        # # Numerator = condomless anal sex with casual partner (Table 5)
        # # Denominator = total (Table 5)
        # data.frame(
        #     year = 2021,
        #     p = c(1196/2214, 69/172, 206/405, 427/760, 225/387, 269/517, 343/850, 217/549, 390/597, (3+66+7+57)/(4+110+15+111)),
        #     race = c('all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
        #     age = c('all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
        #     weight = c(2214, 172, 405, 760, 387, 517, 850, 549, 597, (4+110+15+111)) / 2214,
        #     src = 'nhbs'
        # ),
        # 
        # # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf 
        # # Numerators and denominators from Table 6, HIV-negative
        # data.frame(
        #     year = 2017,
        #     p = c(3325/7125, 555/1273, 923/1792, 1052/2098, 436/983, 359/979, 660/1672, 907/2002, 1437/2774, (14+90+18+177)/(49+192+32+365)),
        #     race = c('all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
        #     age = c('all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
        #     weight = c(7125, 1273, 1792, 2098, 983, 979, 1672, 2002, 2774, (49+192+32+365)) / 7125,
        #     src = 'nhbs'
        # ),
        # 
        # # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-15.pdf
        # data.frame(
        #     year = 2014,
        #     p = c(2520/6847, 528/1576, 642/1598, 674/1733, 404/1054, 272/886, 509/1537, 718/1935, 1076/2796, (13+54+15+121)/(43+144+37+319)),
        #     race = c('all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
        #     age = c('all', '18-24 years', '25-29 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
        #     weight = c(6847, 1576, 1598, 1733, 1054, 886, 1537, 1935, 2796, (43+144+37+319)) / 6847,
        #     src = 'nhbs'
        # ),
        
        ## <!-- https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2017-United-States-tables-REV_20171204.pdf -->
        data.frame(
            year = 2017,
            p = c(19.0, 17.7, 20.1, 21.9, 18.5, 25.0, 20.6, 18.3, 19.1)/100,
            race = c('all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', '15-24 years', '25-29 years', '30-39 years', '40+ years', 'all', 'all', 'all', 'all'),
            weight = c(100, 27.1, 12.4, 15.8, 44.6, 6.5, 15.3, 69.2, 6.8) / 100,
            src = 'amis'
        ),
        
        ## 2018 : https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2018-United-States-Report.pdf
        data.frame(
            year = 2018,
            p = c(21.6, 19.1, 23.0, 22.0, 24.6, 28.5, 23.6, 20.5, 23.1)/100,
            race = c('all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', '15-24 years', '25-29 years', '30-39 years', '40+ years', 'all', 'all', 'all', 'all'),
            weight = c(100, 41.8, 12.9, 14.8, 30.5, 5.5, 16.1, 69.2, 7.4) / 100,
            src = 'amis'
        ),
        
        # 2019
        ## https://emoryamis.org/wp-content/uploads/2021/12/AMIS-2019-United-States-Report.pdf 
        data.frame(
            year = 2019,
            p = c(22.0, 19.7, 23.3, 25.7, 23.3, 30.6, 24.9, 19.4, 23.0)/100,
            race = c('all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', '15-24 years', '25-29 years', '30-39 years', '40+ years', 'all', 'all', 'all', 'all'),
            weight = c(100, 41.5, 17.9, 14.6, 25.9, 14.9, 15.4, 59.4, 8.3) / 100,
            src = 'amis'
        )
    )
    
    p.msm = restratify.data.to.specification(data = p.msm.raw,
                                             dim.names = specification.metadata$dim.names,
                                             covariate.names = c('age','race'))
    p.msm$year = p.msm$year - anchor.year
  #  p.msm$logit.p = log(p.msm$p / max.p.indicated) - log(1 - p.msm$p / max.p.indicated)
    
    # Fit data for MSM
#    fit.msm <- lm(logit.p ~ year + race + age, data = p.msm, weights = p.msm$weight)  
#    fit.msm <- suppressWarnings(glm(p/max.p.indicated ~ year + race + age, data = p.msm, weights = p.msm$weight, family = 'binomial')  )
 
    # A linear fit
    fit.msm <- glm(p/max.p.indicated ~ year + race + age, data = p.msm, weights = p.msm$weight, family = 'gaussian')
       
    #-- IDU --#
    
    # Set up data for IDU
    p.idu.raw = rbind(
      
        # https://www.cdc.gov/mmwr/preview/mmwrhtml/ss6306a1.htm 
        # (Table 4)
        data.frame(
            year = 2009,
            p = c((4249+1696)/(6992+2660), 4249/6992, 1696/2660, 745/995, 1174/1760, 1802/2961, 1916/3303, 308/633, 2544/4436, 1312/2095, 1816/2673, (56+25+182)/(88+39+306)),
            race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', 'all', 'all', '18-29 years', '30-39 years', '40-49 years', '50-59 years', '60+ years', 'all', 'all', 'all', 'all'),
            sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
            weight = c((6992+2660), 6992, 2660, 995, 1760, 2961, 3303, 633, 4436, 2095, 2673, (88+39+306)) / (6992+2660)
        ),
        
        # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-11.pdf 
        data.frame(
            year = 2012,
            p = c(5517/9099, 3909/6520, 1608/2579, 267/360, 475/639, 1194/1759, 1438/2431, 2143/3910, 2133/3897, 1349/2189, 1753/2550, (54+14+10+199)/(84+27+15+323)),
            race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', 'all', 'all', '18-24 years', '25-34 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
            sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
            weight = c(9099, 6520, 2579, 360, 639, 1759, 2431, 3910, 3897, 2189, 2550, (84+27+15+323)) / 9099
        ),
        
        # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-18.pdf
        data.frame(
            year = 2015,
            p = c(5867/9675, 4169/6954, 1677/2685, 440/578, 837/1148, 1588/2317, 1285/2159, 1717/3473, 1586/3137, 1320/2162, 2620/3837, (64+19+10+240)/(99+28+15+380)),
            race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
            age = c('all', 'all', 'all', '18-24 years', '25-34 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
            sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
            weight = c(9675, 6954, 2685, 578, 1148, 2317, 2159, 3473, 3137, 2162, 3837, (99+28+15+380)) / 9675
        ),
        
        # https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf 
        data.frame(
          year = 2018,
          p = c(6350/10617, 4287/7326, 2015/3221, 290/403, 812/1152, 1922/2861, 1448/2430, 1878/3771, 1698/3410, 1255/2170, 2927/4287, (77+20+5+364)/(12+37+14+566)),
          race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'hispanic', 'white', 'other'),
          age = c('all', 'all', 'all', '18-24 years', '25-34 years', '30-39 years', '40-49 years', '50+ years', 'all', 'all', 'all', 'all'),
          sex = c('all', 'male', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
          weight = c(10617, 7326, 3221, 403, 1152, 2861, 2430, 3771, 3410, 2170, 4287, (12+37+14+566)) / 10617
        )
    )
    
    # Fit data for IDU
    idu.dim.names = specification.metadata$dim.names
    idu.dim.names$sex = setdiff(idu.dim.names$sex, 'msm')
    idu.dim.names$sex[idu.dim.names$sex=='heterosexual_male'] = 'male'
    p.idu = restratify.data.to.specification(data = p.idu.raw,
                                             dim.names = idu.dim.names)
    p.idu$year = p.idu$year - anchor.year
    p.idu$sex[p.idu$sex=='male'] = 'heterosexual_male'
    
   # p.msm$logit.p = log(p.msm$p / max.p.indicated) - log(1 - p.msm$p / max.p.indicated)
    
    # Fit data for non-MSM
    
    #fit.idu <- suppressWarnings(glm(p/max.p.indicated ~ year + race + age + sex, data = p.idu, weights = p.idu$weight, family='binomial'))
    fit.idu <- glm(p ~ year + race + age + sex, data = p.idu, weights = p.idu$weight, family='gaussian')
    
    #-- Heterosexual --#
    fitted.heterosexual.female = get.cached.object.for.version('female.prep.indications.atlas', specification.metadata$version)
    fitted.or.heterosexual.male = get.cached.object.for.version('male.prep.indications.OR', specification.metadata$version)
    
    #-- Plug in to Unified Intercept and Slope Arrays --#
    # Set up our intercept/slope arrays
    
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    int = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
    slope = array(0, dim=sapply(dim.names, length),
                  dimnames = dim.names)
    
    # Plug in from MSM fit
    msm.subset = reshape2::melt(int[,,'msm',])
    msm.subset$year = 0
    int.msm = predict(fit.msm, msm.subset, type='link')
    msm.subset$year = 1
    slope.msm = predict(fit.msm, msm.subset, type='link') - int.msm
    
    int[,,'msm',] = int.msm
    slope[,,'msm',] = slope.msm
    
    # Plug in from IDU fit
    non.msm.sex = setdiff(dim.names$sex, 'msm')
    idu.states = 'active_IDU'
    
    idu.subset = reshape2::melt(int[,,non.msm.sex,idu.states])
    idu.subset$year = 0
    int.idu = predict(fit.idu, idu.subset, type='link')
    idu.subset$year = 1
    slope.idu = predict(fit.idu, idu.subset, type='link') - int.idu

    int[,,non.msm.sex,idu.states] = int.idu
    slope[,,non.msm.sex,idu.states] = slope.idu
    
    # Plug in from heterosexual fit
    non.idu.states = setdiff(dim.names$risk, idu.states)
    int[,,'female',non.idu.states] = fitted.heterosexual.female$intercepts
    int[,,'heterosexual_male',non.idu.states] = fitted.heterosexual.female$intercepts * fitted.or.heterosexual.male
    slope[,,'female',non.idu.states] = fitted.heterosexual.female$slopes
    slope[,,'heterosexual_male',non.idu.states] = fitted.heterosexual.female$slopes * fitted.or.heterosexual.male
    
    # create.logistic.linear.functional.form(
    #     intercept = int,
    #     slope = slope,
    #     anchor.year = anchor.year,
    #     min = 0,
    #     max = max.p.indicated,
    #     parameters.are.on.logit.scale = T
    # )
    
    
    # create.linear.functional.form(
    #   intercept = int,
    #   slope = slope,
    #   anchor.year = anchor.year,
    #   min = 0,
    #   max = max.p.indicated,
    #   link = 'identity'
    # )
    
    int[int<0] = 0
    create.logistic.tail.functional.form(
        intercept = int,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.p.indicated,
        intercept.link = 'logit',
        intercept.robust.to.negative = F,
        slope.link = 'logit',
        slope.robust.to.negative = T
    )
}


get.prep.persistence.functional.form <- function(specification.metadata,
                                                 anchor.year = 2020,
                                                 min.persistence = 0,
                                                 max.persistence = 1)
{
    p.persistence.raw = rbind(
      # [2012-2017 Persistence Data]; SF; 12 months of observation
      # https://academic.oup.com/ofid/article/6/4/ofz101/5365426
      data.frame(
          p = c(50/94, 23/52, 26/41, 22/39, 12/20, 12/22, 2/6, 2/7),
          race = c('all', 'all', 'all', 'hispanic', 'black', 'white', 'asian', 'other'),
          age = c('all', '18-29 years', '30+ years', 'all', 'all', 'all', 'all', 'all'),
          sex = 'msm',
          weight = c(94, 52, 41, 39, 20, 22, 6, 7) / 94
      ),
      
      # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9203912/
      data.frame(
          p = c(138/364, 13/43, 65/187, 57/129, 3/5, 23/56, 96/240, (115-96)/(308-240), 15/45, 12/29, 39/95, 56/136, 16/59), #NB - their Table 1 lists 113 for n persistent, but that does not add up with other numbers - 138 does
          race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'black', 'asian', 'hispanic', 'white', 'other'),
          age = c('all', '18-25 years', '25-39 years', '40-64 years', '65+ years', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'),
          sex = c('all', 'all', 'all', 'all', 'all', 'female', 'msm', 'heterosexual_male', 'all', 'all', 'all', 'all', 'all'),
          weight = c(364, 43, 187, 129, 5, 56, 240, (308-240), 45, 29, 95, 136, 59) / 364
      ),

      # https://academic.oup.com/cid/article/72/3/379/5703638?login=true
      # Commercially insured
      data.frame(
          p = c(54.4, 36.7, 35.0, 50.6, 57.9, 62.9, 64.3)/100,
          race = 'all',
          age = c('all', 'all', '18-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years'),
          sex = c('msm', 'female', 'msm', 'msm', 'msm', 'msm', 'msm'), #since the sample is overwhelmingly male, will presume it's MSM
          weight = c(11541, 266, 1463, 4145, 3037, 2366, 796) / 11807
      )#,

      # https://academic.oup.com/cid/article/72/3/379/5703638?login=true
      # Medicaid
      # data.frame(
      #     p = c(29.9, 32.4, 20.8, 16.8, 30.3, 31.8, 34.1, 45.7, 33.0, 21.6, 35.2)/100,
      #     race = c('all', 'all', 'all', 'all', 'all', 'all', 'all', 'all','white', 'black', 'other'),
      #     age = c('all', 'all', 'all', '18-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', 'all', 'all', 'all'),
      #     sex = c('all', 'msm', 'female', 'all', 'all', 'all', 'all', 'all', 'all', 'all', 'all'), #sinwill presume most males are MSM, will presume it's MSM
      #     weight = c(647, 502, 145, 117, 259, 150, 91, 30, 282, 167, 162) / 647
      # )
    )
    
    
    p.persistence = restratify.data.to.specification(data = p.persistence.raw,
                                             dim.names = specification.metadata$dim.names,
                                             covariate.names = c('age','race'))
    #p.persistence$year = p.persistence$year - anchor.year
    
    fit <- suppressWarnings(glm((p-min.persistence)/(max.persistence-min.persistence) ~ race + age + sex, data = p.persistence, weights = p.persistence$weight, family='binomial'))
    
    #-- Plug in to Unified Intercept and Slope Arrays --#
    # Set up our intercept/slope arrays
    
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    value = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
    
    # Plug in from MSM fit
    values.long = reshape2::melt(value)
    predicted.long = predict(fit, values.long, type='response')
    
    value[] = predicted.long
    
    create.static.functional.form(
        value = value,
        link = 'logit',
        min = min.persistence,
        max = max.persistence,
        value.is.on.transformed.scale = F
    )
}

restratify.data.to.specification <- function(data, dim.names,
                                             covariate.names = setdiff(names(data), c('year','p','value','weight')),
                                             max.age = 85)
{
    missing.covariate.names = setdiff(covariate.names, names(dim.names))
    if (length(missing.covariate.names)>0)
        stop(paste0(collapse.with.and("'", missing.covariate.names, "'"),
                    " ",
                    ifelse(length(missing.covariate.names)==1, 
                           "is listed as a covariate but is not a dimension",
                           "are listed as covariates but are not dimensions"),
                    " in the given dim.names"))
    
    if (all(names(data)!='weight'))
        data$weight = 1
    
    non.age.covariates = setdiff(covariate.names, 'age')
    for (covariate in non.age.covariates)
    {
        from.ontology = list(setdiff(unique(data[,covariate]), 'all'))
        names(from.ontology) = covariate
        
        mapping = get.ontology.mapping(from.ontology = from.ontology,
                                       to.ontology = dim.names[covariate])
        
        not.all.mask = data[,covariate] != 'all'
        
        if (is.null(mapping))
        {   
            mapping = get.ontology.mapping(from.ontology = dim.names[covariate],
                                           to.ontology = from.ontology)
            
            if (is.null(mapping))
              stop(paste0("Cannot find a mapping for the '", covariate, "' covariate to or from the given dim.names"))
            
            mapping.vector = mapping$get.mapping.vector(from.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
            tabled.mapping.vector = table(mapping.vector)
            
            reverse.mapping.vector = mapping$get.reverse.mapping.vector(to.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
            
            mapped.data = NULL
            unmapped.mask = !not.all.mask
            unmapped.mask[!unmapped.mask] = tabled.mapping.vector[ data[!unmapped.mask,covariate] ] == 1
            data.to.map = data[!unmapped.mask,]
            unmapped.data = data[unmapped.mask,]
            unmapped.data[unmapped.data[,covariate]!='all', covariate] = reverse.mapping.vector[ unmapped.data[,covariate] ]
            
            for (i in 1:nrow(data.to.map))
            {
                row = data.to.map[i,]
                val = data.to.map[i,covariate]
                row$weight = row$weight / tabled.mapping.vector[val]
                for (map.to.val in names(mapping.vector)[mapping.vector==val])
                {
                    row[,covariate] = map.to.val
                    mapped.data = rbind(mapped.data, row)
                }
            }
            
            data = rbind(unmapped.data,
                         mapped.data)
        }
        else
        {
            mapping.vector = mapping$get.mapping.vector(from.values = from.ontology[[1]], from.dimension = covariate, to.dimension = covariate)
            weight.mapping = rep(1, length(mapping.vector))
            names(weight.mapping) = names(mapping.vector)
            data[not.all.mask,covariate] = mapping.vector[ data[not.all.mask,covariate] ]
        }
        
    }
    
    
    if (any(covariate.names=='age'))
    {
        not.all.age.mask = data$age != 'all'
        mapped.data = NULL
        
        not.all.age.data = data[not.all.age.mask,]
        
        parsed.ages = parse.age.strata.names(not.all.age.data$age)
        if (is.null(parsed.ages))
            stop("Could not parse all age strata in the data")
        
        parsed.dim.names.age = parse.age.strata.names(dim.names$age)
        if (is.null(parsed.ages))
            stop("Could not parse all age strata in the given dim.names$age")
        spec.lower = parsed.dim.names.age$lower
        spec.upper = pmin(parsed.dim.names.age$upper, max.age)
        
        for (i in 1:nrow(not.all.age.data))
        {
            lower = parsed.ages$lower[i]
            upper = min(max.age, parsed.ages$upper[i])
            fraction.in.specification.age = pmax(0,
              (pmin(upper, spec.upper) - pmax(lower, spec.lower)) / (upper - lower)
            )
            
            non.zero.ages = (1:length(spec.upper))[fraction.in.specification.age>0]
            
            data.row = not.all.age.data[i,]
            weight = data.row$weight 
            
            for (age in non.zero.ages)
            {
                data.row$age = dim.names$age[age]
                data.row$weight = weight * fraction.in.specification.age[age]
                mapped.data = rbind(mapped.data, data.row)
            }
        }
        
        all.age.data = data[!not.all.age.mask,]
        data = rbind(all.age.data,
                     mapped.data)
    }
    
    data
}
