    library(ggplot2)
    
    # From
    # https://www.sciencedirect.com/science/article/pii/0021968155901399
    # the pdf:
    #   https://pdf.sciencedirectassets.com/273120/1-s2.0-S0021968100X02998/1-s2.0-0021968155901399/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjECsaCXVzLWVhc3QtMSJIMEYCIQCZSIYMMDcHniY%2FrlQqAfK23W48cskk1wFpgRQtgmN%2B9gIhAO8PIcZIpwkrzt0EtMkXJ4W%2BBNLoUnn1fpf1k4vLcFm%2FKrIFCCQQBRoMMDU5MDAzNTQ2ODY1Igz2zxvr89kBGsKGzssqjwXP2PaWrVU7faBKwy6GVqdMAJeRp1c97ShtH3xp9ovb49paE4sbdeQCNn%2BUb0Ly%2FrOrcRwBn%2BePrqQE75lX9Na262tY2lzGlyGPGEsz5UzT28QQHNUVY%2BrZa9hxA1AMwubriqKgoH7TemQjdiHIHf%2F0DTo5o9M2mT4cRbm8lURBMQ%2F5ONd%2Bl%2F13vjBL%2FnrdD5JU5DfFtjBai7M2HA0uuQ3JtpGtWWLGV41xUuD8PFvWmamRy7K3W6wZE7RKQj1FYGX7z5RyduSLHNz3avkkoTWn3O2XfLffrblszPgPZbLLlSh959bmi3FmOyKjZyYLoKcnHrv3jOfIgMf%2F%2BC0EiAiudDRSx8yBomJwyevpnUM%2BO4OPfHI%2B8B1w7PyYpfXWQ8mr1z%2FTvEpIJ%2Bupo%2F3g48MNSqpAizqg4PRrQHd6q2sSM0c%2BBFe5Ns4s4cOufj0eOBmQscpcHMaeV1yzJa5BqtBt%2BUNZffqMY0IKd7PM9wK1f7n%2FAmdT0lRWjvjTEoQCHqTIHcXD04v4dmgKwzvIQ54S8Jw3pwSC00dF4xoTw0ISKCj8mIqsZh2ThIaupIXCTAPdkxKymhfGdvDWh4AaCZSiyzkoCQXWi3irED7ghaTgPk8HfKzNXIGr6H4HdaakU2%2BwyB5LWgU%2BiIEL8ZqDYcOXCadp4RPbD4p2USmlpEBcjcIZh2BTqMaA67LVLm3zSw3f%2FGTv2unzl0BigY1%2BVDg%2Bb3RZZua170xNhi9NwYiTwuXniQWhZONU6Jy9Eo7sso53mCY1eoWuib%2BXIa95CrQpH%2FE4HO4PA2F5lzuYK3Jrxwcu5fMq7%2BOys%2Fckbg1wi%2FFKu1yFNOcnEVbR%2B0UtGEQbLltkMijCqfDEZhv9Tu8AMKDHnLwGOrABFryDi%2B66%2FM9EWDa6ajcsBiV5qpBsN1Ao6jYFkCD1e4KwYMz7pAxqLjcknovpUR8oYTz358GyJp%2B8aRqlJJF%2FemwKCLufxOzybsjrCFWSYYkAw0lVXGoCoTuSU9oUkhie2aLt28o%2FitqKaKB4LHuKtgu%2FeMIG4RdUp3mdbPJJzu0aHcFS1mLVjiwRe%2Bd9eksAfzAoshU3lWOQkbvRRd1RrkrV4JCtrZk9C%2BKROflL85w%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20250115T040536Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYW4SYPWKZ%2F20250115%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=bf5c7c4735f4c6372322836510507ab3dd4825d5f5fc488ab01bcd6753cc289b&hash=72a6c4f4061ca864f68f8ba09a69c8bc31bee37328e9d4b716a826ea92231288&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=0021968155901399&tid=spdf-dec051e8-4525-49fa-82fa-71863c684ec8&sid=5f3ae68722c55640f968fcc22ce757619d47gxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=161c5856575304075e&rr=9022fd060fcaf283&cc=us
    
    ESTIMATES = list()
    
    #-- BENIGN LATE SYPHILIS --#
    
    # From Table 7: cumulative percent developing benign late syphilis by the end of the 15th, 30th, and 35th years
    male.p.benign.late = c('15'=10.9, '30'=15.7, '35'=16.4)/100
    female.p.benign.late = c('15'=15.9, '30'=19.8, '35'=21.4)/100
    times.p.benign.late = as.numeric(names(male.p.benign.late))
    
    # rates for men:
    -log(1-male.p.benign.late)/times.p.benign.late
    fit.male.p.benign.late = lm(-log(1-male.p.benign.late)~times.p.benign.late + 0)
    ESTIMATES$rate.male.benign.late.est = as.numeric(fit.male.p.benign.late$coefficients[1])
    ESTIMATES$rate.male.benign.late.sd = sqrt(as.numeric(vcov(fit.male.p.benign.late)))
    # women for women:
    -log(1-female.p.benign.late)/times.p.benign.late
    fit.female.p.benign.late = lm(-log(1-female.p.benign.late)~times.p.benign.late + 0)
    ESTIMATES$rate.female.benign.late.est = as.numeric(fit.female.p.benign.late$coefficients[1])
    ESTIMATES$rate.female.benign.late.sd = sqrt(as.numeric(vcov(fit.female.p.benign.late)))
    
    print(ESTIMATES)
    
    #-- CARDIOVASCULAR SYPHILIS --#
    
    # from Fig9: proportion of each agegroup developled some type of cardiovascular involvement
    # cardiovascular.male.15older=0.15
    # cardiovascular.female.15older=0.08
    # average.time=35 #years Fig10
    # ESTIMATES$rate.male.cardiovascular.15older.est.PK = -log(1-cardiovascular.male.15older)/average.time #MALE
    # ESTIMATES$rate.female.cardiovascular.15older.est.PK = -log(1-cardiovascular.female.15older)/average.time #FEMALE
    
    # MALE
    cardiovascular.male.outcomes = data.frame(
        # from table 7
        p = c(uncomplicated.aortitis=2.6/100,
              aortic.insufficiency=7.3/100,
              saccular.aneurym=3.6/100,
              ostial.stenosis=0.7/100,
              aortitis.at.death=0.7/100,
              total.complicated=12.3/100),
        
        # from figure 10 (digitized)
        mean.time = c(uncomplicated.aortitis=28.9,
                      aortic.insufficiency=31.5,
                      saccular.aneurym=31.4,
                      ostial.stenosis=NA,
                      aortitis.at.death=NA,
                      total.complicated=30.7)
    )
    
    cardiovascular.male.rates.among.those.with.outcome = 1/cardiovascular.male.outcomes$mean.time
    cardiovascular.male.rates = cardiovascular.male.rates.among.those.with.outcome * cardiovascular.male.outcomes$p
    names(cardiovascular.male.rates) = dimnames(cardiovascular.male.outcomes)[[1]]
    ESTIMATES$rate.male.cardiovascular.est = as.numeric(cardiovascular.male.rates['uncomplicated.aortitis'] + cardiovascular.male.rates['total.complicated'])
    
    # FEMALE
    cardiovascular.female.outcomes = data.frame(
        # from table 7
        p = c(uncomplicated.aortitis=2.9/100,
              aortic.insufficiency=3.3/100,
              saccular.aneurym=1.5/100,
              ostial.stenosis=0.3/100,
              aortitis.at.death=0.0/100,
              total.complicated=5.1/100),
        
        # from figure 10 (digitized)
        mean.time = c(uncomplicated.aortitis=23.7,
                      aortic.insufficiency=32.8,
                      saccular.aneurym=36.8,
                      ostial.stenosis=NA,
                      aortitis.at.death=NA,
                      total.complicated=32.8)
    )
    
    cardiovascular.female.rates.among.those.with.outcome = 1/cardiovascular.female.outcomes$mean.time
    cardiovascular.female.rates = cardiovascular.female.rates.among.those.with.outcome * cardiovascular.female.outcomes$p
    names(cardiovascular.female.rates) = dimnames(cardiovascular.female.outcomes)[[1]]
    ESTIMATES$rate.female.cardiovascular.est = as.numeric(cardiovascular.female.rates['uncomplicated.aortitis'] + 
                                                              cardiovascular.female.rates['total.complicated'])
    
    
    
    #-- NEUROSYPHILIS ----
    
    # Fig11. proportion of each age and sex group developed neurosyphilis 
    # neurosyphilis.male.by.age=c("0-14"=0.12, "15-39"=0.1, "40+"=0)
    # neurosyphilis.female.by.age=c("0-14"=0.03, "15-39"=0.05, "40+"=0.04)
    # average.time=c(30,15) # fig12
    # 
    # ESTIMATES$neurosyphilis.male.by.age = 
    #     c("0-14"=list(-log(1-neurosyphilis.male.by.age[1])/average.time),
    #        "15-39"=list(-log(1-neurosyphilis.male.by.age[2])/average.time),
    #        "40+"=list(-log(1-neurosyphilis.male.by.age[3])/average.time))
    # ESTIMATES$neurosyphilis.female.by.age = 
    #     c("0-14"=list(-log(1-neurosyphilis.female.by.age[1])/average.time),
    #       "15-39"=list(-log(1-neurosyphilis.female.by.age[2])/average.time),
    #       "40+"=list(-log(1-neurosyphilis.female.by.age[3])/average.time))
    
    # MALE
    neurosyphilis.male.outcomes = data.frame(
        # from table 9
        p = c(diffuse.meningovascular = 3.6/100,
              general.paresis = 3.0/100,
              tabes.dorsalis = 2.5/100,
              gumma.of.brain = 0.3/100),
        
        
        # from figure 12
        mean.time = c(diffuse.meningovascular = 14.9,
                      general.paresis = 25.4,
                      tabes.dorsalis = 28.8,
                      gumma.of.brain = NA)
    )
    
    neurosyphilis.male.rates.among.those.with.outcome = 1/neurosyphilis.male.outcomes$mean.time
    neurosyphilis.male.rates = neurosyphilis.male.rates.among.those.with.outcome * neurosyphilis.male.outcomes$p
    names(neurosyphilis.male.rates) = dimnames(neurosyphilis.male.outcomes)[[1]]
    ESTIMATES$rate.male.neurosyphilis.est = as.numeric(neurosyphilis.male.rates['diffuse.meningovascular'] + neurosyphilis.male.rates['general.paresis'] + neurosyphilis.male.rates['tabes.dorsalis'])
    # a check: over 40 years, how many would have the outcome
    1 - exp(-ESTIMATES$rate.male.neurosyphilis.est*40) #'@Todd: This should be 0 according to Fig11
    
    
    # FEMALE
    neurosyphilis.female.outcomes = data.frame(
        # from table 9
        p = c(diffuse.meningovascular = 1.7/100,
              general.paresis = 1.7/100,
              tabes.dorsalis = 1.4/100,
              gumma.of.brain = 0.2/100),
        
        
        # from figure 12
        mean.time = c(diffuse.meningovascular = 18.8,
                      general.paresis = 19.6,
                      tabes.dorsalis = 30.7,
                      gumma.of.brain = NA)
    )
    
    neurosyphilis.female.rates.among.those.with.outcome = 1/neurosyphilis.female.outcomes$mean.time
    neurosyphilis.female.rates = neurosyphilis.female.rates.among.those.with.outcome * neurosyphilis.female.outcomes$p
    names(neurosyphilis.female.rates) = dimnames(neurosyphilis.female.outcomes)[[1]]
    ESTIMATES$rate.female.neurosyphilis.est = as.numeric(neurosyphilis.female.rates['diffuse.meningovascular'] + neurosyphilis.female.rates['general.paresis'] + neurosyphilis.female.rates['tabes.dorsalis'])
    # a check: over 40 years, how many would have the outcome
    1 - exp(-ESTIMATES$rate.female.neurosyphilis.est*40)
    
    
    #-- CHECK --#
    
    # MALE
    # from page 339
    # male.with.outcome = c(
    #     infection.relapse = 22.7/100,
    #     benign.late = 14.4/100,
    #     cardiovascular = 13.6/100, 
    #     neurosyphilis = 9.4/100,
    #     death = 15/100
    # )
    # 
    # 
    # # FEMALE
    # # from page 32
    # female.with.outcome = c(
    #     infection.relapse = 24/100,
    #     benign.late = 16.7/100,
    #     cardiovascular = 7.6/100,
    #     neurosyphilis = 5.0/100,
    #     death = 8/100
    # )
    # 
    # years.for.check.male = 29
    # years.for.check.female = 26
    # cbind(
    #     observed.male = male.with.outcome[c('benign.late','cardiovascular','neurosyphilis')],
    #     projected.male = round(1-exp(-as.numeric(ESTIMATES[c('rate.male.benign.late.est','rate.male.cardiovascular.est','rate.male.neurosyphilis.est')]) * years.for.check.male),3),
    #     observed.female = female.with.outcome[c('benign.late','cardiovascular','neurosyphilis')],
    #     projected.female = round(1-exp(-as.numeric(ESTIMATES[c('rate.female.benign.late.est','rate.female.cardiovascular.est','rate.female.neurosyphilis.est')]) * years.for.check.female),3)
    #     
    # )
    
    
    # Golden (2003) & Kent (2008) estimate that 25–60% of patients experience CNS invasion during the primary and secondary stages, with 5% of these cases being symptomatic. 
    # 	This results in an estimated [1.25–3%] of patients experience symptomatic CNS disease during the primary and secondary stages (2, 7)
    times.cns.primary.secondary = 3/12 #3 months
    p.cns.primary.secondary = c(0.0125, .03)
    ESTIMATES$rate.cns.primary.secondary =  -log(1-p.cns.primary.secondary)/times.cns.primary.secondary
    