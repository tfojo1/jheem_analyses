
if (1==2)
{
    # POPULATION: 0.1561269 (ethnicity); 0.1939618 (age-ethnicity)
    calculate.error.terms.pop(stratification = "ethnicity",
                              output = 'cv')
    
    calculate.error.terms.pop(stratification = "age-ethnicity",
                              output = 'cv')
    # other estimates: 0.3023304 (all); 0.1031615 (race); 0.2771433 (age-race)
    
    
    # SUPPRESSION: 0.04560282 
    calculate.error.terms(data.type = "suppression",
                          data.source.1 = "cdc.aggregated.proportion",
                          data.source.2 = "lhd",
                          output = 'sd')
    
    # DIAGNOSES:  0.05368198 (should be 0.04514847)
    calculate.lhd.error.terms("diagnoses")
    calculate.lhd.error.terms("diagnoses", output='exponent.of.variance')
    calculate.lhd.error.terms("diagnoses", output='cv.and.exponent.of.variance')
    # OLD VALUE: 0.04621778
    calculate.error.terms(data.type = "diagnoses",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv')
    
    
    # PREVALENCE: 0.08384422
    calculate.lhd.error.terms("diagnosed.prevalence")
    calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.exponent.of.variance.eq.1')
    calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.exponent.of.variance')
    calculate.lhd.error.terms("diagnosed.prevalence", output='exponent.of.variance')
    calculate.lhd.error.terms("diagnosed.prevalence", output='cv.sqrt')
    calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.cv.sqrt')
    calculate.lhd.error.terms("diagnosed.prevalence", output='cv.and.fixed.exponent.of.variance',PREVALENCE.EXP.OF.VAR)
    # OLD VALUE: 0.04711922 --> NO LONGER GETTING THIS, NOW GETTING 0.03623443??
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv')
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv.sqrt')
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'c.of.v.and.sqrt')
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv.and.exponent.of.variance')
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv.and.exponent.of.variance.eq.1')
    calculate.error.terms(data.type = "diagnosed.prevalence",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aggregated.county",
                          output = 'cv.and.cv.sqrt')
    
    # AIDS DIAGNOSES: 0.2277531 - from 1993-1997 only; one source only has totals so no stratifications anyway 
    calculate.error.terms(data.type = "aids.diagnoses",
                          data.source.1 = "cdc.surveillance.reports",
                          data.source.2 = "cdc.aids",
                          years = c(1993:1997),
                          output = 'cv')
    # PREP UPTAKE: 0.01239159
    calculate.error.terms(data.type = "prep",
                          data.source.1 = "aidsvu",
                          data.source.2 = "cdc.prep",
                          output = 'cv')
}

calculate.lhd.error.terms = function(data.type, 
                                     output=c('cv',
                                              'exponent.of.variance',
                                              'cv.and.exponent.of.variance',
                                              'cv.and.exponent.of.variance.eq.1',
                                              'cv.sqrt')[1],
                                     fixed.exp.of.var=NA,
                                     verbose = T){
    lhd.data = read.csv("input_managers/LHD_Diagnoses_and_Diagnosed_Prevalence.csv")
    
    # iffy ones for prevalence
    lhd.data = lhd.data[lhd.data$MSA!="Riverside",]
    lhd.data = lhd.data[lhd.data$MSA!="New York",]
    lhd.data = lhd.data[lhd.data$MSA!="Tucson",]
    
    # iffy ones for diagnoses
    lhd.data = lhd.data[lhd.data$MSA!="Philadelphia",]
    lhd.data = lhd.data[lhd.data$MSA!="Seattle",]
    
    lhd.data = lhd.data[lhd.data$MSA!="Honolulu",]
    
    
    if(data.type=="diagnoses"){
        all.values1 = suppressWarnings(as.numeric(lhd.data$LHD.New.Diganoses))
        all.values2 = suppressWarnings(as.numeric(lhd.data$Atlas.Plus.Summed.New.Diagnoses..Data.Manager.))
        
    } else if(data.type=="diagnosed.prevalence"){
        all.values1 = suppressWarnings(as.numeric(lhd.data$LHD.Diagnosed.Prevalence))
        all.values2 = suppressWarnings(as.numeric(lhd.data$Atlas.Plus.Summed.Prevalence..Data.Manager.))
    } else 
        stop("only set up for diagnoses and diagnosed prevalence")
    
    x1 = all.values1
    e1 = all.values2 - all.values1
    cvs.1 = e1/all.values1 
    mask = !is.na(all.values1) & all.values1>100
    x1 = x1[mask]
    e1 = e1[mask]
    cvs.1 = cvs.1[mask]
    
    #sort(abs(cvs.1))
    
    x2 = all.values2
    e2 = all.values1 - all.values2
    cvs.2 = e2/all.values2
    mask = !is.na(all.values2) & all.values2>100
    x2 = x2[mask]
    e2 = e2[mask]
    cvs.2 = cvs.2[mask]
    
    x = c(x1, x2)
    e = c(e1, e2)
    cvs = c(cvs.1,cvs.2)
    
    nan.mask = is.nan(cvs)
    cvs[nan.mask] = 0
    e[nan.mask] = 0
    
    keep.mask = !is.na(cvs) & !is.infinite(cvs)
    cvs = cvs[keep.mask]
    x = x[keep.mask]
    e = e[keep.mask]
    
    # cv.sd = sd(cvs)
    # cv.mean = mean(cvs)
    # reject.gt.z = qnorm(1-1/length(cvs), 0, 1)
    # cvs.z = (cvs-cv.mean)/cv.sd
    # reject.mask = abs(cvs.z) > reject.gt.z
    # 
    # cvs = cvs[!reject.mask]
    # e = e[!reject.mask]
    # x = x[!reject.mask]
    
    if (verbose)
        print(paste0("N observations = ", length(cvs)/2))
    
    
    do.calculate.variance.parameters(e=e, x=x, output=output, fixed.exp.of.var=fixed.exp.of.var, verbose=verbose)
}

do.calculate.variance.parameters <- function(e, x, output, fixed.exp.of.var=NA, verbose=T)
{
    mask = !is.na(e) & !is.na(x)
    e = e[mask]
    x = x[mask]
    n = sum(mask)
    
    if (verbose)
    {
        print(paste0("Calculating '", output, "'; mean(x) = ", mean(x), 
                     ", range(x) = [",
                     min(x), " to ", max(x), "]"))
        
        #  print(ggplot2::qplot(x))
    }
    
    if (output=='sd')
    {
        # errors = all.values1 - all.values2
        rv = sqrt(sum(e^2)/n)
        
        rv
    }
    else if (output=='cv')
    {
        mask = x != 0
        e = e[mask]
        x = x[mask]
        
        cv = sqrt(sum(e^2/x^2)/n) 
        log.l = sum(dnorm(e, mean=0, sd=cv*x, log=T))
        
        print(paste0("With CV = ", cv, ", log L = ", log.l))
        
        cv
    }
    else if (output=='cv.sqrt')
    {
        cv.sqrt = sqrt(sum(e^2/x)/n)
        log.l = sum(dnorm(e, mean=0, sd=cv.sqrt*sqrt(x), log=T))
        
        print(paste0("With CV.sqrt = ", cv.sqrt, ", log L = ", log.l))
        
        cv.sqrt
    }
    else if (output=='c.of.v.and.sqrt')
    {
        c.of.v.and.sqrt = sqrt(sum(e^2/(x+x^2))/n)
        log.l = sum(dnorm(e, mean=0, sd=c.of.v.and.sqrt*sqrt(x+x^2), log=T))
        
        print(paste0("With c.of.v.and.sqrt = ", c.of.v.and.sqrt, ", log L = ", log.l))
        
        c.of.v.and.sqrt
    }
    else if (output=='exponent.of.variance')
    {
        #   return(sum(log(e^2)) / sum(log(x^2)))
        
        optimize.result = optimize(
            f = function(v){
                sum(dnorm(e, mean=0, sd=x^v, log=T))
            },
            interval = c(0,2),
            maximum = T)
        
        exponent.of.variance = as.numeric(optimize.result$maximum)
        log.l = sum(dnorm(e, mean=0, sd=x^exponent.of.variance, log=T))
        print(paste0("With exponent.of.variance = ", exponent.of.variance, ", log L = ", log.l))
        
        exponent.of.variance
    }
    else if (output=='cv.and.fixed.exponent.of.variance')
    {
        optimize.result = optimize(
            f = function(v){
                sum(dnorm(e, mean=0, sd=sqrt(x^(2*fixed.exp.of.var) + (v*x)^2), log=T))
            },
            interval = c(0,1),
            maximum = T)
        
        cv = as.numeric(optimize.result$maximum)
        log.l = sum(dnorm(e, mean=0, sd=sqrt(x^(2*fixed.exp.of.var) + (cv*x)^2), log=T))
        print(paste0("With exponent.of.variance fixed to = ", fixed.exp.of.var, "and cv = ", cv, ", log L = ", log.l))
        
        cv
    }
    else if (output=='cv.and.exponent.of.variance.eq.1')
    {
        optimize.result = optimize(
            f = function(v){
                sum(dnorm(e, mean=0, sd=sqrt(x + (v*x)^2), log=T))
            },
            interval = c(0,1),
            maximum = T)
        
        cv = as.numeric(optimize.result$maximum)
        log.l = sum(dnorm(e, mean=0, sd=sqrt(x + (cv*x)^2), log=T))
        print(paste0("With exponent.of.variance fixed to = 0.5 and cv = ", cv, ", log L = ", log.l))
        
        cv
    }
    else if (output=='cv.and.cv.sqrt')
    {
        optim.result = optim(
            par = c(.05, .05),
            fn = function(par){
                
                - sum(dnorm(e, mean=0, sd=sqrt(x*par[1] + (x*par[2])^2), log=T))
                
            },
            lower = c(0,0),
            upper = c(1,5),
            method = 'L-BFGS-B'
        )
        
        rv = list(cv=as.numeric(optim.result$par[2]),
                  cv.sqrt=as.numeric(optim.result$par[1]))
        
        log.l = sum(dnorm(e, mean=0, sd=sqrt(x*rv$cv.sqrt + (x*rv$cv)^2), log=T))
        
        print(paste0("With CV = ", rv$cv, " and CV.sqrt = ", rv$cv.sqrt, ", log L = ", log.l))
        
        rv
    }
    else if (output=='cv.and.exponent.of.variance')
    {
        optim.result = optim(
            par = c(1, .05),
            fn = function(par){
                
                - sum(dnorm(e, mean=0, sd=sqrt(x^(2*par[1]) + (x*par[2])^2), log=T))
                
            },
            lower = c(0,0),
            upper = c(1,2),
            method = 'L-BFGS-B'
        )
        
        rv = list(cv=as.numeric(optim.result$par[2]),
                  exponent.of.variance=as.numeric(optim.result$par[1]))
        
        log.l = sum(dnorm(e, mean=0, sd=sqrt(x^(2*rv$exponent.of.variance) + (x*rv$cv)^2), log=T))
        
        print(paste0("With CV = ", rv$cv, " and exponent.of.variance = ", rv$exponent.of.variance, ", log L = ", log.l))
        
        rv
    }
    else
        stop(paste0("Invalid 'output' for variance parameters: '", output, "'"))
}


calculate.error.terms = function(data.type,
                                 data.source.1,
                                 data.source.2,
                                 years=NULL,
                                 use.totals = T,
                                 use.age = T,
                                 use.risk = T,
                                 use.sex = T,
                                 use.race = F,
                                 output='cv',
                                 fixed.exp.of.var=NA){
    
    all.values1 = numeric()
    all.values2 = numeric()
    
    #data1 = SURVEILLANCE.MANAGER$data$suppression$estimate$cdc.aggregated.proportion$cdc
    #data2 = SURVEILLANCE.MANAGER$data$suppression$estimate$lhd$lhd # local health departments 
    data1 = SURVEILLANCE.MANAGER$data[[data.type]]$estimate[[data.source.1]][[1]]
    data2 = SURVEILLANCE.MANAGER$data[[data.type]]$estimate[[data.source.2]][[1]]
    
    
    # TOTAL
    if(use.totals){
        years.in.both.total = intersect(dimnames(data1[["year__location"]])$year,
                                        dimnames(data2[["year__location"]])$year)
        if(!is.null(years)){
            years.in.both.total = intersect(years.in.both.total,years)
        }
        locations.in.both.total = intersect(dimnames(data1[["year__location"]])$location,
                                            dimnames(data2[["year__location"]])$location)
        
        values1 = data1[["year__location"]][years.in.both.total, locations.in.both.total]
        values2 = data2[["year__location"]][years.in.both.total, locations.in.both.total]
        
        all.values1 = c(all.values1, values1)
        all.values2 = c(all.values2, values2)
    }
    
    if(use.age){
        # AGE
        years.in.both.age = intersect(dimnames(data1$year__location__age)$year,
                                      dimnames(data2$year__location__age)$year)
        locations.in.both.age = intersect(dimnames(data1$year__location__age)$location,
                                          dimnames(data2$year__location__age)$location)
        ages.in.both = intersect(dimnames(data1$year__location__age)$age,
                                 dimnames(data2$year__location__age)$age)
        
        values1 = data1$year__location__age[years.in.both.age, locations.in.both.age,ages.in.both]
        values2 = data2$year__location__age[years.in.both.age, locations.in.both.age,ages.in.both]
        
        all.values1 = c(all.values1, values1)
        all.values2 = c(all.values2, values2) 
    }
    
    
    if(use.risk){
        # RISK
        years.in.both.risk = intersect(dimnames(data1$year__location__risk)$year,
                                       dimnames(data2$year__location__risk)$year)
        locations.in.both.risk = intersect(dimnames(data1$year__location__risk)$location,
                                           dimnames(data2$year__location__risk)$location)
        risks.in.both = intersect(dimnames(data1$year__location__risk)$risk,
                                  dimnames(data2$year__location__risk)$risk)
        
        values1 = data1$year__location__risk[years.in.both.risk, locations.in.both.risk,risks.in.both]
        values2 = data2$year__location__risk[years.in.both.risk, locations.in.both.risk,risks.in.both]
        
        all.values1 = c(all.values1, values1)
        all.values2 = c(all.values2, values2)
    }
    if(use.sex){
        # SEX
        years.in.both.sex = intersect(dimnames(data1$year__location__sex)$year,
                                      dimnames(data2$year__location__sex)$year)
        locations.in.both.sex = intersect(dimnames(data1$year__location__sex)$location,
                                          dimnames(data2$year__location__sex)$location)
        sexes.in.both = intersect(dimnames(data1$year__location__sex)$sex,
                                  dimnames(data2$year__location__sex)$sex)
        
        values1 = data1$year__location__sex[years.in.both.sex, locations.in.both.sex,sexes.in.both]
        values2 = data2$year__location__sex[years.in.both.sex, locations.in.both.sex,sexes.in.both]
        
        all.values1 = c(all.values1, values1)
        all.values2 = c(all.values2, values2)
        
    }
    
    if(use.race){
        # RACE
        years.in.both.race = intersect(dimnames(data1$year__location__race)$year,
                                       dimnames(data2$year__location__race)$year)
        locations.in.both.race = intersect(dimnames(data1$year__location__race)$location,
                                           dimnames(data2$year__location__race)$location)
        races.in.both = intersect(dimnames(data1$year__location__race)$race,
                                  dimnames(data2$year__location__race)$race) # HAVE TO DO MAPPINGS HERE FOR RACE TO WORK
        
        values1 = data1$year__location__race[years.in.both.race, locations.in.both.race,]
        values2 = data2$year__location__race[years.in.both.race, locations.in.both.race,]
        
        all.values1 = c(all.values1, values1)
        all.values2 = c(all.values2, values2)
    }
    
    # Calculate it
    cvs.1 = (all.values1 - all.values2)/all.values1 
    cvs.2 = (all.values1 - all.values2)/all.values2
    
    cvs = c(cvs.1,cvs.2)
    x = c(all.values1, all.values2)
    errors = c(all.values1 - all.values2, all.values1 - all.values2)
    
    errors[is.nan(cvs)] = 0
    cvs[is.nan(cvs)] = 0
    
    keep.mask = !is.na(cvs) & !is.infinite(cvs)
    cvs = cvs[keep.mask]
    x = x[keep.mask]
    errors = errors[keep.mask]
    
    # check for outliers
    mean.cv = mean(cvs)
    sd.cv = sd(cvs)
    outliers.mask = cvs<(mean.cv - (3*sd.cv)) | cvs>(mean.cv + (3*sd.cv))
    cvs = cvs[!outliers.mask]
    x = x[!outliers.mask]
    errors = errors[!outliers.mask]
    print(qplot(all.values1,all.values2)+geom_abline(intercept = 0,slope = 1))
    do.calculate.variance.parameters(e = errors, x = x, fixed.exp.of.var=fixed.exp.of.var, output=output)
}


calculate.error.terms.pop = function(stratification,
                                     is.cv){
    
    all.values1 = numeric()
    all.values2 = numeric()
    
    # anything other than age alone, sex alone, and age-sex
    
    if(stratification=="age-ethnicity"){
        data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity["2019",,,]
        data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity["2020",,,]  
    } else if(stratification=="ethnicity"){
        data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__ethnicity["2019",,]
        data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__ethnicity["2020",,]
    } else if(stratification=="age-race"){
        data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race["2019",,,]
        data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race["2020",,,]
    } else if(stratification=="race"){
        data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race["2019",,]
        data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race["2020",,]
    } else if(stratification=="all"){
        data1 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex["2019",,,,,]
        data2 = CENSUS.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex["2020",,,,,]
    } else
        stop("right now only set up for age-ethnicity, ethnicity, age-race, race, or all")
    
    all.values1 = data1
    all.values2 = data2
    
    if(is.cv){
        # Calculate it
        #cvs.1 = (all.values1 - all.values2)/all.values1 
        cvs.2 = (all.values1 - all.values2)/all.values2
        
        cvs = c(cvs.2) #,cvs.2)
        cvs[is.nan(cvs)] = 0
        cvs = cvs[!is.na(cvs) & !is.infinite(cvs)]
        
        # check for outliers
        mean.cv = mean(cvs)
        sd.cv = sd(cvs)
        outliers.mask = cvs<(mean.cv - (3*sd.cv)) | cvs>(mean.cv + (3*sd.cv))
        cvs = cvs[!outliers.mask]
        rv = sqrt(sum(cvs^2, na.rm=T)/sum(!is.na(cvs))) 
        
    } else {
        
        # Calculate it
        errors = all.values1 - all.values2
        rv = sqrt(sum(errors^2, na.rm=T)/sum(!is.na(errors))) 
    }
    
    rv
    
}
