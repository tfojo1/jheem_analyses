
# RUN THE CODE INSIDE THIS 'IF' STATEMENT TO GENERATE AND SAVE FUNCTIONAL FORMS
if (1==2)
{
    source('applications/ryan_white/ryan_white_specification.R')
    spec.meta = get.specification.metadata('rw','C.12580') # the location doesn't matter here. Could even use EHE version
  
    p.non.adap.functional.form = get.p.non.adap.functional.form(spec.meta)
    cache.object.for.version(object = p.non.adap.functional.form, 
                             name = "p.non.adap.functional.form", 
                             version = 'rw', overwrite=T)  
    
    p.oahs.functional.form = get.p.oahs.functional.form(spec.meta)
    cache.object.for.version(object = p.oahs.functional.form, 
                             name = "p.oahs.functional.form", 
                             version = 'rw', overwrite=T)  
    
    p.suppression.oahs.functional.form = get.p.suppression.oahs.functional.form(spec.meta)
    cache.object.for.version(object = p.suppression.oahs.functional.form, 
                             name = "p.suppression.oahs.functional.form", 
                             version = 'rw', overwrite=T) 
    
    p.adap.functional.form = get.p.adap.functional.form(spec.meta)
    cache.object.for.version(object = p.adap.functional.form, 
                             name = "p.adap.functional.form", 
                             version = 'rw', overwrite=T) 
}

RW.REGRESSION.FORMULA = p ~ age + race + sex + risk
RW.REGRESSION.FORMULA.SANS.RISK = p ~ age + race + sex

get.p.non.adap.functional.form <- function(specification.metadata, verbose=F)
{
    numerator.data = read.non.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/non_adap_clients')
    denominator.data = SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk
    
    df = build.rw.proportion.outcome.data.frame(numerator.data = numerator.data,
                                                denominator.data = denominator.data,
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)
    
    do.create.rw.functional.form(df)
}

get.p.oahs.functional.form <- function(specification.metadata, verbose=F)
{
    oahs.data = read.oahs.and.suppression.data('../jheem_analyses/applications/ryan_white/ryan_white_data/oahs_suppression/')
    non.adap.data = read.non.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/non_adap_clients')
    
    oahs.years = sapply(oahs.data$oahs, function(x){dimnames(x)$year})
    non.adap.years = sapply(non.adap.data, function(x){dimnames(x)$year})
    
    denom.indices = get.rw.denominator.data.indices(oahs.data$oahs, non.adap.data)
    
    numerators = oahs.data$oahs[!is.na(denom.indices)]
    denominators = non.adap.data[denom.indices[!is.na(denom.indices)]]
    
    # Pull numerators and denominators
    for (year in oahs.years)
    {
        oahs.for.year = oahs.data$oahs[oahs.years==year]
        
        oahs.age.race.sex.for.year = oahs.for.year[sapply(oahs.for.year, function(x){
            length(setdiff(c('age','race','sex'), names(dim(x))))==0
        })]
        
        if (length(oahs.age.race.sex.for.year)==3)
        {
            dim.names = union.shared.dim.names(
              dimnames(oahs.age.race.sex.for.year[[1]]),
              union.shared.dim.names(dimnames(oahs.age.race.sex.for.year[[2]]),
                                              dimnames(oahs.age.race.sex.for.year[[3]]))
            )
            
            arr = array(NA, dim=sapply(dim.names, length), dimnames=dim.names)
            for (x in oahs.age.race.sex.for.year)
            {
                array.access(arr, dimnames(x)) = x
            }
            
            non.adap.age.race.mask = sapply(non.adap.data, function(x){dimnames(x)$year==year && any(names(dim(x))=='age') && any(names(dim(x))=='race')})
            if (any(non.adap.age.race.mask))
            {
                denominators = c(denominators, non.adap.data[non.adap.age.race.mask][1])
                numerators = c(numerators, list(apply(arr, setdiff(names(dim(arr)), 'sex'), sum)))
            }
        }
        else if (verbose)
            print(paste0("Skipping age/race/sex for year ", year))
        
       
    }
    
    # Make it into a data frame and fit a regression model
    df = build.rw.proportion.outcome.data.frame(numerator.data = numerators,
                                                denominator.data = denominators,
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)
    
    do.create.rw.functional.form(df)
    
}

get.p.suppression.oahs.functional.form <- function(specification.metadata, verbose=F)
{
    oahs.data = read.oahs.and.suppression.data('../jheem_analyses/applications/ryan_white/ryan_white_data/oahs_suppression/')
    
    df = build.rw.proportion.outcome.data.frame(numerator.data = oahs.data$suppression,
                                                denominator.data = oahs.data$oahs,
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)

    
    do.create.rw.functional.form(df)
}

get.p.adap.functional.form <- function(specification.metadata, verbose=F)
{
    adap.data = read.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/adap_clients/')
    non.adap.data = read.non.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/non_adap_clients')
    
    denom.indices = get.rw.denominator.data.indices(adap.data, non.adap.data)
    
    df = build.rw.proportion.outcome.data.frame(numerator.data = adap.data,
                                                denominator.data = non.adap.data[denom.indices],
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)
    
    
    do.create.rw.functional.form(df, ff=RW.REGRESSION.FORMULA.SANS.RISK)
}

##-- HELPERS --##

do.create.rw.functional.form <- function(df, ff=RW.REGRESSION.FORMULA, check.in=F)
{    
    anchor.year = 2020
    df$year = df$year - anchor.year

    fit = glm(ff, data=df, weights = n)

    if (check.in)
    {
        print(exp(fit$coefficients))
        browser()
    }
    
    # Transform the fit into slope and intercept
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    iterated.values = as.data.frame(get.every.combination(dim.names))
    
    year0.data = cbind(iterated.values, year=0)
    year1.data = cbind(iterated.values, year=1)
    
    intercept = suppressWarnings(predict(fit, year0.data, type = 'link'))
    slope = suppressWarnings(predict(fit, year1.data, type='link')) - intercept
    
    dim(intercept) = dim(slope) = sapply(dim.names, length)
    dimnames(intercept) = dimnames(slope) = dim.names
    
    # Make a functional form
    create.logistic.linear.functional.form(intercept = intercept,
                                           slope = slope,
                                           anchor.year = anchor.year,
                                           parameters.are.on.logit.scale = T)
}

get.rw.denominator.data.indices <- function(numerator.data, denominator.data)
{
    sapply(1:length(numerator.data), function(i){
      
        num = numerator.data[[i]]
        for (j in 1:length(denominator.data))
        {
            denom = denominator.data[[j]]
            if (dim.names.are.subset(sub.dim.names = dimnames(num),
                                     super.dim.names = dimnames(denom)))
            #    if (!is.null(get.mappings.to.align.ontologies(dimnames(num), dimnames(denom))))
            
                return (j)
        }
        
        return (NA)
    })
}
