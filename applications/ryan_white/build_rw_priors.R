
RW.REGRESSION.FORMULA = p ~ age + race + sex + risk

get.p.non.adap.functional.form <- function(specification.metadata, verbose=F)
{
    numerator.data = read.non.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/non_adap_clients')
    denominator.data = SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk
    
    df = build.rw.proportion.outcome.data.frame(numerator.data = numerator.data,
                                                denominator.data = denominator.data,
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)
    
    anchor.year = 2020
    df$year = df$year - anchor.year
    
    fit = glm(RW.REGRESSION.FORMULA, data=df, weights = df$n); exp(fit$coefficients)
    
    # Transform the fit into slope and intercept
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    iterated.values = as.data.frame(get.every.combination(dim.names))
    
    year0.data = cbind(iterated.values, year=0)
    year1.data = cbind(iterated.values, year=1)
    
    intercept = predict(fit, year0.data, type = 'link')
    slope = predict(fit, year1.data, type='link') - intercept
    
    dim(intercept) = dim(slope) = sapply(dim.names, length)
    dimnames(intercept) = dimnames(slope) = dim.names
    
    # Make a functional form
    create.logistic.linear.functional.form(intercept = intercept,
                                           slope = slope,
                                           anchor.year = anchor.year,
                                           parameters.are.on.logit.scale = T)
}

get.p.oahs.functional.form <- function(specification.metadata, verbose=F)
{
    oahs.data = read.oahs.and.suppression.data('../jheem_analyses/applications/ryan_white/ryan_white_data/oahs_suppression/')
    non.adap.data = read.non.adap.data('../jheem_analyses/applications/ryan_white/ryan_white_data/non_adap_clients')
    
    denom.indices = get.rw.denominator.data.indices(numerator.data = oahs.data$oahs,
                                                    denominator.data = non.adap.data)
}

get.p.suppression.oahs.functional.form <- function(specification.metadata, verbose=F)
{
    oahs.data = read.oahs.and.suppression.data('../jheem_analyses/applications/ryan_white/ryan_white_data/oahs_suppression/')
    
    df = build.rw.proportion.outcome.data.frame(numerator.data = oahs.data$suppression,
                                                denominator.data = oahs.data$oahs,
                                                specification.metadata = specification.metadata,
                                                verbose = verbose)
    anchor.year = 2020
    df$year = df$year - anchor.year
    
    fit = glm(RW.REGRESSION.FORMULA, data=df, weights = df$n)
    
    # Transform the fit into slope and intercept
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    iterated.values = as.data.frame(get.every.combination(dim.names))
    
    year0.data = cbind(iterated.values, year=0)
    year1.data = cbind(iterated.values, year=1)
    
    intercept = predict(fit, year0.data, type = 'link')
    slope = predict(fit, year1.data, type='link') - intercept
    
    dim(intercept) = dim(slope) = sapply(dim.names, length)
    dimnames(intercept) = dimnames(slope) = dim.names
    
    # Make a functional form
    create.logistic.linear.functional.form(intercept = intercept,
                                           slope = slope,
                                           anchor.year = anchor.year,
                                           parameters.are.on.logit.scale = T)
}

get.p.adap.functional.form <- function(specification.metadata, verbose=F)
{
  
    adap.data = read.oahs.and.suppression.data('../jheem_analyses/applications/ryan_white/ryan_white_data/oahs_suppression/')
}

get.rw.denominator.data.indices <- function(numerator.data, denominator.data)
{
    sapply(1:length(numerator.data), function(i){
      
        num = numerator.data[[i]]
        for (j in 1:length(denominator.data))
        {
            denom = denominator.data[[j]]
            #if (dim.names.are.subset(sub.dim.names = dimnames(num),
            #                         super.dim.names = dimnames(denom)))
                if (!is.null(get.mappings.to.align.ontologies(dimnames(num), dimnames(denom))))
            
                return (j)
        }
        
        return (NA)
    })
}
