
if (1==2)
{
    test.n = c(2816,513,4951,56552,151476,89716,23030,6263)
    test.endpoints = c(0,13,15,25,35,45,55,65,100)
    
    smoothed.endpoints = 0:100
    smoothed.n = get.smoothed.age.bracket.counts(observed.n.per.bracket = test.n,
                                                 observed.age.endpoints = test.endpoints,
                                                 desired.age.endpoints = smoothed.endpoints)
    
    library(ggplot2)
    
    qplot(x=c((test.endpoints[-1] + test.endpoints[-length(test.endpoints)])/2,
              (smoothed.endpoints[-1] + smoothed.endpoints[-length(smoothed.endpoints)])/2),
          y=c(test.n/max(test.n), smoothed.n/max(smoothed.n)),
          geom = 'line',
          color = c(rep("observed", length(test.n)), rep("smoothed", length(smoothed.n))))
    
    load('../jheem_analyses/cached/ALL.DATA.MANAGERS.Rdata')
    source('../jheem2/R/ONTOLOGY_ontology_mappings.R')
    source('../jheem_analyses/EHE/ehe_specification_helpers.R')
    round(cbind(
        get.aging.rate.mid.of.20(test.n[-length(test.n)],test.n[-1]),
        get.smoothed.aging.rates(test.n, test.endpoints, test.endpoints[-length(test.endpoints)]),
        get.smoothed.aging.rates(test.n, test.endpoints, test.endpoints[-length(test.endpoints)], method='hyman')
    ),3)
    
    
    get.smoothed.aging.rates(test.n, test.endpoints, c(13,25,35,45,55))
    
    range = test.endpoints[1]:test.endpoints[length(test.endpoints)]
    qplot(c(range, (test.endpoints[-1]+test.endpoints[-length(test.endpoints)])/2), 
          c(get.smoothed.age.counts(test.n, test.endpoints, range), test.n*10/100),
          color = c(rep('smooth', length(range)), rep('obs', length(test.n))),
          shape = c(rep('smooth', length(range)), rep('obs', length(test.n))),
          geom='line')
    qplot(c(range, (test.endpoints[-1]+test.endpoints[-length(test.endpoints)])/2), 
          c(get.smoothed.age.counts(test.n, test.endpoints, range, method='hyman'), test.n*10/100),
          color = c(rep('smooth', length(range)), rep('obs', length(test.n))),
          shape = c(rep('smooth', length(range)), rep('obs', length(test.n))),
          geom='line')
    
    sum(get.smoothed.age.counts(test.n, test.endpoints, 0:12, as.proportions = F))
    sum(get.smoothed.age.counts(test.n, test.endpoints, 13:14, as.proportions = F))
    
    
    sum(get.smoothed.age.counts(test.n, test.endpoints, 130:149/10, as.proportions = F, resolution.in.years = 1/10))
    sum(get.smoothed.age.counts(test.n, test.endpoints, 0:129/10, as.proportions = F, resolution.in.years = 1/10))
    
    
    ehe.endpoints = c(13,25,35,45,55)
    
    
    # From 2000 data in table 10 of
    #  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2002-vol-14.pdf
    get.smoothed.aging.rates(observed.n.per.bracket = c(2816,513,4951,56552,151476,89716,23030,6263),
                             observed.age.endpoints = c(0,13,15,25,35,45,55,65,100),
                             desired.age.endpoints = c(13,25,35,45,55))
    
    
    # From 2000 data in table 10 of
    #  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-report-2002-vol-14.pdf
    get.smoothed.aging.rates(observed.n.per.bracket = c(2895,1179,7272,29953,50968,70292,91788,139770,171939,137404,88018,46811,34701),
                             observed.age.endpoints = c(0,13,15,20,25,30,35,40,45,50,55,60,65,100),
                             desired.age.endpoints = c(13,25,35,45,55))
    
    raw.counts = c(50968,70292,91788,139770,171939,137404)
    get.aging.rate.last.of.10(raw.counts[2*1:3-1],raw.counts[2*1:3])
    
    
    
    
}

get.smoothed.age.counts <- function(observed.n.per.bracket,
                                    observed.age.endpoints,
                                    desired.ages,
                                    as.proportions = F,
                                    method=c('monoH.FC','hyman')[1],
                                    resolution.in.years=1)
{
    if (!is.numeric(desired.ages))
        stop("'desired.ages' must be a NUMERIC vector")
    if (any(is.na(desired.ages)))
        stop("'desired.ages' cannot contain NA values")
    if (any(desired.ages<0))
        stop("'desired.ages' must contain only non-negative values")
    if (any(is.infinite(desired.ages)))
        stop("'desired.ages' must contain only finite values")
    
    fn = get.cumulative.age.smoother(observed.n.per.bracket = observed.n.per.bracket,
                                     observed.age.endpoints = observed.age.endpoints,
                                     method = method)
    
    counts = fn(desired.ages+resolution.in.years) - fn(desired.ages)
    
    if (as.proportions)
        counts / sum(counts)
    else
        counts
}

get.smoothed.age.bracket.counts <- function(observed.n.per.bracket,
                                            observed.age.endpoints,
                                            desired.age.endpoints,
                                            method=c('monoH.FC','hyman')[1])
{
    if (!is.numeric(desired.age.endpoints))
        stop("'desired.age.endpoints' must be a NUMERIC vector")
    if (any(is.na(desired.age.endpoints)))
        stop("'desired.age.endpoints' cannot contain NA values")
    if (any(desired.age.endpoints<0))
        stop("'desired.age.endpoints' must contain only non-negative values")
    if (any(is.infinite(desired.age.endpoints)))
        stop("'desired.age.endpoints' must contain only finite values")
    if (any(sort(desired.age.endpoints) != desired.age.endpoints))
        stop("'desired.age.endpoints' must be sorted in ascending order (with n.per.bracket correspondingly ordered)")
    
    
    fn = get.cumulative.age.smoother(observed.n.per.bracket = observed.n.per.bracket,
                                     observed.age.endpoints = observed.age.endpoints,
                                     method = method)
    
    fn(desired.age.endpoints[-1]) - fn(desired.age.endpoints[-length(desired.age.endpoints)])
}

get.smoothed.aging.rates <- function(observed.n.per.bracket,
                                     observed.age.endpoints,
                                     desired.age.endpoints,
                                     method=c('monoH.FC','hyman')[1],
                                     resolution.in.years=1)
{
    if (!is.numeric(desired.age.endpoints))
        stop("'desired.age.endpoints' must be a NUMERIC vector")
    if (any(is.na(desired.age.endpoints)))
        stop("'desired.age.endpoints' cannot contain NA values")
    if (any(desired.age.endpoints<0))
        stop("'desired.age.endpoints' must contain only non-negative values")
    if (any(is.infinite(desired.age.endpoints)))
        stop("'desired.age.endpoints' must contain only finite values")
    if (any(sort(desired.age.endpoints) != desired.age.endpoints))
        stop("'desired.age.endpoints' must be sorted in ascending order (with n.per.bracket correspondingly ordered)")
    
    
    fn = get.cumulative.age.smoother(observed.n.per.bracket = observed.n.per.bracket,
                                     observed.age.endpoints = observed.age.endpoints,
                                     method = method)
    
    cum.n = fn(desired.age.endpoints)
    cum.n.in.bracket = cum.n[-1]
    n.in.bracket = cum.n[-1] - cum.n[-length(desired.age.endpoints)]
    n.in.last.age.of.bracket = cum.n.in.bracket - fn(desired.age.endpoints[-1] - resolution.in.years)
    
    n.in.last.age.of.bracket / n.in.bracket
}

get.smoothed.age.specific.rates <- function(observed.n.per.bracket,
                                            observed.rates.per.bracket,
                                            observed.age.endpoints,
                                            desired.age.endpoints,
                                            method=c('monoH.FC','hyman')[1])
{
    #-- Check Desired Enpoints Argument --#
    
    if (!is.numeric(desired.age.endpoints))
        stop("'desired.age.endpoints' must be a NUMERIC vector")
    if (any(is.na(desired.age.endpoints)))
        stop("'desired.age.endpoints' cannot contain NA values")
    if (any(desired.age.endpoints<0))
        stop("'desired.age.endpoints' must contain only non-negative values")
    if (any(is.infinite(desired.age.endpoints)))
        stop("'desired.age.endpoints' must contain only finite values")
    if (any(sort(desired.age.endpoints) != desired.age.endpoints))
        stop("'desired.age.endpoints' must be sorted in ascending order (with n.per.bracket correspondingly ordered)")
    
    #-- Get Smoother For Age --#
    fn.age = get.cumulative.age.smoother(observed.n.per.bracket = observed.n.per.bracket,
                                         observed.age.endpoints = observed.age.endpoints,
                                         method = method)
    
    n.brackets = length(observed.n.per.bracket)
    
    #-- Check Observed Rates Argument --#
    if (!is.numeric(observed.rates.per.bracket))
        stop("'observed.rates.per.bracket' must be a NUMERIC vector with at least two elements")
    if (length(observed.rates.per.bracket)<2)
        stop("'observed.rates.per.bracket' must be have at least TWO elements")
    if (any(is.na(observed.rates.per.bracket)))
        stop("'observed.rates.per.bracket' cannot contain NA values")
    if (any(observed.rates.per.bracket<0))
        stop("'observed.rates.per.bracket' must contain only non-negative values")
    if (length(observed.rates.per.bracket) != n.brackets)
        stop(paste0("'observed.rates.per.bracket' (length ", length(observed.rates.per.bracket), 
                    ") must have the same length as observed.n.per.bracket (", length(observed.n.per.bracket), ")"))
    
    #-- Get Smoother For Events --#
    
    fn.events = get.cumulative.age.smoother(observed.n.per.bracket = observed.n.per.bracket * observed.rates.per.bracket,
                                            observed.age.endpoints = observed.age.endpoints,
                                            method = method)
    
    #-- Simulate n's and events by age and divide --#
    desired.cum.n = fn.age(desired.age.endpoints)
    desired.cum.events = fn.events(desired.age.endpoints)
    
    (desired.cum.events[-1] - desired.cum.events[-n.brackets]) /
        (desired.cum.n[-1] - desired.cum.n[-n.brackets])
}

get.cumulative.age.smoother <- function(observed.n.per.bracket,
                                        observed.age.endpoints,
                                        method=c('monoH.FC','hyman')[1])
{
    #-- Check Arguments --#
    if (!is.numeric(observed.n.per.bracket))
        stop("'observed.n.per.bracket' must be a NUMERIC vector with at least two elements")
    if (length(observed.n.per.bracket)<2)
        stop("'observed.n.per.bracket' must be have at least TWO elements")
    if (any(is.na(observed.n.per.bracket)))
        stop("'observed.n.per.bracket' cannot contain NA values")
    if (any(observed.n.per.bracket<0))
        stop("'observed.n.per.bracket' must contain only non-negative values")
    
    n.brackets = length(observed.n.per.bracket)
    
    if (!is.numeric(observed.age.endpoints))
        stop("'observed.age.endpoints' must be a NUMERIC vector")
    if (length(observed.age.endpoints) != (n.brackets+1))
        stop("'observed.age.endpoints' must have length == length(observed.n.per.bracket) + 1")
    if (any(is.na(observed.age.endpoints)))
        stop("'observed.age.endpoints' cannot contain NA values")
    if (any(observed.age.endpoints<0))
        stop("'observed.age.endpoints' must contain only non-negative values")
    if (any(is.infinite(observed.age.endpoints)))
        stop("'observed.age.endpoints' must contain only finite values")
    if (any(sort(observed.age.endpoints) != observed.age.endpoints))
        stop("'observed.age.endpoints' must be sorted in ascending order (with n.per.bracket correspondingly ordered)")

    
    obs.cum.n = cumsum(observed.n.per.bracket)
    splinefun(x=observed.age.endpoints, y=c(0,obs.cum.n), method=method)
}