

##-----------------------------##
##-- HELPERS FOR AGING RATES --##
##-----------------------------##

#assumes that the n in each age = m*a + b
# if we set a=0 for the last age bracket, then
# -35*m + 5*b = n.first.5 
#    and
# -10*m + 5*b = n.second.5
get.aging.rate.last.of.10 <- function(n.first.5, n.second.5)
{
    m = (n.second.5 - n.first.5) / 25
    b = (n.second.5 + 10*m) / 5
    
    b / (n.first.5 + n.second.5)
}

#assumes that the n in each age = m*a + b
# if we set a=0 for the last of the first 10 age brackets, then
# -45*m + 10*b = n.first.10
#    and
# 55*m + 10*b = n.second.10
get.aging.rate.mid.of.20 <- function(n.first.10, n.second.10)
{
    m = (n.second.10 - n.first.10) / 100
    b = (n.first.10 + 45*m) / 10
    
    b / n.first.10
}

##--------------------------------------------------------##
##-- HELPERS TO CREATE MULTIVARIATE COMPONENTS OF PRIOR --##
##--------------------------------------------------------##

create.transmission.prior.distribution <- function(r1.log.mean,
                                                   r1.log.sd,
                                                   rr.2.to.1.log.mean=0,
                                                   rr.2.to.1.log.sd,
                                                   rr.0.to.1.log.mean=rr.2.to.1.log.mean,
                                                   rr.0.to.1.log.sd=rr.2.to.1.log.sd,
                                                   #rr.peak.to.0.log.mean=rr.0.to.1.log.mean,
                                                   #rr.peak.to.0.log.sd=rr.0.to.1.log.sd,
                                                   race='black',
                                                   route=c('msm')
)
{
    mean = c(r1 = r1.log.mean,
             rr.2.to.1 = rr.2.to.1.log.mean,
             rr.0.to.1 = rr.0.to.1.log.mean)#,
#             rr.peak.to.0 = rr.peak.to.0.log.mean)

    var.mat = diag(c(r1.log.sd,
                     rr.2.to.1.log.sd,
                     rr.0.to.1.log.sd)^2)#,
#                     rr.peak.to.0.log.sd)^2)

    M = rbind(r0 = c(1,0,1),
              r1 = c(1,0,0),
              r2 = c(1,1,0))

    Multivariate.Lognormal.Distribution(mu = M %*% mean,
                                        sigma = M %*% var.mat %*% t(M),
                                        var.names = paste0(race,
                                                           '.',
                                                           route,
                                                           '.trate.',
                                                           c(0,1,2))
    )
}

create.age.stratified.transmission.prior.distribution <- function(r1.log.mean,
                                                                  r1.log.sd,
                                                                  rr.2.to.1.log.mean=0,
                                                                  rr.2.to.1.log.sd,
                                                                  rr.0.to.1.log.mean=rr.2.to.1.log.mean,
                                                                  rr.0.to.1.log.sd=rr.2.to.1.log.sd,
                                                                  age.stratifications,
                                                                  race='black',
                                                                  route=c('msm'))
{
    age.stratified.times = 1:2
    n.ages = length(age.stratifications)
  
    mean = c(r0 = r1.log.mean-rr.0.to.1.log.mean,
             rr.2.to.1. = rep(rr.2.to.1.log.mean,n.ages),
             rr.0.to.1. = rep(rr.0.to.1.log.mean,n.ages))
    
    var.mat = diag(c(r1.log.sd^2 + rr.0.to.1.log.sd^2,
                     rep(rr.2.to.1.log.sd^2, n.ages),
                     rep(rr.0.to.1.log.sd^2, n.ages)))
    
    M = c(r0 = c(1,rep(0,2*n.ages)))
    for (age in 1:n.ages)
        M = rbind(M, r1. = c(1, rep(0, n.ages), as.numeric(1:n.ages==age)))
    for (age in 1:n.ages)
        M = rbind(M, r2. = c(1, as.numeric(1:n.ages==age), rep(0, n.ages)))
    
    time.names = unlist(lapply(0:2, function(time){
      if (any(age.stratified.times==time))
        rep(time, n.ages)
      else
        time
    }))
    
    age.names = unlist(lapply(0:2, function(time){
      if (any(age.stratified.times==time))
        paste0('age', age.stratifications, ".")
      else
        ""
    }))
    
    
    Multivariate.Lognormal.Distribution(mu = M %*% mean,
                                        sigma = M %*% var.mat %*% t(M),
                                        var.names = paste0(age.names,
                                                           race,
                                                           '.',
                                                           route,
                                                           '.trate.',
                                                           time.names)
    )
}

create.mortality.prior.distribution <- function(mort2.log.mean = log(23/1000),
                                                mort2.log.sd = 0.25*log(2),
                                                mort2.to.0.log.mean = log(9.5/6.1),
                                                mort2.to.0.log.sd = 0.25*log(2),
                                                mort2.to.peak.log.mean = log(41/6.1),
                                                mort2.to.peak.log.sd = 0.5*log(2))
{
    mean = c(r2 = mort2.log.mean,
             rr2.to.0 = mort2.to.0.log.mean,
             rr.peak.to.0 = mort2.to.peak.log.mean)

    var.mat = diag(c(r2 = mort2.log.sd,
                rr2.to.0 = mort2.to.0.log.sd,
                rr.peak.to.0 = mort2.to.peak.log.sd))

    M = rbind(r.peak=c(1,0,1),
              r0 = c(1,1,0),
              r2 = c(1,0,0))

    Multivariate.Lognormal.Distribution(mu = M %*% mean,
                                        sigma = M %*% var.mat %*% t(M),
                                        var.names = c('peak.hiv.mortality','hiv.mortality.0','hiv.mortality.2'))
}

##--------------------------------------------------##
##-- TO CALCULATE ACTUAL VALUES TO GO INTO PRIORS --##
##--------------------------------------------------##


get.msm.age.susceptibility.1.vs.2 <- function(year1 = 2010,
                                              year2= 2019,
                                              ages = c('13-24 years', '25-34 years'),
                                              races = c('black','hispanic','other'))
{
    # Pull dx counts and map races
    dx = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk[as.character(c(year1, year2)), 'US',,,,]
    race.mapping = get.ontology.mapping(from.ontology = dimnames(dx)['race'],
                                        to.ontology = list(race=races))
    dx.mapped.race = race.mapping$apply(dx, na.rm=T)
    
    # Map age
    dx.mapped.age = restratify.age.counts(dx.mapped.race, ages, smooth.infinite.age.to = 100)
    
    # Calculate ratio time2/time1 for strata
    age.race.msm.dx.ratio = dx.mapped.age[2,,,'male','msm'] / dx.mapped.age[1,,,'male','msm']
    
    # Calculate the reference ratio time2/time1
    #    reference.dx.ratio = sum(dx[2,,,'male','msm'], na.rm=T) / sum(dx[1,,,'male','msm'], na.rm=T)
    #    reference.dx.ratio = apply(dx.mapped.race[2,,,'male','msm'], 'race', sum, na.rm=T) / apply(dx.mapped.race[1,,,'male','msm'], 'race', sum, na.rm=T)
    
    reference.dx.ratio = (apply(dx.mapped.race[2,,,'male','msm'], 'race', sum, na.rm=T) - apply(dx.mapped.age[2,,,'male','msm'], 'race', sum, na.rm=T)) /
      (apply(dx.mapped.race[1,,,'male','msm'], 'race', sum, na.rm=T) - apply(dx.mapped.age[1,,,'male','msm'], 'race', sum, na.rm=T))
                 
    # Return the ratio of ratios                              
    t(age.race.msm.dx.ratio) / reference.dx.ratio
}


get.sexual.susceptibility.by.age <- function(age.brackets,
                                             ref.age.index = ceiling(length(age.brackets)/2))
{
  #https://link.springer.com/article/10.1007/s10508-017-0953-1
  # from table 2
    raw.data = matrix(c(81.29, 80.28, 62.79, 46.31, 27.01, 9.68,
                        83.60, 76.34, 65.66, 48.76, 28.78, 10.66,
                        86.60, 83.08, 63.34, 43.08, 26.55, 13.13,
                        81.30, 79.76, 65.62, 43.12, 26.64, 9.00,
                        78.50, 77.84, 63.22, 38.29, 25.03, 10.88),
                      nrow = 6, ncol = 5,
                      dimnames = list(age=c('18-29 years', '30-39 years', '40-49 years', '50-59 years', '60-69 years', '70+ years'),
                                      year=c('1989-1994', '1995-1999', '2000-2004', '2005-2009', '2010-2014')))
    
    raw.frequency.by.age = rowMeans(raw.data)
    raw.parsed.ages = parse.age.strata.names(names(raw.frequency.by.age))
     max.age = 85
    
    raw.spans = pmin(100, raw.parsed.ages$upper) - raw.parsed.ages$lower
    tweaked.counts = raw.frequency.by.age * raw.spans
    
    parsed.desired.ages = parse.age.strata.names(age.brackets)
    min.age = parsed.desired.ages$lower[1]
    restratified.frequency = restratify.age.counts(tweaked.counts, 
                                                   desired.age.brackets = min.age:max.age, 
                                                   smooth.infinite.age.to = 100,
                                                   allow.extrapolation = T)
    
    linear.spline.multiplier = pmin(1, pmax(0, 1/(18-12) * (min.age:(max.age-1)-12)))
    
    single.year.sexual.availability = get.sexual.availability()
    
    single.year.frequency.times.availability = restratified.frequency * single.year.sexual.availability[(1+min.age):max.age] * linear.spline.multiplier
    
    aggregate.frequency.times.availability = sapply(1:length(age.brackets), function(a){
      
        indices = parsed.desired.ages$lower[a]:min(parsed.desired.ages$upper[a], max.age-1) 
        sub.freq = single.year.frequency.times.availability[indices - min.age + 1]
        sum(sub.freq) / length(sub.freq)
    })
    
    array(aggregate.frequency.times.availability / aggregate.frequency.times.availability[ref.age.index],
          dim = c(age=length(age.brackets)),
          dimnames = list(age=age.brackets))
}

get.idu.susceptibility.by.age <- function(age.brackets,
                                          ref.age.index = ceiling(length(age.brackets)/2))
{
  #idu by age from table 9 and 10 from
  #  https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-24.pdf
  # RR based on prob of daily use (heroin) * prob of needle sharing
  
  sharing = c('18-24 years' = 0.720,
              '25-29 years' = 0.705,
              '30-39 years' = 0.672,
              '40-49 years' = 0.596,
              '50+ years' = 0.498)
  
  daily.use = c('18-24 years' = 0.762,
                '25-29 years' = 0.797,
                '30-39 years' = 0.788,
                '40-49 years' = 0.745,
                '50+ years' = 0.740)
  
  raw.frequency.by.age = sharing * daily.use
  raw.parsed.ages = parse.age.strata.names(names(raw.frequency.by.age))
  
  max.age = 85
  
  raw.spans = pmin(100, raw.parsed.ages$upper) - raw.parsed.ages$lower
  tweaked.counts = raw.frequency.by.age * raw.spans
  
  parsed.desired.ages = parse.age.strata.names(age.brackets)
  min.age = parsed.desired.ages$lower[1]
  restratified.frequency = restratify.age.counts(tweaked.counts, 
                                                 desired.age.brackets = min.age:max.age, 
                                                 smooth.infinite.age.to = 100,
                                                 allow.extrapolation = T)
  
  aggregate.frequency.times.availability = sapply(1:length(age.brackets), function(a){
    
    indices = parsed.desired.ages$lower[a]:min(parsed.desired.ages$upper[a], max.age-1) 
    sub.freq = restratified.frequency[indices - min.age + 1]
    sum(sub.freq) / length(sub.freq)
  })
  
  array(aggregate.frequency.times.availability / aggregate.frequency.times.availability[ref.age.index],
        dim = c(age=length(age.brackets)),
        dimnames = list(age=age.brackets))
}

get.peak.susceptibility.by.age <- function(age.brackets,
                                           ref.age.index = ceiling(length(age.brackets)/2))
{
    aids.dx = apply(SURVEILLANCE.MANAGER$data$aids.diagnoses$estimate$cdc.aids$cdc.aids$year__location__age,
                    'age', sum, na.rm=T)
    
    aids.dx.restratified = restratify.age.counts(aids.dx, age.brackets, smooth.infinite.age.to = 100)
    aids.dx.restratified / aids.dx.restratified[ref.age.index]
}
