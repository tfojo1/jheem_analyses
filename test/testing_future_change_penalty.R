

do.assess.likelihood <- function(lik,
                                 x = seq(-1, 1, length=2000),
                                 points.of.interest=c(pre.change = delta.pre,
                                                      post.change = delta.post,
                                                      pre.plus.10 = delta.pre + .1,
                                                      pre.minus.10 = delta.pre - .1,
                                                      pre.plus.20 = delta.pre + .2,
                                                      pre.minus.20 = delta.pre - .2,
                                                      '+0.5x' = 0.5,
                                                      '-0.5x' = -0.5))
{
    #x = seq(0.01, 2, length=2000)
    d = lik(x)
    
    max.d = max(d)
    mle = x[d==max(d)][1]
    
    d.of.interest = lik(points.of.interest)
    
    if (any(d.of.interest>max.d))
    {
        max.d = max(d.of.interest)
        mle = points.of.interest[d.of.interest==max.d][1]
    }
    
    print(qplot(x, d, geom='point') + ylim(0,NA) +
      geom_vline(xintercept=points.of.interest) +
      geom_vline(xintercept = mle, size=2))
    
    cat(paste0("The MLE is ", round(mle,3), "\n"))
    cat(paste0(
      paste0(" ", names(points.of.interest), " = ", d.of.interest/max.d),
      collapse='\n'
    ))
}


# DATA (from Atlanta sim)

diagnoses = c('1970' = 0, '1971' = 0, '1972' = 0, '1973' = 0, '1974' = 0, '1975' = 0, '1976' = 0, '1977' = 0, '1978' = 0, '1979' = 0, '1980' = 0, '1981' = 0, '1982' = 13.6976979870208, '1983' = 35.4416936335297, '1984' = 55.1799546564832, '1985' = 85.8073204650339, '1986' = 132.242692869172, '1987' = 201.898870658319, '1988' = 303.350563055016, '1989' = 445.75497416242, '1990' = 634.138468752179, '1991' = 862.279918467956, '1992' = 1101.82869391426, '1993' = 1302.40556826996, '1994' = 1405.56634399267, '1995' = 1235.05586783861, '1996' = 1032.66140441804, '1997' = 955.021616390943, '1998' = 951.171464586697, '1999' = 996.417291177561, '2000' = 1078.36460227139, '2001' = 1188.50871370098, '2002' = 1315.28708903317, '2003' = 1443.78772797755, '2004' = 1558.01971031265, '2005' = 1643.47474575973, '2006' = 1689.53547710392, '2007' = 1708.52187387968, '2008' = 1701.03186763924, '2009' = 1653.08158806495, '2010' = 1548.30495752861, '2011' = 1456.60465268351, '2012' = 1397.36752806726, '2013' = 1342.97332843393, '2014' = 1294.74757289253, '2015' = 1276.02558927678, '2016' = 1303.17210735184, '2017' = 1354.25628115821, '2018' = 1418.32775989298, '2019' = 1478.9326056211, '2020' = 1164.39767159095, '2021' = 1274.2472050272, '2022' = 1559.43497353144, '2023' = 1597.40402888944, '2024' = 1622.91314613111, '2025' = 1653.37452392644, '2026' = 1690.75512347355, '2027' = 1735.7492908632, '2028' = 1788.33414560403, '2029' = 1847.50669342778, '2030' = 1911.61730288624)

diag.pre1 = diagnoses['2009']
diag.pre2 = diagnoses['2019']

diag.post1 = diagnoses['2019']
diag.post2 = diagnoses['2029']


# UNIFORM LIKELIHOOD (for testing)

unif.lik = function(x)
{
  rep(1, length(x))
}

do.assess.likelihood(unif.lik)



# NORMAL ON ABS SCALE AROUND CHANGE
delta.pre = (diag.pre2 - diag.pre1) / diag.pre1
delta.post = (diag.post2 - diag.post1) / diag.post1

lik.abs.delta <- function(x){
  dnorm(x, delta.pre, .1)
}

do.assess.likelihood(lik.abs.delta)


# NORMAL MIX ON ABS SCALE
lik.abs.delta.mix <- function(x){
  
  spread = 0.15
#  spread = 0.125
  
  0.5 * dnorm(x, delta.pre - spread, spread) +
    0.5 * dnorm(x, delta.pre + spread, spread)
}
do.assess.likelihood(lik.abs.delta.mix)

# OTHER

pre.change = .972
log.sd = 1.2

mean1 = 1.1 * pre.change
mean2 = pre.change / 1.1
mix.weight = 0.5

post.change = 1.1

x= post.change
(mix.weight * dlnorm(x, meanlog = mean1, sdlog = log.sd) + (1-mix.weight) * dlnorm(x, meanlog = mean2, sdlog = log.sd)) / 
  (mix.weight * dlnorm((mean1+mean2)/2, meanlog = mean1, sdlog = log.sd) + (1-mix.weight) * dlnorm((mean1+mean2)/2, meanlog = mean2, sdlog = log.sd))
  

x = seq(0.01,2, length=1000)
d = (mix.weight * dlnorm(x, meanlog = mean1, sdlog = log.sd) + (1-mix.weight) * dlnorm(x, meanlog = mean2, sdlog = log.sd)) / 
  (mix.weight * dlnorm((mean1+mean2)/2, meanlog = mean1, sdlog = log.sd) + (1-mix.weight) * dlnorm((mean1+mean2)/2, meanlog = mean2, sdlog = log.sd))

library(ggplot2)

qplot(x=x, y=d, geom='point') + geom_vline(xintercept = c(pre.change, post.change, 0.5, 1.5))
