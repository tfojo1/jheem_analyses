

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

