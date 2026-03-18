library(ggplot2)

# Fitting for truncated normal

# params.adap.basic = fit.tobit.normal.distribution.to.quantiles(p = adap.income.dist.2023,
#                                                                   upper.limit = 500)
params.adap = fit.tobit.normal.distribution.with.ssi.to.quantiles(p = adap.income.dist.2023,
                                                                  upper.limit = 500)

params.adap.black = fit.tobit.normal.distribution.with.ssi.to.quantiles(p = adap.black.income.dist.2023,
                                                                        upper.limit = 500)

params.adap.white = fit.tobit.normal.distribution.with.ssi.to.quantiles(p = adap.white.income.dist.2023,
                                                                        upper.limit = 500)

params.adap.hispanic = fit.tobit.normal.distribution.with.ssi.to.quantiles(p = adap.hispanic.income.dist.2023,
                                                                           upper.limit = 500)



est.p.full.pay.of.adap = pmin(1-frac.on.medicaid.by.income,
                              full.pay.income.counts.2023 / (full.pay.income.counts.2023 + adap.insurance.income.counts.2023))

sim.df.inc = data.frame(
    value = c(adap.black.income.dist.2023,
              adap.hispanic.income.dist.2023,
              adap.white.income.dist.2023),
    income = factor(rep(names(adap.income.dist.2023), 3), levels=names(adap.income.dist.2023)),
    race = rep(c("Black", "Hispanic", "White"), each=length(adap.income.dist.2023))
)
ggplot(sim.df.inc) + geom_bar(stat='identity', aes(x=income, y=value, fill=race), position = 'dodge') + facet_wrap(~race) +
    scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ylab("Proportion of Clients") + xlab("Income") + ggtitle("Income Distribution Among ADAP Clients")


sim.df.full.pay = data.frame(
    p = est.p.full.pay.of.adap,
    income = factor(names(est.p.full.pay.of.adap), levels=names(est.p.full.pay.of.adap))
)
ggplot(sim.df.full.pay) + geom_bar(stat='identity', aes(x=income, y=p), position = 'dodge')+
    scale_y_continuous(limits = c(0,1), labels = scales::percent) +
    ylab("Proportion with Full-Pay") + xlab("Income") + ggtitle("ADAP Clients Receiving Full-Pay Services")


z = cbind(
    all = as.numeric(params.adap),
    black = as.numeric(params.adap.black),
    white = as.numeric(params.adap.white),
    hispanic = as.numeric(params.adap.hispanic)
)


params.logistic = fit.logistic.parameters.to.p.distribution(
    dist.parameters = list(
        params.adap,
        params.adap.black,
        params.adap.white,
        params.adap.hispanic
    ),
    p = list(
        est.p.full.pay.of.adap,
        p.full.pay.of.adap.black,
        p.full.pay.of.adap.white,
        p.full.pay.of.adap.hispanic
    )
)

params.logistic = fit.logistic.parameters.to.p.distribution(
    dist.parameters = list(
        params.adap
    ),
    p = list(
        est.p.full.pay.of.adap
    ),
    vary.max = F
)



sim.df.race.tnorm = data.frame(
    p = c(p.full.pay.of.adap.black,
          p.full.pay.of.adap.white,
          p.full.pay.of.adap.hispanic,
          est.p.full.pay.of.adap,
          
          get.cumulative.logistic.times.distribution.p(params.adap.black,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.distribution.p(params.adap.white,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.distribution.p(params.adap.hispanic,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.distribution.p(params.adap,
                                                           params.logistic,
                                                           lower=parse.income.brackets(names(full.pay.income.counts.2023))$lower,
                                                           upper=parse.income.brackets(names(full.pay.income.counts.2023))$upper)
    ),
    race = c('black','white','hispanic',
             paste0(rep('all: ', 5), names(full.pay.income.counts.2023)),
             'black','white','hispanic',
             paste0(rep('all: ', 5), names(full.pay.income.counts.2023))),
    type = rep(c('obs','sim'), each=8)
)
ggplot(sim.df.race.tnorm) + geom_bar(stat='identity', aes(x=race, y=p, fill=type), position = 'dodge')

# fitting to zero-inflated lognormal


params.adap = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.income.dist.2023,
                                                             upper.limit = 500)

params.adap.black = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.black.income.dist.2023,
                                                                   upper.limit = 500)

params.adap.white = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.white.income.dist.2023,
                                                                   upper.limit = 500)

params.adap.hispanic = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.hispanic.income.dist.2023,
                                                                      upper.limit = 500)



params.logistic = fit.logistic.parameters.to.p.of.zero.inflated.lognormal(
    lognormal.parameters = list(
        params.adap,
        params.adap.black,
        params.adap.white,
        params.adap.hispanic
    ),
    p = list(
        full.pay.income.counts.2023 / (full.pay.income.counts.2023 + adap.insurance.income.counts.2023),
        p.full.pay.of.adap.black,
        p.full.pay.of.adap.white,
        p.full.pay.of.adap.hispanic
    )
)

params.logistic = fit.logistic.parameters.to.p.of.zero.inflated.lognormal(
    lognormal.parameters = list(
        params.adap
    ),
    p = list(
        full.pay.income.counts.2023 / (full.pay.income.counts.2023 + adap.insurance.income.counts.2023)
    )
)



sim.df.race.lognorm = data.frame(
    p = c(p.full.pay.of.adap.black,
          p.full.pay.of.adap.white,
          p.full.pay.of.adap.hispanic,
          full.pay.income.counts.2023 / (full.pay.income.counts.2023 + adap.insurance.income.counts.2023),
          
          get.cumulative.logistic.times.zero.inflated.lognormal.p(params.adap.black,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.zero.inflated.lognormal.p(params.adap.white,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.zero.inflated.lognormal.p(params.adap.hispanic,
                                                           params.logistic,
                                                           lower=0, upper=500),
          get.cumulative.logistic.times.zero.inflated.lognormal.p(params.adap,
                                                           params.logistic,
                                                           lower=parse.income.brackets(names(full.pay.income.counts.2023))$lower,
                                                           upper=parse.income.brackets(names(full.pay.income.counts.2023))$upper)
    ),
    race = c('black','white','hispanic',
             paste0(rep('all: ', 5), names(full.pay.income.counts.2023)),
             'black','white','hispanic',
             paste0(rep('all: ', 5), names(full.pay.income.counts.2023))),
    type = rep(c('obs','sim'), each=8)
)
ggplot(sim.df.race.lognorm) + geom_bar(stat='identity', aes(x=race, y=p, fill=type), position = 'dodge')



# vs mmp
fit.tobit.normal.distribution.to.quantiles(p = mmp.black.income.dist.2022,
                                               upper.limit = 1000)
fit.two.param.dist.to.quantiles(p = mmp.black.income.dist.2022,
                                            q = c(100, 138, 400, 1000),
                                            upper.limit = 1000)
fit.zero.inflated.lognormal.distribution(p = mmp.black.income.dist.2022,
                                q = c(100, 138, 400, 500),
                                upper.limit = 500)


fit.two.param.dist.to.quantiles(p = mmp.black.income.dist.2022,
                                q = c(100, 138, 400, 1000),
                                upper.limit = 1000)



fit.zero.inflated.lognormal.distribution.to.quantiles(p = mmp.income.dist.2022, upper.limit = 1000000)

pp.all = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.income.dist.2023, upper.limit = 500)
pp.all.mmp = fit.zero.inflated.lognormal.distribution.to.quantiles(p = mmp.black.income.dist.2022, upper.limit = 100000)

pp.black = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.black.income.dist.2023, upper.limit = 500)
pp.black.mmp = fit.zero.inflated.lognormal.distribution.to.quantiles(p = mmp.black.income.dist.2022, upper.limit = 100000)

pp.white = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.white.income.dist.2023, upper.limit = 500)
pp.white.mmp = fit.zero.inflated.lognormal.distribution.to.quantiles(p = mmp.white.income.dist.2022, upper.limit = 100000)

pp.hispanic = fit.zero.inflated.lognormal.distribution.to.quantiles(p = adap.hispanic.income.dist.2023, upper.limit = 500)
pp.hispanic.mmp = fit.zero.inflated.lognormal.distribution.to.quantiles(p = mmp.hispanic.income.dist.2022, upper.limit = 100000)

z=cbind(
    all = as.numeric(pp.all),
    all.mmp = as.numeric(pp.all.mmp),
    black = as.numeric(pp.black),
    black.mmp = as.numeric(pp.black.mmp),
    hispanic = as.numeric(pp.hispanic),
    hispanic.mmp = as.numeric(pp.hispanic.mmp),
    white = as.numeric(pp.white),
    white.mmp = as.numeric(pp.white.mmp)
)

z.mmp=cbind(
    all.mmp = as.numeric(pp.all.mmp),
    black.mmp = as.numeric(pp.black.mmp),
    hispanic.mmp = as.numeric(pp.hispanic.mmp),
    white.mmp = as.numeric(pp.white.mmp)
)

z[,1:4*2-1] / z[,1:4*2]

fit.tobit.normal.distribution.to.quantiles(p = fl.adap.income.dist.2023, upper.limit = 500)


pp=pp.all; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.black; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.white; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.hispanic; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))



pp=pp.all; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd)) / 
    (plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero) / plnorm(200, pp$log.mean, pp$log.sd)*(1-pp$p.zero))
pp=pp.black; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd)) / 
    (plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero) / plnorm(200, pp$log.mean, pp$log.sd)*(1-pp$p.zero))
pp=pp.white; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd)) / 
    (plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero) / plnorm(200, pp$log.mean, pp$log.sd)*(1-pp$p.zero))
pp=pp.hispanic; pp$p.zero / (pp$p.zero + plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd)) / 
    (plnorm(100, pp$log.mean, pp$log.sd)*(1-pp$p.zero) / plnorm(200, pp$log.mean, pp$log.sd)*(1-pp$p.zero))


pp=pp.all; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.black; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.white; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))
pp=pp.hispanic; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/plnorm(pp$upper.limit, pp$log.mean, pp$log.sd))


pp=pp.all; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/(plnorm(200, pp$log.mean, pp$log.sd) - plnorm(100, pp$log.mean, pp$log.sd)))
pp=pp.black; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/(plnorm(200, pp$log.mean, pp$log.sd) - plnorm(100, pp$log.mean, pp$log.sd)))
pp=pp.white; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/(plnorm(200, pp$log.mean, pp$log.sd) - plnorm(100, pp$log.mean, pp$log.sd)))
pp=pp.hispanic; pp$p.zero / (plnorm(100, pp$log.mean, pp$log.sd)/(plnorm(200, pp$log.mean, pp$log.sd) - plnorm(100, pp$log.mean, pp$log.sd)))
