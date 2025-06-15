
if (!exists('RW.effect.values'))
    source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

ADJUST.ADAP = T
ADJUST.OAHS = F
ADJUST.SUPPORT = F

OAHS.SD.INFLATION = 1
SUPPORT.SD.INFLATION = 1

N.ITER = 20000
N.CHAINS = 2
KEEP = 1000
BURN = 0.5 * N.ITER
THIN = N.CHAINS * (N.ITER - BURN) / KEEP
RETAIN.ITERATION.INDICES = N.ITER/THIN - KEEP/N.CHAINS + (1:(KEEP/N.CHAINS))

target.acceptance.rate=0.238

scaling.base.update = 1
scaling.update.prior=100
scaling.update.decay=.5

n.iter.before.cov = 0
cov.base.update=1
cov.update.prior=500
cov.update.decay=1


set.seed(12333533)

##---------------------##
##-- GENERAL HELPERS --##
##---------------------##

prepare.kde.arcsin <- function(service.type, expansion)
{
    SERVICE.INDICES = c(adap=1, oahs=2, support=3)
    
    # Mask by medicaid status
    expansion.states = names(STATE.MEDICAID.EXPANSION)[STATE.MEDICAID.EXPANSION]
    expansion.states = gsub("_",".",expansion.states)
    expansion.col.names = paste0("X.choice.", expansion.states, ".")
    
    medicaid.expansion.mask = rowSums(rw_survey[,expansion.col.names]) > 0
    
    
    non.expansion.states = names(STATE.MEDICAID.EXPANSION)[!STATE.MEDICAID.EXPANSION]
    non.expansion.states = gsub("_",".",non.expansion.states)
    non.expansion.col.names = paste0("X.choice.", non.expansion.states, ".")
    
    medicaid.nonexpansion.mask = rowSums(rw_survey[,non.expansion.col.names]) > 0
    
    # Extract relevant columns, remove rows with NA values, and scale 0->1
    X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
    X.expansion = X[medicaid.expansion.mask,]
    X.nonexpansion = X[medicaid.nonexpansion.mask,]
    
    X = X[complete.cases(X), ]/100
    X.expansion = X.expansion[complete.cases(X.expansion), ]/100
    X.nonexpansion = X.nonexpansion[complete.cases(X.nonexpansion), ]/100
    
    
    
    ## Convert to matrix for KDE estimation
    X = as.matrix(X)
    X.expansion = as.matrix(X.expansion)
    X.nonexpansion = as.matrix(X.nonexpansion)
    
    .GlobalEnv$rw.survey = X
    .GlobalEnv$rw.survey.expansion = X.expansion
    .GlobalEnv$rw.survey.nonexpansion = X.nonexpansion
    
    # Small constant to avoid logit errors
    epsilon = 1e-6  
    X = pmax(pmin(X, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
    X.expansion = pmax(pmin(X.expansion, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
    X.nonexpansion = pmax(pmin(X.nonexpansion, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
    
    
    
    # Arcsin transformation
    arcsin_X = asin(sqrt(X))
    arcsin_X.expansion = asin(sqrt(X.expansion))
    arcsin_X.nonexpansion = asin(sqrt(X.nonexpansion))
    
    # Fit KDE in arcsin space
    #    kde_fit = kde(x = arcsin_X )
    
    if (expansion) 
        kde_fit = kde(x = arcsin_X.expansion[,SERVICE.INDICES[service.type]])
    else
        kde_fit = kde(x = arcsin_X.nonexpansion[,SERVICE.INDICES[service.type]])
}

create.arcsin.kde.prior <- function(kde.fit, param.name)
{
    function(parameters)
    {
        x = parameters[param.name]
        log(dkde(asin(sqrt(x)), kde.fit) / 2 / sqrt(-((x-1)*x)))
    }
}

run.adjust.mcmc <- function(service.type, 
                            run.sim,
                            likelihood, 
                            expansion,
                            start.value = 0.5)
{
    param.name = paste0(service.type, '.loss')
    
    prior = create.arcsin.kde.prior(kde.fit = prepare.kde.arcsin(service.type=service.type, expansion=expansion), 
                                    param.name = param.name)
    
    tsfx = 'logit'
    names(tsfx) = param.name
    
    ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = param.name,
        simulation.function = run.sim,
        log.prior.distribution = prior,
        log.likelihood = likelihood,
        burn = 0,
        thin = THIN,
        var.blocks = as.list(param.name),
        reset.adaptive.scaling.update.after = 0,
        transformations = tsfx,
        
        initial.covariance.mat = diag(1)/400,
        initial.scaling.parameters = 1,
        
        target.acceptance.probability = target.acceptance.rate,
        
        n.iter.before.use.adaptive.covariance = n.iter.before.cov,
        adaptive.covariance.base.update = cov.base.update,
        adaptive.covariance.update.prior.iter = cov.update.prior,
        adaptive.covariance.update.decay = cov.update.decay,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = scaling.base.update,
        adaptive.scaling.update.prior.iter= scaling.update.prior,
        adaptive.scaling.update.decay = scaling.update.decay
    )
    
    mcmc = NULL
        
    for (i in 1:N.CHAINS)
    {
        one.chain = bayesian.simulations::run.mcmc(control = ctrl,
                             n.iter = N.ITER, 
                             starting.values = start.value,
                             cache.frequency = NA)
        
        if (is.null(mcmc))
            mcmc = one.chain
        else
            mcmc = bayesian.simulations::mcmc.merge.parallel(mcmc, one.chain)
    }
    
    values = as.numeric(mcmc@samples[,RETAIN.ITERATION.INDICES,])
    
    values
}

resample.adjust <- function(n = KEEP,
                            service.type, 
                            run.sim,
                            likelihood, 
                            expansion)
{
    n.sample = 1000*n
    
    
    param.name = paste0(service.type, '.loss')
    
    prior.kde = prepare.kde.arcsin(service.type=service.type, expansion=expansion)
    
    samples = seq(0,1, length=n.sample)
    
    d.prior = dkde(samples, prior.kde)
    sims = lapply(samples, function(x){
        names(x) = param.name
        run.sim(x)
    })
    d.lik = exp(sapply(sims, likelihood))
    
    d.post = d.prior * d.lik
    cum.post = cumsum(d.post) / sum(d.post)
    
    rands = runif(n)
    
    sapply(rands, function(rand){
        lte.samples = samples[rand >= cum.post]
        lte.samples[length(lte.samples)]
    })
}

##--------------------------##
##-- Parse Diepstra Study --##
##--------------------------##

# From 
# https://pmc.ncbi.nlm.nih.gov/articles/PMC5848228/

p.comprehensive = .882
o.comprehensive = p.comprehensive / (1-p.comprehensive)
or.comprehensive = 4.3
ci.comprehensive = c(3.7, 5.0)

o.none = o.comprehensive / or.comprehensive
p.none = o.none/(1+o.none)

or.core = 1.5
ci.core = c(1.3, 1.8)
o.core = o.none * or.core
p.core = o.core/(1+o.core)

or.core.support = 2.9
ci.core.support = c(2.5, 3.4)
o.core.support = o.none * or.core.support
p.core.support = o.core.support / (1+o.core.support)

or.core.adap = 1.9
ci.core.adap = c(1.6, 2.1)
o.core.adap = o.none * or.core.adap
p.core.adap = o.core.adap/(1+o.core.adap)

or.support.adap = 1.7
ci.support.adap = c(1.2, 2.5)
o.support.adap = o.none * or.support.adap
p.support.adap = o.support.adap / (1+o.support.adap)

or.adap = 0.91
ci.adap = c(0.79, 1.0)
o.adap = o.none * or.adap
p.adap = o.adap / (1+o.adap)

or.support = 0.75
ci.support = c(0.59, 0.96)
o.support = o.none * or.support
p.support = o.support / (1+o.support)

n.comprehensive = 2228
n.core.adap = 1353
n.support.adap = 178
n.adap = 1098
n.core.support = 1536
n.core = 855
n.support = 298

log.mean.core = log(or.core)
log.sd.core = (log(ci.core[2]) - log(ci.core[1])) / 2/ 1.96

log.mean.core.support = log(or.core.support)
log.sd.core.support = (log(ci.core.support[2]) - log(ci.core.support[1])) / 2/ 1.96

log.mean.comprehensive = log(or.comprehensive)
log.sd.comprehensive = (log(ci.comprehensive[2]) - log(ci.comprehensive[1])) / 2/ 1.96

log.mean.core.adap = log(or.core.adap)
log.sd.core.adap = (log(ci.core.adap[2]) - log(ci.core.adap[1])) / 2/ 1.96

log.mean.support.adap = log(or.support.adap)
log.sd.support.adap = (log(ci.support.adap[2]) - log(ci.support.adap[1])) / 2/ 1.96

log.mean.adap = log(or.adap)
log.sd.adap = (log(ci.adap[2]) - log(ci.adap[1])) / 2/ 1.96

log.mean.support = log(or.support)
log.sd.support = (log(ci.support[2]) - log(ci.support[1])) / 2/ 1.96


# Other studies showing benefit:
#   https://pmc.ncbi.nlm.nih.gov/articles/PMC5087096/

##---------------------------------##
##--   SET UP ADDITIONAL ERROR   --##
##--    Based on variation in    --##
##-- p/odds suppression by state --##
##---------------------------------##

#-- Calculate the N0 for the beta-binomial for P suppressed --#

p.expansion = RW.DATA.MANAGER$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location[,MEDICAID.EXPANSION.STATES]
#p.expansion = p.expansion[!is.na(p.expansion)]

p.nonexpansion = RW.DATA.MANAGER$data$oahs.suppression$estimate$ryan.white.program$ryan.white.pdfs$year__location[,MEDICAID.NONEXPANSION.STATES]
#p.nonexpansion = p.nonexpansion[!is.na(p.nonexpansion)]


mse.p.expansion = mean(apply(p.expansion, 'year', var, na.rm=T)) #var(as.numeric(p.expansion), na.rm = T)
mse.p.nonexpansion = (mean(p.nonexpansion, na.rm = T)-mean(p.expansion, na.rm = T))^2 + var(as.numeric(p.nonexpansion), na.rm = T)


p.for.erly = 1 - 0.64/.82

N0.EXPANSION.FOR.ERLY = p.for.erly * (1-p.for.erly) / rmse.expansion - 1
N0.NONEXPANSION.FOR.ERLY = p.for.erly * (1-p.for.erly) / rmse.nonexpansion - 1

#-- Calculate the log var for Odds Suppressed --#

log.odds.expansion = log(p.expansion) - log(1-p.expansion)
log.odds.nonexpansion = log(p.nonexpansion) - log(1-p.nonexpansion)

MSE.LOG.ODDS.EXPANSION = mean(apply(log.odds.expansion, 'year', var, na.rm=T))#var(log.odds.expansion)
MSE.LOG.ODDS.NONEXPANSION = (mean(log.odds.nonexpansion, na.rm=T)-mean(log.odds.expansion, na.rm=T))^2 + mean(apply(log.odds.nonexpansion, 'year', var, na.rm=T)) #var(log.odds.expansion)


##-----------------##
##-- ADJUST ADAP --##
##-----------------##

adjust.adap.run.sim = function(parameters)
{
    adap.loss = parameters['adap.loss']
    
    # From erly et al
    # https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0285326
    
    n.long.term.disenrolled = 174
    
    # Assumption
    n.long.term.disenrolled.previously.suppressed = round(0.82 * n.long.term.disenrolled)
    n.long.term.disenrolled.subsequently.suppressed = round(0.64 * n.long.term.disenrolled)
    n.long.term.disenrolled.newly.unsuppressed = n.long.term.disenrolled.previously.suppressed - n.long.term.disenrolled.subsequently.suppressed

    # Package it up
    list(
        n.long.term.disenrolled.newly.unsuppressed = n.long.term.disenrolled.newly.unsuppressed,
        n.long.term.disenrolled.previously.suppressed = n.long.term.disenrolled.previously.suppressed,
        n.long.term.disenrolled = n.long.term.disenrolled,
        adap.loss = adap.loss,
        
        p.comprehensive = p.comprehensive * (1-adap.loss),
        p.core.adap = p.core.adap * (1-adap.loss),
        p.support.adap = p.support.adap * (1-adap.loss),
        p.adap = p.adap * (1-adap.loss)
    )
}

adjust.adap.likelihood.expansion <- function(sim, log=T)
{
    adjust.adap.likelihood(sim, log=log, n0 = N0.EXPANSION.FOR.ERLY)
}

adjust.adap.likelihood.nonexpansion <- function(sim, log=T)
{
    adjust.adap.likelihood(sim, log=log, n0 = N0.NONEXPANSION.FOR.ERLY)
}

adjust.adap.likelihood <- function(sim, log=T, n0)
{
    # d.erly = dbinom(x = round(sim$n.long.term.disenrolled.newly.unsuppressed/1),
    #        size = round(sim$n.long.term.disenrolled.previously.suppressed/1),
    #        prob = sim$adap.loss,
    #        log = log)
    
    
    d.erly = dnorm(x = sim$n.long.term.disenrolled.newly.unsuppressed,
                   mean = sim$n.long.term.disenrolled.previously.suppressed * sim$adap.loss, #np
                   sd = sqrt(sim$n.long.term.disenrolled.previously.suppressed * sim$adap.loss * (1-sim$adap.loss) * (n0 + sim$n.long.term.disenrolled.previously.suppressed) / (n0 + 1)),
                   log = log
                   )
    
    # log.or.comprehensive = log(o.comprehensive) - log(sim$p.comprehensive) + log(1-sim$p.comprehensive)
    # log.or.core.adap = log(o.core.adap) - log(sim$p.core.adap) + log(1-sim$p.core.adap)
    # log.or.support.adap = log(o.support.adap) - log(sim$p.support.adap) + log(1-sim$p.support.adap)
    # log.or.adap = log(o.adap) - log(sim$p.adap) + log(1-sim$p.adap)
    # 
    # d.diepstra = log(n.comprehensive * dnorm(log.or.comprehensive, log.mean.comprehensive, log.sd.comprehensive, log=F) +
    #     n.core.adap * dnorm(log.or.core.adap, log.mean.core.adap, log.sd.core.adap, log=F) +
    #   #  n.support.adap * dnorm(log.or.support.adap, log.mean.support.adap, log.sd.support.adap, log=F) +
    #     n.adap * dnorm(log.or.adap, log.mean.adap, log.sd.adap, log=F)) - 
    #     log(n.comprehensive + n.core.adap + n.adap)
    
    
    #    d.diepstra + d.erly

    # return
    d.erly

}


if (ADJUST.ADAP)
{
    
    adjusted.adap.expansion.values = resample.adjust(n = KEEP,
                                                     service.type = 'adap', 
                                                     run.sim = adjust.adap.run.sim,
                                                     likelihood = adjust.adap.likelihood.expansion,
                                                     expansion = T)
    
    adjusted.adap.nonexpansion.values = resample.adjust(n = KEEP,
                                                        service.type = 'adap', 
                                                        run.sim = adjust.adap.run.sim,
                                                        likelihood = adjust.adap.likelihood.nonexpansion,
                                                        expansion = F)
    
    # adjusted.adap.expansion.values = run.adjust.mcmc(service.type = 'adap', 
    #                                                  run.sim = adjust.adap.run.sim,
    #                                                  likelihood = adjust.adap.likelihood.expansion,
    #                                                  expansion = T,
    #                                                  start.value = 0.22)
    # 
    # adjusted.adap.nonexpansion.values = run.adjust.mcmc(service.type = 'adap', 
    #                                                     run.sim = adjust.adap.run.sim,
    #                                                     likelihood = adjust.adap.likelihood.nonexpansion,
    #                                                     expansion = F,
    #                                                     start.value = 0.22)
}


##-----------------##
##-- ADJUST OAHS --##
##-----------------##

adjust.oahs.run.sim = function(parameters)
{
    oahs.loss = parameters['oahs.loss']
    
    list(
        oahs.loss = oahs.loss,
        
        p.core = p.core * (1-oahs.loss),
        p.core.support = p.core.support * (1-oahs.loss),
        p.comprehensive = p.comprehensive * (1-oahs.loss)
    )
}

adjust.oahs.likelihood.expansion <- function(sim, log=T)
{
    adjust.oahs.likelihood(sim, mse=MSE.LOG.ODDS.EXPANSION, log=log)
}

adjust.oahs.likelihood.nonexpansion <- function(sim, log=T)
{
    adjust.oahs.likelihood(sim, mse=MSE.LOG.ODDS.NONEXPANSION, log=log)
}

adjust.oahs.likelihood <- function(sim, mse, log=T)
{
    log.or.core = log(o.core) - log(sim$p.core) + log(1-sim$p.core)
    d.core = dnorm(log.or.core, log.mean.core, sqrt(log.sd.core^2 + mse), log=T)
    
   log.or.core.support = log(o.core.support) - log(sim$p.core.support) + log(1-sim$p.core.support)
   d.core.support = dnorm(log.or.core.support, log.mean.core.support, sqrt(log.sd.core.support^2 + mse), log=T)
 
  # d.core.support
           
    d.core + d.core.support
   
#   log(d.core * n.core + d.core.support * n.core.support) - log(n.core + n.core.support)
}

if (ADJUST.OAHS)
{   
    adjusted.oahs.expansion.values = resample.adjust(n = KEEP,
                                                     service.type = 'oahs', 
                                                     run.sim = adjust.oahs.run.sim,
                                                     likelihood = adjust.oahs.likelihood.expansion,
                                                     expansion = T)
    
    adjusted.oahs.nonexpansion.values = resample.adjust(n = KEEP,
                                                     service.type = 'oahs', 
                                                     run.sim = adjust.oahs.run.sim,
                                                     likelihood = adjust.oahs.likelihood.expansion,
                                                     expansion = F)
    
    
#     adjusted.oahs.expansion.values = run.adjust.mcmc(service.type = 'oahs', 
#                                                      run.sim = adjust.oahs.run.sim,
#                                                      likelihood = adjust.oahs.likelihood.expansion,
#                                                      expansion = T)
#     
#     adjusted.oahs.nonexpansion.values = run.adjust.mcmc(service.type = 'oahs', 
#                                                         run.sim = adjust.oahs.run.sim,
#                                                         likelihood = adjust.oahs.likelihood.nonexpansion,
#                                                         expansion = F)
 }

    
##--------------------##
##-- ADJUST Support --##
##--------------------##

adjust.support.run.sim = function(parameters)
{
    support.loss = parameters['support.loss']
    
    
    list(
        support.loss = support.loss,
        
        p.support = p.support * (1-support.loss),
        p.core.support = p.core.support * (1-support.loss),
        p.support.adap = p.support.adap * (1-support.loss),
        p.comprehensive = p.comprehensive * (1-support.loss)
    )
}

adjust.support.likelihood.expansion <- function(sim, log=T)
{
    adjust.support.likelihood(sim, mse=MSE.LOG.ODDS.EXPANSION, log=log)
}

adjust.support.likelihood.nonexpansion <- function(sim, log=T)
{
    adjust.support.likelihood(sim, mse=MSE.LOG.ODDS.NONEXPANSION, log=log)
}

adjust.support.likelihood <- function(sim, mse, log=T)
{
    log.or.core.support = log(o.core.support) - log(sim$p.core.support) + log(1-sim$p.core.support)
    log.or.core = log(o.core) - log(o.none)
    
    d.core.support = dnorm(log.or.core.support,# - log.or.core,
          mean = log.mean.core.support - log.mean.core,
          sd = sqrt(log.sd.core.support^2 + log.sd.core^2 + mse),
          log = T)
    
    
    log.or.support.adap = log(o.support.adap) - log(sim$p.support.adap) + log(1-sim$p.support.adap)
    log.or.adap = log(o.adap) - log(o.none)
    
    d.support.adap = dnorm(log.or.support.adap,# - log.or.adap,
                           mean = log.mean.support.adap - log.mean.adap,
                           sd = sqrt(log.sd.support.adap^2 + log.sd.adap^2 + mse),
                           log = T)
    
    
    log.or.comprehensive = log(o.comprehensive) - log(sim$p.comprehensive) + log(1-sim$p.comprehensive)
    log.or.core.adap = log(o.core.adap) - log(o.none)
    
    d.comprehensive = dnorm(log.or.comprehensive,# - log.or.core.adap,
                           mean = log.mean.comprehensive - log.mean.core.adap,
                           sd = sqrt(log.sd.comprehensive^2 + log.sd.core.adap^2 + mse),
                           log = T)
    
    log.or.support = log(o.support) - log(sim$p.support) + log(1-sim$p.support)
    
    d.support = dnorm(log.or.support,
                      mean = log.mean.support,
                      sd = sqrt(log.sd.support^2 + mse),
                      log = T)
    
    # log(prod(sqrt(n.core.support) * d.core.support + 
    #           #   n.support.adap * d.support.adap +
    #              sqrt(n.comprehensive) * d.comprehensive +
    #              sqrt(n.support) * d.support)) -
    #     log(n.core.support + n.comprehensive + n.support)
    
    d.core.support + d.support.adap + d.comprehensive + d.support
}

if (ADJUST.SUPPORT)
{
    adjusted.support.expansion.values = resample.adjust(n = KEEP,
                                                     service.type = 'support', 
                                                     run.sim = adjust.support.run.sim,
                                                     likelihood = adjust.support.likelihood.expansion,
                                                     expansion = T)
    
    adjusted.support.nonexpansion.values = resample.adjust(n = KEEP,
                                                        service.type = 'support', 
                                                        run.sim = adjust.support.run.sim,
                                                        likelihood = adjust.support.likelihood.nonexpansion,
                                                        expansion = F)
    
    # adjusted.support.expansion.values = run.adjust.mcmc(service.type = 'support', 
    #                                                  run.sim = adjust.support.run.sim,
    #                                                  likelihood = adjust.support.likelihood.expansion,
    #                                                  expansion = T)
    # 
    # adjusted.support.nonexpansion.values = run.adjust.mcmc(service.type = 'support', 
    #                                                     run.sim = adjust.support.run.sim,
    #                                                     likelihood = adjust.support.likelihood.nonexpansion,
    #                                                     expansion = F)
}


##--------------------------------------##
##-- COMBINE ADJUSTED VALUES AND SAVE --##
##--------------------------------------##

if (ADJUST.ADAP && ADJUST.OAHS && ADJUST.SUPPORT)
{
    adjusted.RW.effect.values = rbind(
        sort(adjusted.adap.expansion.values)[order(RW.effect.values[1,])],
        sort(adjusted.oahs.expansion.values)[order(RW.effect.values[2,])],
        sort(adjusted.support.expansion.values)[order(RW.effect.values[3,])],
        sort(adjusted.adap.nonexpansion.values)[order(RW.effect.values[4,])],
        sort(adjusted.oahs.nonexpansion.values)[order(RW.effect.values[5,])],
        sort(adjusted.support.nonexpansion.values)[order(RW.effect.values[6,])]
    )
    dimnames(adjusted.RW.effect.values) = dimnames(RW.effect.values)
    
    
    # save
    save(adjusted.RW.effect.values,
         file = '../jheem_analyses/applications/ryan_white/adjusted.RW.effect.values.Rdata')
    
    # examine
    
    rowMeans(adjusted.RW.effect.values)
    cbind(rowMeans(adjusted.RW.effect.values),
          t(apply(adjusted.RW.effect.values, 1, quantile, probs=c(.25, .75))))
    
    qplot(adjusted.RW.effect.values[1,])
    qplot(adjusted.RW.effect.values[2,])
    qplot(adjusted.RW.effect.values[3,])
}

if (1==2)
{
    df = rbind(
        data.frame(value = RW.effect.values[1,],
                   src = 'survey'),
        data.frame(value = adjusted.adap.expansion.values,
                   src = 'erly')
    )
    
    ggplot(df) + geom_histogram(aes(value, fill=src, position = 'dodge2'))
    
    c(mean(RW.effect.values[1,]), quantile(RW.effect.values[1,], probs=c(.25, .75)))
    c(mean(adjusted.adap.expansion.values), quantile(adjusted.adap.expansion.values, probs=c(.25, .75)))
    
    c(mean(RW.effect.values[2,]), quantile(RW.effect.values[2,], probs=c(.25, .75)))
    c(mean(adjusted.oahs.expansion.values), quantile(adjusted.oahs.expansion.values, probs=c(.25, .75)))
    
    c(mean(RW.effect.values[3,]), quantile(RW.effect.values[3,], probs=c(.25, .75)))
    c(mean(adjusted.support.expansion.values), quantile(adjusted.support.expansion.values, probs=c(.25, .75)))
    
    
    c(mean(RW.effect.values[4,]), quantile(RW.effect.values[4,], probs=c(.25, .75)))
    c(mean(adjusted.adap.nonexpansion.values), quantile(adjusted.adap.nonexpansion.values, probs=c(.25, .75)))
    
    c(mean(RW.effect.values[5,]), quantile(RW.effect.values[5,], probs=c(.25, .75)))
    c(mean(adjusted.oahs.nonexpansion.values), quantile(adjusted.oahs.nonexpansion.values, probs=c(.25, .75)))
    
    c(mean(RW.effect.values[6,]), quantile(RW.effect.values[6,], probs=c(.25, .75)))
    c(mean(adjusted.support.nonexpansion.values), quantile(adjusted.support.nonexpansion.values, probs=c(.25, .75)))
    
    
    
    c(mean(adjusted.adap.values), quantile(adjusted.adap.values, probs=c(.25, .75)))
    c(mean(adjusted.oahs.values), quantile(adjusted.oahs.values, probs=c(.25, .75)))
    c(mean(adjusted.support.values), quantile(adjusted.support.values, probs=c(.25, .75)))
}