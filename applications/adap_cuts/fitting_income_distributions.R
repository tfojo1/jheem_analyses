
# helper
parse.income.brackets <- function(brackets)
{
    orig.brackets = brackets
    upper.is.infinite = grepl("[>+]", brackets)
    brackets = gsub("[%>+]", "", brackets)
    split = strsplit(brackets, '-')
    
    lowers = as.numeric(sapply(split, function(x){x[1]}))
    lowers[!upper.is.infinite] = lowers[!upper.is.infinite]-1
    lowers[lowers<0] = 0
    uppers = as.numeric(sapply(split, function(x){x[2]}))
    uppers[upper.is.infinite] = Inf
    
    rv = list(lower=lowers, upper=uppers)
    
    
    if (any(is.na(rv$lower)) || any(is.na(rv$upper)))
        stop(paste0("Could not parse income brackets: ",
                    paste0("'", orig.brackets, "'", collapse = ', ')))
    
    rv
}

# for testing
if (1==2)
{
    brackets = c("1-100%","101-138%",">400%", "400+%")
    parse.income.brackets(brackets)
}

##--------------------------------##
##-- FIT DISTRIBUTION to INCOME --##
##--------------------------------##

fit.tobit.normal.distribution.to.quantiles <-  function(p,
                                                        upper.limit,
                                                        print.summary = T)
{
    parsed.bounds = parse.income.brackets(names(p))
    parsed.bounds$lower[parsed.bounds$lower==0] = -Inf
    
    objective.fn = function(params){
        
        mean = params[1]
        sd = params[2]
        
        upper.p = pnorm(q=upper.limit, mean, sd)
        
        sim.p = (pmin(pnorm(q=parsed.bounds$upper, mean, sd), upper.p) -
                     pmin(pnorm(q=parsed.bounds$lower, mean, sd), upper.p)) /
            upper.p
        
        score = sum( (p-sim.p)^2 )
    }
    
    optim.result = optim(par = c(upper.limit/2, upper.limit/4),
                         fn = objective.fn)
    
    mean = optim.result$par[1]
    sd = optim.result$par[2]
    
    upper.p = pnorm(q=upper.limit, mean, sd)
    
    sim.p = (pmin(pnorm(q=parsed.bounds$upper, mean, sd), upper.p) -
                 pmin(pnorm(q=parsed.bounds$lower, mean, sd), upper.p)) /
        upper.p
    
    if (print.summary)
    {
        print(
            round(rbind(
                '<0' = c(obs.p = NA, sim.p=pnorm(0, mean, sd) / pnorm(upper.limit, mean, sd)),
                cbind(
                    obs.p = p,
                    sim.p = sim.p
                )), 4)
        )
    }
    
    list(mean = mean, sd = sd, upper.limit = upper.limit, type='tobit.normal')
}

fit.tobit.normal.distribution.with.ssi.to.quantiles <-  function(p,
                                                                 upper.limit,
                                                                 ssi.benefit = 995*12 / 15960,
                                                                 ssi.breakeven = 2073*12 / 15960,
                                                                 p.ssi.if.income.eligible = 587 / (1106+449),
                                                                 print.summary = T)
{
    parsed.bounds = parse.income.brackets(names(p))
    parsed.bounds$lower[parsed.bounds$lower==0] = -Inf
    
    objective.fn = function(params){
        
        mean = params[1]
        sd = params[2]
        
        
        all.bounds = c(parsed.bounds$upper, parsed.bounds$lower, upper.limit)
        all.bounds.before.ssi.benefit = (all.bounds - ssi.benefit) * ssi.breakeven  / (ssi.breakeven - ssi.benefit)
        
        p.for.bounds = pnorm(all.bounds, mean, sd)
        p.for.bounds[all.bounds<=ssi.benefit] = (1-p.ssi.if.income.eligible) * p.for.bounds[all.bounds<=ssi.benefit]
        p.for.bounds[all.bounds>ssi.benefit & all.bounds<ssi.breakeven] = (1-p.ssi.if.income.eligible) * p.for.bounds[all.bounds>ssi.benefit & all.bounds<ssi.breakeven] +
            p.ssi.if.income.eligible * pnorm(all.bounds.before.ssi.benefit[all.bounds>ssi.benefit & all.bounds<ssi.breakeven], mean, sd)
        
        upper.p = p.for.bounds[length(p.for.bounds)]
        
        p.for.bounds = pmin(p.for.bounds, upper.p) / upper.p   
        
        sim.p = p.for.bounds[1:length(parsed.bounds$upper)] -
            p.for.bounds[length(parsed.bounds$upper) + 1:length(parsed.bounds$lower)]

        score = sum( (p-sim.p)^2 )
    }
    
    optim.result = optim(par = c(upper.limit/2, upper.limit/4),
                         fn = objective.fn)
    
    mean = optim.result$par[1]
    sd = optim.result$par[2]
    
    upper.p = pnorm(q=upper.limit, mean, sd)
    
    sim.p = (pmin(pnorm(q=parsed.bounds$upper, mean, sd), upper.p) -
                 pmin(pnorm(q=parsed.bounds$lower, mean, sd), upper.p)) /
        upper.p
    
    if (print.summary)
    {
        print(
            round(rbind(
                '<0' = c(obs.p = NA, sim.p=pnorm(0, mean, sd) / pnorm(upper.limit, mean, sd)),
                cbind(
                    obs.p = p,
                    sim.p = sim.p
                )), 4)
        )
    }
    
    list(mean = mean, sd = sd, upper.limit = upper.limit,
         ssi.benefit = ssi.benefit,
         ssi.breakeven = ssi.breakeven,
         p.ssi.if.income.eligible = p.ssi.if.income.eligible,
         type='tobit.normal.with.ssi')
}



fit.zero.inflated.lognormal.distribution.to.quantiles <-  function(p,
                                                                   upper.limit,
                                                                   print.summary = T)
{
    parsed.bounds = parse.income.brackets(names(p))
    
    objective.fn = function(params){
        
        log.mean = params[1]
        log.sd = exp(params[2])
        p.zero = 1 / (1 + exp(-params[3]))
        
        upper.p = plnorm(q=upper.limit, log.mean, log.sd)
        
        sim.p = (pmin(plnorm(q=parsed.bounds$upper, log.mean, log.sd), upper.p) -
                     pmin(plnorm(q=parsed.bounds$lower, log.mean, log.sd), upper.p)) *
            (1 - p.zero) / upper.p
        
        sim.p[parsed.bounds$lower==0] = sim.p[parsed.bounds$lower==0] + p.zero
        
        score = sum( (p-sim.p)^2 )
    }
    
    optim.result = optim(par = c(log(upper.limit*.4), log(0.5), log(0.05)-log(0.95)),
                         fn = objective.fn)
    
    log.mean = optim.result$par[1]
    log.sd = exp(optim.result$par[2])
    p.zero = 1 / (1 + exp(-optim.result$par[3]))
    
    
    if (print.summary)
    {
        upper.p = plnorm(q=upper.limit, log.mean, log.sd)
        
        sim.p = (pmin(plnorm(q=parsed.bounds$upper, log.mean, log.sd), upper.p) -
                     pmin(plnorm(q=parsed.bounds$lower, log.mean, log.sd), upper.p)) *
            (1 - p.zero) / upper.p
        
        sim.p[parsed.bounds$lower==0] = sim.p[parsed.bounds$lower==0] + p.zero
        
        print(
            round(rbind(
                'p0' = c(obs.p = NA, sim.p=p.zero),
                cbind(
                    obs.p = p,
                    sim.p = sim.p
                )),4)
        )
    }
    
    list(log.mean = log.mean,
         log.sd = log.sd,
         p.zero = p.zero,
         upper.limit = upper.limit,
         type = 'zero.inflated.lognormal')
}




##-------------------------------------------------------##
##-- FIT LOGISTIC P CONDITIONAL ON INCOME DISTRIBUTION --##
##-------------------------------------------------------##


fit.logistic.parameters.to.p.distribution <- function(dist.parameters, p, vary.max = T, print.summary=T)
{
    if (!is.null(names(dist.parameters)) && any(names(dist.parameters)[1]=='type'))
        dist.parameters = list(dist.parameters)
    
    if (!is.list(p))
        p = list(p)
    
    parsed.bounds = lapply(p, function(one.p){
        parse.income.brackets(names(one.p))})
    
    
    objective.fn <- function(par)
    {
        logistic.params = list(
            midpoint = par[1],
            slope = exp(par[2])
        )
        if (vary.max)
            logistic.params$max = 1 / (1 + exp(-par[4]))
        else
            logistic.params$max = 1
        logistic.params$min = logistic.params$max / (1 + exp(-par[3]))
        
        
        score.per.elem = sapply(1:length(dist.parameters), function(i){
            
            sim.p = get.cumulative.logistic.times.distribution.p(dist.parameters[[i]],
                                                                 logistic.params,
                                                                 lower = parsed.bounds[[i]]$lower,
                                                                 upper = parsed.bounds[[i]]$upper)
            
            sum( (sim.p - p[[i]])^2 )
        })
        
        sum(score.per.elem)
    }
    
    init.params = c(dist.parameters[[1]]$upper.limit/2, 
                    log(200/dist.parameters[[1]]$upper.limit), 
                    log(0.2) - log(0.8))
    
    if (vary.max)
        init.params = c(init.params, log(0.95)-log(0.05))
    
    optim.result = optim(par = init.params,
                         fn = objective.fn)
    
    par = optim.result$par
    logistic.params = list(
        midpoint = par[1],
        slope = exp(par[2])
    )
    if (vary.max)
        logistic.params$max = 1 / (1 + exp(-par[4]))
    else
        logistic.params$max = 1
    logistic.params$min = logistic.params$max / (1 + exp(-par[3]))
    
    
    if (print.summary)
    {
        summ = NULL
        for (i in 1:length(dist.parameters))
        {   
            sim.p = get.cumulative.logistic.times.distribution.p(dist.parameters[[i]],
                                                                 logistic.params,
                                                                 lower = parsed.bounds[[i]]$lower,
                                                                 upper = parsed.bounds[[i]]$upper)
            
            summ = rbind(summ, 
                         cbind(obs.p = p[[i]],
                               sim.p = sim.p))
        }
        
        print(paste0(optim.result$counts[1], " optim iterations"))
        print(summ)
    }
    
    logistic.params$type = 'logistic.given.income'
    logistic.params
}

get.cumulative.logistic.times.distribution.p <- function(dist.parameters,
                                                         logistic.parameters,
                                                         lower, upper)
{
    sapply(1:length(lower), function(i){
        
        if (lower[i]>dist.parameters$upper.limit)
            return (0)
        
        sim.x = (lower[i]-1):min(upper[i], dist.parameters$upper.limit)

        dist.cum.p = get.distribution.p(sim.x, dist.parameters)
        dist.p = dist.cum.p[-1] - dist.cum.p[-length(dist.cum.p)]
        
        logistic.p = logistic.parameters$min + (logistic.parameters$max-logistic.parameters$min) / (1 + exp(logistic.parameters$slope * (sim.x[-1]-logistic.parameters$midpoint)))
        
        sum(logistic.p * dist.p) / sum(dist.p)
    })
}

get.distribution.p <- function(x, dist.parameters)
{
    if (!is.list(dist.parameters) || is.null(dist.parameters$type))
        stop("'dist.parameters' must be the result of a call to a function fit.<x>.distribution.to.quantiles")
    
    if (dist.parameters$type=='tobit.normal')
        get.tobit.normal.distribution.p(x, dist.parameters)
    else if (dist.parameters$type=='tobit.normal.with.ssi')
        get.tobit.normal.distribution.with.ssi.p(x, dist.parameters)
    else if (dist.parameters$type=='zero.inflated.lognormal')
        get.zero.inflated.lognormal.distribution.p(x, dist.parameters)
    else
        stop("'dist.parameters' must have type 'tobit.normal', 'tobit.normal.with.ssi', or 'zero.inflated.lognormal")
}

get.tobit.normal.distribution.p <- function(x, tobit.normal.parameters)
{
    p = pnorm(pmax(0, x), tobit.normal.parameters$mean, tobit.normal.parameters$sd) / 
        pnorm(tobit.normal.parameters$upper.limit, tobit.normal.parameters$mean, tobit.normal.parameters$sd)
    
    p[x < 0] = 0
    
    p
}

get.tobit.normal.distribution.with.ssi.p <- function(x, tobit.normal.parameters)
{
    x.plus = c(x, tobit.normal.parameters$upper.limit)
    
    p = pnorm(x.plus, tobit.normal.parameters$mean, tobit.normal.parameters$sd)
    p[x.plus<=tobit.normal.parameters$ssi.benefit] = (1-tobit.normal.parameters$p.ssi.if.income.eligible) * p[x.plus<=tobit.normal.parameters$ssi.benefit]
    p[x.plus>tobit.normal.parameters$ssi.benefit & x.plus<tobit.normal.parameters$ssi.breakeven] = 
        (1-tobit.normal.parameters$p.ssi.if.income.eligible) * p[x.plus>tobit.normal.parameters$ssi.benefit & x.plus<tobit.normal.parameters$ssi.breakeven] +
        tobit.normal.parameters$p.ssi.if.income.eligible * pnorm(x.plus[x.plus>tobit.normal.parameters$ssi.benefit & x.plus<tobit.normal.parameters$ssi.breakeven], tobit.normal.parameters$mean, tobit.normal.parameters$sd)
    
    upper.p = p[length(p)]
    
    p = pmin(upper.p, p[-length(p)]) / upper.p
    
    p[x < 0] = 0
    
    p
}

get.zero.inflated.lognormal.distribution.p <- function(x, lognormal.parameters)
{
    p = plnorm(x, lognormal.parameters$log.mean, lognormal.parameters$log.sd) *
        lognormal.parameters$p.zero /
        plnorm(lognormal.parameters$upper.limit, lognormal.parameters$log.mean, lognormal.parameters$log.sd)

    p[x==0] = p[x==0] + lognormal.parameters$p.zero
    
    p
}
