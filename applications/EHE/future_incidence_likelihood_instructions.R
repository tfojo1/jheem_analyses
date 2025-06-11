

get.new.diagnoses.fold.change.distribution <- function(year.span,
                                                       dimensions,
                                                       min.year=2008,
                                                       max.year=2019)
{
    possible.dimensions = setdiff(names(dimnames(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk)),
                                  c('year','location'))
    
    data.name = paste0(c('year', 'location', intersect(possible.dimensions, dimensions)), collapse='__')
    
    data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc[[data.name]]
    
    last.years = (min.year:max.year)
    first.years = last.years - year.span
    mask = first.years >= min.year
    
    last.years = last.years[mask]
    first.years = first.years[mask]
    
    fold.change = array.access(data, year=as.character(last.years)) / array.access(data, year=as.character(first.years))
    
    z = fold.change[!is.na(fold.change) & !is.infinite(fold.change) & fold.change>0] 
    
    print(ggplot2::qplot(log(z[z<quantile(z,.99)])))
    
    lt.thresholds = c(1,1.5,2,2.5,3)
    smooth.p.gt = 1-plnorm(lt.thresholds,
                    meanlog = mean(log(z)),
                    sdlog = sd(log(z)))
    names(smooth.p.gt) = as.character(lt.thresholds)
    
    obs.p.gt = sapply(lt.thresholds, function(thresh){
        mean(z>thresh)
    })
    names(obs.p.gt) = as.character(lt.thresholds)
    
    list(
        dimensions = dimensions,
        meanlog = mean(log(z)),
        sdlog = sd(log(z)),
        smooth.p.gt = smooth.p.gt,
        obs.p.gt = obs.p.gt
    )
}

get.new.diagnoses.smoothed.fold.change.distribution <- function(year.span,
                                                                dimensions,
                                                                min.year=2008,
                                                                max.year=2019)
{
    possible.dimensions = setdiff(names(dimnames(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk)),
                                  c('year','location'))
    
    data.name = paste0(c('year', 'location', intersect(possible.dimensions, dimensions)), collapse='__')
    
    data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc[[data.name]]
    
    last.years = (min.year:max.year)
    first.years = last.years - year.span
    mask = first.years >= min.year
    
    last.years = last.years[mask]
    first.years = first.years[mask]
    
    fold.change = sapply(1:length(last.years), function(year.index){
        years = first.years[year.index]:last.years[year.index]
        years.to.pull = as.character(years)
        apply(data, setdiff(names(dim(data)), 'year'), function(values){
            
            values = values[years.to.pull]
            values[values==0] = NA
            if (sum(!is.na(values))<2)
                NA
            else if (all(values<20, na.rm=T))
                NA
            else
            {
                fit = lm(values ~ years)
                
                smoothed.1 = fit$coefficients[1] + fit$coefficients[2] * first.years[year.index]
                smoothed.2 = fit$coefficients[1] + fit$coefficients[2] * last.years[year.index]
                
                smoothed.2 / smoothed.1
            }
        })
    })
    
    
    z = fold.change[!is.na(fold.change) & !is.infinite(fold.change) & fold.change>0] 
    
    print(ggplot2::qplot(log(z[z<quantile(z,.995) & z>quantile(z,.005)])))
    
    lt.thresholds = c(1,1.5,2,2.5,3)
    smooth.p.gt = 1-plnorm(lt.thresholds,
                           meanlog = mean(log(z)),
                           sdlog = sd(log(z)))
    names(smooth.p.gt) = as.character(lt.thresholds)
    
    obs.p.gt = sapply(lt.thresholds, function(thresh){
        mean(z>thresh)
    })
    names(obs.p.gt) = as.character(lt.thresholds)
    
    list(
        dimensions = dimensions,
        meanlog = mean(log(z)),
        sdlog = sd(log(z)),
        smooth.p.gt = smooth.p.gt,
        obs.p.gt = obs.p.gt
    )
}

get.new.diagnoses.smoothed.fold.change.pairs.distribution <- function(year.span,
                                                             dimensions,
                                                             pivot.lag,
                                                             min.year=2008,
                                                             max.year=2019)
{
    possible.dimensions = setdiff(names(dimnames(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk)),
                                  c('year','location'))
    
    data.name = paste0(c('year', 'location', intersect(possible.dimensions, dimensions)), collapse='__')
    
    data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc[[data.name]]
    
    last.years = (min.year:max.year)
    pivot2.years = last.years - year.span
    pivot1.years = pivot2.years - pivot.lag
    first.years = pivot1.years - year.span
    mask = first.years >= min.year
    
    last.years = last.years[mask]
    pivot1.years = pivot1.years[mask]
    pivot2.years = pivot2.years[mask]
    first.years = first.years[mask]
    
    fold.change.1 = sapply(1:length(last.years), function(year.index){
        
        y1 = first.years[year.index]
        y2 = pivot1.years[year.index]
        
        years = y1:y2
        years.to.pull = as.character(years)
        
        apply(data, setdiff(names(dim(data)), 'year'), function(values){
            
            values = values[years.to.pull]
            values[values==0] = NA
            if (sum(!is.na(values))<2)
                NA
            else if (all(values<20, na.rm=T))
                NA
            else
            {
                fit = lm(values ~ years)
                
                smoothed.1 = fit$coefficients[1] + fit$coefficients[2] * y1
                smoothed.2 = fit$coefficients[1] + fit$coefficients[2] * y2
                
                smoothed.2 / smoothed.1
            }
        })
    })
    
    fold.change.2 = sapply(1:length(last.years), function(year.index){
        
        y1 = pivot2.years[year.index]
        y2 = last.years[year.index]
        
        years = y1:y2
        years.to.pull = as.character(years)
        
        apply(data, setdiff(names(dim(data)), 'year'), function(values){
            
            values = values[years.to.pull]
            values[values==0] = NA
            if (sum(!is.na(values))<2)
                NA
            else if (all(values<20, na.rm=T))
                NA
            else
            {
                fit = lm(values ~ years)
                
                smoothed.1 = fit$coefficients[1] + fit$coefficients[2] * y1
                smoothed.2 = fit$coefficients[1] + fit$coefficients[2] * y2
                
                smoothed.2 / smoothed.1
            }
        })
    })
    
    mask = !is.na(fold.change.1) & !is.infinite(fold.change.1) & fold.change.1>0 &
        !is.na(fold.change.2) & !is.infinite(fold.change.2) & fold.change.2>0
    
    z1 = fold.change.1[mask]
    z2 = fold.change.2[mask]
    
    z = z2/z1    
    
    print(ggplot2::qplot(log(z[z<quantile(z,.995) & z>quantile(z,.005)])))
    
    lt.thresholds = c(1,1.5,1.75,2,2.5,3)
    smooth.p.gt = 1-plnorm(lt.thresholds,
                           meanlog = mean(log(z)),
                           sdlog = sd(log(z)))
    names(smooth.p.gt) = as.character(lt.thresholds)
    
    obs.p.gt = sapply(lt.thresholds, function(thresh){
        mean(z>thresh)
    })
    names(obs.p.gt) = as.character(lt.thresholds)
    
    list(
        dimensions = dimensions,
        meanlog = mean(log(z)),
        sdlog = sd(log(z)),
        smooth.p.gt = smooth.p.gt,
        obs.p.gt = obs.p.gt
    )
    
}

get.new.diagnoses.fold.change.pairs.distribution <- function(year.span,
                                                             dimensions,
                                                             pivot.lag,
                                                             min.year=2008,
                                                             max.year=2019)
{
    possible.dimensions = setdiff(names(dimnames(SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc$year__location__age__race__sex__risk)),
                                  c('year','location'))
    
    data.name = paste0(c('year', 'location', intersect(possible.dimensions, dimensions)), collapse='__')
    
    data = SURVEILLANCE.MANAGER$data$diagnoses$estimate$cdc.hiv$cdc[[data.name]]
    
    last.years = (min.year:max.year)
    pivot2.years = last.years - year.span
    pivot1.years = pivot2.years - pivot.lag
    first.years = pivot1.years - year.span
    mask = first.years >= min.year
    
    last.years = last.years[mask]
    pivot1.years = pivot1.years[mask]
    pivot2.years = pivot2.years[mask]
    first.years = first.years[mask]
    
    fold.change.1 = array.access(data, year=as.character(pivot1.years)) / array.access(data, year=as.character(first.years))
    fold.change.2 = array.access(data, year=as.character(last.years)) / array.access(data, year=as.character(pivot2.years))
    
    mask = !is.na(fold.change.1) & !is.infinite(fold.change.1) & fold.change.1>0 &
        !is.na(fold.change.2) & !is.infinite(fold.change.2) & fold.change.2>0
    
    z1 = fold.change.1[mask]
    z2 = fold.change.2[mask]
    
    z = z2/z1    
    
    print(ggplot2::qplot(log(z[z<quantile(z,.995) & z>quantile(z,.005)])))
    
    lt.thresholds = c(1,1.5,1.75,2,2.5,3)
    smooth.p.gt = 1-plnorm(lt.thresholds,
                           meanlog = mean(log(z)),
                           sdlog = sd(log(z)))
    names(smooth.p.gt) = as.character(lt.thresholds)
    
    obs.p.gt = sapply(lt.thresholds, function(thresh){
        mean(z>thresh)
    })
    names(obs.p.gt) = as.character(lt.thresholds)
    
    list(
        dimensions = dimensions,
        meanlog = mean(log(z)),
        sdlog = sd(log(z)),
        smooth.p.gt = smooth.p.gt,
        obs.p.gt = obs.p.gt
    )
    
}


get.pivot.likelihood <- function(sim, 
                                 pivot.years,
                                 year.span,
                                 dimensions,
                                 pivot.lag,
                                 outcomes = c('new','incidence'))
{   
    pivot1.years = pivot.years - pivot.lag
    pivot2.years = pivot.years
    
    pre.pivot.years = pivot1.years - year.span
    post.pivot.years = pivot2.years + year.span
    
    all.years = union(union(pre.pivot.years, pivot1.years),
                      union(pivot2.years, post.pivot.years))
    
    data = sim$get(outcomes=outcomes,
                   year = as.character(all.years),
                   keep.dimensions = c('year',dimensions))
    
    dim.names = dimnames(data)
    pre.pivot.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pre.pivot.years)))
    pivot1.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pivot1.years)))
    pivot2.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pivot2.years)))
    post.pivot.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(post.pivot.years)))
    
    pre.fold.change = data[pivot1.indices] / data[pre.pivot.indices]
    post.fold.change = data[post.pivot.indices] / data[pivot2.indices]
    
    pre.post.fold.change = post.fold.change / pre.fold.change
    
    #for debugging
    fold.change.dim.names = dim.names
    fold.change.dim.names$year = as.character(pivot.years)
    
    dim(pre.post.fold.change) = dim(post.fold.change) = dim(pre.fold.change) = sapply(fold.change.dim.names, length)
    dimnames(pre.post.fold.change) = dimnames(post.fold.change) = dimnames(pre.fold.change) = fold.change.dim.names
    
    pre.post.fold.change
}

create.future.change.likelihood.instructions <- function(outcomes,
                                                         end.years,
                                                         year.span,
                                                         dimensions,
                                                         log.ratio.mean,
                                                         log.ratio.sd,
                                                         ratio.threshold,
                                                         name,
                                                         debug=F)
{
    get.data.function = function(version, location)
    {
        #-- Work out years --#
        start.years = end.years - year.span

        all.years = union(start.years, end.years)
        
        #-- Optimized get instructions --#
        sim.metadata = get.simulation.metadata(version=version, location=location)
        optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
            outcomes = outcomes, 
            dimension.values = list(year=all.years),
            keep.dimensions = c('year',dimensions)
        )
        
        #-- Indices --#
        dim.names = sim.metadata$get.dim.names(outcomes = outcomes, 
                                               dimension.values = list(year=all.years),
                                               keep.dimensions = c('year',dimensions))
        
        start.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(start.years)))
        end.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(end.years)))

        #-- Statistical Quantities --#
        
        p.lte.threshold = plnorm(ratio.threshold, meanlog = log.ratio.mean, sdlog = log.ratio.sd)
        log.d.lte.threshold = log(p.lte.threshold / ratio.threshold)
        
        #-- For debugging --#
        ratio.dim.names = dim.names
        ratio.dim.names$year = end.years
        
        #-- Package it up --#
        list(
            optimized.get.instr = optimized.get.instr,
            
            end.indices = end.indices,
            start.indices = start.indices,
            
            ratio.threshold = ratio.threshold,
            log.ratio.mean = log.ratio.mean,
            log.ratio.sd = log.ratio.sd,
            log.d.lte.threshold = log.d.lte.threshold,
            
            ratio.dim.names = ratio.dim.names,
            debug = debug
        )
    }
    
    compute.function = function(sim, data, log=T)
    {
        #-- Get the data and calculate fold change ratio --#
        sim.values = sim$optimized.get(data$optimized.get.instr)
        
        fold.change = sim.values[data$end.indices] / sim.values[data$start.indices]
        fold.change[(sim.values[data$end.indices]==0 & sim.values[data$start.indices]==0) | 
                        (is.infinite(sim.values[data$end.indices] & is.infinite(sim.values[data$start.indices])))] = 1
        fold.change[is.infinite(fold.change)] = 100

        #-- Plug in to stats --#
        
        mask.fold.change.lte.threshold = fold.change <= data$ratio.threshold
        
        d.gt.threshold = stats::dlnorm(fold.change[!mask.fold.change.lte.threshold], 
                                       meanlog = data$log.ratio.mean,
                                       sdlog = data$log.ratio.sd,
                                       log = T)
        
        d.lte.threshold = data$log.d.lte.threshold * sum(mask.fold.change.lte.threshold)
        
        rv = sum(d.gt.threshold) + d.lte.threshold
        
        if (data$debug)
        {
            dim(fold.change) = dim(mask.fold.change.lte.threshold) = sapply(data$ratio.dim.names, length)
            dimnames(fold.change) = dimnames(mask.fold.change.lte.threshold) = data$ratio.dim.names
            
            print("Ratio-ratios exceeding threshold: ")
            print(apply(!mask.fold.change.lte.threshold, intersect(c('sex','race','outcome'), names(data$ratio.dim.names)), sum))
            print(apply(!mask.fold.change.lte.threshold, intersect(c('year','outcome'), names(data$ratio.dim.names)), sum))
        }
        
        if (!log)
            exp(rv)
        else
            rv
        
    }
    environment(compute.function) = baseenv()
    
    create.custom.likelihood.instructions(
        name = name,
        compute.function = compute.function,
        get.data.function = get.data.function
    )
}

create.future.change.pivot.likelihood.instructions <- function(outcomes,
                                                               pivot.years,
                                                               pivot.lag,
                                                               year.span,
                                                               dimensions,
                                                               log.ratio.ratio.mean,
                                                               log.ratio.ratio.sd,
                                                               ratio.ratio.threshold,
                                                               name,
                                                               debug=F)
{
    get.data.function = function(version, location)
    {
        #-- Work out years --#
        pivot1.years = pivot.years - pivot.lag
        pivot2.years = pivot.years
        
        pre.pivot.years = pivot1.years - year.span
        post.pivot.years = pivot2.years + year.span
        
        all.years = union(union(pre.pivot.years, pivot1.years),
                          union(pivot2.years, post.pivot.years))
        
        #-- Optimized get instructions --#
        sim.metadata = get.simulation.metadata(version=version, location=location)
        optimized.get.instr = sim.metadata$prepare.optimized.get.instructions(
            outcomes = outcomes, 
            dimension.values = list(year=all.years),
            keep.dimensions = c('year',dimensions)
        )
        
        #-- Indices --#
        dim.names = sim.metadata$get.dim.names(outcomes = outcomes, 
                                               dimension.values = list(year=all.years),
                                               keep.dimensions = c('year',dimensions))
        
        pre.pivot.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pre.pivot.years)))
        pivot1.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pivot1.years)))
        pivot2.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(pivot2.years)))
        post.pivot.indices = get.array.access.indices(dim.names, dimension.values=list(year=as.character(post.pivot.years)))
        
        #-- Statistical Quantities --#
        
        p.lte.threshold = plnorm(ratio.ratio.threshold, meanlog = log.ratio.ratio.mean, sdlog = log.ratio.ratio.sd)
        log.d.lte.threshold = log(p.lte.threshold / ratio.ratio.threshold)
        
        #-- For debugging --#
        ratio.ratio.dim.names = dim.names
        ratio.ratio.dim.names$year = pivot.years
        
        #-- Package it up --#
        list(
            optimized.get.instr = optimized.get.instr,
            
            pre.pivot.indices = pre.pivot.indices,
            pivot1.indices = pivot1.indices,
            pivot2.indices = pivot2.indices,
            post.pivot.indices = post.pivot.indices,
            
            ratio.ratio.threshold = ratio.ratio.threshold,
            log.ratio.ratio.mean = log.ratio.ratio.mean,
            log.ratio.ratio.sd = log.ratio.ratio.sd,
            log.d.lte.threshold = log.d.lte.threshold,
            
            ratio.ratio.dim.names = ratio.ratio.dim.names,
            debug = debug
        )
    }
    
    compute.function = function(sim, data, log=T)
    {
        #-- Get the data and calculate fold change ratio --#
        sim.values = sim$optimized.get(data$optimized.get.instr)
        
        pre.fold.change = sim.values[data$pivot1.indices] / sim.values[data$pre.pivot.indices]
        pre.fold.change[ sim.values[data$pivot1.indices]==0 & sim.values[data$pre.pivot.indices]==0 ] = 1
        
        post.fold.change = sim.values[data$post.pivot.indices] / sim.values[data$pivot2.indices]
        post.fold.change[ sim.values[data$post.pivot.indices]==0 & sim.values[data$pivot2.indices]==0 ] = 1
        
        pre.post.fold.change.ratio = post.fold.change / pre.fold.change
        pre.post.fold.change.ratio[(post.fold.change==0 & pre.fold.change==0) | (is.infinite(post.fold.change) & is.infinite(pre.fold.change))] = 1
        pre.post.fold.change.ratio[is.infinite(pre.post.fold.change.ratio)] = 100
        
        #-- Plug in to stats --#
        
        mask.fold.change.ratio.lte.threshold = pre.post.fold.change.ratio <= data$ratio.ratio.threshold
        
        d.gt.threshold = stats::dlnorm(pre.post.fold.change.ratio[!mask.fold.change.ratio.lte.threshold], 
                                        meanlog = data$log.ratio.ratio.mean,
                                        sdlog = data$log.ratio.ratio.sd,
                                        log = T)
        
        d.lte.threshold = data$log.d.lte.threshold * sum(mask.fold.change.ratio.lte.threshold)
        
        rv = sum(d.gt.threshold) + d.lte.threshold
        
        if (data$debug)
        {
            dim(pre.post.fold.change.ratio) = dim(mask.fold.change.ratio.lte.threshold) = sapply(data$ratio.ratio.dim.names, length)
            dimnames(pre.post.fold.change.ratio) = dimnames(mask.fold.change.ratio.lte.threshold) = data$ratio.ratio.dim.names
            
            print("Ratio-ratios exceeding threshold: ")
            print(apply(!mask.fold.change.ratio.lte.threshold, intersect(c('sex','race','outcome'), names(data$ratio.ratio.dim.names)), sum))
        }
        
        if (!log)
            exp(rv)
        else
            rv
        
    }
    environment(compute.function) = baseenv()
    
    create.custom.likelihood.instructions(
        name = name,
        compute.function = compute.function,
        get.data.function = get.data.function
    )
}

if (1==2)
{
    get.new.diagnoses.smoothed.fold.change.pairs.distribution(5, c('race','sex','risk','age'), pivot.lag = 1)
    
    lik.instr = create.future.change.pivot.likelihood.instructions(outcomes = c('new','incidence'),
                                                                   pivot.years = 2026:2030,
                                                                   pivot.lag = 1,
                                                                   year.span = 5,
                                                                   dimensions = c('age','race','sex','risk'),
                                                                   log.ratio.ratio.mean = -0.02176556,
                                                                   log.ratio.ratio.sd = 0.4892191,
                                                                   ratio.ratio.threshold = 2,
                                                                   name = 'future.incidence.new.pivot.5y',
                                                                   debug=T)
    
    lik = lik.instr$instantiate.likelihood('ehe','AL')
    lik$compute(sim)
    
    
    simplot(sim, 'incidence', facet.by='sex', split.by='race')
}

if (1==2)
{
#    get.new.diagnoses.fold.change.distribution(10, c('age','race','sex','risk'))
    get.new.diagnoses.smoothed.fold.change.distribution(10, c('age','race','sex','risk'))
    
    lik.instr = create.future.change.likelihood.instructions(outcomes = c('new','incidence'),
                                                             end.years = 2026:2035,
                                                             year.span = 10,
                                                             dimensions = c('age','race','sex','risk'),
                                                             log.ratio.mean = -0.3306992,
                                                             log.ratio.sd = 0.7790756,
                                                             ratio.threshold = 2.5,
                                                             name = 'future.incidence.new.10y',
                                                             debug=T)
    lik = lik.instr$instantiate.likelihood('ehe','AL')
    lik$compute(sim)
    simplot(sim, 'incidence', facet.by='sex', split.by='race')
    
}

if (1==2)
{

get.pivot.likelihood(sim, 2026:2032, year.span=3, pivot.lag = 1, dimensions=c('sex','race'))[,'msm','black',,]
get.pivot.likelihood(sim, 2026:2030, year.span=4, pivot.lag = 1, dimensions=c('sex','race'))[,'msm','black',,]
get.pivot.likelihood(sim, 2026:2030, year.span=5, pivot.lag = 1, dimensions=c('sex','race'))[,'msm','black',,]
get.pivot.likelihood(sim, 2026:2030, year.span=5, pivot.lag = 1, dimensions=c('sex','race'))[,'msm','other',,]
get.pivot.likelihood(sim, 2026:2030, year.span=5, pivot.lag = 1, dimensions=c('sex','race','risk','age'))[,'msm','other',,,,]
apply(get.pivot.likelihood(sim, 2026:2030, year.span=5, pivot.lag = 1, dimensions=c('sex','race','risk','age'))>2, c('sex','race','outcome'), sum)
apply(get.pivot.likelihood(sim, 2026:2030, year.span=5, pivot.lag = 0, dimensions=c('sex','race','risk','age'))>2, c('sex','race','outcome'), sum)

get.new.diagnoses.fold.change.pairs.distribution(5, c('race','sex'), pivot.lag = 1)
get.new.diagnoses.smoothed.fold.change.pairs.distribution(5, c('race','sex'), pivot.lag = 1)

get.new.diagnoses.fold.change.pairs.distribution(5, c('race','sex','risk','age'), pivot.lag = 1)
get.new.diagnoses.smoothed.fold.change.pairs.distribution(5, c('race','sex','risk','age'), pivot.lag = 1)

get.new.diagnoses.smoothed.fold.change.pairs.distribution(5, c('race','sex','risk','age'), pivot.lag = 1)

simplot(sim, 'incidence')

get.new.diagnoses.smoothed.fold.change.pairs.distribution(5, c('race','sex'))

get.new.diagnoses.log.slope.distribution(10, c('age','race','sex','risk'))
get.new.diagnoses.log.slope.distribution(10, c('race','sex'))
get.new.diagnoses.log.slope.distribution(5, c('race','sex'))
get.new.diagnoses.log.slope.distribution(5, c('age','race','sex','risk'))
get.new.diagnoses.log.slope.distribution(5, character())

get.new.diagnosed.fold.change.pairs.distribution(5, character())
get.new.diagnosed.fold.change.pairs.distribution(3, character())
get.new.diagnosed.fold.change.pairs.distribution(3, c('race','sex'))


#get.new.diagnoses.fold.change.distribution(10, c('age','race','sex','risk'), min.year=2009)
get.new.diagnoses.fold.change.distribution(10, c('age','race','sex','risk'))
get.new.diagnoses.fold.change.distribution(10, c('race','sex'))
get.new.diagnoses.fold.change.distribution(10, character(), min.year=2009)


get.new.diagnoses.fold.change.distribution(5, c('age','race','sex','risk'))
get.new.diagnoses.fold.change.distribution(5, c('race','sex'))
get.new.diagnoses.fold.change.distribution(5, character(), min.year=2009)


get.new.diagnoses.fold.change.distribution(3, c('age','race','sex','risk'))
get.new.diagnoses.fold.change.distribution(3, c('race','sex'))
get.new.diagnoses.fold.change.distribution(3, character(), min.year=2009)

ch = sim$get(outcomes=c('new','incidence'), year=2025:2035, keep.dimensions=c('year','sex','race')) /
    sim$get(outcomes=c('new','incidence'), year=2020:2030, keep.dimensions=c('year','sex','race'))

ch[,'msm','black',,]


simplot(sim, 'incidence', facet.by='sex', split.by='race')
simplot(sim, 'new', facet.by='risk', split.by='race')

inc = sim$get(outcomes=c('new','incidence'), year=2030:2040, keep.dimensions=c('year','sex','race'))

inc[,'msm','black',,]

}