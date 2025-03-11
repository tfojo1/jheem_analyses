source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')

RW.DIFFERENT.YEARS.CORRELATION = 0.1
RW.CALIBRATE.FROM.YEAR = 2017

rw.non.adap.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "non.adap.clients", 
  outcome.for.sim = "non.adap.clients",
  dimensions = c("age","sex","race"),
  levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
  from.year = RW.CALIBRATE.FROM.YEAR,
  correlation.different.years = RW.DIFFERENT.YEARS.CORRELATION, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)

# for testing/debugging
rw.total.non.adap.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "non.adap.clients", 
  outcome.for.sim = "non.adap.clients",
  dimensions = c("age","sex","race"),
  levels.of.stratification = c(0), # 0 = totals, 1 = 1-way stratification
  from.year = RW.CALIBRATE.FROM.YEAR,
  correlation.different.years = RW.DIFFERENT.YEARS.CORRELATION, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)


rw.non.adap.sex.risk.proportion.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'non.adap.sex.risk',
    
    compute.function = function(sim, data, log=T)
    {
        sim.data = sim$optimized.get(data$optimized.instr)
        sim.vector = vapply(data$mapping.indices, function(ind){sum(sim.data[ind])}, FUN.VALUE = numeric(1))
        sim.year.totals = vapply(data$year.total.mapping.indices, function(ind){sum(sim.data[ind])}, FUN.VALUE = numeric(1))
        sim.denominators = sim.year.totals[data$year.for.obs.indices]
        sim.p = sim.vector / sim.denominators
        
        sim.sigma = sapply(1:length(sim.p), function(i){
            sapply(1:length(sim.p), function(j){
                if (i==j)
                    sim.denominators[i] * sim.p[i] * (1-sim.p[i])
                else if (data$obs.metadata$year[i] == data$obs.metadata$year[j])
                    -sim.denominators[i] * sim.p[i] * sim.p[j]
                else
                    0
            })
        })
        
        sigma = sim.sigma + data$obs.cov.mat * sim.denominators %*% t(sim.denominators)
        lik.summary = cbind(
            data$obs.metadata,
            obs.p = data$obs.vector,
            sim.p = sim.p,
            sd = sqrt(diag(sigma))/ sim.denominators
        )
      
        mvtnorm::dmvnorm(x = data$obs.vector * sim.denominators,
                         mean = sim.vector,
                         sigma = sigma * 16,
                         log = log
                         )
    },
    
    get.data.function = function(version, location)
    {
        sim.meta = get.simulation.metadata('rw',location,from.year=1970,to.year=2030)
      
        non.adap.race.risk = RW.DATA.MANAGER$pull("non.adap.clients",
                                                  keep.dimensions = c('year','location','sex','risk'),
                                                  location = location,
                                                  target.ontology = sim.meta$outcome.ontologies$non.adap.clients,
                                                  allow.mapping.from.target.ontology = T)[,,,,1,drop=T]
        data.dim.names = dimnames(non.adap.race.risk)
#        data.dim.names = data.dim.names[names(data.dim.names)!='source']
 #       dim(non.adap.race.risk) = sapply(data.dim.names, length)
  #      dimnames(non.adap.race.risk) = data.dim.names
        
        mapping = get.ontology.mapping(from.ontology = sim.meta$outcome.ontologies$non.adap.clients[names(sim.meta$outcome.ontologies$non.adap.clients)!='location'], 
                                       to.ontology = data.dim.names)
        
        data.years = dimnames(non.adap.race.risk)$year
        sim.data.dim.names = sim.meta$get.dim.names('non.adap.clients', keep.dimensions = c('year','sex','risk'), dimension.values = list(year=data.years))
        sim.data.dim.names = sim.data.dim.names[names(sim.data.dim.names)!='sim']
        optimized.instr = sim.meta$prepare.optimized.get.instructions('non.adap.clients',keep.dimensions = c('year','sex','risk'), dimension.values = list(year=data.years))
        
        # mapping.matrix = mapping$get.matrix(from.dim.names = sim.data.dim.names,
        #                                     to.dim.names = dimnames(non.adap.race.risk))
        mapping.indices = mapping$get.mapping.indices(from.dim.names = sim.data.dim.names,
                                                      to.dim.names = data.dim.names)
        
        year.total.mapping.indices = lapply(data.years, function(year){
            get.array.access.indices(arr.dim.names = sim.data.dim.names, dimension.values = list(year=year))
        })
        names(year.total.mapping.indices)
        
        response.metadata = reshape2::melt(non.adap.race.risk)
        
        response.na.mask = is.na(response.metadata$value)
        response.metadata = response.metadata[!response.na.mask,]
        mapping.indices = mapping.indices[!response.na.mask]
        
        response.vector = response.metadata$value
        year.for.response.indices = sapply(response.metadata$year, function(year){
            (1:length(data.years))[year==data.years]
        })
        
        data.year.totals = vapply(data.years, function(year){sum(response.vector[response.metadata$year==year])}, FUN.VALUE = numeric(1))
        response.denominators = data.year.totals[year.for.response.indices]
        
        CV = 0.05
        RHO = RW.DIFFERENT.YEARS.CORRELATION
        data.sds = response.vector * CV
        
        cor.mat = sapply(1:length(response.vector), function(i){
            sapply(1:length(response.vector), function(j){
                if (i==j)
                    1
                else if (response.metadata$sex[i]==response.metadata$sex[j] &&
                         response.metadata$risk[i]==response.metadata$risk[j])
                    RHO
              else
                  0
            })
        })
        obs.cov.mat = data.sds %*% t(data.sds) * cor.mat
        
        data = list(
            optimized.instr = optimized.instr,
            
            mapping.indices = mapping.indices,
            year.total.mapping.indices = year.total.mapping.indices,
            year.for.obs.indices = year.for.response.indices,
            
            obs.vector = response.vector / response.denominators,
            obs.metadata = response.metadata,
            obs.cov.mat = obs.cov.mat * (1/response.denominators) %*% t(1/response.denominators)
        )
        data
    }
)


rw.oahs.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.clients", 
  outcome.for.sim = "oahs.clients",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
  from.year = RW.CALIBRATE.FROM.YEAR,
  correlation.different.years = RW.DIFFERENT.YEARS.CORRELATION, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)

rw.oahs.suppression.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.suppression", 
  outcome.for.sim = "oahs.suppression",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = RW.CALIBRATE.FROM.YEAR,
  correlation.different.years = 0.5, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)




get.adap.proportion.of.diagnosed.bias <- function(version, location, 
                                                  multiply.by.p.suppressed = F)
{
    #-- Set Up --#
    target.years = RW.CALIBRATE.FROM.YEAR:2023
  
    states = get.overlapping.locations(location, 'state') 
    counties = get.contained.locations(location, 'county')
    
    #-- Pull Data --#
    state.diagnosed.pwh = RW.DATA.MANAGER$pull('diagnosed.prevalence', location=states, year=target.years)
    state.adap.clients = RW.DATA.MANAGER$pull('adap.clients', location=states, year=target.years)
    state.non.adap.clients = RW.DATA.MANAGER$pull('non.adap.clients', location=states, year=target.years)
    msa.non.adap.clients = RW.DATA.MANAGER$pull('non.adap.clients', location=location, year=target.years)
    
    years = intersect(
      intersect(dimnames(state.non.adap.clients)$year,
                dimnames(state.diagnosed.pwh)$year),
      intersect(dimnames(msa.non.adap.clients)$year,
                dimnames(state.adap.clients)$year)
    )
    
    if (multiply.by.p.suppressed)
    {
        state.p.suppressed.adap = RW.DATA.MANAGER$pull('adap.suppression', location=states, year=years)
        years = intersect(years, dimnames(state.p.suppressed.adap)$year)
    }
    
    #-- Get prevalence counts for portion of MSA that is in each state --#
    msa.portion.in.state.diagnosed.pwh = sapply(states, function(state){
        msa.counties.in.state = intersect(counties, get.contained.locations(state, 'county'))
        rv = RW.DATA.MANAGER$pull('diagnosed.prevalence', location=msa.counties.in.state, keep.dimensions=c('year'), year=years)
        years <<- dimnames(rv)$year
        rv
    })
    dimnames(msa.portion.in.state.diagnosed.pwh) = list(year=years, location=states)
    
    msa.diagnosed.pwh = rowSums(msa.portion.in.state.diagnosed.pwh)
    
    if (ADAP.PROPORTION.N.STATES < length(states))
        states.to.use = states[order(colSums(msa.portion.in.state.diagnosed.pwh), decreasing = T)][1:ADAP.PROPORTION.N.STATES]
    else
        states.to.use = states
    
    #-- Clean up our data: limit to the years we have everywhere --#
    state.dim.names = list(year = as.character(years), location=states.to.use)
   
    state.diagnosed.pwh = state.diagnosed.pwh[years,states.to.use,]
    dim(state.diagnosed.pwh) = sapply(state.dim.names, length)
    dimnames(state.diagnosed.pwh) = state.dim.names
    
    state.adap.clients = state.adap.clients[years,states.to.use,]
    dim(state.adap.clients) = sapply(state.dim.names, length)
    dimnames(state.adap.clients) = state.dim.names
    
    state.non.adap.clients = state.non.adap.clients[years,states.to.use,]
    dim(state.non.adap.clients) = sapply(state.dim.names, length)
    dimnames(state.non.adap.clients) = state.dim.names
    
    msa.non.adap.clients = msa.non.adap.clients[years,,]
    
    msa.portion.in.state.diagnosed.pwh = msa.portion.in.state.diagnosed.pwh[,states.to.use,drop=F]
    
    non.na.mask = !is.na(msa.diagnosed.pwh)
    state.weights = colSums(msa.portion.in.state.diagnosed.pwh[non.na.mask,,drop=F]) / sum(msa.portion.in.state.diagnosed.pwh[non.na.mask,,drop=F])
  
    if (multiply.by.p.suppressed)
    {
        state.p.suppressed.adap = state.p.suppressed.adap[years,states.to.use,]
        dim(state.p.suppressed.adap) = sapply(state.dim.names, length)
        dimnames(state.p.suppressed.adap) = state.dim.names
    }
    
    #-- Calculate ADAP/non-ADAP for state --#
    state.adap.to.non.adap.ratio = state.adap.clients / state.non.adap.clients
    non.na.mask = apply(!is.na(state.adap.to.non.adap.ratio), 1, all)
    use.state.mask = rep(T, ncol(state.adap.to.non.adap.ratio))
    if (multiply.by.p.suppressed)
    {
        use.state.mask = use.state.mask & apply(!is.na(state.p.suppressed.adap), 2, any)
        non.na.mask = non.na.mask & apply(!is.na(state.p.suppressed.adap[,use.state.mask,drop=F]), 1, all)
    }
    
    state.weights = state.weights[use.state.mask] / sum(state.weights[use.state.mask]) 
    
    state.adap.to.non.adap.ratio = colSums(t(state.adap.to.non.adap.ratio[non.na.mask,use.state.mask,drop=F]) * state.weights)
    years = names(state.adap.to.non.adap.ratio)
    
    #-- Calculate non-ADAP/prevalence for state and MSA --#
    state.non.adap.to.prevalence.ratio = state.non.adap.clients[years,use.state.mask,drop=F] / state.diagnosed.pwh[years,use.state.mask,drop=F]
    msa.non.adap.to.prevalence.ratio = msa.non.adap.clients[years] / msa.diagnosed.pwh[years]

    #-- msa.portion.in.state.adap.proportion.of.diagnosed = msa.portion.in.state.adap.clients / msa.portion.in.state.diagnosed.pwh --#
    msa.portion.in.state.adap.clients = msa.portion.in.state.diagnosed.pwh[years,use.state.mask,drop=F] * msa.non.adap.to.prevalence.ratio * state.adap.to.non.adap.ratio[years]
    msa.adap.proportion = msa.non.adap.to.prevalence.ratio * state.adap.to.non.adap.ratio[years]
    
    in.state.out.of.msa.adap.clients = state.adap.clients[years,use.state.mask,drop=F] - msa.portion.in.state.adap.clients
    in.state.out.of.msa.diagnosed.pwh = state.diagnosed.pwh[years,use.state.mask,drop=F] - msa.portion.in.state.diagnosed.pwh[years,use.state.mask,drop=F]
    in.state.out.of.msa.adap.proportion.of.diagnosed = in.state.out.of.msa.adap.clients / in.state.out.of.msa.diagnosed.pwh
    
    p.bias.by.year.state = in.state.out.of.msa.adap.proportion.of.diagnosed - msa.adap.proportion

    if (multiply.by.p.suppressed)
        p.bias.by.year.state = p.bias.by.year.state * state.p.suppressed.adap[non.na.mask,use.state.mask,drop=F];
    
    p.bias.by.year = colSums(t(p.bias.by.year.state)*state.weights)
    mean(p.bias.by.year)
}

ADAP.PROPORTION.N.STATES = 2
# rw.adap.likelihood.instructions = 
#   create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.proportion.of.diagnosed",
#                                                    outcome.for.sim = "adap.proportion.of.diagnosed",
#                                                    denominator.outcome.for.data = 'diagnosed.prevalence',
#                                                    
#                                                    location.types = c('STATE','CBSA'), 
#                                                    maximum.locations.per.type = ADAP.PROPORTION.N.STATES,
#                                                    minimum.geographic.resolution.type = 'COUNTY',
#                                                    location.stratum.keep.threshold = 2, # default
#                                                    
#                                                    correlation.different.years = RW.DIFFERENT.YEARS.CORRELATION,
#                                                    
#                                                    dimensions = c("age","sex","race","risk"),
#                                                    
#                                                    levels.of.stratification = c(0,1), 
#                                                    from.year = RW.CALIBRATE.FROM.YEAR, 
#                                                    
#                                                    p.bias.inside.location = 0, 
#                                                    p.bias.outside.location = get.adap.proportion.of.diagnosed.bias,
#                                                    p.bias.sd.inside.location = 0.05,
#                                                    p.bias.sd.outside.location = function(version, location){
#                                                      0.05 + 0.1 * abs(get.adap.proportion.of.diagnosed.bias(version, location))
#                                                    },
#                                                    
#                                                    within.location.p.error.correlation = 0.5,
#                                                    within.location.n.error.correlation = 0.5,
#                                                    
#                                                    observation.correlation.form = 'compound.symmetry', 
#                                                    p.error.variance.term = 0.05, 
#                                                    p.error.variance.type = 'sd',
#                                                    
#                                                    partitioning.function = EHE.PARTITIONING.FUNCTION, 
#                                                    
#                                                    weights = 1,
#                                                    equalize.weight.by.year = T
#   )

rw.adap.suppression.likelihood.instructions =
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.suppressed.proportion.of.diagnosed",
                                                   outcome.for.sim = "adap.suppressed.proportion.of.diagnosed",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',

                                                   location.types = c('STATE','CBSA'),
                                                   maximum.locations.per.type = ADAP.PROPORTION.N.STATES,
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 1, # default
                                                  

                                                   dimensions = c("age","sex","race","risk"),

                                                   correlation.different.years = 0.5,
                                                   levels.of.stratification = c(0,1),
                                                   from.year = RW.CALIBRATE.FROM.YEAR,

                                                   p.bias.inside.location = suppression.bias.estimates$in.mean,
                                                   p.bias.outside.location = function(version, location){get.adap.proportion.of.diagnosed.bias(version, location, multiply.by.p.suppressed = T)},
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = function(version, location){
                                                     suppression.bias.estimates$out.sd + 0.1 * abs(get.adap.proportion.of.diagnosed.bias(version, location, multiply.by.p.suppressed = T))
                                                     },

                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,

                                                   observation.correlation.form = 'compound.symmetry',
                                                   p.error.variance.term = 0.05,
                                                   p.error.variance.type = 'sd',

                                                   partitioning.function = EHE.PARTITIONING.FUNCTION,

                                                   weights = 1,
                                                   equalize.weight.by.year = T
  )

rw.adap.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.proportion",
                                                   outcome.for.sim = "adap.proportion",
                                                   denominator.outcome.for.data = 'non.adap.clients',
                                                   
                                                   location.types = c('STATE','CBSA'), 
                                                   maximum.locations.per.type = ADAP.PROPORTION.N.STATES,
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 1, # default
                                                   
                                                   correlation.different.years = RW.DIFFERENT.YEARS.CORRELATION,
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = RW.CALIBRATE.FROM.YEAR, 
                                                   to.year = 2021,
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = 0,
                                                   p.bias.sd.inside.location = 0.05,
                                                   p.bias.sd.outside.location = 0.05,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.05, 
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = 1,
                                                   equalize.weight.by.year = T
  )

# rw.adap.suppression.likelihood.instructions = 
#   create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.suppression",
#                                                    outcome.for.sim = "adap.suppression",
#                                                    denominator.outcome.for.data = 'adap.clients',
#                                                    
#                                                    location.types = c('STATE','CBSA'), 
#                                                    maximum.locations.per.type = ADAP.PROPORTION.N.STATES,
#                                                    minimum.geographic.resolution.type = 'COUNTY',
#                                                    location.stratum.keep.threshold = 1, # default
#                                                    
#                                                    dimensions = c("age","sex","race","risk"),
#                                                    
#                                                    correlation.different.years = 0.5,
#                                                    levels.of.stratification = c(0,1), 
#                                                    from.year = RW.CALIBRATE.FROM.YEAR, 
#                                                    
#                                                    p.bias.inside.location = suppression.bias.estimates$in.mean, 
#                                                    p.bias.outside.location = suppression.bias.estimates$out.mean, 
#                                                    p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
#                                                    p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
#                                                    
#                                                    within.location.p.error.correlation = 0.5,
#                                                    within.location.n.error.correlation = 0.5,
#                                                    
#                                                    observation.correlation.form = 'compound.symmetry', 
#                                                    p.error.variance.term = 0.05,
#                                                    p.error.variance.type = 'sd',
#                                                    
#                                                    partitioning.function = EHE.PARTITIONING.FUNCTION, 
#                                                    
#                                                    weights = 1,
#                                                    equalize.weight.by.year = T
#   )

ryan.white.likelihood.instructions = join.likelihood.instructions(
  
    rw.adap.likelihood.instructions,
    rw.adap.suppression.likelihood.instructions,
  
    rw.non.adap.likelihood.instructions,
    rw.non.adap.sex.risk.proportion.likelihood.instructions,
    
    rw.oahs.likelihood.instructions,
    rw.oahs.suppression.likelihood.instructions
    
)
