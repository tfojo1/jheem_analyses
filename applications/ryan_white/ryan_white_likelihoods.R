source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
rw.non.adap.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "non.adap.clients", 
  outcome.for.sim = "non.adap.clients",
  dimensions = c("age","sex","race"),
  levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
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

rw.non.adap.sex.risk.proportion.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'non.adap.sex.risk',
    
    compute.function = function(sim, log=T)
    {
      
    },
    
    get.data.function = function(version, location)
    {
        sim.meta = get.simulation.metadata('rw',location,from.year=1970,to.year=2030)
      
        non.adap.race.risk = RW.DATA.MANAGER$pull("non.adap.clients",
                                                  keep.dimensions = c('year','sex','risk'),
                                                  location = location,
                                                  target.ontology = sim.meta$outcome.ontologies$non.adap.clients)[]
        data.dim.names = dimnames(non.adap.race.risk)
        
        mapping = attr(non.adap.race.risk, 'mapping')
        
        data.years = dimnames(non.adap.race.risk)$year
        sim.data.dim.names = sim.meta$get.dim.names('non.adap.clients', keep.dimensions = c('year','sex','risk'), dimension.values = list(year=data.years))
        sim.data.dim.names = sim.data.dim.names[names(sim.data.dim.names)!='sim']
        optimized.instr = sim.meta$prepare.optimized.get.instructions('non.adap.clients',keep.dimensions = c('year','sex','risk'), dimension.values = list(year=data.years))
        
        mapping.matrix = mapping$get.matrix(from.dim.names = sim.data.dim.names,
                                            to.dim.names = dimnames(non.adap.race.risk))
    }
)


rw.oahs.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.clients", 
  outcome.for.sim = "oahs.clients",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
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

rw.oahs.suppression.likelihood.instructions = create.basic.likelihood.instructions(
  outcome.for.data = "oahs.suppression", 
  outcome.for.sim = "oahs.suppression",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
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




get.adap.proportion.of.diagnosed.bias <- function(version, location)
{
    #-- Set Up --#
    target.years = 2017:2023
  
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
    
    #-- Get prevalence counts for portion of MSA that is in each state --#
    msa.portion.in.state.diagnosed.pwh = sapply(states, function(state){
        msa.counties.in.state = intersect(counties, get.contained.locations(state, 'county'))
        rv = RW.DATA.MANAGER$pull('diagnosed.prevalence', location=msa.counties.in.state, keep.dimensions=c('year'), year=years)
        years <<- dimnames(rv)$year
        rv
    })
    dimnames(msa.portion.in.state.diagnosed.pwh)[[1]] = years
    
    msa.diagnosed.pwh = rowSums(msa.portion.in.state.diagnosed.pwh)
    
    #-- Clean up our data: limit to the years we have everywhere --#
    state.dim.names = list(year = as.character(years), location=states)
    
    state.diagnosed.pwh = state.diagnosed.pwh[years,,]
    dim(state.diagnosed.pwh) = sapply(state.dim.names, length)
    dimnames(state.diagnosed.pwh) = state.dim.names
    
    state.adap.clients = state.adap.clients[years,,]
    dim(state.adap.clients) = sapply(state.dim.names, length)
    dimnames(state.adap.clients) = state.dim.names
    
    state.non.adap.clients = state.non.adap.clients[years,,]
    dim(state.non.adap.clients) = sapply(state.dim.names, length)
    dimnames(state.non.adap.clients) = state.dim.names
    
    msa.non.adap.clients = msa.non.adap.clients[years,,]
    
    non.na.mask = !is.na(msa.diagnosed.pwh)
    state.weights = colSums(msa.portion.in.state.diagnosed.pwh[non.na.mask,,drop=F]) / sum(msa.portion.in.state.diagnosed.pwh[non.na.mask,,drop=F])
  
    # Calculate ADAP/non-ADAP for state
    state.adap.to.non.adap.ratio = state.adap.clients / state.non.adap.clients
    non.na.mask = apply(!is.na(state.adap.to.non.adap.ratio), 1, all)
    
    state.adap.to.non.adap.ratio = colSums(t(state.adap.to.non.adap.ratio[non.na.mask,,drop=F]) * state.weights)
    years = names(state.adap.to.non.adap.ratio)
    
    # Calculate non-ADAP/prevalence for state and MSA
    state.non.adap.to.prevalence.ratio = state.non.adap.clients[years,,drop=F] / state.diagnosed.pwh[years,,drop=F]
    msa.non.adap.to.prevalence.ratio = msa.non.adap.clients[years] / msa.diagnosed.pwh[years]

    # msa.portion.in.state.adap.proportion.of.diagnosed = msa.portion.in.state.adap.clients / msa.portion.in.state.diagnosed.pwh
    msa.portion.in.state.adap.clients = msa.portion.in.state.diagnosed.pwh[years,,drop=F] * msa.non.adap.to.prevalence.ratio * state.adap.to.non.adap.ratio[years]
    msa.adap.proportion = msa.non.adap.to.prevalence.ratio * state.adap.to.non.adap.ratio[years]
    
    in.state.out.of.msa.adap.clients = state.adap.clients[years,,drop=F] - msa.portion.in.state.adap.clients
    in.state.out.of.msa.diagnosed.pwh = state.diagnosed.pwh[years,,drop=F] - msa.portion.in.state.diagnosed.pwh[years,,drop=F]
    in.state.out.of.msa.adap.proportion.of.diagnosed = in.state.out.of.msa.adap.clients / in.state.out.of.msa.diagnosed.pwh
    
    p.bias.by.year.state = msa.adap.proportion - in.state.out.of.msa.adap.proportion.of.diagnosed
    
    p.bias.by.year = colSums(t(p.bias.by.year.state)*state.weights)
    mean(p.bias.by.year)
}


rw.adap.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.proportion.of.diagnosed",
                                                   outcome.for.sim = "adap.proportion.of.diagnosed",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 2, # default
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = 0, 
                                                   p.bias.outside.location = get.adap.proportion.of.diagnosed.bias,
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

rw.adap.suppression.likelihood.instructions = 
  create.nested.proportion.likelihood.instructions(outcome.for.data = "adap.suppressed.proportion.of.diagnosed",
                                                   outcome.for.sim = "adap.suppressed.proportion.of.diagnosed",
                                                   denominator.outcome.for.data = 'diagnosed.prevalence',
                                                   
                                                   location.types = c('STATE','CBSA'), 
                                                   minimum.geographic.resolution.type = 'COUNTY',
                                                   location.stratum.keep.threshold = 2, # default
                                                   
                                                   dimensions = c("age","sex","race","risk"),
                                                   
                                                   levels.of.stratification = c(0,1), 
                                                   from.year = 2008, 
                                                   
                                                   p.bias.inside.location = suppression.bias.estimates$in.mean, 
                                                   p.bias.outside.location = suppression.bias.estimates$out.mean,
                                                   p.bias.sd.inside.location = suppression.bias.estimates$in.sd,
                                                   p.bias.sd.outside.location = suppression.bias.estimates$out.sd,
                                                   
                                                   within.location.p.error.correlation = 0.5,
                                                   within.location.n.error.correlation = 0.5,
                                                   
                                                   observation.correlation.form = 'compound.symmetry', 
                                                   p.error.variance.term = 0.05,
                                                   p.error.variance.type = 'sd',
                                                   
                                                   partitioning.function = EHE.PARTITIONING.FUNCTION, 
                                                   
                                                   weights = 1,
                                                   equalize.weight.by.year = T
  )

ryan.white.likelihood.instructions = join.likelihood.instructions(
  
    rw.adap.likelihood.instructions,
    rw.adap.suppression.likelihood.instructions,
  
    rw.non.adap.likelihood.instructions,
    rw.oahs.likelihood.instructions,
    rw.oahs.suppression.likelihood.instructions
    
)
