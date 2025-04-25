
min.aids.diagnoses.penalty.instructions = create.custom.likelihood.instructions(
    name = 'min.aids.diagnoses',
    
    compute.function = function(sim, data, log=T)
    {
        if (log)
        {
            penalty = -Inf
            score = 0
        }
        else
        {
            penalty = 0
            score = 1
        }
        
        if (any(sim$params[data$trate0.param.names] > sim$params[data$peak.trate.param.names]))
            penalty
        else
            score
    },
    
    get.data.function = function(version, location)
    {
        YEARS = as.character(1992:1996)
        MIN.FRAC = rep(3/4)
        names(MIN.FRAC) = YEARS
        MIN.FRAC['1993'] = 0.5
        
        aids.dx = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                            location = location,
                                            keep.dimensions = c('year','location'))
        aids.dx = rowMeans(aids.dx, na.rm=T)
        
        list(
            peak.trate.param.names = sort(peak.trate.param.names),
            trate0.param.names = sort(trate0.param.names)
        )
    }
)


state.aids.diagnoses.proportions.instructions = create.custom.likelihood.instructions(
    name = 'aids.diagnoses.stratified.proportions',
    
    compute.function = function(sim, data, log=T)
    {
        # RISK
        
        sim.aids.risk = sim$get('aids.diagnoses', 
                                year = data$years,
                                keep.dimensions = c('year','sex','risk'))
        sim.aids.risk = data$risk.mapping$apply(sim.aids.risk, to.dim.names = data$risk.dim.names)
        n.sim.aids.risk = rowSums(sim.aids.risk, na.rm=T)
        p.sim.aids.risk = sim.aids.risk / n.sim.aids.risk
        
        var.p.aids.risk = as.numeric(p.sim.aids.risk * (1-p.sim.aids.risk) / n.sim.aids.risk)
        risk.sigma = data$risk.cov.mat + diag(var.p.aids.risk[data$risk.mask])
        

        d.risk = mvtnorm::dmvnorm(x = data$obs.proportions.msa.aids.dx.risk,
                         mean = as.numeric(p.sim.aids.risk)[data$risk.mask],
                         sigma = risk.sigma,
                         log = log)
        
        # RACE
        
        sim.aids.race = sim$get('aids.diagnoses', 
                                year = data$years,
                                keep.dimensions = c('year','race'))
        
        sim.aids.race = sim.aids.race[,data$race.dim.names$race,1]
        n.sim.aids.race = rowSums(sim.aids.race, na.rm=T)
        p.sim.aids.race = sim.aids.race / n.sim.aids.race
        
        var.p.aids.race = as.numeric(p.sim.aids.race * (1-p.sim.aids.race) / n.sim.aids.race)
        race.sigma = data$race.cov.mat + diag(var.p.aids.race)
        
        
        d.race = mvtnorm::dmvnorm(x = data$obs.proportions.msa.aids.dx.race,
                                  mean = as.numeric(p.sim.aids.race),
                                  sigma = race.sigma,
                                  log = log)
        
        # RETURN
        if (log)
            d.risk + d.race
        else
            d.risk * d.race
        
    },
    
    get.data.function = function(version, location)
    {
        YEARS = as.character(1985:1993)
        RHO = 0.5
        
        spec.meta = get.specification.metadata(version, location)
        sim.meta = get.simulation.metadata(version, location, from.year = 1970, to.year = 2030)
        
        if (get.location.type(location)!='STATE')
            stop("This custom likelihood only works for states")
        
        msas = get.overlapping.locations(location, 'CBSA')
        
        # Pull MSA aids x risk
        msa.aids.dx.risk = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                     location = msas,
                                                     source='cdc.aids',
                                                     keep.dimensions = c('year','location','risk'))
        aggregate.msa.aids.dx.risk = apply(msa.aids.dx.risk, c('year','risk'), sum, na.rm=T)
        proportions.msa.aids.dx.risk = aggregate.msa.aids.dx.risk / rowSums(aggregate.msa.aids.dx.risk)
        
        # Pull MSA aids x race
        msa.aids.dx.race = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                     location = msas,
                                                     source='cdc.aids',
                                                     keep.dimensions = c('year','location','race'))
        aggregate.msa.aids.dx.race = apply(msa.aids.dx.race, c('year','race'), sum, na.rm=T)
        
        race.mapping = get.ontology.mapping(from.ontology = dimnames(aggregate.msa.aids.dx.race)['race'],
                                            to.ontology = spec.meta$dim.names['race'])
        
        aggregate.msa.aids.dx.race = race.mapping$apply(aggregate.msa.aids.dx.race)
        
        proportions.msa.aids.dx.race = aggregate.msa.aids.dx.race / rowSums(aggregate.msa.aids.dx.race)
        
        
        
        # Pull state aids x risk
        state.aids.dx.risk = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                     location = location,
                                                     keep.dimensions = c('year','location','risk'))
        state.aids.dx.risk = apply(state.aids.dx.risk, c('year','risk'), mean, na.rm=T)
        state.aids.dx.other = state.aids.dx.risk[,'other']
        state.aids.dx.risk = state.aids.dx.risk[,setdiff(dimnames(state.aids.dx.risk)$risk, 'other')]
        state.aids.dx.risk[,'heterosexual'] = state.aids.dx.risk[,'heterosexual'] + state.aids.dx.other
        
        proportions.state.aids.dx.risk = state.aids.dx.risk / rowSums(state.aids.dx.risk)
        
        # Pull state aids x race
        state.aids.dx.race = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                       location = location,
                                                       keep.dimensions = c('year','location','race'))
        state.aids.dx.race = apply(state.aids.dx.race, c('year','race'), mean, na.rm=T)

        proportions.state.aids.dx.race = state.aids.dx.race / rowSums(state.aids.dx.race)
        
        
        # Figure out the average state vs aggregate MSA error
        
        overlap.years.state.msa = intersect(dimnames(proportions.msa.aids.dx.risk)$year,
                                            dimnames(proportions.state.aids.dx.risk)$year)
        
        errors.risk = proportions.msa.aids.dx.risk[overlap.years.state.msa,colnames(proportions.state.aids.dx.risk)] - proportions.state.aids.dx.risk[overlap.years.state.msa,]
        errors.risk.cv = errors.risk / proportions.state.aids.dx.risk[overlap.years.state.msa,]
        errors.risk.weight = proportions.state.aids.dx.risk[overlap.years.state.msa,]
        
        
        overlap.years.state.msa = intersect(dimnames(proportions.msa.aids.dx.race)$year,
                                            dimnames(proportions.state.aids.dx.race)$year)
        
        errors.race = proportions.msa.aids.dx.race[overlap.years.state.msa,colnames(proportions.state.aids.dx.race)] - proportions.state.aids.dx.race[overlap.years.state.msa,]
        errors.race.cv = errors.race / proportions.state.aids.dx.race[overlap.years.state.msa,]
        errors.race.weight = proportions.state.aids.dx.race[overlap.years.state.msa,]
        
        # Strip down our data
        proportions.msa.aids.dx.risk = proportions.msa.aids.dx.risk[YEARS,]
        proportions.msa.aids.dx.race = proportions.msa.aids.dx.race[YEARS,]
        
        
        # Package up
        
        data = new.env()
        data$obs.proportions.msa.aids.dx.risk = as.numeric(proportions.msa.aids.dx.risk)
        data$risk.dim.names = dimnames(proportions.msa.aids.dx.risk)
        data$obs.proportions.msa.aids.dx.race = as.numeric(proportions.msa.aids.dx.race)
        data$race.dim.names = dimnames(proportions.msa.aids.dx.race)
        data$risk.cv = sqrt(sum(errors.risk.cv^2 * errors.risk.weight) / sum(errors.risk.weight))
        data$race.cv = sqrt(sum(errors.race.cv^2 * errors.race.weight) / sum(errors.race.weight))
        data$years = YEARS
        
        
        # Get the mappings
        data$risk.mapping = get.ontology.mapping(from.ontology = spec.meta$dim.names[c('sex','risk')],
                                            to.ontology = dimnames(proportions.msa.aids.dx.risk)['risk'])
        
        # Build obs variance matrices
        
        obs.risk.values = get.every.combination(dimnames(proportions.msa.aids.dx.risk))
        n.obs.risk = nrow(obs.risk.values)
        risk.cor.mat = sapply(1:n.obs.risk, function(i){
            sapply(1:n.obs.risk, function(j){
                if (obs.risk.values[i,'risk']==obs.risk.values[j,'risk'])
                {
                    if (obs.risk.values[i,'year']==obs.risk.values[j,'year'])
                        1
                    else
                        RHO
                }
                else
                    0
            })
        })
        data$risk.cov.mat = data$obs.proportions.msa.aids.dx.risk %*% t(data$obs.proportions.msa.aids.dx.risk) * data$risk.cv^2 * risk.cor.mat
        
        obs.race.values = get.every.combination(dimnames(proportions.msa.aids.dx.race))
        n.obs.race = nrow(obs.race.values)
        race.cor.mat = sapply(1:n.obs.race, function(i){
            sapply(1:n.obs.race, function(j){
                if (obs.race.values[i,'race']==obs.race.values[j,'race'])
                {
                    if (obs.race.values[i,'year']==obs.race.values[j,'year'])
                        1
                    else
                        RHO
                }
                else
                    0
            })
        })
        data$race.cov.mat = data$obs.proportions.msa.aids.dx.race %*% t(data$obs.proportions.msa.aids.dx.race) * data$race.cv^2 * race.cor.mat
        
        
        # Filter out NA risk values
        
        data$risk.mask = !is.na(data$obs.proportions.msa.aids.dx.risk)
        data$obs.proportions.msa.aids.dx.risk = data$obs.proportions.msa.aids.dx.risk[data$risk.mask]
        data$risk.cov.mat = data$risk.cov.mat[data$risk.mask,data$risk.mask]
       
        # Return
        data
    }
)


state.aids.diagnoses.ratio.instructions = create.custom.likelihood.instructions(
    name = 'aids.diagnoses.total.ratio',
    
    compute.function = function(sim, data, log=T)
    {
        sim.aids = sim$get('aids.diagnoses', 
                                year = c(data$rel.to.year, data$years),
                                keep.dimensions = c('year'))
        
        sim.aids.ratios = sim.aids[data$years,1] / sim.aids[data$rel.to.year,1]
        
        print(cbind(sim=sim.aids.ratios, obs=data$obs.aids.dx.ratio))
        d = dnorm(data$obs.aids.dx.ratio, mean=sim.aids.ratios, sd=data$sds, log=log)

        # RETURN
        if (log)
            sum(d)
        else
            prod(d)
        
    },
    
    get.data.function = function(version, location)
    {
        REL.TO.YEAR = as.character(1985)
        YEARS = as.character(1988:1993)
        GAP.AFTER.REL.TO = as.numeric(YEARS[1]) - as.numeric(REL.TO.YEAR)
        
        spec.meta = get.specification.metadata(version, location)
        sim.meta = get.simulation.metadata(version, location, from.year = 1970, to.year = 2030)
        
        if (get.location.type(location)!='STATE')
            stop("This custom likelihood only works for states")
        
        msas = get.overlapping.locations(location, 'CBSA')
        
        # Pull MSA aids
        msa.aids.dx = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                location = msas,
                                                source='cdc.aids',
                                                keep.dimensions = c('year','location'))
        aggregate.msa.aids.dx = apply(msa.aids.dx, c('year'), sum, na.rm=T)
        ratios.msa.aids.dx = aggregate.msa.aids.dx /aggregate.msa.aids.dx['1985']
        
        
        # Pull state aids dx
        state.aids.dx = SURVEILLANCE.MANAGER$pull('aids.diagnoses',
                                                     location = location,
                                                     keep.dimensions = c('year','location'))
        
        
        state.aids.dx = rowMeans(state.aids.dx, na.rm=T)
        overlapping.years = intersect(names(state.aids.dx), names(aggregate.msa.aids.dx))
        
        rel.errors = ( aggregate.msa.aids.dx[overlapping.years[-1-0:GAP.AFTER.REL.TO] ] / aggregate.msa.aids.dx[ overlapping.years[1] ] -
                          state.aids.dx[ overlapping.years[-1-0:GAP.AFTER.REL.TO] ] / state.aids.dx[ overlapping.years[1] ] ) /
            (state.aids.dx[ overlapping.years[-1-0:GAP.AFTER.REL.TO] ] / state.aids.dx[ overlapping.years[1] ])
        
        # df = data.frame(
        #     var = rel.errors^2,
        #     delta = (GAP.AFTER.REL.TO):(length(overlapping.years)-2)
        # )
        # fit = lm(var ~ delta, data=df)
        
        cv = sqrt(mean(rel.errors^2))
        
        # Package up
        
        data = new.env()
        data$obs.aids.dx.ratio = ratios.msa.aids.dx[YEARS]
        data$years = YEARS
        data$rel.to.year = REL.TO.YEAR
        data$sds = data$obs.aids.dx.ratio * cv

        # Return
        data
    }
)

trate.peak.penalty.likelihood.instructions = create.custom.likelihood.instructions(
    name = 'aids.diagnoses.total.ratio',
    
    compute.function = function(sim, data, log=T)
    {
        if (log)
        {
            penalty = -Inf
            score = 0
        }
        else
        {
            penalty = 0
            score = 1
        }
        
        if (any(sim$params[data$trate0.param.names] > sim$params[data$peak.trate.param.names]))
            penalty
        else
            score
    },
    
    get.data.function = function(version, location)
    {
        param.names = get.parameter.names.for.version(version, 'calibrated')
        peak.trate.param.names = param.names[grepl('trate.*peak', param.names)]
        trate0.param.names = param.names[grepl('trate.*0', param.names)]
        
        list(
            peak.trate.param.names = sort(peak.trate.param.names),
            trate0.param.names = sort(trate0.param.names)
        )
    }
)


prop.lik = state.aids.diagnoses.ratio.instructions$instantiate.likelihood('ehe','FL')

 # lik = state.aids.diagnoses.ratio.instructions$instantiate.likelihood('ehe','FL')
 # lik$compute(sim)

# 
# states = c('AL','LA','FL','TX','GA','CA','NY','MO','IL','MS')
# liks = lapply(states, state.aids.diagnoses.proportions.instructions$instantiate.likelihood, version='ehe')
# 
# sapply(liks, function(lik){lik$compute(sim)})
