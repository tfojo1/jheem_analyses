
RW.N.ITER.FIRST.SIM = 2000
RW.N.ITER.SUBSEQUENT.SIMS = 200
RW.N.ITER.SUBSEQUENT.SIMS.IF.RESTART.PARAMS = 1000

RW.MCMC.CHECKING = new.env()
RW.DEBUG = T

fit.rw.simset <- function(simset, verbose=F)
{
    if (verbose)
        print(paste0("Setting up to fit Ryan White parameters for ", simset$n.sim, " simulations in ", simset$location))
  
    mcmc.settings = new.env()
    mcmc.settings$sim.index = 1
  
    # Set up the transmuter
    if (verbose)
        print("Setting up the JHEEM transmuter...")
    transmuter = create.jheem.transmuter(simset, to.version='rw')
    
    # Set up the 'run' simulation function
    transmute.simulation = function(parameters)
    {
        tryCatch({
            transmuter$transmute(sim.index = mcmc.settings$sim.index,
                                 parameters = parameters)
        },
        error = function(e){
          browser()
            NULL
        })
    }
    
    # Instantiate the likelihood
    
    if (verbose)
        print("Instantiating the likelihood...")
    likelihood = ryan.white.likelihood.instructions$instantiate.likelihood('rw',
                                                                           location=simset$location, 
                                                                           data.manager = RW.DATA.MANAGER)
    
    if (RW.DEBUG)
        RW.MCMC.CHECKING$likelihoods = likelihood
    
    compute.likelihood = function(sim)
    {
        if (is.null(sim))
            -Inf
        else
            likelihood$compute(sim, use.optimized.get=T, log=T, check.consistency=F)
    }
    
    # Set up Parameter Scales
    
    if (verbose)
      print("Setting up parameters...")
    parameter.scales = get.distribution.variable.transformation(RYAN.WHITE.PARAMETERS.PRIOR)
    
    
    # Set up initial cov mat
    parameter.sds = suppressWarnings(get.sds(RYAN.WHITE.PARAMETERS.PRIOR))
    if (any(is.na(parameter.sds)))
        stop("Cannot have improper variables in the Ryan White Prior Distribution")
    parameter.means = suppressWarnings(get.means(RYAN.WHITE.PARAMETERS.PRIOR))
    
    parameter.sds[parameter.scales=='log'] = sqrt(log(parameter.sds[parameter.scales=='log']^2 / parameter.means[parameter.scales=='log']^2 + 1)) # This comes from the relationship between mean and SD in a lognormal
    DEFAULT.SD.REDUCTION.FACTOR = 20 # The 20 here is arbitrary, but seems to work well
    parameter.sds = parameter.sds / DEFAULT.SD.REDUCTION.FACTOR
    
    initial.cov.mat = diag(parameter.sds^2)
    
    
    # Set up initial scaling parameters
    initial.scaling.parameters = lapply(RYAN.WHITE.SAMPLING.BLOCKS, function(block){
        sapply(block, function(var.in.block){
            2.38^2/length(block)
        })
    })
    
    # Take a quick guess for non.adap.proportion
    default.start.values = get.medians(RYAN.WHITE.PARAMETERS.PRIOR)
    sim0 = transmuter$transmute(sim.index = 1, parameters = default.start.values)
    
    non.adap.clients = RW.DATA.MANAGER$pull('non.adap.clients', location=simset$location)[,1,1]
    diagnosed.pwh = apply(SURVEILLANCE.MANAGER$pull('diagnosed.prevalence', location=simset$location, year=names(non.adap.clients)), 'year', mean, na.rm=T)
    years.to.use = intersect(names(non.adap.clients)[!is.na(non.adap.clients)], names(diagnosed.pwh)[!is.na(diagnosed.pwh)])
    mean.p.non.adap = mean(non.adap.clients[years.to.use] / diagnosed.pwh[years.to.use])
    
    sim.p.adap = mean(sim0$get(outcomes='non.adap.clients', year=years.to.use)[,,] / sim0$get(outcomes='diagnosed.prevalence', year=years.to.use)[,,])
    
    adjust.by.or = ( mean.p.non.adap / (1-mean.p.non.adap) ) / ( sim.p.adap / (1-sim.p.adap) )
    
    default.start.values['non.adap.or'] = default.start.values['non.adap.or'] * adjust.by.or
    
    # Pull it together in MCMC settings
    mcmc.settings$start.values = default.start.values
    mcmc.settings$ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
        simulation.function = transmute.simulation,
        log.prior.distribution = get.density.function(RYAN.WHITE.PARAMETERS.PRIOR),
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = ifelse(RW.DEBUG, 0, RW.N.ITER - 1),
        thin = 1,
        var.blocks = RYAN.WHITE.SAMPLING.BLOCKS,
        reset.adaptive.scaling.update.after = 0,
        transformations = parameter.scales,
        
        initial.covariance.mat = initial.cov.mat,
        initial.scaling.parameters = initial.scaling.parameters,
        
        target.acceptance.probability = 0.238,
        
        n.iter.before.use.adaptive.covariance = 0,
        adaptive.covariance.base.update = 1,
        adaptive.covariance.update.prior.iter = 50,
        adaptive.covariance.update.decay = 1,
        adaptive.scaling = 'componentwise',
        adaptive.scaling.base.update = 1,
        adaptive.scaling.update.prior.iter= 10,
        adaptive.scaling.update.decay = 0.5
    )
    
    # Run an MCMC for each sim
    
    if (RW.DEBUG)
        RW.MCMC.CHECKING$mcmc.runs = list()
    
    if (verbose)
        print(paste0("Looping through all ", simset$n.sim, " simulations..."))
    sim.list = lapply(1:simset$n.sim, function(i){
        mcmc.settings$sim.index = i
        if (i==1)
            n.iter.for.i = RW.N.ITER.FIRST.SIM
        else
            n.iter.for.i = RW.N.ITER.SUBSEQUENT.SIMS
        
        if (verbose)
            print(paste0("STARTING MCMC FOR SIM ", i, " of ", simset$n.sim))
        
        # Run the first sim and make sure the likelihood evaluates
        sim = transmute.simulation(mcmc.settings$start.values)
        if (is.null(sim))
        {
            if (i!=1)
            {
                sim = transmute.simulation(default.start.values)
            }
          
            if (is.null(sim))
            {
                errored.params <<- mcmc.settings$start.values
                stop(paste0("The ", get.ordinal(i), " simulation throws an error when computing.\nThe parameters have been saved in the global environment as 'errored.params'"))
            }
            
            mcmc.settings$start.values = default.start.values
            n.iter.for.i = RW.N.ITER.SUBSEQUENT.SIMS.IF.RESTART.PARAMS
        }
        
        if (likelihood$compute(sim, use.optimized.get=T)==-Inf)
        {
            lik.pieces = likelihood$compute.piecewise(sim, use.optimized.get=T)
            errored.likelihood <<- likelihood
            errored.sim <<- sim
            errored.params <<- mcmc.settings$start.values
            
            stop(paste0("The likelihood evaluates to -Inf on the initial parameter values for the ", 
                        get.ordinal(i), " simulation. The likelihood components are:\n",
                        paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                        "\nThe parameters, simulation, and likelihood have been saved in the global environment as 'errored.params', 'errored.sim', and 'errored.likelihood'"))
        }
        
        # Run the MCMC
        mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                              n.iter = n.iter.for.i,
                                              starting.values = mcmc.settings$start.values,
                                              cache.frequency = NA,
                                              #update.frequency = NA)
                                              update.frequency = ifelse(verbose, 50, NA))
        
        if (verbose)
            print("   DONE")
        
        if (RW.DEBUG)
            RW.MCMC.CHECKING$mcmc.runs = c(RW.MCMC.CHECKING$mcmc.runs, mcmc)
        
        update.rw.mcmc.settings(mcmc = mcmc,
                                mcmc.settings = mcmc.settings)
        
        mcmc@simulations[[length(mcmc@simulations)]]
    })
    
    # Put the sims back together
    if (verbose)
      print(paste0("All done fitting Ryan White parameters for ", simset$n.sim, " simulations in ", simset$location, ". Packaging up"))
    
    join.simulation.sets(sim.list)
}

get.fitted.rw.simulation <- function(sim.transmuter,
                                     sim.index,
                                     mcmc.settings)
{
    params = get.medians(RYAN.WHITE.PARAMETERS.PRIOR)#generate.random.samples(RYAN.WHITE.PARAMETERS.PRIOR, 1)
    sim.transmuter$transmute(sim.index = sim.index,
                             parameters = params)
    
    mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                          n.iter = 1000,
                                          starting.values = mcmc.settings$start.values,
                                          update.frequency = ifelse(verbose, 50, NA))
    
    
    mcmc@simulations[[length(mcmc@simulations)]]
}

##-- HELPERS --##

update.rw.mcmc.settings <- function(mcmc,
                                    mcmc.settings)
{
    new.ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
      var.names = mcmc.settings$ctrl@var.names,
      simulation.function = mcmc.settings$ctrl@simulation.function,
      log.prior.distribution = mcmc.settings$ctrl@log.prior.distribution,
      log.likelihood = mcmc.settings$ctrl@log.likelihood,
      burn = mcmc.settings$ctrl@burn,
      thin = mcmc.settings$ctrl@thin,
      var.blocks = mcmc.settings$ctrl@var.blocks,
      reset.adaptive.scaling.update.after = mcmc.settings$ctrl@reset.adaptive.scaling.update.after,
      transformations = mcmc.settings$ctrl@transformations,
      
      initial.covariance.mat = mcmc@chain.states[[1]]@cov.mat,
      initial.scaling.parameters = lapply(mcmc@chain.states[[1]]@log.scaling.parameters, exp),
      
      target.acceptance.probability = mcmc.settings$ctrl@target.acceptance.probability,
      
      n.iter.before.use.adaptive.covariance = 0,
      adaptive.covariance.base.update = mcmc.settings$ctrl@adaptive.covariance.base.update,
      adaptive.covariance.update.prior.iter = mcmc.settings$ctrl@adaptive.covariance.update.prior.iter + ceiling(mcmc@n.iter / 50),
      adaptive.covariance.update.decay = mcmc.settings$ctrl@adaptive.covariance.update.decay,
      adaptive.scaling = mcmc.settings$ctrl@adaptive.scaling,
      adaptive.scaling.base.update = mcmc.settings$ctrl@adaptive.scaling.base.update,
      adaptive.scaling.update.prior.iter= mcmc.settings$ctrl@adaptive.scaling.update.prior.iter + ceiling(mcmc@n.iter / 50),
      adaptive.scaling.update.decay = mcmc.settings$ctrl@adaptive.scaling.update.decay
    )
    
    mcmc.settings$ctrl = new.ctrl
    mcmc.settings$start.values = mcmc@chain.states[[1]]@current.parameters
    
    mcmc.settings
}

get.distribution.variable.transformation = function(dist, unknown.value)
{
    if (is(dist, "Joint_Independent_Distributions"))
        rv = unlist(lapply(dist@subdistributions, 
                           get.distribution.variable.transformation, 
                           unknown.value = unknown.value))
    else if (is.null(dist@transformation))
        rv = rep(unknown.value, dist@n.var)
    else
        rv = rep(dist@transformation@name, dist@n.var)
    
    names(rv) = dist@var.names
    rv
}