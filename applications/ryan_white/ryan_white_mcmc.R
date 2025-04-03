
RW.MCMC.CHECKING = new.env()

set.up.fit.rw.simset <- function(location,
                                 n.sim = 1000,
                                 calibration.code = 'final.ehe',
                                 n.chunks = N.CHUNKS,
                                 n.iter.first.sim=3000, 
                                 n.iter.subsequent.sims=300, 
                                 n.iter.subsequent.sims.if.restart.params=1000,
                                 verbose=F)
{
    if (verbose)
        print(paste0("Setting up to fit Ryan White parameters for ", n.sim, " simulations in ", location))
    
    
    control.dir = get.rw.chunk.dir(location = location,
                                   n.sim = n.sim,
                                   calibration.code = calibration.code)
    if (!dir.exists(control.dir))
        dir.create(control.dir, recursive = T)
    
    simset = retrieve.simulation.set(version = 'ehe', location = location, calibration.code = calibration.code, n.sim = n.sim)    
    
    mcmc.settings = new.env(parent = baseenv())
    mcmc.settings$sim.index = 1
    
    # Set up the transmuter
    if (verbose)
        print("Setting up the JHEEM transmuter...")
    transmuter = create.jheem.transmuter(simset, to.version='rw')
    
    # Set up the 'run' simulation function
    transmute.simulation = function(parameters)
    {
        return(
            transmuter$transmute(sim.index = mcmc.settings$sim.index,
                                 parameters = parameters))
        tryCatch({
            transmuter$transmute(sim.index = mcmc.settings$sim.index,
                                 parameters = parameters)
        },
        error = function(e){
            #browser()
            NULL
        })
    }
    
    trans.env = new.env(parent = baseenv())
    trans.env$transmuter = transmuter
    trans.env$mcmc.settings = mcmc.settings
    environment(transmute.simulation) = trans.env
    
    # Set up RW engine
    # if (verbose)
    #     print("Setting up the Ryan-White engine...")
    # rw.engine = create.jheem.engine('rw', simset$location, simset$to.year)
    
    # Instantiate the likelihood
    
    if (verbose)
        print("Instantiating the likelihood...")
    likelihood = ryan.white.likelihood.instructions$instantiate.likelihood('rw',
                                                                           location=simset$location, 
                                                                           data.manager = RW.DATA.MANAGER)
    
    compute.likelihood = function(sim)
    {
        if (is.null(sim))
            -Inf
        else
            likelihood$compute(sim, use.optimized.get=T, log=T, check.consistency=F)
    }
    
    lik.env = new.env(parent = baseenv())
    lik.env$likelihood = likelihood
    environment(compute.likelihood) = lik.env
    
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
    mean.p.non.adap = min(.95, mean.p.non.adap)
    
    sim.p.adap = mean(sim0$get(outcomes='non.adap.clients', year=years.to.use)[,,] / sim0$get(outcomes='diagnosed.prevalence', year=years.to.use)[,,])
    
    adjust.by.or = ( mean.p.non.adap / (1-mean.p.non.adap) ) / ( sim.p.adap / (1-sim.p.adap) )
    
    default.start.values['non.adap.or'] = default.start.values['non.adap.or'] * adjust.by.or
    
    log.prior.fn = function(x, log=T)
    {
        distributions::calculate.density(RYAN.WHITE.PARAMETERS.PRIOR, x, log=log)
    }
    
    prior.env = new.env(baseenv())
    prior.env$RYAN.WHITE.PARAMETERS.PRIOR = RYAN.WHITE.PARAMETERS.PRIOR
    environment(log.prior.fn) = prior.env
    
    # Pull it together in MCMC settings
    mcmc.settings$start.values = default.start.values
    mcmc.settings$ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
        var.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
        simulation.function = transmute.simulation,
        log.prior.distribution = log.prior.fn,
        log.likelihood = compute.likelihood, # saves the data manager in here!
        burn = n.iter.first.sim,
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

    # Run the first sim
    
    if (verbose)
        print("Running the first sim")
    
    mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                          n.iter = n.iter.first.sim,
                                          starting.values = mcmc.settings$start.values,
                                          cache.frequency = NA,
                                          update.detail = 'none',
                                          update.frequency = NA)

    update.rw.mcmc.settings(mcmc = mcmc,
                            mcmc.settings = mcmc.settings,
                            n.iter = n.iter.subsequent.sims)
    
    first.sim = mcmc@simulations[[length(mcmc@simulations)]]
    
    # set up the control and save it
    
    if (verbose)
        print("All done - saving")
    
    n.sim = simset$n.sim
    sim.index.breaks = floor(seq(1,n.sim,length=n.chunks+1))
    sim.indices.for.chunk = lapply(1:n.chunks, function(chunk){
        sim.index.breaks[chunk]:sim.index.breaks[chunk+1]
    })
    
    rw.control = list(
        n.chunks = n.chunks,
        n.iter.first.sim = n.iter.first.sim,
        n.iter.subsequent.sims = n.iter.subsequent.sims,
        n.iter.subsequent.sims.if.restart.params = n.iter.subsequent.sims.if.restart.params,
        sim.indices.for.chunk = sim.indices.for.chunk,
        init.mcmc.settings = mcmc.settings,
        default.start.values = mcmc.settings$start.values,
        likelihood = likelihood,
        transmute.simulation = transmute.simulation,
        first.sim = first.sim
    )
    
    files = list.files(control.dir, full.names = T)
    file.remove(files)
    
    control.filename = get.rw.control.filename(location = location,
                                               n.sim = n.sim,
                                               calibration.code = calibration.code)
    
    save(rw.control, file = control.filename)
}

fit.rw.simset.chunk <- function(location,
                                chunks,
                                n.sim = 1000,
                                calibration.code = 'final.ehe',
                                verbose = T)
{
    # set up the control
    control.filename = get.rw.control.filename(location = location,
                                               n.sim = n.sim,
                                               calibration.code = calibration.code)
    if (!file.exists(control.filename))
        stop(paste0("No RW control has been set up at ", control.filename))
    
    
    # Load the simset
    simset = retrieve.simulation.set(version = 'ehe', location = location, calibration.code = calibration.code, n.sim = n.sim)
    
    mcmc.settings = NULL
    
    start.time = Sys.time()
    # Run the chunks
    for (chunk in chunks)
    {
        sim.indices = rw.control$sim.indices.for.chunk[[chunk]]
        
        tryCatch({
            
            # Get the MCMC settings
            if (is.null(mcmc.settings))
            {
                all.chunks = 1:rw.control$n.chunks
                all.chunk.files = get.rw.chunk.filenames(location = location,
                                                         chunks = all.chunks,
                                                         n.sim = n.sim,
                                                         calibration.code = calibration.code)
                
                chunks.done.mask = file.exists(all.chunk.files)
                done.chunks = all.chunks[chunks.done.mask]
                
                if (length(done.chunks)==0)
                    mcmc.settings = rw.control$init.mcmc.settings
                else if (any(done.chunks<chunk))
                {
                    pull.from.chunk = max(done.chunks[done.chunks < chunk])
                    load(all.chunk.files[pull.from.chunk])
                }
                else
                {
                    pull.from.chunk = max(done.chunks)
                    load(all.chunk.files[pull.from.chunk])
                }
            }
            
            # Actually Run It
            chunk.sims = list()
            for (i in sim.indices)
            {
                if (i==1)
                {
                    chunk.sims = c(chunk.sims, rw.control$first.sim)
                }
                else
                {
                    mcmc.settings$sim.index = i
                    if (i==1)
                        n.iter.for.i = rw.control$n.iter.first.sim
                    else
                        n.iter.for.i = rw.control$n.iter.subsequent.sims
                    
                    if (verbose)
                        print(paste0("STARTING MCMC FOR SIM ", i, " of ", simset$n.sim))
                    
                    look.back.i.sims.for.parameters = 0
                    successful.first.sim = F
                    max.lookback.attempts = 20
                    while (!successful.first.sim && look.back.i.sims.for.parameters<max.lookback.attempts)
                    {
                        look.back.i.sims.for.parameters = look.back.i.sims.for.parameters + 1
                        look.back.to.sim.i = i - look.back.i.sims.for.parameters
                        if (look.back.i.sims.for.parameters > 1)
                            mcmc.settings$start.values = sim.list[[look.back.to.sim.i]]$params[names(mcmc.settings$start.values)]
                        
                        # Run the first sim and make sure the likelihood evaluates
                        sim = rw.control$transmute.simulation(mcmc.settings$start.values)
                        if (is.null(sim))
                        {
                            if (i!=1)
                            {
                                sim = rw.control$transmute.simulation(default.start.values)
                            }
                            
                            if (is.null(sim))
                            {
                                errored.params <<- mcmc.settings$start.values
                                stop(paste0("The ", get.ordinal(i), " simulation throws an error when computing.\nThe parameters have been saved in the global environment as 'errored.params'"))
                            }
                            
                            mcmc.settings$start.values = rw.control$default.start.values
                            n.iter.for.i = rw.control$n.iter.subsequent.sims.if.restart.params
                        }
                        
                        successful.first.sim = rw.control$likelihood$compute(sim, use.optimized.get=T)!=-Inf
                    }
                    
                    if (!successful.first.sim)
                    {
                        lik.pieces = rw.control$likelihood$compute.piecewise(sim, use.optimized.get=T)
                        errored.likelihood <<- rw.control$likelihood
                        errored.sim <<- sim
                        errored.params <<- mcmc.settings$start.values
                        
                        stop(paste0("The likelihood evaluates to -Inf on the initial parameter values for the ", 
                                    get.ordinal(i), " simulation after 20 attempts. The likelihood components are:\n",
                                    paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                                    "\nThe parameters, simulation, and likelihood have been saved in the global environment as 'errored.params', 'errored.sim', and 'errored.likelihood'"))
                    }
                    
                    # Run the MCMC
                    mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                                          n.iter = n.iter.for.i,
                                                          starting.values = mcmc.settings$start.values,
                                                          cache.frequency = NA,
                                                          update.detail = 'none',
                                                          update.frequency = NA)
                    #update.frequency = ifelse(verbose, 50, NA))
                    
                    if (verbose)
                    {
                        total.minutes = (as.numeric(Sys.time())-as.numeric(start.time)) / 60
                        
                        print(paste0("   DONE. ",
                                     round(total.minutes, 1), " minutes elapsed"))
    
                    }
                
                    update.rw.mcmc.settings(mcmc = mcmc,
                                            mcmc.settings = mcmc.settings,
                                            n.iter = rw.control$n.iter.subsequent.sims)
                
                    chunk.sims = c(chunk.sims, list(mcmc@simulations[[length(mcmc@simulations)]]))
                }
            }
            
            chunk.simset = join.simulation.sets(chunk.sims)
            
            # Save the results
            save(chunk.simset,
                 mcmc.settings,
                 file = get.rw.chunk.filenames(location = location,
                                               chunks = chunk,
                                               n.sim = n.sim,
                                               calibration.code = calibration.code)
            )
        },
        error = function(e){
            
            print(paste0("There was an error fitting rw chunk ", chunk, " for ", location, ":"))
            print(e$message)
            print(paste0("Skipping chunk ", chunk, " and moving on to the next one"))
            
        })
    }
}

assemble.rw.simset <- function(location,
                               chunks = 1:N.CHUNKS,
                               n.sim = 1000,
                               calibration.code = 'final.ehe',
                               allow.incomplete = F,
                               verbose = T)
{
    if (verbose)
        print(paste0("Loading rw simset across ", length(chunks), " chunks"))
    chunk.filenames = get.rw.chunk.filenames(location = location,
                                             chunks = chunks,
                                             n.sim = n.sim,
                                             calibration.code = calibration.code)
    
    sims.list = list()
    for (i in 1:length(chunks))
    {
        if (file.exists(chunk.filenames[i]))
        {
            if (verbose)
                print(paste0("- Loading chunk ", chunks[i]))
            
            load(chunk.filenames[i])
            sims.list = c(sims.list, chunk.simset)
        }
        else if (!allow.incomplete)
            stop(paste0("Cannot assemble.rw.simset() - chunk ", chunks[i], " has not been completed. Use allow.incomplete=T to assemble anyway"))
        else if (verbose)
            print(paste0("Skipping chunk ", chunks[i], " - it has not been done"))
    }
    
    if (verbose)
        print(paste0("All done loading chunks. Rerunning and packaging up"))
    transmuted.simset = join.simulation.sets(sim.list)
    
    rerun.simset = rerun.simulations(transmuted.simset, verbose=verbose)
    
    rerun.simset
}

get.rw.chunk.filenames <- function(location,
                                    chunks,
                                    n.sim,
                                   calibration.code)
{
    dir = get.rw.chunk.dir(location = location, n.sim = n.sim, calibration.code = calibration.code)
    file.path(dir, paste0("chunk", chunks, ".Rdata"))
}

get.rw.chunk.dir <- function(location,
                             n.sim,
                             calibration.code)
{
    file.path(get.jheem.root.directory(),
              'tmp',
              'rw_cache',
              location)
}

get.rw.control.filename <- function(location,
                                n.sim,
                                calibration.code)
{
    dir = get.rw.chunk.dir(location = location, n.sim = n.sim, calibration.code = calibration.code)
    file.path(dir, 'control.Rdata')
}

fit.rw.simset <- function(simset, 
                          n.iter.first.sim=3000, 
                          n.iter.subsequent.sims=300, 
                          n.iter.subsequent.sims.if.restart.params=1000,
                          load.previous = T,
                          verbose=F, track.mcmc=F)
{
    SAVE.DIR = file.path(get.jheem.root.directory(), "tmp")
    if (!dir.exists(SAVE.DIR))
        dir.create(SAVE.DIR, recursive = )
    SAVE.FILE = file.path(SAVE.DIR, paste0(simset$location, "__rw_sims.Rdata"))
    SAVE.FREQUENCY = 50
    
    if (verbose)
        print(paste0("Setting up to fit Ryan White parameters for ", simset$n.sim, " simulations in ", simset$location))
  
    mcmc.settings = new.env(parent = emptyenv())
    mcmc.settings$sim.index = 1
  
    # Set up the transmuter
    if (verbose)
        print("Setting up the JHEEM transmuter...")
    transmuter = create.jheem.transmuter(simset, to.version='rw')
    
    # Set up the 'run' simulation function
    transmute.simulation = function(parameters)
    {
      return(
        transmuter$transmute(sim.index = mcmc.settings$sim.index,
                             parameters = parameters))
        tryCatch({
            transmuter$transmute(sim.index = mcmc.settings$sim.index,
                                 parameters = parameters)
        },
        error = function(e){
              #browser()
              NULL
        })
    }
    
    # Set up RW engine
    # if (verbose)
    #     print("Setting up the Ryan-White engine...")
    # rw.engine = create.jheem.engine('rw', simset$location, simset$to.year)
    
    # Instantiate the likelihood
    
    if (verbose)
        print("Instantiating the likelihood...")
    likelihood = ryan.white.likelihood.instructions$instantiate.likelihood('rw',
                                                                           location=simset$location, 
                                                                           data.manager = RW.DATA.MANAGER)
    
    if (track.mcmc)
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
    mean.p.non.adap = min(.95, mean.p.non.adap)
    
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
        burn = ifelse(track.mcmc, 0, n.iter.first.sim - 1),
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
    
    if (track.mcmc)
        RW.MCMC.CHECKING$mcmc.runs = list()
    
    if (verbose)
        print(paste0("Looping through all ", simset$n.sim, " simulations..."))
    
    start.time = Sys.time()
    start.time.after.first = NULL
    
    sim.list = list()
    if (load.previous && file.exists(SAVE.FILE))
    {
        load(SAVE.FILE) #will overwrite sim.list and mcmc.settings
    }
    
    
#    sim.list = lapply(1:simset$n.sim, function(i){
    for (i in (1+length(sim.list)):simset$n.sim)
    {
        mcmc.settings$sim.index = i
        if (i==1)
            n.iter.for.i = n.iter.first.sim
        else
            n.iter.for.i = n.iter.subsequent.sims
        
        if (verbose)
            print(paste0("STARTING MCMC FOR SIM ", i, " of ", simset$n.sim))
        
        
        look.back.i.sims.for.parameters = 0
        successful.first.sim = F
        max.lookback.attempts = 20
        while (!successful.first.sim && look.back.i.sims.for.parameters<max.lookback.attempts)
        {
            look.back.i.sims.for.parameters = look.back.i.sims.for.parameters + 1
            look.back.to.sim.i = i - look.back.i.sims.for.parameters
            if (look.back.i.sims.for.parameters > 1)
                mcmc.settings$start.values = sim.list[[look.back.to.sim.i]]$params[names(mcmc.settings$start.values)]
            
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
                n.iter.for.i = n.iter.subsequent.sims.if.restart.params
            }
            
           
            successful.first.sim = likelihood$compute(sim, use.optimized.get=T)!=-Inf
        }
        
        if (!successful.first.sim)
        {
            lik.pieces = likelihood$compute.piecewise(sim, use.optimized.get=T)
            errored.likelihood <<- likelihood
            errored.sim <<- sim
            errored.params <<- mcmc.settings$start.values
            
            stop(paste0("The likelihood evaluates to -Inf on the initial parameter values for the ", 
                        get.ordinal(i), " simulation after 20 attempts. The likelihood components are:\n",
                        paste0(paste0(" - ", names(lik.pieces), " = ", lik.pieces), collapse='\n'),
                        "\nThe parameters, simulation, and likelihood have been saved in the global environment as 'errored.params', 'errored.sim', and 'errored.likelihood'"))
        }
        
        # Run the MCMC
        mcmc = bayesian.simulations::run.mcmc(control = mcmc.settings$ctrl,
                                              n.iter = n.iter.for.i,
                                              starting.values = mcmc.settings$start.values,
                                              cache.frequency = NA,
                                              update.detail = 'none',
                                              update.frequency = NA)
                                              #update.frequency = ifelse(verbose, 50, NA))
        
        if (i==1)
          start.time.after.first <<- Sys.time()
        
        if (verbose)
        {
            total.minutes = (as.numeric(Sys.time())-as.numeric(start.time)) / 60
          
            if (i==1)
            {
                seconds.per = (as.numeric(Sys.time()) - as.numeric(start.time)) 
                print(paste0("   DONE. ",
                             round(total.minutes, 1), " minutes elapsed - ",
                             round(seconds.per, 1), " seconds for the first simulation"))
            }
            else
            {
                seconds.per = (as.numeric(Sys.time()) - as.numeric(start.time.after.first)) / i
                print(paste0("   DONE. ",
                             round(total.minutes, 1), " minutes elapsed - ",
                             round(seconds.per, 1), " seconds per simulation (after the first simulation) on average"))          
            }
        }
        
        if (track.mcmc)
            RW.MCMC.CHECKING$mcmc.runs = c(RW.MCMC.CHECKING$mcmc.runs, mcmc)
        
        update.rw.mcmc.settings(mcmc = mcmc,
                                mcmc.settings = mcmc.settings,
                                n.iter = n.iter.subsequent.sims)
        
       # params = mcmc@simulations[[length(mcmc@simulations)]]$params
      #   rw.engine$run(params)
        sim.list[[i]] = mcmc@simulations[[length(mcmc@simulations)]]
        
        if ((i %% SAVE.FREQUENCY)==0)
            save(sim.list, mcmc.settings, file=SAVE.FILE)
    }
    
    # Put the sims back together
    
    if (verbose)
      print(paste0("All done fitting Ryan White parameters for ", simset$n.sim, " simulations in ", simset$location, ". Packaging up"))
    transmuted.simset = join.simulation.sets(sim.list)
    
    rerun.simset = rerun.simulations(transmuted.simset, verbose=verbose)
    
    rerun.simset
}


##-- HELPERS --##

update.rw.mcmc.settings <- function(mcmc,
                                    mcmc.settings,
                                    n.iter,
                                    track.mcmc = F)
{
    new.ctrl = bayesian.simulations::create.adaptive.blockwise.metropolis.control(
      var.names = mcmc.settings$ctrl@var.names,
      simulation.function = mcmc.settings$ctrl@simulation.function,
      log.prior.distribution = mcmc.settings$ctrl@log.prior.distribution,
      log.likelihood = mcmc.settings$ctrl@log.likelihood,
      burn = ifelse(track.mcmc, 0, n.iter - 1),
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