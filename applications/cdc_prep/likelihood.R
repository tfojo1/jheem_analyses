


cdc.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "hiv.tests", 
                                                                         outcome.for.sim = "cdc.funded.tests",
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2010,
                                                                         to.year = 2019,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0,
                                                                         error.variance.term = .005,
                                                                         error.variance.type = c('cv')
)

cdc.nonhealthcare.tests.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "non.healthcare.hiv.tests", 
                                                                         outcome.for.sim = "cdc.funded.tests.nonhealthcare", 
                                                                         dimensions = character(), #total
                                                                         levels.of.stratification = 0,
                                                                         from.year = 2018,
                                                                         to.year = 2021,
                                                                         observation.correlation.form = 'compound.symmetry',
                                                                         correlation.different.years = 0,
                                                                         error.variance.term = .015,
                                                                         error.variance.type = c('sd')
)

cdc.test.positivity.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "cdc.hiv.test.positivity", 
                                                                                   outcome.for.sim = "total.cdc.hiv.test.positivity",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2010,
                                                                                   to.year = 2019,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0,
                                                                                   error.variance.term = .00005,
                                                                                   error.variance.type = c('sd')
)



cdc.prep.referred.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "number.eligible", 
                                                                                   outcome.for.sim = "cumulative.cdc.prep.eligible",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2025,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = .0009, 
                                                                                   error.variance.type = c('cv')
) 



cdc.prep.eligible.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "proportion.referred", 
                                                                                   outcome.for.sim = "cdc.fraction.prep.referred.of.eligible",
                                                                                   dimensions = character(), #total
                                                                                   levels.of.stratification = 0,
                                                                                   from.year = 2020,
                                                                                   to.year = 2022,
                                                                                   observation.correlation.form = 'compound.symmetry',
                                                                                   correlation.different.years = 0.5,
                                                                                   error.variance.term = 0.0845,
                                                                                   error.variance.type = c('sd')
)
 
cdc.prep.ratio.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "5.year.ratio", #target name 
                                                                                 outcome.for.sim = "log.ratio.cdc.funded.hiv.tests.2028.2023", #saved outcome
                                                                                 dimensions = character(), #total
                                                                                 levels.of.stratification = 0,
                                                                                 from.year = 2020,
                                                                                 to.year = 2022,
                                                                                 observation.correlation.form = 'compound.symmetry',
                                                                                 correlation.different.years = 0, #no correlation needed only one value per state 
                                                                                 error.variance.term = 0.44,
                                                                                 error.variance.type = c('sd')
)




#Custom Likelihood Instructions

create.cdc.funded.tests.ratio.likelihood.instructions <- function(outcomes,
                                                                  end.years,
                                                                  year.span,
                                                                  dimensions,
                                                                  log.ratio.mean,
                                                                  log.ratio.sd,
                                                                  ratio.threshold, #what is ratio threshold
                                                                  name,
                                                                  debug = FALSE,
                                                                  outcome.for.sim)
{
    get.data.function <- function(version, location)
    {
        #--- Work out years ---#
        start.years <- end.years - year.span
        all.years   <- union(start.years, end.years)
        
        #--- Optimized get instructions ---#
        sim.metadata <- get.simulation.metadata(version = version, location = location)
        optimized.get.instr <- sim.metadata$prepare.optimized.get.instructions(
            outcomes          = outcomes,
            dimension.values  = list(year = all.years),
            keep.dimensions   = c('year', dimensions)
        )
        
        #--- Indices ---#
        dim.names <- sim.metadata$get.dim.names(
            outcomes         = outcomes,
            dimension.values = list(year = all.years),
            keep.dimensions  = c('year', dimensions)
        )
        
        # indices accept vectors of years; coerce to character to match dimnames
        start.indices <- get.array.access.indices(dim.names, dimension.values = list(year = as.character(start.years)))
        end.indices   <- get.array.access.indices(dim.names, dimension.values = list(year = as.character(end.years)))
        
        #--- Statistical quantities (threshold-mass shortcut) ---#
        # Probability mass at/below threshold and uniform-density proxy on [0, threshold]
        p.lte.threshold    <- stats::plnorm(ratio.threshold, meanlog = log.ratio.mean, sdlog = log.ratio.sd)
        log.d.lte.threshold <- log(p.lte.threshold / ratio.threshold)
        
        #--- For debugging / reshaping ---#
        ratio.dim.names <- dim.names
        ratio.dim.names$year <- end.years
        
        # Package
        list(
            optimized.get.instr   = optimized.get.instr,
            end.indices           = end.indices,
            start.indices         = start.indices,
            ratio.threshold       = ratio.threshold,
            log.ratio.mean        = log.ratio.mean,
            log.ratio.sd          = log.ratio.sd,
            log.d.lte.threshold   = log.d.lte.threshold,
            ratio.dim.names       = ratio.dim.names,
            debug                 = debug
        )
    }
    
    compute.function <- function(sim, data, log = TRUE)
    {
        #--- Pull simulated values ---#
        sim.values <- sim$optimized.get(data$optimized.get.instr)
        
        # Form fold change (end/start)
        num <- sim.values[data$end.indices]
        den <- sim.values[data$start.indices]
        fold.change <- num / den
        
        # (0,0) -> 1 (no change)
        both.zero <- (num == 0 & den == 0)
        
        # If both are +Inf (or both are the same infinite), treat as 1 as well
        both.inf.same <- is.infinite(num) & is.infinite(den) & (sign(num) == sign(den))
        
        fold.change[both.zero | both.inf.same] <- 1
        
        # Replace remaining Inf with a large finite cap (kept for stability; threshold handles mass below)
        fold.change[is.infinite(fold.change)] <- max(100, data$ratio.threshold)
        
        #--- Likelihood with threshold split ---#
        # <= threshold: add log mass proxy
        mask.lte <- (fold.change <= data$ratio.threshold)
        
        # > threshold: evaluate lognormal density directly (requires positive values)
        gt.values <- fold.change[!mask.lte]
        if (length(gt.values)) {
            # Guard: drop non-positive just in case (dlnorm undefined)
            gt.values <- gt.values[gt.values > 0]
            d.gt <- stats::dlnorm(gt.values,
                                  meanlog = data$log.ratio.mean,
                                  sdlog   = data$log.ratio.sd,
                                  log     = TRUE)
            ll.gt <- sum(d.gt)
        } else {
            ll.gt <- 0
        }
        
        ll.lte <- data$log.d.lte.threshold * sum(mask.lte, na.rm = TRUE)
        rv <- ll.gt + ll.lte
        
        if (data$debug) {
            dims <- sapply(data$ratio.dim.names, length)
            if (prod(dims) == length(fold.change)) {
                dim(fold.change) <- dims
                dimnames(fold.change) <- data$ratio.dim.names
                dim(mask.lte) <- dims
                dimnames(mask.lte) <- data$ratio.dim.names
            }
            
            cat("Fold-change examples (finite):\n")
            print(utils::head(fold.change[is.finite(fold.change)], 10))
            
            # Helpful summaries by common dims if present
            group_dims <- intersect(c('sex','race','outcome'), names(data$ratio.dim.names))
            if (length(group_dims)) {
                cat("Counts > threshold by {sex,race,outcome} subsets:\n")
                print(apply(!mask.lte, group_dims, sum))
            }
            yr_dims <- intersect(c('year','outcome'), names(data$ratio.dim.names))
            if (length(yr_dims)) {
                cat("Counts > threshold by {year,outcome} subsets:\n")
                print(apply(!mask.lte, yr_dims, sum))
            }
            
            cat(sprintf("Total log-likelihood = %.3f\n", rv))
        }
        
        if (!log) exp(rv) else rv
    }
    environment(compute.function) <- baseenv()
    
    create.custom.likelihood.instructions(
        name = name,
        compute.function = compute.function,
        get.data.function = get.data.function
    )
}


future.tests.likelihood.instructions <- create.cdc.funded.tests.ratio.likelihood.instructions(
    outcomes        = 'hiv.tests',
    end.years       = 2023:2028,
    year.span       = 1,
    dimensions      = c('age','race','sex','risk'),
    log.ratio.mean  = -0.25, # how to specify multiple log.ratio.means 
    log.ratio.sd    = 0.44,
    ratio.threshold = 2,
    name            = 'future.tests.new.pivot.5y',
    debug           = FALSE
)


cdc.prep.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.nonhealthcare.tests.likelihood.instructions,cdc.tests.likelihood.instructions,cdc.prep.referred.likelihood.instructions,cdc.prep.eligible.likelihood.instructions,cdc.prep.ratio.likelihood.instructions,future.tests.likelihood.instructions)



