


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


future.test.likelihood.instructions <- create.custom.likelihood.instructions(name = "cdc.funded.test.ratio",
    get.data.function = function(version, location){
        
        start.years = 2023
        end.years = 2028
        
        keep_dims <- c("year")
        
        sim.metadata = get.simulation.metadata(version=version, location=location)
        
        optimized.get.instr.sim <- sim.metadata$prepare.optimized.get.instructions(
            outcomes         = "cdc.funded.tests",        
            dimension.values = list(year = sort(unique(c(as.integer(start.years), as.integer(end.years))))),
            keep.dimensions  = keep_dims          
        )
        
        SURVEILLANCE.MANAGER = load.data.manager("../../cached/surveillance.manager.rdata")
        
        states = dimnames(SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location)[[2]]
        
        z<- sapply(states,function(st){y = SURVEILLANCE.MANAGER$data$hiv.tests$estimate$cdc.testing$cdc$year__location[as.character(2011:2019),st]
        slope = coefficients(lm(log(y)~as.numeric(names(y))))[2]
        return(exp(slope*5))})
        
        logR5 <- log(unlist(z))
        names(logR5) <- gsub("\\.as\\.numeric\\(names\\(y\\)\\)", "", names(logR5))
        sd_logR5 <- sd(logR5, na.rm = TRUE)
        
        
        data = list(log.mean = unname(logR5[location]), 
                    log.sd = sd_logR5,
                    optimized.get.instr = optimized.get.instr) 
        
        
    },
    compute.function = function(sim,data,log = TRUE){
        location = sim$location
        lower = exp(data$log.mean - 2*data$log.sd)
        upper = exp(data$log.mean + 2*data$log.sd)
        
        #Calculate ratio 
        
        x =sim$get("cdc.funded.tests")
        sim.ratio = x["2028",,]/x["2023",,]
        
        if ((sim.ratio <= upper) && (sim.ratio >= lower)){
            return(dnorm(log(lower),data$log.mean,data$log.sd)*1/sim.ratio)
        }
        else{
            return(dnorm(log(sim.ratio),data$log.mean,data$log.sd)*1/sim.ratio)
        } 
    }
    
)


cdc.prep.joint.likelihood.instructions = join.likelihood.instructions(cdc.test.positivity.likelihood.instructions,cdc.nonhealthcare.tests.likelihood.instructions,cdc.tests.likelihood.instructions,cdc.prep.referred.likelihood.instructions,cdc.prep.eligible.likelihood.instructions,future.test.likelihood.instructions)



