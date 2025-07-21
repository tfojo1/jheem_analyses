# In the census manager: 
# anywhere that we have a county level data, we should have a national level data with the same stratification and ontology names 
# anywhere that we have a MSA level data, we should have a national level data with the same stratification and ontology names
# we can have more national level estimates if we can pull them directly 
# in the surveillance manager, we only need to have MSA level and national level data

# we should bound agegroups at 85+


# WEIGHTS: The weights are used to weaken the likelihoods for better mixing 
TOTAL.WEIGHT=1/16
EL.DIAGNOSIS.WEIGHT = TOTAL.WEIGHT 
POPULATION.WEIGHT = TOTAL.WEIGHT
DIAGNOSIS.WEIGHT = TOTAL.WEIGHT
TESTING.WEIGHT = TOTAL.WEIGHT
PRENATAL.WEIGHT = TOTAL.WEIGHT


# Population weights: 
# the census runs population count every 10 years, in 2010, and 2020.
# the values reported between these years are extrapolated based on the previous census data.
# so we have more faith in those years and less int he ones between decades
w1=lapply(2010:2019, function(year){
    total.weight = 0.95^(year-2010)
    create.likelihood.weights(total.weight,dimension.values = list(year=year))
})
w2=lapply(2020:2023, function(year){
    total.weight = 0.95^(year-2020)
    create.likelihood.weights(total.weight,dimension.values = list(year=year))
})
w.population <- lapply(c(w1,w2), function(wi) {
    create.likelihood.weights(
        total.weight = POPULATION.WEIGHT * wi$total.weight,
        dimension.values = wi$dimension.values,
        is.recursive = wi$is.recursive
    )
})

# adding more weight to diagnosis data before 2000 and those after 2010
# w.diagnosis=lapply(c(1993:2023),function(year){
#   if(year<2000){w=1
#   }else{ if(year<2010){w=.5
#   }else{ w=1}
#   }
#   total.weight = DIAGNOSIS.WEIGHT * w
#   create.likelihood.weights(total.weight,dimension.values = list(year=year))
#   
# })

#** POPULATION SIZES ** ----
# Basic likelihood: where we have data at the location level desired
# sometimes we dont have the calibration data for the location of interest. 
# so for example we need to calibrate prop aware in Baltimiore to data from MD and building 
# some uncertainty to account for similarities between those locations

# Error variance for population data <From EHE model>
population.error.sd.shield = function(data, details=attr(data, 'details'), version, location)
{
    melted.data = reshape2::melt(data)
    years.since.preceding.census = melted.data$year %% 10
    years.from.nearest.census = pmin(years.since.preceding.census, -melted.data$year %% 10)
    
    inherent.census.cv = 0.015
    
    stratified.dimension.candidates = c('age','race','sex')
    n.stratified.dimensions = length(intersect(names(dim(data)), stratified.dimension.candidates))
    
    if (n.stratified.dimensions<=1)
        max.post.censal.cv = 0.1561269
    else
        max.post.censal.cv = 0.1939618
    
    max.post.censal.var = inherent.census.cv^2 + max.post.censal.cv^2
    
    post.censal.cv = exp(log(inherent.census.cv) + years.since.preceding.census * (0.5*log(max.post.censal.var) - log(inherent.census.cv)) / 9)
    WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL = 4
    intercensal.cv = exp(log(inherent.census.cv) + years.from.nearest.census * (0.5*log(max.post.censal.var / WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL) - log(inherent.census.cv)) / 9)
    
    is.intercensal = grepl('intercensal', details, ignore.case = TRUE) | n.stratified.dimensions==0
    cv = post.censal.cv
    cv[is.intercensal] = intercensal.cv
    
    data * cv
}
population.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.sim = "population",
                                         outcome.for.data = "population", 
                                         na.rm =T,
                                         dimensions = c("age","sex","race"),
                                         levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification (e.g., age), 2 = 2-way stratification (e.g., age race)
                                         from.year = 2010, # the year calibration starts (population size and demographics are fix to 2007)
                                         #measurement error: if census data is off in one year, how much is it off the next year or different strata?
                                         correlation.different.years = 0.5, # this is the default
                                         correlation.different.strata = 0.1, # this is the default
                                         # correlation.different.sources = 0, # default from one source
                                         correlation.same.source.different.details = 0.3, # default: 
                                         
                                         # assumes correlation between all combos of years is the same
                                         observation.correlation.form = 'autoregressive.1', 
                                         
                                         # should always be specified; describes how precise the estimates are; 
                                         # e.g., estimates can be off by 3% each year
                                         error.variance.term = population.error.sd.shield, 
                                         error.variance.type = 'function.sd',
                                         
                                         # downweight because large population size; 
                                         # can get more specific with create.likelihood.weights 
                                         #(e.g., different weight for age X)
                                         weights = w.population,
                                         equalize.weight.by.year = F #if we dont have as many data points in one year it'll be up weighted
                                         #in years that we have more data points we will down weight them
                                         
                                         # if there are more data points for certain years, this will normalize
                                         # e.g., if there are a few years with only the totals 
                                         # before the stratification are available
                                         # equalize.weight.by.year = F Default is TRUE
    )

#** DEATHS **  ----
# CalibTarget: deaths: 2001-2020 by agegroup,sex, race, ethnicity for the US model 
deaths.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "deaths",
                                         outcome.for.data = "deaths", 
                                         levels.of.stratification = c(0), 
                                         from.year = 2010, 
                                         observation.correlation.form = 'compound.symmetry', 
                                         error.variance.term = population.error.sd.shield, #assuming the population level uncertainty
                                         error.variance.type = 'function.sd',
                                         weights = POPULATION.WEIGHT ,
                                         na.rm =T
    )

#** FETILITY RATE **  ----
# CalibTarget: Fertility.rate: 2007-2023 age group race ethnicty 
fertility.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "fertility.rate",
                                         outcome.for.data = "fertility.rate",  
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 2007,  #data available from 2007-2023
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = population.error.sd.shield,  #assuming the population level uncertainty
                                         error.variance.type = 'function.sd',
                                         weights = POPULATION.WEIGHT ,
                                         na.rm =T
    )

#** MIGRATION **  ----
#*#data is available for 6 overlapping 5-year period (2011-2015, 2012-2016, ....)
#*#one way stratifiation (by age, by race, by sex) is only available for 2011-2015
immigration.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.sim = "immigration",
                                         outcome.for.data = "immigration", 
                                         dimensions = c('age','race','sex'),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2011, 
                                         to.year=2020,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary ??? 
                                         #'@Ryan: can you check the error variance? 
                                         error.variance.type = 'cv',
                                         weights = POPULATION.WEIGHT ,
                                         equalize.weight.by.year = T,  #'@Ryan: do we need this? 
                                         na.rm =T
    )

emigration.likelihood.instructions = 
    create.basic.likelihood.instructions( outcome.for.sim = "emigration",
                                          outcome.for.data = "emigration", 
                                          
                                          dimensions = c("age","race"), 
                                          levels.of.stratification = c(0,1,2),
                                          from.year = 2011, 
                                          to.year=2020,
                                          observation.correlation.form = 'compound.symmetry', 
                                          error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary
                                          error.variance.type = 'cv',#'@Ryan: can you check the error variance? 
                                          equalize.weight.by.year = T, #'@Ryan: do we need this? 
                                          weights = POPULATION.WEIGHT ,
                                          na.rm =T
    )


#** SYPHILIS DIAGNOSIS ** ----
##---- Total ----
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total)
total.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.total",
                                         outcome.for.data = "total.syphilis.diagnoses",  
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT ,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1 #
    )
##---- PS ----
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 1994-1999 (cdc.pdf.report) for national model Only (by race, agegroup, sex)
# data from 2000-2023 (cdc.sti) for county; state; national level (total; sex; race; age group; age group+sex; age group + race; race+sex)
# data from 2000-2023 (cdc.sti) for state & national (by race, agegroup, sex)
#
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total, sex)
# data from 1998-2023 for MSA level (cdc.sti) for MSA (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total; sex; race; age group)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (age group+sex; age group + race; race+sex)

ps.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         levels.of.stratification = c(0), 
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1  
    )

ps.diagnosis.total.likelihood.instructions.N =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         levels.of.stratification = c(0), 
                                         from.year = 1993,
                                         to.year = 2022 ,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1  
    )


ps.diagnosis.by.strata.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         dimensions = c("age","race","sex"),
                                         levels.of.stratification = c(0,1,2), 
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1  
    )


source("applications/SHIELD/shield_historical_likelihood_penalty_helper.R")

peak_to_historical_baseline.penalty.instructions <-
    create.custom.likelihood.instructions(
        name = "peak_to_historical_baseline_penalty",
        
        compute.function = function(sim, data, log = T) {
            vals  <- sim$optimized.get(data$get.instr)
            years <- data$years
            
            idx1990  <- which(years == 1990)
            baseline <- vals[idx1990]
            peak     <- max(vals, na.rm = TRUE)
            ratio    <- baseline / peak
            
            # unpack two thresholds and spreads
            min_r   <-  data$min.ratio
            max_r   <-  data$max.ratio  
            σ_low   <- log(2)/2      # tune: how sharply to punish too-flat
            σ_high  <- log(2)/2      # tune: how sharply to punish too-spiky
            
            # piecewise penalty
            if (ratio <= min_r) {
                # too flat: penalize with a lognormal centered at min_r
                μ_low <- log(min_r)
                logp  <- dlnorm(ratio, meanlog = μ_low, sdlog = σ_low, log = TRUE)
                
            } else if (ratio >= max_r) {
                # too spiky: penalize with a lognormal centered at max_r
                μ_high <- log(max_r)
                logp   <- dlnorm(ratio, meanlog = μ_high, sdlog = σ_high, log = TRUE)
                
            } else {
                # “just right”: no penalty
                logp <- 0
            }
            
            if (log) logp else exp(logp)
        },
        get.data.function = function(version, location) {
            sim.meta <- get.simulation.metadata(version = version, location = location)
            
            # instruction to fetch your outcome over time
            get.instr <- sim.meta$prepare.optimized.get.instructions(
                outcome             = "diagnosis.total",
                dimension.values          = list(year = seq(1970, 1990)),
                keep.dimensions           = "year",
                drop.single.sim.dimension = TRUE
            )
            
            # supply the historical vectors and computed params
            list(
                get.instr       = get.instr,
                years           = hist_df$year,
                national.values = hist_df$value,
                max.ratio    = max_ratio_hist,
                min.ratio   = min_ratio_hist,
                sdlog           = sdlog_hist
            )
        }
    )


##---- EARLY ----
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 2000-2023 (cdc.sti) for county; state; national level (total; sex; race; age group; age group+sex; age group + race; race+sex)
# data from 2000-2023 (cdc.sti) for state & national (by race, agegroup, sex)
#
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total)
# data from 1998-2023 for MSA level (cdc.sti) for MSA (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total; sex; race; age group; age group+sex; race+sex; age group+race; age group+race+sex)
#
#MISCLASSIFICATION ERROR 
# we have modeled the misclassification of EL/LL diagnosis in the model and here we only fit to reported (biased) data
early.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = EL.DIAGNOSIS.WEIGHT, # DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

early.diagnosis.total.likelihood.instructions.N =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         to.year = 2022,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = EL.DIAGNOSIS.WEIGHT, # DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

early.diagnosis.by.strata.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         dimensions = c("age","race","sex"),
                                         levels.of.stratification = c(0,1,2,3),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )




##---- Late/Unknown ---- 
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 2000-2023 (cdc.sti) for county; state; national level (total; sex; race; age group; age group+sex; age group + race; race+sex)
# data from 2000-2023 (cdc.sti) for state & national (by race, agegroup, sex)
#
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total)
# data from 1998-2023 for MSA level (cdc.sti) for MSA (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total; sex; race; age group; age group+sex; race+sex; age group+race; age group+race+sex)
late.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

late.diagnosis.total.likelihood.instructions.M =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT*2.42,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

late.diagnosis.total.likelihood.instructions.A =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT*5.10,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

late.diagnosis.total.likelihood.instructions.N =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT*1.73,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

late.diagnosis.by.strata.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         dimensions = c("age","race","sex"),
                                         levels.of.stratification = c(0,1,2,3),
                                         from.year = 1993,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = 0.0764791209420945, #'@Ryan: we need to estimate this 
                                         error.variance.type = 'cv',
                                         weights = DIAGNOSIS.WEIGHT,
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

##** HIV TESTS ** ----
hiv.testing.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "hiv.testing",
                                         outcome.for.data = "proportion.tested.for.hiv", 
                                         # dimensions = c("age","race","sex"),
                                         levels.of.stratification = c(0),
                                         from.year = 2014,
                                         to.year = 2019,
                                         observation.correlation.form = 'compound.symmetry', #short duration of data warrants using the CS
                                         error.variance.term = 0.05,
                                         weights = TESTING.WEIGHT,
                                         error.variance.type = 'cv',
    )


hiv.testing.by.strata.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "hiv.testing",
                                         outcome.for.data = "proportion.tested.for.hiv", 
                                         dimensions = c("age","race","sex"),
                                         levels.of.stratification = c(0,1,2,3),
                                         from.year = 2014,
                                         to.year = 2019,
                                         observation.correlation.form = 'compound.symmetry', #short duration of data warrants using the CS
                                         error.variance.term = 0.05,
                                         weights = TESTING.WEIGHT,
                                         error.variance.type = 'cv',
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

SHIELD.DUMMY.PARTITIONING.FUNCTION <- function(arr, version = 'shield', location) {
    # Intentionally do nothing:
    return(arr)
}

proportion.tested.bias.estimates = get.cached.object.for.version(name = "proportion.tested.bias.estimates", 
                                                                 version = 'ehe')
proportion.tested.nested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested.for.hiv",
                                                     outcome.for.sim = "hiv.testing",
                                                     denominator.outcome.for.data = "adult.population",
                                                     
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     
                                                     #dimensions = c("age","sex"),
                                                     levels.of.stratification = c(0),
                                                     from.year = 2010,
                                                     
                                                     p.bias.inside.location = 0,
                                                     p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                     
                                                     within.location.p.error.correlation = 0.5,
                                                     within.location.n.error.correlation = 0.5,
                                                     
                                                     observation.correlation.form = 'compound.symmetry',
                                                     p.error.variance.term = 0.5,
                                                     p.error.variance.type = 'cv', 
                                                     
                                                     partitioning.function = SHIELD.DUMMY.PARTITIONING.FUNCTION,
                                                     
                                                     weights = (TESTING.WEIGHT),
                                                     equalize.weight.by.year = T
    )

state.HIV.tested.likelihood.instructions =
    create.ifelse.likelihood.instructions(
        hiv.testing.total.likelihood.instructions,
        proportion.tested.nested.likelihood.instructions
    )


##---- Congenital ----
#poportion of state level births that are complicated by congenital syphilis 
# 1) using CDC reported state level targets
# 2) estimating MSA level diagnoses from local health department and estiamteign proporiton of MSA level births 
#'@TODD: to add the nested likelihood for prop of births with congenital 

# congenital.diagnosis.basic.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "congenital.syphilis.diagnoses", #fix type
#                                        outcome.for.sim = "diagnosis.congenital",
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 2010,
#                                        observation.correlation.form = 'autoregressive.1',
#                                        error.variance.term = 0.05,
#                                        error.variance.type = 'cv'
#   )
# 
# 
congenital.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                 dimensions = c("age","race"),
                                                 levels.of.stratification = c(0,1),
                                                 outcome.for.p = "proportion.of.congenital.syphilis.births",
                                                 outcome.for.n = "births.denominator.for.congenital.syphilis.proportion",
                                                 sub.location.type = NULL, #if you had MSA level data
                                                 super.location.type = "STATE",
                                                 main.location.type = "CBSA"
                                                 # main.location.type.p.source = "cdc.aggregated.proportion", #specific source of data that should be used here
                                                 # main.location.type.n.source = "cdc.hiv"
)
# 
SHIELD.DUMMY.PARTITIONING.FUNCTION <- function(arr, version = 'shield', location) {
    # Intentionally do nothing:
    return(arr)
}

congenital.nested.likelihood.instructions.trans =
    create.nested.proportion.likelihood.instructions( outcome.for.sim = "proportion.births.congenital",
                                                      outcome.for.data = "proportion.of.congenital.syphilis.births",
                                                      
                                                      denominator.outcome.for.data = 'population', # evaluate , we need better outcome denominator 
                                                      
                                                      location.types = c('STATE',"CBSA"), #CBSA is MSA level
                                                      minimum.geographic.resolution.type = 'COUNTY',
                                                      
                                                      dimensions = character(),
                                                      levels.of.stratification = c(0),
                                                      from.year = 2008,
                                                      
                                                      p.bias.inside.location = 0,
                                                      p.bias.outside.location = congenital.bias.estimates$out.mean, #to be calculated using Todd's code
                                                      p.bias.sd.inside.location = congenital.bias.estimates$out.sd,
                                                      p.bias.sd.outside.location = congenital.bias.estimates$out.sd,
                                                      
                                                      within.location.p.error.correlation = 0.5, #Default assumption #correlation from one year to other in the bias in the city and outside the city
                                                      within.location.n.error.correlation = 0.5, #Default assumption #ratio of births outside MSA to those inside MSA (for MSA we usually dont have fully stratified numbers)
                                                      #
                                                      
                                                      observation.correlation.form = 'autoregressive.1',
                                                      p.error.variance.term = 0.08617235 , 
                                                      p.error.variance.type = "cv",
                                                      
                                                      partitioning.function = SHIELD.DUMMY.PARTITIONING.FUNCTION,# we use for unknown outcomes (e.g., number of IDUs by age race in Baltimore) (not needed here)
                                                      
                                                      weights = (1*DIAGNOSIS.WEIGHT),
                                                      equalize.weight.by.year = T
    )

#  
#  congenital.diagnosis.likelihood.instructions = 
#      create.location.based.ifelse.likelihood.instructions(
#          congenital.diagnosis.basic.likelihood.instructions,
#          congenital.nested.likelihood.instructions.trans,
#          locations.list = list(c(locations::get.all.for.type("state")))  # anything not in this list will use second instructions
#      )

# cache.object.for.version(object = suppression.bias.estimates,
#                          name = "suppression.bias.estimates",
# version = 'ehe', overwrite=T)


##** PRENATAL CARE COVERAGE ** ----
# we have 4 Categories representing a multinomial likelihood . 
# for now we are modeling 4 independant likelihoods. This may overpenalize deviations from a single bin because we are not accounting for the correlation between the 4 categories.)
# Error estimate: @zoe: what proportion of prenatals were unknown at the national level? we can use that to inform this error here 
## source("applications/SHIELD/inputs/input_prenatal_msa_variance.R")
ave.msa.variance= 0.0032 #estimated for all 33 msa combined
prenatal.care.first.trimester.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "prp.prenatal.care.first.trimester",
                                         outcome.for.data = "prenatal.care.initiation.first.trimester",
                                         
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2016,
                                         observation.correlation.form = 'compound.symmetry', #'@PK: autoregressive.1?
                                         error.variance.term = function(data,details,version, location){
                                             # browser()
                                             w=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,location]
                                             msa.variance=(1-mean(w))^2 * ave.msa.variance
                                             data[is.na(data)]<-0 #'@Andrew: to take out after the update
                                             var= (data* (0.05))^2+msa.variance
                                             return(sqrt(var))
                                         },
                                         weights = PRENATAL.WEIGHT,
                                         error.variance.type = 'function.sd')
prenatal.care.second.trimester.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "prp.prenatal.care.second.trimester",
                                         outcome.for.data = "prenatal.care.initiation.second.trimester",
                                         
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2016,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = function(data,details,version, location){
                                             w=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,location]
                                             msa.variance=(1-mean(w))^2 * ave.msa.variance
                                             data[is.na(data)]<-0 #'@Andrew: to take out after the update
                                             var= (data* (0.05))^2+msa.variance
                                             sd=sqrt(var)
                                             
                                             return(sd)
                                         },
                                         weights = PRENATAL.WEIGHT,
                                         error.variance.type = 'function.sd')

prenatal.care.third.trimester.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "prp.prenatal.care.third.trimester",
                                         outcome.for.data = "prenatal.care.initiation.third.trimester",
                                         
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2016,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = function(data,details,version, location){
                                             w=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,location]
                                             msa.variance=(1-mean(w))^2 * ave.msa.variance
                                             data[is.na(data)]<-0 #'@Andrew: to take out after the update
                                             var= (data* (0.05))^2+msa.variance
                                             return(sqrt(var))
                                         },
                                         weights = PRENATAL.WEIGHT,
                                         error.variance.type = 'function.sd')
no.prenatal.care.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "prp.no.prenatal.care",
                                         outcome.for.data = "no.prenatal.care",
                                         
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2016,
                                         observation.correlation.form = 'compound.symmetry',
                                         error.variance.term = function(data,details,version, location){
                                             w=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,location]
                                             msa.variance=(1-mean(w))^2 * ave.msa.variance
                                             data[is.na(data)]<-0 #'@Andrew: to take out after the update
                                             var= (data* (0.05))^2+msa.variance
                                             return(sqrt(var))
                                         },
                                         weights = PRENATAL.WEIGHT,
                                         error.variance.type = 'function.sd')


likelihood.instructions.PNC = join.likelihood.instructions(
    prenatal.care.first.trimester.likelihood.instructions,
    prenatal.care.second.trimester.likelihood.instructions,
    prenatal.care.third.trimester.likelihood.instructions,
    no.prenatal.care.likelihood.instructions
)


#-- LIKELIHOODS --# ----
likelihood.instructions.demographics=join.likelihood.instructions(
    population.likelihood.instructions,
    deaths.likelihood.instructions,
    fertility.likelihood.instructions,
    immigration.likelihood.instructions,
    emigration.likelihood.instructions)



# Total + strata diagnosis by stage only
likelihood.instructions.syphilis.diag.strata.no.demog=join.likelihood.instructions(
    ps.diagnosis.by.strata.likelihood.instructions,
    early.diagnosis.by.strata.likelihood.instructions,
    late.diagnosis.by.strata.likelihood.instructions,
    hiv.testing.by.strata.likelihood.instructions
)



# penalty 
lik.inst.diag.only.totals.no.demog=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    ps.diagnosis.total.likelihood.instructions,
    early.diagnosis.total.likelihood.instructions,
    late.diagnosis.total.likelihood.instructions,
    hiv.testing.total.likelihood.instructions,
    peak_to_historical_baseline.penalty.instructions
)

# No Penalty
lik.inst.diag.only.totals.no.demog.no.penalty=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    ps.diagnosis.total.likelihood.instructions,
    early.diagnosis.total.likelihood.instructions,
    late.diagnosis.total.likelihood.instructions,
    hiv.testing.total.likelihood.instructions
)

#Congential + Penalty
likelihood.instructions.complete.no.demog = join.likelihood.instructions(
    lik.inst.diag.only.totals.no.demog,
    congenital.nested.likelihood.instructions.trans,
    likelihood.instructions.PNC
)
#Congential + No Penalty
likelihood.instructions.complete.no.demog.no.penalty = join.likelihood.instructions(
    lik.inst.diag.only.totals.no.demog.no.penalty,
    congenital.nested.likelihood.instructions.trans,
    likelihood.instructions.PNC
)

# All total diagnosis+HIV-test+CNS+prenatal care
# lik.inst.diag.by.strata.no.demog=join.likelihood.instructions(
#     total.diagnosis.likelihood.instructions,
#     ps.diagnosis.by.strata.likelihood.instructions,
#     early.diagnosis.by.strata.likelihood.instructions,
#     late.diagnosis.by.strata.likelihood.instructions,
#     hiv.testing.total.likelihood.instructions
#     
# )
# 
# # All total diagnosis+CNS+prenatal care
# lik.inst.diag.by.strata.no.demog=join.likelihood.instructions(
#     total.diagnosis.likelihood.instructions,
#     ps.diagnosis.by.strata.likelihood.instructions,
#     early.diagnosis.by.strata.likelihood.instructions,
#     late.diagnosis.by.strata.likelihood.instructions,
#     
# )


##--OPTIONAL:CNS ----
# cns.diagnosis.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "neurosyphilis", 
#                                        outcome.for.sim = "diagnosis.cns",
#                                        dimensions = c("age","race","sex"),
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 2000,
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.05, 
#                                        error.variance.type = 'cv'
#   )
##--OPTIONAL:Primary ----
# primary.diagnosis.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "primary.syphilis",  
#                                        outcome.for.sim = "diagnosis.primary",
#                                        dimensions = c("age","race","sex"),
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 1941,
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.05, 
#                                        error.variance.type = 'cv')
##--OPTIONAL: Secondary ----
# secondary.diagnosis.likelihood.instructions =
#   create.basic.likelihood.instructions(outcome.for.data = "secondary.syphilis",  
#                                        outcome.for.sim = "secondary.total",
#                                        dimensions = c("age","race","sex"),
#                                        levels.of.stratification = c(0,1,2),
#                                        from.year = 1941,
#                                        observation.correlation.form = 'compound.symmetry',
#                                        error.variance.term = 0.05, 
#                                        error.variance.type = 'cv')


#manual setup: 
# lik=population.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=deaths.likelihood.instructions$instantiate.likelihood('shield',"US")
# lik=fertility.likelihood.instructions$instantiate.likelihood('shield',"US")
# dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)



# estiamting errors for syphilis stages: # Zoe will be estimating this based on discrepency between CDC reported numbers and those reported from local health departments
# ps.syphilis: @Zoe: we have 0,1,2 way interactions for this, but for early.syphilis, we also have the 3 way interaction
# for unknown.duration.or.late.syphilis we only have year__location__age
# Zoe: what about primary.syphilis and secondary.syphilis estimates? 


# lik=ps.diagnosis.likelihood.instructions$instantiate.likelihood('shield',"C.12580")
#dimnames(SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.sti$cdc.sti$year__location__sex)
#this is reported for male/female: no msm 

# can we ssume rates of diagnosis among het-male and female are similar and the rest of new diagnosis among male are msm specific 
# @Zoe: can you please add a mapping for this? male= msm + het_male

# observation.correlation.form 
# 1- 'AUTOREGRESSIVE', 
# Better choice for long time spans (data available > 10 years)
#Correlations decay with time lag:  capturing time-dependent correlation deca
# 2- 'COMPUND SYMMETRY'
# constant correlation between all pairs of observations regardeless of time difference
# Simpler model is preferred (fewer parameters), or the data is sparse.

#better choice for long duration of data, where we dont think that the correlation 