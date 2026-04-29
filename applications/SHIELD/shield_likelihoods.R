# In the census manager: 
# anywhere that we have a county level data, we should have a national level data with the same stratification and ontology names 
# anywhere that we have a MSA level data, we should have a national level data with the same stratification and ontology names
# we can have more national level estimates if we can pull them directly 
# in the surveillance manager, we only need to have MSA level and national level data
# we should bound agegroups at 85+
source("applications/SHIELD/inputs/input_historical_likelihood_penalty_helper.R")

#estiamting cv for ps diagnosis from 2 available sources:
source("applications/SHIELD/inputs/input_estimate_error_variances.R") #calculates PS_CV

diagnosis_cv=PS_CV #we will use this error variance for all diagnosis categories

# STAGE.0: Demographic calibration + total PS diagnosis
# STAGE.1: Diagnosis (race, sex stratified) +other likelihoods
# STAGE.2: Diagnosis (race, sex, age stratified) +other likelihoods
# WEIGHTS: The weights are used to weaken the likelihoods for better mixing 
#
STAGE.0.WEIGHT= 1/32 # lowered by half on 3/13/2026 
STAGE.1.WEIGHT= 1/8
STAGE.2.WEIGHT= 1/8
STAGE.3.WEIGHT= 1

FUTURE.CHANGE.LIKELIHOOD.WEIGHT = 8 # representing the eight points we would have post 2022 (eight times as many points)
HIV.TESTING.BY.SEX.WEIGHT= 8 #increasing the weight for sex a specific HIV test testing rates because this is the only targets that's available among MSM


#** POPULATION SIZES ** ---- 
#'# Error variance for population data <From EHE model>
population.error.sd.shield = function(data, details=attr(data, 'details'), version, location)
{
    melted.data = reshape2::melt(data)
    years.since.preceding.census = melted.data$year %% 10
    years.from.nearest.census = pmin(years.since.preceding.census, -melted.data$year %% 10)
    # Set up error terms
    inherent.census.cv = 0.015
    
    stratified.dimension.candidates = c('age','race','sex')
    n.stratified.dimensions = length(intersect(names(dim(data)), stratified.dimension.candidates))
    
    if (n.stratified.dimensions<=1)
        max.post.censal.cv = 0.1561269 #from calculating_error_terms_for_ehe_likelihoods.R
    else
        max.post.censal.cv = 0.1939618
    
    max.post.censal.var = inherent.census.cv^2 + max.post.censal.cv^2
    
    post.censal.cv = exp(log(inherent.census.cv) + years.since.preceding.census * (0.5*log(max.post.censal.var) - log(inherent.census.cv)) / 9)
    WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL = 4
    intercensal.cv = exp(log(inherent.census.cv) + years.from.nearest.census * (0.5*log(max.post.censal.var / WEIGHT.TO.INTERCENSAL.VS.POSTCENSAL) - log(inherent.census.cv)) / 9)
    
    # this is a hack now - need to talk to Zoe about how she will formulate the details field
    # I just know that totals are intercensal at this time
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
                                         
                                         # ERROR VARIANCE: describes how precise the estimates are; # e.g., estimates can be off by X% each year
                                         error.variance.type = 'function.sd',
                                         error.variance.term = population.error.sd.shield, 
                                         
                                         #CORRELATION STRUCTURE: if census data is off in one year, how much is it off the next year or different strata?
                                         observation.correlation.form = 'autoregressive.1', # errors in consecutive time periods are more strongly correlated, but this correlation weakens over time
                                         #
                                         correlation.different.strata = 0, # default is 0.1. We added this to address an issue with calibration where racial data didnt comply well
                                         equalize.weight.by.year = F # if set to true: it'll down-weight years with more data points
                                         # to balance with those years where we have fewer data points
    )

#** DEATHS **  ----
#*For county & MSAs: only available Total (2011-2023)
#*For national: also available by age, race/eth, sex (2001-2020)
deaths.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "deaths",
                                         outcome.for.data = "deaths",
                                         dimensions = c("age", "race", "sex"),
                                         levels.of.stratification = c(0, 1, 3), # we have three-way and one-way but not two-way it appears 
                                         from.year = 2010, 
                                         #
                                         error.variance.type = 'function.sd',
                                         error.variance.term = population.error.sd.shield, 
                                         #
                                         observation.correlation.form = 'compound.symmetry', 
                                         #
                                         na.rm =T,
                                         equalize.weight.by.year = T
    )

#** FERTILITY RATE **  ----
# For county; MSA; national data available by age & race (2007-2023)
fertility.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "fertility.rate",
                                         outcome.for.data = "fertility.rate",  
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(2), # only have age-race stratified 
                                         from.year = 2007,  #data available from 2007-2023
                                         #
                                         error.variance.type = 'function.sd',
                                         error.variance.term = population.error.sd.shield,  
                                         #
                                         correlation.different.strata = 0, # to stay consistent with population likelihood
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         na.rm =T
    )

#** MIGRATION **  ----
#* data is available for 6 overlapping 5-year period (2011-2015, 2012-2016, ....)
#* one way stratifiation (by age, by race, by sex) is only available for 2011-2015
immigration.likelihood.instructions = 
    create.basic.likelihood.instructions(outcome.for.sim = "immigration",
                                         outcome.for.data = "immigration", 
                                         dimensions = c('age','race','sex'),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2011, 
                                         to.year=2020,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary #'@PK
                                         #
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         equalize.weight.by.year = T,  #To apply weight evenly over time
                                         na.rm =T
    )

emigration.likelihood.instructions = 
    create.basic.likelihood.instructions( outcome.for.sim = "emigration",
                                          outcome.for.data = "emigration", 
                                          dimensions = c("age","race", "sex"), ### WHY NOT SEX? AND WHY 2 WAY? WE HAVE SAME LEVELS OF DATA AS IMMIGRATION!
                                          levels.of.stratification = c(0,1),
                                          from.year = 2011, 
                                          to.year=2020,
                                          #
                                          error.variance.type = 'cv',
                                          error.variance.term = 0.13, # using MOEs from data - see migration_MOE_summary #'@PK
                                          #
                                          observation.correlation.form = 'compound.symmetry',
                                          #
                                          equalize.weight.by.year = T,  #To apply weight evenly over time 
                                          na.rm =T
    )


#** SYPHILIS DIAGNOSIS ** ----
## Total DIAGNOSIS ----
##---- Historical Penalty ----
# the model starts in 170 but we dont have any local data from 1970 to 1990
# to inform the general range of diagnosis data in this period, we approxiate the general trend in diagnosis at the national level and apply it to locall projections
# this helps us avoid models that project very large or very small number of new diagnosis between 1970-1990 before calibration begins
# >> using national level data on total diagnosis, we estimated the min and max value of the ratio between annual diagnosis relative to the peak in 1990
# for each year between 1970-1990 in the model, we calculate a similar ratio and bound the values to fall within this min/max threshold (to align with national trend)
# if the values fall below min, we penalize the likelihood by dlnorm. 
# if they fall between min and max, they likelihood is 1
# if the value falls over max, we penalize it by dlnorm
historical.diagnosis.likelihood.instructions <-
    create.custom.likelihood.instructions(
        name = "historical.diagnosis.likelihood",
        compute.function = function(sim, data, weights, log = T) {
            vals  <- sim$optimized.get(data$get.instr) #get simulated values
            years <- data$years #get years
            # ratio of total diagnosis from 1970-1989 to diagnosis in 1990:
            idx1990  <- which(years == 1990)
            peak_1990 <- vals[idx1990]
            ratio <- vals[1:(idx1990-1)]/peak_1990
            #
            # unpack two nationa thresholds:
            min_r   <-  data$min.ratio
            max_r   <-  data$max.ratio  
            σ_low   <- log(2)/2      # tune: how sharply to punish below 1/2 1990 val
            σ_high  <- log(2)/2      # tune: how sharply to punish too-spiky
            #
            # piecewise penalty
            logp_annual = 0 #by default
            #
            total.logp = lapply(ratio, function(r){
                # if fall below min threshold: penalize with a lognormal centered at min_r
                if (r  < min_r) {
                    μ_low <- log(min_r)
                    logp_annual  <- dlnorm(r, meanlog = μ_low, sdlog = σ_low/sqrt(weights), log = TRUE)
                }
                # if fall over max threshold:  penalize with a lognormal centered at max_r
                if (r > max_r) {
                    μ_high <- log(max_r)
                    logp_annual   <- dlnorm(r, meanlog = μ_high, sdlog = σ_high/sqrt(weights), log = TRUE)
                }
                #
                logp_annual
            })
            #
            total.logp = sum(unlist(total.logp))
            if (log) total.logp else exp(total.logp)
        },
        get.data.function = function(version, location) {
            sim.meta <- get.simulation.metadata(version = version, location = location)
            #
            # instruction to fetch your outcome over time
            get.instr <- sim.meta$prepare.optimized.get.instructions(
                outcome             = "diagnosis.total",
                dimension.values          = list(year = seq(1970, 1990)),
                keep.dimensions           = "year",
                drop.single.sim.dimension = TRUE
            )
            #
            # supply the historical vectors and computed params
            list(
                get.instr       = get.instr,
                years           = hist_df$year,
                national.values = hist_df$value,
                max.ratio    = max_ratio_hist,
                min.ratio   = min_ratio_hist,
                sdlog           = sdlog_hist
            )
        },
        weights = 1
    )

##---- Overall 1993-2022 ----
total.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.total",
                                         outcome.for.data = "total.syphilis.diagnoses",  
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv', 'sd'),
                                         error.variance.term = list(diagnosis_cv, 10),  #see inputs folder file input_diag_cv_estimates
                                         # total variance = (cv=sigma/mu * observed_n)^2 + sd^2 : this ensures when mu is super small, our variance stays up (at least to sd^2)
                                         #keep us from over penalizing years with small mu (early years)
                                         #
                                         observation.correlation.form = 'autoregressive.1', #long timeframe
                                         #
                                         equalize.weight.by.year = T
                                         # minimum.error.sd = 1 #redundant because we have sd in variance structure 
                                         # if variance <1, it bumps it up to this value
    )
##---- Strata Stage1 2019-2022----
total.diagnosis.by.strata.stage1.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.total",
                                         outcome.for.data = "total.syphilis.diagnoses",
                                         levels.of.stratification = c(1,2), # why 0 again?
                                         dimensions = c("sex","race"),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0, #after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before. 
                                         #
                                         observation.correlation.form = 'compound.symmetry', #short time frame
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )
##---- Strata Stage2 2019-2022 ----
total.diagnosis.by.strata.stage2.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.total",
                                         outcome.for.data = "total.syphilis.diagnoses",  
                                         levels.of.stratification = c(1,2),
                                         dimensions = c("sex","race","age"),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0, #after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before. 
                                         #
                                         observation.correlation.form = 'compound.symmetry',  #short time frame
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1 
    )



## PS DIAGNOSIS ----
##---- Overall 1993-2022 ----
# this is only used in stage 0 calibration (unique weight for stage 0)
ps.diagnosis.stage0.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         levels.of.stratification = c(0), 
                                         from.year = 1993,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv', 'sd'),
                                         error.variance.term = list(diagnosis_cv, 10),  
                                         #
                                         observation.correlation.form = 'autoregressive.1',
                                         #
                                         weights = 4, # changed for calib.3.24.stage0.az
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )


# this is used in stage 1 and stage 2 (different weight than above)
ps.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         levels.of.stratification = c(0), 
                                         from.year = 1993,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv', 'sd'),
                                         error.variance.term = list(diagnosis_cv, 10),  
                                         #
                                         observation.correlation.form = 'autoregressive.1',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

##---- Strata Stage1 2019-2022 ----
ps.diagnosis.by.strata.stage1.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         dimensions = c("sex","race"), 
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,  
                                         #
                                         observation.correlation.form = 'compound.symmetry', #short timeframe
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1  #@Andrew: how does this compare to above? 
    )

##---- Strata Stage2 2019-2022 ----
ps.diagnosis.by.strata.stage2.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.ps", 
                                         outcome.for.data = "ps.syphilis.diagnoses",  
                                         dimensions = c("sex","race","age"),
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv'),
                                         error.variance.term = list(diagnosis_cv),
                                         correlation.different.strata = 0,#after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before.
                                         #
                                         observation.correlation.form = 'compound.symmetry', #short timeframe
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

##---- New Future Penalty ----
# We estimated the 10-year ratio of ps.diagnosis across all MSAs. The log of this ratio was well approximated by a lognormal distribution. 
# We used the mean and standard deviation of log(x) to characterize this distribution. The observed values ranged from 0.3 to 9.4, 
# so we assumed an upper threshold corresponding to a 10-fold increase. Simulations producing values outside this range were penalized accordingly.
# To avoid redundant calculations, this ratio computed only once by comparing simulations in 2030 to 2020 and penalizing sims that fall outside of the 10X increase
future.change.likelihood.instructions =
    create.custom.likelihood.instructions(
        name = "future.change.likelihood",
        compute.function = function(sim, data, weights, log = T) {
            get.instr = data$get.instr
            start_year = data$start_year #2020
            end_year = data$end_year #2030
            meanlog = data$meanlog #0.938
            sdlog = data$sdlog #0.587
            penalty_cutoff = data$penalty_cutoff #10-fold
            
            vals <- sim$optimized.get(get.instr)
            
            # 10-year ratio
            ratio <- vals[as.character(end_year)] / vals[as.character(start_year)]
            # browser()
            lik <- dlnorm(max(ratio, penalty_cutoff),
                          meanlog = meanlog,
                          sdlog = sdlog/sqrt(weights),
                          log=T)
            
            if (log) lik else exp(lik)
        },
        get.data.function = function(version, location) {
            sim.meta <- get.simulation.metadata(version = version, location = location)
            
            start_year  <- 2020L
            end_year    <- 2030L
            
            get.instr <- sim.meta$prepare.optimized.get.instructions(
                outcome                   = "diagnosis.ps",
                dimension.values          = list(year = c(start_year, end_year)),
                keep.dimensions           = "year",
                drop.single.sim.dimension = TRUE
            )
            
            list(
                get.instr   = get.instr,
                start_year  = start_year,
                end_year    = end_year,
                meanlog     = 0.938, # from input_future_change_ten_year_ratio_likelihood
                sdlog       = 0.588,
                penalty_cutoff=10 # penalizing sims falling outside of 10X increase
            )
        },
        weights = FUTURE.CHANGE.LIKELIHOOD.WEIGHT
    )

##---- Proportion of Male Diagnosis among MSM ----
# Penalizing simulations where the proportion of Male diagnosis among MSM falls below a certain threshold  (2018-2022):last 5 years
# We estimated the prop of msm/male at 0.6 from the national data 
# assume a normal distribution centered at 0.6 with sd= 0.05. Penalize sims that fall below 2sd threshold (0.6-2*.05=0.5) according to the normal likelihood
proportion_ps_male_among_msm_likelihood_instructions <-
    create.custom.likelihood.instructions(
        name = "proportion_msm_likelihood",
        compute.function = function(sim, data, weights, log = TRUE, debug = F) {
            if (debug) browser()
            
            get_instr <- data$get_instr
            years <- data$years
            
            vals <- sim$optimized.get(get_instr)
            
            prp_msm <- vals[,"msm"] / rowSums(vals) #proportion of male diagnosis among MSM:
            
            # Because it is possible to have 0 diagnoses in some simulations,
            # we will have to set the ratio to something between 0 and 1.
            # Extreme ends are good so that having any diagnoses at all will
            # improve this likelihood.
            prp_msm[is.na(prp_msm)] <- 1
            
            # Normal band edges; likelihood is constant inside the band
            band_mean <- 0.6 # mean value in 2022 (from national data)
            band_sd <- 0.05 # assuming a 0.05 sd, which puts 2sd band at 0.1
            # the lower threshold is set at 0.6 - 2*.05 = 0.5 (penalizing sims where prop of male diagnosis among MSM is less than 0.5)
            lo <- band_mean - 2 * band_sd
            
            total_log_likelihood <- sum(pmin(dnorm(lo, band_mean, band_sd/sqrt(weights), log=T),
                                             dnorm(prp_msm, band_mean, band_sd/sqrt(weights), log=T)))
            
            if (log) total_log_likelihood else exp(total_log_likelihood)
        },
        get.data.function = function(version, location) {
            sim_metadata <- get.simulation.metadata(version = version, location = location)
            #
            start_year <- 2018L
            end_year <- 2022L
            years <- seq(start_year, end_year)
            #
            get_instr <- sim_metadata$prepare.optimized.get.instructions(
                outcome = "diagnosis.ps",
                dimension.values = list(year = years, sex = c("heterosexual_male", "msm")),
                keep.dimensions = c("year", "sex"),
                drop.single.sim.dimension = TRUE
            )
            #
            list(
                get_instr = get_instr,
                years = years
            )
        },
        weights = 1
    )
## EARLY Diagnosis ----
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 2000-2023 (cdc.sti) for county; state; national level (total; sex; race; age group; age group+sex; age group + race; race+sex)
# data from 2000-2023 (cdc.sti) for state & national (by race, agegroup, sex)
#
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total)
# data from 1998-2023 for MSA level (cdc.sti) for MSA (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total; sex; race; age group; age group+sex; race+sex; age group+race; age group+race+sex)
#
##---- Overall 1993-2022 ----
# we have modeled the misclassification of EL/LL diagnosis in the model and here we only fit to reported (biased) data
early.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv', 'sd'),
                                         error.variance.term = list(diagnosis_cv, 10),  
                                         #
                                         observation.correlation.form = 'autoregressive.1',
                                         #  
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )
##---- Strata Stage1 2019-2022 ----
early.diagnosis.by.strata.stage1.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         dimensions = c("race","sex"),
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0, #after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before. 
                                         #
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )
##---- Strata Stage2 2019-2022 ----
early.diagnosis.by.strata.stage2.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.el.misclassified",
                                         outcome.for.data = "early.syphilis.diagnoses", 
                                         dimensions = c("race","sex","age"),
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0,#after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before.
                                         #
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )


## Late/Unknown Diagnosis---- 
# data from 1941-2022 (cdc.pdf.report) for national model Only (total)
# data from 2000-2023 (cdc.sti) for county; state; national level (total; sex; race; age group; age group+sex; age group + race; race+sex)
# data from 2000-2023 (cdc.sti) for state & national (by race, agegroup, sex)
#
# data from 1993-1999 (cdc.pdf.report) for MSA and national (total)
# data from 1998-2023 for MSA level (cdc.sti) for MSA (total)
# data from 2000-2023 for MSA level (cdc.sti) for MSA (total; sex; race; age group; age group+sex; race+sex; age group+race; age group+race+sex)
##---- Overall 1993-2022 ----
late.diagnosis.total.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         levels.of.stratification = c(0),
                                         from.year = 1993,
                                         to.year = 2022,
                                         #
                                         error.variance.type = c('cv', 'sd'),
                                         error.variance.term = list(diagnosis_cv, 10),  
                                         #
                                         observation.correlation.form = 'autoregressive.1',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )
##---- Strata Stage1 2019-2022 ----
late.diagnosis.by.strata.stage1.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         dimensions = c("race","sex"),
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0, #after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before. 
                                         #
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )
##---- Strata Stage2 2019-2022 ----
late.diagnosis.by.strata.stage2.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "diagnosis.late.misclassified", #late latent misclassified + tertiary+cns
                                         outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses", 
                                         dimensions = c("race","sex","age"),
                                         levels.of.stratification = c(1,2),
                                         from.year = 2019,
                                         to.year = 2022,
                                         #
                                         error.variance.type = 'cv',
                                         error.variance.term = diagnosis_cv,
                                         correlation.different.strata = 0,#after adding age-specific targets in stage2, the model didnt fit to overall targets as well as before.
                                         #
                                         observation.correlation.form = 'compound.symmetry',
                                         #
                                         equalize.weight.by.year = T,
                                         minimum.error.sd = 1
    )

##** PROPORTION TESTED ** ----
## State-level: for situations where MSA level data is not available 
# need to figure out how to write this for MSM and Heterosexual
SHIELD.DUMMY.PARTITIONING.FUNCTION <- function(arr, version = 'shield', location) {
    # Intentionally do nothing:
    return(arr)
}
proportion.tested.bias.estimates = get.cached.object.for.version(name = "proportion.tested.bias.estimates", 
                                                                 version = 'ehe')
SHIELD.PARTITIONING.FUNCTION <- function(arr, version, location)
{
    # We only do anything if:
    #  (a) there is a "sex" dimension,
    #  (b) both "msm" and "heterosexual_male" are present as sex levels, and
    #  (c) the two male slices are IDENTICAL everywhere (i.e., they are a duplicated slab).
    # This matches the EHE pattern: only redistribute when the two male strata are copies.
    if ("sex" %in% names(dim(arr)) &&
        all(c("msm","heterosexual_male") %in% dimnames(arr)$sex) &&
        all(array.access(arr, sex = "msm") == array.access(arr, sex = "heterosexual_male")))
    {
        # ---- Pull metadata needed by the helper that returns MSM proportions ----
        # specification.metadata informs how to shape (age/race) the MSM proportion array.
        specification.metadata <- get.specification.metadata(version = version, location = location)
        
        # ---- Get best-guess MSM proportions for this location ----
        # keep.age/keep.race tell the helper to return proportions stratified to match 'arr'
        # (only if those dimensions exist). 'ages' pins the age ordering to arr's dimnames.
        # The result 'proportion.msm' is typically an array over year/age/race (subset thereof).
        proportion.msm <- get.best.guess.msm.proportions(
            location,
            specification.metadata = specification.metadata,
            keep.age  = any(names(dim(arr)) == "age"),
            keep.race = any(names(dim(arr)) == "race"),
            ages      = dimnames(arr)$age
        )
        
        # ---- Build a partition array over sex = {msm, heterosexual_male} ----
        # Concatenate p(MSM) and 1 - p(MSM), then give it the same non-sex dimnames as
        # 'proportion.msm', plus a two-level 'sex' dimension ordered c("msm","heterosexual_male").
        sex.partition.arr <- c(as.numeric(proportion.msm), 1 - as.numeric(proportion.msm))
        sex.partition.dimnames <- c(dimnames(proportion.msm), list(sex = c("msm", "heterosexual_male")))
        dim(sex.partition.arr)    <- sapply(sex.partition.dimnames, length)
        dimnames(sex.partition.arr) <- sex.partition.dimnames
        
        # ---- Select the portion of 'arr' that aligns with the partition dims ----
        # This pulls the slab of 'arr' whose dimensions match sex.partition.dimnames.
        sex.modified <- array.access(arr, sex.partition.dimnames)
        
        # ---- Apply the partition to split the duplicated male mass ----
        # expand.array broadcasts the partition over any remaining dims in sex.modified.
        # Multiplying implements: new(msm) = total_male * p_msm; new(hetero) = total_male * (1 - p_msm).
        sex.modified <- sex.modified * expand.array(sex.partition.arr, dimnames(sex.modified))
        
        # ---- Write the modified slab back into the original array ----
        array.access(arr, dimnames(sex.modified)) <- sex.modified
    }
    
    # Return the (possibly) modified array. If the condition above didn't hold,
    # we return 'arr' unchanged (again, matching EHE behavior).
    arr
}

##---- Total and by Race ----
proportion.tested.total.by.race.nested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested.for.hiv",
                                                     outcome.for.sim = "hiv.testing",
                                                     denominator.outcome.for.data = "adult.population",
                                                     #
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     #
                                                     dimensions = c("race"),
                                                     levels.of.stratification = c(0,1),
                                                     from.year = 2010,
                                                     to.year = 2019,
                                                     #
                                                     p.bias.inside.location = 0,
                                                     p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                     #
                                                     within.location.p.error.correlation = 0.5, #Default: correlation from one year to other in the bias in the city and outside the city
                                                     within.location.n.error.correlation = 0.5, #Default: ratio of tests outside MSA to those inside MSA (for MSA we usually dont have fully stratified numbers)
                                                     #
                                                     observation.correlation.form = 'compound.symmetry',
                                                     p.error.variance.term = NULL, # this was cv=50% until "calib.3.16.stage1.az"
                                                     p.error.variance.type = "data.variance",
                                                     minimum.error.sd = 0.01, # to fix two Houston points where variance data says 0
                                                     #
                                                     partitioning.function = SHIELD.PARTITIONING.FUNCTION,
                                                     #
                                                     equalize.weight.by.year = T
    )
##---- Total and by Age & Race ----
proportion.tested.total.by.age.race.nested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested.for.hiv",
                                                     outcome.for.sim = "hiv.testing",
                                                     denominator.outcome.for.data = "adult.population",
                                                     #
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     #
                                                     dimensions = c("age", "race"),
                                                     levels.of.stratification = c(0,1),
                                                     from.year = 2010,
                                                     to.year = 2019,
                                                     #
                                                     p.bias.inside.location = 0,
                                                     p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                     #
                                                     within.location.p.error.correlation = 0.5, #Default: correlation from one year to other in the bias in the city and outside the city
                                                     within.location.n.error.correlation = 0.5, #Default: ratio of tests outside MSA to those inside MSA (for MSA we usually dont have fully stratified numbers)
                                                     #
                                                     observation.correlation.form = 'compound.symmetry',
                                                     p.error.variance.term = NULL, # this was cv=50% until "calib.3.16.stage1.az"
                                                     p.error.variance.type = "data.variance",
                                                     minimum.error.sd = 0.01, # to fix two Houston points where variance data says 0
                                                     #
                                                     partitioning.function = SHIELD.PARTITIONING.FUNCTION,
                                                     #
                                                     equalize.weight.by.year = T
    )
##---- By Sex Only ----
proportion.tested.by.sex.nested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions(outcome.for.data = "proportion.tested.for.hiv",
                                                     outcome.for.sim = "hiv.testing",
                                                     denominator.outcome.for.data = "adult.population",
                                                     #
                                                     location.types = c('STATE','CBSA'),
                                                     minimum.geographic.resolution.type = 'COUNTY',
                                                     #
                                                     dimensions = c("sex"),
                                                     levels.of.stratification = c(1),
                                                     from.year = 2010,
                                                     to.year = 2019,
                                                     #
                                                     p.bias.inside.location = 0,
                                                     p.bias.outside.location = proportion.tested.bias.estimates$out.mean,
                                                     p.bias.sd.inside.location = proportion.tested.bias.estimates$out.sd,
                                                     p.bias.sd.outside.location = proportion.tested.bias.estimates$out.sd,
                                                     #
                                                     within.location.p.error.correlation = 0.5, #Default: correlation from one year to other in the bias in the city and outside the city
                                                     within.location.n.error.correlation = 0.5, #Default: ratio of tests outside MSA to those inside MSA (for MSA we usually dont have fully stratified numbers)
                                                     #
                                                     observation.correlation.form = 'compound.symmetry',
                                                     p.error.variance.term = NULL, # this was cv=50% until "calib.3.16.stage1.az"
                                                     p.error.variance.type = "data.variance",
                                                     minimum.error.sd = 0.01, # to fix two Houston points where variance data says 0
                                                     #
                                                     partitioning.function = SHIELD.PARTITIONING.FUNCTION,
                                                     #
                                                     weights = HIV.TESTING.BY.SEX.WEIGHT,
                                                     #
                                                     equalize.weight.by.year = T
    ) 
#-- LIKELIHOODS --# ----
## STAGE0 ----
# popualtion targets+total ps
lik.inst.stage0 =join.likelihood.instructions(
    population.likelihood.instructions,
    deaths.likelihood.instructions,
    fertility.likelihood.instructions,
    immigration.likelihood.instructions,
    emigration.likelihood.instructions,
    ps.diagnosis.stage0.total.likelihood.instructions,
    #
    additional.weights = STAGE.0.WEIGHT
)

## STAGE1 ----- 
### without future trend -----
# stage1 likelihoods without future trends
lik.inst.stage1=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    total.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    ps.diagnosis.total.likelihood.instructions,
    ps.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    early.diagnosis.total.likelihood.instructions,
    early.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    late.diagnosis.total.likelihood.instructions,
    late.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    proportion.tested.total.by.race.nested.likelihood.instructions,
    proportion.tested.by.sex.nested.likelihood.instructions,
    #
    historical.diagnosis.likelihood.instructions,
    proportion_ps_male_among_msm_likelihood_instructions,    # Future change penalty
    future.change.likelihood.instructions,
    #
    additional.weights = STAGE.1.WEIGHT
)

## STAGE2 ----- 
#total syphilis +stage 2 stratas (by age, sex, race)### with future trend -----
lik.inst.stage2=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    total.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    ps.diagnosis.total.likelihood.instructions,
    ps.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    early.diagnosis.total.likelihood.instructions,
    early.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    late.diagnosis.total.likelihood.instructions,
    late.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    proportion.tested.total.by.age.race.nested.likelihood.instructions,
    proportion.tested.by.sex.nested.likelihood.instructions,
    #
    historical.diagnosis.likelihood.instructions,
    proportion_ps_male_among_msm_likelihood_instructions,
    future.change.likelihood.instructions,    # Future change penalty
    #
    additional.weights = STAGE.2.WEIGHT
)
## STAGE3 ----
# STAGE 3 now has demographics split into a separate group
# so that you can set different weights for them if you want.
lik.inst.stg3.demographics=join.likelihood.instructions(
    population.likelihood.instructions,
    deaths.likelihood.instructions,
    fertility.likelihood.instructions,
    immigration.likelihood.instructions,
    emigration.likelihood.instructions,
    #
    additional.weights = STAGE.3.WEIGHT # consider lowering to balance out large datapoints for population
)
lik.inst.stg3.nondemographics=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    total.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    ps.diagnosis.total.likelihood.instructions,
    ps.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    early.diagnosis.total.likelihood.instructions,
    early.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    late.diagnosis.total.likelihood.instructions,
    late.diagnosis.by.strata.stage2.likelihood.instructions,
    #
    proportion.tested.by.strata.stage2.nested.likelihood.instructions,
    #
    historical.diagnosis.likelihood.instructions,
    proportion_ps_male_among_msm_likelihood_instructions,
    future.change.likelihood.instructions,    # Future change penalty
    #
    additional.weights = STAGE.3.WEIGHT
)
lik.inst.stage3 = join.likelihood.instructions(
    lik.inst.stg3.demographics,
    lik.inst.stg3.nondemographics
) 