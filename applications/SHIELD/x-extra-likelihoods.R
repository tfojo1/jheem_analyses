
##**Congenital ** ----
#poportion of state level births that are complicated by congenital syphilis 
# 1) using CDC reported state level targets
# 2) estimating MSA level diagnoses from local health department and estimating proportion of MSA level births 

congenital.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                 dimensions = c("age","race"),
                                                 levels.of.stratification = c(0,1),
                                                 outcome.for.p = "proportion.of.congenital.syphilis.births",
                                                 outcome.for.n = "births.denominator.for.congenital.syphilis.proportion",
                                                 sub.location.type = NULL, #if we had county level data
                                                 super.location.type = "STATE",
                                                 main.location.type = "CBSA"
                                                 # main.location.type.p.source = "cdc.aggregated.proportion", #specific source of data that should be used here
                                                 # main.location.type.n.source = "cdc.hiv"
)
# 
SHIELD.DUMMY.PARTITIONING.FUNCTION <- function(arr, version = 'shield', location) {
    return(arr)
}

congenital.nested.likelihood.instructions =
    create.nested.proportion.likelihood.instructions( outcome.for.sim = "proportion.births.congenital",
                                                      outcome.for.data = "proportion.of.congenital.syphilis.births",
                                                      denominator.outcome.for.data = 'population', #'@Ryan: we need better outcome denominator here for fertile women
                                                      #
                                                      location.types = c('STATE',"CBSA"), #CBSA is MSA level
                                                      minimum.geographic.resolution.type = 'COUNTY',
                                                      #
                                                      dimensions = character(),
                                                      levels.of.stratification = c(0),
                                                      from.year = 2008,
                                                      to.year= 2019,
                                                      #
                                                      p.bias.inside.location = 0,
                                                      p.bias.outside.location = congenital.bias.estimates$out.mean, #to be calculated using Todd's code
                                                      p.bias.sd.inside.location = congenital.bias.estimates$out.sd,
                                                      p.bias.sd.outside.location = congenital.bias.estimates$out.sd,
                                                      #                                                     #
                                                      within.location.p.error.correlation = 0.5, #Default: correlation from one year to other in the bias in the city and outside the city
                                                      within.location.n.error.correlation = 0.5, #Default: ratio of births outside MSA to those inside MSA (for MSA we usually dont have fully stratified numbers)
                                                      #
                                                      observation.correlation.form = 'autoregressive.1',
                                                      p.error.variance.term = 0.08617235 , 
                                                      p.error.variance.type = "cv",
                                                      #
                                                      partitioning.function = SHIELD.DUMMY.PARTITIONING.FUNCTION,# we use for unknown outcomes (e.g., number of IDUs by age race in Baltimore) (not needed here)
                                                      #
                                                      weights = (1*DIAGNOSIS.WEIGHT),
                                                      equalize.weight.by.year = T
    )


##** PRENATAL CARE COVERAGE ** ----
# we have 4 Categories representing a multinomial likelihood . 
# for now we are modeling 4 independent likelihoods. This may over penalize deviations from a single bin because we are not accounting for the correlation between the 4 categories.)
#'@Zoe: Error estimate: what proportion of prenatal were unknown at the national level? we can use that to inform this error here 
## source("applications/SHIELD/inputs/input_prenatal_msa_variance.R")
ave.msa.variance= 0.0032 #estimated for all 33 msa combined
prenatal.care.first.trimester.likelihood.instructions =
    create.basic.likelihood.instructions(outcome.for.sim = "prp.prenatal.care.first.trimester",
                                         outcome.for.data = "prenatal.care.initiation.first.trimester",
                                         dimensions = c("age","race"),
                                         levels.of.stratification = c(0,1),
                                         from.year = 2016,
                                         observation.correlation.form = 'autoregressive.1',  
                                         error.variance.term = function(data,details,version, location){
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
                                         observation.correlation.form = 'autoregressive.1',
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
                                         observation.correlation.form = 'autoregressive.1',
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
                                         observation.correlation.form = 'autoregressive.1',
                                         error.variance.term = function(data,details,version, location){
                                             w=SURVEILLANCE.MANAGER$data$completeness.prenatal.care.initiation.first.trimester$estimate$cdc.wonder.natality$cdc.fertility$year__location[,location]
                                             msa.variance=(1-mean(w))^2 * ave.msa.variance
                                             data[is.na(data)]<-0 #'@Andrew: to take out after the update
                                             var= (data* (0.05))^2+msa.variance
                                             return(sqrt(var))
                                         },
                                         weights = PRENATAL.WEIGHT,
                                         error.variance.type = 'function.sd')



## Prenatal care ----
likelihood.instructions.PNC = join.likelihood.instructions(
    prenatal.care.first.trimester.likelihood.instructions,
    prenatal.care.second.trimester.likelihood.instructions,
    prenatal.care.third.trimester.likelihood.instructions,
    no.prenatal.care.likelihood.instructions
)

## Total adult syphilis diagnosis / HIV tests + Historical diagnosis + congenital and PNC----
# lik.inst.diag.totals.no.demog.w.hist.w.cong = join.likelihood.instructions(
#     lik.inst.diag.totals.no.demog.w.historical,
#     congenital.nested.likelihood.instructions,
#     likelihood.instructions.PNC
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
