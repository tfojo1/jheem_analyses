
# COVID-19 Impact on HIV Testing and Sexual Transmission — Model Parameters

# Sources:
#   - DiNenno et al. 2022 (PMID 35737573): MMWR, HIV Testing Before/During COVID
#   - Viguerie et al. 2024 (PMID 38181069): AIDS, Excess Missed HIV Diagnoses 2021
#   - Hoover et al. 2022 (DOI 10.15585/mmwr.mm7148a1): MMWR, HIV Services 2019-2021
#   - Patel et al. 2022 (PMID 36094476): JAIDS, CDC-Funded Testing During COVID

 
# get.covid.reduction.in.testing.by.sex.race() ----
# Constructs a model array of the multiplicative reduction in HIV testing
# probability during COVID, stratified by race and sex/risk group.
#
# Data source: Viguerie et al. 2024 (PMID 38181069), Table 1. This paper
# uses NHSS data to estimate the % of expected HIV diagnoses that were
# "missed" in 2020 among PWH who acquired infection pre-2020 (i.e., the
# deficit is attributable to testing disruption, not incidence decline).

# NOTE: Age stratification is NOT applied here; it is handled separately below
get.covid.reduction.in.testing.by.sex.race = function(specification.metadata){
    dim.names = specification.metadata$dim.names[c('age','race','sex')]
    
    # The proportion tested in each stratum is modeled as:
    #   P(tested | COVID) = 1 - proportion of diagnoses missed
    
    # Relative risk of probability of being tested = 1 - Percent of diagnoses missed 
    # Overall testing probability during COVID (all groups pooled)
    total.p = 0.817  # 1 - 0.183 missed
    
    # --- Race multipliers (relative to White) ---
    # Baseline (reference group): White race, MSM transmission category
    #   White: missed 15.8%  → P(tested) = 0.842  [reference]
    #   Black vs. White:    missed 16.4% → P = 0.836 → RR = 0.836/0.842 = 0.993
    #   Hispanic vs. White: missed 22.4% → P = 0.776 → RR = 0.776/0.842 = 0.922
    black.rr = 0.836/0.842  
    hispanic.rr = 0.776/0.842 
    
   # --- Sex/risk multipliers (relative to MSM) ---
    #   White: missed 16%  → P(tested) = 0.84  [reference]
    #   Het female vs. MSM: missed 23.2% → P = 0.768 → RR = 0.768/0.840 = 0.914
    het.female.rr = 0.768 / 0.84   
    
    # Heterosexual male:   derived by subtraction from all-male and MSM counts
    het.male.num   = 2400 - 1900       # missed diagnoses: all male - MSM
    het.male.denom = 14200 - 11850     # expected diagnoses: all male - MSM
    het.male       = het.male.num / het.male.denom   # = 0.213
    #   Het male vs. MSM: missed 21% → P = 0.79 → RR = 0.79/0.840 = 0.94
    het.male.rr    = 0.79/0.84              
     
    # Initialize array to overall testing probability
    # The array is initialized to total.p (0.817), then race and sex/risk
    # multipliers are applied multiplicatively to the relevant strata.
    rv = array(total.p,
               dim=sapply(dim.names, length),
               dimnames = dim.names)
    # Apply race multipliers
    rv[, "black", ]    = rv[, "black", ]    * black.rr
    rv[, "hispanic", ] = rv[, "hispanic", ] * hispanic.rr
    
    # Apply sex/risk multipliers
    rv[, , "heterosexual_male"] = rv[, , "heterosexual_male"] * het.male.rr
    rv[, , "female"]            = rv[, , "female"]            * het.female.rr
    
    rv
}

# get.q2.covid.reduction() ----
# Returns a scalar that represents how much deeper the Q2 2020 testing nadir
# was relative to the full-year 2020 average reduction.
#
# Data source: Hoover et al. 2022 (DOI 10.15585/mmwr.mm7148a1), Table 1.
# Quarterly HIV diagnosis counts from NHSS (LabCorp + Quest Diagnostics):
#
#   2019: Q1=9,488  Q2=9,431  Q3=9,164  Q4=8,392  → Total=36,475
#   2020: Q1=8,438  Q2=6,228  Q3=7,905  Q4=7,758  → Total=30,329
#
# Full-year ratio:  30,329 / 36,475 = 0.831  (i.e., ~83% of 2019 diagnoses)
# Q2 ratio:          6,228 /  9,431 = 0.660  (i.e., ~66% of 2019 Q2 diagnoses)
#
# The function returns: Q2 ratio / full-year ratio = 0.660 / 0.831 ≈ 0.794
#
# Interpretation: Q2 2020 was ~79.4% as severe as the full-year average,
# meaning Q2 had about 20.6 percentage points more reduction relative to
# the annual mean. This scalar is used as a multiplier in
# get.q2.full.stratified.covid.reduction.in.testing() to scale the annual
# testing reduction down to its Q2 nadir.
#
# NOTE: The code comments show equivalent calculations using raw test counts
# rather than diagnoses; the diagnosis-based version is what is implemented.
# Both approaches yield similar results (0.734 vs 0.794).
get.q2.covid.reduction = function(){
    # the reductions in HIV test and diagnosis are similar; we used the diagnosis below 
    # Using number of tests (Table1)
    # tests.2019 = sum(2101633,2523317,2572963,2451303)
    # tests.2020 = sum(2471614,1682578,2325554,2274593)
    # 
    # overall.reducton.tests = tests.2020/tests.2019 #0.9
    # 
    # q2.2019.tests = 2523317
    # q2.2020.tests = 1682578
    # 
    # q2.reduction.tests = q2.2020.tests/q2.2019.tests #0.66
    # q2.to.full.year.tests = q2.reduction.tests/overall.reducton.tests # 0.734 (reductions in HIV test in Q2 relative to full year)
    # 
    # Using diagnosed HIV 
    diagnosed.2019 = sum(9488,9431,9164,8392)
    diagnosed.2020 = sum(8438,6228,7905,7758)
    
    overall.reduction.diagnosed = diagnosed.2020/diagnosed.2019 #.83
    
    q2.2019.diagnosed = 9431
    q2.2020.diagnosed = 6228
    
    q2.reduction.diagnosed = q2.2020.diagnosed/q2.2019.diagnosed #0.66
    
    q2.to.full.year.diagnosed = q2.reduction.diagnosed/overall.reduction.diagnosed # 0.794 (reductions in HIV diagnosis in Q2 relative to full year)
    
    q2.to.full.year.diagnosed # multiply this by overall reduction in previous function to get max reduction in q2
}

# get.covid.reduction.in.testing.by.age() ----
# Data source: Patel et al. 2022 (PMID 36094476), JAIDS. Uses CDC-funded health
# department testing data (NHM&E system), comparing 2019 and 2020 counts across

# Constructs age-specific multipliers for the COVID testing reduction, expressed
# relative to the overall (all-ages) reduction. These multipliers are then
# combined with the race/sex multipliers from get.covid.reduction.in.testing.by.sex.race().
# six age bands:
#
#   Age band  | 2019 tests | 2020 tests | Ratio (2020/2019)
#   <13       |      4,589 |      3,035 | 0.661
#   13-19     |    152,741 |     73,039 | 0.478
#   20-29     |    836,193 |    441,721 | 0.528
#   30-39     |    589,461 |    323,769 | 0.549
#   40-49     |    326,494 |    178,929 | 0.548
#   50+       |    394,254 |    218,445 | 0.554
#
#   Overall:  2,324,421 → 1,255,895 → ratio = 0.540
#
# Age-specific multipliers = (age ratio) / (overall ratio), which captures
# the differential effect of COVID by age relative to the mean:
#   13-19 experienced the sharpest relative decline (~0.478/0.540 = 0.885)
#   50+   experienced a less extreme relative decline (~0.554/0.540 = 1.026)
#
# The raw age bands are then mapped to the model's age strata via
# map.age.values(), which handles any mismatch between the data age groupings
# and the model age structure.
get.covid.reduction.in.testing.by.age = function(specification.metadata){
    tests.2019 = 2324421
    tests.2020 = 1255895
    
    overall.reduction = tests.2020/tests.2019
    
    age.2019 = c(4589,152741,836193,589461,326494,394254)
    age.2020 = c(3035,73039,441721,323769,178929,218445)
    
    names(age.2019) = names(age.2020) = c("<13","13-19","20-29","30-39","40-49","50+")
    
    age.reduction = age.2020/age.2019
    
    age.to.all.ages.reduction = age.reduction/overall.reduction
    
    model.ages = specification.metadata$dim.names[c('age')]$age
    age.info = parse.age.strata.names(names(age.to.all.ages.reduction))
    
    age.to.all.ages.reduction.mapped = map.age.values(values = age.to.all.ages.reduction,
                                                      given.age.lowers = age.info$lower,
                                                      given.age.uppers = age.info$upper, 
                                                      desired.ages = model.ages)
    
    
    age.to.all.ages.reduction.mapped
}


# get.q2.full.stratified.covid.reduction.in.testing() ----
# Master function that combines all COVID testing reduction components into a
# single fully stratified array representing the multiplicative reduction in
# HIV testing probability at the Q2 2020 nadir.
#
# It multiplies three components together element-wise:
#
#   1. get.covid.reduction.in.testing.by.sex.race():  race × sex/risk stratified annual
#      reduction in testing probability (from Viguerie 2024, PMID 38181069)
#
#   2. get.covid.reduction.in.testing.by.sex.race():         age-specific multipliers relative to
#      the overall age-pooled reduction (from Patel 2022, PMID 36094476)
#
#   3. get.q2.covid.reduction():          scalar (~0.794) that scales the annual
#      average reduction to the Q2 2020 nadir (from Hoover 2022,
#      DOI 10.15585/mmwr.mm7148a1)
#
# The result is an array indexed by [age × race × sex] giving the probability
# of being tested for HIV at the peak COVID disruption (Q2 2020), relative to
# pre-COVID baseline. Values below 1.0 indicate testing suppression; values
# closer to 0 indicate more severe disruption.
#
# This array is intended to be applied as a time-varying multiplier on the
# model's baseline testing rates during the COVID period. The calling code
# (commented out) passes it to max.covid.effect.testing.reduction.
get.q2.full.stratified.covid.reduction.in.testing = function(specification.metadata){
    
    testing.no.age = get.covid.reduction.in.testing.by.sex.race(specification.metadata = specification.metadata)
    
    testing.age.only = get.covid.reduction.in.testing.by.age(specification.metadata = specification.metadata)
    
    rv = testing.no.age*testing.age.only
    
    rv = rv*get.q2.covid.reduction()
    
    rv
}



# max.covid.effect.testing.reduction = get.q2.full.stratified.covid.reduction.in.testing(
#   specification.metadata = specification.metadata)

# max.covid.effect.sexual.transmission.reduction = get.covid.reduction.in.sexual.transmission(
#   specification.metadata = specification.metadata)




if(1==2){
    source('applications/SHIELD/shield_specification.R')
    specification.metadata = get.specification.metadata('shield', 
                                                        location = 'c.12580')
    get.q2.full.stratified.covid.reduction.in.testing(specification.metadata)
}
# additional source not used so far: 
# HIV Testing Before and During the COVID-19 Pandemic — United States, 2019–2020
# DiNenno et al. 2022 (PMID 35737573) 
# Compares data from 4 CDC national surveillance systems (NHSS, FHAPS, NCHSTP data, BRFSS)
# From 2019 to 2020, new HIV diagnoses reported to CDC decreased by 17%
# weekly chart suggests: starting week 9: testing fell by 50% reching lowest to week 12, then ramped up to 80% in week 39 and stayed there

# In 2020, the total number of HIV tests funded by CDC that were distributed in health care and non–health care settings decreased by nearly one half (42.6% and 49.5%, respectively) 
# % change in HIV tests 2019-2020 (Fig2)
# Black=44.1%
# Hispanic= 46.3%
# White= 45%
# MSM= 49.2%
# HEt women= 50%
# Het men = 55%


## COVID impact on sexual transmission: McKay et al: 
# https://link.springer.com/article/10.1007/s13178-021-00625-3
# Using Table 3, averaged stay-at-home order estimates for: 
# "stopped having sex with casual partners" (0.178)
# "had fewer sexual partners compared to Feb/early March" (0.393)

# # pass a functional form with just this value - don't need to make an array here; functional form will do it 
# get.covid.reduction.in.sexual.transmission = function(specification.metadata){
#     dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
#     
#     stopped.sex.with.casual.partners = 0.178
#     fewer.sexual.partners = 0.393
#     
#     avg.reduction = mean(c(stopped.sex.with.casual.partners,fewer.sexual.partners))
#     
#     rv = array(1-avg.reduction,
#                dim=sapply(dim.names, length),
#                dimnames = dim.names)
#     
#     rv
# }
