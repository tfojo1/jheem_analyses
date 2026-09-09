# extract the data from the interventions

CALIB_CODE <- "calib.8.21.stage3.az"
INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.cov.10   = "coverage 10%",
    doxy.cov.20   = "coverage 20%",
    doxy.cov.30   = "coverage 30%",
    doxy.cov.40   = "coverage 40%",
    doxy.cov.50   = "coverage 50%",
    doxy.cov.60   = "coverage 60%",
    doxy.cov.70   = "coverage 70%",
    doxy.cov.80   = "coverage 80%",
    doxy.cov.90   = "coverage 90%",
    doxy.cov.100  = "coverage 100%"
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)

if (!dir.exists(paste0("Q:/shield/outputs/",CALIB_CODE)))
    dir.create(paste0("Q:/shield/outputs/",CALIB_CODE))

int.simsets <- load.int.simsets(
    SHIELD.TEN.MSAS, INTERVENTION.CODES, CALIB_CODE, 400
)


# Order loc1/int1, loc1/int2, ... loc10/int5, loc10/int6
cc = extract.int.simsets(int.simsets, calib.code = CALIB_CODE)

# AGE (LIMITED DUE TO SIZE; NEEDED JUST FOR DOXY COVERAGE) ----
if (1==2) {
    AGE_OUTCOMES <- c(
        "doxy.coverage" # Needed in order to find totals, because current total-level erroneously includes youngest & oldest ages
    )
    
    dn_age = list(
        year = 2000:2040,
        age = c("0-14 years",  "15-19 years", "20-24 years","25-29 years",
                "30-34 years", "35-39 years", "40-44 years", "45-49 years",
                "50-54 years", "55-64 years", "65+ years"),
        sim = 1:400,
        outcome = AGE_OUTCOMES,
        intervention = INTERVENTION.CODES,
        location = SHIELD.TEN.MSAS
    )
    
    # We'll just keep intervention 2nd to last and location last for simplicity
    age_raw_results <- array(
        # cc is ordered by location followed by intervention
        Reduce(`c`, lapply(cc, function(x) {
            print(paste0("On ", x$location.name, " / ", x$int.label))
            # browser()
            rv = x$full_simset$get(AGE_OUTCOMES,
                                   keep.dimensions = c("year", "location", "age"),
                                   dimension.values = list(year=2000:2040),
                                   drop.single.outcome.dimension=F)
            # Need to put location on the end
            apply(rv, c("year", "age", "sim", "outcome", "location"), function(x) {x})
        })),
        sapply(dn_age, length), dn_age
    )
    
    save(age_raw_results, file = paste0("Q:/shield/outputs/",CALIB_CODE,"/age_raw_results.Rdata"))
}

# TOTAL ----
if (1==2) {
    OUTCOMES = c(
        "diagnosis.el.misclassified",
        "diagnosis.late.misclassified",
        "diagnosis.ps",
        "diagnosis.total",
        "incidence",
        "population",
        "population.msm",
        "prevalence",
        "prop.male.ps.diag.among.msm",
        "sti.screening"
    )
    
    # We want dimensions year - location - sim - outcome - intervention?
    
    dn = list(
        year = 2000:2040,
        sim = 1:400,
        outcome = OUTCOMES,
        intervention = INTERVENTION.CODES,
        location = SHIELD.TEN.MSAS
    )
    
    # We'll just keep intervention 2nd to last and location last for simplicity
    total_raw_results <- array(
        # cc is ordered by location followed by intervention
        Reduce(`c`, lapply(cc, function(x) {
            print(paste0("On ", x$location.name, " / ", x$int.label))
            rv = x$full_simset$get(OUTCOMES,
                                   dimension.values = list(year=2000:2040),drop.single.outcome.dimension=F)
            # Need to put location on the end
            apply(rv, c("year", "sim", "outcome", "location"), function(x) {x})
        })),
        sapply(dn, length), dn
    )
    
    # Add doxy-coverage total, which we have to aggregate from the age-stratified
    # results since the specification doesn't exclude <14 and >65 year olds.
    age_raw_results <- get(load(paste0("Q:/shield/outputs/", CALIB_CODE, "/age_raw_results.Rdata")))
    doxy_total <- apply(
        age_raw_results[,2:10,,,,],
        c("year", "sim","intervention", "location"),
        sum
    )
    total_temp <- apply(total_raw_results, c("year", "sim", "intervention", "location", "outcome"), function(x) {x})
    dn_temp <- dimnames(total_temp)
    dn_temp$outcome <- c(dn_temp[["outcome"]], "doxy.coverage")
    
    total_raw_results <- array(c(total_temp, doxy_total),
                        sapply(dn_temp, length),
                        dn_temp
    )
    
    save(total_raw_results, file = paste0("Q:/shield/outputs/",CALIB_CODE,"/total_raw_results.Rdata"))
}

# SEX ----
if (1==2) {
    SEX_OUTCOMES <- c(
        "diagnosis.total",
        "diagnosis.ps",
        "diagnosis.el.misclassified",
        "diagnosis.late.misclassified",
        # "hiv.testing", # fails for HIV testing because it has different ages than the other outcomes!!
        # only starts at 18-19, no 0-14 or 15-19 like the rest!! Why????
        # I'll have to make the sim$get code smarter, because it assumes shared dimnames
        # between outcomes
        # "doxy.coverage, # this is NOT sex stratified, so we shouldn't use it (breaks the sim$get)
        "incidence",
        "population",
        "prevalence",
        "sti.screening"
    )
    
    dn_sex = list(
        year = 2000:2040,
        sex = c("heterosexual_male", "msm", "female"),
        sim = 1:400,
        outcome = SEX_OUTCOMES,
        intervention = INTERVENTION.CODES,
        location = SHIELD.TEN.MSAS
    )
    
    # We'll just keep intervention 2nd to last and location last for simplicity
    sex_raw_results <- array(
        # cc is ordered by location followed by intervention
        Reduce(`c`, lapply(cc, function(x) {
            print(paste0("On ", x$location.name, " / ", x$int.label))
            # browser()
            rv = x$full_simset$get(SEX_OUTCOMES,
                                   keep.dimensions = c("year", "location", "sex"),
                                   dimension.values = list(year=2000:2040),
                                   drop.single.outcome.dimension=F)
            # Need to put location on the end
            apply(rv, c("year", "sex", "sim", "outcome", "location"), function(x) {x})
        })),
        sapply(dn_sex, length), dn_sex
    )
    save(sex_raw_results, file = paste0("Q:/shield/outputs/",CALIB_CODE,"/sex_raw_results.Rdata"))
}
