# =============================================================================
# OAKLAND TGA: WHICH LIKELIHOODS HAVE DATA?
# Tries to instantiate each component of a joint likelihood for TGA.OAKLAND and
# reports which work and which fail (e.g. "No data found for any stratifications").
# Run from code/jheem_analyses.
# =============================================================================

# Registers the Oakland calibrations AND applies the in-session data patches
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations_oakland.R')

LOCATION = 'TGA.OAKLAND'

# Components of transmission.oakland.likelihood.instructions
# (see ehe_likelihoods_oakland.R)
TRANS.COMPONENTS = c(
    'race.risk.halfx.cv.expv.new.diagnoses.likelihood.instructions',
    'race.risk.halfx.cv.expv.prevalence.likelihood.instructions',
    'total.new.diagnoses.8x.cv.expv.likelihood.instructions',
    'total.prevalence.8x.cv.expv.likelihood.instructions',
    'population.likelihood.instructions.trans',
    'heroin.likelihood.instructions.trans',
    'cocaine.likelihood.instructions.trans',
    'idu.active.prior.ratio.likelihood.instructions'
)

check.likelihood.components = function(component.names, location = LOCATION)
{
    results = lapply(component.names, function(nm) {
        start = Sys.time()
        msg = tryCatch({
            get(nm)$instantiate.likelihood('ehe', location)
            'OK'
        }, error = function(e) conditionMessage(e))
        data.frame(likelihood = nm,
                   status = ifelse(msg == 'OK', 'OK', 'FAILED'),
                   seconds = round(as.numeric(Sys.time() - start, units = 'secs'), 1),
                   message = ifelse(msg == 'OK', '', gsub('\\s+', ' ', msg)),
                   stringsAsFactors = F)
    })
    do.call(rbind, results)
}

# Components of FULL.oakland.likelihood.instructions (see ehe_likelihoods_oakland.R)
FULL.COMPONENTS = c(
    'population.likelihood.instructions.full',
    'immigration.likelihood.instructions.full',
    'emigration.likelihood.instructions.full',
    'total.new.diagnoses.16x.cv.expv.likelihood.instructions',
    'new.diagnoses.halfx.cv.expv.likelihood.instructions',
    'total.prevalence.16x.cv.expv.likelihood.instructions',
    'prevalence.halfx.cv.and.exp.v.likelihood.instructions',
    'general.mortality.likelihood.instructions.full',
    'proportion.tested.likelihood.instructions',
    'hiv.test.positivity.likelihood.instructions',
    'awareness.likelihood.instructions',
    'suppression.likelihood.instructions',
    'prep.uptake.likelihood.instructions',
    'prep.indications.likelihood.instructions',
    'heroin.likelihood.instructions.full',
    'cocaine.likelihood.instructions.full',
    'idu.active.prior.ratio.likelihood.instructions',
    'number.of.tests.year.on.year.change.likelihood.instructions',
    'gonorrhea.year.on.year.change.likelihood.instructions',
    'ps.syphilis.year.on.year.change.likelihood.instructions',
    'future.new.incidence.change.likelihood.instructions'
)

report.check = function(check, label)
{
    cat("\n=====", label, "=====\n")
    print(check[, c('likelihood', 'status', 'seconds')], row.names = F)
    # Full error messages for the failures
    for (i in which(check$status == 'FAILED'))
        cat("\n--", check$likelihood[i], "--\n", check$message[i], "\n")
}

CHECK.STAGES = c('full')   # any of 'trans', 'full'

if ('trans' %in% CHECK.STAGES)
    report.check(check.likelihood.components(TRANS.COMPONENTS), 'TRANS')
if ('full' %in% CHECK.STAGES)
    report.check(check.likelihood.components(FULL.COMPONENTS), 'FULL')
