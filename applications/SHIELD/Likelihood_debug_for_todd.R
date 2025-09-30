
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.35620'  # NYC

# --------------------------------------------------------------------
# 1) Load calibration simsets (NYC.26 and NYC.27)
# --------------------------------------------------------------------
{
    simset.NYC.26        <- extract.last.simulation.from.calibration(
        calibration.code = "calib.NYC.09.25.2way.2",
        version = VERSION,
        location = LOCATION,
        include.first.sim = TRUE
    )
    sim.first.NYC.26     <- simset.NYC.26$first.sim()
    sim.last.NYC.26      <- simset.NYC.26$last.sim()
    params.first.NYC.26  <- simset.NYC.26$first.sim()$params
    params.last.NYC.26   <- simset.NYC.26$last.sim()$params
}

{
    simset.NYC.27        <- extract.last.simulation.from.calibration(
        calibration.code = "calib.NYC.09.29.2way.1",
        version = VERSION,
        location = LOCATION,
        include.first.sim = TRUE
    )
    sim.first.NYC.27     <- simset.NYC.27$first.sim()
    sim.last.NYC.27      <- simset.NYC.27$last.sim()
    params.first.NYC.27  <- simset.NYC.27$first.sim()$params
    params.last.NYC.27   <- simset.NYC.27$last.sim()$params
}

# --------------------------------------------------------------------
# 2) Likelihood instruction blocks (one-way & two-way)
# --------------------------------------------------------------------

# --- Primary/Secondary (PS) ---
one.way.sex.ps.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.ps",
        outcome.for.data = "ps.syphilis.diagnoses",
        dimensions = c("sex"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

one.way.sex.current.ps.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.ps",
        outcome.for.data = "ps.syphilis.diagnoses",
        dimensions = c("sex"),
        levels.of.stratification = c(1),
        from.year = 2019,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

one.way.sex.older.ps.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.ps",
        outcome.for.data = "ps.syphilis.diagnoses",
        dimensions = c("sex"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 1999,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

one.way.race.ps.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.ps",
        outcome.for.data = "ps.syphilis.diagnoses",
        dimensions = c("race"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

# --- Early (EL) ---
one.way.sex.el.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.el.misclassified",
        outcome.for.data = "early.syphilis.diagnoses",
        dimensions = c("sex"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

one.way.race.el.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.el.misclassified",
        outcome.for.data = "early.syphilis.diagnoses",
        dimensions = c("race"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

# --- Late (LL) ---
one.way.sex.ll.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.late.misclassified", # late latent misclassified + tertiary + cns
        outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses",
        dimensions = c("sex"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

one.way.race.ll.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.late.misclassified", # late latent misclassified + tertiary + cns
        outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses",
        dimensions = c("race"),
        levels.of.stratification = c(1),
        from.year = 1993,
        to.year = 2022,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

# --- Two-way: Sex × Race ---
.two.way.sex.race.common <- list(dimensions = c("sex", "race"), levels = c(2), from = 1993, to = 2022)

two.way.sex.race.ps.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.ps",
        outcome.for.data = "ps.syphilis.diagnoses",
        dimensions = .two.way.sex.race.common$dimensions,
        levels.of.stratification = .two.way.sex.race.common$levels,
        from.year = .two.way.sex.race.common$from,
        to.year = .two.way.sex.race.common$to,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

two.way.sex.race.el.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.el.misclassified",
        outcome.for.data = "early.syphilis.diagnoses",
        dimensions = .two.way.sex.race.common$dimensions,
        levels.of.stratification = .two.way.sex.race.common$levels,
        from.year = .two.way.sex.race.common$from,
        to.year = .two.way.sex.race.common$to,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )

two.way.sex.race.ll.diagnosis.likelihood.instructions =
    create.basic.likelihood.instructions(
        outcome.for.sim = "diagnosis.late.misclassified", # late latent misclassified + tertiary + cns
        outcome.for.data = "unknown.duration.or.late.syphilis.diagnoses",
        dimensions = .two.way.sex.race.common$dimensions,
        levels.of.stratification = .two.way.sex.race.common$levels,
        from.year = .two.way.sex.race.common$from,
        to.year = .two.way.sex.race.common$to,
        observation.correlation.form = 'autoregressive.1',
        error.variance.term = 0.0764791209420945,
        error.variance.type = 'cv',
        weights = DIAGNOSIS.WEIGHT,
        equalize.weight.by.year = TRUE,
        minimum.error.sd = 1
    )


# --------------------------------------------------------------------
# 3) Instantiate and compare (no stratification + complete)
# --------------------------------------------------------------------
lik.complete = lik.inst.diag.strata.no.demog.w.future$instantiate.likelihood(
    version = VERSION, location = LOCATION
)
lik.complete$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

lik.ps = ps.diagnosis.total.likelihood.instructions$instantiate.likelihood(
    version = VERSION, location = LOCATION
)
lik.el = early.diagnosis.total.likelihood.instructions$instantiate.likelihood(
    version = VERSION, location = LOCATION
)
lik.ll = late.diagnosis.total.likelihood.instructions$instantiate.likelihood(
    version = VERSION, location = LOCATION
)

lik.ps$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

lik.ps$compute(sim.last.NYC.26, debug = TRUE)
lik.ps$compute(sim.last.NYC.27, debug = TRUE)

lik.el$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.ll$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

# --------------------------------------------------------------------
# 4) Instantiate one-way likelihoods and compare
# --------------------------------------------------------------------
lik.sex.ps        = one.way.sex.ps.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.sex.ps.current= one.way.sex.current.ps.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.sex.ps.older  = one.way.sex.older.ps.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)

lik.sex.ps$compute(sim.last.NYC.26, debug = TRUE) 
lik.sex.ps$compute(sim.last.NYC.27, debug = TRUE)


lik.race.ps       = one.way.race.ps.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)


lik.sex.el        = one.way.sex.el.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.race.el       = one.way.race.el.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)

lik.sex.ll        = one.way.sex.ll.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.race.ll       = one.way.race.ll.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)

lik.sex.ps$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.sex.ps.current$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.sex.ps.older$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.race.ps$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

lik.sex.el$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.race.el$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

lik.sex.ll$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.race.ll$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

# --------------------------------------------------------------------
# 5) Instantiate two-way likelihoods and compare
# --------------------------------------------------------------------
lik.sex.race.ps = two.way.sex.race.ps.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.sex.race.el = two.way.sex.race.el.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)
lik.sex.race.ll = two.way.sex.race.ll.diagnosis.likelihood.instructions$instantiate.likelihood(version = VERSION, location = LOCATION)

lik.sex.race.ps$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.sex.race.el$compare.sims(sim.last.NYC.26, sim.last.NYC.27)
lik.sex.race.ll$compare.sims(sim.last.NYC.26, sim.last.NYC.27)

