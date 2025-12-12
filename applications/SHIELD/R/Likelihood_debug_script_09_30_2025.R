# ====================================================================
# NYC Likelihood Comparison 
# ====================================================================

suppressPackageStartupMessages({
    library(dplyr)
    library(kableExtra)
    # ggplot2 & cowplot used implicitly by simplot/plots
})


source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.35620'  # NYC

set.jheem.root.directory('/Volumes/jheem$')

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
#   
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

# --------------------------------------------------------------------
# 6) Plot helper and figure generation
# --------------------------------------------------------------------
make_scored_plot <- function(lik_obj, sim1, sim2, outcome,
                             split = NULL, facet = NULL,
                             title_prefix = "") {
    score <- round(as.numeric(lik_obj$compare.sims(sim1, sim2)), 3)
    p <- simplot(
        sim1, sim2,
        outcomes = outcome,
        split.by = split,
        facet.by = facet,
        dimension.values = list(year = 1970:2030)
    )
    p + ggtitle(paste0(title_prefix, outcome, "\nLikelihood Comparison: ", score)) +
        theme(plot.title = element_text(hjust = 0.5))
}

# — No Stratification —
p.ps <- make_scored_plot(lik.ps, sim.last.NYC.26, sim.last.NYC.27,
                         outcome = "diagnosis.ps",
                         title_prefix = "No Stratification – ")

p.el <- make_scored_plot(lik.el, sim.last.NYC.26, sim.last.NYC.27,
                         outcome = "diagnosis.el.misclassified",
                         title_prefix = "No Stratification – ")

p.ll <- make_scored_plot(lik.ll, sim.last.NYC.26, sim.last.NYC.27,
                         outcome = "diagnosis.late.misclassified",
                         title_prefix = "No Stratification – ")

# — One-Way (Sex) —
p.sex.ps <- make_scored_plot(lik.sex.ps, sim.last.NYC.26, sim.last.NYC.27,
                             outcome = "diagnosis.ps",
                             facet = "sex",
                             title_prefix = "One-Way (Sex) – ")

p.sex.el <- make_scored_plot(lik.sex.el, sim.last.NYC.26, sim.last.NYC.27,
                             outcome = "diagnosis.el.misclassified",
                             facet = "sex",
                             title_prefix = "One-Way (Sex) – ")

p.sex.ll <- make_scored_plot(lik.sex.ll, sim.last.NYC.26, sim.last.NYC.27,
                             outcome = "diagnosis.late.misclassified",
                             facet = "sex",
                             title_prefix = "One-Way (Sex) – ")

# — One-Way (Race) —
p.race.ps <- make_scored_plot(lik.race.ps, sim.last.NYC.26, sim.last.NYC.27,
                              outcome = "diagnosis.ps",
                              facet = "race",
                              title_prefix = "One-Way (Race) – ")

p.race.el <- make_scored_plot(lik.race.el, sim.last.NYC.26, sim.last.NYC.27,
                              outcome = "diagnosis.el.misclassified",
                              facet = "race",
                              title_prefix = "One-Way (Race) – ")

p.race.ll <- make_scored_plot(lik.race.ll, sim.last.NYC.26, sim.last.NYC.27,
                              outcome = "diagnosis.late.misclassified",
                              facet = "race",
                              title_prefix = "One-Way (Race) – ")

# — Two-Way (Sex × Race) —
p.sex.race.ps <- make_scored_plot(lik.sex.race.ps, sim.last.NYC.26, sim.last.NYC.27,
                                  outcome = "diagnosis.ps",
                                  split = "race", facet = "sex",
                                  title_prefix = "Two-Way (Sex × Race) – ")

p.sex.race.el <- make_scored_plot(lik.sex.race.el, sim.last.NYC.26, sim.last.NYC.27,
                                  outcome = "diagnosis.el.misclassified",
                                  split = "race", facet = "sex",
                                  title_prefix = "Two-Way (Sex × Race) – ")

p.sex.race.ll <- make_scored_plot(lik.sex.race.ll, sim.last.NYC.26, sim.last.NYC.27,
                                  outcome = "diagnosis.late.misclassified",
                                  split = "race", facet = "sex",
                                  title_prefix = "Two-Way (Sex × Race) – ")

# Print individual plots (optional)
# print(p.ps); print(p.el); print(p.ll)
# print(p.sex.ps); print(p.sex.el); print(p.sex.ll)
# print(p.race.ps); print(p.race.el); print(p.race.ll)
# print(p.sex.race.ps)

# Combined grid
cowplot::plot_grid(
    # Row 1: No stratification
    p.ps, p.el, p.ll,
    # Row 2: One-way (Sex)
    p.sex.ps, p.sex.el, p.sex.ll,
    # Row 3: One-way (Race)
    p.race.ps, p.race.el, p.race.ll,
    # Row 4: Two-way (Sex × Race)
    p.sex.race.ps, p.sex.race.el, p.sex.race.ll,
    ncol = 3, labels = "AUTO"
)

# --------------------------------------------------------------------
# 7) Results table 
# --------------------------------------------------------------------
compare_value <- function(lik_obj, sim1, sim2) {
    val <- lik_obj$compare.sims(sim1, sim2)
    val <- suppressWarnings(as.numeric(val))
    return(val)
}

results <- data.frame(
    Stratification = c(
        rep("None", 3),
        rep("Sex", 3),
        rep("Race", 3),
        rep("Sex × Race", 3)
    ),
    Outcome = rep(c("Primary/Secondary", "Early", "Late"), 4),
    Value = c(
        compare_value(lik.ps,            sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.el,            sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.ll,            sim.last.NYC.26, sim.last.NYC.27),
        
        compare_value(lik.sex.ps,        sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.sex.el,        sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.sex.ll,        sim.last.NYC.26, sim.last.NYC.27),
        
        compare_value(lik.race.ps,       sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.race.el,       sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.race.ll,       sim.last.NYC.26, sim.last.NYC.27),
        
        compare_value(lik.sex.race.ps,   sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.sex.race.el,   sim.last.NYC.26, sim.last.NYC.27),
        compare_value(lik.sex.race.ll,   sim.last.NYC.26, sim.last.NYC.27)
    )
)

results %>%
    mutate(ColorValue = cell_spec(
        ifelse(is.na(Value), "NA", sprintf("%.3f", Value)),
        color = "white",
        bold = TRUE,
        background = case_when(
            is.na(Value) ~ "grey",
            Value < 1    ~ "red",
            Value >= 1   ~ "darkgreen"
        )
    )) %>%
    select(Stratification, Outcome, ColorValue) %>%
    kable("html", escape = FALSE, align = "c",
          col.names = c("Stratification", "Outcome", "Compare Sims")) %>%
    kable_styling(full_width = FALSE,
                  bootstrap_options = c("striped", "hover", "condensed"))
