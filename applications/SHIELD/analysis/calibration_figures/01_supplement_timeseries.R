# ****************************************************************************************************
# 01 SUPPLEMENT CALIBRATION FIGURES: KEY TARGETS ACROSS THE 10 MSAs ----
# ****************************************************************************************************
# One figure per key calibration target, 10 cities in a 5 x 2 grid, model (median + 95% CrI)
# against surveillance data. First pass of the curated supplement set; a fit-quality heatmap
# covering all targets/strata comes next (02_*).
#
#   S1  Total syphilis diagnoses
#   S2  P&S diagnoses
#   S3  P&S diagnoses by sex
#   S4  P&S diagnoses by race
#   S5  Early latent diagnoses (misclassified)
#   S6  Late latent / unknown-duration diagnoses (misclassified)
#   S7  Proportion tested for HIV
#   S8  Share of male P&S diagnoses among MSM
#
# NOTE: loading 10 cities x N.SIM sims is memory intensive (same load as 02_pretty).
#       For a quick test set N.SIM / CITIES in 00_calib_fig_settings.R to a smaller value
#       only if a thinned simset of that size exists.
# ****************************************************************************************************

source('../jheem_analyses/applications/SHIELD/analysis/calibration_figures/00_calib_fig_settings.R')

# ---- READ DATA ----
calib.simsets <- load.calib.simsets(
    locations         = CITIES,
    calibration.codes = CALIB.NAME,
    n.sim             = N.SIM
)

# ---- FIGURE SPECIFICATIONS ----
# One row per figure. window: a name from SHIELD.TIME in pretty_plots_style.R.
#   EL / late data may start later than 1970 -- switch to "manuscript2000" if the early
#   decades are empty. HIV testing data are recent, so it uses "manuscript2010".
FIG.SPECS <- list(
    list(id = "S1", file = "calib-supp-S1-diagnosis-total",       outcome = "diagnosis.total",
         split.by = NULL,   window = "history",        title = "Total Syphilis Diagnoses"),
    list(id = "S2", file = "calib-supp-S2-diagnosis-ps",          outcome = "diagnosis.ps",
         split.by = NULL,   window = "history",        title = "P&S Syphilis Diagnoses"),
    list(id = "S3", file = "calib-supp-S3-diagnosis-ps-by-sex",   outcome = "diagnosis.ps",
         split.by = "sex",  window = "history",        title = "P&S Syphilis Diagnoses by Sex"),
    list(id = "S4", file = "calib-supp-S4-diagnosis-ps-by-race",  outcome = "diagnosis.ps",
         split.by = "race", window = "history",        title = "P&S Syphilis Diagnoses by Race"),
    list(id = "S5", file = "calib-supp-S5-diagnosis-el",          outcome = "diagnosis.el.misclassified",
         split.by = NULL,   window = "history",        title = "Early Latent Syphilis Diagnoses"),
    list(id = "S6", file = "calib-supp-S6-diagnosis-late",        outcome = "diagnosis.late.misclassified",
         split.by = NULL,   window = "history",        title = "Late Latent / Unknown-Duration Syphilis Diagnoses"),
    list(id = "S7", file = "calib-supp-S7-hiv-testing",           outcome = "hiv.testing",
         split.by = NULL,   window = "manuscript2010", title = "Proportion Tested for HIV"),
    list(id = "S8", file = "calib-supp-S8-prop-ps-msm",           outcome = "prop.male.ps.diag.among.msm",
         split.by = NULL,   window = "history",        title = "Share of Male P&S Diagnoses among MSM",
         shape.by = "both")
)

# ---- PLOT AND SAVE ----
make.supp.fig <- function(spec) {
    args <- list(simsets           = calib.simsets,
                 outcomes          = spec$outcome,
                 locations         = CITIES,
                 overlay.locations = FALSE,
                 titles            = CITY.TITLES,
                 window            = spec$window,
                 log.y             = FALSE,
                 free.y            = TRUE,
                 ncol              = GRID.NCOL,
                 show.sources      = TRUE,
                 show.proj.line    = TRUE,
                 show.data         = TRUE,
                 title             = spec$title)
    if (!is.null(spec$split.by)) args$split.by <- spec$split.by
    if (!is.null(spec$shape.by)) args$shape.by <- spec$shape.by
    do.call(shield.plot, args)
}

supp.figs <- list()
for (spec in FIG.SPECS) {
    message("---- ", spec$id, ": ", spec$title, " ----")
    p <- tryCatch(make.supp.fig(spec),
                  error = function(e) { warning(spec$id, " failed: ", conditionMessage(e)); NULL })
    if (is.null(p)) next
    supp.figs[[spec$id]] <- p
    # nrow is passed so a 5 x 2 grid gets two panel-heights
    save.shield.fig(p, spec$file, dir = FIG.DIR, size = "panel",
                    ncol = GRID.NCOL, nrow = GRID.NROW, formats = FIG.FORMATS)
}

# View interactively, e.g.: supp.figs$S5
