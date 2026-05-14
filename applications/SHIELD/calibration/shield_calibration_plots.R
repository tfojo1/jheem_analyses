# ****************************************************************************************************
# SHIELD CALIBRATION PLOTS — CALLER SCRIPT ----
# ****************************************************************************************************
# Entry point for generating and saving calibration diagnostic plots.
#
# LOCATION INPUT CONVENTION:
#   All functions accept locations as a named character vector:
#     SHIELD.TEN.MSAS              → all 10 cities
#     SHIELD.TEN.MSAS[1:3]         → first 3 cities
#     SHIELD.TEN.MSAS["Baltimore"] → Baltimore only
#     SHIELD.TEN.MSAS[c("Baltimore", "Atlanta")] → specific cities
#     NULL                         → all available locations
#
# PUBLIC PLOT FUNCTIONS:
#   create_plots_for_calibration()   — per-location stage plots
#   create_multipanel_comparison()   — multi-panel comparison grids
# ****************************************************************************************************

library(plotly)
library(patchwork)
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source('../jheem_analyses/applications/SHIELD/calibration//shield_calibration_helper_functions.R')

# Style managers
location.style.manager <- create.style.manager(color.data.by = "location.type")
source.style.manager   <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
stratum.style.manager  <- create.style.manager(color.data.by = "stratum")


# ****************************************************************************************************
# USAGE EXAMPLES ----
# ****************************************************************************************************
# read the data
calibname <- "calib.5.12.stage2.pk"
calibration.simsets <- load.calibration.simsets(
    locations           = SHIELD.TEN.MSAS,
    calibration.codes   = calibname,
    assemble.incomplete = FALSE
)
# --- Per-city stage plots (all loaded locations) ---
if (1 == 2) {
    stage     <- 2
    
    # Plot ALL loaded locations
    create_plots_for_calibration(
        calibration.code = calibname,
        stage            = stage,
        create.dirs      = TRUE,
    )
    # Plot only Houston and Baltimore
    create_plots_for_calibration(
        calibration.code = calibname,
        stage            = stage,
        locations        = SHIELD.TEN.MSAS[c("Houston", "Baltimore")],
        create.dirs      = TRUE
    )
}


# --- Multi-panel comparison by outcome ---
if (1 == 2) {
    outcomes <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified",
                  "hiv.testing")
    
    # One file per outcome, panels = locations
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,  # or NULL for all available
        outcomes          = outcomes,
        separate.by       = "outcome",
        create.dirs       = TRUE
    )
    # One file per location, panels = outcomes
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE
    )
}

# --- Compare two calibration codes ---
if (1 == 2) {
    calibname1 <- "calib.5.12.stage2.pk"
    calibname2 <- "calib.4.24.stage2.az"
    
    calibration.simsets <- load.calibration.simsets(
        locations           = SHIELD.TEN.MSAS,
        calibration.codes   = c(calibname1, calibname2),
        assemble.incomplete = FALSE
    )
    
    outcomes <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified",
                  "hiv.testing")
    
    # Each panel overlays both calibrations
    create_multipanel_comparison(
        calibration.codes = c(calibname1, calibname2),
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        create.dirs       = TRUE
    )
    
    # ... -split.by sex
    create_multipanel_comparison(
        calibration.codes = c(calibname1, calibname2),
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        split.by = "sex",
        create.dirs       = TRUE
    )
    # .. plot.which to check MSM
    create_multipanel_comparison(
        calibration.codes = c(calibname1, calibname2),
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        split.by = "sex",
        plot.which = "sim.only",
        create.dirs       = TRUE
    )
}

# --- Specific subset with custom options ---
if (1 == 2) {
    
    # Simulation only, custom grid, specific cities
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS[c("Houston", "Miami", "NYC", "LA")],
        outcomes          = c("diagnosis.total", "diagnosis.ps"),
        separate.by       = "outcome",
        plot.which        = "sim.only",
        summary.type      = "mean.and.interval",
        years             = 2010:2025,
        nrow              = 2,
        ncol              = 2,
        create.dirs       = TRUE
    )
}