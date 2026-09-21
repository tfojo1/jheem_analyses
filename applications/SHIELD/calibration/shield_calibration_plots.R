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
# READ DATA ----
# ****************************************************************************************************
if (1==1)
{
    calibname <- "calib.9.19.stage2"
    LOGS <- normalizePath("~/jheem/code/jheem_analyses/applications/SHIELD/logs/")
        #
    calibration.simsets <- load.calibration.simsets(
        locations           = SHIELD.TEN.MSAS,
        calibration.codes   = calibname,
        assemble.incomplete = FALSE,
        # force.reload = T,
        root.dir = LOGS
    )
}
# ****************************************************************************************************
# FULL SET OF PLOTS ----
# ****************************************************************************************************
# --- Per-city stage plots (all loaded locations) ---
if (1 == 2) {
    stage     <- 2
    calibname <- "calib.9.19.stage2"
    SHARED.DRIVE <- normalizePath("~/../../home/jheem-shared/")
    # Plot ALL loaded locations
    create_plots_for_calibration(
        calibration.code = calibname,
        stage            = stage,
        create.dirs      = TRUE,
        root.dir = SHARED.DRIVE
    )
}

SHARED.DRIVE <- normalizePath("~/../../home/jheem-shared/")
# # --- Multi-panel comparison by outcome ---
if (1 == 1) {
    calibname <- "calib.9.19.stage2"
    outcomes <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified",
                  "hiv.testing")

    # One file per outcome, panels = locations
    # create_multipanel_comparison(
    #     calibration.codes = calibname,
    #     locations         = "C.12060",  # or NULL for all available
    #     outcomes          = outcomes,
    #     separate.by       = "outcome",
    #     create.dirs       = TRUE,
    #     years = 1970:2030,
    #     root.dir = SHARED.DRIVE
    # )
    # One file per location, panels = outcomes
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS[1],
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE,
        years = 1970:2030,
        root.dir = SHARED.DRIVE
    )
    create_multipanel_comparison(
        calibration.codes = calibname,
        split.by = "sex",
        locations         = SHIELD.TEN.MSAS[1],
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE,
        years = 1970:2030,
        root.dir = SHARED.DRIVE
    )
    create_multipanel_comparison(
        calibration.codes = calibname,
        split.by = "race",
        locations         = SHIELD.TEN.MSAS[1],
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE,
        years = 1970:2030,
        root.dir = SHARED.DRIVE
    )
    create_multipanel_comparison(
        calibration.codes = calibname,
        split.by = "sex", plot.which = "sim.only",
        locations         = SHIELD.TEN.MSAS[1],
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE,
        years = 1970:2030,
        root.dir = SHARED.DRIVE
    )
    create_multipanel_comparison(
        calibration.codes = calibname,
        split.by = "sex", facet.by = "race",
        locations         = SHIELD.TEN.MSAS[1],
        outcomes          = outcomes,
        separate.by       = "location",
        create.dirs       = TRUE,
        years = 1970:2030,
        root.dir = SHARED.DRIVE
    )
}

# --- Compare two calibration codes ---

# calibration.simsets$`NYC – calib.9.19.stage0`$full_simset$get.mcmc.mixing.statistic()

if (1 == 1) {
    calibname <- c("calib.8.21.stage2.az","calib.9.19.stage2")
    years = 1970:2030
    # READ the outputs:
    # calibration.simsets <- load.calibration.simsets(
    #     locations           = SHIELD.TEN.MSAS,
    #     calibration.codes   = "calib.8.21.stage2.az",
    #     assemble.incomplete = FALSE
    # )
    # 
    outcomes <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified",
                  "hiv.testing")
    
    # Each panel overlays both calibrations
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        years = years,
        separate.by       = "outcome",
        create.dirs       = TRUE,
        root.dir = SHARED.DRIVE
    )
    
    # ... -split.by sex
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        split.by = "sex",
        years = years,
        create.dirs       = TRUE,
        root.dir = SHARED.DRIVE
    )
    # ... -split.by sex
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        split.by = "race",
        years = years,
        create.dirs       = TRUE,
        root.dir = SHARED.DRIVE
    )
    # .. plot.which to check MSM
    create_multipanel_comparison(
        calibration.codes = calibname,
        locations         = SHIELD.TEN.MSAS,
        outcomes          = outcomes,
        separate.by       = "outcome",
        split.by = "sex",
        years = years,
        plot.which = "sim.only",
        create.dirs       = TRUE,
        root.dir = SHARED.DRIVE
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