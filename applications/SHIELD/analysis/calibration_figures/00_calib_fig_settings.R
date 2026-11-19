# ****************************************************************************************************
# 00 CALIBRATION FIGURES: SHARED SETTINGS ----
# ****************************************************************************************************
# Settings, dependencies and local style tweaks for the supplement calibration figures.
# Sourced by every numbered script in this folder. It draws nothing and writes nothing
# (except creating the output folder).
#
# 1. This folder does NOT modify the pretty-plots code used for the paper
#    (analysis/intervention/pretty_plots_*.R, 02_pretty_calibration_plots.R).
#    It only sources those helpers. Style tweaks below are applied to the in-memory copy
#    of SHIELD.OUTCOMES for this session; the style file on disk is unchanged.
# 2. Figures are written to <root>/shield/outputs/<CALIB.NAME>/figures/calibration_supplement/,
#    a subfolder, so the paper figures in figures/ are never overwritten.
# 3. Run from the same working directory as the intervention scripts (paths start with
#    '../jheem_analyses/...').
# ****************************************************************************************************

# ---- DEPENDENCIES (checked up front for all scripts in this folder) ----
# Required by the model code (shield_source_code.R): ggplot2, reshape2, locations, distributions
# Required by the plotting helpers: scales, R6, patchwork
# jheem2 is NOT checked here: shield_source_code.R loads it either as an installed package or
#   from the local ../jheem2 clone, depending on use_jheem2_package_setting.R.
# Optional: ggnewscale (shape.by = "both"), ragg (better PNG text)
local({
    required <- c("ggplot2", "reshape2", "locations", "distributions",
                  "scales", "R6", "patchwork")
    optional <- c("ggnewscale", "ragg")
    missing.req <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
    missing.opt <- optional[!vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing.req) > 0)
        stop("Missing required packages: ", paste(missing.req, collapse = ", "),
             "\nInstall with install.packages(c(", paste0('"', missing.req, '"', collapse = ", "), "))")
    if (length(missing.opt) > 0)
        message("Optional packages not installed (figures still run): ",
                paste(missing.opt, collapse = ", "))
})
library(ggplot2)
library(patchwork)

# ---- SOURCES (read-only use of existing code) ----
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source('../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R')
source('../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/pretty_plots_helper_functions.R')

# ---- SETTINGS ----
# CALIB.NAME: keep the same as 00_settings.R / 02_pretty_calibration_plots.R
# N.SIM:      sims kept by the calibration run (the "-1000" in the simset folder name)
CALIB.NAME <- "calib.10.20.stage4"
N.SIM      <- 1000

CITIES      <- SHIELD.TEN.MSAS
CITY.TITLES <- c(LA = "Los Angeles", NYC = "New York")   # same display names as 02_pretty
GRID.NCOL   <- 5                                         # 10 cities -> 5 x 2 grid
GRID.NROW   <- ceiling(length(CITIES) / GRID.NCOL)
FIG.FORMATS <- c("png")                                  # add "pdf" for submission

# ---- OUTPUT FOLDER ----
print(paste("Root directory is set to: ", get.jheem.root.directory()))
FIG.DIR <- file.path(shield.fig.path(CALIB.NAME, create = TRUE), "calibration_supplement")
if (!dir.exists(FIG.DIR)) dir.create(FIG.DIR, recursive = TRUE)
print(paste("Figures will be written to: ", FIG.DIR))

# ---- LOCAL STYLE TWEAKS (session only; pretty_plots_style.R is not edited) ----
# a) hiv.testing is calibrated to proportion.tested.for.hiv, but the style file labels it
#    as a count ("Tests (n)"). Treat it as a proportion here.
# b) Proportion axis labels are paste0(x * y.mult, "%"). With y.mult = 1 and values on a
#    0-1 scale the axis reads "0.6%". PCT.MULT = 100 gives "60%".
#    Confirmed (Nov 2026): hiv.testing is a proportion and proportions are on a 0-1 scale.
PCT.MULT <- 100
local({
    so <- SHIELD.OUTCOMES
    i  <- match("hiv.testing", so$outcome)
    so$title[i] <- "Proportion tested for HIV"
    so$y.lab[i] <- "Proportion (%)"
    so$type[i]  <- "proportion"
    so$y.zero[i] <- TRUE
    so$y.mult[so$type == "proportion"] <- PCT.MULT
    SHIELD.OUTCOMES <<- so
})
