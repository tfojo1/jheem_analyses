# ****************************************************************************************************
# 00 SETTINGS: SEATTLE VS KING COUNTY ----
# ****************************************************************************************************
# Shared by the two scripts in this folder. Kept SEPARATE from ../00_settings.R on purpose: the
# King County-aligned scenarios (doxy.kc.*) never enter the main result arrays (total_raw, sex_raw),
# the `intervention.results` object, or the main tables/ and figures/ folders.
#
# RUN ORDER (from the jheem_analyses folder; every source() path is relative to it)
#   00_settings_seattle_kc.R        sourced by the scripts below; never run on its own
#   01_extract_seattle_kc.R         Seattle simsets -> one small results file (seattle.kc.results)
#                                   Loads one simset at a time (memory ~ 1 simset)
#   02_seattle_vs_king_county.R     tables + figure comparing SHIELD Seattle with Menza et al. 2026
#
# WHERE THINGS ARE WRITTEN (never inside the main output folders)
#   <root>/shield/outputs/<CALIB.NAME>/seattle_vs_king_county/
#       extracts/                     one file per simset (step 01)
#       seattle_kc_raw_results.Rdata  stacked arrays (step 01)
#       tables/  figures/             step 02
#
# Superseded: 06_seattle_vs_king_county.R in this folder read the main arrays; kept for reference.
# ****************************************************************************************************


## Libraries: every package steps 01 and 02 need, checked before anything runs ----
REQUIRED.PACKAGES <- c("dplyr", "tibble", "tidyr", "ggplot2", "patchwork", "parallel")
missing.pkgs <- REQUIRED.PACKAGES[!vapply(REQUIRED.PACKAGES, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing.pkgs) > 0)
  stop("Install these packages before running: ", paste(missing.pkgs, collapse = ", "))
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R')


## Calibration and location ----
CALIB.NAME <- "calib.10.20.stage4"
N.SIM      <- 1000
CITY       <- "Seattle"
LOC.CODE   <- "C.42660"


## Scenarios ----
# Defined in intervention/intervention_definitions_KC.R (start Jan 2023 at 0, 15% at mid-2023,
# 30% or 45% at May 2024, then flat or rising to 50% by the end of 2025)
NOINT    <- "noint"
KC.CODES <- c("doxy.kc.30.flat", "doxy.kc.30.to50", "doxy.kc.45.flat", "doxy.kc.45.to50")
SCENARIO.LABELS <- c(
  noint            = "No doxy-PEP",
  doxy.kc.30.flat  = "30% May 2024, flat",
  doxy.kc.30.to50  = "30% May 2024, 50% end 2025",
  doxy.kc.45.flat  = "45% May 2024, flat",
  doxy.kc.45.to50  = "45% May 2024, 50% end 2025"
)

# Optional: also extract Seattle's 2030-target scenarios (doxy.cov.5-50) into THIS folder's results,
# to show them side by side. Adds 10 simset loads. They stay out of the main arrays either way.
INCLUDE.COV.SCENARIOS <- FALSE
COV.CODES <- paste0("doxy.cov.", seq(5, 50, 5))
if (INCLUDE.COV.SCENARIOS)
  SCENARIO.LABELS <- c(SCENARIO.LABELS,
                       setNames(paste0(seq(5, 50, 5), "% by 2030 (linear)"), COV.CODES))

# Scenarios analysed: the two FLAT scenarios only. The *.to50 variants differ from them by <1.5
# percentage points in the comparison window, so they are not carried through (they stay in KC.CODES
# and on disk; put them back here to include them).
FOCUS.CODES        <- c("doxy.kc.30.flat", "doxy.kc.45.flat")
SCENARIOS          <- c(FOCUS.CODES, if (INCLUDE.COV.SCENARIOS) COV.CODES)   # compared with noint
INTERVENTION.CODES <- c(NOINT, SCENARIOS)                                 # every simset step 01 loads


## Years pulled from the simsets ----
YEARS <- 2015:2035


## Folders ----
SIM.DIR        <- file.path(get.jheem.root.directory(), "simulations", "shield",
                            paste0(CALIB.NAME, "-", N.SIM), LOC.CODE)
KC.OUT.DIR     <- file.path(shield.output.path(CALIB.NAME, create = TRUE), "seattle_vs_king_county")
KC.EXTRACT.DIR <- file.path(KC.OUT.DIR, "extracts")
KC.TABLE.DIR   <- file.path(KC.OUT.DIR, "tables")
KC.FIG.DIR     <- file.path(KC.OUT.DIR, "figures")
for (d in c(KC.EXTRACT.DIR, KC.TABLE.DIR, KC.FIG.DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
KC.RESULTS.FILE <- file.path(KC.OUT.DIR, "seattle_kc_raw_results.Rdata")
