# EHE26: settings shared by the two JHEEM baseline steps
#   Step 1: ehe26_jheem_extract.R          loads each simset ONCE, saves the raw model numbers
#   Step 2: ehe26_jheem_baseline_trends.R  reads the saved numbers, builds measures and figures
# Both source this file, so they always point to the same simsets and folders.
# This file does not load jheem2 or any simulations.
# Working directory is assumed to be jheem_analyses/

# 1. WHERE THE JHEEM FILES ARE ----
# ROOT.DIR comes from commoncode/file_paths.R (same root jheem2 uses)
source('../jheem_analyses/commoncode/file_paths.R')

# 2. WHICH SIMULATIONS ----
CALIBRATION.CODE = 'final.ehe.state'
N.SIM = 1000
# Which saved simset file to load for each state:
#   'noint'    = the no-intervention run (~400 MB, faster to load). Has years from 2025 on only,
#                so it cannot be compared with the observed data before 2025.
#   'baseline' = the calibrated simsets themselves (~1.5 GB, slower to load), 1970 to the end of
#                the projection. Needed for the years before 2025 (data comparison, 2017-18 PrEP need).
# Both have no added interventions.
# Changing this also changes RAW.DIR below, so step 2 reads the matching extracts automatically.
SIMSET.FILE = 'baseline'

# States with calibrated simsets
SIM.DIR = file.path(ROOT.DIR, 'simulations', 'ehe', paste0(CALIBRATION.CODE, '-', N.SIM))
MODEL.STATES = list.files(SIM.DIR)

# Full path of one state's simset file, e.g.
#   <root>/simulations/ehe/final.ehe.state-1000/AL/ehe_final.ehe.state-1000_AL_noint.Rdata
simset.file = function(state)
    file.path(SIM.DIR, state,
              paste0('ehe_', CALIBRATION.CODE, '-', N.SIM, '_', state, '_', SIMSET.FILE, '.Rdata'))

# 3. YEARS ----
# Step 1 saves every year from MODEL.FROM.YEAR to the LAST year in each simset file
# (the end of the projection). Years before 2025 are kept to compare the model with the data.
MODEL.FROM.YEAR = 2013

# 4. FOLDERS ----
if (!exists('OUT.DIR'))
    OUT.DIR = file.path(ROOT.DIR, 'results', 'ehe26', 'benchmarks')
# Raw model numbers from step 1, one file per state. Separate folder per simset file,
# so numbers from 'noint' and 'baseline' never mix.
RAW.DIR = file.path(OUT.DIR, paste0('jheem_', SIMSET.FILE, '_raw'))
