# EHE26: impact of scaling up PrEP, HIV testing and viral suppression among MSM
# Working directory is assumed to be jheem_analyses/

# 1. LOAD THE EHE MODEL ----
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
get.jheem.root.directory()

# 2. TIMELINE ----
# 2026 = baseline year. Interventions start here.
# 2030 = targets are fully reached (linear scale-up from 2026 to 2030).
# 2035 = end of projection.
BASELINE.YEAR = 2026
IMPLEMENTED.BY.YEAR = 2030
END.YEAR = 2035

# Years of history we keep in the saved simsets (for plots)
KEEP.FROM.YEAR = 2020

# 3. CALIBRATED SIMULATIONS TO START FROM ----
# 'final.ehe'       = MSA-level calibration
# 'final.ehe.state' = state-level calibration
VERSION = 'ehe'
CALIBRATION.CODE = 'final.ehe'
N.SIM = 1000

# 4. LOCATIONS ----
# Start small: Baltimore only.
# Later, switch to all EHE MSAs: LOCATIONS = MSAS.OF.INTEREST
LOCATIONS = c("C.12580") #baltimore

# 5. INTERVENTIONS ----
# Interventions are defined in define_interventions.R
# That file also creates INTERVENTION.LIST and INTERVENTION.CODES.
# 'noint' = no intervention (the comparison scenario)
source('../jheem_analyses/applications/EHE26/ehe26_define_interventions.R')
print(paste0("Interventions: ", paste0(INTERVENTION.CODES, collapse = ', ')))

# 6. OUTPUT ----
RESULTS.DIR = file.path(get.jheem.root.directory(), 'results', 'ehe26')
VERBOSE = T
