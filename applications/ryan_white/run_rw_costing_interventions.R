
###### Needed to work on SHIELD server #########################################
library(jheem2)

print("Sourcing code prior to running interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')
source('../jheem_analyses/commoncode/file_paths.R')
set.jheem.root.directory(ROOT.DIR)
################################################################################
################################################################################

LOCATIONS =  'FL'  #RW.LOCATIONS # 'FL' 
FORCE.OVERWRITE = T #FORCE.REDO
INTERVENTION.CODES = c('adap.end.50.no.trate.26','noint.no.trate.26') 

print(paste0("Doing locations: ",
             paste0(LOCATIONS, collapse=', ')))


int.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                        locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)

int.collection$run(2025, 2036, verbose=TRUE, stop.for.errors=F, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2024)
