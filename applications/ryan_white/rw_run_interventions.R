
LOCATION.INDICES = (6-1) * 5 + 1:5
print(paste0("Running RW Interventions for indices: ", paste0(LOCATION.INDICES, collapse=', ')))

source('applications/ryan_white/ryan_white_main.R')
LOCATIONS = RW.LOCATIONS[LOCATION.INDICES[LOCATION.INDICES<=length(RW.LOCATIONS)]]


print(paste0("Running RW Interventions for locations: ", paste0(LOCATIONS, collapse=', ')))


simset.collection = create.simset.collection(version = 'rw',
                                             calibration.code = CALIBRATION.CODE,
                                             locations = LOCATIONS,
                                             interventions = setdiff(RW.INTERVENTION.CODES, 'noint'),
                                             n.sim = N.SIM)

simset.collection$run(start.year = RW.INT.RUN.FROM.YEAR, end.year = RW.INT.RUN.TO.YEAR, overwrite.prior = F, verbose=T)

