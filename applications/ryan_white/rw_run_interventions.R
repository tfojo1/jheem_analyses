

LOCATIONS = c(BALTIMORE.MSA, HOUSTON.MSA)

source('applications/ryan_white/ryan_white_main.R')


simset.collection = create.simset.collection(version = 'rw',
                                             calibration.code = 'final.ehe',
                                             locations = LOCATIONS,
                                             interventions = c('noint','rw.end','rw.end.cons'),
                                             n.sim = N.SIM)

simset.collection$run(start.year = 2025, end.year = 2035, overwrite.prior = F, verbose=T)


quick.results = simset.collection$get(outcomes = 'incidence',
                                      year = 2020:2035)

YEARS = 2025:2030
cumulative.inc = apply(quick.results[as.character(YEARS),,,,drop=F], c('sim', 'location', 'intervention'), sum)

rel.excess.inc.end = (cumulative.inc[,,'rw.end',drop=F] - cumulative.inc[,,'noint',drop=F]) / cumulative.inc[,,'noint',drop=F]
rel.excess.inc.end.cons = (cumulative.inc[,,'rw.end.cons',drop=F] - cumulative.inc[,,'noint',drop=F]) / cumulative.inc[,,'noint',drop=F]

round(100*cbind(
    end = apply(rel.excess.inc.end, 'location', mean),
    end.cons = apply(rel.excess.inc.end.cons, 'location', mean)
))
