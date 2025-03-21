
source('applications/ryan_white/ryan_white_main.R')
library(ggplot2)

loc = 'C.26420'
simset.baseline = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM)
simset.noint = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'noint')
simset.end = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.end')
simset.b.intr = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.b.intr')
simset.p.intr = retrieve.simulation.set('rw', loc, CALIBRATION.CODE, N.SIM, 'rw.p.intr')

PLOT.YEARS = 2010:2030


simplot(simset.baseline, c('non.adap.clients','oahs.clients','adap.proportion'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('oahs.suppression','adap.suppression'), data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('non.adap.clients'), facet.by='age', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('non.adap.clients'), facet.by='race', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('non.adap.clients'), facet.by='sex', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('oahs.suppression'), facet.by='race', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('oahs.suppression'), facet.by='age', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)

simplot(simset.baseline, c('oahs.suppression'), facet.by='sex', data.manager=RW.DATA.MANAGER, dimension.values = list(year=PLOT.YEARS)) +
  ggplot2::theme_bw() + ggtitle(NULL)


PLOT.YEARS.INT = 2015:2035
simplot(simset.noint, simset.lose, c('incidence','new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval') +
  ggplot2::theme_bw() + ggtitle(NULL)
simplot(simset.noint, simset.templose, c('incidence','new'), dimension.values = list(year=PLOT.YEARS.INT), summary.type = 'mean.and.interval') +
  ggplot2::theme_bw() + ggtitle(NULL) + geom_vline(xintercept = 2028, linetype='dashed')
