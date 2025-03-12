
source('applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

x=RW.LOCATIONS[c(1,9,17,25)+5];x

# load simset
simset = retrieve.simulation.set('rw','C.12060','full.with.covid2',100)

simplot(simset, 'non.adap.clients', data.manager=RW.DATA.MANAGER)
simplot(simset, 'non.adap.clients', data.manager=RW.DATA.MANAGER, facet.by='race')
simplot(simset, 'non.adap.clients', data.manager=RW.DATA.MANAGER, facet.by='sex')
simplot(simset, 'non.adap.clients', data.manager=RW.DATA.MANAGER, facet.by='age')
simplot(simset, 'non.adap.clients', data.manager=RW.DATA.MANAGER, facet.by='risk', split.by='sex')

simplot(simset, 'oahs.clients', data.manager=RW.DATA.MANAGER)
 simplot(simset, 'oahs.clients', data.manager=RW.DATA.MANAGER, facet.by='race')
simplot(simset, 'oahs.clients', data.manager=RW.DATA.MANAGER, facet.by='age')

simplot(simset, 'oahs.suppression', data.manager=RW.DATA.MANAGER)
simplot(simset, 'oahs.suppression', data.manager=RW.DATA.MANAGER, facet.by='race')
simplot(simset, 'oahs.suppression', data.manager=RW.DATA.MANAGER, facet.by='age')

simplot(simset, 'adap.proportion', data.manager=RW.DATA.MANAGER)
#simplot(simset, 'adap.proportion.of.diagnosed', data.manager=RW.DATA.MANAGER)
simplot(simset, 'adap.proportion', data.manager=RW.DATA.MANAGER, facet.by='race')
#simplot(simset, 'adap.proportion.of.diagnosed', data.manager=RW.DATA.MANAGER, facet.by='race')
simplot(simset, 'adap.proportion', data.manager=RW.DATA.MANAGER, facet.by='sex')
#simplot(simset, 'adap.proportion.of.diagnosed', data.manager=RW.DATA.MANAGER, facet.by='sex')
simplot(simset, 'adap.proportion', data.manager=RW.DATA.MANAGER, facet.by='age')
#simplot(simset, 'adap.proportion.of.diagnosed', data.manager=RW.DATA.MANAGER, facet.by='age')

simplot(simset, 'adap.suppression', data.manager=RW.DATA.MANAGER)
simplot(simset, 'adap.suppressed.proportion.of.diagnosed', data.manager=RW.DATA.MANAGER)





overlapping.years = intersect(
  dimnames(RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location)$year,
  dimnames(RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race)$year
)
cbind(
  totals = RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location[overlapping.years,simset$location],
  aggregated = apply(RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race[overlapping.years,simset$location,], 'year', sum)
)

RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location[,simset$location]
rowSums(RW.DATA.MANAGER$data$non.adap.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__race[,simset$location,],na.rm=T)


lik = ryan.white.likelihood.instructions$instantiate.likelihood('rw',simset$location,data.manager = RW.DATA.MANAGER)
lik$compute(simset$last.sim(), debug=T)
