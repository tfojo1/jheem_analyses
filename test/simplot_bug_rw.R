
source('applications/ryan_white/ryan_white_main.R')

simset = retrieve.simulation.set('rw', 'C.12580', 'final.ehe', n.sim=80, intervention.code = 'noint', sub.version='w')

# This gives sim in the jheem sex ontology (msm, female, heterosexual_male)
simplot(simset,
        split.by='sex',
        data.manager = RW.DATA.MANAGER,
        'non.adap.clients', dimension.values = list(year=2015:2025)) 

# Gives an error
simplot(simset,
        split.by='risk',
        data.manager = RW.DATA.MANAGER,
        'non.adap.clients', dimension.values = list(year=2015:2025)) 

simplot(simset, 'new', 
        summary.type = 'mean.and.interval')
