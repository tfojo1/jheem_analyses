
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

locs = setdiff(MSAS.OF.INTEREST, c(CINCINATTI.MSA, ST.LOUIS.MSA))

prog = get.calibration.progress('ehe',
                         locations = locs,
                         calibration.code = 'final.ehe'); prog

dimnames(prog)[[1]][prog[,1]<100]
prog[prog<100,drop=F,]
prog[prog==100,drop=F,]

paste0("'", dimnames(prog)[[1]][prog[,1]==100], "'", collapse=', ')

sum(prog==100)



cache.mcmc.summary('ehe', 'C.40140', 'full.ehe')
cache.mcmc.summary('ehe', 'C.41740', 'full.ehe')
cache.mcmc.summary('ehe', 'C.29820', 'full.ehe')

cache.mcmc.summary('ehe', 'C.31080', 'full.ehe')



done.rw = list.dirs('Q:simulations/rw/full.ehe-100', full.names = F, recursive = F)

setdiff(MSAS.OF.INTEREST,
        c(done.rw, CINCINATTI.MSA, ST.LOUIS.MSA, locs))

x.sub = lapply(x, function(y){list.files(file.path('Q:simulations/rw/full.ehe-100', y))})
