
library(jheem2)

.real_sim_root <- "/Volumes/jheem$/simulations"

.rewrite_q_path <- function(path) {
    if (is.character(path)) sub("^Q:simulations", .real_sim_root, path) else path
}

file.exists <- local({
    f <- base::file.exists
    function(...) {
        args <- lapply(list(...), .rewrite_q_path)
        do.call(f, args)
    }
})

dir.exists <- local({
    f <- base::dir.exists
    function(paths) f(.rewrite_q_path(paths))
})

list.files <- local({
    f <- base::list.files
    function(path = ".", ...) f(path = .rewrite_q_path(path), ...)
})

list.dirs <- local({
    f <- base::list.dirs
    function(path = ".", ...) f(path = .rewrite_q_path(path), ...)
})

normalizePath <- local({
    f <- base::normalizePath
    function(path, ...) f(.rewrite_q_path(path), ...)
})

Sys.glob <- local({
    f <- base::Sys.glob
    function(paths, ...) f(.rewrite_q_path(paths), ...)
})


print("Sourcing code prior to running interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATIONS = RW.LOCATIONS # 'FL' 
FORCE.OVERWRITE = T #FORCE.REDO
INTERVENTION.CODES = c('adap.100.end.26', "noint") 

print(paste0("Doing locations: ",
             paste0(LOCATIONS, collapse=', ')))


int.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                        locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)

int.collection$run(2025, 2036, verbose=TRUE, stop.for.errors=F, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2024)
