# 4-22-2026
source("../jheem_analyses/applications/SHIELD/shield_engine_test.R")
source("../jheem_analyses/applications/SHIELD/doxy_interventions_april.R")

# An intervention changing doxy coverage and feeding in values for
# doxy effectiveness and doxy discontinuation rate
sim_int = doxyPEP_10$run(sim, end.year=2030, start.year=2020)

# You can replace the line generating the error with this tryCatch:
if (1==2) {
    tryCatch({private$i.top.level.value.may.not.apply.times[[top.level.name]] = setdiff_sorted_vectors(private$i.quantity.value.times[[top.level.name]],
                                                                                                       all.background.times)}, error=function(e) {browser()})
}