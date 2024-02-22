# depression test #
# source dep.sp

source('../jheem_analyses/applications/Depression/dep_specification.R')

joint.prior = join.distributions(DEP.PARAMETERS.PRIOR, EHE.PARAMETERS.PRIOR)
params = suppressWarnings(get.medians(joint.prior))
params["global.trate"] = 0.075

engine=create.jheem.engine("dep", "C.12580", end.year = 2025)
sim = engine$run(params)
