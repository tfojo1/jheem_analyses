### Estimating the bias between MSA and state proportions tested for HIV (BRFSS)
### Data: BRFSS proportion.tested.for.hiv (SURVEILLANCE.MANAGER), census adult.population
### Returning proportion.tested.bias.estimates (out.mean, out.sd, n.out; the in.* values are NA)
### called by shield_likelihoods.R through get.cached.object.for.version("proportion.tested.bias.estimates", "shield")
###   (run only once on each machine; the cached object is saved in commoncode/object_for_version_cache)
## ******************************* ******************************* ******************************* ******************************* ##
# Used in: proportion.tested.hiv.age.sex.race.oneway.nested.likelihood.instructions (shield_likelihoods.R)
#   p.bias.outside.location   = out.mean
#   p.bias.sd.inside.location = p.bias.sd.outside.location = out.sd
#
# History:
#   The cached file was first made on 2026-05-08 (commit 793b0602) without this script. I checked and rebuilt
#   the cached obj on 2026-10-20
#
# Why age and sex only (no race):
#   The likelihood is stratified by age, race and sex, but the bias estimate uses age and sex only.
#   With race added, the estimate changes a lot (same call, levels 0 and 1, syphilis-manager-v2026.10.09):
#                           out.mean   out.sd   n.out
#     age, sex (used)        -0.015     0.055    1656
#     age only               -0.015     0.056    1518
#     age, race, sex         -0.029     0.150   ~2320
#     age, race              -0.029     0.155   ~2180
#   BRFSS race-specific proportions at MSA level rest on small samples, so the MSA-vs-state differences
#   by race are mostly sampling noise. Including them would almost triple the bias SD and let the
#   likelihood treat MSA testing data as much less informative than it is. We therefore leave race out.
#   (Our reading of the numbers above; the original choice was made by Andrew Zalesak - confirm with him.)
#
# Note on sex:
#   With the sex mapping in R/shield_ontology_mappings.R as of 2026-10 (msm added to male), BRFSS male is NaN
#   at MSA level, so the sex strata contain female only (12 strata per comparison: total + 10 ages + female).
#   Once the mapping drops msm instead, male is included (13 strata per comparison). Leaving sex out changes
#   the estimate very little (age only, above), so we expect the fix to move it only slightly; re-cache after it.

source("../jheem_analyses/applications/SHIELD/shield_specification.R")

# Estimate the bias ----
# CBSAs compared with their states (super.location.type); no comparison with counties (sub.location.type = NULL)
proportion.tested.bias.estimates <- get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                         dimensions               = c("age", "sex"),
                                                         levels.of.stratification = c(0, 1),
                                                         outcome.for.p            = "proportion.tested.for.hiv",
                                                         outcome.for.n            = "adult.population",
                                                         sub.location.type        = NULL,
                                                         super.location.type      = "STATE",
                                                         main.location.type       = "CBSA")
print(proportion.tested.bias.estimates[c("out.mean", "out.sd", "n.out")])

# Compare with the cached object ----
old.bias <- get.cached.object.for.version("proportion.tested.bias.estimates", version = "shield")
print(rbind(cached = unlist(old.bias[c("out.mean", "out.sd", "n.out")]),
            new    = unlist(proportion.tested.bias.estimates[c("out.mean", "out.sd", "n.out")])))

# Cache ----
# off by default, so sourcing this script does not overwrite the cached object by accident
if (1==2) {
    cache.object.for.version(object  = proportion.tested.bias.estimates,
                             name    = "proportion.tested.bias.estimates",
                             version = 'shield',
                             overwrite = T)
    print("cached proportion.tested.bias.estimates for SHIELD")
}
