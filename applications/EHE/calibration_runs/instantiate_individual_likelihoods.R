source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
LOCATION = SAN.DIEGO.MSA

# likelihoods in trans
race.risk.new.lik = race.risk.new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
race.risk.prev.lik = race.risk.prevalence.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
non.age.aids.lik = non.age.aids.diagnoses.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)
pop.trans.lik = population.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)
heroin.trans.lik = heroin.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)
cocaine.trans.lik = cocaine.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)
aids.deaths.trans.lik = aids.deaths.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)
hiv.mortality.trans.lik = hiv.mortality.likelihood.instructions.trans$instantiate.likelihood('ehe',LOCATION)



# likelihoods in full
pop.lik = population.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
imm.lik = immigration.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
em.lik = emigration.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
new.lik = new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
prev.lik = prevalence.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)

hiv.mort.lik = hiv.mortality.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
gen.mort.lik = general.mortality.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
aids.deaths.lik = aids.deaths.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
aids.diag.lik = non.age.aids.diagnoses.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)

prop.tested.lik = proportion.tested.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
positivity.lik = hiv.test.positivity.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
aware.lik = awareness.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
supp.lik = suppression.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)

prep.uptake.lik = prep.uptake.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
prep.ind.lik = prep.indications.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
heroin.lik = heroin.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
cocaine.lik = cocaine.likelihood.instructions.full$instantiate.likelihood('ehe',LOCATION)
tests.change.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
gon.change.lik = gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
syph.change.lik = ps.syphilis.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)


# other likelihoods/old code
new.lik.two.way = race.risk.sex.two.way.new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)
prev.lik.two.way = race.risk.sex.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe',LOCATION)

exp(pop.lik$compute.piecewise(sim.full) - pop.lik$compute.piecewise(sim.old))
exp(imm.lik$compute.piecewise(sim.full) - imm.lik$compute.piecewise(sim.old))
exp(em.lik$compute.piecewise(sim.full) - em.lik$compute.piecewise(sim.old))
exp(new.lik$compute.piecewise(sim.full) - new.lik$compute.piecewise(sim.old))
exp(prev.lik$compute.piecewise(sim.full) - prev.lik$compute.piecewise(sim.old))
exp(hiv.mort.lik$compute.piecewise(sim.full) - hiv.mort.lik$compute.piecewise(sim.old))
exp(gen.mort.lik$compute.piecewise(sim.full) - gen.mort.lik$compute.piecewise(sim.old))
exp(aids.deaths.lik$compute.piecewise(sim.full) - aids.deaths.lik$compute.piecewise(sim.old))
exp(aids.diag.lik$compute.piecewise(sim.full) - aids.diag.lik$compute.piecewise(sim.old))
exp(prop.tested.lik$compute.piecewise(sim.full) - prop.tested.lik$compute.piecewise(sim.old))
exp(positivity.lik$compute.piecewise(sim.full) - positivity.lik$compute.piecewise(sim.old))
exp(aware.lik$compute.piecewise(sim.full) - aware.lik$compute.piecewise(sim.old))
exp(supp.lik$compute.piecewise(sim.full) - supp.lik$compute.piecewise(sim.old))
exp(prep.uptake.lik$compute.piecewise(sim.full) - prep.uptake.lik$compute.piecewise(sim.old))
exp(prep.ind.lik$compute.piecewise(sim.full) - prep.ind.lik$compute.piecewise(sim.old))
exp(heroin.lik$compute.piecewise(sim.full) - heroin.lik$compute.piecewise(sim.old))
exp(cocaine.lik$compute.piecewise(sim.full) - cocaine.lik$compute.piecewise(sim.old))


prop.tested.lik.old$compute.piecewise(sim.full) - prop.tested.lik.old$compute.piecewise(sim.old)
prop.test.lik.new$compute.piecewise(sim.full) - prop.test.lik.new$compute.piecewise(sim.old)

aware.lik.old$compute.piecewise(sim.full) - aware.lik.old$compute.piecewise(sim.old)
