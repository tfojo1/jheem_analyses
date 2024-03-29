source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-29_C.12580.Rdata")

likelihoods =  list(# POPULATION LIKELIHOODS
  "population"= population.likelihood.instructions, 
  "immigration" = immigration.likelihood.instructions, 
  "emigration" = emigration.likelihood.instructions,
  
  # TRANSMISSION LIKELIHOODS
  "new.diagnoses" = new.diagnoses.likelihood.instructions,
  "prevalence" = prevalence.likelihood.instructions,
  
  # MORTALITY LIKELIHOODS
  "hiv.mortality" = hiv.mortality.likelihood.instructions,
  "general.mortality" = general.mortality.likelihood.instructions,
  "aids.deaths" = aids.deaths.likelihood.instructions,
  
  # CONTINUUM LIKELIHOODS
  #proportion.tested.likelihood.instructions,
  #hiv.test.positivity.likelihood.instructions, # does this one work?
  "awareness" = awareness.likelihood.instructions,
  "suppression" = suppression.likelihood.instructions,
  
  # PREP LIKELIHOODS
  "prep.uptake" = prep.uptake.likelihood.instructions,
  "prep.indications" = prep.indications.likelihood.instructions,
  
  # IDU LIKELIHOODS
  heroin.likelihood.instructions,
  cocaine.likelihood.instructions
)

for (i in 1:length(likelihoods)) {
  print(paste0("testing ",names(likelihoods)[i]," likelihood"))
  lik = likelihoods[[i]]$instantiate.likelihood('ehe',"C.12580")
  print(lik$compute(sim))
}


if(1==2){
  
  problem.likelihoods = list("hiv.test.positivity" = hiv.test.positivity.likelihood.instructions,
                             "proportion.tested" = proportion.tested.likelihood.instructions)
  
  for (i in 1:length(problem.likelihoods)) {
    print(paste0("testing ",names(problem.likelihoods)[i]," likelihood"))
    lik = problem.likelihoods[[i]]$instantiate.likelihood('ehe',"C.12580")
    print(lik$compute(sim))
  }
  
  FULL.likelihood.instructions =  join.likelihood.instructions(# POPULATION LIKELIHOODS
                                                             population.likelihood.instructions, 
                                                             immigration.likelihood.instructions, 
                                                             emigration.likelihood.instructions,
                                                             
                                                             # TRANSMISSION LIKELIHOODS
                                                             new.diagnoses.likelihood.instructions,
                                                             prevalence.likelihood.instructions,
                                                             
                                                             # MORTALITY LIKELIHOODS
                                                             hiv.mortality.likelihood.instructions,
                                                             general.mortality.likelihood.instructions,
                                                             aids.deaths.likelihood.instructions,
                                                             
                                                             # CONTINUUM LIKELIHOODS
                                                             #proportion.tested.likelihood.instructions,
                                                             hiv.test.positivity.likelihood.instructions, # does this one work?
                                                             awareness.likelihood.instructions,
                                                             suppression.likelihood.instructions,
                                                             
                                                             # PREP LIKELIHOODS
                                                             prep.uptake.likelihood.instructions,
                                                             prep.indications.likelihood.instructions,
                                                             
                                                             # IDU LIKELIHOODS
                                                             heroin.likelihood.instructions,
                                                             cocaine.likelihood.instructions
                                                             )
  
}

# 15 working likelihoods; waiting for proportion.tested (16th); not including aids.diagnoses; haven't written covid stuff yet 
# COVID stuff: change in # tests done; change in gonorrhea diagnoses; change in primary/secondary syphilis diagnoses