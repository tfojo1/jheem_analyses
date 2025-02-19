supp.lik.boston = suppression.nested.likelihood.instructions$instantiate.likelihood('ehe','C.14460')
load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.ehe_simset_2025-02-12_C.14460.Rdata")
supp.lik.boston$compute(simset$last.sim())

supp.lik.charlotte = suppression.nested.likelihood.instructions$instantiate.likelihood('ehe','C.16740')
load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.ehe_simset_2025-02-11_C.16740.Rdata")
supp.lik.charlotte$compute(simset$last.sim())

supp.lik.new.orleans = suppression.nested.likelihood.instructions$instantiate.likelihood('ehe','C.35380')
load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.ehe_simset_2025-02-11_C.35380.Rdata")
supp.lik.new.orleans$compute(simset$last.sim())
