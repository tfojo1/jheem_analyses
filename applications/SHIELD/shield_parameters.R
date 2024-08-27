# set of prior distributions for the parameters
SHIELD.PARAMETERS.PRIOR=join.distributions(
  global.trate = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.multiplier0 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess 
  msm.trate.multiplier1 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.multiplier2 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.black.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.hispanic.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
  
)

# linking the parameters to their functional forms 
SHIELD.APPLY.PARAMETERS.FN = function(model.settings, parameters){
  #alphas are main effects  (corresponds to our margines)
  for(time in 0:2){
    set.element.functional.form.main.effect.alphas(model.settings, 
                                                   element.name = "msm.trates",
                                                   alpha.name = paste0('time',time),
                                                   values = parameters[paste0("msm.trate.multiplier",time)],
                                                   dimension = 'all', 
                                                   applies.to.dimension.values = 'all')
    set.element.functional.form.main.effect.alphas(model.settings, 
                                                   element.name = "msm.trates",
                                                   alpha.name = paste0('time',time),
                                                   values = parameters[c("msm.trate.black.multiplier","msm.trate.hispanic.multiplier")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic"))
  }
  
}

#classic mcmc samples one param at a time, adaptive mcms samples multiple params
SHIELD.PARAMETER.SAMPLING.BLOCKS = list( 
  basic.transmission = c("global.trate",
                         "msm.trate.multiplier0",
                         "msm.trate.multiplier1",
                         "msm.trate.multiplier2",
                         "msm.trate.black.multiplier",
                         "msm.trate.hispanic.multiplier") #could add more params here (1-5 per block)
  
  
)