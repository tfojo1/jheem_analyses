SHIELD.PARAMETERS.PRIOR=join.distributions(
  global.trate = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
  
)


SHIELD.APPLY.PARAMETERS.FN = function(model.settings, parameters){
  
}

#classic mcmc samples one param at a time
#adaptive mcms samples multiple params
SHIELD.PARAMETER.SAMPLING.BLOCKS = list( 
  basic.transmission = c("global.trate") #could add more params here
  
  
  )