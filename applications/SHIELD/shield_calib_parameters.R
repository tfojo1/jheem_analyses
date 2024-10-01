#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

# PARAMETER PRIORS:----
## POPULATION.PARAMETERS.PRIOR ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
  ## Fertility rates by race
  ##?should we add more coef by agegroup?
  black.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2))
)

## TRANSMISSION.PARAMETERS.PRIOR ----
TRANSMISSION.PARAMETERS.PRIOR=join.distributions( 
  ## Transmission
  global.trate = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)), #directly used in specification
  msm.trate.multiplier0 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.multiplier1 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  msm.trate.multiplier2 = Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  ### by race:
  black.msm.trate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.msm.trate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  ## Sexual Mixing by Age
  age.mixing.sd.mult = Lognormal.Distribution(0, 0.25*log(2)) #directly used in specification helper function
  #to control the standard deviation of the contact matrix by age

) 


## SHIELD.FULL.PARAMETERS.PRIOR ----
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  TRANSMISSION.PARAMETERS.PRIOR
)

# Linking the parameters to their functional forms...  -----
SHIELD.APPLY.PARAMETERS.FN = function(model.settings, parameters){ 
  #alphas are main effects  (corresponds to our margines)
  ## Transmission ----
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
                                                   values = parameters[c("black.msm.trate.multiplier","hispanic.msm.trate.multiplier")],
                                                   dimension = "race.to", #recipient
                                                   applies.to.dimension.values = c("black","hispanic"))
  }
  
  ## Fertility rates by race----
  #applies the 3 race multipliers to the fertility
  races=model.settings$specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "fertility.rate",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,".fertility.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  
}



# SAMPLING BLOCKS: ----
# classic mcmc samples one param at a time, adaptive mcms samples multiple params
#could add more params here (1-5 per block)
## SHIELD.POPULATION.SAMPLING.BLOCKS ----
SHIELD.POPULATION.SAMPLING.BLOCKS = list(
   fertility.rates=c("black.fertility.rate.multiplier",
                    "hispanic.fertility.rate.multiplier",
                    "other.fertility.rate.multiplier")
  
)
## SHIELD.TRANSMISSION.SAMPLING.BLOCKS ----
SHIELD.TRANSMISSION.SAMPLING.BLOCKS = list(
  global.trate=c("global.trate"),

msm.transmission = c(
  "msm.trate.multiplier0",
  "msm.trate.multiplier1",
  "msm.trate.multiplier2",
  #
  "black.msm.trate.multiplier",
  "hispanic.msm.trate.multiplier"),

age.mixing.transmission=("age.mixing.sd.mult")
)
SHIELD.PARAMETER.SAMPLING.BLOCKS=c(SHIELD.POPULATION.SAMPLING.BLOCKS,
                                   SHIELD.TRANSMISSION.SAMPLING.BLOCKS)