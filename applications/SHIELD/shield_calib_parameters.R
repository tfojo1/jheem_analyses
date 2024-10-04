#my best guess for this parameter is different in different locations, so we formulate prior as a multiply of the best guess
# Defining the calibration parameters and prior distributions

#1- PARAMETER PRIORS:----
##--- Population param priors ----
POPULATION.PARAMETERS.PRIOR=join.distributions( 
  ## Fertility rates by race
  black.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.fertility.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  
  # Mortality rates by race:
  black.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  hispanic.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  other.general.mortality.rate.multiplier= Lognormal.Distribution(meanlog = 0, sdlog = 0.5*log(2)),
  # Mortality rates by sex:
  male.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2)), 
  female.general.mortality.rate.multiplier = Lognormal.Distribution(meanlog = 0,sdlog = 0.5*log(2))
  
)

##--- Transmission param priors ----
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


##--- Full param priors ----
SHIELD.FULL.PARAMETERS.PRIOR = distributions::join.distributions(
  POPULATION.PARAMETERS.PRIOR,
  TRANSMISSION.PARAMETERS.PRIOR
)

#2- LINKING PARAMETERS TO FUNCTIONAL FORMS....  -----
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
  
  ## Mortality rates by race----
  races=model.settings$specification.metadata$dim.names$race
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "general.mortality.rate",
                                                 alpha.name = 'value',
                                                 values = parameters[paste0(races,".general.mortality.rate.multiplier")],
                                                 dimension = "race",
                                                 applies.to.dimension.values = races)
  ## Mortality rates by sex----
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "general.mortality.rate",
                                                 alpha.name = 'value',
                                                 values = parameters["male.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('heterosexual_male','msm'))
  set.element.functional.form.main.effect.alphas(model.settings,
                                                 element.name = "general.mortality.rate",
                                                 alpha.name = 'value',
                                                 values = parameters["female.general.mortality.rate.multiplier"],
                                                 dimension = "sex",
                                                 applies.to.dimension.values = c('female'))
}



#3- SAMPLING BLOCKS: ----
# classic mcmc samples one param at a time, adaptive mcms samples multiple params
#could add more params here (1-5 per block)
##---- Population sampling block ----
SHIELD.POPULATION.SAMPLING.BLOCKS = list(
  fertility.rates=c("black.fertility.rate.multiplier",
                    "hispanic.fertility.rate.multiplier",
                    "other.fertility.rate.multiplier"),
  
  mortality.rates.by.race=c("black.general.mortality.rate.multiplier",
                            "hispanic.general.mortality.rate.multiplier",
                            "other.general.mortality.rate.multiplier"),
  
  moratlity.rates.by.sex=c("male.general.mortality.rate.multiplier",
                           "female.general.mortality.rate.multiplier")
)

##---- Transmission sampling block ----
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