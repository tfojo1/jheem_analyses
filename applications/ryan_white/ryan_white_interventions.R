# Modeling the loss Ryan White funding

source("../jheem_analyses/applications/ryan_white/ryan_white_specification.R")

#Interventions are scaled up linearly from July 1st of START.YEAR to October 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025.5
IMPLEMENTED.BY.YEAR = 2025.8

simset = assemble.simulations.from.calibration('ehe','C.12580','full.with.covid2')

simset = load.simulation.set("../jheem_analyses/cached/simsets_for_lauren/simset_2025_01-31_C.12580.Rdata")
ss=simset$subset(1:5)


# Complete Loss of ADAP
lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = 0,
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )


lose.adap = create.intervention(lose.adap.2.effect, WHOLE.POPULATION, code = "loseAdap")

# Temporary Lapse of ADAP
temp.lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = c(0,0,1),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, 2028, 2028.25),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

temp.lose.adap = create.intervention(temp.lose.adap.effect, WHOLE.POPULATION, code = "loseAdap2")



# Complete Loss of ADAP and X% OAHS

lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = X,
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )

lose.adap.oahs = create.intervention(c(lose.adap.effect, lose.oahs.effect) , WHOLE.POPULATION, code = "loseRW")


# Temporary Lapse of ADAP and X% OAHS

temp.lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = c(X,X,1),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, 2028, 2028.25),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

temp.lose.adap.oahs = create.intervention(c(temp.lose.adap.effect, temp.lose.oahs.effect) , WHOLE.POPULATION, code = "loseRW2")





#No intervention
noint=get.null.intervention()

# Complete Loss of ADAP
lose.adap = create.intervention.effect(quantity.name = 'ryan.white.adap.effect',
                                              start.time = START.YEAR,
                                              effect.values = 0,
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )
                                              
                                              
                                              
                                              
#Test intervention
no.adap.simset = lose.adap.2$run(simset, start.year=2025, end.year=2035, verbose=TRUE)

#no intervention
adap.simset = noint$run(simset, start.year=2025, end.year=2035, verbose=T)

simplot(adap.simset, no.adap.simset, 'incidence', summary.type = 'mean.and.interval', dimension.values=list(year=2020:2035))

total.inc.no.adap = no.adap.simset$get(outcomes = 'incidence', keep.dimensions = character(), dimension.values=list(year=2025:2035))
total.inc.adap = adap.simset$get(outcomes = 'incidence', keep.dimensions = character(), dimension.values=list(year=2025:2035))

infections.averted = total.inc.no.adap - total.inc.adap

print(paste0("Continuing RW was projected to avert ",
             format(round(mean(infections.averted), 0), big.mark=','),
             " [",
             format(round(quantile(infections.averted, probs=0.025)), big.mark=','),
             "-",
             format(round(quantile(infections.averted, probs=0.975)), big.mark=','),
             "] new infections from 2025-2035"))

relative.new.infections = total.inc.no.adap / total.inc.adap

print(paste0("Without continued RW, we  projected that new infections would be ",
             format(round(mean(relative.new.infections), 1), big.mark=','),
             " [",
             format(round(quantile(relative.new.infections, probs=0.025), 1), big.mark=','),
             "-",
             format(round(quantile(relative.new.infections, probs=0.975), 1), big.mark=','),
             "] times as many new infections as if RW continued"))
