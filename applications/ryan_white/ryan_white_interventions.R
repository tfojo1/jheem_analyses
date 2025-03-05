# Modeling the loss Ryan White funding

source("../jheem_analyses/applications/ryan_white/ryan_white_specification.R")

install.packages('ks')
library(ks)
get_suppresion_effect = function(n, rw_survey) {
  ## returns a matrix with 3 rows and n columns ##
  ## columns lose.adap.effect, lose.oahs.effect, lose.rw.support.effect ##
  
  # Select the relevant columns for KDE
  X = rw_survey[,c("q1_adap_loss","q2_oahs_loss","q3_support_loss")]
  mask = apply(!is.na(X),1, all)
  X = X[mask,]
  
  # Convert to matrix for KDE estimation
  X = as.matrix(X)
  
  # Fit multivariate KDE using ks::kde()
  kde_fit = kde(x = X)
  
  # Sample 'n' points from the estimated KDE
  sampled_values = rkde(n, kde_fit)
  
  # Return the sampled values as a matrix with 3 rows and n columns
  return(t(sampled_values))  # Transpose to match the required format
}

rw_survey = read.csv("../jheem_analyses/applications/ryan_white/rw_survey.csv")
reset.seed = floor(runif(1, 0, .Machine$integer.max))
set.seed(1234)
RW.effect.values = get_suppresion_effect(1000, rw_survey)/100
set.seed(reset.seed)

#Interventions are scaled up linearly from July 1st of START.YEAR to October 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025.5
IMPLEMENTED.BY.YEAR = 2025.8
RESTART.YEAR = 2029

# Complete Loss of ADAP
lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = expression(1-lose.adap.effect),
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )



# Complete Loss of OAHS
lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = expression(1-lose.oahs.effect),
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )


# Complete Loss of RW support
lose.rw.support.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                              start.time = START.YEAR,
                                              effect.values = expression(1-lose.rw.support.effect),
                                              apply.effects.as = 'value',
                                              scale = 'proportion',
                                              times = IMPLEMENTED.BY.YEAR,
                                              allow.values.less.than.otherwise = T,
                                              allow.values.greater.than.otherwise = F )


lose.RW.intervention = create.intervention(lose.adap.effect,lose.oahs.effect,lose.rw.support.effect, parameters = RW.effect.values, WHOLE.POPULATION, code = "loseRW")

# Temporary Lapse of ADAP
temp.lose.adap.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                   start.time = START.YEAR,
                                                   end.time = RESTART.YEAR + 0.25,
                                                   effect.values = expression(c(1-lose.adap.effect,1-lose.adap.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, RESTART.YEAR),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
temp.lose.oahs.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                   start.time = START.YEAR,
                                                   end.time = RESTART.YEAR + 0.25,
                                                   effect.values = expression(c(1-lose.oahs.effect,1-lose.oahs.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, RESTART.YEAR),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
temp.lose.rw.support.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                                   start.time = START.YEAR,
                                                   end.time = RESTART.YEAR + 0.25,
                                                   effect.values = expression(c(1-lose.rw.support.effect,1-lose.rw.support.effect)),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = c(START.YEAR + 0.25, RESTART.YEAR),
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

temp.lose.RW.intervention = create.intervention(temp.lose.adap.effect,temp.lose.oahs.effect,temp.lose.rw.support.effect, parameters = RW.effect.values, WHOLE.POPULATION, code = "temploseRW")
