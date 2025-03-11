# Modeling the loss Ryan White funding

get_suppresion_effect_arcsin = function(n, rw_survey) {
  # Install 'ks' package if not installed
  if (!requireNamespace("ks", quietly = T)) {
    install.packages("ks")}
  library(ks)
  
  # Extract relevant columns, remove rows with NA values, and scale 0->1
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ]/100
  
  ## Convert to matrix for KDE estimation
  X = as.matrix(X)
  
  # Small constant to avoid logit errors
  epsilon = 1e-6  
  X = pmax(pmin(X, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
  
  # Logit transformation
  arcsin_X = asin(sqrt(X))
  
  # Fit KDE in logit space
  kde_fit = kde(x = arcsin_X )
  
  # Sample 'n' points in logit space
  sampled_arcsin = rkde(n, kde_fit)
  
  # Transform back using inverse logit
  sampled_values = sampled_arcsin = (sin(sampled_arcsin))^2
  
  
  ## Ensure the output is a 3 x n matrix
  rv = t(sampled_values)  # Transpose to match the required format
  
  dimnames(rv)[[1]] = c('lose.adap.effect', 'lose.oahs.effect', 'lose.rw.support.effect')
  
  rv
}

get_suppresion_effect_bootstrapped = function(n, rw_survey) {
  
  # Extract relevant columns and remove rows with NA values, scale 0->1 
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ] / 100  # Convert percentage to proportions
  
  # Convert to matrix 
  X = as.matrix(X)
  
  # --- BOOTSTRAP SAMPLING ---
  # Randomly sample 'n' points with replacement from the original data
  sampled_values = X[sample(1:nrow(X), size = n, replace = TRUE), ]
  
  # Ensure output is a 3 x n matrix
  return(t(sampled_values))  # Transpose for correct dimensions
}

rw_survey = read.csv("../jheem_analyses/applications/ryan_white/rw_survey.csv")
reset.seed = floor(runif(1, 0, .Machine$integer.max))
set.seed(1234)
RW.effect.values = get_suppresion_effect_arcsin(1000, rw_survey)
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
