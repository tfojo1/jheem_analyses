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
LOSS.LAG = 0.25
BRIEF.INTERRUPTION.RESTART.YEAR = 2027
PROLONGED.INTERRUPTION.RESTART.YEAR = 2029
RESTART.LAG = 1

# Complete Loss of ADAP
adap.cessation.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(1-lose.adap.effect),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = START.YEAR + LOSS.LAG,
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )



# Complete Loss of OAHS
oahs.cessation.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(1-lose.oahs.effect),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = START.YEAR + LOSS.LAG,
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )


# Complete Loss of RW support
rw.support.cessation.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                                         start.time = START.YEAR,
                                                         effect.values = expression(1-lose.rw.support.effect),
                                                         apply.effects.as = 'value',
                                                         scale = 'proportion',
                                                         times = START.YEAR + LOSS.LAG,
                                                         allow.values.less.than.otherwise = T,
                                                         allow.values.greater.than.otherwise = F )


rw.cessation = create.intervention(adap.cessation.effect,
                                   oahs.cessation.effect,
                                   rw.support.cessation.effect, 
                                   parameters = RW.effect.values, 
                                   WHOLE.POPULATION, 
                                   code = "rw.end")

# Temporary Lapse of ADAP
adap.brief.interruption.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                            start.time = START.YEAR,
                                                            end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.adap.effect,1-lose.adap.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
oahs.brief.interruption.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                            start.time = START.YEAR,
                                                            end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.oahs.effect,1-lose.oahs.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
rw.support.brief.interruption.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                                                  start.time = START.YEAR,
                                                                  end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.rw.support.effect,1-lose.rw.support.effect)),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

rw.brief.interruption = create.intervention(adap.brief.interruption.effect,
                                            oahs.brief.interruption.effect,
                                            rw.support.brief.interruption.effect,
                                            parameters = RW.effect.values, 
                                            WHOLE.POPULATION,
                                            code = "rw.b.intr")

# Temporary Lapse of ADAP
adap.prolonged.interruption.effect = create.intervention.effect(quantity.name = 'adap.suppression.effect',
                                                            start.time = START.YEAR,
                                                            end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.adap.effect,1-lose.adap.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
oahs.prolonged.interruption.effect = create.intervention.effect(quantity.name = 'oahs.suppression.effect',
                                                            start.time = START.YEAR,
                                                            end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.oahs.effect,1-lose.oahs.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
rw.support.prolonged.interruption.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.effect',
                                                                  start.time = START.YEAR,
                                                                  end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.rw.support.effect,1-lose.rw.support.effect)),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

rw.prolonged.interruption = create.intervention(adap.prolonged.interruption.effect,
                                                oahs.prolonged.interruption.effect,
                                                rw.support.prolonged.interruption.effect,
                                                parameters = RW.effect.values, 
                                                WHOLE.POPULATION,
                                                code = "rw.p.intr")
