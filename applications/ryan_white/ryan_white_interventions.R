# Modeling the loss Ryan White funding

# depends on source ryan_white_main 
# Install 'ks' package if not installed
if (!requireNamespace("ks", quietly = T)) {
  install.packages("ks")}
library(ks)

get_suppresion_effect_arcsin = function(n, rw_survey) {

  
  # Mask by medicaid status
  expansion.states = names(STATE.MEDICAID.EXPANSION)[STATE.MEDICAID.EXPANSION]
  expansion.states = gsub("_",".",expansion.states)
  expansion.col.names = paste0("X.choice.", expansion.states, ".")

  medicaid.expansion.mask = rowSums(rw_survey[,expansion.col.names]) > 0
  
  
  non.expansion.states = names(STATE.MEDICAID.EXPANSION)[!STATE.MEDICAID.EXPANSION]
  non.expansion.states = gsub("_",".",non.expansion.states)
  non.expansion.col.names = paste0("X.choice.", non.expansion.states, ".")
  
  medicaid.nonexpansion.mask = rowSums(rw_survey[,non.expansion.col.names]) > 0
  
  # Extract relevant columns, remove rows with NA values, and scale 0->1
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X.expansion = X[medicaid.expansion.mask,]
  X.nonexpansion = X[medicaid.nonexpansion.mask,]
  
  X = X[complete.cases(X), ]/100
  X.expansion = X.expansion[complete.cases(X.expansion), ]/100
  X.nonexpansion = X.nonexpansion[complete.cases(X.nonexpansion), ]/100
  
  
  
  ## Convert to matrix for KDE estimation
  X = as.matrix(X)
  X.expansion = as.matrix(X.expansion)
  X.nonexpansion = as.matrix(X.nonexpansion)
  
  .GlobalEnv$rw.survey = X
  .GlobalEnv$rw.survey.expansion = X.expansion
  .GlobalEnv$rw.survey.nonexpansion = X.nonexpansion
  
  # Small constant to avoid logit errors
  epsilon = 1e-6  
  X = pmax(pmin(X, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
  X.expansion = pmax(pmin(X.expansion, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
  X.nonexpansion = pmax(pmin(X.nonexpansion, 1 - epsilon), epsilon)  # Ensure all values are within (0,1)
  
  
  
  # Arcsin transformation
  arcsin_X = asin(sqrt(X))
  arcsin_X.expansion = asin(sqrt(X.expansion))
  arcsin_X.nonexpansion = asin(sqrt(X.nonexpansion))
  
  # Fit KDE in arcsin space
  kde_fit = kde(x = arcsin_X )
  
  kde_fit_marginals = apply(arcsin_X, 2, kde)
  kde_fit.expansion_marginals = apply(arcsin_X.expansion, 2, kde)
  kde_fit.nonexpansion_marginals = apply(arcsin_X.nonexpansion, 2, kde)
  
  # Sample 'n' points in logit space
  sampled_arcsin = rkde(n, kde_fit)
  
  # Figure out what is the quantile for each sampled point, according to the marginals
  sampled_quantiles = lapply(1:3, function(i){
   #   pkde(sampled_arcsin[,i], kde_fit_marginals[[i]])
      sapply(sampled_arcsin[,i], function(x){
        mean(x>sampled_arcsin[,i])
      })
  })
  
  sampled_arcsin.expansion = sapply(1:3, function(i){
      qkde(sampled_quantiles[[i]], kde_fit.expansion_marginals[[i]])
  })
  
  sampled_arcsin.nonexpansion = sapply(1:3, function(i){
      qkde(sampled_quantiles[[i]], kde_fit.nonexpansion_marginals[[i]])
  })
  
  # Transform back using inverse logit
#  sampled_values = (sin(sampled_arcsin))^2
  sampled_values.expansion = (sin(sampled_arcsin.expansion))^2
  sampled_values.nonexpansion = (sin(sampled_arcsin.nonexpansion))^2
 
  sampled_values.exp.nonexp = cbind(sampled_values.expansion,
                                    sampled_values.nonexpansion)
   
  ## Ensure the output is a 3 x n matrix
 # rv = t(sampled_values)  # Transpose to match the required format
  rv = t(sampled_values.exp.nonexp)
  
#  dimnames(rv)[[1]] = c('lose.adap.effect', 'lose.oahs.effect', 'lose.rw.support.effect')
  dimnames(rv)[[1]] = c('lose.adap.expansion.effect', 'lose.oahs.expansion.effect', 'lose.rw.support.expansion.effect',
                        'lose.adap.nonexpansion.effect', 'lose.oahs.nonexpansion.effect', 'lose.rw.support.nonexpansion.effect')
  
  rv
}

get_suppresion_effect_logit = function(n, rw_survey, max.p=0.99, min.p = 1-max.p) {
  
  p.span = max.p - min.p
  
  # Mask by medicaid status
  expansion.states = names(STATE.MEDICAID.EXPANSION)[STATE.MEDICAID.EXPANSION]
  expansion.states = gsub("_",".",expansion.states)
  expansion.col.names = paste0("X.choice.", expansion.states, ".")
  
  medicaid.expansion.mask = rowSums(rw_survey[,expansion.col.names]) > 0
  
  
  non.expansion.states = names(STATE.MEDICAID.EXPANSION)[!STATE.MEDICAID.EXPANSION]
  non.expansion.states = gsub("_",".",non.expansion.states)
  non.expansion.col.names = paste0("X.choice.", non.expansion.states, ".")
  
  medicaid.nonexpansion.mask = rowSums(rw_survey[,non.expansion.col.names]) > 0
  
  # Extract relevant columns, remove rows with NA values, and scale 0->1
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X.expansion = X[medicaid.expansion.mask,]
  X.nonexpansion = X[medicaid.nonexpansion.mask,]
  
  X = X[complete.cases(X), ]
  X.expansion = X.expansion[complete.cases(X.expansion), ]
  X.nonexpansion = X.nonexpansion[complete.cases(X.nonexpansion), ]
  
  
  
  ## Convert to matrix for KDE estimation
  X = as.matrix(X)/100
  X.expansion = as.matrix(X.expansion)/100
  X.nonexpansion = as.matrix(X.nonexpansion)/100
  
  .GlobalEnv$rw.survey = X
  .GlobalEnv$rw.survey.expansion = X.expansion
  .GlobalEnv$rw.survey.nonexpansion = X.nonexpansion
  
  # Small constant to avoid logit errors
  X = min.p + X * p.span  # Ensure all values are within (0,1)
  X.expansion = min.p + X.expansion * p.span  # Ensure all values are within (0,1)
  X.nonexpansion = min.p + X.nonexpansion * p.span  # Ensure all values are within (0,1)
  
  
  # Logit transformation
  logit_X = log(X) - log(1-X)
  logit_X.expansion = log(X.expansion) - log(1-X.expansion)
  logit_X.nonexpansion = log(X.nonexpansion) - log(1-X.nonexpansion)
  
  # Fit KDE in logit space
  kde_fit = kde(x = logit_X )
  
  kde_fit_marginals = apply(logit_X, 2, kde)
  kde_fit.expansion_marginals = apply(logit_X.expansion, 2, kde)
  kde_fit.nonexpansion_marginals = apply(logit_X.nonexpansion, 2, kde)
  
  # Sample 'n' points in logit space
  sampled_logit = rkde(n, kde_fit)
  
  # Figure out what is the quantile for each sampled point, according to the marginals
  sampled_quantiles = lapply(1:3, function(i){
#    pkde(sampled_logit[,i], kde_fit_marginals[[i]])
    sapply(sampled_logit[,i], function(x){
      mean(x>sampled_logit[,i])
    })
  })
  
  sampled_logit.expansion = sapply(1:3, function(i){
    qkde(sampled_quantiles[[i]], kde_fit.expansion_marginals[[i]])
  })
  
  sampled_logit.nonexpansion = sapply(1:3, function(i){
    qkde(sampled_quantiles[[i]], kde_fit.nonexpansion_marginals[[i]])
  })
  
  # Transform back using inverse logit
  #  sampled_values = (sin(sampled_logit))^2
  sampled_values.expansion = 1 / (1+exp(-sampled_logit.expansion))
  sampled_values.nonexpansion = 1 / (1+exp(-sampled_logit.nonexpansion))

  sampled_values.exp.nonexp = cbind(sampled_values.expansion,
                                    sampled_values.nonexpansion)
  
  ## Ensure the output is a 3 x n matrix
  # rv = t(sampled_values)  # Transpose to match the required format
  rv = t(sampled_values.exp.nonexp)
  
  #  dimnames(rv)[[1]] = c('lose.adap.effect', 'lose.oahs.effect', 'lose.rw.support.effect')
  dimnames(rv)[[1]] = c('lose.adap.expansion.effect', 'lose.oahs.expansion.effect', 'lose.rw.support.expansion.effect',
                        'lose.adap.nonexpansion.effect', 'lose.oahs.nonexpansion.effect', 'lose.rw.support.nonexpansion.effect')
  
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
#RW.effect.values = get_suppresion_effect_logit(1000, rw_survey)
set.seed(reset.seed)

load('../jheem_analyses/applications/ryan_white/adjusted.RW.effect.values.Rdata')
    # gives us adjusted.RW.effect.values

if (1==2) # for checking the simulated distribution approximates the observed one
{
    cbind(rowMeans(RW.effect.values),
          c(colMeans(rw.survey.expansion), colMeans(rw.survey.nonexpansion)))
    cbind(apply(RW.effect.values, 1, median),
          c(apply(rw.survey.expansion, 2, median), apply(rw.survey.nonexpansion, 2, median)))
    p=0.25;cbind(apply(RW.effect.values, 1, quantile, probs=p),
                 c(apply(rw.survey.expansion, 2, quantile, probs=p), apply(rw.survey.nonexpansion, 2, quantile, probs=p)))
    p=0.75;cbind(apply(RW.effect.values, 1, quantile, probs=p),
                 c(apply(rw.survey.expansion, 2, quantile, probs=p), apply(rw.survey.nonexpansion, 2, quantile, probs=p)))
}
#Interventions are scaled up linearly from July 1st of START.YEAR to October 1st of IMPLEMENTED.BY.YEAR
START.YEAR = 2025.5
LOSS.LAG = 0.25
BRIEF.INTERRUPTION.RESTART.YEAR = 2027
PROLONGED.INTERRUPTION.RESTART.YEAR = 2029
RESTART.LAG = 1

# Complete Loss of ADAP
adap.cessation.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                   start.time = START.YEAR,
                                                   effect.values = expression(1-lose.adap.expansion.effect),
                                                   apply.effects.as = 'value',
                                                   scale = 'proportion',
                                                   times = START.YEAR + LOSS.LAG,
                                                   allow.values.less.than.otherwise = T,
                                                   allow.values.greater.than.otherwise = F )

adap.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                             start.time = START.YEAR,
                                                             effect.values = expression(1-lose.adap.nonexpansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )


# Complete Loss of OAHS
oahs.cessation.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                             start.time = START.YEAR,
                                                             effect.values = expression(1-lose.oahs.expansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )

oahs.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                             start.time = START.YEAR,
                                                             effect.values = expression(1-lose.oahs.nonexpansion.effect),
                                                             apply.effects.as = 'value',
                                                             scale = 'proportion',
                                                             times = START.YEAR + LOSS.LAG,
                                                             allow.values.less.than.otherwise = T,
                                                             allow.values.greater.than.otherwise = F )


# Complete Loss of RW support
rw.support.cessation.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                         start.time = START.YEAR,
                                                         effect.values = expression(1-lose.rw.support.expansion.effect),
                                                         apply.effects.as = 'value',
                                                         scale = 'proportion',
                                                         times = START.YEAR + LOSS.LAG,
                                                         allow.values.less.than.otherwise = T,
                                                         allow.values.greater.than.otherwise = F )

rw.support.cessation.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                   start.time = START.YEAR,
                                                                   effect.values = expression(1-lose.rw.support.nonexpansion.effect),
                                                                   apply.effects.as = 'value',
                                                                   scale = 'proportion',
                                                                   times = START.YEAR + LOSS.LAG,
                                                                   allow.values.less.than.otherwise = T,
                                                                   allow.values.greater.than.otherwise = F )


rw.cessation = create.intervention(adap.cessation.expansion.effect,
                                   adap.cessation.nonexpansion.effect,
                                   oahs.cessation.expansion.effect,
                                   oahs.cessation.nonexpansion.effect,
                                   rw.support.cessation.expansion.effect, 
                                   rw.support.cessation.nonexpansion.effect, 
                                   parameters = RW.effect.values, 
                                   WHOLE.POPULATION, 
                                   code = "rw.end")

rw.cessation.conservative = create.intervention(adap.cessation.expansion.effect,
                                                adap.cessation.nonexpansion.effect,
                                                oahs.cessation.expansion.effect,
                                                oahs.cessation.nonexpansion.effect,
                                                rw.support.cessation.expansion.effect, 
                                                rw.support.cessation.nonexpansion.effect, 
                                                parameters = adjusted.RW.effect.values, 
                                                WHOLE.POPULATION, 
                                                code = "rw.end.cons")

# Temporary Lapse of ADAP
adap.brief.interruption.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                            start.time = START.YEAR,
                                                            end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.adap.expansion.effect,1-lose.adap.expansion.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

adap.brief.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                      start.time = START.YEAR,
                                                                      end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                      effect.values = expression(c(1-lose.adap.nonexpansion.effect,1-lose.adap.nonexpansion.effect)),
                                                                      apply.effects.as = 'value',
                                                                      scale = 'proportion',
                                                                      times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                                      allow.values.less.than.otherwise = T,
                                                                      allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
oahs.brief.interruption.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                            start.time = START.YEAR,
                                                            end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.oahs.expansion.effect,1-lose.oahs.expansion.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

oahs.brief.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                      start.time = START.YEAR,
                                                                      end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                      effect.values = expression(c(1-lose.oahs.nonexpansion.effect,1-lose.oahs.nonexpansion.effect)),
                                                                      apply.effects.as = 'value',
                                                                      scale = 'proportion',
                                                                      times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                                      allow.values.less.than.otherwise = T,
                                                                      allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
rw.support.brief.interruption.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                  start.time = START.YEAR,
                                                                  end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.rw.support.expansion.effect,1-lose.rw.support.expansion.effect)),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

rw.support.brief.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                            start.time = START.YEAR,
                                                                            end.time = BRIEF.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                            effect.values = expression(c(1-lose.rw.support.nonexpansion.effect,1-lose.rw.support.nonexpansion.effect)),
                                                                            apply.effects.as = 'value',
                                                                            scale = 'proportion',
                                                                            times = c(START.YEAR + LOSS.LAG, BRIEF.INTERRUPTION.RESTART.YEAR),
                                                                            allow.values.less.than.otherwise = T,
                                                                            allow.values.greater.than.otherwise = F )

rw.brief.interruption = create.intervention(adap.brief.interruption.expansion.effect,
                                            adap.brief.interruption.nonexpansion.effect,
                                            oahs.brief.interruption.expansion.effect,
                                            oahs.brief.interruption.nonexpansion.effect,
                                            rw.support.brief.interruption.expansion.effect,
                                            rw.support.brief.interruption.nonexpansion.effect,
                                            parameters = RW.effect.values, 
                                            WHOLE.POPULATION,
                                            code = "rw.b.intr")

rw.brief.interruption.conservative = create.intervention(adap.brief.interruption.expansion.effect,
                                                         adap.brief.interruption.nonexpansion.effect,
                                                         oahs.brief.interruption.expansion.effect,
                                                         oahs.brief.interruption.nonexpansion.effect,
                                                         rw.support.brief.interruption.expansion.effect,
                                                         rw.support.brief.interruption.nonexpansion.effect,
                                                         parameters = adjusted.RW.effect.values, 
                                                         WHOLE.POPULATION,
                                                         code = "rw.b.intr.cons")

# Temporary Lapse of ADAP
adap.prolonged.interruption.expansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.expansion.effect',
                                                            start.time = START.YEAR,
                                                            end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.adap.expansion.effect,1-lose.adap.expansion.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

adap.prolonged.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'adap.suppression.nonexpansion.effect',
                                                                          start.time = START.YEAR,
                                                                          end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                          effect.values = expression(c(1-lose.adap.nonexpansion.effect,1-lose.adap.nonexpansion.effect)),
                                                                          apply.effects.as = 'value',
                                                                          scale = 'proportion',
                                                                          times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                                          allow.values.less.than.otherwise = T,
                                                                          allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
oahs.prolonged.interruption.expansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.expansion.effect',
                                                            start.time = START.YEAR,
                                                            end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                            effect.values = expression(c(1-lose.oahs.expansion.effect,1-lose.oahs.expansion.effect)),
                                                            apply.effects.as = 'value',
                                                            scale = 'proportion',
                                                            times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                            allow.values.less.than.otherwise = T,
                                                            allow.values.greater.than.otherwise = F )

oahs.prolonged.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'oahs.suppression.nonexpansion.effect',
                                                                          start.time = START.YEAR,
                                                                          end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                          effect.values = expression(c(1-lose.oahs.nonexpansion.effect,1-lose.oahs.nonexpansion.effect)),
                                                                          apply.effects.as = 'value',
                                                                          scale = 'proportion',
                                                                          times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                                          allow.values.less.than.otherwise = T,
                                                                          allow.values.greater.than.otherwise = F )

# Temporary Lapse of ADAP
rw.support.prolonged.interruption.expansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.expansion.effect',
                                                                  start.time = START.YEAR,
                                                                  end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                  effect.values = expression(c(1-lose.rw.support.expansion.effect,1-lose.rw.support.expansion.effect)),
                                                                  apply.effects.as = 'value',
                                                                  scale = 'proportion',
                                                                  times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                                  allow.values.less.than.otherwise = T,
                                                                  allow.values.greater.than.otherwise = F )

rw.support.prolonged.interruption.nonexpansion.effect = create.intervention.effect(quantity.name = 'rw.support.suppression.nonexpansion.effect',
                                                                                start.time = START.YEAR,
                                                                                end.time = PROLONGED.INTERRUPTION.RESTART.YEAR + RESTART.LAG,
                                                                                effect.values = expression(c(1-lose.rw.support.nonexpansion.effect,1-lose.rw.support.nonexpansion.effect)),
                                                                                apply.effects.as = 'value',
                                                                                scale = 'proportion',
                                                                                times = c(START.YEAR + LOSS.LAG, PROLONGED.INTERRUPTION.RESTART.YEAR),
                                                                                allow.values.less.than.otherwise = T,
                                                                                allow.values.greater.than.otherwise = F )

rw.prolonged.interruption = create.intervention(adap.prolonged.interruption.expansion.effect,
                                                adap.prolonged.interruption.nonexpansion.effect,
                                                oahs.prolonged.interruption.expansion.effect,
                                                oahs.prolonged.interruption.nonexpansion.effect,
                                                rw.support.prolonged.interruption.expansion.effect,
                                                rw.support.prolonged.interruption.nonexpansion.effect,
                                                parameters = RW.effect.values, 
                                                WHOLE.POPULATION,
                                                code = "rw.p.intr")

rw.prolonged.interruption.conservative = create.intervention(adap.prolonged.interruption.expansion.effect,
                                                adap.prolonged.interruption.nonexpansion.effect,
                                                oahs.prolonged.interruption.expansion.effect,
                                                oahs.prolonged.interruption.nonexpansion.effect,
                                                rw.support.prolonged.interruption.expansion.effect,
                                                rw.support.prolonged.interruption.nonexpansion.effect,
                                                parameters = adjusted.RW.effect.values, 
                                                WHOLE.POPULATION,
                                                code = "rw.p.intr.cons")
