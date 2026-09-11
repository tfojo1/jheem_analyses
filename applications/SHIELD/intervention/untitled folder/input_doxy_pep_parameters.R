# DOXY-PEP EFFECTIVENESS # -----
# we have 3 sets of estiamtes on HR of Syphilis diagnosis among doxy-pep recipients from clinical trials
# IPERGY: HR:0·20, CI: 0·08–0·48
# Doxy-EP: HR: 0·27; CI 0·07–0·98 
# Doxy-VAC HR: 0·21; CI 0·11–0·41

#combining them
df <- data.frame(
    study = c("IPERGAY", "Doxy-PEP", "Doxy-VAC"),
    hr    = c(0.20, 0.27, 0.21),
    lb    = c(0.08, 0.07, 0.11),
    hb    = c(0.48, 0.98, 0.41)
)
df
# Log-transform
df$log_hr <- log(df$hr)

# Correct SE calculation 
# CI= log(HR) +-1.96 * SE >>> SE= CI-width / (2 * 1.96)
df$se <- (log(df$hb) - log(df$lb)) / (2 * 1.96)

# Weights (inverse variance)
df$w <- 1 / df$se^2

# ON THE LOG SCALE:
# log(HR) ~ Normal(mean, sd) >>> HR=lognormal(meanlog,sdlog)
# Pooled estimate of mean
pooled_meanlog <- sum(df$log_hr * df$w) / sum(df$w); pooled_log_hr
# Pooled estimate of se
pooled_selog <- sqrt(1 / sum(df$w)); pooled_se
# CI
pooled_hr_lo <- (pooled_meanlog - 1.96 * pooled_selog); pooled_hr_lo
pooled_hr_hi <- (pooled_meanlog + 1.96 * pooled_selog); pooled_hr_hi

# Back-transform
# rr_range    <- exp(c(pooled_meanlog,pooled_hr_lo,pooled_hr_hi));estimates

# final params
rr_params=c(meanlog=pooled_meanlog,sdlog=pooled_selog);rr_params

#draw random rr:
rr_samples <- rlnorm(n, meanlog = rr_params['meanlog'], sdlog = rr_params['sdlog']) 

# DOXY-PEP UPTAKE # -----
# reported proportion of general population and prescribed Doxy PEP across the different studies
prescribed_estimates <- c(0.21, 0.251, 0.20, 0.15)

p_mean <- mean(prescribed_estimates)
p_sd   <- sd(prescribed_estimates)
p_lo   <- min(prescribed_estimates)
p_hi   <- max(prescribed_estimates)
c(p_mean,p_sd,p_lo,p_hi)


#' Fit beta from mean and range (treating range as approximate 95% CI)
#'
fit_beta_from_mean_range <- function(p_mean, p_lo, p_hi) {
    # Approximate SD from range
    p_sd <- (p_hi - p_lo) / (2 * 1.96)
    
    # Method of moments
    # var = mean*(1-mean) / (alpha + beta + 1)
    # Solve for (alpha + beta)
    common <- p_mean * (1 - p_mean) / p_sd^2 - 1
    alpha  <- p_mean * common
    beta   <- (1 - p_mean) * common
    
    list(alpha = alpha, beta = beta, p_mean = p_mean, p_sd = p_sd)
}

# Fit
uptake_params <- fit_beta_from_mean_range(p_mean = 0.203, p_lo = 0.15, p_hi = 0.251);params
# draw random samples
uptake_samples <- rbeta(10000, shape1 = params$alpha, shape2 = params$beta)
