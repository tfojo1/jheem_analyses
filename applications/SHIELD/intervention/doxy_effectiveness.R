# Doxy-PEP Efficacy: Monte Carlo Simulation
#
# This section represents uncertainty in the efficacy of doxycycline post-exposure
# prophylaxis (doxy-PEP) against incident syphilis.
#
# Instead of assigning a single fixed efficacy value, we generate 1,000 possible
# relative risk (RR) values based on the reported RR and 95% confidence interval.
# Each simulated RR is then converted to an efficacy estimate using:
#
#     Efficacy = 1 - RR
#
# Clinical evidence:
# Luetkemeyer et al. reported an RR of 0.20 (95% CI: 0.08–0.48) for incident
# early syphilis comparing the doxy-PEP group with the standard-care group.
# The reported RR corresponds to an estimated efficacy of:
#
#     1 - 0.20 = 0.80 (80%)
#
# The RR and 95% CI are used to parameterize a lognormal distribution, which
# represents uncertainty around the estimated treatment effect.
# ------------------------------------------------------------------------------
# Fit a lognormal distribution to the reported RR and 95% CI
# ------------------------------------------------------------------------------
fit_rr_lognorm_from_mean_ci <- function(
        rr_mean,
        rr_lo,
        rr_hi,
        p_lo = 0.025,
        p_hi = 0.975
) {

    # RR values must be greater than zero for a lognormal distribution.
    if (any(c(rr_mean, rr_lo, rr_hi) <= 0)) {
        stop("RR point estimate and confidence limits must be > 0.")
    }
    
    # Calculate the width of the 95% confidence interval on the
    # standard-normal scale.
    zspan <- qnorm(p_hi) - qnorm(p_lo)
    
    # Estimate the standard deviation on the log scale from the
    # reported 95% confidence interval.
    sdlog <- (log(rr_hi) - log(rr_lo)) / zspan
    
    # Calculate the mean on the log scale.
    # For a lognormal distribution:
    # E(X) = exp(meanlog + 0.5 * sdlog^2)
    meanlog <- log(rr_mean) - 0.5 * sdlog^2
    
    list(
        meanlog = meanlog,
        sdlog = sdlog
    )
}
# ------------------------------------------------------------------------------
# Generate Monte Carlo samples of relative risk
# ------------------------------------------------------------------------------
draw_rr_lognorm <- function(
        n,
        rr_mean,
        rr_lo,
        rr_hi,
        cap_at_one = TRUE
) {
    
    # Estimate the lognormal parameters.
    p <- fit_rr_lognorm_from_mean_ci(
        rr_mean = rr_mean,
        rr_lo   = rr_lo,
        rr_hi   = rr_hi
    )
    
    # Generate random RR values from the fitted lognormal distribution.
    rr <- rlnorm(
        n,
        meanlog = p$meanlog,
        sdlog   = p$sdlog
    )
    
    # Restrict RR to a maximum of 1 if requested.
    # RR = 1 represents no treatment effect.
    if (cap_at_one) {
        rr <- pmin(rr, 1)
    }
    
    return(rr)
}

# 
# # ------------------------------------------------------------------------------
# # Generate 1,000 simulated RR values
# # ------------------------------------------------------------------------------
# 
# # Generate 1,000 plausible RR values using the reported estimate:
# #     RR = 0.20
# #     95% CI = 0.08–0.48
# 
# doxy_rr_draws <- draw_rr_lognorm(
#     n          = 1000,
#     rr_mean    = 0.20,
#     rr_lo      = 0.08,
#     rr_hi      = 0.48,
#     cap_at_one = TRUE
# )
# 
# # ------------------------------------------------------------------------------
# # Convert RR to doxy-PEP efficacy
# # ------------------------------------------------------------------------------
# 
# effectiveness_samples <- 1 - doxy_rr_draws
