#Estimate meanlog and SDlog of doxypep effectiveness

fit_rr_lognorm_from_ci <- function(rr_lo, rr_hi, p_lo = 0.025, p_hi = 0.975){
    z_lo  <- qnorm(p_lo)
    z_hi  <- qnorm(p_hi)
    zspan <- z_hi - z_lo
    
    sdlog   <- (log(rr_hi) - log(rr_lo)) / zspan
    meanlog <- (log(rr_hi) + log(rr_lo)) / 2   # midpoint in log-space
    
    implied_mean <- exp(meanlog + 0.5 * sdlog^2)
    
    list(meanlog = meanlog, sdlog = sdlog, implied_mean = implied_mean)
}

q <- fit_rr_lognorm_from_ci(0.08, 0.48)
q
