
#' @title Make Joint Multivariate Spline Prior
#' @description This make can a multivariate joint distribution for any number of parameters and spline points.
#' @param parameters Parameters for which spline point values will be correlated
#' @param logmean.baseline,logsd.baseline A vector of log means and sds for the baseline year, each with one value per parameter.
#' @param logsd.deltas.past,logsd.deltas.future A vector of log sd's, one per spline point before, or after, the baseline year
#' @param spline.times A character vector of years indicating, in order, the past spline point years, the baseline year, and the future years
#' @param correlation The correlation between parameter values for the same spline point year
make.joint.mv.spline.prior <- function(parameters = c("A", "B"),
                                       logmean.baseline = log(2),
                                       logsd.baseline = 0.01,
                                       logsd.deltas.past = 1:2,
                                       logsd.deltas.future = 5:6,
                                       spline.times = c("1970", "1995", "2000", "2010", "2020"),
                                       correlation = 0.5,
                                       debug = F) {
    
    if (debug) browser()
    # Validate arguments ----
    error.prefix = "error making joint multivariate spline prior: "
    if (!is.character(parameters) || length(parameters)==0 || any(is.na(parameters)))
        stop(paste0(error.prefix, "'parameters' must be a character vector with no missing values"))
    if (!is.numeric(logmean.baseline) || length(logmean.baseline) != 1 || any(is.na(logmean.baseline)))
        stop(paste0(error.prefix, "'logmean.baseline' must be a single numeric value"))
    if (!is.numeric(logsd.baseline) || length(logsd.baseline) != 1 || any(is.na(logsd.baseline)) || logsd.baseline<=0)
        stop(paste0(error.prefix, "'logsd.baseline' must be a single, positive numeric value"))
    if (!is.numeric(logsd.deltas.past) || any(is.na(logsd.deltas.past)) || any(logsd.deltas.past<=0))
        stop(paste0(error.prefix, "'logsd.deltas.past' must be a vector containing only positive numeric values. Can have length 0."))
    if (!is.numeric(logsd.deltas.future) || any(is.na(logsd.deltas.future)) || any(logsd.deltas.future<=0))
        stop(paste0(error.prefix, "'logsd.deltas.future' must be a vector containing only positive numeric values. Can have length 0."))
    if (!is.character(spline.times) || length(spline.times)!=(1 + length(logsd.deltas.past) + length(logsd.deltas.future)) || any(is.na(spline.times)))
        stop(paste0(error.prefix, "'spline.times' must be a character vector with one value per spline time"))
    if (!is.numeric(correlation) || length(correlation)!=1 || is.na(correlation) || correlation < -1 || correlation > 1)
        stop(paste0(error.prefix, "'correlation' must be a single numeric value between 1 and -1 inclusive"))
    
    num_past_years <- length(logsd.deltas.past)
    num_future_years <- length(logsd.deltas.future)
    num_spline_points <- num_past_years + 1 + num_future_years
    num_parameters <- length(parameters)
    
    
    # Create the untransformed mu and sigma ----
    # The untransformed mu has zero for all the log deltas, since we expect no change between years.
    untransformed_mu <- rep(c(logmean.baseline, rep(0, num_spline_points - 1)), each=num_parameters)
    
    # Since the parameters are correlated, the untransformed sigma is no longer diagonal,
    # but rather block-diagonal by spline point.
    # The first block is for the baseline and is diagonal with the logsd.baseline on its diagonals.
    # The other blocks each contain their own logsd.delta squared,
    # and their off-diagonals are multipled by the correlation.
    
    # The following code produces this sigma matrix in stages.
    n_col <- num_parameters * num_spline_points
    block_size <- num_parameters
    
    # Start fresh
    untransformed_sigma <- diag(n_col)
    
    # Change all diagonal blocks (including the baseline block) to variance times correlation
    diagonal_block_idx <- rep(1:block_size, n_col) +
        rep((1:n_col) * n_col - n_col, each=block_size) +
        rep(1:(n_col / block_size) * block_size - block_size, each = block_size^2)
    
    untransformed_sigma[diagonal_block_idx] <- rep(c(
        logsd.baseline,
        logsd.deltas.past,
        logsd.deltas.future
    )^2, each = block_size^2) * correlation
    
    # Divide the diagonals by correlation so only the off-diagonals have it
    diag(untransformed_sigma) <- diag(untransformed_sigma) / correlation
    
    # Reset the baseline block, which is first, to be diagonal with its own variance
    untransformed_sigma[1:block_size, 1:block_size] <- diag(logsd.baseline^2, nrow = block_size, ncol = block_size)
    
    # Create the mapping matrix M ----
    # Now create a matrix M that can map from the space of
    # [baseline log trates, past log deltas..., future log deltas...]
    # to [past log trates..., baseline log trates, future log trates...]
    # Note that if theta(2020 vs 2010) = trate(2020) / trate(2010)
    # and theta(2010 vs 2000) = trate(2010) / trate(2000),
    # where 2000 is the baseline year,
    # then log(trate(2020)) = log(trate(2000)) + log(theta(2010 vs 2000)) + log(theta(2020 vs 2010))
    
    # The only complexity that having multiple parameters adds is that
    # each 1 in the mapping matrix for spline points described above
    # becomes an identity matrix. This is because cells corresponding to certain
    # parameters will never map to cells corresponding to a different parameter.
    
    # M might look like this before considering multiple parameters:
    
    # rbind(c(1,1,0,0), # a past year (= baseline + past year's delta)
    #       c(1,0,0,0), # the baseline year
    #       c(1,0,1,0), # the 1st future year (= baseline + 1st future year's delta)
    #       c(1,0,1,1)) # the 2nd future year (= baseline + 1st future year's delta + 2nd future year's delta)
    
    # After expanding to consider 2 parameters, M might look like this:
    
    # rbind(c(1,0,1,0,0,0,0,0),
    #       c(0,1,0,1,0,0,0,0),
    #       c(1,0,0,0,0,0,0,0), # parameter 1's baseline
    #       c(0,1,0,0,0,0,0,0), # parameter 2's baseline
    #       c(1,0,0,0,1,0,0,0),
    #       c(0,1,0,0,0,1,0,0),
    #       c(1,0,0,0,1,0,1,0),
    #       c(0,1,0,0,0,1,0,1))
    
    M_per_parameter <- matrix(0, nrow = num_spline_points, ncol = num_spline_points)

    # The entire first column will be 1s because every value uses the baseline trate.
    M_per_parameter[,1] <- 1
    
    # Each past year has 1s in the columns for itself and later years
    # E.g. 1970 might have 1s for 1970 and 1995 (along with the 2000 baseline)
    for (i in seq_len(num_past_years)) {
        M_per_parameter[i,
                        2 + 0 : (num_past_years - i)] <- 1
    }
    
    # Each future year has 1s in the columns for itself and earlier years
    # E.g. 2020 might have 1s for 2020 and 2010 (along with the 2000 baseline)
    for (i in seq_len(num_future_years)) {
        M_per_parameter[i + 1 + num_past_years,
                        1 + num_past_years + 1:i] <- 1
    }
    
    # Now expand it to consider all the parameters, as described above
    M <- kronecker(M_per_parameter, diag(num_parameters))
    
    # Transform mu and sigma and create distribution ----
    mu = M %*% untransformed_mu # E[y] = M E[x]
    
    sigma = M  %*% untransformed_sigma %*% t(M) # Σ_y = M Σ_x Mᵀ 
    
    var_names <- paste0(rep(parameters, num_spline_points),
                        rep(spline.times, each = num_parameters))
    
    dist = Multivariate.Lognormal.Distribution(mu = mu, sigma = sigma, 
                                               var.names = var_names)
    return(dist)
    
}

# make.mv.spline.prior(parameter = "transmission.rate.multiplier.msm", #relative to heterosexuals
#                      logmean00 = log(3),logsd00 = log(2), #reference year 2000 # 3X transmission assumption based on https://pmc.ncbi.nlm.nih.gov/articles/PMC11307151
#                      #
#                      logsd.delta95 = log(sqrt(1.5))/2, #'@Ryan: why are we using these values?
#                      logsd.delta90 = log(sqrt(1.5))/2, 
#                      logsd.delta70 = log(1.5^2)/2,
#                      logsd.delta10 = log(1.5)/2, 
#                      logsd.delta17 = log(1.5)/2 # change this to 2017
# ),