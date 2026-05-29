# **************************************************************************************************************************************************************************
# make.mv.spline.prior.five.points ----
# (Ryan's initial version - only works with 5 points)
# **************************************************************************************************************************************************************************
#' @title Make Multivariate Spline Prior
#' @description
#' Builds a single-parameter multivariate lognormal prior over values at
#' six spline years: 1970, 1990, 1995, 2000, 2010, and 2017.
#'
#' The model is parameterized using:
#' - a baseline log-mean and log-SD at year 2000
#' - independent log-delta terms for changes between spline knots
#'
#' The latent variables are assumed independent on the log scale, and a
#' linear transformation matrix is used to map them into correlated
#' year-specific values.
#'
#' @param parameter Character string naming the parameter being modeled.
#' @param logmean00 Numeric scalar. Mean of the log-parameter at year 2000.
#' @param logsd00 Numeric scalar. Standard deviation of the log-parameter at year 2000.
#' @param logsd.delta95 Numeric scalar. Standard deviation of the log-delta for 1995 relative to 2000.
#' @param logsd.delta90 Numeric scalar. Standard deviation of the log-delta for 1990 relative to 1995.
#' @param logsd.delta70 Numeric scalar. Standard deviation of the log-delta for 1970 relative to 1990.
#' @param logsd.delta10 Numeric scalar. Standard deviation of the log-delta for 2010 relative to 2000.
#' @param logsd.delta17 Numeric scalar. Standard deviation of the log-delta for 2017 relative to 2010.
#'
#' @return A Multivariate.Lognormal.Distribution object for the parameter
#' values at the six spline years.
make.mv.spline.prior.five.points <- function(parameter,
                                 logmean00,
                                 logsd00,
                                 logsd.delta95,
                                 logsd.delta90,
                                 logsd.delta70,
                                 logsd.delta10,
                                 logsd.delta17) {
    
    # Latent variables: (deltas are calculated between adjacent spline points)
    # x1 = log transmission rate at 2000: log(t2000)
    # x2 = log-change from 2000 to 1995:
    #      x2 = log(t1995) - log(t2000)
    # x3 = log-change from 1995 to 1990:
    #      x3 = log(t1990) - log(t1995)
    # x4 = log-change from 1990 to 1970:
    #      x4 = log(t1970) - log(t1990)
    # x5 = log-change from 2000 to 2010:
    #      x5 = log(t2010) - log(t2000)
    # x6 = log-change from 2010 to 2017:
    #      x6 = log(t2017) - log(t2010)
    #
    # We collect these into a latent vector:
    # X = [x1, x2, x3, x4, x5, x6]^T 
    # X has a MVN distribution with a given mean and variance
    
    # The year-specific log transmission rates are then formed by accumulating
    # the baseline and the relevant deltas:
    # log(t1970) = x1 + x2 + x3 + x4
    # log(t1990) = x1 + x2 + x3
    # log(t1995) = x1 + x2
    # log(t2000) = x1
    # log(t2010) = x1 + x5
    # log(t2017) = x1 + x5 + x6
    #
    # In matrix form, Y = M %*% X
    # where Y is the vector of year-specific log transmission rates.
    # The goal is to derive the distribution of Y from the distribution of X.
    # The math is E[y]=M*E[X]     Var(Y)= M * Var(X) * M'
    # we need to construct the transformation matrix M
    
    untransformed.mu <- c(logmean00, #for t2000
                          0, #for theta95
                          0, #for theta90
                          0, #for theta70
                          0, #for theta10
                          0 #for theta17
    )
    
    # Independent latent covariance matrix (all off diagonal values=0)
    # Use variances, not standard deviations.
    untransformed.sigma <- diag(c(
        logsd00^2,
        logsd.delta95^2,
        logsd.delta90^2,
        logsd.delta70^2,
        logsd.delta10^2,
        logsd.delta17^2
    ))
    
    # Transformation matrix from latent variables to year-specific log values.
    # The idea is to start with a small set of independent latent variables and then build the value at each year by adding up the pieces that apply to that year.
    #
    # log P_1970 = x1 + x2 + x3 + x4
    # log P_1990 = x1 + x2 + x3
    # log P_1995 = x1 + x2
    # log P_2000 = x1
    # log P_2010 = x1 + x5
    # log P_2017 = x1 + x5 + x6
    M <- rbind(
        c(1, 1, 1, 1, 0, 0),  # 1970
        c(1, 1, 1, 0, 0, 0),  # 1990
        c(1, 1, 0, 0, 0, 0),  # 1995
        c(1, 0, 0, 0, 0, 0),  # 2000
        c(1, 0, 0, 0, 1, 0),  # 2010
        c(1, 0, 0, 0, 1, 1)   # 2017
    )
    
    # Transform mean and covariance to the observed year-specific values.
    mu <- M %*% untransformed.mu
    sigma <- M %*% untransformed.sigma %*% t(M)
    
    dist <- Multivariate.Lognormal.Distribution(
        mu = mu,
        sigma = sigma,
        var.names = paste0(parameter, c(1970, 1990, 1995, 2000, 2010, 2017))
    )
    
    return(dist)
}

if (1==2){
    # make.mv.spline.prior.five.points(parameter = "transmission.rate.multiplier.heterosexual", 
    #                      logmean00 = 0,
    #                      logsd00 = log(2),  
    #                      logsd.delta95 = log(sqrt(1.5))/2, 
    #                      logsd.delta90 = log(sqrt(1.5))/2, 
    #                      logsd.delta70 = log(1.5^2)/2,
    #                      logsd.delta10 = log(1.5)/2, 
    #                      logsd.delta17 = log(1.5)/2 
    # ),
}
# **************************************************************************************************************************************************************************
# make.mv.spline.prior ----
# (geenral version works with any number of points)
# **************************************************************************************************************************************************************************
#' @title Make Multivariate Spline Prior
#' @description
#' Constructs a joint multivariate lognormal prior for one or more parameters
#' across a set of spline years, using an explicit baseline year and
#' independent latent log-delta terms.
#'
#' The latent vector consists of:
#' - baseline log values at the baseline year, one per parameter
#' - log-deltas for each non-baseline spline year, one per parameter
#'
#' The year-specific log values are obtained by a linear transformation of the
#' latent vector.
#'
#' @param parameters Character vector of parameter names.
#' @param logmean.baseline Numeric vector of baseline log means, one per parameter.
#' @param logsd.baseline Numeric vector of baseline log standard deviations, one per parameter.
#' @param logsd.deltas Named numeric vector of log-delta standard deviations, one per non-baseline spline year.
#'   The names must be the spline years.
#' @param spline.times Character vector of spline years, ordered from earliest to latest.
#' @param baseline.year Character scalar giving the baseline year.
#'
#' @return A Multivariate.Lognormal.Distribution object.
make.mv.spline.prior <- function(parameters,
                                 logmean.baseline,
                                 logsd.baseline,
                                 logsd.deltas,
                                 spline.times,
                                 baseline.year) {
    
    # ----------------------------
    # Validate inputs
    # ----------------------------
    if (!is.character(parameters) || length(parameters) == 0)
        stop("'parameters' must be a non-empty character vector")
    
    n_param <- length(parameters)
    
    if (!is.numeric(logmean.baseline) || length(logmean.baseline) != n_param)
        stop("'logmean.baseline' must have one numeric value per parameter")
    
    if (!is.numeric(logsd.baseline) || length(logsd.baseline) != n_param || any(logsd.baseline <= 0))
        stop("'logsd.baseline' must have one positive numeric value per parameter")
    
    if (!is.numeric(logsd.deltas) || any(logsd.deltas <= 0) || is.null(names(logsd.deltas)))
        stop("'logsd.deltas' must be a named numeric vector of positive values")
    
    if (!is.character(spline.times) || length(spline.times) == 0 || any(is.na(spline.times)))
        stop("'spline.times' must be a non-empty character vector")
    
    if (!is.character(baseline.year) || length(baseline.year) != 1 || is.na(baseline.year))
        stop("'baseline.year' must be a single non-missing character value")
    
    if (!(baseline.year %in% spline.times))
        stop("'baseline.year' must be one of 'spline.times'")
    
    nonbaseline.times <- setdiff(spline.times, baseline.year)
    if (!all(nonbaseline.times %in% names(logsd.deltas)))
        stop("All non-baseline spline times must appear in names(logsd.deltas)")
    
    # order times by numeric year
    spline.times <- spline.times[order(as.numeric(spline.times))]
    past.times <- spline.times[as.numeric(spline.times) < as.numeric(baseline.year)]
    future.times <- spline.times[as.numeric(spline.times) > as.numeric(baseline.year)]
    
    # Keep non-baseline times in the same order as spline.times
    ordered.times <- c(past.times, baseline.year, future.times)
    n_time <- length(ordered.times)
    
    # ----------------------------
    # Build latent mean and covariance
    # ----------------------------
    # Latent vector layout:
    # [baseline values for each parameter,
    #  all past/future deltas for each parameter]
    #
    # Here we use one latent delta per non-baseline year per parameter.
    # this assumes that we only have a need for baseline year (log(transmission rate in 2000)) and the rest are zero (log(delta)=log(t2010/t2000)=log(1)=0)
    untransformed.mu <- c(logmean.baseline, rep(0, (n_time - 1) * n_param))
    # this assumes that all latent variables are independant of each other (off diagonal values=0)
    latent_sds <- c(
        logsd.baseline,
        rep(logsd.deltas[ordered.times[ordered.times != baseline.year]], each = n_param)
    )
    
    untransformed.sigma <- diag(latent_sds^2)

    # ----------------------------
    # Build transformation matrix for one parameter
    # ----------------------------
    # For each parameter:
    # - baseline year depends only on baseline latent value
    # - earlier years accumulate all past deltas between that year and baseline
    # - later years accumulate all future deltas between baseline and that year
    #
    # We index latent variables as:
    # 1 = baseline
    # 2..k = past years (from closest to baseline outward)
    # k+1..end = future years (from closest to baseline outward)
    
    M <- matrix(0, nrow = n_time, ncol = n_time)
    M[, 1] <- 1
    
    # Fill past years
    if (length(past.times) > 0) {
        for (i in seq_along(past.times)) {
            # The i-th past time uses baseline plus all deltas from baseline back to that time
            M[i, 1:(i + 1)] <- 1
        }
    }
    
    # Fill future years
    if (length(future.times) > 0) {
        for (i in seq_along(future.times)) {
            row <- length(past.times) + 1 + i
            col <- length(past.times) + 1 + i
            M[row, c(1, col)] <- 1
            if (i > 1) {
                M[row, c(1, (length(past.times) + 2):col)] <- 1
            }
        }
    }
     # ----------------------------
    # Transform mean and covariance
    # ----------------------------
    mu <- M %*% untransformed.mu
    sigma <- M %*% untransformed.sigma %*% t(M)
    
    var.names <- as.vector(outer(parameters, ordered.times, paste0))
    
    dist <- Multivariate.Lognormal.Distribution(
        mu = mu,
        sigma = sigma,
        var.names = var.names
    )
    
    return(dist)
}
if (1==2){
xx=make.mv.spline.prior(parameters = "transmission.msm",
                        logmean.baseline = 2,
                        logsd.baseline = 0.5*log(2),
                        logsd.deltas =  c("1970"=log(2),"1990"=log(2),"2000"=log(2),"2010"=log(2),"2020"=log(2)),
                        spline.times = c("1970","1990","2000","2010","2020"),
                        baseline.year = "2000" )

}
# **************************************************************************************************************************************************************************
# make.joint.mv.spline.prior----
# **************************************************************************************************************************************************************************
# This function builds a joint multivariate lognormal prior distribution for one or more parameters across multiple spline time points.
# It creates a prior over values like:
# -parameter 1 at 1970, 1990, 1995, 2000, 2010, 2017
# -parameter 2 at 1970, 1990, 1995, 2000, 2010, 2017
# each parameter has a baseline value at one year,
# values in other years are modeled as multiplicative deltas relative to the baseline,
# the baseline values across parameters can be correlated,
# the delta values across parameters can also be correlated within the same year.
# e.g., transmission multipliers for the MSM and heterosexual population. We allow these multipliers to have a joint distribution for each group and we also want them to have a similar direction of change overtime.

#' @title Make Joint Multivariate Spline Prior
#' @description This make can a multivariate joint distribution for any number of parameters and spline points.
#' @param parameters Parameters for which spline point values will be correlated
#' @param logmean.baseline,logsd.baseline A vector of log means and sds for the baseline year, each with one value per parameter.
#' @param logsd.deltas.past,logsd.deltas.future A named vector of log sd's, one per spline point either before or after the baseline, called past or future points. Names are years, matching spline times.
#' @param spline.times A character vector of years indicating, in order, the past spline point years, the baseline year, and the future years. Whichever year isn't referred to in the names of the deltas future and past sd vectors is taken to be the baseline year.
#' @param correlation The correlation between parameter values for the same spline point year
#' It returns a Multivariate.Lognormal.Distribution object with:
# mu: the mean vector on the log scale
# sigma: the covariance matrix on the log scale
# var.names: names for each variable in the final joint distribution
make.joint.mv.spline.prior <- function(parameters,
                                       logmean.baseline,
                                       logsd.baseline,
                                       logsd.deltas.past,
                                       logsd.deltas.future,
                                       spline.times = c("1970", "1990", "1995", "2000", "2010", "2017"),
                                       correlation = 0.5,
                                       debug = F) {
    
    if (debug) browser()
    # Validate arguments ----
    error.prefix = "error making joint multivariate spline prior: "
    if (!is.character(parameters) || length(parameters)==0 || any(is.na(parameters)))
        stop(paste0(error.prefix, "'parameters' must be a character vector with no missing values"))
    if (!is.numeric(logmean.baseline) || length(logmean.baseline) != length(parameters) || any(is.na(logmean.baseline)))
        stop(paste0(error.prefix, "'logmean.baseline' must be a numeric vector with one non-missing value per parameter"))
    if (!is.numeric(logsd.baseline) || length(logsd.baseline) != length(parameters) || any(is.na(logsd.baseline)) || any(logsd.baseline<=0))
        stop(paste0(error.prefix, "'logsd.baseline' must be a numeric vector with one positive, non-missing value per parameter"))
    if (!is.numeric(logsd.deltas.past) || any(is.na(logsd.deltas.past)) || any(logsd.deltas.past<=0))
        stop(paste0(error.prefix, "'logsd.deltas.past' must be a vector containing only positive numeric values. Can have length 0."))
    if (!is.numeric(logsd.deltas.future) || any(is.na(logsd.deltas.future)) || any(logsd.deltas.future<=0))
        stop(paste0(error.prefix, "'logsd.deltas.future' must be a vector containing only positive numeric values. Can have length 0."))
    if (!is.character(spline.times) || length(spline.times)!=(1 + length(logsd.deltas.past) + length(logsd.deltas.future)) || any(is.na(spline.times)))
        stop(paste0(error.prefix, "'spline.times' must be a character vector with one value per spline time"))
    if (!is.numeric(correlation) || length(correlation)!=1 || is.na(correlation) || correlation < -1 || correlation > 1)
        stop(paste0(error.prefix, "'correlation' must be a single numeric value between 1 and -1 inclusive"))
    
    # Check vector names
    if (length(logsd.deltas.past)>0 &
        (!is.character(names(logsd.deltas.past)) ||
         any(is.na(names(logsd.deltas.past))) ||
         !all(names(logsd.deltas.past) %in% spline.times)))
        stop(paste0(error.prefix, "the names of 'logsd.deltas.past' must all be character values found in 'spline.times'"))
    if (length(logsd.deltas.future)>0 &
        (!is.character(names(logsd.deltas.future)) ||
         any(is.na(names(logsd.deltas.future))) ||
         !all(names(logsd.deltas.future) %in% spline.times)))
        stop(paste0(error.prefix, "the names of 'logsd.deltas.future' must all be character values found in 'spline.times'"))
    if (length(intersect(names(logsd.deltas.past), names(logsd.deltas.future)))!=0)
        stop(paste0(error.prefix, "'logsd.deltas.past' and 'logsd.deltas.future' can not share any names"))
    baseline_year <- setdiff(spline.times, union(names(logsd.deltas.past), names(logsd.deltas.future)))
    if (length(baseline_year)!=1)
        stop(paste0(error.prefix, "'spline.times' must contain all the names of 'logsd.deltas.past' and 'logsd.deltas.future' with exactly one additonal baseline year"))
    if (any(names(logsd.deltas.past)>=baseline_year) || any(names(logsd.deltas.future)<=baseline_year))
        stop(paste0(error.prefix, "the baseline year must be later than all past years and earlier than all future years"))
    
    # Create the untransformed mu and sigma ----
    
    num_past_years <- length(logsd.deltas.past)
    num_future_years <- length(logsd.deltas.future)
    num_spline_points <- num_past_years + 1 + num_future_years
    num_parameters <- length(parameters)
    
    # We want to re-order the sd vectors from oldest to newest,
    # since we assume this order when making the mapping matrix.
    logsd.deltas.past <- logsd.deltas.past[sort(names(logsd.deltas.past))]
    logsd.deltas.future <- logsd.deltas.future[sort(names(logsd.deltas.future))]
    
    # The untransformed mu has zero for all the log deltas, since we expect no change between years.
    untransformed_mu <- c(logmean.baseline,
                          rep(0, (num_spline_points - 1) * num_parameters))
    
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
    
    #'@Andrew!!!
    # the baseline block is treated differently from the delta blocks
    # the code assumes baseline parameters are independent across parameters and they are always stacked
    untransformed_sigma[diagonal_block_idx] <- rep(c(
        0,
        logsd.deltas.past,
        logsd.deltas.future
    )^2, each = block_size^2) * correlation
    
    # Divide the diagonals by correlation so only the off-diagonals have it
    diag(untransformed_sigma) <- diag(untransformed_sigma) / correlation
    
    # Reset the baseline block, which is first, to be diagonal with its own variances
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
    # becomes an identity matrix. This is because cells corresponding to one
    # parameter will never map to cells corresponding to a different parameter.
    
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
    
    M <- matrix(0, nrow = num_spline_points, ncol = num_spline_points)
    
    # The entire first column will be 1s because every value uses the baseline trate.
    M[,1] <- 1
    
    # Each past year has 1s in the columns for itself and later years
    # E.g. 1990 might have 1s for 1990 and 1995 (along with the 2000 baseline)
    for (i in seq_len(num_past_years)) {
        M[i,
                        2 + 0 : (num_past_years - i)] <- 1
    }
    
    # Each future year has 1s in the columns for itself and earlier years
    # E.g. 2017 might have 1s for 2017 and 2010 (along with the 2000 baseline)
    for (i in seq_len(num_future_years)) {
        M[i + 1 + num_past_years,
                        1 + num_past_years + 1:i] <- 1
    }
    
    # Now expand it to consider all the parameters, as described above
    M <- kronecker(M, diag(num_parameters))
    
    # Transform mu and sigma and create distribution ----
    mu = M %*% untransformed_mu # E[y] = M E[x]
    
    sigma = M  %*% untransformed_sigma %*% t(M) # Σ_y = M Σ_x Mᵀ 
    
    var_names <- paste0(rep(parameters, num_spline_points),
                        rep(spline.times, each = num_parameters))
    
    dist = Multivariate.Lognormal.Distribution(mu = mu, sigma = sigma, 
                                               var.names = var_names)
    return(dist)
    
}

if(1==2){
xx=make.joint.mv.spline.prior(
    parameters = paste0("transmission.rate.multiplier.", c("msm", "heterosexual")),
    # parameters = c("A", "B"), # shorter names are easier for debugging the output array
    logmean.baseline = c(log(3), 0),
    logsd.baseline = c(log(2), log(2)),
    logsd.deltas.past = c("1970" = log(1.5^2)/2,
                          "1990" = log(sqrt(1.5))/2,
                          "1995" = log(sqrt(1.5))/2),
    logsd.deltas.future = c("2010" = log(1.5)/2,
                            "2017" = log(1.5)/2),
    spline.times = c("1970", "1990", "1995", "2000", "2010", "2017"),
    correlation = 0.5)
}