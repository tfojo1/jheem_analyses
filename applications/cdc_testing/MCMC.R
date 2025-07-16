y <- c(8, 9, 9.5, 10.5, 7, 13)
y2 <- y + c(0.1,-0.3,0.5,0.4,-0.3,-0.7)


sigma <- 2
tau <- 3
mu0 <- 8.5


log_posterior <- function(mu, y) {
    log_likelihood <- sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
    log_prior <- dnorm(mu, mean = mu0, sd = tau, log = TRUE)
    return(log_likelihood + log_prior)  
}

metropolis_sampler <- function(y, n_iter = 1000, proposal_sd = 0.75, init = 8.5) {
    samples <- numeric(n_iter)
    mu_current <- init
    log_post_current <- log_posterior(mu_current, y)
    
    for (i in 1:n_iter) {
        mu_proposed <- rnorm(1, mean = mu_current, sd = proposal_sd)
        log_post_proposed <- log_posterior(mu_proposed, y)
        
        # Compute log of acceptance ratio
        log_accept_ratio <- log_post_proposed - log_post_current
        
        # Accept or reject
        if (log(runif(1)) < log_accept_ratio) {
            mu_current <- mu_proposed
            log_post_current <- log_post_proposed
        }
        
        samples[i] <- mu_current
    }
    
    return(samples)
}

# Run the sampler
samples <- metropolis_sampler(y, n_iter = 1000, proposal_sd = 0.75)

sd(samples)

qplot(1:length(samples), samples, geom = "line")

# Plot results
hist(samples, breaks = 50, col = "skyblue", main = "Posterior of μ (Metropolis Sampler)",
     xlab = expression(mu), probability = TRUE)
abline(v = mean(samples), col = "red", lwd = 2, lty = 2)



#Sample resample MCMC

y <- c(8, 9, 9.5, 10.5, 7, 13)

sigma <- 2
tau <- 3
mu0 <- 8.5


propose_mu <- 0
propose_sd <- 100


posterior <- function(mu, y) {
    likelihood <- sapply(mu, function(m) prod(dnorm(y, mean = m, sd = sigma)))
    prior <- dnorm(mu, mean = mu0, sd = tau)
    return(likelihood * prior)
}

sample_resample <- function (y, n_iter = 10000, n_resample = 1000){
    
    samples_r <- numeric(n_resample)
    mu_proposed <- rnorm(n_iter, mean = propose_mu, sd = propose_sd)
    post_proposed <- posterior(mu_proposed, y)
    proposal_density <- dnorm(mu_proposed, mean = propose_mu, sd = propose_sd)
    weights <- post_proposed / proposal_density
    weights <- weights / sum(weights)
    resampled <- sample(mu_proposed, size = n_resample, replace = TRUE, prob = weights)
    return(resampled)
   
}
    
resampled <-sample_resample(y, n_iter = 10000, n_resample = 1000)

hist(resampled, breaks = 50, col = "skyblue", main = paste0("Posterior of μ (Sample Resample) mu = ",propose_mu," sd = ",propose_sd),
     xlab = expression(mu), probability = TRUE)
abline(v = mean(resampled), col = "red", lwd = 2, lty = 2)



