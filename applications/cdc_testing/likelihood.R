
source("test/engine_test.R")
source("applications/cdc_testing/cdc_testing_specification.R")

transmuter = create.jheem.transmuter(simulation.set = sim,to.version = "cdct")

# Example data
y <- c(12, 25, 18)        # CDC-funded diagnoses (2019-2021)
n <- c(20, 40, 30)        # total diagnoses observed (2019-2021)
p_hat <- c(0.6, 0.55, 0.5)  # model-predicted proportion of cdc funded diagnoses (cdc funded diagnoses/total diagnoses)

# Likelihood (product of binomial probabilities)
likelihood <- function(y, n, p_hat) {
    prod(dbinom(y, size = n, prob = p_hat))
}

# Log-likelihood (more stable)
log_likelihood <- function(y, n, p_hat) {
    sum(dbinom(y, size = n, prob = p_hat, log = TRUE))
}

# Use
log_likelihood(y, n, p_hat)
