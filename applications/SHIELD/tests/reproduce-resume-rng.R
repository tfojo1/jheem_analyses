# Isolate the pinned sampler's extra starting-simulation call on restart.
# Run with bayesian.simulations@4e0d13e installed; no model/data files needed:
# Rscript applications/SHIELD/tests/reproduce-resume-rng.R
# This is a mechanism demonstration, not a general production fix.
suppressPackageStartupMessages(library(bayesian.simulations))
single <- utils::getFromNamespace("run.single.chain", "bayesian.simulations")
initial.state <- utils::getFromNamespace("create.initial.chain.state", "bayesian.simulations")
seeds <- as.integer(c(-1380206413, 1549575653, -206854178, -1830359111))

run <- function(consumes.rng, restart = FALSE, preserve.warm.rng = FALSE) {
    simulation <- function(parameters) {
        if (consumes.rng) invisible(runif(1))
        parameters
    }
    control <- create.adaptive.blockwise.metropolis.control(
        var.names = c("rate.a", "rate.b"), simulation.function = simulation,
        log.prior.distribution = function(parameters) sum(dnorm(parameters, 0, 2, log = TRUE)),
        log.likelihood = function(simulation) -sum((simulation - 1.5)^2),
        initial.covariance.mat = matrix(c(0.01, 0, 0, 0.01), 2, 2,
            dimnames = list(c("rate.a", "rate.b"), c("rate.a", "rate.b"))),
        thin = 1, burn = 0)
    state <- initial.state(control, c(rate.a = 1, rate.b = 1))
    current <- NULL
    samples <- likelihoods <- numeric()
    for (chunk in seq_along(seeds)) {
        chunk.control <- control
        if (restart && chunk %in% c(2L, 3L)) {
            control <- unserialize(serialize(control, NULL))
            state <- unserialize(serialize(state, NULL))
            chunk.control <- control
            current <- NULL
            if (preserve.warm.rng) {
                wrapper <- local({
                    first <- TRUE
                    original <- control@simulation.function
                    function(parameters) {
                        if (!first) return(original(parameters))
                        first <<- FALSE
                        saved <- .Random.seed
                        on.exit(assign(".Random.seed", saved, envir = .GlobalEnv))
                        original(parameters)
                    }
                })
                chunk.control@simulation.function <- wrapper
            }
        }
        result <- single(control = chunk.control, chain.state = state,
            n.iter = 2, update.frequency = NA, update.detail = "none",
            initial.sim = current, total.n.iter = 8, prior.n.iter = 2 * (chunk - 1),
            prior.n.accepted = 0, prior.run.time = 0, return.current.sim = TRUE,
            chain = 1, output.stream = function(...) invisible(NULL), seed = seeds[[chunk]])
        state <- result$mcmc@chain.states[[1L]]
        current <- result$current.sim
        samples <- c(samples, as.vector(result$mcmc@samples))
        likelihoods <- c(likelihoods, as.vector(result$mcmc@log.likelihoods))
    }
    list(samples = samples, log_likelihoods = likelihoods)
}

no.rng <- identical(run(FALSE), run(FALSE, restart = TRUE))
with.rng <- identical(run(TRUE), run(TRUE, restart = TRUE))
isolated <- identical(run(TRUE), run(TRUE, restart = TRUE, preserve.warm.rng = TRUE))
stopifnot(no.rng, !with.rng, isolated)
cat("No RNG draw in simulation: uninterrupted/resumed identical:", no.rng, "\n")
cat("One metadata RNG draw: uninterrupted/resumed identical:", with.rng, "\n")
cat("Preserve RNG around the toy's extra warm-start call: identical:", isolated, "\n")
cat("This isolates a mechanism; RNG preservation here is not a general sampler fix.\n")
