# SHIELD calibration diagnostics: methods

As of 2026-11-09. Code: `mcmc/mcmc_helpers.R`, `mcmc/mixing_correlated_parameter_pairs.R`,
`likelihood/likelihood_helpers.R`.

We judge a calibration in two steps:

1. **Do the chains mix?** We use the raw MCMC, and each city gets PASS, CHECK or FAIL.
2. **What does each likelihood contribute?** We run this only once the chains mix. It uses the saved simsets.

The numbers below are for stage 4 (`calib.10.20.stage4`): 4 chains of 250,000 iterations, with one draw saved
every 50 iterations. That gives 5,000 saved draws per chain. There are 173 parameters in 71 sampling blocks.

---

## 1. Which draws each check uses

| Check | Reads | Draws used (stage 4) |
|---|---|---|
| Mixing statistics (sections 3–4) | raw MCMC | last 2,500 saved draws of each chain (iterations 125,001–250,000), 10,000 in all |
| Burn-in check | raw MCMC | whole chain, then without the first 25%, 50% and 75% |
| Correlated parameter pairs | raw MCMC | last 2,500 saved draws of each chain |
| Likelihood: change during the run | 2,000-sim simset, no burn | first and last sim of each chain |
| Likelihood: spread and conflicts | 2,000-sim simset, no burn | 50 sims spread over the second half of the chains |
| Fit-to-data scripts | 1,000-sim simset | 250 per chain, from the second half |

**Why only the second half (`MIXING.BURN.KEEP = 0.5`)**

1. Each chain starts where the previous stage ended. Its first part is still moving toward where the chain settles.
2. Including that start measures the early movement, not whether the settled chains agree. R-hat goes up, and
   Geweke flags almost every parameter.
3. The fit scripts use the second half, so we judge mixing on the same stretch of the chain.

---

## 2. Verdict rules

A city gets **FAIL** if any FAIL rule is met. Otherwise it gets **CHECK** if any CHECK rule is met, and **PASS**
if none are. The cut-offs are in `MIXING.THRESHOLDS`.

| Check | Statistic | CHECK if | FAIL if |
|---|---|---|---|
| Chains agree | rank-normalized split R-hat, worst parameter | > 1.05 | > 1.1 |
| Whole model agrees | rank-normalized split R-hat of the log-posterior | > 1.05 | > 1.1 |
| Enough independent draws | bulk ESS across chains, worst parameter | < 400 | – |
| Chain moves | acceptance rate of each chain | > 0.50 | < 0.10 |
| No block stuck | acceptance rate of each block in each chain | < 0.05 | – |

These are shown but do not affect the verdict: Geweke z, the log-likelihood gap between chains, lag-1
autocorrelation, the classic (unsplit) R-hat, and the chain-means spread ratio. Section 6 explains why.

---

## 3. Checks in the verdict

### 3.1 Acceptance rate (by chain)

**Basis**

1. The sampler is an adaptive block Metropolis sampler. Each iteration proposes a new value for one block of
   parameters, and accepts it with probability min(1, posterior ratio).
2. The acceptance rate is accepted proposals divided by all proposals, over the kept half of the chain.
3. For random-walk Metropolis in many dimensions, the most efficient rate is about 0.234 (Roberts, Gelman and
   Gilks 1997). The sampler adapts its step sizes toward a target of 0.238.

**How to read it**

1. Between 0.15 and 0.35 is fine.
2. **Below 0.10 (FAIL):** steps are too large for where the chain sits, so it rarely moves.
3. **Above 0.50 (CHECK):** steps are very small. The chain moves often but explores slowly. ESS will usually be
   low too.

### 3.2 Acceptance rate by block and chain

**Basis**

1. This is the same rate, counted for each sampling block in each chain separately.
2. In stage 4 each block gets about 1,760 proposals per chain in the kept half (125,000 iterations ÷ 71 blocks).
   At a rate of 0.05, that is about 90 accepted moves.

**How to read it**

1. **Below 0.05 in one chain (CHECK):** that block's parameters barely move in that chain. That chain is likely
   stuck for those parameters, even if its overall acceptance looks normal.
2. Low in every chain: the block's proposal is poorly tuned, or its parameters are tightly constrained. The
   block may need to be split or rearranged (see 4.6).
3. The report lists these blocks under "Blocks with acceptance < 0.05 in a chain".

### 3.3 Rank-normalized split R-hat

**Basis** (Vehtari et al. 2021)

1. **Split.** Each chain is cut in half, so 4 chains become M = 8 half-chains of N = 1,250 draws each. A drift
   inside a chain makes its two halves disagree. This catches a trend that all chains share, which the unsplit
   R-hat misses.
2. **Rank-normalize.** Each draw is replaced by the normal quantile of its rank r among all S draws (all chains
   together). This makes R-hat reliable for skewed or heavy-tailed parameters:

$$ z = \Phi^{-1}\!\left(\frac{r - 3/8}{S + 1/4}\right) $$

3. **R-hat** compares the spread of all draws with the spread within half-chains. Here $W$ is the mean
   within-half-chain variance, and $B$ is $N$ times the variance of the half-chain means:

$$ \hat{V} = \frac{N-1}{N}\,W + \frac{B}{N}, \qquad \hat{R} = \sqrt{\hat{V} / W} $$

4. **Bulk and tail.** R-hat is computed on the rank-normalized draws (bulk: do the centres agree?) and on the
   distance from the median (tail: do the spreads agree?). We report the larger of the two.

**How to read it**

1. When the chains sample the same distribution, R-hat is close to 1. It is above 1 by roughly the share of
   the total variance that comes from differences between chains.
2. **Below 1.05:** good.
3. **1.05 to 1.1 (CHECK):** the chains mostly agree. Look at the worst parameters' trace plots.
4. **Above 1.1 (FAIL):** the chains, or the halves of a chain, are in different places. Don't use summaries of
   those parameters yet.
5. Vehtari et al. recommend 1.01. We use 1.05 and 1.1 because we check 173 parameters and take the worst one.
6. Rank-normalized R-hat is usually lower than the classic one for heavy-tailed parameters. On stage 3
   Baltimore, the worst was 3.25 with this method and 5.16 classic. Both mean "not mixed".

### 3.4 R-hat of the log-posterior

**Basis**

1. This is the same split rank-normalized R-hat, applied to the log-posterior (log-likelihood + log-prior) of
   each saved draw.
2. It summarizes the whole model in one number, and it doesn't depend on the scale of the likelihood.

**How to read it**

1. **Above 1.05 or 1.1:** the chains sit at different levels of fit. This is the same question the
   log-likelihood gap asks, but without an arbitrary cut-off.
2. **Parameters fail but the log-posterior passes:** the chains reach similar fit with different parameter
   values. Those parameters are probably not separately identified (see 4.6).

### 3.5 Bulk ESS (effective sample size across chains)

**Basis** (Vehtari et al. 2021; Geyer 1992)

1. ESS is how many independent draws the correlated draws are worth.
2. The autocorrelation at lag $t$ is estimated with all half-chains together. It uses the full variance
   $\hat{V}$, which counts disagreement between chains. Here $C_{m,t}$ is the autocovariance of half-chain $m$
   at lag $t$:

$$ \hat{\rho}_t = 1 - \frac{W - \tfrac{1}{M}\sum_m C_{m,t}}{\hat{V}}, \qquad
   \hat{\tau} = -1 + 2\sum_{k} \left(\hat{\rho}_{2k} + \hat{\rho}_{2k+1}\right), \qquad
   \text{ESS} = \frac{M N}{\hat{\tau}} $$

3. Pairs of lags are added while their sum stays positive, and the pair sums are kept from going up
   (Geyer's initial monotone sequence).
4. "Bulk" means the ESS is computed on the rank-normalized split chains.
5. We no longer add up each chain's ESS. If the chains sit in different places, each one can look well mixed
   on its own, and the sum overstates the ESS. In a test with one chain shifted, the sum said 568 and the bulk
   ESS said 15.

**How to read it**

1. The Monte Carlo error of a posterior mean is the posterior SD divided by √ESS. At ESS = 400, that error is
   5% of the posterior SD.
2. **Below 400 (CHECK)**, which is about 100 per chain: summaries of that parameter are imprecise.
3. ESS is not the number of draws to keep. The 1,000-sim simset holds 250 draws per chain whatever the ESS.
   A low ESS means those 1,000 sims are worth fewer independent draws.
4. Bulk ESS measures how well the centre of the distribution is estimated. We don't compute tail ESS, so 2.5%
   and 97.5% quantiles may be less precise than the bulk ESS suggests.

---

## 4. Supporting checks (not in the verdict)

These tables help explain why a city fails, and what to do about it.

### 4.1 Leave one chain out

**Basis:** recompute the split rank-normalized R-hat with each chain left out in turn.

**How to read it**

1. Dropping one chain brings R-hat down much more than dropping the others: that chain is in a different place.
2. All four drops give about the same R-hat: no single chain is to blame. The problem is general mixing.

### 4.2 Burn-in check

**Basis:** R-hat on the whole chain, then after throwing away the first 25%, 50% and 75% of each chain.

**How to read it**

1. **R-hat falls from 0% to 50%:** the early part was still converging, and the burn is doing its job.
2. **R-hat still falls from 50% to 75%:** the kept half is itself still settling. Run longer, or keep less of
   each chain.
3. **R-hat stays flat and high:** the chains are not converging toward each other. More burn-in won't help.
4. At 75%, fewer draws remain, so R-hat is noisier there. In stage 4 that is still 1,250 draws per chain.

### 4.3 Geweke z

**Basis** (Geweke 1992): within each chain's kept half, compare the mean of the first 10% with the mean of the
last 50%. Each mean's variance is corrected for autocorrelation through its ESS:

$$ z = \frac{\bar{\theta}_A - \bar{\theta}_B}{\sqrt{s_A^2/\text{ESS}_A + s_B^2/\text{ESS}_B}} $$

**How to read it**

1. For a converged chain, z is roughly standard normal.
2. We run one test per parameter per chain: 692 tests. Even perfect chains give about 31 with |z| > 2, so
   "any |z| > 2" can't be a rule.
3. Use it as a pointer. A parameter with large |z| in several chains is drifting, and split R-hat will usually
   flag it too.

### 4.4 Log-likelihood by chain

**Basis:** the mean log-likelihood of each chain over the kept half, and its gap from the best chain.

**How to read it**

1. A chain far below the others is stuck in a worse region.
2. How big a gap matters depends on the number of data points. For example, 3.9 units is small next to a
   log-likelihood of −22,900. R-hat of the log-posterior (3.4) is the scale-free version.

### 4.5 Lag-1 autocorrelation, classic R-hat, chain-means spread ratio

1. **Lag-1 autocorrelation:** the correlation between consecutive saved draws. Close to 1 means slow mixing.
   ESS already accounts for it.
2. **Classic R-hat:** `get.rhats()` from bayesian.simulations: not split, not rank-normalized. It's kept only
   to compare with older reports.
3. **Chain-means spread ratio:** the SD of the chain means divided by the within-chain SD. It is R-hat in
   another form: R̂² ≈ 1 + ratio². So 0.1 corresponds to R-hat ≈ 1.005, and 0.3 to about 1.04.

### 4.6 Correlated parameter pairs

**Basis**

1. For every pair of parameters (about 15,000 pairs), we compute the Pearson correlation inside each chain,
   over the kept half.
2. Parameters are put on their sampling scale first: logit for proportions, log for positive rates and
   multipliers.
3. The correlations are averaged across chains on the Fisher-z scale: z = atanh(r), take the mean, then
   r = tanh(mean z).
4. We don't pool the chains. If chains sit in different places, a pooled correlation mostly reflects where the
   chains ended up.
5. A pair is "strong" if |r| > 0.8. Results are combined across cities, and each pair is matched to its
   sampling blocks.

**How to read it**

1. **A strong negative r** (for example, global transmission rate against a time multiplier) means the data
   identify a combination of the two, not each one separately.
2. **Strong in many cities:** a structural trade-off in the model, or in the priors.
3. **Same block = TRUE:** the adaptive sampler learns the correlation within the block, but needs enough
   iterations to do so.
4. **Same block = FALSE:** the sampler cannot learn it. Consider putting the two parameters in one block.
   Pairs like this often explain low ESS or high R-hat.

---

## 5. Likelihood diagnostics

The likelihood is computed from model outputs, so these checks use the saved simset instead of the raw MCMC.
We use the 2,000-sim simset with no burn: one sim every 500 iterations, 500 per chain. For each sim,
`compute.piecewise()` gives the log-likelihood of each component.

**What it measures**

1. **Change during the run:** each component's log-likelihood at the last sim of a chain minus at its first
   sim, averaged over the chains. It shows which targets the chains improved on during this stage.
2. **Posterior SD:** the SD of each component's log-likelihood across 50 sims spread over the kept half.
3. **Conflicts:** pairs of components whose log-likelihoods have a correlation below −0.5 across those 50 sims.

**How to read it**

1. Raw log-likelihood values can't be compared between components. Their constants and numbers of data points
   differ. Compare changes and spreads instead.
2. **Large change during the run:** the chains were still improving the fit to that target. If it's large for
   many targets, check the burn-in check (4.2) too.
3. **Large posterior SD:** this component decides which sims are kept. It drives the posterior.
4. **Posterior SD near zero:** the component barely constrains the kept sims. Either it's already fit equally
   well by all of them, or it carries little weight.
5. **Conflict (r < −0.5):** improving the fit to one target worsens the other. This points to a model
   structure or data inconsistency that the weights are trading off.
6. With 50 sims, a correlation has a standard error of about 0.15, and there are many pairs. Expect a few
   conflicts by chance. Trust the ones that repeat across cities.
7. Read these results only for cities whose chains mix. For a FAIL city, the 50 sims mix chains that don't
   agree.

---

## 6. What to do with each result

| What you see | Likely cause | Next step |
|---|---|---|
| R-hat > 1.1; leaving out one chain fixes it | one chain is in a different place | look at that chain's trace and log-likelihood; restart it, or run longer |
| R-hat > 1.1; burn-in check still falling at 75% | still converging | run longer, or keep less of each chain |
| R-hat > 1.1; burn-in check flat; log-posterior R-hat fine | parameters not separately identified | correlated pairs; rearrange blocks or revisit priors |
| R-hat fine; ESS < 400 | slow mixing | more iterations; check acceptance and block structure |
| Log-posterior R-hat > 1.1 | chains at different levels of fit | log-likelihood by chain; leave one chain out |
| Chain acceptance < 0.10 | steps too large, chain barely moves | check the start values and the adaptation |
| Chain acceptance > 0.50 | steps too small | expect low ESS; check the adaptation |
| A block < 0.05 in one chain | block stuck in that chain | trace plots of that block's parameters in that chain |
| Same parameters fail in many cities | prior or model issue, not run length | "Parameters that mix badly" table; correlated pairs |

---

## 7. Checks we dropped, and why

1. **Stuck fraction** (share of saved draws with no change). The sampler proposes one block per iteration, and
   there are 71 blocks. Between two saved draws (50 iterations), a block gets at most one proposal, and accepts
   it about 1 time in 6. So about 89% of saved draws show no change even in a healthy chain. Stage 3 showed
   73–97%, and every parameter counted as "stuck" in every chain. Block acceptance (3.2) replaces it.
2. **Geweke in the verdict.** With 692 tests, chance alone gives CHECK every time (4.3). Split R-hat covers
   drift within a chain.
3. **Log-likelihood gap in the verdict.** The cut-off depends on the scale of the likelihood. R-hat of the
   log-posterior replaces it.
4. **Sum of per-chain ESS.** It overstates ESS when the chains disagree. Bulk ESS across chains replaces it.
5. **Unsplit R-hat in the verdict.** It misses a drift that all chains share. It's kept only as `rhat.classic`.

---

## References

1. Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner P-C (2021). Rank-normalization, folding, and
   localization: an improved R-hat for assessing convergence of MCMC. *Bayesian Analysis* 16(2): 667–718.
2. Gelman A, Rubin DB (1992). Inference from iterative simulation using multiple sequences. *Statistical
   Science* 7(4): 457–472.
3. Geyer CJ (1992). Practical Markov chain Monte Carlo. *Statistical Science* 7(4): 473–483.
4. Geweke J (1992). Evaluating the accuracy of sampling-based approaches to the calculation of posterior
   moments. In *Bayesian Statistics 4*, 169–193. Oxford University Press.
5. Roberts GO, Gelman A, Gilks WR (1997). Weak convergence and optimal scaling of random walk Metropolis
   algorithms. *Annals of Applied Probability* 7(1): 110–120.
6. Gelman A, Carlin JB, Stern HS, Dunson DB, Vehtari A, Rubin DB (2013). *Bayesian Data Analysis*, 3rd ed.,
   chapter 11. CRC Press.
