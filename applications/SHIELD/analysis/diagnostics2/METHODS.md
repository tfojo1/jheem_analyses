# SHIELD calibration diagnostics: methods (diagnostics2)

Version: 2026-11-12. Part A covers the MCMC mixing report. Part B (the deep-dive analyses of the doxy-PEP effect)
will be added once those analyses have been reviewed and merged.

| Code | What it does |
|---|---|
| `0-setup.R` | settings (calibration, cities, share of each chain, output folder) and sources |
| `helpers/mixing_stats.R` | loads the MCMC, computes every statistic in sections 2 to 4 |
| `helpers/identifiability.R` | builds and tests the parameter combinations in section 5 |
| `helpers/report_text.R` | all fixed text in the report ("How to read", statistics explained, CSV column notes) |
| `helpers/report_html.R` | lays out the HTML report and writes the automatic "What we see" lines (section 6) |
| `1-mixing_report.R` | the script you run |

Each function in the helpers has a comment that names the section of this file it implements, for example
`# METHODS 2.2`.

---

# Part A. Mixing report

## 1. Data and draws

1. **Source.** The report reads the raw MCMC saved by the calibration, not the simsets. For each saved draw it uses
   the parameter values, the log-likelihood, the log-prior and the number of accepted proposals by block. It never
   runs the model.
2. **Stage 4 (`calib.10.20.stage4`).** There are 4 chains × 250,000 iterations, and one draw is saved every
   50 iterations. That gives 5,000 saved draws per chain. There are 173 calibrated parameters in 71 sampling blocks.
3. **Kept part of each chain (`MIXING.BURN.KEEP = 0.5`).** All statistics use the second half of each chain:
   2,500 saved draws per chain (iterations 125,001–250,000), 10,000 per city. The burn-in check (4.2) is the only
   exception; it uses the whole chain.
4. **Why the second half.** Each chain starts where the previous stage ended, and its first part is still moving.
   Including it would measure that early movement rather than whether the settled chains agree. The 1,000-sim
   posterior simset (`calib.10.20.stage4`, N.SIM = 1000) also comes from the second half: 250 draws per chain,
   every 10th saved draw. So good mixing here means the 1,000 sims come from a well-mixed stretch.
5. **Cache.** Loading a stage 4 MCMC reads about 84 GB of chunks per city. The first load saves a slimmed copy
   (no simulations) in the calibration's `mcmc_cache` folder (section 10), and later runs read that copy.

---

## 2. Statistics

All R-hat and ESS functions follow Vehtari et al. (2021), the method used by Stan and the R `posterior` package.
That package is not installed on our server, so the functions are written out in `helpers/mixing_stats.R`.
Notation: M chains (or half-chains), N draws in each, θ one parameter.

### 2.1 Classic R-hat (Gelman–Rubin)

**How it is calculated**

1. W = the mean of the M within-chain variances.
2. B = N × the variance of the M chain means.
3. V̂ = (N − 1)/N × W + B/N, an estimate of the variance of all draws together.
4. R̂ = √(V̂ / W).

We take it from `get.rhats()` in bayesian.simulations, on the kept half of each chain. It is the number in
parentheses in the trace-plot panels drawn by `trace.plot()`.

**How to read it**

1. If all chains sample the same distribution, B is small and R̂ is close to 1.
2. R̂ is a ratio of standard deviations. The share of the total variance that comes from differences between
   chains is about 1 − 1/R̂². So R̂ = 1.05 means about 9% of the variance is between chains; 1.1 means about 17%;
   1.5 about 56%; 2 about 75%.

**Advantages:** simple, widely known, and the value printed in the bayesian.simulations trace plots.

**Limitations**

1. Not split: a drift that every chain shares (all chains moving the same way) is missed.
2. Not rank-based: it uses means and variances, so a skewed or heavy-tailed parameter (for example, a multiplier
   on the natural scale) can give a misleading value in either direction.
3. Centre only: chains with the same mean but different spreads look fine.

It is shown for comparison with older reports. It is **not** used in the verdict or in any ranking.

### 2.2 Rank-normalized split R-hat (used in the verdict)

**How it is calculated**

1. **Split.** Each chain's kept half is cut in two. 4 chains become M = 8 half-chains of N = 1,250 draws. A drift
   inside a chain now shows up as two halves that disagree.
2. **Rank-normalize.** All 10,000 draws are ranked together (rank r out of S = 10,000), and each draw is replaced
   by z = Φ⁻¹((r − 3/8)/(S + 1/4)), the normal quantile of its rank. This makes the statistic depend only on the
   order of the draws, so skewness and heavy tails no longer matter, and the result is the same on the natural, log
   or logit scale.
3. **Bulk R-hat** = the classic formula (2.1) applied to the rank-normalized half-chains. It asks: do the centres
   agree?
4. **Tail (folded) R-hat** = the same applied to |θ − median(θ)|, rank-normalized. It asks: do the spreads agree?
5. **R-hat** = the larger of the two.

**How to read it**

1. ≤ 1.05: the chains and their halves agree.
2. 1.05–1.1 (CHECK): they mostly agree; look at the trace plots.
3. \> 1.1 (FAIL): the chains, or the two halves of a chain, are in different places. Summaries of that parameter
   depend on which chain you look at.
4. Vehtari et al. recommend 1.01 for a single parameter of interest. We use 1.05 and 1.1 because we take the worst
   of 173 parameters, and with 8 half-chains R-hat itself is noisy.

**Advantages:** catches shared drift (split), works for any distribution shape (rank), and checks the spread as
well as the centre (tail).

**Limitations**

1. With 4 chains, a posterior with several separate modes can be missed if, by chance, no chain reaches one of
   them. R-hat only compares the chains we have.
2. It is computed per parameter. It does not say whether a function of several parameters (for example, the
   doxy-PEP effect) has mixed. Section 5 addresses this for combinations of parameters.
3. Because it is rank-based, it does not say how far apart the chains are in the parameter's own units.

**This is the R-hat used for every verdict and every ranking in the report** (worst parameters, trace-plot
selection, parameters failing across cities, leave one chain out, burn-in check, identifiability).

### 2.3 R-hat of the log-posterior

**How it is calculated:** the rank-normalized split R-hat (2.2) of log-likelihood + log-prior, one value per saved
draw.

**How to read it**

1. ≤ 1.05: the chains reach the same level of fit to the data.
2. \> 1.1 (FAIL): some chains sit at a better (or worse) fit than others. One chain may be stuck in a poorer region.
3. Parameters fail but the log-posterior passes: the chains fit the data equally well with different parameter
   values. That is the signature of parameters that the data cannot separate (section 5).

**Advantages:** one number for the whole model, and free of the scale of the likelihood. It replaces the old
log-likelihood gap, whose cut-off depended on the number of data points.

**Limitations:** chains can fit equally well and still disagree on outputs we care about. A passing log-posterior
does not mean the outputs have mixed.

### 2.4 Bulk ESS (effective sample size across chains) and MCSE

**How it is calculated** (Vehtari et al. 2021; Geyer 1992)

1. Use the rank-normalized half-chains of 2.2.
2. The autocorrelation at lag t is estimated from all half-chains together, against the variance of all draws V̂:
   ρ̂ₜ = 1 − (W − mean of the half-chains' autocovariances at lag t) / V̂. Because V̂ includes the differences
   between chains, chains in different places give a high autocorrelation and so a low ESS.
3. Lags are added in pairs (ρ̂₂ₖ + ρ̂₂ₖ₊₁) while the pair sum stays positive, and the pair sums are forced not to
   increase (Geyer's initial monotone sequence). τ̂ = −1 + 2 × (sum of the kept pairs).
4. ESS = M × N / τ̂.

**How to read it**

1. ESS is the number of independent draws that the 10,000 correlated draws are worth.
2. The Monte Carlo standard error (MCSE) of a posterior mean is the posterior SD / √ESS. At ESS = 400 this is 5% of
   the posterior SD.
3. < 400, about 100 per chain (CHECK): summaries of that parameter are imprecise.
4. ESS is not the number of draws to keep. The 1,000-sim simset holds 1,000 draws whatever the ESS; a low ESS
   means they are worth fewer independent draws.

**Advantages:** accounts for both autocorrelation and disagreement between chains. Adding up each chain's own ESS
would not: four chains each well mixed in a different place would look fine. (In a test with one chain shifted,
the sum of per-chain ESS was 568 and the bulk ESS 15.)

**Limitations:** bulk ESS is about the centre of the distribution. We do not compute tail ESS, so 2.5% and 97.5%
quantiles may be less precise than the bulk ESS suggests.

### 2.5 Acceptance rate, by chain and by block

**Background: how the sampler works** (bayesian.simulations)

1. Adaptive block Metropolis. Each iteration proposes a new value for **one** block of parameters, and the blocks
   are taken in a fixed cycle (block 1, 2, …, 71, 1, …).
2. The proposal covariance is adapted from the chain's history, toward an acceptance rate of 0.238.
3. Splitting a block into two does not make each parameter update more often: the cycle just becomes longer.

**How it is calculated**

1. By chain: accepted proposals / all proposals, over the kept half.
2. By block and chain: the same for each block in each chain. In stage 4 each block gets about 1,760 proposals per
   chain in the kept half (125,000 iterations ÷ 71 blocks). At a rate of 0.05 that is about 88 accepted moves.

**How to read it**

1. For random-walk Metropolis the most efficient rate is about 0.234 (Roberts, Gelman and Gilks 1997); 0.15–0.35 is
   fine.
2. Chain < 0.10 (FAIL): the steps are too large and the chain rarely moves.
3. Chain > 0.50 (CHECK): the steps are very small; the chain moves often but explores slowly.
4. A block < 0.05 in one chain (CHECK): that block barely moves in that chain.

**Limitations:** a healthy acceptance rate does not mean good mixing. A chain can accept 25% of tiny moves along a
long ridge and still take far longer than the run to cross it. This is what stage 4 shows (section 5).

---

## 3. Verdict rules

A city gets **FAIL** if any FAIL rule is met; otherwise **CHECK** if any CHECK rule is met; otherwise **PASS**.
The cut-offs are in `MIXING.THRESHOLDS` (`helpers/mixing_stats.R`).

| Check | Statistic | CHECK if | FAIL if |
|---|---|---|---|
| Chains agree | worst rank-normalized split R-hat over all parameters (2.2) | > 1.05 | > 1.1 |
| Whole model agrees | rank-normalized split R-hat of the log-posterior (2.3) | > 1.05 | > 1.1 |
| Enough independent draws | worst bulk ESS (2.4) | < 400 | – |
| Chains move | acceptance of each chain (2.5) | > 0.50 | < 0.10 |
| No block stuck | acceptance of each block in each chain (2.5) | < 0.05 | – |

The verdict is about the worst parameter. One failing parameter makes the city FAIL even if it plays no role in
the outputs we report. Section 5 and the deep-dive analyses (Part B) ask whether the failure matters for the
outputs.

---

## 4. Supporting checks (not in the verdict)

### 4.1 Leave one chain out

**How:** recompute the worst R-hat (2.2) with each chain left out in turn.

**How to read it**

1. Leaving out one chain brings the worst R-hat to ≤ 1.1: that chain alone is in a different place.
2. One chain gives much the largest drop (at least twice the next): that chain contributes most, but the others
   also disagree.
3. All drops are similar: no single chain is to blame; the problem is general.

### 4.2 Burn-in check

**How:** worst R-hat and log-posterior R-hat on the whole chain, then after removing the first 25%, 50% and 75%
of each chain. This is the only check that looks at the first half.

**How to read it**

1. R-hat falls from 0% to 50%: the start of the chains was still converging, and the burn is doing its job.
2. R-hat still falls from 50% to 75%: the kept half is itself still settling. Run longer.
3. R-hat stays flat and high: the chains are not moving toward each other. More burn-in will not help.
4. At 75%, only 1,250 draws per chain remain, so R-hat is noisier there.

### 4.3 Parameters failing across cities

**How:** for each parameter, the number of cities where its R-hat > 1.1 or its ESS < 400.

**How to read it:** a parameter that fails in most cities points to the model structure or the priors, not to
run length in one city.

---

## 5. Identifiability: do combinations of parameters mix when the parameters do not?

### 5.1 Why

If the data can only determine a combination of parameters (for example, a product), the chains can wander along
the set of values that give the same combination. Each parameter then fails R-hat while the combination mixes. In
that case the failure is not the sampler failing to find the posterior: the posterior itself is a long ridge, and
outputs that depend only on the combination are still well estimated.

### 5.2 Model structure

The sexual transmission rate for a group is

rate = global.transmission.rate.⟨msm | het⟩ × time multiplier × race multiplier × age multiplier

1. Time: one multiplier per knot (1970, 1990, 1995, 2000, 2010, 2022), separate for MSM and heterosexual.
2. Race: black, hispanic, other; **shared** by MSM and heterosexual transmission (race of the recipient).
3. Age: one multiplier per age knot (14, 19, …, 65), separate for MSM and heterosexual.

Multiplying every multiplier in one group by c and the global rate by 1/c gives exactly the same rates. The data
cannot tell these apart, so this redundancy is built into the model.

### 5.3 Sampling scale

Before combining, each parameter is put on the scale the sampler works on: log for positive rates and multipliers,
logit for proportions (names starting with `fraction`, `prop` or `prp`). A product or ratio is then a sum or
difference on that scale. For proportions, "product" means a sum of log-odds.

R-hat of a single parameter (2.2) is the same on any scale, except for small differences in the tail part.

### 5.4 Level, shape and product (designed from the model structure)

For one sex (MSM or het), on the log scale:

1. **Level** = log global + mean log of the time multipliers + mean log of the race multipliers + mean log of the
   age multipliers. The overall rate level.
2. **Shape** = log of one multiplier − mean log of its group. A multiplier relative to the others in its group
   (for example, 2022 relative to the other knots; black relative to the other races).
3. **Product** = log global + log one multiplier. The rate at one knot, before the other groups.

**Why level and shape.** Every rate in the model is exactly level + time shape + race shape + age shape. Scaling a
group by c and the global rate by 1/c changes none of these: the level gains log c from the group mean and loses
log c from the global rate, and each shape is unchanged because every member and the group mean move together.
So the level and the shapes are what the data can determine.

**Worked example** (het, black, age 24, year 2022):

log rate = log global.het + log mult.het2022 + log mult.black + log mult.age24.het
         = level.het + shape.het.2022 + shape.race.black + shape.het.age24

**Why products can still fail.** A product cancels the trade-off with one group only. The global rate also trades
off with the other two groups, so a product can mix poorly while the level and shapes mix well.

The race shapes are computed once, because the race multipliers are shared. Scaling the race multipliers by c
needs both global rates scaled by 1/c; both levels absorb this.

### 5.5 Other designed combinations

Each is tested as a product and as a ratio (sum and difference on the sampling scale). A product that mixes means
the two trade off (one up, the other down); a ratio that mixes means they move together.

| Type | Combinations | Question |
|---|---|---|
| race.mixing | race multiplier × the same race's assortative mixing (`black.black.sexual.multi`, …); race shape × the same race's mixing | does the race pattern of transmission trade off with race mixing? |
| natural.history | `el.rel.secondary.transmissibility` and `prop.early.latent.to.secondary`, each × each global rate and × each level | does infectiousness trade off with the transmission rate? |
| bridging | the three MSM–female mixing parameters (`oe.female.pairings.with.msm`, `fraction.msm.pairings.with.female`, `fraction.heterosexual_male.pairings.with.male`) with each other and × `global.transmission.rate.het` | does bridging trade off with heterosexual transmission? |
| diagnosis | symptomatic share × care-seeking (MSM); the two symptomatic shares | does detection trade off with symptoms? |

### 5.6 Data-driven pairs and best partner

1. Take every parameter with R-hat > 1.5 (`IDENT.PAIR.RHAT`), of any kind.
2. For every pair, compute the R-hat of their product and of their ratio (sampling scale).
3. **Best partner** of each parameter: the other parameter whose combination with it has the lowest R-hat.
4. Also reported: the within-chain correlation of the two (sampling scale), computed inside each chain and averaged
   on the Fisher-z scale (z = atanh r; mean; back with tanh). Chains are not pooled, because a pooled correlation
   mostly reflects where the chains ended up.
5. This finds trade-offs that the model structure does not make obvious. It replaces the earlier "correlated
   parameter pairs" check (strong |r| alone did not show whether the combination mixed).

### 5.7 Verdict for a combination

| Verdict | Rule | Meaning |
|---|---|---|
| IDENTIFIED | combination R-hat ≤ 1.1 while at least one part has R-hat > 2 | the data identify the combination only |
| PARTLY | not IDENTIFIED, and drop share ≥ 0.5 | part of the trade-off is captured; the ridge involves other parameters too |
| NOT | otherwise | not this trade-off |
| NA | no part has R-hat > 2 | nothing to explain |

When a combination is built from another combination (natural history × level; race mixing × race shape), the
"parts" in this rule are the level (or shape) as a whole and the other parameter, not every parameter inside the
level. Otherwise a combination could be called IDENTIFIED only because the level mixes while one of its own members
fails. (The earlier deep-dive script 05 counted every parameter inside the level as a part. That could only change
the verdict where the natural-history parameter itself mixed; el.rel, whose own R-hat was above 2, is not affected.)

Drop share = (worst part's R-hat − combination R-hat) / (worst part's R-hat − 1). It is 1 when the combination mixes
completely and 0 when it is no better than its worst part.

### 5.8 Limitations of the identifiability check

1. **Pairs only.** Apart from the designed levels (which combine many parameters), only two parameters are combined
   at a time. A ridge involving three or more parameters is missed, or shows as PARTLY.
2. **Fixed coefficients.** Combinations use coefficients of ±1 (or the group means). A trade-off such as
   a × b² is missed.
3. **Chance.** Many pairs are tested. Two non-mixing parameters can give a combination that mixes by chance if their
   chain means happen to offset. Trust a pair more when it repeats across cities and has a strong within-chain
   correlation of the matching sign (negative for a product, positive for a ratio).
4. **Mixing of a combination is necessary, not sufficient,** for an output that depends on it to be well estimated.
   The outputs themselves are checked in Part B.

---

## 6. "What we see" lines

Each table and figure in the report has a fixed "How to read" line and an automatic "What we see" line. The
automatic lines use only these rules, so they can be checked against the tables.

| Section | Rule |
|---|---|
| Overview | counts of PASS/CHECK/FAIL; the city with the highest worst R-hat |
| Parameters failing across cities | parameters failing in ≥ half of the cities |
| Chains | range of chain acceptance; lowest mean log-likelihood chain and its gap from the best |
| Log-likelihood and log-posterior | log-posterior R-hat ≤ 1.05: "same fit"; > 1.1: "different fit"; plus whether parameters fail at the same time (2.3, point 3) |
| Worst parameters | number with R-hat > 1.1; how many of those are transmission parameters (`TRANSMISSION.PATTERN`); the worst one |
| Blocks with low acceptance | number of blocks < 0.05 in at least one chain, and which chains |
| Acceptance by block | the lowest block |
| Leave one chain out | rules 4.1 |
| Burn-in check | drop share from 0% to 50% and from 50% to 75%; ≥ 0.3 is called a fall, otherwise flat |
| Identifiability | median R-hat of levels and shapes vs their parts; number of IDENTIFIED combinations by type |

---

## 7. What to do with each result

| What you see | Likely cause | Next step |
|---|---|---|
| R-hat > 1.1; leaving out one chain fixes it | one chain in a different place | check that chain's trace and log-likelihood |
| R-hat > 1.1; burn-in still falling at 75% | still converging | run longer |
| R-hat > 1.1; burn-in flat; log-posterior R-hat fine; levels and shapes mix | identifiability ridge | check that the outputs mix (Part B); reparameterize in a future calibration |
| Log-posterior R-hat > 1.1 | chains at different fits | log-likelihood by chain; leave one chain out |
| ESS < 400 with R-hat fine | slow mixing | more iterations |
| Chain acceptance < 0.10 or > 0.50 | step size | check the adaptation |
| A block < 0.05 in one chain | block stuck in that chain | trace plots of that block in that chain |
| Same parameters fail in most cities | model or prior structure | identifiability section |

---

## 8. Checks dropped from the earlier report, and why

| Check | Why dropped |
|---|---|
| Stuck fraction | one block is proposed per iteration and there are 71 blocks, so a block gets at most one proposal between two saved draws (50 iterations). No change between saved draws is normal: a block is proposed in about 50/71 of those gaps and accepted 17–24% of the time, so 83–88% of saved draws show no change even in a healthy chain (1 − 50/71 × acceptance). Block acceptance replaces it. |
| Geweke z | one test per parameter per chain (692 tests); about 5% exceed \|z\| > 2 by chance. Split R-hat covers drift within a chain. |
| Log-likelihood gap in the verdict | its cut-off depends on the scale of the likelihood; log-posterior R-hat replaces it. The chain means are still shown. |
| Sum of per-chain ESS, ESS of the worst chain | overstate ESS when chains disagree; bulk ESS replaces them. |
| Lag-1 autocorrelation | ESS already accounts for it. |
| Chain-means spread ratio | R-hat in another form: R̂² ≈ 1 + ratio². |
| Correlated parameter pairs (\|r\| > 0.8) | replaced by the data-driven pairs (5.6), which test whether the combination mixes. |

---

## 9. Assumptions and limitations (all in one place)

1. The second half of each chain is treated as past burn-in (1.3). The burn-in check (4.2) tests this.
2. The thresholds (1.05, 1.1, 400, 0.10, 0.50, 0.05 for the verdict; 2 and 1.5 for identifiability) are
   conventions, not tests with a known error rate.
3. Four chains limit how well R-hat can detect separate modes (2.2, limitation 1).
4. All chains of stage 4 start from where stage 3 ended. If those starting points were close together, the chains
   may agree with each other and still all miss part of the posterior. R-hat cannot detect this.
5. Mixing statistics are per parameter or per designed combination. They do not show directly whether model outputs
   (incidence, the doxy-PEP effect) have mixed. That is checked in Part B.
6. The identifiability check is pairwise with fixed coefficients (5.8).
7. Parameters are transformed to the sampling scale by name (5.3). A proportion whose name does not start with
   `fraction`, `prop` or `prp` is put on the log scale instead. This affects only the combinations, not single-
   parameter R-hat.

---

## 10. Folders and files

All diagnostics of one calibration live in one folder, so a calibration's diagnostics can be deleted together:

```
<jheem root>/shield/diagnostics/<calibration code>/
    mcmc_cache/   slimmed MCMC per city (<location>.rds), reused by every diagnostic
    mixing/       mixing_report_<code>_<date>.html and its CSVs
```

The root is set once in `0-setup.R` (`DIAGNOSTICS.ROOT`). Caches built earlier in
`shield/mcmc_diagnostics/mcmc_cache/` are read if no cache exists in the new folder (`OLD.CACHE.DIR`), and copied
into it.

---

## References

1. Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner P-C. Rank-normalization, folding, and localization: an
   improved R-hat for assessing convergence of MCMC. *Bayesian Analysis* 2021;16(2):667–718.
   doi:10.1214/20-BA1221
2. Gelman A, Rubin DB. Inference from iterative simulation using multiple sequences. *Statistical Science*
   1992;7(4):457–472. doi:10.1214/ss/1177011136
3. Geyer CJ. Practical Markov chain Monte Carlo. *Statistical Science* 1992;7(4):473–483.
   doi:10.1214/ss/1177011137
4. Roberts GO, Gelman A, Gilks WR. Weak convergence and optimal scaling of random walk Metropolis algorithms.
   *Annals of Applied Probability* 1997;7(1):110–120. doi:10.1214/aoap/1034625254

None of these is indexed in PubMed (no PMID); the DOIs were checked against Project Euclid.
