# ****************************************************************************************************
# DIAGNOSTICS2: FIXED TEXT OF THE MIXING REPORT ----
# ****************************************************************************************************
# All the wording in the report that does not depend on the results. Edit the text here, not in report_html.R.
# The automatic "What we see" lines are in report_html.R (rules: METHODS 6).
#   1. REPORT.INTRO          methods summary at the top of the report
#   2. STATISTICS.EXPLAINED  the collapsible "Statistics explained" table (METHODS 2)
#   3. HOW.TO.READ           one line above each table and figure
#   4. COLUMN.NOTES          column descriptions: hover text in the report, first two rows of each CSV
# ****************************************************************************************************

## 1. METHODS SUMMARY ----
REPORT.INTRO <- c(
    "Every number comes from the raw MCMC (parameter draws, log-likelihood, log-prior, accepted proposals), on the last MIXING.BURN.KEEP share of each chain. No model is run.",
    "<b>All verdicts and all rankings use the rank-normalized split R-hat</b> (worst parameters, trace-plot selection, leave one chain out, burn-in, identifiability). The classic R-hat is shown only for comparison.",
    "<b>Trace plots:</b> the number in parentheses in each panel title is the <b>classic</b> R-hat (drawn by bayesian.simulations::trace.plot). The panels are chosen and ordered by the rank-normalized split R-hat, so the two numbers can differ.",
    "Full methods, assumptions and limitations: METHODS.md in analysis/diagnostics2. Section numbers below (e.g. METHODS 2.2) refer to that file.")


## 2. STATISTICS EXPLAINED (METHODS 2) ----
STATISTICS.EXPLAINED <- data.frame(
    statistic = c("Classic R-hat (Gelman-Rubin)",
                  "Rank-normalized split R-hat (used in the verdict)",
                  "R-hat of the log-posterior",
                  "Bulk ESS (effective sample size, all chains)",
                  "Acceptance rate (by chain, by block)"),
    how.calculated = c(
        "W = mean within-chain variance; B = N x variance of the chain means; R-hat = sqrt(((N-1)/N W + B/N) / W). Taken from get.rhats() on the kept half of each chain.",
        "1. Split each chain in half (4 chains -> 8 half-chains). 2. Replace every draw by the normal quantile of its rank among all draws. 3. Classic formula on these (bulk) and on the distance from the median (tail). 4. Report the larger.",
        "The rank-normalized split R-hat of log-likelihood + log-prior, one value per saved draw.",
        "Autocorrelation estimated from all half-chains together against the variance of all draws (so disagreement between chains counts), summed in pairs of lags while positive (Geyer). ESS = draws / (1 + 2 x sum of autocorrelations).",
        "Accepted proposals / all proposals over the kept half. The sampler proposes one block per iteration, cycling through the blocks."),
    how.to.read = c(
        "Close to 1 = chains agree. Share of the variance between chains is about 1 - 1/R-hat^2: 1.05 = 9%, 1.1 = 17%, 2 = 75%.",
        "<= 1.05 good; 1.05-1.1 CHECK (look at the trace plots); > 1.1 FAIL (the chains, or the halves of a chain, are in different places).",
        "<= 1.05: chains reach the same fit. > 1.1: some chains fit the data better. Parameters fail but this passes: chains fit equally well with different values (identifiability).",
        "Independent draws the 10,000 draws are worth. Monte Carlo error of a mean = posterior SD / sqrt(ESS): 5% of the SD at ESS 400. < 400 CHECK.",
        "0.15-0.35 fine (target 0.238). Chain < 0.10 FAIL (rarely moves); > 0.50 CHECK (tiny steps). A block < 0.05 in a chain CHECK (stuck there)."),
    advantages = c(
        "Simple and widely known; the value printed in the trace plots.",
        "Catches a drift all chains share (split); works for any distribution shape and any scale (rank); checks spread as well as centre (tail).",
        "One number for the whole model; free of the likelihood's scale.",
        "Accounts for autocorrelation and for chains in different places.",
        "Directly shows whether the sampler moves."),
    limitations = c(
        "Misses a drift all chains share; misleading for skewed or heavy-tailed parameters; centre only. Not used in any verdict or ranking.",
        "With 4 chains a separate mode can be missed if no chain reaches it; per parameter only (says nothing direct about outputs); does not give the distance between chains in the parameter's units.",
        "Chains can fit equally well and still disagree on outputs.",
        "About the centre only (no tail ESS): 2.5% and 97.5% quantiles may be less precise.",
        "A healthy rate does not mean good mixing: tiny accepted moves along a long ridge can take far longer than the run."),
    methods = c("2.1", "2.2", "2.3", "2.4", "2.5"),
    stringsAsFactors = FALSE)


## 3. HOW TO READ (one line above each table or figure) ----
HOW.TO.READ <- list(
    settings      = "Settings of the run and of this report.",
    overview      = "One row per city. Verdict rules in METHODS 3: FAIL if any parameter or the log-posterior has R-hat > 1.1, or a chain's acceptance < 0.10; CHECK if R-hat > 1.05, ESS < 400, a chain's acceptance > 0.50 or a block's < 0.05 in a chain. Click a city to go to its section.",
    failing       = "Parameters with R-hat > 1.1 or ESS < 400, and in how many cities. One that fails in most cities points to the model structure or priors, not run length (METHODS 4.3).",
    chains        = "One row per chain. Acceptance 0.15-0.35 is fine. A chain whose mean log-likelihood is far below the others is in a worse region; the log-posterior R-hat below says whether the gap matters.",
    log.density   = "R-hat and ESS of the log-likelihood and the log-posterior (METHODS 2.3). If parameters fail but these pass, the chains fit equally well with different parameter values.",
    loglik.plot   = "Log-likelihood of each saved draw in the kept half, one colour per chain. Overlapping bands = same fit; one band lower = that chain fits worse; a trend = still moving.",
    worst         = "The parameters with the highest rank-normalized split R-hat (the verdict R-hat). rhat.classic is shown for comparison only. transmission = TRUE for transmission parameters.",
    trace         = "Kept draws of the worst parameters, one colour per chain (panels ordered by the rank-normalized split R-hat; the number in parentheses is the classic R-hat). Mixed = all chains overlap like one fuzzy band. Not mixed = chains in separate bands, or slow waves.",
    blocks.low    = "Sampling blocks whose acceptance is below 0.05 in at least one chain: their parameters barely move in that chain (METHODS 2.5).",
    accept.block  = "Acceptance of each block, all chains together, lowest first. The 10 lowest are shown; open the list for the rest.",
    accept.time   = "Acceptance over the kept half, by chain. Flat lines near 0.24 = the adaptation has settled. A falling line = steps too large for where the chain now sits.",
    leave.one     = "Worst R-hat recomputed with each chain left out (METHODS 4.1). One chain whose removal brings R-hat to <= 1.1 is alone in a different place; similar drops for all chains = the problem is general.",
    burn.in       = "Worst R-hat after removing the first 0, 25, 50 and 75% of each chain (whole chain; METHODS 4.2). Falling to 50% = the burn works; still falling at 75% = run longer; flat and high = more burn-in will not help.",
    ident.types   = "Designed combinations of the transmission parameters (METHODS 5.4): level = overall rate; shape = a multiplier relative to its group; product = global x one multiplier. Compare the median R-hat of the combinations with that of their worst part.",
    ident.v2      = "For each other designed type (METHODS 5.5), the combination that mixes best. IDENTIFIED = it mixes (R-hat <= 1.1) while a part has R-hat > 2: the data identify only the combination.",
    ident.partner = "Best partner of each parameter with R-hat > 1.5 (METHODS 5.6): the parameter it combines with best. Product mixing with a negative within-chain correlation, or ratio mixing with a positive one, = a two-parameter ridge. The 5 pairs that mix best are shown.",
    ident.trace   = "Traces of the overall rate levels and two shapes (log scale), one colour per chain. If these overlap while the raw parameters do not, the chains only trade off along the redundant scale.",
    cross.verdict = "Designed combinations, all cities: number with each verdict, by type (METHODS 5.7).",
    cross.partner = "Best-partner pairs found in 3 or more cities. A pair that repeats across cities is less likely to be chance (METHODS 5.8, limitation 3).")


## 4. COLUMN NOTES (hover text in the report; first two rows of each CSV) ----
# c(what it is, how to read it)
COLUMN.NOTES <- list(
    city             = c("MSA name", "-"),
    location         = c("MSA code", "-"),
    verdict          = c("PASS / CHECK / FAIL (METHODS 3); for combinations IDENTIFIED / PARTLY / NOT / NA (METHODS 5.7)", "-"),
    reasons          = c("Rules that set the verdict", "-"),
    parameter        = c("Calibrated parameter", "-"),
    rhat             = c("Rank-normalized split R-hat (METHODS 2.2)", "<= 1.05 good; > 1.1 FAIL"),
    rhat.classic     = c("Classic, unsplit R-hat from get.rhats() (METHODS 2.1)", "For comparison only; the number in the trace plots"),
    ess.bulk         = c("Bulk ESS across all chains (METHODS 2.4)", "< 400 CHECK"),
    transmission     = c("TRUE for transmission parameters (TRANSMISSION.PATTERN)", "-"),
    chain            = c("Chain number", "-"),
    acceptance       = c("Accepted / proposed, kept half", "0.15-0.35 fine"),
    min.block.accept = c("Lowest acceptance of any block in this chain (or city)", "< 0.05 CHECK"),
    mean.loglik      = c("Mean log-likelihood of the chain, kept half", "Compare between chains"),
    gap.from.best    = c("Best chain's mean log-likelihood minus this chain's", "0 = best chain"),
    quantity         = c("log-likelihood or log-posterior", "-"),
    block            = c("Sampling block", "-"),
    n.chains         = c("Number of chains", "-"),
    draws.per.chain  = c("Saved draws per chain used", "-"),
    accept.min.chain = c("Lowest chain acceptance", "< 0.10 FAIL"),
    accept.max.chain = c("Highest chain acceptance", "> 0.50 CHECK"),
    n.blocks.low     = c("Blocks with acceptance < 0.05 in at least one chain", "-"),
    max.rhat         = c("Highest rank-normalized split R-hat over all parameters", "> 1.1 FAIL"),
    n.rhat.over.1.1  = c("Parameters with R-hat > 1.1", "-"),
    n.rhat.over.2    = c("Parameters with R-hat > 2", "-"),
    rhat.logpost     = c("R-hat of the log-posterior (METHODS 2.3)", "> 1.1 FAIL"),
    min.ess          = c("Lowest bulk ESS over all parameters", "< 400 CHECK"),
    n.ess.low        = c("Parameters with ESS < 400", "-"),
    burn.keep        = c("Share of each chain used (last part)", "-"),
    n.cities         = c("Number of cities", "-"),
    cities           = c("Cities", "-"),
    chain.left.out   = c("Chain left out", "-"),
    worst.parameter  = c("Worst parameter without that chain", "-"),
    drop.in.max.rhat = c("Worst R-hat with all chains minus without this one", "Large for one chain only = that chain is the problem"),
    share.removed    = c("Share of each chain removed from the start", "-"),
    combination      = c("Combination (METHODS 5.4-5.5)", "level.het = overall het rate; shape.het.2022 = 2022 knot relative to the other het knots; product.het.2022 = global x 2022 multiplier"),
    type             = c("product, level, shape (transmission); race.mixing, natural.history, bridging, diagnosis", "-"),
    parts            = c("Parameters combined, sampling scale (+ = multiplied, - = divided; coefficient if not 1)", "-"),
    rhat.combination = c("R-hat of the combination", "<= 1.1: the combination mixes"),
    ess.combination  = c("Bulk ESS of the combination", "-"),
    max.rhat.parts   = c("Highest R-hat among the parts", "-"),
    min.ess.parts    = c("Lowest ESS among the parts", "-"),
    rhat.drop.share  = c("(worst part R-hat - combination R-hat) / (worst part R-hat - 1)", "1 = mixes completely; 0 = no better than the worst part"),
    n                = c("Number of combinations", "-"),
    median.rhat.combination = c("Median R-hat of the combinations of this type", "Compare with the next column"),
    median.max.rhat.parts   = c("Median of the worst part's R-hat", "-"),
    n.identified     = c("Combinations with verdict IDENTIFIED", "-"),
    own.rhat         = c("R-hat of the parameter", "-"),
    best.partner     = c("Parameter (R-hat > 1.5) whose combination with this one has the lowest R-hat", "-"),
    partner.rhat     = c("R-hat of the best partner", "-"),
    form             = c("product = sum on the sampling scale; ratio = difference", "product mixes = they trade off; ratio mixes = they move together"),
    within.chain.cor = c("Correlation inside each chain (sampling scale), averaged on the Fisher-z scale", "Negative + product, or positive + ratio = a two-parameter ridge"),
    parameter.1      = c("First parameter of the pair", "-"),
    parameter.2      = c("Second parameter of the pair", "-"),
    pair             = c("Parameter pair", "-"))
