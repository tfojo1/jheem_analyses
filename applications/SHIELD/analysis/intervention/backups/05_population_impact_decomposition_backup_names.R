# ****************************************************************************************************
# 05 POPULATION-LEVEL IMPACT: DECOMPOSITION BY GROUP ----
# ****************************************************************************************************
# How much of the reduction in total-population incidence comes from MSM, women and heterosexual
# men, in each MSA, and how much of the DIFFERENCE between MSAs each group explains.
# Run it after 01a_generate_raw_results_arrays.R: it reads only the raw arrays (total and sex).
#
# THE IDENTITY (within each simulation s and MSA m, EVAL.YEAR, COVERAGE vs no Doxy-PEP)
#   I0_g, I1_g   incident infections in group g without / with Doxy-PEP, g = MSM, women, het men
#   w_g          = I0_g / sum(I0)                     share of incidence without Doxy-PEP
#   R_g          = 100 (I0_g - I1_g) / I0_g           % reduction in group g
#   C_g          = w_g R_g                            contribution to the total reduction, % points
#   R_total      = sum_g C_g                          exact: the strata add up to the total
#   S_g          = 100 C_g / R_total                  % of infections averted that are in group g
#   Equivalently R_total = R_MSM (w_MSM + w_women RIR_women + w_hetmale RIR_hetmale), but C_g is
#   computed directly, so a simulation where MSM incidence did not fall is not lost.
#
# BETWEEN-MSA VARIANCE SHARE (within each simulation, across the ten MSAs)
#   V_g = Cov(C_g, R_total) / Var(R_total), and sum_g V_g = 1.
#   The share of the variation in population-level impact across MSAs explained by group g.
#
# WHAT IT BUILDS (<cov> = the coverage, e.g. 20pct)
#   1. tables/5-decomposition_by_msa_at_<cov>_doxy.csv        w_g, R_g, C_g, S_g and R_total per MSA,
#                                                             median [95% interval]
#   2. tables/5-variance_share_at_<cov>_doxy.csv              V_g, median [95% interval]
#   3. figures/5-contribution_by_msa_at_<cov>_doxy.png        stacked bars: median C_g per MSA
#   Each table starts with description rows, then the results.
#
# SUMMARISING
#   Every quantity is computed within each simulation; each is then summarised as the median and
#   2.5th-97.5th percentile. Medians of the parts do not add up exactly to the median of the total.
# ****************************************************************************************************


# ---- SETUP ----
source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')

FIG.DIR   <- shield.fig.path(CALIB.NAME,   create = TRUE)
TABLE.DIR <- shield.table.path(CALIB.NAME, create = TRUE)

COVERAGE  <- "doxy.cov.20"                                            # same coverage as 04
COV.LABEL <- paste0(sub("doxy.cov.", "", COVERAGE, fixed = TRUE), "pct")

# Printed wherever a 95% interval across MSAs or a within-simulation statistic is reported
PAIRING.NOTE <- paste0(
  "NOTE on simulation pairing: simulations are paired within each MSA across scenarios ",
  "(sim i with and without Doxy-PEP share the same parameters), but NOT across MSAs ",
  "(sim 1 in Baltimore is unrelated to sim 1 in Atlanta). Quantities that combine MSAs within a ",
  "simulation (Total-row sums, within-simulation rho or variance across MSAs) therefore pair ",
  "independent draws arbitrarily; their 95% intervals assume independence across MSAs.")

# Reuse the arrays if 03 or 04 already loaded them for this calibration
if (!exists("intervention.results") ||
    !identical(attr(intervention.results, "calibration.code"), CALIB.NAME) ||
    !all(c("total_raw", "sex_raw") %in% names(intervention.results)))
  intervention.results <- load.shield.results(CALIB.NAME, which = c("total_raw", "sex_raw"))

total.raw <- intervention.results$total_raw
sex.raw   <- intervention.results$sex_raw


# ---- 1. HELPER ----
# One outcome, one intervention, one year, optionally one sex group: MSA x simulation matrix
sim_values <- function(arr, outcome, intervention, year, sex = NULL) {
  keep <- list(outcome = outcome, intervention = intervention, year = year)
  if (!is.null(sex)) keep$sex <- sex
  x <- apply(subset_array(arr, keep), c("location", "sim"), sum)
  rownames(x) <- names(LOCATIONS)[match(rownames(x), LOCATIONS)]   # location codes -> city names
  x[MSAS, ]
}

# "median [2.5th to 97.5th percentile]", rounded to d decimals
show <- function(x, d) {
  q <- round(quantile(x, c(0.5, 0.025, 0.975), na.rm = TRUE), d)
  paste0(q[1], " [", q[2], " to ", q[3], "]")
}

# Writes a table with description rows on top
write_described_csv <- function(df, file, description) {
  con <- file(file, "w")
  writeLines(paste0('"# ', description, '"'), con)
  write.csv(df, con, row.names = FALSE)
  close(con)
}


# ---- 2. THE PARTS, WITHIN EACH SIMULATION (each is MSA x simulation) ----
GROUPS       <- c(msm = "msm", women = "female", hetmale = "heterosexual_male")
GROUP.LABELS <- c(msm = "MSM", women = "Women", hetmale = "Heterosexual men")

I0 <- lapply(GROUPS, function(g) sim_values(sex.raw, "incidence", "noint",  EVAL.YEAR, g))
I1 <- lapply(GROUPS, function(g) sim_values(sex.raw, "incidence", COVERAGE, EVAL.YEAR, g))
I0.total <- Reduce(`+`, I0)
I1.total <- Reduce(`+`, I1)

# Check: the sex strata add up to the total-population incidence
I0.check <- sim_values(total.raw, "incidence", "noint", EVAL.YEAR)
max.rel.diff <- max(abs(I0.total - I0.check) / I0.check, na.rm = TRUE)
if (max.rel.diff > 1e-6)
  warning("The sex strata do not add up to total incidence (largest relative difference ",
          signif(max.rel.diff, 3), "). The decomposition below is for MSM + women + heterosexual men.")

w       <- lapply(I0, function(x) x / I0.total)                              # share of incidence
R       <- Map(function(a, b) 100 * (a - b) / a, I0, I1)                     # % reduction in group
C       <- Map(function(wg, rg) wg * rg, w, R)                               # contribution, % points
R.total <- 100 * (I0.total - I1.total) / I0.total                            # = Reduce(`+`, C)
S       <- lapply(C, function(cg) 100 * cg / R.total)                        # % of averted infections

stopifnot(max(abs(Reduce(`+`, C) - R.total), na.rm = TRUE) < 1e-8)        # the identity holds


# ---- 3. TABLE: DECOMPOSITION PER MSA ----
decomp.table <- data.frame(MSA = MSAS, check.names = FALSE)
decomp.table[["% reduction in incidence, total population"]] <- apply(R.total, 1, show, d = 1)
for (g in names(GROUPS)) {
  lab <- GROUP.LABELS[[g]]
  decomp.table[[paste0(lab, ": share of incidence without Doxy-PEP (%)")]]   <- apply(100 * w[[g]], 1, show, d = 1)
  decomp.table[[paste0(lab, ": % reduction in incidence")]]                  <- apply(R[[g]],       1, show, d = 1)
  decomp.table[[paste0(lab, ": contribution to total reduction (% points)")]] <- apply(C[[g]],       1, show, d = 1)
  decomp.table[[paste0(lab, ": share of infections averted (%)")]]           <- apply(S[[g]],       1, show, d = 1)
}

write_described_csv(
  decomp.table,
  file.path(TABLE.DIR, paste0("5-decomposition_by_msa_at_", COV.LABEL, "_doxy.csv")),
  c(paste0("Decomposition of the % reduction in total-population incidence, ", EVAL.YEAR, ", ", COVERAGE,
           " vs no Doxy-PEP. Contribution of a group = its share of incidence without Doxy-PEP x its own % reduction;",
           " the three contributions add up to the total reduction within each simulation."),
    paste0("Share of infections averted = contribution / total reduction. Each cell: median [2.5th to 97.5th",
           " percentile] across simulations; medians of the parts do not add up exactly to the median total.")))
decomp.table


# ---- 4. TABLE: SHARE OF THE BETWEEN-MSA VARIANCE ----
# Within each simulation, across the ten MSAs: Cov(C_g, R_total) / Var(R_total). Sums to 1.
var.share <- sapply(names(GROUPS), function(g)
  sapply(seq_len(ncol(R.total)), function(s)
    cov(C[[g]][, s], R.total[, s], use = "complete.obs") / var(R.total[, s], na.rm = TRUE)))

var.table <- data.frame(Group = GROUP.LABELS[names(GROUPS)],
                        "Share of between-MSA variance in total reduction (%)" =
                          apply(100 * var.share, 2, show, d = 1),
                        check.names = FALSE, row.names = NULL)

write_described_csv(
  var.table,
  file.path(TABLE.DIR, paste0("5-variance_share_at_", COV.LABEL, "_doxy.csv")),
  c(paste0("Share of the variation across the 10 MSAs in the % reduction in total-population incidence (",
           EVAL.YEAR, ", ", COVERAGE, ") explained by each group's contribution: Cov(contribution, total) / Var(total)."),
    paste0("Computed within each simulation across the 10 MSAs, then median [2.5th to 97.5th percentile]; the",
           " three shares sum to 100% in each simulation. Simulations are not paired across MSAs.")))
var.table
message(PAIRING.NOTE)


# ---- 5. FIGURE: CONTRIBUTION BY MSA ----
# Stacked bars: median contribution of each group (% points) in each MSA. MSAs ordered by the
# median total reduction, largest at the top. The open circle is the median total reduction; the
# stack and the circle need not match exactly (medians do not add up).
contrib <- bind_rows(lapply(names(GROUPS), function(g)
  data.frame(location = MSAS, group = GROUP.LABELS[[g]],
             value = apply(C[[g]], 1, median, na.rm = TRUE))))
contrib$group <- factor(contrib$group, levels = rev(GROUP.LABELS))   # MSM nearest the axis

total.median <- data.frame(location = MSAS, value = apply(R.total, 1, median, na.rm = TRUE))
msa.order    <- total.median$location[order(total.median$value)]

contrib.fig <- ggplot(contrib, aes(y = location, x = value, fill = group)) +
  geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
  geom_point(data = total.median, aes(y = location, x = value), inherit.aes = FALSE,
             shape = 21, size = 2.4, fill = "white", colour = "black") +
  scale_y_discrete(limits = msa.order) +
  scale_fill_manual(values = c("MSM" = PAL.MSM, "Women" = PAL.WOMEN, "Heterosexual men" = "#F4A582"),
                    breaks = GROUP.LABELS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = paste0("Contribution to the reduction in total-population incidence, ", EVAL.YEAR, " (% points)"),
       y = NULL, fill = NULL,
       subtitle = "Bars: median contribution of each group. Circle: median total reduction.") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())
contrib.fig

ggsave(file.path(FIG.DIR, paste0("5-contribution_by_msa_at_", COV.LABEL, "_doxy.png")), contrib.fig,
       width = 8, height = 5, dpi = 300, bg = "white")
