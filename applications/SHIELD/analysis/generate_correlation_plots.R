
pct_inc_2030 <- reshape2::melt(apply(total_calc_results["2030",,,,"cum_incidence_averted_percent"], c("intervention", "location"), mean), value.name = "pct_inc_2030")

prp_mpsdam_2021 <- reshape2::melt(apply(total_raw_results["2021",,"prop.male.ps.diag.among.msm",,], c("intervention", "location"), mean), value.name = "prp_mpsdam_2021")

inc_2021 <- reshape2::melt(apply(total_raw_results["2021",,"incidence",,], c("intervention", "location"), mean), value.name = "inc_2021")

prev_2021 <- reshape2::melt(apply(total_raw_results["2021",,"prevalence",,], c("intervention", "location"), mean), value.name = "prev_2021")

screening_2021 <-reshape2::melt(apply(total_raw_results["2021",,"sti.screening",,], c("intervention", "location"), mean), value.name = "screening_2021")

# prp_msm_2021 <- apply(sex_results["2021","msm",,,, "diagnosis.ps"] /
#     apply(sex_results["2021",c("heterosexual_male", "msm"),,,, "diagnosis.ps"],
#           c("sim", "intervention", "location"), sum),
#     c("intervention", "location"), mean)

df <- cbind(
    pct_inc_2030,
    prp_mpsdam_2021["prp_mpsdam_2021"],
    inc_2021["inc_2021"],
    prev_2021["prev_2021"],
    screening_2021["screening_2021"]
) %>%
    filter(intervention == "doxy.cov.20") %>%
    pivot_longer(names_to = "outcome", cols = -(1:3), values_to = "value")

ggplot(df) +
    geom_point(mapping = aes(x = value, y = pct_inc_2030, color = location), size=4) +
    facet_wrap(vars(outcome), scales = "free_x") +
    theme_bw() +
    labs(y = "Percent Cumulative Incidence Averted by 2030 (%)")


# Priorities:
# the figure of outcomes vs. % cumulative incidence averted


# I need:
# age-stratified results only for:
# finding Doxy-coverage totals

# sex-stratified results for:
# regular outcomes:
# diagnosis EL, LL, PS, Total; inc, prev; population; sti.screening

# I'll need these to calculate:
# % adult males who are MSM
# incidence rate among MSM
# cumulative inc/diagnosis total/diagnosis PS 