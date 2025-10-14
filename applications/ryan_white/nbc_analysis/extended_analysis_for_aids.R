# JAIDS analysis
# Same as Annals analysis + conservative analysis & age/sex/race breakdown

library(reshape2)
library(tidyverse)

RESULTS.FILEPATH <- "../../results/ryan_white/nbc/"

if (!exists("total.incidence"))
    x = load("Q:results/ryan_white/ryan_white_results_state_nanb_2026_2025-10-14.Rdata")
total.incidence <- total.incidence[as.character(2026:2030),,,]
incidence.by.race <- incidence.by.race[as.character(2026:2030),,,,]
incidence.by.age<- incidence.by.age[as.character(2026:2030),,,,]
incidence.by.sex.risk<- incidence.by.sex.risk[as.character(2026:2030),,,,,]

SURVEY.INTERVENTION.NAMES <- c("nanb.rw.end.26", "nanb.rw.intr.26")
CONSERVATIVE.INTERVENTION.NAMES <- c("nanb.rw.end.cons.26", "nanb.rw.intr.cons.26")
INTERVENTION.NAMES <- c(SURVEY.INTERVENTION.NAMES, CONSERVATIVE.INTERVENTION.NAMES)

# Define helpers
get_mean <- function(data, total=F, extra.keep.dimensions=NULL) {
    location <- if (total) NULL else "location"
    apply(data,
          c(extra.keep.dimensions, location, "intervention"),
          mean)
}
get_quantile <- function(data, total=F, extra.keep.dimensions=NULL) {
    location <- if (total) NULL else "location"
    apply(data,
          c(extra.keep.dimensions, location, "intervention"),
          quantile,
          probs = c(0.025, 0.975))
}

# Initial data parsing----
data_int_years <- apply(total.incidence,
                        c("sim", "location", "intervention"),
                        sum)
data_noint <- data_int_years[,,"noint"]
data_ints <- data_int_years[,,INTERVENTION.NAMES]

excess_temp <- data_ints - as.vector(data_noint)
pct_excess_temp <- excess_temp / as.vector(data_noint)
total_excess_temp <- apply(excess_temp,
                           c("sim", "intervention"),
                           sum)
total_pct_excess_temp <- total_excess_temp /
    as.vector(apply(data_noint, "sim", sum))

# Absolutes----
# No-int
baseline_new_infections_mean <- mean(apply(data_noint,
                                           c("sim"),
                                           sum))
baseline_new_infections_quantile <- quantile(apply(data_noint,
                                                   c("sim"),
                                                   sum),
                                             probs = c(0.025, 0.975))

# By state
new_infections_mean <- get_mean(data_int_years)
new_infections_quantile <- get_quantile(data_int_years)

# Total
total_new_infections_mean <- get_mean(apply(data_int_years,
                                            c("sim", "intervention"),
                                            sum),
                                      total = T)
total_new_infections_quantile <- get_quantile(apply(data_int_years,
                                                    c("sim", "intervention"),
                                                    sum),
                                              total = T)

# Excess----
# By state
excess_mean <- get_mean(excess_temp)
excess_quantile <- get_quantile(excess_temp)
excess_df <- merge(
    melt(round(excess_mean), value.name = "mean"),
    melt(round(excess_quantile)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("location", "intervention")
)
write.csv(excess_df,
          file = paste0(RESULTS.FILEPATH, "main_results/excess.csv"),
          row.names = F)

# Total
total_excess_mean <- get_mean(total_excess_temp, total=T)
total_excess_quantile <- get_quantile(total_excess_temp, total=T)
total_excess_df <- merge(
    melt(round(total_excess_mean), value.name = "mean") %>%
        rownames_to_column(var = "intervention"),
    melt(round(total_excess_quantile)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("intervention")
)
write.csv(total_excess_df,
          file = paste0(RESULTS.FILEPATH, "main_results/total_excess.csv"),
          row.names = F)

# Percent Excess----
# By state
pct_excess_mean <- get_mean(pct_excess_temp)
pct_excess_quantile <- get_quantile(pct_excess_temp)
pct_excess_df <- merge(
    melt(round(pct_excess_mean*100,1), value.name = "mean"),
    melt(round(pct_excess_quantile*100,1)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("location", "intervention")
)
write.csv(pct_excess_df,
          file = paste0(RESULTS.FILEPATH, "main_results/pct_excess.csv"),
          row.names = F)

# Total
total_pct_excess_mean <- get_mean(total_pct_excess_temp,
                                  total=T)
total_pct_excess_quantile <- get_quantile(total_pct_excess_temp,
                                          total=T)
total_pct_excess_df <- merge(
    melt(round(total_pct_excess_mean*100,1), value.name = "mean") %>%
        rownames_to_column(var = "intervention"),
    melt(round(total_pct_excess_quantile*100,1)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("intervention")
)
write.csv(total_pct_excess_df,
          file = paste0(RESULTS.FILEPATH, "main_results/total_pct_excess.csv"),
          row.names = F)

# Age Results----
age_data_cumulative <- apply(incidence.by.age,
                            c("age", "sim", "location", "intervention"),
                            sum)
age_excess_temp <- age_data_cumulative[,,,SURVEY.INTERVENTION.NAMES] -
    as.vector(age_data_cumulative[,,,"noint"])
age_pct_excess_temp <- age_excess_temp / as.vector(age_data_cumulative[,,,"noint"])
age_total_excess_temp <- apply(age_excess_temp,
                               c("age", "sim", "intervention"),
                               sum)
age_total_pct_excess_temp <- age_total_excess_temp /
    as.vector(apply(age_data_cumulative[,,,"noint"], c("age", "sim"), sum))
age_total_excess_mean <- get_mean(age_total_excess_temp, total=T,
                                  extra.keep.dimensions = "age")
age_total_excess_quartile <- get_quantile(age_total_excess_temp, total=T,
                                      extra.keep.dimensions = "age")
age_total_pct_excess_mean <- get_mean(age_total_pct_excess_temp, total=T,
                                      extra.keep.dimensions = "age")
age_total_pct_excess_quartile <- get_quantile(age_total_pct_excess_temp, total=T,
                                          extra.keep.dimensions = "age")
# Absolute excess
age_total_excess_df <- merge(
    melt(round(age_total_excess_mean), value.name = "mean"),
    melt(round(age_total_excess_quartile)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("age", "intervention")
)
write.csv(age_total_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/age_abs_excess.csv"),
          row.names = F)

# Percent excess
age_percent_excess_df <- merge(
    melt(round(age_total_pct_excess_mean*100, 1), value.name = "mean"),
    melt(round(age_total_pct_excess_quartile*100, 1)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("age", "intervention")
)
write.csv(age_percent_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/age_pct_excess.csv"),
          row.names = F)

# Race Results----
race_data_cumulative <- apply(incidence.by.race,
                             c("race", "sim", "location", "intervention"),
                             sum)
race_excess_temp <- race_data_cumulative[,,,SURVEY.INTERVENTION.NAMES] -
    as.vector(race_data_cumulative[,,,"noint"])
race_pct_excess_temp <- race_excess_temp / as.vector(race_data_cumulative[,,,"noint"])
race_total_excess_temp <- apply(race_excess_temp,
                               c("race", "sim", "intervention"),
                               sum)
race_total_pct_excess_temp <- race_total_excess_temp /
    as.vector(apply(race_data_cumulative[,,,"noint"], c("race", "sim"), sum))
race_total_excess_mean <- get_mean(race_total_excess_temp, total=T,
                                  extra.keep.dimensions = "race")
race_total_excess_quartile <- get_quantile(race_total_excess_temp, total=T,
                                          extra.keep.dimensions = "race")
race_total_pct_excess_mean <- get_mean(race_total_pct_excess_temp, total=T,
                                      extra.keep.dimensions = "race")
race_total_pct_excess_quartile <- get_quantile(race_total_pct_excess_temp, total=T,
                                              extra.keep.dimensions = "race")
# Absolute excess
race_total_excess_df <- merge(
    melt(round(race_total_excess_mean), value.name = "mean"),
    melt(round(race_total_excess_quartile)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("race", "intervention")
)
write.csv(race_total_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/race_abs_excess.csv"),
          row.names = F)

# Percent excess
race_percent_excess_df <- merge(
    melt(round(race_total_pct_excess_mean*100, 1), value.name = "mean"),
    melt(round(race_total_pct_excess_quartile*100, 1)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("race", "intervention")
)
write.csv(race_percent_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/race_pct_excess.csv"),
          row.names = F)

# Sex/risk Results----
sexrisk_data_cumulative <- apply(incidence.by.sex.risk,
                              c("sex", "risk", "sim", "location", "intervention"),
                              sum)
sexrisk_excess_temp <- sexrisk_data_cumulative[,,,,SURVEY.INTERVENTION.NAMES] -
    as.vector(sexrisk_data_cumulative[,,,,"noint"])
sexrisk_pct_excess_temp <- sexrisk_excess_temp / as.vector(sexrisk_data_cumulative[,,,,"noint"])
sexrisk_total_excess_temp <- apply(sexrisk_excess_temp,
                                c("sex", "risk", "sim", "intervention"),
                                sum)
sexrisk_total_pct_excess_temp <- sexrisk_total_excess_temp /
    as.vector(apply(sexrisk_data_cumulative[,,,,"noint"], c("sex", "risk", "sim"), sum))
sexrisk_total_excess_mean <- get_mean(sexrisk_total_excess_temp, total=T,
                                   extra.keep.dimensions = c("sex", "risk"))
sexrisk_total_excess_quartile <- get_quantile(sexrisk_total_excess_temp, total=T,
                                           extra.keep.dimensions = c("sex", "risk"))
sexrisk_total_pct_excess_mean <- get_mean(sexrisk_total_pct_excess_temp, total=T,
                                       extra.keep.dimensions = c("sex", "risk"))
sexrisk_total_pct_excess_quartile <- get_quantile(sexrisk_total_pct_excess_temp, total=T,
                                               extra.keep.dimensions = c("sex", "risk"))

# Absolute excess
sexrisk_total_excess_df <- merge(
    melt(round(sexrisk_total_excess_mean), value.name = "mean"),
    melt(round(sexrisk_total_excess_quartile)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("sex", "risk", "intervention")
)
write.csv(sexrisk_total_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/sexrisk_abs_excess.csv"),
          row.names = F)

# Percent excess
sexrisk_percent_excess_df <- merge(
    melt(round(sexrisk_total_pct_excess_mean*100, 1), value.name = "mean"),
    melt(round(sexrisk_total_pct_excess_quartile*100, 1)) %>%
        pivot_wider(names_from = Var1, values_from = value),
    by=c("sex", "risk", "intervention")
)
write.csv(sexrisk_percent_excess_df,
          file = paste0(RESULTS.FILEPATH, "stratified_results/sexrisk_pct_excess.csv"),
          row.names = F)
