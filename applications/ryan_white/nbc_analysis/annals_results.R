if (!exists("total.incidence"))
    x = load("Q:results/ryan_white/ryan_white_results_state_nanb_2026_2025-10-02.Rdata")
total.incidence <- total.incidence[as.character(2025:2030),,,]

# I'll mostly use "total.incidence"

# Find excess and % excess by location
# For both interventions vs. "noint"
# mean and quantile

# "total.incidence" has 26 years (use only 2026:2030),
# 1000 sims, 30 locations, and 3 interventions
# "noint", "nanb.rw.end.26", and "nanb.rw.intr.26"

# Define helpers
get_mean <- function(data, total=F) {
    location <- if (total) NULL else "location"
    apply(data,
          c(location, "intervention"),
          mean)
}
get_quantile <- function(data, total=F) {
    location <- if (total) NULL else "location"
    apply(data,
          c(location, "intervention"),
          quantile,
          probs = c(0.025, 0.975))
}

# Initial data parsing----
data_int_years <- apply(total.incidence[as.character(2026:2030),,,],
                        c("sim", "location", "intervention"),
                        sum)
data_noint <- data_int_years[,,"noint"]
data_ints <- data_int_years[,,2:3]

excess_temp <- data_ints - rep(data_noint, 2)
pct_excess_temp <- excess_temp / rep(data_noint, 2)
total_excess_temp <- apply(excess_temp,
                           c("sim", "intervention"),
                           sum)
total_pct_excess_temp <- total_excess_temp /
    rep(apply(data_noint, "sim", sum), 2)

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

# Total
total_excess_mean <- get_mean(total_excess_temp, total=T)
total_excess_quantile <- get_quantile(total_excess_temp, total=T)

# Percent Excess----
# By state
pct_excess_mean <- get_mean(pct_excess_temp)
pct_excess_quantile <- get_quantile(pct_excess_temp)

# Total
total_pct_excess_mean <- get_mean(total_pct_excess_temp,
                                  total=T)
total_pct_excess_quantile <- get_quantile(total_pct_excess_temp,
                                          total=T)

# Analysis Highlights----
(round(total_excess_mean))
(round(total_excess_quantile))
(round(total_pct_excess_mean*100,1))
(round(total_pct_excess_quantile*100,1))
