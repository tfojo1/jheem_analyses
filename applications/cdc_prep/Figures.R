
#Install libraries

library(ppcor)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(tibble)
library(ggrepel)
library(purrr)
library(ggforce)
library(tidytext)
library(forcats)


#calibration plots

#Alabama

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/cdcp_final.ehe.state-1000_AL_baseline.Rdata')


simplot(simset$last.sim(),simset,"testing",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cdc.funded.tests",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cumulative.cdc.prep.eligible",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cdc.fraction.prep.referred.of.eligible",dimension.values = list(year = 2014:2035))

#Louisiana 

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/cdcp_final.ehe.state-1000_LA_baseline.Rdata')

simplot(simset$last.sim(),simset,"testing",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cdc.funded.tests",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cumulative.cdc.prep.eligible",dimension.values = list(year = 2014:2035))
simplot(simset$last.sim(),simset,"cdc.fraction.prep.referred.of.eligible",dimension.values = list(year = 2014:2035))



#load simulation set

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_prep/cdc_prevention_results_2026-03-06.Rdata')

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

states <- dimnames(total.results)$location

#Plot all interventions 

# 1) Convert array -> long df
df <- as.data.frame(as.table(total.results)) |>
  setNames(c("year","sim","outcome","location","intervention","value")) |>
  mutate(
    year = as.integer(as.character(year)),
    sim  = as.integer(as.character(sim))
  )

# 2) Filter to what you want: LA & AL, incidence, years 2020-2035
df_inc <- df |>
  filter(
    outcome == "incidence",
    location %in% c("LA","AL"),
    year >= 2025, year <= 2035
  )

# 3) Summarize across sims (mean + optional uncertainty ribbon)
df_sum <- df_inc |>
  group_by(year, location, intervention) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    lo   = quantile(value, 0.025, na.rm = TRUE),
    hi   = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Set intervention colors ONCE (same colors in both states)
int_levels <- sort(unique(df_sum$intervention))
pal <- setNames(scales::hue_pal()(length(int_levels)), int_levels)

# 5) Plot (facet labels show state names)
ggplot(df_sum, aes(x = year, y = mean, color = intervention, fill = intervention)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  facet_wrap(~ location, ncol = 1,scales = "free_y") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    x = "Year",
    y = "Incidence",
    color = "Intervention",
    fill = "Intervention"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


#Excess Incidence Plots
library(dplyr)
library(tidyr)
library(ggplot2)

years <- 2025:2035

excess_summary <- function(state, scenario) {
  scen_mat  <- total.results[as.character(years), , "incidence", state, scenario]
  noint_mat <- total.results[as.character(years), , "incidence", state, "noint"]
  exc_mat   <- scen_mat - noint_mat   # year x sim
  
  as.data.frame(exc_mat) %>%
    mutate(year = years) %>%
    pivot_longer(-year, names_to = "sim", values_to = "excess") %>%
    group_by(year) %>%
    summarise(mean = mean(excess, na.rm = TRUE),
              lo   = quantile(excess, 0.05, na.rm = TRUE),
              hi   = quantile(excess, 0.95, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(state = state, scenario = scenario)
}

states <- c("AL","LA")
scenarios <- setdiff(dimnames(total.results)$intervention, "noint")

plots <- lapply(states, function(st) {
  lapply(scenarios, function(sc) {
    d <- excess_summary(st, sc)
    
    ggplot(d, aes(year, mean)) +
      geom_hline(yintercept = 0, linewidth = 0.4, alpha = 0.6) +
      geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
      geom_line(linewidth = 1) +
      theme_bw() +
      labs(title = paste(st, sc, sep = " — "),
           x = "Year", y = "Excess incidence (scenario − noint)")
  })
})


plots[[which(states=="LA")]][[which(scenarios=="cdcp.end.tracing.26")]]

#to run an intervention 
sim_int = cdc.cessation$run(sim = sim$first.sim(),end.year = 2035)



