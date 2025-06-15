

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/cdc_testing_results_2025-06-11.Rdata')

dimnames(total.results)

dim(total.results)

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

total.results[as.character(2025:2030),,"incidence","CA","cdct.pintr"]
              
              
states <- dimnames(total.results)$location

plot_incidence <- function(state, scenario){
  
    cdct_end_df <- as.data.frame(total.results[as.character(2025:2035),,"incidence",state,scenario])
    
    noint_df <- as.data.frame(total.results[as.character(2025:2035),,"incidence",state,"noint"])
    
    # Add year as a column
    cdct_end_long <- cdct_end_df %>%
        mutate(year = 2025:2035) %>%
        pivot_longer(cols = -year, names_to = "sim", values_to = "incidence") %>%
        mutate(scenario = scenario)
    
    noint_long <- noint_df %>%
        mutate(year = 2025:2035) %>%
        pivot_longer(cols = -year, names_to = "sim", values_to = "incidence") %>%
        mutate(scenario = "noint")
    
    # Combine
    all_data <- bind_rows(cdct_end_long, noint_long)

    
    
    summary_df <- all_data %>%
        group_by(year, scenario) %>%
        summarise(
            mean = mean(incidence),
            lower = quantile(incidence, 0.05),
            upper = quantile(incidence, 0.95),
            .groups = "drop"
        ) %>%
        mutate(state = state)
    
    return(summary_df)
}

all.summary.data.cdct.end <- map_dfr(states, ~plot_incidence(.x, scenario = "cdct.end"))

all.summary.data.cdct.pintr <- map_dfr(states, ~plot_incidence(.x, scenario = "cdct.pintr"))

all.summary.data.cdct.bintr <- map_dfr(states, ~plot_incidence(.x, scenario = "cdct.bintr"))



#CDCT End Excess Incidence

ggplot(all.summary.data.cdct.end, aes(x = year, y = mean, color = scenario, fill = scenario)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(
        title = "Excess HIV Incidence Across States: CDC Testing Cessation",
        x = "Year",
        y = "Incidence"
    ) +
    facet_wrap(~ state) +
    scale_x_continuous(breaks = 2025:2035) +
    theme_minimal()


#CDCT Prolonged Intr Excess Incidence

ggplot(all.summary.data.cdct.pintr, aes(x = year, y = mean, color = scenario, fill = scenario)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(
        title = "Excess HIV Incidence Across States: Prolonged Interruption",
        x = "Year",
        y = "Incidence"
    ) +
    facet_wrap(~ state) +
    scale_x_continuous(breaks = 2025:2035) +
    theme_minimal()

#CDCT Brief Intr Excess Inicdence 

ggplot(all.summary.data.cdct.bintr, aes(x = year, y = mean, color = scenario, fill = scenario)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    labs(
        title = "Excess HIV Incidence Across States: Brief Interruption",
        x = "Year",
        y = "Incidence"
    ) +
    facet_wrap(~ state) +
    scale_x_continuous(breaks = 2025:2035) +
    theme_minimal()

#Combined Excess Incidence Plots Cessation

#CDCT.END
wide_data.cdct.end <- all.summary.data.cdct.end %>%
    pivot_wider(names_from = scenario, values_from = c(mean, lower, upper))

excess_data.cdct.end <- wide_data.cdct.end %>%
    mutate(
        excess_mean = mean_cdct.end - mean_noint,
        excess_lower = lower_cdct.end - lower_noint,
        excess_upper = upper_cdct.end - upper_noint
    ) %>%
    select(year, state, excess_mean, excess_lower, excess_upper)


ggplot(excess_data.cdct.end, aes(x = year, y = excess_mean)) +
    geom_line(color = "steelblue") +
    geom_ribbon(aes(ymin = excess_lower, ymax = excess_upper), fill = "lightblue", alpha = 0.4) +
    facet_wrap(~ state, scales = "fixed") +  
    scale_y_continuous(limits = c(0, 1000)) +  
    labs(
        title = "Excess Incidence Cessation",
        y = "Excess Incidence",
        x = "Year"
    ) +
    theme_minimal()

#Combined Prolonged Intr Excess Incidence

#CDCT.PINTR
wide_data.cdct.pintr <- all.summary.data.cdct.pintr %>%
    pivot_wider(names_from = scenario, values_from = c(mean, lower, upper))

excess_data.cdct.pintr <- wide_data.cdct.pintr %>%
    mutate(
        excess_mean = mean_cdct.pintr - mean_noint,
        excess_lower = lower_cdct.pintr - lower_noint,
        excess_upper = upper_cdct.pintr - upper_noint
    ) %>%
    select(year, state, excess_mean, excess_lower, excess_upper)


ggplot(excess_data.cdct.pintr, aes(x = year, y = excess_mean)) +
    geom_line(color = "steelblue") +
    geom_ribbon(aes(ymin = excess_lower, ymax = excess_upper), fill = "lightblue", alpha = 0.4) +
    facet_wrap(~ state, scales = "fixed") +  
    scale_y_continuous(limits = c(0, 700)) + 
    labs(
        title = "Excess Incidence Prolonged Interruption",
        y = "Excess Incidence",
        x = "Year"
    ) +
    theme_minimal()

#Combined Excess Incidence Plots Brief Interruption

#CDCT.BINTR
wide_data.cdct.bintr <- all.summary.data.cdct.bintr %>%
    pivot_wider(names_from = scenario, values_from = c(mean, lower, upper))

excess_data.cdct.bintr <- wide_data.cdct.bintr %>%
    mutate(
        excess_mean = mean_cdct.bintr - mean_noint,
        excess_lower = lower_cdct.bintr - lower_noint,
        excess_upper = upper_cdct.bintr - upper_noint
    ) %>%
    select(year, state, excess_mean, excess_lower, excess_upper)


ggplot(excess_data.cdct.bintr, aes(x = year, y = excess_mean)) +
    geom_line(color = "steelblue") +
    geom_ribbon(aes(ymin = excess_lower, ymax = excess_upper), fill = "lightblue", alpha = 0.4) +
    facet_wrap(~ state, scales = "fixed") +  
    scale_y_continuous(limits = c(0, 500)) +  
    labs(
        title = "Excess Incidence Brief Interruption",
        y = "Excess Incidence",
        x = "Year"
    ) +
    theme_minimal()
