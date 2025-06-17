
#Install libraries

library(ppcor)
library(tidyr)
library(dplyr)
library(ggplot2)

#load simualtion set

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/cdc_testing_results_2025-06-11.Rdata')

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

total.results[as.character(2025:2030),,"incidence","CA","cdct.pintr"]

total.results[as.character(2025:2030),,"incidence","AL","cdct.end"]

              
              
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

#State by state relative incidence 

# List of interventions to include
interventions <- c("cdct.end", "cdct.pintr", "cdct.bintr")

# Create an empty list to hold data.frames
plot_data_list <- list()

# Loop through interventions and states to collect results
for (intervention in interventions) {
    for (state in states) {
        ri <- (colSums(total.results[as.character(2025:2030), , "incidence", state, intervention]) -
                   colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])) /
            colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])
        
        temp_df <- data.frame(
            state = state,
            intervention = intervention,
            relative_incidence = ri * 100  # convert to percent
        )
        plot_data_list[[length(plot_data_list) + 1]] <- temp_df
    }
}

plot_data <- do.call(rbind, plot_data_list)

ggplot(plot_data, aes(x = relative_incidence, y = state, fill = intervention)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    labs(
        title = "Relative Incidence by State and Intervention (2025–2030)",
        x = "Relative Incidence (%)",
        y = "State",
        fill = "Intervention"
    ) +
    theme_minimal()


#Texas Plots 

# Add intervention name to each TX subset
tx_end <- subset(all.summary.data.cdct.end, state == "TX")
tx_end$intervention <- "CDC Testing Cessation"

tx_pintr <- subset(all.summary.data.cdct.pintr, state == "TX")
tx_pintr$intervention <- "Prolonged Interruption"

tx_bintr <- subset(all.summary.data.cdct.bintr, state == "TX")
tx_bintr$intervention <- "Brief Interruption"

# Combine into one dataframe
tx_combined <- rbind(tx_end, tx_pintr, tx_bintr)

ggplot(tx_combined, aes(x = year, y = mean, color = scenario, fill = scenario)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    facet_wrap(~ intervention) +
    scale_x_continuous(breaks = 2025:2035) +
    labs(
        title = "Excess HIV Incidence in TX Across Interventions",
        x = "Year",
        y = "Incidence"
    ) +
    theme_minimal()


#sensitivity analyses 

#Set Intervention:

int <- "cdct.bintr"

param_names <- dimnames(all.parameters)$parameter[389:423] #all cdc parameters 
states <- dimnames(all.parameters)[[3]]

param_by_state <- list()

for (state in states) {
    param_matrix <- all.parameters[389:423, , state, int]  
    param_df <- as.data.frame(t(param_matrix))                 
    colnames(param_df) <- param_names
    param_by_state[[state]] <- param_df
}

#Create relative incidence 

rel_incidence_list <- list()

for (state in states) {
    rel_incidence_list[[state]] <- (colSums(total.results[as.character(2025:2030), , "incidence", state, int]) - colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"]))/colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])
}


prcc_results <- list()

for (state in states) {
    param_df <- param_by_state[[state]]       # 100 × 35
    y <- rel_incidence_list[[state]]          # length 100
    
    prcc_input <- cbind(y,param_df)
    colnames(prcc_input)[1] <- "rel_inc"
    
    #correlation
    prcc_matrix <- pcor(prcc_input, method = "spearman")$estimate
    prcc_values <- prcc_matrix["rel_inc", -1]
    
    #p-values 
    prcc_pvals <- pcor(prcc_input, method = "spearman")$p.value
    pvals <- prcc_pvals["rel_inc", -1]
    
    df <- data.frame(
        parameter = names(prcc_values),
        PRCC = as.numeric(prcc_values),
        pvalue = as.numeric(pvals),
        state = state,
        stringsAsFactors = FALSE
    )
    
    prcc_results[[state]] <- df
    
}

#combine prcc results
all_prcc <- do.call(rbind, prcc_results)

all_prcc_plot <- all_prcc %>%
    mutate(
        direction = ifelse(PRCC > 0, "Positive", "Negative"),
        significant = pvalue < 0.05
    ) %>%
    group_by(state) %>%
    mutate(parameter = reorder(parameter, PRCC)) %>%
    ungroup()

#Lolliplot plot 

ggplot(all_prcc_plot, aes(x = PRCC, y = parameter)) +
    geom_segment(aes(x = 0, xend = PRCC, yend = parameter), color = "gray70") +
    geom_point(aes(color = direction, shape = significant), size = 2.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ state, scales = "free_y") +
    scale_color_manual(values = c("Positive" = "#1f78b4" , "Negative" = "#d95f02")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) +
    labs(
        title = paste0("PRCC of Parameters on Relative Incidence by State ",int),
        x = "Partial Rank Correlation Coefficient (PRCC)",
        y = "Parameter",
        color = "Direction",
        shape = "Significant (p < 0.05)"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(size = 6),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
    )

