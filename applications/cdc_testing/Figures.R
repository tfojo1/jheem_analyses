
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
#load simulation set

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/cdc_testing_results_2025-06-19.Rdata')

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

total.results[as.character(2020:2030),,"incidence",,"noint"]

total.results[as.character(2020:2030),,"incidence",,"cdct.end"]

              
              
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

#State by state relative incidence Box Plots


# List of interventions to include
interventions <- c("cdct.end", "cdct.pintr", "cdct.bintr")

# Create an empty list to hold data.frames
plot_data_list <- list()

# Loop through interventions and states to collect state-level results 
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

# Add a national-level "Total" row (sum across all states)
for (intervention in interventions) {
    total_int <- apply(total.results[as.character(2025:2030), , "incidence", , intervention], 2, sum)
    total_noint <- apply(total.results[as.character(2025:2030), , "incidence", , "noint"], 2, sum)
    
    ri_total <- (total_int - total_noint) / total_noint
    
    total_df <- data.frame(
        state = "Total",
        intervention = intervention,
        relative_incidence = ri_total * 100  # in percent
    )
    
    plot_data_list[[length(plot_data_list) + 1]] <- total_df
}

# Combine all data into one dataframe
plot_data <- do.call(rbind, plot_data_list)

# Remove trailing dots in state names
plot_data$state <- gsub("\\.*$", "", plot_data$state)

# Sort states by mean relative incidence of 'cdct.end' (excluding "Total")
sorted_states <- plot_data %>%
    filter(intervention == "cdct.end", state != "Total") %>%
    group_by(state) %>%
    summarise(mean_ri = mean(relative_incidence, na.rm = TRUE)) %>%
    arrange(mean_ri) %>%
    pull(state)

# Set factor levels with "Total" at the top
plot_data$state <- factor(plot_data$state, levels = c("Total", sorted_states))

# Set factor order for interventions
plot_data$intervention <- factor(plot_data$intervention, levels = c("cdct.bintr", "cdct.pintr", "cdct.end"))

# Plot
ggplot(plot_data, aes(x = relative_incidence, y = state, fill = intervention)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    scale_fill_manual(
        values = c(
            "cdct.bintr" = "#00A1D5",
            "cdct.pintr" = "#DF8F44",
            "cdct.end"   = "#374E55"
        )
    ) +
    labs(
        title = "Relative Incidence by State and Intervention (2025–2030)",
        x = "Relative Incidence (%)",
        y = "State",
        fill = "Intervention"
    ) +
    theme_minimal()


#Texas Plots, LA Plots, IL Plots
# Subset and label TX
tx_end <- subset(all.summary.data.cdct.end, state == "TX")
tx_end$intervention <- "CDC Testing Cessation"

tx_pintr <- subset(all.summary.data.cdct.pintr, state == "TX")
tx_pintr$intervention <- "Prolonged Interruption"

tx_bintr <- subset(all.summary.data.cdct.bintr, state == "TX")
tx_bintr$intervention <- "Brief Interruption"

# Subset and label LA
la_end <- subset(all.summary.data.cdct.end, state == "LA")
la_end$intervention <- "CDC Testing Cessation"

la_pintr <- subset(all.summary.data.cdct.pintr, state == "LA")
la_pintr$intervention <- "Prolonged Interruption"

la_bintr <- subset(all.summary.data.cdct.bintr, state == "LA")
la_bintr$intervention <- "Brief Interruption"

# Subset and label IL
il_end <- subset(all.summary.data.cdct.end, state == "IL")
il_end$intervention <- "CDC Testing Cessation"

il_pintr <- subset(all.summary.data.cdct.pintr, state == "IL")
il_pintr$intervention <- "Prolonged Interruption"

il_bintr <- subset(all.summary.data.cdct.bintr, state == "IL")
il_bintr$intervention <- "Brief Interruption"

# Combine all
combined <- rbind(tx_end, tx_pintr, tx_bintr,
                  la_end, la_pintr, la_bintr,
                  il_end, il_pintr, il_bintr)

combined$intervention <- factor(combined$intervention,
                                levels = c("Brief Interruption", "Prolonged Interruption", "CDC Testing Cessation"))

combined$scenario <- factor(
    combined$scenario,
    levels = c("noint", "cdct.bintr", "cdct.pintr", "cdct.end")
)

combined$state <- factor(combined$state, levels = c("IL", "TX", "LA"))  # desired facet order

# Custom colors
jama_colors <- c(
    "noint"      = "purple",
    "cdct.bintr" = "#00A1D5",
    "cdct.pintr" = "#DF8F44",
    "cdct.end"   = "#374E55"
)

vline_data <- data.frame(
    intervention = c("Brief Interruption", "Prolonged Interruption"),
    year = c(2027, 2029)
)

# Plot
p <- ggplot(combined, aes(x = year, y = mean, color = scenario, fill = scenario)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    facet_grid(state ~ intervention)+  
    scale_x_continuous(breaks = 2025:2035) +
    scale_y_continuous(limits = c(0, 6500)) +
    scale_color_manual(values = jama_colors) +
    scale_fill_manual(values = jama_colors) +
    labs(
        title = "Excess HIV Incidence Across States and Interventions",
        x = "Year",
        y = "Incidence",
        color = "Scenario",
        fill = "Scenario"
    ) +
    theme_minimal() +
    geom_vline(
        data = vline_data,
        aes(xintercept = year),
        linetype = "dashed",
        color = "red",
        inherit.aes = FALSE
    )

print(p)


#Lolliplot sensitivity 

int <- "cdct.end"
param_names <- dimnames(all.parameters)$parameter[389:423]  # all CDC parameters
states <- dimnames(all.parameters)[[3]]

# Extract parameter values per state
param_by_state <- lapply(states, function(state) {
    param_matrix <- all.parameters[389:423, , state, int]
    param_df <- as.data.frame(t(param_matrix))
    colnames(param_df) <- param_names
    param_df
})
names(param_by_state) <- states

# Create relative incidence per state
rel_incidence_list <- lapply(states, function(state) {
    (colSums(total.results[as.character(2025:2030), , "incidence", state, int]) -
         colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])) /
        colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])
})
names(rel_incidence_list) <- states

# ---- Step 2: Calculate PRCC per state ----

prcc_results <- lapply(states, function(state) {
    param_df <- param_by_state[[state]]
    y <- rel_incidence_list[[state]]
    
    prcc_input <- cbind(rel_inc = y, param_df)
    
    prcc_out <- pcor(prcc_input, method = "spearman")
    
    data.frame(
        parameter = names(prcc_out$estimate["rel_inc", -1]),
        PRCC = as.numeric(prcc_out$estimate["rel_inc", -1]),
        pvalue = as.numeric(prcc_out$p.value["rel_inc", -1]),
        state = state,
        stringsAsFactors = FALSE
    )
})

# Combine all states
all_prcc <- do.call(rbind, prcc_results)

# ---- Step 3: Add direction and calculate mean PRCC across states ("Total") ----

all_prcc <- all_prcc %>%
    mutate(direction = ifelse(PRCC > 0, "Positive", "Negative"))

total_prcc <- all_prcc %>%
    group_by(parameter) %>%
    summarise(
        PRCC = mean(PRCC, na.rm = TRUE),
        state = "Total",
        direction = ifelse(mean(PRCC, na.rm = TRUE) > 0, "Positive", "Negative"),
        .groups = "drop"
    )

# Combine with state-level PRCCs
all_prcc_combined <- bind_rows(all_prcc, total_prcc)

# ---- Step 4: Order parameters for Total facet, and apply to all ----

# Order by absolute PRCC in Total
ordered_params <- total_prcc %>%
    arrange(desc(abs(PRCC))) %>%
    pull(parameter)

# Apply consistent factor levels
all_prcc_combined <- all_prcc_combined %>%
    mutate(
        parameter = factor(parameter, levels = rev(ordered_params)),
        state = factor(state, levels = c("Total", sort(unique(all_prcc$state))))
    )

# ---- Step 5: Plot ----

ggplot(all_prcc_combined, aes(x = PRCC, y = parameter)) +
    geom_segment(aes(x = 0, xend = PRCC, yend = parameter), color = "gray70") +
    geom_point(aes(color = direction), size = 2.5) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    facet_wrap(~ state, scales = "free_y") +
    scale_color_manual(values = c("Positive" = "#1f78b4", "Negative" = "#d95f02")) +
    labs(
        title = paste0("PRCC of Parameters on Relative Incidence by State (", int, ")"),
        x = "Partial Rank Correlation Coefficient (PRCC)",
        y = "Parameter",
        color = "Direction"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(size = 6),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
    )
#Table 1 Results 

# --- Step 1: Extract data per state for each intervention ---

states <- dimnames(total.results)[[4]]

# Cdct.end
plot_data_list_cdct.end <- vector("list", length(states))
names(plot_data_list_cdct.end) <- states

intervention <- "cdct.end"
for (a in 1:length(states)) {
    abs_diff <- colSums(total.results[as.character(2025:2030), , "incidence", states[a], intervention]) -
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    ri <- abs_diff /
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    temp_df <- data.frame(
        state = states[a],
        intervention = intervention,
        relative_incidence = ri * 100,
        abs_incidence = abs_diff
    )
    plot_data_list_cdct.end[[a]] <- temp_df
}

# Cdct.bintr
plot_data_list_cdct.bintr <- vector("list", length(states))
names(plot_data_list_cdct.bintr) <- states

intervention <- "cdct.bintr"
for (a in 1:length(states)) {
    abs_diff <- colSums(total.results[as.character(2025:2030), , "incidence", states[a], intervention]) -
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    ri <- abs_diff /
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    temp_df <- data.frame(
        state = states[a],
        intervention = intervention,
        relative_incidence = ri * 100,
        abs_incidence = abs_diff
    )
    plot_data_list_cdct.bintr[[a]] <- temp_df
}

# Cdct.pintr
plot_data_list_cdct.pintr <- vector("list", length(states))
names(plot_data_list_cdct.pintr) <- states

intervention <- "cdct.pintr"
for (a in 1:length(states)) {
    abs_diff <- colSums(total.results[as.character(2025:2030), , "incidence", states[a], intervention]) -
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    ri <- abs_diff /
        colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
    
    temp_df <- data.frame(
        state = states[a],
        intervention = intervention,
        relative_incidence = ri * 100,
        abs_incidence = abs_diff
    )
    plot_data_list_cdct.pintr[[a]] <- temp_df
}

# --- Step 2: Extract summary stats ---
extract_summary <- function(df_list, intervention_name) {
    do.call(rbind, lapply(seq_along(df_list), function(i) {
        rel_x <- df_list[[i]]$relative_incidence
        abs_x <- df_list[[i]]$abs_incidence
        tibble(
            State = states[i],
            Intervention = intervention_name,
            Mean = mean(rel_x),
            Lower = quantile(rel_x, 0.025),
            Upper = quantile(rel_x, 0.975),
            AbsMean = mean(abs_x),
            AbsLower = quantile(abs_x, 0.025),
            AbsUpper = quantile(abs_x, 0.975)
        )
    }))
}

summary_data <- bind_rows(
    extract_summary(plot_data_list_cdct.end, "Cessation"),
    extract_summary(plot_data_list_cdct.bintr, "Brief Interruption"),
    extract_summary(plot_data_list_cdct.pintr, "Prolonged Interruption")
)

# --- Step 3: Add TOTAL row for each intervention ---
total_rows <- list()
interventions <- c("cdct.end", "cdct.pintr", "cdct.bintr")
intervention_labels <- c("Cessation", "Prolonged Interruption", "Brief Interruption")

for (i in seq_along(interventions)) {
    intervention <- interventions[i]
    label <- intervention_labels[i]
    
    total_int <- apply(total.results[as.character(2025:2030), , "incidence", , intervention], 2, sum)
    total_noint <- apply(total.results[as.character(2025:2030), , "incidence", , "noint"], 2, sum)
    
    abs_diff <- total_int - total_noint
    ri_total <- abs_diff / total_noint
    
    total_rows[[i]] <- tibble(
        State = "Total",
        Intervention = label,
        Mean = mean(ri_total) * 100,
        Lower = quantile(ri_total, 0.025) * 100,
        Upper = quantile(ri_total, 0.975) * 100,
        AbsMean = mean(abs_diff),
        AbsLower = quantile(abs_diff, 0.025),
        AbsUpper = quantile(abs_diff, 0.975)
    )
}
summary_data_total <- bind_rows(total_rows)

# --- Step 4: Combine state + total summaries ---
summary_data <- bind_rows(summary_data, summary_data_total)

# --- Step 5: Format labels ---
summary_data <- summary_data %>%
    mutate(
        across(c(Mean, Lower, Upper), round, 1),
        across(c(AbsMean, AbsLower, AbsUpper), round, 0),
        AbsMean_fmt = format(AbsMean, big.mark = ","),
        AbsLower_fmt = format(AbsLower, big.mark = ","),
        AbsUpper_fmt = format(AbsUpper, big.mark = ","),
        abs_label = paste0(AbsMean_fmt, " [", AbsLower_fmt, "–", AbsUpper_fmt, "]"),
        label = paste0(abs_label, "\n", Mean, "% [", Lower, "–", Upper, "]")
    )

# --- Step 6: Prepare final data for heatmap ---
# Separate 'Total' from the rest
heatmap_data_main <- summary_data %>%
    filter(State != "Total") %>%
    arrange(Mean)  # Worst (highest Mean) to best

# Set factor levels based on ordered states
ordered_states <- unique(heatmap_data_main$State)

# Append Total
heatmap_data_total <- summary_data %>%
    filter(State == "Total")

# Combine data back
heatmap_data <- bind_rows(heatmap_data_main, heatmap_data_total) %>%
    mutate(
        State = factor(State, levels = c(ordered_states, "Total")),
        Intervention = factor(Intervention, levels = c("Cessation", "Prolonged Interruption", "Brief Interruption"))
    )

# --- Step 7: Plot heatmap with TOTAL row at bottom ---
    ggplot(heatmap_data, aes(x = Intervention, y = State, fill = Mean)) +
    geom_tile(color = "white") +
    geom_text(
        aes(label = label),
        color = "black", size = 4.2, lineheight = 1.1
    ) +
    scale_fill_gradient2(
        low = "yellow",
        mid = "orange",
        high = "red",
        midpoint = 10,
        limits = range(heatmap_data$Mean, na.rm = TRUE),
        space = "Lab"
    ) +
    labs(
        title = "Relative and Absolute Excess Incidence by State and Intervention (2025–2030)",
        x = "Intervention",
        y = "State",
        fill = "Mean %"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 30, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")
    )

#Scatter plot Fraction CDC HIV Testing
    
    #Metric of urbancity from 2020 US census
    urbanicity <- read.csv("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/urbanicity.csv", header = TRUE)
    urbanicity<- urbanicity[,-1]
    
    urbanicity_numeric <- as.data.frame(
        lapply(urbanicity, function(col) as.numeric(gsub(",", "", col)))
    )
    
    urbanicity_numeric[4,] <- urbanicity_numeric[2,]/urbanicity_numeric[1,]
    
    urban_metric <- urbanicity_numeric[4, c("Alabama","California","Florida","Georgia",
                                            "Illinois","Louisiana","Missouri",
                                            "Mississippi","New.York","Texas","Wisconsin")]
    
    save(urban_metric, file = "urban_metric.RData")
    
    colnames(urban_metric) <- NULL
    rownames(urban_metric) <- NULL
    
    
    intervention <- "cdct.end"
    # Loop through interventions and states to collect results
    for (a in 1:length(states)) {
        ri <- (colSums(total.results[as.character(2025:2030), , "incidence", states[a], intervention]) -
                   colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])) /
            colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
        
        HIV.tests <- mean(total.results[as.character(2025),,"cdc.funded.tests",states[a],intervention]/total.results[as.character(2025),,"total.hiv.tests",states[a],intervention])
        HIV.suppression <- mean(total.results[as.character(2025),,"suppression",states[a],intervention]/total.results[as.character(2025),,"diagnosed.prevalence",states[a],intervention])
        HIV.transmission <- mean(total.results[as.character(2025),,"new",states[a],intervention]/total.results[as.character(2025),,"diagnosed.prevalence",states[a],intervention])
        HIV.diagnoses <- mean(total.results[as.character(2025),,"total.cdc.hiv.test.positivity",states[a],intervention]/total.results[as.character(2025),,"new",states[a],intervention])
        incidence <- mean(total.results[as.character(2025),,"incidence",states[a],intervention])
        
        
        temp_df <- data.frame(
            state = states[a],
            intervention = intervention,
            relative_incidence = mean(ri*100),
            cdc.effect.test = HIV.tests,
            cdc.effect.supp = HIV.suppression,
            cdc.effect.transmission = HIV.transmission, 
            cdc.effect.diagnoses = HIV.diagnoses,
            total.incidence = incidence,
            urban = urban_metric[a]
            
        )
        cdc_effect_scatter_data_list_cdct.end[[a]] <- temp_df
    }
    
    scatter_plot.cdct.end <- do.call(rbind, cdc_effect_scatter_data_list_cdct.end)
    
    # Keep only cdct.end data
    scatter_df <- scatter_plot.cdct.end
    
    # Pivot to long format for x-axis effects
    scatter_long <- scatter_df %>%
        pivot_longer(
            cols = c(cdc.effect.test, cdc.effect.supp, cdc.effect.transmission, cdc.effect.diagnoses),
            names_to = "EffectType",
            values_to = "EffectValue"
        )
    
    # Rename facets for clarity
    scatter_long$EffectType <- recode(scatter_long$EffectType,
                                      "cdc.effect.test" = "CDC HIV Tests",
                                      "cdc.effect.supp" = "Viral Suppression",
                                      "cdc.effect.transmission" = "Transmission Rate",
                                      "cdc.effect.diagnoses" = "CDC Diagnoses")
    
    #Calculate PRCC
    
    pcor_data <- scatter_df %>%
        dplyr::select(
            relative_incidence,
            cdc.effect.test,
            cdc.effect.supp,
            cdc.effect.transmission,
            cdc.effect.diagnoses,
            urban,
            total.incidence
        )
    
    
    pcor_results <- pcor(pcor_data, method = "spearman")$estimate["relative_incidence", -1]
    
    
    prcc_df <- tibble::tibble(
        EffectType = c("cdc.effect.test", "cdc.effect.supp", "cdc.effect.transmission", "cdc.effect.diagnoses"),
        PRCC = round(as.numeric(pcor_results[1:4]),3)
    ) %>%
        mutate(
            EffectType = recode(EffectType,
                                "cdc.effect.test" = "CDC HIV Tests",
                                "cdc.effect.supp" = "Viral Suppression",
                                "cdc.effect.transmission" = "Transmission Rate",
                                "cdc.effect.diagnoses" = "CDC Diagnoses"
            ),
            label = paste0("Correlation = ", PRCC),
            x = -Inf,  
            y = Inf    
        )
    
    
    
    # Plot
    ggplot(scatter_long, aes(x = EffectValue, y = relative_incidence, label = state)) +
        geom_point(aes(size = total.incidence, color = urban), alpha = 0.8) +
        geom_text_repel(size = 3, max.overlaps = 100) +
        facet_wrap(~ EffectType, scales = "free_x") +
        scale_size_continuous(name = "Total Incidence") +
        scale_color_gradient(name = "% Urban Population", low = "yellow", high = "red") +
        labs(
            title = "Drivers of Relative Excess Incidence (Cessation Intervention)",
            x = "CDC Effect Value",
            y = "Relative Excess Incidence (%)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            panel.border = element_rect(color = "black", fill = NA),
            strip.background = element_rect(color = "black", fill = "lightgray"),
            strip.text = element_text(face = "bold")
        ) + geom_text(
            data = prcc_df,
            mapping = aes(x = x, y = y, label = label),
            hjust = -0.1, vjust = 1.3,
            inherit.aes = FALSE,
            size = 4, fontface = "italic"
        )
    
    
#Urbancity vs RI 
    
    ggplot(scatter_df, aes(x = urban, y = relative_incidence, label = state)) +
        geom_point(color = "darkblue", size = 3, alpha = 0.8) +
        geom_text_repel(size = 3, max.overlaps = 100) +
        labs(
            title = "Urbanicity vs Relative Excess Incidence",
            x = "Proportion of Population in Urban Areas",
            y = "Relative Excess Incidence (%)"
        ) +
        theme_minimal(base_size = 12)

#High Low sensitivity plot

# -- Setup --
int <- "cdct.end"
param_names <- dimnames(all.parameters)$parameter[389:423]
states <- dimnames(all.parameters)[[3]]

# -- Extract parameter values per state (list of dataframes) --
param_by_state <- lapply(states, function(state) {
    param_matrix <- all.parameters[389:423, , state, int]
    param_df <- as.data.frame(t(param_matrix))
    colnames(param_df) <- param_names
    param_df
})
names(param_by_state) <- states

# -- Relative excess incidence per simulation per state --
rel_incidence_list <- lapply(states, function(state) {
    (colSums(total.results[as.character(2025:2030), , "incidence", state, int]) -
         colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])) /
        colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])
})
names(rel_incidence_list) <- states

# -- Boxplot data: group simulations by parameter quintile and collect relative incidence --
plot_data <- list()

for (state in names(param_by_state)) {
    param_df <- param_by_state[[state]]
    param_df$sim <- 1:nrow(param_df)
    rel_inc <- rel_incidence_list[[state]]
    
    for (param in colnames(param_df)[-ncol(param_df)]) {
        param_vals <- param_df[[param]]
        quintiles <- quantile(param_vals, probs = c(0.2, 0.8), na.rm = TRUE)
        
        sim_class <- case_when(
            param_vals <= quintiles[1] ~ "Low Quintile",
            param_vals >= quintiles[2] ~ "High Quintile",
            TRUE ~ "All"
        )
        
        temp_df <- data.frame(
            parameter = param,
            rel_inc = rel_inc,
            group = sim_class,
            state = state,
            stringsAsFactors = FALSE
        )
        
        plot_data[[length(plot_data) + 1]] <- temp_df
    }
}

plot_df <- bind_rows(plot_data)

# -- Compute range of rel_inc by parameter and state and reorder accordingly --
range_df <- plot_df %>%
    group_by(state, parameter) %>%
    summarize(range = max(rel_inc, na.rm = TRUE) - min(rel_inc, na.rm = TRUE), .groups = "drop")

plot_df <- left_join(plot_df, range_df, by = c("state", "parameter"))
plot_df$parameter <- reorder_within(plot_df$parameter, plot_df$range, plot_df$state)
plot_df$group <- factor(plot_df$group, levels = c("Low Quintile", "All", "High Quintile"))

# -- Plot function --
plot_by_state <- function(state_name) {
    state_data <- plot_df %>% filter(state == state_name)
    
    ggplot(state_data, aes(x = rel_inc * 100, y = parameter, fill = group)) +
        geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.7), width = 0.6) +
        scale_fill_manual(values = c(
            "Low Quintile" = "#FFD700",
            "All" = "#FDBE85",
            "High Quintile" = "#D94701"
        )) +
        labs(
            title = paste0("Sensitivity of Relative Excess Incidence – ", state_name),
            x = "Relative Excess Incidence (%)",
            y = NULL,
            fill = "Simulation Subset"
        ) +
        scale_y_reordered() +
        theme_minimal(base_size = 12) +
        theme(
            axis.text.y = element_text(size = 6),
            plot.title = element_text(face = "bold", size = 14),
            legend.position = "bottom"
        )
}

# -- Generate plots by state --
for (state_name in unique(plot_df$state)) {
    print(plot_by_state(state_name))
    readline(prompt = "Press [Enter] to show the next plot...")
}

#Additional Infections per Test Saved

test_averted <- function(int,state){
    
    infections_noint <- mean(colSums(total.results[as.character(2025:2030),,"incidence",state,"noint"]))
    infections_int <- mean(colSums(total.results[as.character(2025:2030),,"incidence",state,int]))
    test_noint <- mean(colSums(total.results[as.character(2025:2030),,"total.hiv.tests",state,"noint"]))
    test_int <- mean(colSums(total.results[as.character(2025:2030),,"total.hiv.tests",state,int]))
    
    infections_per_test_saved <- (infections_int-infections_noint)/(test_noint-test_int)
    return(infections_per_test_saved)
    
}


combos <- expand.grid(int = interventions, state = states, stringsAsFactors = FALSE)

combos$value <- mapply(test_averted, combos$int, combos$state)

# Ensure intervention order is correct
combos$int <- factor(combos$int, levels = c("cdct.end", "cdct.pintr", "cdct.bintr"))

# Order states by decreasing infection-per-test-saved
state_order <- combos %>%
    group_by(state) %>%
    summarize(avg_value = mean(value, na.rm = TRUE)) %>%
    arrange(desc(avg_value)) %>%
    pull(state)

combos$state <- factor(combos$state, levels = state_order)

# Round values for labels
combos$label <- round(combos$value, 2)

# Plot
ggplot(combos, aes(x = state, y = int, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3, color = "black") +
    scale_fill_gradient(low = "yellow", high = "red", name = "Infections / Test Saved") +
    labs(x = "State", y = "Intervention", title = "Infections per Test Saved (2025–2030)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5))


#Calibration plots for no intervention scenario 

# Load calibration function
print("Registering calibrations")
source('applications/cdc_testing/register_cdc_testing_calibration.R')

results.by.state <- list()


# Loop through each state
for (state in states) {
    cat("Processing state:", state, "\n")
    
    simset = retrieve.simulation.set("ehe",state, "final.ehe.state", n.sim = 1000)
    # Load the corresponding RData file
  
    #simset$location
    #simset$save()

    
    
    simplot(simset,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030),summary.type = "mean.and.interval")
    simplot(simset,"cdc.funded.tests",dimension.values = list(year = 2010:2030),summary.type = "mean.and.interval")
    
    
    
    # Run setup calibration
   # results <- set.up.transmute.calibration('cdct', 
                                         #   from.calibration.code = 'final.ehe.state', 
                                         #   location = state, 
                                         #   n.chunks = 10,
                                         #   n.sim = 1000,
                                         #   allow.overwrite.cache = TRUE,
                                         #   return.simulations = TRUE)
    
   # results.by.state[[state]] <- results
}

#simplot(results,"total.cdc.hiv.test.positivity",dimension.values = list(year = 2010:2030))
#simplot(results,"cdc.funded.tests",dimension.values = list(year = 2010:2030))
