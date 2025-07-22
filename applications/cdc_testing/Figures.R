
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

#load simulation set

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/cdc_testing_results_2025-07-21.Rdata')

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

total.results[as.character(2015:2030),,"incidence",,"noint"]

total.results[as.character(2015:2030),,"incidence",,"cdct.end"]

              
              
states <- dimnames(total.results)$location

plot_incidence <- function(state, scenario){
    
    cdct_end_df <- as.data.frame(total.results[as.character(2020:2035),,"incidence",state,scenario])
    
    noint_df <- as.data.frame(total.results[as.character(2020:2035),,"incidence",state,"noint"])
    
    # Add year as a column
    cdct_end_long <- cdct_end_df %>%
        mutate(year = 2020:2035) %>%
        pivot_longer(cols = -year, names_to = "sim", values_to = "incidence") %>%
        mutate(scenario = scenario)
    
    noint_long <- noint_df %>%
        mutate(year = 2020:2035) %>%
        pivot_longer(cols = -year, names_to = "sim", values_to = "incidence") %>%
        mutate(scenario = "noint")
    
    # Combine
    all_data <- bind_rows(cdct_end_long, noint_long)
    
    summary_df <- all_data %>%
        group_by(year, scenario) %>%
        summarise(
            mean = mean(incidence, na.rm = TRUE),
            lower = quantile(incidence, 0.05, na.rm = TRUE),
            upper = quantile(incidence, 0.95, na.rm = TRUE),
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

# Combine all data into one dataframe
plot_data <- do.call(rbind, plot_data_list)

# Remove trailing dots in state names
plot_data$state <- gsub("\\.*$", "", plot_data$state)

# OPTIONAL: Convert state abbreviations to full names (except "Total")
state_lookup <- setNames(state.name, state.abb)
plot_data$state <- ifelse(
    plot_data$state %in% names(state_lookup),
    state_lookup[plot_data$state],
    plot_data$state
)

# Sort states by mean relative incidence of 'cdct.end' (excluding "Total")
sorted_states <- plot_data %>%
    filter(intervention == "cdct.end", state != "Total") %>%
    group_by(state) %>%
    summarise(mean_ri = mean(relative_incidence, na.rm = TRUE)) %>%
    arrange(mean_ri) %>%
    pull(state)

# Set factor levels with "Total" at the top
plot_data$state <- factor(plot_data$state, levels = c("Total", sorted_states))

# Set factor order for interventions with descriptive labels
plot_data$intervention <- factor(
    plot_data$intervention,
    levels = c("cdct.bintr", "cdct.pintr", "cdct.end"),
    labels = c("Brief Interruption", "Prolonged Interruption", "Cessation")
)


# Flag outliers above 40%
plot_data$relative_incidence_capped <- ifelse(
    plot_data$relative_incidence > 40,
    40,
    plot_data$relative_incidence
)
plot_data$asterisk <- plot_data$relative_incidence > 40

# --- Plot ---

ggplot(plot_data, aes(x = relative_incidence_capped, y = state, fill = intervention)) +
    geom_boxplot(
        position = position_dodge(width = 0.8),
        outlier.shape = NA
    ) +
    geom_text(
        data = subset(plot_data, asterisk),
        aes(label = "*"),
        x = 40.5,  # just past truncation
        size = 5,
        hjust = 0
    ) +
    scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, 45),
        expand = expansion(mult = c(0, 0.1))  # give space for asterisk
    ) +
    scale_fill_manual(
        values = c(
            "Cessation" = "#374E55",
            "Prolonged Interruption" = "#DF8F44",
            "Brief Interruption" = "#00A1D5"
        )
    ) +
    labs(
        x = "Relative Excess HIV Infections from 2025–2030",
        y = NULL,
        fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 13)
    )

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
# Factor levels
combined$intervention <- factor(combined$intervention,
                                levels = c("Brief Interruption", "Prolonged Interruption", "CDC Testing Cessation"))
combined$scenario <- factor(combined$scenario,
                            levels = c("noint", "cdct.bintr", "cdct.pintr", "cdct.end"))
combined$state <- factor(combined$state, levels = c("IL", "TX", "LA"))

# Colors
jama_colors <- c(
    "noint" = "#1b7837",      # Continuation (green)
    "cdct.bintr" = "#00A1D5", # Brief Interruption (blue)
    "cdct.pintr" = "#DF8F44", # Prolonged Interruption (orange)
    "cdct.end" = "#374E55"    # Cessation (dark gray)
)

# Intervention year markers
vline_data <- data.frame(
    intervention = c("Brief Interruption", "Prolonged Interruption"),
    year = c(2027, 2029)
)

# Y-axis limits by state
y_max_by_state <- list(
    IL = 1500,
    TX = 6000,
    LA = 2000
)

# Annotation text
annotations <- data.frame(
    state = rep(c("IL", "TX", "LA"), each = 3),
    intervention = rep(c("CDC Testing Cessation", "Prolonged Interruption", "Brief Interruption"), 3),
    year = c(2032, 2031, 2030, 2032, 2031, 2030, 2032, 2031, 2030),
    value = c(900, 800, 700, 3000, 2500, 2000, 1800, 1500, 1200),
    label = c(
        "178 (64–324)", "153 (55–278)", "78 (28–140)",             # IL
        "2304 (835–4120)", "1868 (693–3305)", "838 (328–1424)",    # TX
        "1388 (429–2823)", "1082 (349–2138)", "432 (155–780)"      # LA
    )
)

# Output folder
dir.create("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/individual_panels", showWarnings = FALSE)

# Split and plot
plot_data_list <- split(combined, list(combined$state, combined$intervention), drop = TRUE)

for (name in names(plot_data_list)) {
    df <- plot_data_list[[name]]
    
    this_state <- as.character(unique(df$state))
    this_intervention <- as.character(unique(df$intervention))
    ymax <- y_max_by_state[[this_state]]
    
    ann <- annotations %>%
        filter(state == this_state, intervention == this_intervention)
    
    vline <- vline_data %>%
        filter(intervention == this_intervention)
    
    p <- ggplot(df, aes(x = year, y = mean, color = scenario, fill = scenario)) +
        geom_line(size = 1.2) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
        geom_vline(data = vline, aes(xintercept = year), linetype = "dashed", color = "red") +
        geom_text_repel(data = ann,
                        aes(x = year, y = value, label = paste(label, "excess infections")),
                        inherit.aes = FALSE,
                        nudge_y = 300,
                        size = 5,
                        box.padding = 0.5,
                        point.padding = 0.5,
                        segment.color = "gray30",
                        max.overlaps = 10) +
        scale_x_continuous(breaks = 2020:2035) +
        scale_y_continuous(limits = c(0, ymax)) +
        scale_color_manual(values = jama_colors,
                           labels = c("Continuation", "Brief Interruption", "Prolonged Interruption", "Cessation")) +
        scale_fill_manual(values = jama_colors,
                          labels = c("Continuation", "Brief Interruption", "Prolonged Interruption", "Cessation")) +
        labs(
            title = paste(this_state, "-", this_intervention),
            x = "Year",
            y = "Incidence"
        ) +
        theme_bw(base_size = 18) +
        theme(
            panel.grid.major = element_line(color = "black", size = 0.2),
            panel.grid.minor = element_line(color = "black", size = 0.1),
            legend.position = "none",
            plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        guides(
            color = guide_legend(nrow = 1),
            fill = guide_legend(nrow = 1)
        )
    
    ggsave(
        filename = paste0("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/individual_panels/", gsub(" ", "_", paste(this_state, this_intervention)), ".png"),
        plot = p,
        width = 7,
        height = 6,
        dpi = 300
    )
}
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
    
    # --- Step 8: Format table with baseline incidence and 95% CIs ---
    
    # Get mean incidence if CDC-funded tests continue (column a)
    baseline_incidence <- sapply(states, function(state) {
        mean(colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"]))
    })
    
    baseline_df <- tibble(
        State = names(baseline_incidence),
        `Incidence if CDC-funded Tests Continue` = format(round(baseline_incidence, 0), big.mark = ",")
    )
    
    # Format existing summary_data for 95% CIs
    summary_data_formatted <- summary_data %>%
        mutate(
            AbsMean_fmt = format(round(AbsMean, 0), big.mark = ","),
            AbsLower_fmt = format(round(AbsLower, 0), big.mark = ","),
            AbsUpper_fmt = format(round(AbsUpper, 0), big.mark = ","),
            Mean_fmt = round(Mean, 1),
            Lower_fmt = round(Lower, 1),
            Upper_fmt = round(Upper, 1),
            
            AbsLabel = paste0(AbsMean_fmt, " [", AbsLower_fmt, "–", AbsUpper_fmt, "]"),
            RelLabel = paste0(Mean_fmt, "% [", Lower_fmt, "–", Upper_fmt, "]")
        ) %>%
        select(State, Intervention, AbsLabel, RelLabel)
    
    # Pivot wide for columns b–g
    summary_labels_wide <- summary_data_formatted %>%
        pivot_wider(
            names_from = Intervention,
            values_from = c(AbsLabel, RelLabel),
            names_glue = "{Intervention}_{.value}"
        )
    
    # Combine with baseline incidence to create final table
    final_table_with_cis <- baseline_df %>%
        left_join(summary_labels_wide, by = "State") %>%
        select(
            State,
            `Incidence if CDC-funded Tests Continue`,
            `Cessation_AbsLabel`,
            `Cessation_RelLabel`,
            `Prolonged Interruption_AbsLabel`,
            `Prolonged Interruption_RelLabel`,
            `Brief Interruption_AbsLabel`,
            `Brief Interruption_RelLabel`
        ) %>%
        rename(
            `Cessation - Number of Excess Infections` = `Cessation_AbsLabel`,
            `Cessation - Relative Excess Infections` = `Cessation_RelLabel`,
            `Prolonged Interruption - Number of Excess Infections` = `Prolonged Interruption_AbsLabel`,
            `Prolonged Interruption - Relative Excess Infections` = `Prolonged Interruption_RelLabel`,
            `Brief Interruption - Number of Excess Infections` = `Brief Interruption_AbsLabel`,
            `Brief Interruption - Relative Excess Infections` = `Brief Interruption_RelLabel`
        )
    
    
    write.table(final_table_with_cis, "Final_Excess_Infections_Table.txt", row.names = FALSE)
    

#Scatter plot Fraction CDC HIV Testing
    
  
    # Load or define urbanicity metric (preprocessed)
    #urbanicity <- read.csv("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/urbanicity.csv", header = TRUE)
    #urbanicity <- urbanicity[, -1]
    #urbanicity_numeric <- as.data.frame(lapply(urbanicity, function(col) as.numeric(gsub(",", "", col))))
    #urbanicity_numeric[4, ] <- urbanicity_numeric[2, ] / urbanicity_numeric[1, ]
    #urban_metric <- urbanicity_numeric[4, c("Alabama","Arizona","California", "Florida", "Georgia",
    #                                       "Illinois","Kentucky", "Louisiana","Maryland", "Missouri", "Mississippi",
    #                                      "New.York","Ohio","South.Carolina","Tennessee", "Texas","Washington","Wisconsin")]
    
  
    # -------------------------------
    # STEP 1: Prepare Data
    # -------------------------------
    
    source("/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/urbanicity_calculations.R")
    
    # Ensure urbanicity vector is properly ordered
    CDC.TESTING.LOCATIONS.URBANICITY <- CDC.TESTING.LOCATIONS.URBANICITY[order(names(CDC.TESTING.LOCATIONS.URBANICITY))]
    
    # Get ordered state abbreviations
    state_abb <- dimnames(total.results)$location
    state_abb <- state_abb[-length(state_abb)]
    states <- state_abb[order(state_abb)]
    
    # Drop any row/column names from the urbanicity metric (optional)
    colnames(urban_metric) <- NULL
    rownames(urban_metric) <- NULL
    
    # Select intervention of interest
    intervention <- "cdct.end"
    cdc_effect_scatter_data_list <- list()
    
    # Loop over states to extract relevant metrics
    for (a in 1:length(states)) {
        ri <- (colSums(total.results[as.character(2025:2030), , "incidence", states[a], intervention]) -
                   colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])) /
            colSums(total.results[as.character(2025:2030), , "incidence", states[a], "noint"])
        
        HIV.tests <- mean(total.results["2025", , "cdc.funded.tests", states[a], intervention] /
                              total.results["2025", , "total.hiv.tests", states[a], intervention])
        
        HIV.suppression <- mean(total.results["2025", , "suppression", states[a], intervention] /
                                    total.results["2025", , "diagnosed.prevalence", states[a], intervention])
        
        HIV.transmission <- mean(total.results["2025", , "new", states[a], intervention] /
                                     total.results["2025", , "diagnosed.prevalence", states[a], intervention])
        
        HIV.diagnoses <- mean(total.results["2025", , "total.cdc.hiv.test.positivity", states[a], intervention] /
                                  total.results["2025", , "new", states[a], intervention])
        
        incidence <- mean(total.results["2025", , "incidence", states[a], intervention])
        
        temp_df <- data.frame(
            state = states[a],
            intervention = intervention,
            relative_incidence = mean(ri * 100),
            cdc.effect.test = HIV.tests,
            cdc.effect.supp = HIV.suppression,
            cdc.effect.transmission = HIV.transmission,
            cdc.effect.diagnoses = HIV.diagnoses,
            total.incidence = incidence,
            urban = as.numeric(CDC.TESTING.LOCATIONS.URBANICITY[a])
        )
        
        cdc_effect_scatter_data_list[[a]] <- temp_df
    }
    
    # Combine all state data
    scatter_df <- do.call(rbind, cdc_effect_scatter_data_list)
    
    # -------------------------------
    # STEP 2: Define Variables & Labels
    # -------------------------------
    
    effect_vars <- c(
        "cdc.effect.test" = "CDC HIV Tests",
        "cdc.effect.supp" = "Viral Suppression",
        "cdc.effect.transmission" = "Transmission Rate",
        "cdc.effect.diagnoses" = "CDC Diagnoses",
        "urban" = "Proportion of Epidemic in Urban Areas"
    )
    
    x_labels_all <- c(
        "CDC HIV Tests" = "Proportion of HIV Tests Funded by the CDC in 2025",
        "Viral Suppression" = "Proportion of All HIV+ Residents Virally Suppressed in 2025",
        "Transmission Rate" = "Average Transmission Rate in 2025",
        "CDC Diagnoses" = "Proportion of HIV Diagnoses Made with CDC-Funded Tests in 2025",
        "Proportion of Epidemic in Urban Areas" = "Proportion of Epidemic in Urban Areas"
    )
    
    # -------------------------------
    # STEP 3: Generate Plots w/ Correlation
    # -------------------------------
    
    for (var in names(effect_vars)) {
        
        x_vals <- scatter_df[[var]]
        y_vals <- scatter_df$relative_incidence
        
        # Calculate Spearman correlation
        spearman_corr <- round(cor(x_vals, y_vals, method = "spearman", use = "complete.obs"), 3)
        corr_label <- paste0("Spearman \u03C1 = ", spearman_corr)  # \u03C1 = Greek rho
        
        # Generate scatter plot
        p <- ggplot(scatter_df, aes_string(x = var, y = "relative_incidence", label = "state")) +
            geom_point(aes(size = total.incidence, fill = urban), shape = 21, color = "black", alpha = 0.8) +
            geom_text_repel(size = 3.5, max.overlaps = 100) +
            geom_text(data = data.frame(x = -Inf, y = Inf, label = corr_label),
                      aes(x = x, y = y, label = label),
                      inherit.aes = FALSE,
                      hjust = -0.1, vjust = 1.3,
                      size = 4, fontface = "italic") +
            labs(
                x = x_labels_all[effect_vars[[var]]],
                y = "Relative Excess Incidence (%)",
                fill = "Proportion of Prevalent\nInfections in Urban Areas, 2020",
                size = "New Infections\nin 2025"
            ) +
            scale_fill_gradient(
                limits = c(0, 0.9),
                labels = percent_format(accuracy = 1),
                low = "yellow", high = "red"
            ) +
            scale_size_continuous(labels = comma) +
            theme_bw(base_size = 14) +
            theme(
                legend.position = "bottom",
                legend.box = "horizontal",
                legend.title = element_text(size = 9),
                legend.text = element_text(size = 8),
                axis.title.x = element_text(size = 10),
                axis.title.y = element_text(size = 10),
                axis.text = element_text(size = 9),
                plot.margin = margin(t = 10, r = 10, b = 60, l = 10),
                panel.border = element_rect(color = "black", fill = NA),
                strip.background = element_blank(),
                strip.text = element_blank(),
                plot.title = element_blank()
            ) +
            guides(
                fill = guide_colorbar(
                    title.position = "top",
                    barwidth = 8,
                    barheight = 0.4,
                    label.position = "bottom",
                    ticks.colour = "black"
                ),
                size = guide_legend(
                    title.position = "top",
                    title.hjust = 0.5,
                    nrow = 1,
                    override.aes = list(fill = "grey60")
                )
            )
        
        
        # Apply % x-axis format if applicable
        if (var %in% c("cdc.effect.test", "cdc.effect.supp", "cdc.effect.diagnoses", "urban")) {
            p <- p + scale_x_continuous(labels = percent_format(accuracy = 1))
        }
        
        # Save to file
        ggsave(
            filename = paste0("scatter_", gsub(" ", "_", tolower(effect_vars[[var]])), ".png"),
            plot = p,
            width = 6,
            height = 5,
            dpi = 300
        )
    }
    
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

test_averted <- function(int, state) {
    infections_noint <- colSums(total.results[as.character(2025:2030), , "incidence", state, "noint"])
    infections_int <- colSums(total.results[as.character(2025:2030), , "incidence", state, int])
    test_noint <- colSums(total.results[as.character(2025:2030), , "cdc.funded.tests", state, "noint"])
    test_int <- colSums(total.results[as.character(2025:2030), , "cdc.funded.tests", state, int])
    
    # Vector of estimates
    tests_per_infection <- (test_noint - test_int) / (infections_int - infections_noint)
    
    # Summary statistics
    mean_val <- mean(tests_per_infection, na.rm = TRUE)
    ci_lower <- quantile(tests_per_infection, 0.025, na.rm = TRUE)
    ci_upper <- quantile(tests_per_infection, 0.975, na.rm = TRUE)
    
    return(c(mean = mean_val, lower = ci_lower, upper = ci_upper))
}

# Define your interventions and states vectors first
# interventions <- c("cdct.end", "cdct.pintr", "cdct.bintr")
# states <- c("AL", "CA", "TX", ...)  # Your list of states

# Build combinations
combos <- expand.grid(int = interventions, state = states, stringsAsFactors = FALSE)

# Apply test_averted to all combinations
results_matrix <- mapply(test_averted, combos$int, combos$state)

# Combine results into the combos data frame
combos <- cbind(combos, as.data.frame(t(results_matrix)))

# Ensure intervention order is correct
combos$int <- factor(combos$int, levels = c("cdct.end", "cdct.pintr", "cdct.bintr"))

# Order states by decreasing mean value
state_order <- combos %>%
    group_by(state) %>%
    summarize(avg_value = mean(mean, na.rm = TRUE)) %>%
    arrange(desc(avg_value)) %>%
    pull(state)

combos$state <- factor(combos$state, levels = state_order)

# Round values to nearest whole number
combos$mean <- round(combos$mean)
combos$lower <- round(combos$lower)
combos$upper <- round(combos$upper)

# Create label: "mean [lower, upper]"
combos$label <- paste0(combos$mean, " [", combos$lower, ", ", combos$upper, "]")

# Plot (no shading)
ggplot(combos, aes(x = state, y = int)) +
    geom_tile(fill = "white", color = "black") +
    geom_text(aes(label = label), size = 3, color = "black") +
    labs(x = "State", y = "Intervention",
         title = "Tests per Infection Averted (2025–2030)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5))


# --- Calculate TOTAL rows across all states for each intervention ---
total_rows <- lapply(interventions, function(int) {
    # Sum across all states and years for each draw
    infections_noint <- apply(total.results[as.character(2025:2030), , "incidence", , "noint"], 2, function(x) sum(x, na.rm = TRUE))
    infections_int <- apply(total.results[as.character(2025:2030), , "incidence", , int], 2, function(x) sum(x, na.rm = TRUE))
    test_noint <- apply(total.results[as.character(2025:2030), , "cdc.funded.tests", , "noint"], 2, function(x) sum(x, na.rm = TRUE))
    test_int <- apply(total.results[as.character(2025:2030), , "cdc.funded.tests", , int], 2, function(x) sum(x, na.rm = TRUE))
    
    # Compute tests per infection averted for each draw
    tests_per_infection <- (test_noint - test_int) / (infections_int - infections_noint)
    
    # Summarize across 1000 draws
    tibble(
        int = int,
        state = "Total",
        mean = mean(tests_per_infection, na.rm = TRUE),
        lower = quantile(tests_per_infection, 0.025, na.rm = TRUE),
        upper = quantile(tests_per_infection, 0.975, na.rm = TRUE)
    )
})

# Combine all total rows into a single data frame
total_df <- bind_rows(total_rows) %>%
    mutate(
        mean = round(mean),
        lower = round(lower),
        upper = round(upper),
        label = paste0(mean, " [", lower, ", ", upper, "]")
    )

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

#Dodged boxplots for proportion tested regardless
# Define constants
state <- "Total"
scenarios <- c("cdct.end", "cdct.pintr", "cdct.bintr")
years <- as.character(2025:2030)

scenario_labels <- c(
    "cdct.end" = "Cessation",
    "cdct.pintr" = "Prolonged Interruption",
    "cdct.bintr" = "Brief Interruption"
)

jama_colors_labeled <- c(
    "Cessation" = "#374E55",
    "Prolonged Interruption" = "#DF8F44",
    "Brief Interruption" = "#00A1D5"
)

# Get baseline incidence (noint)
baseline_incidence <- colSums(total.results[years, , "incidence", state, "noint"])

# Get 1000 draws of proportion.tested.regardless for cdct.end
prop_vals <- all.parameters["proportion.tested.regardless", , "AL", "cdct.end"]

# Robust quantile binning
quintiles <- quantile(prop_vals, probs = seq(0, 1, 0.2), na.rm = TRUE)
quintiles <- unique(quintiles)

if (length(quintiles) > 1) {
    quintile_labels <- paste0(
        round(100 * head(quintiles, -1)), "%–",
        round(100 * tail(quintiles, -1)), "%"
    )
    
    prop_quintile <- cut(
        prop_vals,
        breaks = quintiles,
        include.lowest = TRUE,
        labels = quintile_labels
    )
} else {
    # If distribution is flat
    prop_quintile <- factor(rep("Identical", length(prop_vals)))
}

# Create data list
boxplot_data <- list()

for (scenario in scenarios) {
    scen_incidence <- colSums(total.results[years, , "incidence", state, scenario])
    abs_excess <- scen_incidence - baseline_incidence
    
    temp_df <- data.frame(
        excess_infections = abs_excess,
        quintile = prop_quintile,
        scenario = scenario,
        state = state
    )
    
    boxplot_data[[length(boxplot_data) + 1]] <- temp_df
}

# Combine data
plot_df <- do.call(rbind, boxplot_data)

# Convert scenario codes to labeled factors
plot_df$scenario <- factor(
    plot_df$scenario,
    levels = c("cdct.bintr", "cdct.pintr", "cdct.end"),
    labels = c("Brief Interruption", "Prolonged Interruption", "Cessation")
)

# Plot
ggplot(plot_df, aes(x = excess_infections, y = fct_rev(quintile), fill = scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
    scale_x_continuous(labels = comma) +
    scale_fill_manual(values = jama_colors_labeled, name = "Intervention Scenario") +
    labs(
        x = "Absolute Excess Infections (2025–2030)",
        y = "Proportion Tested Regardless (Quintile)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
    )

# Summary statistics
summary_df <- plot_df %>%
    group_by(scenario, quintile) %>%
    summarise(
        mean_excess = mean(excess_infections, na.rm = TRUE),
        iqr_lower = quantile(excess_infections, 0.25, na.rm = TRUE),
        iqr_upper = quantile(excess_infections, 0.75, na.rm = TRUE),
        .groups = 'drop'
    )
