
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

load('/Users/ruchita/Documents/Harvard/JHEEM/code/jheem_analyses/applications/cdc_testing/cdc_testing_results_2025-09-16.Rdata')

#stratified
dimnames(full.incidence)

dimnames(all.parameters)

total.results[as.character(2025:2035),,"incidence","Total","noint"]

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

# Clean state names
plot_data$state <- gsub("\\.*$", "", plot_data$state)
state_lookup <- setNames(state.name, state.abb)
plot_data$state <- ifelse(
    plot_data$state %in% names(state_lookup),
    state_lookup[plot_data$state],
    plot_data$state
)

# Sort states by mean relative incidence for cdct.end
sorted_states <- plot_data %>%
    filter(intervention == "cdct.end", state != "Total") %>%
    group_by(state) %>%
    summarise(mean_ri = mean(relative_incidence, na.rm = TRUE)) %>%
    arrange(mean_ri) %>%
    pull(state)

# Set factor levels
plot_data$state <- factor(plot_data$state, levels = c("Total", sorted_states))
plot_data$intervention <- factor(
    plot_data$intervention,
    levels = c("cdct.bintr", "cdct.pintr", "cdct.end"),
    labels = c("Brief Interruption", "Prolonged Interruption", "Cessation")
)

# Cap values at 40 for visualization
plot_data$relative_incidence_capped <- ifelse(
    plot_data$relative_incidence > 40,
    40,
    plot_data$relative_incidence
)

# Create asterisk annotation dataframe: one row per state only
asterisk_states <- c("Louisiana", "Missouri", "Mississippi")
asterisk_data <- plot_data %>%
    filter(state %in% asterisk_states) %>%
    group_by(state) %>%
    slice(1) %>%
    mutate(label = "*") %>%
    ungroup()

# ----- Plot -----
ggplot(plot_data, aes(x = relative_incidence_capped, y = state, fill = intervention)) +
    geom_boxplot(
        aes(group = interaction(state, intervention)),
        position = position_dodge2(width = 0.8, preserve = "single"),
        outlier.shape = NA
    ) +
    geom_text(
        data = asterisk_data,
        aes(x = 40.5, label = label),
        size = 5,
        hjust = 0
    ) +
    scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, 47),
        expand = expansion(mult = c(0, 0.1))
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
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18)
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
    LA = 2200
)

# Annotation text
annotations <- data.frame(
    state = rep(c("IL", "TX", "LA"), each = 3),
    intervention = rep(c("CDC Testing Cessation", "Prolonged Interruption", "Brief Interruption"), 3),
    year = c(2032, 2031, 2030, 2032, 2031, 2030, 2032, 2031, 2030),
    value = c(900, 800, 700, 3000, 2500, 2000, 1800, 1500, 1200),
    label = c(
        "174 (62–320)", "150 (53–274)", "77 (28–138)",             # IL
        "2303 (835–4120)", "1867 (693–3305)", "837 (328–1424)",    # TX
        "1387 (429–2823)", "1081 (349–2138)", "432 (155–780)"      # LA
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
                        size = 9,  # Increased size ~1.5x (default was 7)
                        box.padding = 0.6,
                        point.padding = 0.6,
                        segment.color = NA,  # Remove lines
                        max.overlaps = 20) +
        scale_x_continuous(breaks = c(2020, 2025, 2030, 2035)) +
        scale_y_continuous(limits = c(0, ymax)) +
        scale_color_manual(values = jama_colors,
                           labels = c("Continuation", "Brief Interruption", "Prolonged Interruption", "Cessation")) +
        scale_fill_manual(values = jama_colors,
                          labels = c("Continuation", "Brief Interruption", "Prolonged Interruption", "Cessation")) +
        labs(
            x = "Year",
            y = "Infections"
        ) +
        theme_bw(base_size = 22) +  # Increased base font size
        theme(
            panel.grid = element_blank(),
            legend.position = "none",
            plot.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 18),  # Enlarged
            axis.text.y = element_text(size = 18),
            axis.title.x = element_text(size = 24, face = "bold"),
            axis.title.y = element_text(size = 24, face = "bold")
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
        dpi = 300,
        limitsize = FALSE
    )
    
}
#Lolliplot sensitivity 

int <- "cdct.end"
param_names <- dimnames(all.parameters)$parameter[389:423]  # all CDC parameters
param_names <- c("CDC Funded Diagnoses OR Spline Point 1", "CDC Funded Diagnoses OR Spline Point 2", "CDC Funded Diagnoses OR Spline Point 3", "CDC Funded Diagnoses Change in Projection","CDC Funded Diagnoses OR MSM","CDC Funded Diagnoses OR MSM IDU", "CDC Funded Diagnoses OR IDU Male", "CDC Funded Diagnoses OR IDU Female", "CDC Funded Diagnoses OR Hetersoexual Male","CDC Funded Diagnoses OR Heterosexual Female","CDC Funded Diagnoses OR Black","CDC Funded Diagnoses OR Hispanic","CDC Funded Diagnoses OR Other Races","CDC Funded Diagnoses OR 13-24","CDC Funded Diagnoses OR 25-34","CDC Funded Diagnoses OR 35-44","CDC Funded Diagnoses OR 45-54","CDC Funded Diagnoses OR 55+","CDC Funded Tests OR", "CDC Funded Tests Slope OR","CDC Funded Tests OR MSM", "CDC Funded Tests OR MSM IDU","CDC Funded Tests OR IDU Male","CDC Funded Tests OR IDU Female","CDC Funded Tests OR Heterosexual Male","CDC Funded Tests OR Heterosexual Female","CDC Funded Tests OR Black","CDC Funded Tests OR Hispanic","CDC Funded Tests OR Other Races","CDC Funded Tests OR 13-24","CDC Funded Tests OR 25-34","CDC Funded Tests OR 35-44","CDC Funded Tests OR 45-54","CDC Funded Tests OR 55+","Proportion Tested Regardless")
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
        title = paste0("PRCC of Parameters on Relative Incidence by State (Cessation)"),
        x = "Partial Rank Correlation Coefficient (PRCC)",
        y = "Parameter",
        color = "Direction"
    ) +
    theme_minimal() +
    theme(
        axis.text.y = element_text(size = 4),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
    )


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
                      size = 5, fontface = "italic") +
            labs(
                x = x_labels_all[effect_vars[[var]]],
                y = "Relative Excess Incidence (%)",
                fill = "Proportion of Prevalent\nInfections in Urban Areas, 2020",
                size = "New Infections\nin 2025"
            ) +
            scale_fill_gradient(
                limits = c(0,1),
                labels = percent_format(accuracy = 1),
                low = "yellow", high = "red"
            ) +
            scale_size_continuous(labels = comma) +
            theme_bw(base_size = 14) +
            theme(
                legend.position = "none",
                axis.title.x = element_text(size = 12),
                axis.title.y = element_text(size = 12),
                axis.text = element_text(size = 10),
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
param_names <- c("CDC Funded Diagnoses OR Spline Point 1", "CDC Funded Diagnoses OR Spline Point 2", "CDC Funded Diagnoses OR Spline Point 3", "CDC Funded Diagnoses Change in Projection","CDC Funded Diagnoses OR MSM","CDC Funded Diagnoses OR MSM IDU", "CDC Funded Diagnoses OR IDU Male", "CDC Funded Diagnoses OR IDU Female", "CDC Funded Diagnoses OR Hetersoexual Male","CDC Funded Diagnoses OR Heterosexual Female","CDC Funded Diagnoses OR Black","CDC Funded Diagnoses OR Hispanic","CDC Funded Diagnoses OR Other Races","CDC Funded Diagnoses OR 13-24","CDC Funded Diagnoses OR 25-34","CDC Funded Diagnoses OR 35-44","CDC Funded Diagnoses OR 45-54","CDC Funded Diagnoses OR 55+","CDC Funded Tests OR", "CDC Funded Tests Slope OR","CDC Funded Tests OR MSM", "CDC Funded Tests OR MSM IDU","CDC Funded Tests OR IDU Male","CDC Funded Tests OR IDU Female","CDC Funded Tests OR Heterosexual Male","CDC Funded Tests OR Heterosexual Female","CDC Funded Tests OR Black","CDC Funded Tests OR Hispanic","CDC Funded Tests OR Other Races","CDC Funded Tests OR 13-24","CDC Funded Tests OR 25-34","CDC Funded Tests OR 35-44","CDC Funded Tests OR 45-54","CDC Funded Tests OR 55+","Proportion Tested Regardless")
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
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 20),
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
scenarios <- c("cdct.end.26", "cdct.pintr.26", "cdct.bintr.26")
years <- as.character(2025:2030)

scenario_labels <- c(
    "cdct.end.26" = "Cessation",
    "cdct.pintr.26" = "Prolonged Interruption",
    "cdct.bintr.26" = "Brief Interruption"
)

jama_colors_labeled <- c(
    "Cessation" = "#374E55",
    "Prolonged Interruption" = "#DF8F44",
    "Brief Interruption" = "#00A1D5"
)

# Get baseline incidence (noint)
baseline_incidence <- colSums(total.results[years, , "incidence", state, "noint"])

# Get 1000 draws of proportion.tested.regardless for cdct.end
prop_vals <- all.parameters["proportion.tested.regardless", , "AL", "cdct.end.26"]

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
    levels = c("cdct.bintr.26", "cdct.pintr.26", "cdct.end.26"),
    labels = c("Brief Interruption", "Prolonged Interruption", "Cessation")
)

# Plot
ggplot(plot_df, aes(x = excess_infections, y = fct_rev(quintile), fill = scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
    scale_x_continuous(labels = comma) +
    scale_fill_manual(values = jama_colors_labeled, name = NULL) +
    labs(
        x = "Number of Excess Infections (2025–2030)",
        y = "Proportion Tested Regardless (Quintile)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
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



#CROI Abstract Results, Updated Intervention 2026 

# ---- Make a copy and remove MN + NC, recompute Total ----
total.results.noMNNC <- total.results

# Get all location names
locs <- dimnames(total.results.noMNNC)$location

# States to include in the recomputed Total (exclude MN, NC, and the existing Total)
states_noMNNC <- setdiff(locs, c("MN", "NC", "Total"))

# Recompute Total across those states (dims: year x sim x outcome x intervention)
total_noMNNC <- apply(
    total.results.noMNNC[ , , , states_noMNNC, , drop = FALSE],
    c(1, 2, 3, 5),
    sum,
    na.rm = TRUE
)

# Replace the Total slice in the copy
total.results.noMNNC[ , , , "Total", ] <- total_noMNNC

# Drop MN and NC entirely from the structure
total.results.noMNNC <- total.results.noMNNC[ , , , setdiff(locs, c("MN", "NC")), , drop = FALSE]

# ---- Derived results based on the updated object ----
states <- dimnames(total.results.noMNNC)$location
years  <- as.character(2025:2030)

# Relative incidence (cdct.end.26 vs noint)
rel_incidence_list_end <- lapply(states, function(state) {
    (colSums(total.results.noMNNC[years, , "incidence", state, "cdct.end.26"]) -
         colSums(total.results.noMNNC[years, , "incidence", state, "noint"])) /
        colSums(total.results.noMNNC[years, , "incidence", state, "noint"])
})
names(rel_incidence_list_end) <- states

# Relative incidence (cdct.pintr.26 vs noint)
rel_incidence_list_pintr <- lapply(states, function(state) {
    (colSums(total.results.noMNNC[years, , "incidence", state, "cdct.pintr.26"]) -
         colSums(total.results.noMNNC[years, , "incidence", state, "noint"])) /
        colSums(total.results.noMNNC[years, , "incidence", state, "noint"])
})
names(rel_incidence_list_pintr) <- states

# Absolute differences
abs_incidence_list_end <- lapply(states, function(state) {
    colSums(total.results.noMNNC[years, , "incidence", state, "cdct.end.26"]) -
        colSums(total.results.noMNNC[years, , "incidence", state, "noint"])
})
names(abs_incidence_list_end) <- states

abs_incidence_list_pintr <- lapply(states, function(state) {
    colSums(total.results.noMNNC[years, , "incidence", state, "cdct.pintr.26"]) -
        colSums(total.results.noMNNC[years, , "incidence", state, "noint"])
})
names(abs_incidence_list_pintr) <- states

# Helper to summarise a *named* list of numeric vectors (each vector = 1000 sims)
summarise_list <- function(x_list, scenario, metric, to_percent = FALSE) {
    purrr::imap_dfr(x_list, ~{
        v <- .x
        tibble(
            state    = .y,
            scenario = scenario,
            metric   = metric,
            mean     = mean(v, na.rm = TRUE),
            lo95     = quantile(v, 0.025, na.rm = TRUE, names = FALSE),
            hi95     = quantile(v, 0.975, na.rm = TRUE, names = FALSE)
        )
    }) %>%
        { if (to_percent) dplyr::mutate(., dplyr::across(c(mean, lo95, hi95), ~ . * 100)) else . }
}

# --- Relative (proportions; switch to_percent=TRUE for %)
summary_rel_end   <- summarise_list(rel_incidence_list_end,   "cdct.end.26",   "relative", to_percent = FALSE)
summary_rel_pintr <- summarise_list(rel_incidence_list_pintr, "cdct.pintr.26", "relative", to_percent = FALSE)

# Optional percent versions
summary_rel_end_pct   <- summarise_list(rel_incidence_list_end,   "cdct.end.26",   "relative", to_percent = TRUE)
summary_rel_pintr_pct <- summarise_list(rel_incidence_list_pintr, "cdct.pintr.26", "relative", to_percent = TRUE)

# --- Absolute (counts)
summary_abs_end   <- summarise_list(abs_incidence_list_end,   "cdct.end.26",   "absolute")
summary_abs_pintr <- summarise_list(abs_incidence_list_pintr, "cdct.pintr.26", "absolute")

# --- Combined table (uses proportions for relative; swap in *_pct if you prefer %)
summary_all <- dplyr::bind_rows(
    summary_rel_end,
    summary_rel_pintr,
    summary_abs_end,
    summary_abs_pintr
)

# ---- Boxplots setup using the updated Total ----
state <- "Total"
scenarios <- c("cdct.end.26", "cdct.pintr.26")

scenario_labels <- c(
    "cdct.end.26"   = "Cessation",
    "cdct.pintr.26" = "Prolonged Interruption"
)

jama_colors_labeled <- c(
    "Cessation" = "#374E55",
    "Prolonged Interruption" = "#DF8F44"
)

# Baseline incidence (noint) using the recomputed Total
baseline_incidence <- colSums(total.results.noMNNC[years, , "incidence", state, "noint"])

# Example quintiles based on AL parameter draws (unchanged)
prop_vals <- all.parameters["proportion.tested.regardless", , "AL", "cdct.end.26"]

quintiles <- unique(quantile(prop_vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
if (length(quintiles) > 1) {
    quintile_labels <- paste0(round(100 * head(quintiles, -1)), "%–", round(100 * tail(quintiles, -1)), "%")
    prop_quintile <- cut(prop_vals, breaks = quintiles, include.lowest = TRUE, labels = quintile_labels)
} else {
    prop_quintile <- factor(rep("Identical", length(prop_vals)))
}

# Build plotting df
boxplot_data <- list()
for (scenario in scenarios) {
    scen_incidence <- colSums(total.results.noMNNC[years, , "incidence", state, scenario])
    abs_excess <- scen_incidence - baseline_incidence
    boxplot_data[[length(boxplot_data) + 1]] <- data.frame(
        excess_infections = abs_excess,
        quintile = prop_quintile,
        scenario = scenario,
        state = state
    )
}
plot_df <- do.call(rbind, boxplot_data)

# Labeled factors for fill
plot_df$scenario <- factor(
    plot_df$scenario,
    levels = c("cdct.pintr.26", "cdct.end.26"),
    labels = c("Prolonged Interruption", "Cessation")
)

# Plot
ggplot(plot_df, aes(x = excess_infections, y = forcats::fct_rev(quintile), fill = scenario)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_manual(values = jama_colors_labeled, name = NULL) +
    labs(
        x = "Number of Excess Infections (2025–2030)",
        y = "Proportion Tested Regardless (Quintile)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        panel.border = element_rect(color = "black", fill = NA),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        plot.margin = margin(t = 10, r = 10, b = 40, l = 10)
    )

summary_df <- plot_df %>%
    group_by(scenario, quintile) %>%
    summarise(
        mean_excess = mean(excess_infections, na.rm = TRUE),
        iqr_lower = quantile(excess_infections, 0.25, na.rm = TRUE),
        iqr_upper = quantile(excess_infections, 0.75, na.rm = TRUE),
        .groups = 'drop'
    )


#Boxplot figure

# List of interventions to include
interventions <- c("cdct.end.26", "cdct.pintr.26")

# Create an empty list to hold data.frames
plot_data_list <- list()

# Loop through interventions and states to collect state-level results 
for (intervention in interventions) {
    for (state in states) {
        ri <- (colSums(total.results.noMNNC[as.character(2025:2030), , "incidence", state, intervention]) -
                   colSums(total.results.noMNNC[as.character(2025:2030), , "incidence", state, "noint"])) /
            colSums(total.results.noMNNC[as.character(2025:2030), , "incidence", state, "noint"])
        
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

# Clean state names
plot_data$state <- gsub("\\.*$", "", plot_data$state)
state_lookup <- setNames(state.name, state.abb)
plot_data$state <- ifelse(
    plot_data$state %in% names(state_lookup),
    state_lookup[plot_data$state],
    plot_data$state
)

# Sort states by mean relative incidence for cdct.end
sorted_states <- plot_data %>%
    filter(intervention == "cdct.end.26", state != "Total") %>%
    group_by(state) %>%
    summarise(mean_ri = mean(relative_incidence, na.rm = TRUE)) %>%
    arrange(mean_ri) %>%
    pull(state)

# Set factor levels
plot_data$state <- factor(plot_data$state, levels = c("Total", sorted_states))
plot_data$intervention <- factor(
    plot_data$intervention,
    levels = c( "cdct.pintr.26", "cdct.end.26"),
    labels = c("3-Year Interruption", "Cessation")
)

# Cap values at 40 for visualization
plot_data$relative_incidence_capped <- ifelse(
    plot_data$relative_incidence > 40,
    40,
    plot_data$relative_incidence
)


# ----- Plot -----
ggplot(plot_data, aes(x = relative_incidence_capped, y = state, fill = intervention)) +
    geom_boxplot(
        aes(group = interaction(state, intervention)),
        position = position_dodge2(width = 0.8, preserve = "single"),
        outlier.shape = NA
    ) +
    scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        limits = c(0, 47),
        expand = expansion(mult = c(0, 0.1))
    ) +
    scale_fill_manual(
        values = c(
            "Cessation" = "#374E55",
            "3-Year Interruption" = "#DF8F44"
        )
    ) +
    labs(
        title = "Figure 1. Projected Excess HIV Infections if CDC-funded HIV Testing is Disrupted",
        x = "Relative Excess HIV Infections until 2030",
        y = NULL,
        fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "bottom",
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 18)
    )

