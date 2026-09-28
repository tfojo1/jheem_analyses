library(dplyr)
library(tidyr)
library(purrr)

# =============================================================================
# FORMATTING HELPERS
# =============================================================================

fmt_n <- function(x) {
    format(
        round(x),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
    )
}

fmt_n_ci <- function(med, lo, hi) {
    paste0(
        fmt_n(med),
        " [",
        fmt_n(lo),
        "–",
        fmt_n(hi),
        "]"
    )
}

fmt_dollar <- function(x) {
    
    abs_x <- abs(x)
    
    val <- ifelse(
        abs_x >= 1e9,
        sprintf("$%.2fB", abs_x / 1e9),
        sprintf("$%.1fM", abs_x / 1e6)
    )
    
    ifelse(
        x < 0,
        paste0("−", val),
        val
    )
}

fmt_dollar_ci <- function(med, lo, hi) {
    paste0(
        fmt_dollar(med),
        " [",
        fmt_dollar(lo),
        "–",
        fmt_dollar(hi),
        "]"
    )
}

fmt_ratio <- function(x) {
    
    out <- sprintf("%.3f", abs(x))
    
    ifelse(
        x < 0,
        paste0("−", out),
        out
    )
}

fmt_ratio_ci <- function(med, lo, hi) {
    paste0(
        fmt_ratio(med),
        " [",
        fmt_ratio(lo),
        "–",
        fmt_ratio(hi),
        "]"
    )
}


# =============================================================================
# BUILD TABLE
# =============================================================================

build_adap_table <- function(
        df,
        new_excess,
        start_paths,
        compare_with_rw,
        final_year = 2035,
        suppression_loss_year = 2026
) {
    
    # =========================================================================
    # STATE LIST
    # =========================================================================
    
    state_locs <- compare_with_rw %>%
        filter(
            year == final_year,
            !location %in% c("Total", "total")
        ) %>%
        distinct(location) %>%
        pull(location) %>%
        sort()
    
    
    # =========================================================================
    # 1. PWH ON ADAP IN 2025
    # =========================================================================
    
    adap_clients <- df %>%
        filter(
            year == 2025,
            intervention == "noint",
            outcome == "adap.clients",
            location %in% c(state_locs, "Total")
        ) %>%
        group_by(location) %>%
        summarise(
            med = median(value, na.rm = TRUE),
            lo  = quantile(value, 0.025, na.rm = TRUE),
            hi  = quantile(value, 0.975, na.rm = TRUE),
            .groups = "drop"
        )
    
    
    # =========================================================================
    # 2. ADAP RECIPIENTS LOSING VIRAL SUPPRESSION
    #
    # IMPLICITLY MEASURED FROM JHEEM OUTPUT:
    #
    # no intervention suppression
    #       -
    # ADAP elimination suppression
    #
    # This works for states AND built-in Total.
    # =========================================================================
    
    suppression_loss_draws <- df %>%
        filter(
            year == suppression_loss_year,
            outcome == "suppression",
            intervention %in% c(
                "noint",
                "adap.100.end.26"
            ),
            location %in% c(state_locs, "Total")
        ) %>%
        select(
            location,
            sim,
            intervention,
            value
        ) %>%
        pivot_wider(
            names_from = intervention,
            values_from = value
        ) %>%
        mutate(
            lost_suppression =
                noint - `adap.100.end.26`
        )
    
    
    suppression_loss <- suppression_loss_draws %>%
        group_by(location) %>%
        summarise(
            med = median(lost_suppression, na.rm = TRUE),
            lo  = quantile(lost_suppression, 0.025, na.rm = TRUE),
            hi  = quantile(lost_suppression, 0.975, na.rm = TRUE),
            .groups = "drop"
        )
    
    
    # =========================================================================
    # 3. EXCESS INCIDENT HIV + EXCESS NEWLY DIAGNOSED HIV CASES
    # =========================================================================
    
    cumulative_counts <- new_excess %>%
        filter(
            year >= 2026,
            year <= final_year,
            location %in% c(state_locs, "Total")
        ) %>%
        group_by(
            location,
            sim
        ) %>%
        summarise(
            excess_incident =
                sum(excess_incidence, na.rm = TRUE),
            
            excess_diagnosed =
                sum(excess_new, na.rm = TRUE),
            
            .groups = "drop"
        )
    
    
    count_summary <- cumulative_counts %>%
        group_by(location) %>%
        summarise(
            
            incident_med =
                median(excess_incident, na.rm = TRUE),
            
            incident_lo =
                quantile(excess_incident, 0.025, na.rm = TRUE),
            
            incident_hi =
                quantile(excess_incident, 0.975, na.rm = TRUE),
            
            diagnosed_med =
                median(excess_diagnosed, na.rm = TRUE),
            
            diagnosed_lo =
                quantile(excess_diagnosed, 0.025, na.rm = TRUE),
            
            diagnosed_hi =
                quantile(excess_diagnosed, 0.975, na.rm = TRUE),
            
            .groups = "drop"
        )
    
    
    # =========================================================================
    # 4. PERSON-YEARS IN HIV CARE / ON ART
    #
    # active_on_art is cumulative starts.
    # Person-years = sum active ART population over years.
    # =========================================================================
    
    py_draws <- start_paths %>%
        filter(
            year >= 2026,
            year <= final_year,
            location %in% c(state_locs, "Total")
        ) %>%
        group_by(
            location,
            sim
        ) %>%
        arrange(
            year,
            .by_group = TRUE
        ) %>%
        mutate(
            active_on_art =
                cumsum(total_starts)
        ) %>%
        summarise(
            person_years =
                sum(active_on_art, na.rm = TRUE),
            .groups = "drop"
        )
    
    
    py_summary <- py_draws %>%
        group_by(location) %>%
        summarise(
            med = median(person_years, na.rm = TRUE),
            lo  = quantile(person_years, 0.025, na.rm = TRUE),
            hi  = quantile(person_years, 0.975, na.rm = TRUE),
            .groups = "drop"
        )
    
    
    # =========================================================================
    # 5. STATE COSTS
    #
    # Pool all cost scenarios + simulations.
    # =========================================================================
    
    state_cost_rows <- purrr::map_dfr(
        state_locs,
        function(loc) {
            
            x <- compare_with_rw %>%
                filter(
                    location == loc,
                    year == final_year
                )
            
            cost_draws <-
                x$cumulative_incremental_cost
            
            adap_spending <-
                first(
                    x$cumulative_drug_only
                )
            
            net_draws <-
                cost_draws -
                adap_spending
            
            ratio_draws <-
                net_draws /
                adap_spending
            
            
            tibble(
                location = loc,
                
                care_med =
                    median(cost_draws, na.rm = TRUE),
                
                care_lo =
                    quantile(cost_draws, 0.025, na.rm = TRUE),
                
                care_hi =
                    quantile(cost_draws, 0.975, na.rm = TRUE),
                
                adap_spending =
                    adap_spending,
                
                net_med =
                    median(net_draws, na.rm = TRUE),
                
                net_lo =
                    quantile(net_draws, 0.025, na.rm = TRUE),
                
                net_hi =
                    quantile(net_draws, 0.975, na.rm = TRUE),
                
                ncer_med =
                    median(ratio_draws, na.rm = TRUE),
                
                ncer_lo =
                    quantile(ratio_draws, 0.025, na.rm = TRUE),
                
                ncer_hi =
                    quantile(ratio_draws, 0.975, na.rm = TRUE)
            )
        }
    )
    
    
    # =========================================================================
    # 6. BUILT-IN JHEEM TOTAL COST
    # =========================================================================
    
    total_cost_data <- compare_with_rw %>%
        filter(
            location == "Total",
            year == final_year
        )
    
    
    total_cost_draws <-
        total_cost_data$cumulative_incremental_cost
    
    
    # -------------------------------------------------------------------------
    # ADAP spending itself comes from state funding data.
    # Sum one cumulative value per state.
    # -------------------------------------------------------------------------
    
    total_adap_spending <- compare_with_rw %>%
        filter(
            location %in% state_locs,
            year == final_year
        ) %>%
        group_by(location) %>%
        summarise(
            adap =
                first(cumulative_drug_only),
            .groups = "drop"
        ) %>%
        summarise(
            total =
                sum(adap, na.rm = TRUE)
        ) %>%
        pull(total)
    
    
    total_net_draws <-
        total_cost_draws -
        total_adap_spending
    
    
    total_ratio_draws <-
        total_net_draws /
        total_adap_spending
    
    
    total_cost_row <- tibble(
        location = "Total",
        
        care_med =
            median(total_cost_draws, na.rm = TRUE),
        
        care_lo =
            quantile(total_cost_draws, 0.025, na.rm = TRUE),
        
        care_hi =
            quantile(total_cost_draws, 0.975, na.rm = TRUE),
        
        adap_spending =
            total_adap_spending,
        
        net_med =
            median(total_net_draws, na.rm = TRUE),
        
        net_lo =
            quantile(total_net_draws, 0.025, na.rm = TRUE),
        
        net_hi =
            quantile(total_net_draws, 0.975, na.rm = TRUE),
        
        ncer_med =
            median(total_ratio_draws, na.rm = TRUE),
        
        ncer_lo =
            quantile(total_ratio_draws, 0.025, na.rm = TRUE),
        
        ncer_hi =
            quantile(total_ratio_draws, 0.975, na.rm = TRUE)
    )
    
    
    cost_summary <- bind_rows(
        state_cost_rows,
        total_cost_row
    )
    
    
    # =========================================================================
    # 7. JOIN EVERYTHING
    # =========================================================================
    
    all_locations <- tibble(
        location = c(
            state_locs,
            "Total"
        )
    )
    
    
    result <- all_locations %>%
        
        left_join(
            adap_clients,
            by = "location"
        ) %>%
        
        rename(
            adap_med = med,
            adap_lo = lo,
            adap_hi = hi
        ) %>%
        
        left_join(
            suppression_loss,
            by = "location"
        ) %>%
        
        rename(
            loss_med = med,
            loss_lo = lo,
            loss_hi = hi
        ) %>%
        
        left_join(
            count_summary,
            by = "location"
        ) %>%
        
        left_join(
            py_summary,
            by = "location"
        ) %>%
        
        rename(
            py_med = med,
            py_lo = lo,
            py_hi = hi
        ) %>%
        
        left_join(
            cost_summary,
            by = "location"
        )
    
    
    # =========================================================================
    # 8. FORMAT FINAL TABLE
    # =========================================================================
    
    final_table <- result %>%
        mutate(
            
            State =
                if_else(
                    location == "Total",
                    "Total (US)",
                    location
                ),
            
            pwh_adap =
                fmt_n_ci(
                    adap_med,
                    adap_lo,
                    adap_hi
                ),
            
            lost_suppression =
                fmt_n_ci(
                    loss_med,
                    loss_lo,
                    loss_hi
                ),
            
            incident =
                fmt_n_ci(
                    incident_med,
                    incident_lo,
                    incident_hi
                ),
            
            diagnosed =
                fmt_n_ci(
                    diagnosed_med,
                    diagnosed_lo,
                    diagnosed_hi
                ),
            
            person_years =
                fmt_n_ci(
                    py_med,
                    py_lo,
                    py_hi
                ),
            
            care_cost =
                fmt_dollar_ci(
                    care_med,
                    care_lo,
                    care_hi
                ),
            
            adap_cost =
                fmt_dollar(
                    adap_spending
                ),
            
            net_cost =
                fmt_dollar_ci(
                    net_med,
                    net_lo,
                    net_hi
                ),
            
            ncer =
                fmt_ratio_ci(
                    ncer_med,
                    ncer_lo,
                    ncer_hi
                )
        ) %>%
        
        select(
            State,
            pwh_adap,
            lost_suppression,
            incident,
            diagnosed,
            person_years,
            care_cost,
            adap_cost,
            net_cost,
            ncer
        ) %>%
        
        rename(
            `PWH on ADAP in 2025` =
                pwh_adap,
            
            `ADAP recipients losing viral suppression` =
                lost_suppression,
            
            `Excess Incident HIV` =
                incident,
            
            `Excess Newly Diagnosed HIV Cases` =
                diagnosed,
            
            `Person-Years in HIV Care and on ART among Excess HIV Cases` =
                person_years,
            
            `Cum. HIV Care Cost (post ADAP elimination)` =
                care_cost,
            
            `Cum. ADAP Spending` =
                adap_cost,
            
            `Net Cost (HIV Care Cost - ADAP Spending)` =
                net_cost,
            
            `NCER` =
                ncer
        )
    
    
    return(final_table)
}


# =============================================================================
# RUN
# =============================================================================

table_s6 <- build_adap_table(
    df = df,
    new_excess = new_excess,
    start_paths = start_paths,
    compare_with_rw = compare_with_rw,
    final_year = 2035,
    suppression_loss_year = 2026
)


# View result
print(
    table_s6,
    n = Inf,
    width = Inf
)

View(table_s6)


write.table(
    table_s6,
    file = pipe("pbcopy"),
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
)



# =============================================================================
# SECOND TABLE:
# 2025 pre-ADAP elimination vs 2035 no elimination vs 2035 ADAP elimination
# Assumes df and formatting helpers already exist
# =============================================================================

make_suppression_block <- function(df, year_select, intervention_select) {
    
    df %>%
        filter(
            year == year_select,
            intervention == intervention_select,
            outcome %in% c(
                "diagnosed.prevalence",
                "suppression"
            )
        ) %>%
        select(
            location,
            sim,
            outcome,
            value
        ) %>%
        pivot_wider(
            names_from = outcome,
            values_from = value
        ) %>%
        mutate(
            prevalence = diagnosed.prevalence,
            prop_suppressed = suppression / prevalence,
            not_suppressed = prevalence - suppression
        ) %>%
        group_by(location) %>%
        summarise(
            prevalence_med = median(prevalence, na.rm = TRUE),
            prevalence_lo  = quantile(prevalence, 0.025, na.rm = TRUE),
            prevalence_hi  = quantile(prevalence, 0.975, na.rm = TRUE),
            
            prop_supp_med = median(prop_suppressed, na.rm = TRUE),
            prop_supp_lo  = quantile(prop_suppressed, 0.025, na.rm = TRUE),
            prop_supp_hi  = quantile(prop_suppressed, 0.975, na.rm = TRUE),
            
            nonsupp_med = median(not_suppressed, na.rm = TRUE),
            nonsupp_lo  = quantile(not_suppressed, 0.025, na.rm = TRUE),
            nonsupp_hi  = quantile(not_suppressed, 0.975, na.rm = TRUE),
            
            .groups = "drop"
        )
}


# =============================================================================
# BUILD THREE BLOCKS
# =============================================================================

b2025 <- make_suppression_block(
    df = df,
    year_select = 2025,
    intervention_select = "noint"
) %>%
    rename_with(
        ~ paste0("y2025_", .x),
        -location
    )


b2035_noint <- make_suppression_block(
    df = df,
    year_select = 2035,
    intervention_select = "noint"
) %>%
    rename_with(
        ~ paste0("y2035_noint_", .x),
        -location
    )


b2035_adap <- make_suppression_block(
    df = df,
    year_select = 2035,
    intervention_select = "adap.100.end.26"
) %>%
    rename_with(
        ~ paste0("y2035_adap_", .x),
        -location
    )


# =============================================================================
# JOIN
# =============================================================================

suppression_table <- b2025 %>%
    full_join(
        b2035_noint,
        by = "location"
    ) %>%
    full_join(
        b2035_adap,
        by = "location"
    ) %>%
    mutate(
        State = if_else(
            location == "Total",
            "Total (US)",
            location
        )
    )


# =============================================================================
# FORMAT
# =============================================================================

suppression_table <- suppression_table %>%
    transmute(
        
        State,
        
        `2025 HIV Prevalence` =
            fmt_n_ci(
                y2025_prevalence_med,
                y2025_prevalence_lo,
                y2025_prevalence_hi
            ),
        
        `2025 Prop of PWH Virally Suppressed` =
            paste0(
                sprintf("%.1f%%", 100 * y2025_prop_supp_med),
                " [",
                sprintf("%.1f%%", 100 * y2025_prop_supp_lo),
                "–",
                sprintf("%.1f%%", 100 * y2025_prop_supp_hi),
                "]"
            ),
        
        `2025 PWH living without Viral Suppression` =
            fmt_n_ci(
                y2025_nonsupp_med,
                y2025_nonsupp_lo,
                y2025_nonsupp_hi
            ),
        
        
        `2035 No ADAP Elimination HIV Prevalence` =
            fmt_n_ci(
                y2035_noint_prevalence_med,
                y2035_noint_prevalence_lo,
                y2035_noint_prevalence_hi
            ),
        
        `2035 No ADAP Elimination Prop of PWH Virally Suppressed` =
            paste0(
                sprintf("%.1f%%", 100 * y2035_noint_prop_supp_med),
                " [",
                sprintf("%.1f%%", 100 * y2035_noint_prop_supp_lo),
                "–",
                sprintf("%.1f%%", 100 * y2035_noint_prop_supp_hi),
                "]"
            ),
        
        `2035 No ADAP Elimination PWH living without Viral Suppression` =
            fmt_n_ci(
                y2035_noint_nonsupp_med,
                y2035_noint_nonsupp_lo,
                y2035_noint_nonsupp_hi
            ),
        
        
        `2035 ADAP Elimination HIV Prevalence` =
            fmt_n_ci(
                y2035_adap_prevalence_med,
                y2035_adap_prevalence_lo,
                y2035_adap_prevalence_hi
            ),
        
        `2035 ADAP Elimination Prop of PWH Virally Suppressed` =
            paste0(
                sprintf("%.1f%%", 100 * y2035_adap_prop_supp_med),
                " [",
                sprintf("%.1f%%", 100 * y2035_adap_prop_supp_lo),
                "–",
                sprintf("%.1f%%", 100 * y2035_adap_prop_supp_hi),
                "]"
            ),
        
        `2035 ADAP Elimination PWH living without Viral Suppression` =
            fmt_n_ci(
                y2035_adap_nonsupp_med,
                y2035_adap_nonsupp_lo,
                y2035_adap_nonsupp_hi
            )
    ) %>%
    mutate(
        total_sort = if_else(
            State == "Total (US)",
            1L,
            0L
        )
    ) %>%
    arrange(
        total_sort,
        State
    ) %>%
    select(-total_sort)


# =============================================================================
# VIEW
# =============================================================================

print(
    suppression_table,
    n = Inf,
    width = Inf
)

View(suppression_table)


# =============================================================================
# COPY VALUES ONLY TO CLIPBOARD ON MAC
# =============================================================================

write.table(
    suppression_table,
    file = pipe("pbcopy"),
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
)
