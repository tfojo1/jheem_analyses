library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

##################################################################
##################################################################
##################################################################
##################################################################

# Future growth penalty

##################################################################
##################################################################
##################################################################
# Pull the two matrices
agg_mat <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location

rep_mat <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location

# All location codes = union of both
all_locs <- union(colnames(agg_mat), colnames(rep_mat))

# Helper to build the "best" series for ONE location
extract_series <- function(loc){
    agg    <- if (loc %in% colnames(agg_mat)) agg_mat[, loc] else NULL
    report <- if (loc %in% colnames(rep_mat)) rep_mat[, loc] else NULL
    
    df_agg <- if (!is.null(agg))    data.frame(year = as.numeric(names(agg)),    county_agg = as.numeric(agg))    else data.frame(year = numeric(0), county_agg = numeric(0))
    df_rep <- if (!is.null(report)) data.frame(year = as.numeric(names(report)), pdf_report = as.numeric(report)) else data.frame(year = numeric(0), pdf_report = numeric(0))
    
    merged <- full_join(df_agg, df_rep, by = "year") %>% arrange(year)
    merged <- merged %>%
        mutate(value = dplyr::coalesce(county_agg, pdf_report)) %>%
        select(year, value) %>%
        mutate(location = loc, .before = 1)
    merged
}

# Build long df for ALL locations, filter to 1993–2022
all_series_long <- map_dfr(all_locs, extract_series) %>%
    filter(year >= 2012, year <= 2022)

# Compute 5-year ratios within each location
changes_yr <- all_series_long %>%
    group_by(location) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(ratio_5yr = value / lag(value, 5), 
           ratio_3yr = value / lag(value, 3)) %>%
    ungroup()

# Pool ALL 5-year ratios across locations; plot ratio
pooled_5yr <- changes_yr %>%
    filter(is.finite(ratio_5yr), ratio_5yr > 0) %>%
    mutate(ratio = ratio_5yr)



# Fit lognormal parameters from the logs
logvals  <- log(pooled_5yr$ratio)
meanlog  <- mean(logvals, na.rm = TRUE)
sdlog    <- sd(logvals,  na.rm = TRUE)   

exp(meanlog + 2*sdlog) #  4.083686
exp(meanlog - 2*sdlog) # 0.608353


# Useful summaries
gm               <- exp(meanlog)                      # geometric mean (= median)
median          <- quantile(pooled_5yr$ratio, probs = 0.5)
mean_lognormal   <- exp(meanlog + 0.5*sdlog^2)        # E[X] under lognormal
pi95             <- exp(meanlog + c(-1.96, 1.96)*sdlog)

# Plot histogram on *linear* scale with lognormal density
ggplot(pooled_5yr, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    geom_density() +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 1000, linewidth = 1.2, color = "blue") +
    geom_vline(xintercept = gm,             linetype = "dashed") +     # geometric mean / median
    geom_vline(xintercept = mean_lognormal,  linetype = "dotdash") +    # lognormal mean E[X]
    labs(
        title    = "Pooled 5-year ratios (linear scale)",
        subtitle = sprintf("Lognormal fit: meanlog = %.3f, sdlog = %.3f | GM (dashed), E[X] (dotdash)", meanlog, sdlog),
        x = "5-year fold-change (ratio)",
        y = "Density"
    ) +
    theme_minimal()

# 95% interval in ratio scale
ci95 <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
q95 = quantile(pooled_5yr$ratio, probs = c(0.025, 0.975))
pi95   

ggplot(pooled_5yr, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 800, linewidth = 1.2, color = "blue") +
    # shaded 95% CI
    annotate("rect", xmin = ci95[1], xmax = ci95[2], ymin = 0, ymax = Inf,
             alpha = 0.1, fill = "blue") +
    geom_vline(xintercept = exp(meanlog),            linetype = "dashed") +
    geom_vline(xintercept = exp(meanlog + 0.5*sdlog^2), linetype = "dotdash") +
    labs(title = "Pooled 5-year ratios with lognormal fit",
         subtitle = sprintf("95%% CI = [%.2f, %.2f]", ci95[1], ci95[2]),
         x = "5-year fold-change", y = "Density")

df_ratio <- pooled_5yr %>% filter(is.finite(ratio), ratio > 0)

ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    labs(
        title = "QQ plot: ratios vs Lognormal(meanlog, sdlog)",
        subtitle = sprintf("meanlog = %.3f, sdlog = %.3f", meanlog, sdlog),
        x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)"
    ) +
    theme_minimal()


df_ratio <- pooled_5yr %>% filter(is.finite(ratio), ratio > 0)
n <- nrow(df_ratio)
probs <- ppoints(n)

# Theoretical 2.5% and 97.5% quantiles for each probability
theo_q <- qlnorm(probs, meanlog, sdlog)
theo_lo <- qlnorm(probs, meanlog, sdlog)  
theo_hi <- qlnorm(probs, meanlog, sdlog)  

# Simple QQ with band
ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm,
            dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm,
                 dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    annotate("rect", xmin = qlnorm(0.025, meanlog, sdlog),
             xmax = qlnorm(0.975, meanlog, sdlog),
             ymin =  ci95[1], ymax =  ci95[2], fill = "blue", alpha = 0.05) +
    labs(title = "QQ plot: ratios vs fitted lognormal",
         subtitle = sprintf("95%% interval [%.2f, %.2f]", 
                            qlnorm(0.025, meanlog, sdlog),
                            qlnorm(0.975, meanlog, sdlog)),
          x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)") +
    theme_minimal()


##########################


pooled_3yr <- changes_yr %>%
    filter(is.finite(ratio_3yr), ratio_3yr > 0) %>%
    mutate(ratio = ratio_3yr)

pooled_3yr_no_out <- pooled_3yr %>%
    mutate(lx = log(ratio)) %>%
    mutate(
        q1  = quantile(lx, 0.25),
        q3  = quantile(lx, 0.75),
        iqr = q3 - q1,
        lower = q1 - 1.5 * iqr,
        upper = q3 + 1.5 * iqr
    ) %>%
    filter(lx >= lower, lx <= upper) %>%
    select(-lx, -q1, -q3, -iqr, -lower, -upper) 

# Fit lognormal parameters from the logs
logvals  <- log(pooled_3yr_no_out$ratio)
meanlog  <- mean(logvals, na.rm = TRUE)
sdlog    <- sd(logvals,  na.rm = TRUE)   # (sample sd; fine for overlay)


# Useful summaries
gm               <- exp(meanlog)                      # geometric mean (= median)
median          <- quantile(pooled_3yr_no_out$ratio, probs = 0.5)
mean_lognormal   <- exp(meanlog + 0.5*sdlog^2)        # E[X] under lognormal
pi95             <- exp(meanlog + c(-1.96, 1.96)*sdlog)

# Plot histogram on *linear* scale with lognormal density
ggplot(pooled_3yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    geom_density() +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 1000, linewidth = 1.2, color = "blue") +
    geom_vline(xintercept = gm,             linetype = "dashed") +     # geometric mean / median
    geom_vline(xintercept = mean_lognormal,  linetype = "dotdash") +    # lognormal mean E[X]
    labs(
        title    = "Pooled 3-year ratios (linear scale)",
        subtitle = sprintf("Lognormal fit: meanlog = %.3f, sdlog = %.3f | GM (dashed), E[X] (dotdash)", meanlog, sdlog),
        x = "5-year fold-change (ratio)",
        y = "Density"
    ) +
    theme_minimal()

# 95% interval in ratio scale
ci95 <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
q95 = quantile(pooled_3yr_no_out$ratio, probs = c(0.025, 0.975))
pi95   

ggplot(pooled_3yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 800, linewidth = 1.2, color = "blue") +
    # shaded 95% CI
    annotate("rect", xmin = ci95[1], xmax = ci95[2], ymin = 0, ymax = Inf,
             alpha = 0.1, fill = "blue") +
    geom_vline(xintercept = exp(meanlog),            linetype = "dashed") +
    geom_vline(xintercept = exp(meanlog + 0.5*sdlog^2), linetype = "dotdash") +
    labs(title = "Pooled 3-year ratios with lognormal fit",
         subtitle = sprintf("95%% CI = [%.2f, %.2f]", ci95[1], ci95[2]),
         x = "5-year fold-change", y = "Density")

df_ratio <- pooled_3yr_no_out %>% filter(is.finite(ratio), ratio > 0 )

ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    labs(
        title = "QQ plot: ratios vs Lognormal(meanlog, sdlog)",
        subtitle = sprintf("meanlog = %.3f, sdlog = %.3f", meanlog, sdlog),
        x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)"
    ) +
    theme_minimal()


n <- nrow(df_ratio)
probs <- ppoints(n)

# Theoretical 2.5% and 97.5% quantiles for each probability
theo_q <- qlnorm(probs, meanlog, sdlog)
theo_lo <- qlnorm(probs, meanlog, sdlog)  
theo_hi <- qlnorm(probs, meanlog, sdlog)  

# Simple QQ with band
ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm,
            dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm,
                 dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    annotate("rect", xmin = qlnorm(0.025, meanlog, sdlog),
             xmax = qlnorm(0.975, meanlog, sdlog),
             ymin =  ci95[1], ymax =  ci95[2], fill = "blue", alpha = 0.05) +
    labs(title = "QQ plot: ratios vs fitted lognormal",
         subtitle = sprintf("95%% interval [%.2f, %.2f]", 
                            qlnorm(0.025, meanlog, sdlog),
                            qlnorm(0.975, meanlog, sdlog)),
         x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)") +
    theme_minimal()


changes_yr_filtered <- all_series_long %>%
    group_by(location) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
        v      = as.numeric(value),
        lag3   = lag(v, 3),
        lag5   = lag(v, 5),
        ratio_3yr = v / lag3,
        ratio_5yr = v / lag5
    ) %>%
    # keep rows only if current and lagged values are >=30 (https://www.cdc.gov/nchs/data/series/sr_02/sr02-198.pdf)
    dplyr::filter(v >= 30, lag3 >= 30, is.finite(ratio_3yr), is.finite(ratio_5yr)) %>%
    ungroup() %>%
    select(-v, -lag3, -lag5)

#

pooled_3yr <- changes_yr_filtered %>%
    filter(is.finite(ratio_3yr), ratio_3yr > 0) %>%
    mutate(ratio = ratio_3yr)

pooled_3yr_no_out <- pooled_3yr %>%
    mutate(lx = log(ratio)) %>%
    mutate(
        q1  = quantile(lx, 0.25),
        q3  = quantile(lx, 0.75),
        iqr = q3 - q1,
        lower = q1 - 1.5 * iqr,
        upper = q3 + 1.5 * iqr
    ) %>%
    filter(lx >= lower, lx <= upper) %>%
    select(-lx, -q1, -q3, -iqr, -lower, -upper) 

# Fit lognormal parameters from the logs
logvals  <- log(pooled_3yr_no_out$ratio)
meanlog  <- mean(logvals, na.rm = TRUE)
sdlog    <- sd(logvals,  na.rm = TRUE)   # (sample sd; fine for overlay)


# Useful summaries
gm               <- exp(meanlog)                      # geometric mean (= median)
median          <- quantile(pooled_3yr_no_out$ratio, probs = 0.5)
mean_lognormal   <- exp(meanlog + 0.5*sdlog^2)        # E[X] under lognormal
pi95             <- exp(meanlog + c(-1.96, 1.96)*sdlog)

# Plot histogram on *linear* scale with lognormal density
ggplot(pooled_3yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    geom_density() +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 1000, linewidth = 1.2, color = "blue") +
    geom_vline(xintercept = gm,             linetype = "dashed") +     # geometric mean / median
    geom_vline(xintercept = mean_lognormal,  linetype = "dotdash") +    # lognormal mean E[X]
    labs(
        title    = "Pooled 3-year ratios (linear scale)",
        subtitle = sprintf("Lognormal fit: meanlog = %.3f, sdlog = %.3f | GM (dashed), E[X] (dotdash)", meanlog, sdlog),
        x = "3-year fold-change (ratio)",
        y = "Density"
    ) +
    theme_minimal()

# 95% interval in ratio scale
ci95 <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
q95 = quantile(pooled_3yr_no_out$ratio, probs = c(0.025, 0.975))
pi95   

ggplot(pooled_3yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 800, linewidth = 1.2, color = "blue") +
    # shaded 95% CI
    annotate("rect", xmin = ci95[1], xmax = ci95[2], ymin = 0, ymax = Inf,
             alpha = 0.1, fill = "blue") +
    geom_vline(xintercept = exp(meanlog),            linetype = "dashed") +
    geom_vline(xintercept = exp(meanlog + 0.5*sdlog^2), linetype = "dotdash") +
    labs(title = "Pooled 3-year ratios with lognormal fit",
         subtitle = sprintf("95%% CI = [%.2f, %.2f]", ci95[1], ci95[2]),
         x = "3-year fold-change", y = "Density")

df_ratio <- pooled_3yr_no_out %>% filter(is.finite(ratio), ratio > 0 )

ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    labs(
        title = "QQ plot: ratios vs Lognormal(meanlog, sdlog)",
        subtitle = sprintf("meanlog = %.3f, sdlog = %.3f", meanlog, sdlog),
        x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)"
    ) +
    theme_minimal()


n <- nrow(df_ratio)
probs <- ppoints(n)

# Theoretical 2.5% and 97.5% quantiles for each probability
theo_q <- qlnorm(probs, meanlog, sdlog)
theo_lo <- qlnorm(probs, meanlog, sdlog)  
theo_hi <- qlnorm(probs, meanlog, sdlog)  

# Simple QQ with band
ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm,
            dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm,
                 dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    annotate("rect", xmin = qlnorm(0.025, meanlog, sdlog),
             xmax = qlnorm(0.975, meanlog, sdlog),
             ymin =  ci95[1], ymax =  ci95[2], fill = "blue", alpha = 0.05) +
    labs(title = "QQ plot: ratios vs fitted lognormal",
         subtitle = sprintf("95%% interval [%.2f, %.2f]", 
                            qlnorm(0.025, meanlog, sdlog),
                            qlnorm(0.975, meanlog, sdlog)),
         x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)") +
    theme_minimal()
######################################################

changes_yr_filtered <- all_series_long %>%
    group_by(location) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
        v      = as.numeric(value),
        lag3   = lag(v, 3),
        lag5   = lag(v, 5),
        ratio_3yr = v / lag3,
        ratio_5yr = v / lag5
    ) %>%
    # keep rows only if current and lagged values are >=30 (https://www.cdc.gov/nchs/data/series/sr_02/sr02-198.pdf)
    dplyr::filter(v >= 30, lag5 >= 30, is.finite(ratio_3yr), is.finite(ratio_5yr)) %>%
    ungroup() %>%
    select(-v, -lag3, -lag5)


pooled_5yr <- changes_yr_filtered %>%
    filter(is.finite(ratio_5yr), ratio_5yr > 0) %>%
    mutate(ratio = ratio_5yr)

pooled_5yr_no_out <- pooled_5yr %>%
    mutate(lx = log(ratio)) %>%
    mutate(
        q1  = quantile(lx, 0.25),
        q3  = quantile(lx, 0.75),
        iqr = q3 - q1,
        lower = q1 - 1.5 * iqr,
        upper = q3 + 1.5 * iqr
    ) %>%
    filter(lx >= lower, lx <= upper) %>%
    select(-lx, -q1, -q3, -iqr, -lower, -upper) 




# Fit lognormal parameters from the logs
logvals  <- log(pooled_5yr_no_out$ratio)
meanlog  <- mean(logvals, na.rm = TRUE)
sdlog    <- sd(logvals,  na.rm = TRUE)   # (sample sd; fine for overlay)


# Useful summaries
gm               <- exp(meanlog)                      # geometric mean (= median)
median          <- quantile(pooled_5yr_no_out$ratio, probs = 0.5)
mean_lognormal   <- exp(meanlog + 0.5*sdlog^2)        # E[X] under lognormal
pi95             <- exp(meanlog + c(-1.96, 1.96)*sdlog)

# Plot histogram on *linear* scale with lognormal density
ggplot(pooled_5yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    geom_density() +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 1000, linewidth = 1.2, color = "blue") +
    geom_vline(xintercept = gm,             linetype = "dashed") +     # geometric mean / median
    geom_vline(xintercept = mean_lognormal,  linetype = "dotdash") +    # lognormal mean E[X]
    labs(
        title    = "Pooled 3-year ratios (linear scale)",
        subtitle = sprintf("Lognormal fit: meanlog = %.3f, sdlog = %.3f | GM (dashed), E[X] (dotdash)", meanlog, sdlog),
        x = "5-year fold-change (ratio)",
        y = "Density"
    ) +
    theme_minimal()

# 95% interval in ratio scale
ci95 <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
q95 = quantile(pooled_5yr_no_out$ratio, probs = c(0.025, 0.975))
pi95   

ggplot(pooled_5yr_no_out, aes(x = ratio)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                  n = 800, linewidth = 1.2, color = "blue") +
    # shaded 95% CI
    annotate("rect", xmin = ci95[1], xmax = ci95[2], ymin = 0, ymax = Inf,
             alpha = 0.1, fill = "blue") +
    geom_vline(xintercept = exp(meanlog),            linetype = "dashed") +
    geom_vline(xintercept = exp(meanlog + 0.5*sdlog^2), linetype = "dotdash") +
    labs(title = "Pooled 5-year ratios with lognormal fit",
         subtitle = sprintf("95%% CI = [%.2f, %.2f]", ci95[1], ci95[2]),
         x = "5-year fold-change", y = "Density")

df_ratio <- pooled_5yr_no_out %>% filter(is.finite(ratio), ratio > 0 )

ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm, dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    labs(
        title = "QQ plot: ratios vs Lognormal(meanlog, sdlog)",
        subtitle = sprintf("meanlog = %.3f, sdlog = %.3f", meanlog, sdlog),
        x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)"
    ) +
    theme_minimal()


n <- nrow(df_ratio)
probs <- ppoints(n)

# Theoretical 2.5% and 97.5% quantiles for each probability
theo_q <- qlnorm(probs, meanlog, sdlog)
theo_lo <- qlnorm(probs, meanlog, sdlog)  
theo_hi <- qlnorm(probs, meanlog, sdlog)  

# Simple QQ with band
ggplot(df_ratio, aes(sample = ratio)) +
    stat_qq(distribution = qlnorm,
            dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    stat_qq_line(distribution = qlnorm,
                 dparams = list(meanlog = meanlog, sdlog = sdlog)) +
    annotate("rect", xmin = qlnorm(0.025, meanlog, sdlog),
             xmax = qlnorm(0.975, meanlog, sdlog),
             ymin =  ci95[1], ymax =  ci95[2], fill = "blue", alpha = 0.05) +
    labs(title = "QQ plot: ratios vs fitted lognormal",
         subtitle = sprintf("95%% interval [%.2f, %.2f]", 
                            qlnorm(0.025, meanlog, sdlog),
                            qlnorm(0.975, meanlog, sdlog)),
         x = "Theoretical quantiles (Lognormal)", y = "Sample quantiles (ratios)") +
    theme_minimal()

##################################################################
##################################################################
##################################################################
##################################################################

# U-turn penalty

##################################################################
##################################################################
##################################################################

# Pull the two matrices
agg_mat <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location

rep_mat <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location

# All location codes = union of both
all_locs <- union(colnames(agg_mat), colnames(rep_mat))

# Helper to build the "best" series for ONE location
extract_series <- function(loc){
    agg    <- if (loc %in% colnames(agg_mat)) agg_mat[, loc] else NULL
    report <- if (loc %in% colnames(rep_mat)) rep_mat[, loc] else NULL
    
    df_agg <- if (!is.null(agg))    data.frame(year = as.numeric(names(agg)),    county_agg = as.numeric(agg))    else data.frame(year = numeric(0), county_agg = numeric(0))
    df_rep <- if (!is.null(report)) data.frame(year = as.numeric(names(report)), pdf_report = as.numeric(report)) else data.frame(year = numeric(0), pdf_report = numeric(0))
    
    merged <- full_join(df_agg, df_rep, by = "year") %>% arrange(year)
    merged <- merged %>%
        mutate(value = dplyr::coalesce(county_agg, pdf_report)) %>%
        select(year, value) %>%
        mutate(location = loc, .before = 1)
    merged
}

# Build long df for ALL locations, filter to 1993–2022
all_series_long <- map_dfr(all_locs, extract_series) %>%
    filter(year >= 2002, year <= 2022)

# Double-delta = (v_{t+h}/v_t) / (v_t/v_{t-h}) = (v_{t+h} * v_{t-h}) / v_t^2
min_count <- 30

dd_base <- all_series_long %>%
    group_by(location) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
        v     = as.numeric(value),
        lag3  = lag(v, 3),  lead3 = lead(v, 3),
        lag5  = lag(v, 5),  lead5 = lead(v, 5),
        
        # forward/backward deltas
        fwd_3  = lead3 / v,  back_3 = v / lag3,
        fwd_5  = lead5 / v,  back_5 = v / lag5,
        
        # raw double-deltas
        dd3_raw = fwd_3 / back_3,
        dd5_raw = fwd_5 / back_5
    ) %>%
    ungroup() %>%
    # keep dd only when denominators are big enough
    mutate(
        dd3 = if_else(v >= min_count & lag3 >= min_count & is.finite(dd3_raw), dd3_raw, NA_real_),
        dd5 = if_else(v >= min_count & lag5 >= min_count & is.finite(dd5_raw), dd5_raw, NA_real_)
    ) %>%
    select(-dd3_raw, -dd5_raw) 

# drop rows where both horizons are NA
dd_base <- dd_base %>% filter(!is.na(dd3) | !is.na(dd5))

# Apply ≥30 rule separately for each horizon and keep valid, positive values
pooled_dd_3 <- dd_base %>%
    filter(v >= 30, lag3 >= 30, lead3 >= 30, is.finite(dd3), dd3 > 0) %>%
    transmute(location, year, horizon = "3-year", double_delta = dd3)

pooled_dd_5 <- dd_base %>%
    filter(v >= 30, lag5 >= 30, lead5 >= 30, is.finite(dd5), dd5 > 0) %>%
    transmute(location, year, horizon = "5-year", double_delta = dd5)

pooled_dd <- bind_rows(pooled_dd_3, pooled_dd_5)


# --- Tukey outlier removal (log-scale), per horizon ---
k <- 1.5  

fences <- pooled_dd %>%
    filter(is.finite(double_delta), double_delta > 0) %>%
    group_by(horizon) %>%
    summarize(
        n  = dplyr::n(),
        q1 = quantile(log(double_delta), 0.25, na.rm = TRUE),
        q3 = quantile(log(double_delta), 0.75, na.rm = TRUE),
        iqr = q3 - q1,
        .groups = "drop"
    ) %>%
    mutate(
        # If a group is tiny or IQR=0, skip filtering for that group
        lower = if_else(n >= 5 & iqr > 0, exp(q1 - k*iqr), 0),
        upper = if_else(n >= 5 & iqr > 0, exp(q3 + k*iqr), Inf)
    )

# Keep only non-outliers; overwrite pooled_dd so the rest of your code just works
pooled_dd <- pooled_dd %>%
    inner_join(select(fences, horizon, lower, upper), by = "horizon") %>%
    filter(double_delta >= lower, double_delta <= upper) %>%
    select(-lower, -upper)

# geometric mean per horizon to mark on the plot
gm_by_horizon <- pooled_dd %>%
    group_by(horizon) %>%
    summarize(gm = exp(mean(log(double_delta))), .groups = "drop")


# Plot: histogram + density; log x-scale; dashed line at 1 (no curvature change)
ggplot(pooled_dd, aes(x = double_delta)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
    geom_density() +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_vline(data = gm_by_horizon, aes(xintercept = gm), linetype = "dotdash") +
    scale_x_log10() +
    facet_wrap(~ horizon, scales = "free_y") +
    labs(
        title = "Distribution of double-delta across locations",
        subtitle = "Double-delta = (v[t+h]/v[t]) / (v[t]/v[t-h]); dot-dash = geometric mean; dashed = 1",
        x = "Double-delta (log10 scale)",
        y = "Density"
    ) +
    theme_minimal()

plot_lognorm_dd <- function(df, title_prefix){
    df <- df %>% filter(is.finite(double_delta), double_delta > 0)
    logvals <- log(df$double_delta)
    meanlog <- mean(logvals, na.rm = TRUE)
    sdlog   <- sd(logvals,  na.rm = TRUE)
    ci95    <- qlnorm(c(0.025, 0.975), meanlog, sdlog)
    
    ggplot(df, aes(x = double_delta)) +
        geom_histogram(aes(y = after_stat(density)), bins = 40, alpha = 0.5) +
        stat_function(fun = dlnorm, args = list(meanlog = meanlog, sdlog = sdlog),
                      n = 800, linewidth = 1.2, color = "blue") +
        annotate("rect", xmin = ci95[1], xmax = ci95[2], ymin = 0, ymax = Inf,
                 alpha = 0.1, fill = "blue") +
        geom_vline(xintercept = 1, linetype = "dashed") + # reversal
        geom_vline(xintercept = exp(meanlog + 0.5*sdlog^2),   linetype = "dotdash") +    # E[X]
        labs(
            title = paste0(title_prefix, " double-delta with lognormal fit"),
            subtitle = sprintf("meanlog = %.3f, sdlog = %.3f | 95%% CI = [%.2f, %.2f]",
                               meanlog, sdlog, ci95[1], ci95[2]),
            x = "Double-delta (raw scale)", y = "Density"
        ) +
        theme_minimal()
}

# 3-year double-delta
p3 <- pooled_dd %>% filter(horizon == "3-year") %>% plot_lognorm_dd("3-year")
# 5-year double-delta
p5 <- pooled_dd %>% filter(horizon == "5-year") %>% plot_lognorm_dd("5-year")

p3
p5

