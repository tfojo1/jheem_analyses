# Table 1 all in one!
# This table has columns:
# Total prevalence, prop 55+, prop 65+, and median age.

library(xlsx)
library(tidyverse)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")
source("../jheem_analyses/presentation/make_pretty_table.R")

# Source necessary objects ----
load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")
sixty_five_plus_estimates <- apply(sixty_five_plus_estimates, c("", "year", "sim", "location"), function(x) {x})

load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

do_conversion_operations <- function(df, is.percentage=F) {
    rv <-  df %>%
        mutate(stats=paste(mean, lower, upper)) %>%
        select(year, location, stats) %>%
        pivot_wider(names_from=year, values_from = stats) %>%
        separate_wider_delim(`2025`, delim=" ", names=c("mean25", "lo25", "up25")) %>%
        separate_wider_delim(`2040`, delim=" ", names=c("mean40", "lo40", "up40"))
    if (is.percentage) {
        rv <- rv %>%
            mutate(up25 = paste0(up25, "%")) %>%
            mutate(up40 = paste0(up40, "%")) %>%
            mutate(mean25 = paste0(mean25, "%")) %>%
            mutate(mean40 = paste0(mean40, "%"))
    }
    rv <- rv[match(state_order, rv$location),] %>%
        mutate(int25 = paste0("[", lo25, " to ", up25, "]")) %>%
        mutate(int40 = paste0("[", lo40, " to ", up40, "]")) %>%
        select(mean25, int25, mean40, int40) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}
do_delta_conversion_operations <- function(df, is.percentage=F) {
    rv <- df
    if (is.percentage) rv <- rv %>% mutate(upper = paste0(upper, "%"), mean = paste0(mean, "%"))
    rv <- rv[match(state_order, rv$location),] %>%
        mutate(interval = paste0("[", lower, " to ", upper, "]")) %>%
        select(mean, interval) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}

do_total_conversion_operations <- function(df) {
    # Assume we're already in the correct order in this case
    rv <- df %>%
        mutate(ci = paste0("[", lower, " to ", upper, "]")) %>%
        select(mean, ci) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}

# Total prev ----
total_prev_by_loc <- get_stats(total_results["2025",,"diagnosed.prevalence",,], c("location"))
total_prev_by_loc <- total_prev_by_loc[,names(sort(total_prev_by_loc["mean",], decreasing=T))]
# Must keep year in to avoid annoying issues
total_total_prev <- get_stats(apply(total_results["2025",,"diagnosed.prevalence",,,drop=F], c("year", "sim"), sum))
total_prev_df <- reshape2::melt(total_prev_by_loc) %>%
    rbind(reshape2::melt(total_total_prev) %>%
              mutate(location="Total") %>%
              select(-year)) %>%
    pivot_wider(names_from="metric")
total_prev_column <- do_total_conversion_operations(total_prev_df)

# Prop 55+ ----

num_totals <- apply(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
num_raw_arr <- array(c(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                       num_totals),
                     dim=c(year=2, age=5, sim=1000, location=length(state_order_names)),
                     dimnames=list(year=c("2025", "2040"), age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))

prop_data <- get_prop_over_55(num_raw_arr,
                              keep.dimensions=c("year", "location"))
prop_delta <- prop_data["2040",,] - prop_data["2025",,]

table_prop_over_55 <- cbind(do_conversion_operations(reshape2::melt(get_stats(prop_data,
                                                                              keep.dimensions=c("year", "location"))) %>%
                                                         pivot_wider(names_from="metric"), is.percentage = T),
                            do_delta_conversion_operations(reshape2::melt(get_stats(prop_delta,
                                                                                    keep.dimensions="location")) %>%
                                                               pivot_wider(names_from="metric"), is.percentage=T))

# Prop 65+ ----

over_65_totals <- apply(sixty_five_plus_estimates["65_plus",,,], c("year", "sim"), sum)
under_65_totals <- apply(sixty_five_plus_estimates["under_65",,,], c("year", "sim"), sum)

prop_65_totals <- over_65_totals / (over_65_totals + under_65_totals)
prop_65_raw_arr <- array(c(sixty_five_plus_estimates["65_plus",,,] / (sixty_five_plus_estimates["65_plus",,,] + sixty_five_plus_estimates["under_65",,,]),
                           prop_65_totals),
                         dim=c(year=2, sim=1000, location=length(state_order_names)),
                         dimnames=list(year=c("2025", "2040"), sim=1:1000, location=c(my_states, "total")))
prop_65_delta <- prop_65_raw_arr["2040",,]-prop_65_raw_arr["2025",,]

table_prop_over_65 <- cbind(do_conversion_operations(reshape2::melt(get_stats(prop_65_raw_arr,
                                                                              keep.dimensions=c("year", "location"),
                                                                              digits=2,
                                                                              multiply.by.100 = T)) %>%
                                                         pivot_wider(names_from="metric") %>%
                                                         mutate(stateName=get.location.name(location)),
                                                     is.percentage = T),
                            do_delta_conversion_operations(reshape2::melt(get_stats(prop_65_delta,
                                                                                    keep.dimensions="location",
                                                                                    digits=2,
                                                                                    multiply.by.100 = T)) %>%
                                                               pivot_wider(names_from="metric") %>%
                                                               mutate(stateName=get.location.name(location)),
                                                           is.percentage = T))

# Median Age ----
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_data.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")
# But we want to overwrite the statistical mean delta with the difference in the means so that the numbers on the
# table look coherent. Otherwise, the rounding makes them look off by one sometimes.
med_age_table_data <- filter(med_age_timeline_data, year%in%c('2025', '2040')) %>%
    select(year, location, mean) %>%
    pivot_wider(names_from = year, values_from = mean) %>%
    mutate(fake_delta = `2040` - `2025`)
med_age_delta_replaced <- med_age_delta_data %>%
    mutate(mean = med_age_table_data$fake_delta)
table_med_age <- cbind(do_conversion_operations(filter(med_age_timeline_data, year%in%c('2025', '2040'))),
                       do_delta_conversion_operations(med_age_delta_replaced))

# Combine to make full table ----
csv_double_rows <- convert_to_double_rows(as.matrix(cbind(total_prev_column, table_prop_over_55, table_prop_over_65, table_med_age)))

# write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.csv", sep=",", row.names=F, col.names=F)
# save(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.Rdata")

do_create_table_colors <- function(df, num.groups=3, groups.with.percent=1:2, groups.with.comma=numeric(0)) {
    my_cols <- df[2 * seq_along(state_order) - 1 , c(1 + 3 * (1:num.groups))]
    my_cols[,groups.with.comma] <- gsub(",", "", my_cols[,groups.with.comma])
    my_cols[,groups.with.percent] <- gsub("%", "", my_cols[,groups.with.percent])
    my_cols <- matrix(as.numeric(my_cols), ncol=ncol(my_cols))
    normalized <- apply(my_cols, 2, function(col) {
        pos <- col > 0
        neg <- col < 0
        pos_max <- if (sum(pos) > 0) max(col[pos]) else 1
        neg_min <- if (sum(neg) > 0) abs(min(col[neg])) else -1
        overall_max <- max(pos_max, neg_min)
        col[pos] <- col[pos] / overall_max
        col[neg] <- col[neg] / overall_max
        col
    })
    normalized <- t(apply(apply(normalized, 2, function(col) {rep(col, each=2)}),
                          1, function(row) {rep(row, each=3)}))
    cbind(rep(0, nrow(df)), normalized)
}

table_colors <- do_create_table_colors(csv_double_rows)
# write.table(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.csv", sep=",", row.names=F, col.names=F)
# save(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.Rdata")
write.shaded.table(csv_double_rows,
                   file = "../jheem_analyses/applications/age_analysis/shaded_table.xlsx",
                   color.by = table_colors,
                   thresholds = c(-1, 0, 1),
                   colors = c("#2171b5", "white", "#fd8d3c"),
                   write.row.names = F,write.col.names = F)
