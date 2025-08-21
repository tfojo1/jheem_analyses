library(smplot2)
library(tidyverse)
library(xlsx)
source("../jheem_analyses/presentation/make_pretty_table.R")

# Stacked Area Plots----
stacked_area_total_arr <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
stacked_area_raw_arr <- array(c(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,],
                                stacked_area_total_arr),
                              dim=c(year=length(2025:2040), age=5, sim=1000, location=12),
                              dimnames=list(year=2025:2040, age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))

stacked_area_plot_data <- reshape2::melt(get_stats(stacked_area_raw_arr,
                                                   keep.dimensions = c('year', 'age', 'location'))) %>%
    pivot_wider(names_from="metric") %>%
    group_by(year, location) %>%
    mutate(percentage=100*median/sum(median)) %>%
    ungroup() %>%
    mutate(stateName = factor(get.location.name(location), levels=state_order_names))
stacked_area_plot_data[stacked_area_plot_data$location == "total", "stateName"] <- "Total"

make_stacked_area_plots <- function(states, type=c("absolute", "percentage")[1]) {
    y_param <- if (type=="absolute") "median" else "percentage"
    ggplot(data=filter(stacked_area_plot_data, location%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=!!(sym(y_param))), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(stateName), scales="free_y") +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(stacked_area_plot_data$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}

make_stacked_area_plots(c(my_states, "total"))
make_stacked_area_plots(c(my_states, "total"), "percentage")

# Alt view of data with CIs for percentage
stacked_area_CI_arr <- stacked_area_raw_arr

# Median Age Timeline----
# To do: make get_stats give quartile range and also mean. And we need to get the delta median age BEFORE taking stats
# To get the totals, we'll get a seperate array that is total, then append it.
med_age_timeline_total_arr <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
med_age_timeline_raw_arr <- array(c(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,],
                                    med_age_timeline_total_arr),
                                  dim=c(year=length(2025:2040), age=5, sim=1000, location=12),
                                  dimnames=list(year=2025:2040, age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))
med_age_timeline_arr <- get_med_age(med_age_timeline_raw_arr,
                                    keep.dimensions=c("year", "location"))
save(med_age_timeline_arr, file="../jheem_analyses/applications/age_analysis/med_age_timeline_arr.Rdata")
med_age_delta_arr <- med_age_timeline_arr["2040",,] - med_age_timeline_arr["2025",,]
med_age_delta_data <- reshape2::melt(get_stats(med_age_delta_arr,
                                               keep.dimensions=c("location"),
                                               include.mean=T)) %>%
    pivot_wider(names_from="metric") %>%
    mutate(stateName = factor(get.location.name(location), levels=state_order_names))
save(med_age_delta_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")
med_age_timeline_data <- reshape2::melt(get_stats(med_age_timeline_arr,
                                        keep.dimensions=c("year", "location"),
                                        include.mean=T, include.quartiles=T)) %>%
    mutate(year=as.factor(year)) %>%
    pivot_wider(names_from="metric") %>%
    mutate(stateName = factor(get.location.name(location), levels=c(state_order_names, "Total")))
med_age_timeline_data[med_age_timeline_data$location=="total", "stateName"] <- "Total"
save(med_age_timeline_data, file="../jheem_analyses/applications/age_analysis/med_age_timeline_data.Rdata")

make_med_age_timeline <- function(states) {
    ggplot(data=filter(med_age_timeline_data, location%in%states), aes(x=year)) +
        geom_point(mapping=aes(y=mean)) +
        facet_wrap(vars(stateName)) +
        geom_errorbar(mapping=aes(ymin=lower, ymax=upper, width = 0.5)) +
        ggtitle(paste0("Median Age")) +
        labs(y="Age", x= "Year") +
        theme_bw() +
        scale_x_discrete(breaks=seq(2025, 2040, by=5)) +
        scale_y_continuous(limits=c(0, NA))
}
make_med_age_timeline(c(my_states, "total"))

# Table----
state_order <- c(names(sort(apply(total_results['2025',,"diagnosed.prevalence",,], "location", mean), decreasing = T)), "total")
state_order_names <- c(get.location.name(state_order[1:11]), total="Total")
save(state_order, file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
save(state_order_names, file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")
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

table_med_age <- cbind(do_conversion_operations(filter(med_age_timeline_data, year%in%c('2025', '2040'))),
                       do_delta_conversion_operations(med_age_delta_data))

num_totals <- apply(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
num_raw_arr <- array(c(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                       num_totals),
                     dim=c(year=2, age=5, sim=1000, location=12),
                     dimnames=list(year=c("2025", "2040"), age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))
num_data <- get_num_over_55(num_raw_arr,
                            keep.dimensions=c("year", "location"))
num_delta <- num_data["2040",,] - num_data["2025",,]

table_num_over_55 <- cbind(do_conversion_operations(reshape2::melt(get_stats(num_data,
                                                                             keep.dimensions=c("year", "location"))) %>%
                                                        pivot_wider(names_from="metric") %>%
                                                        mutate(stateName=get.location.name(location))),
                           do_delta_conversion_operations(reshape2::melt(get_stats(num_delta,
                                                                                   keep.dimensions="location")) %>%
                                                              pivot_wider(names_from="metric") %>%
                                                              mutate(stateName=get.location.name(location))))
prop_data <- get_prop_over_55(num_raw_arr,
                              keep.dimensions=c("year", "location"))
prop_delta <- prop_data["2040",,] - prop_data["2025",,]
table_prop_over_55 <- cbind(do_conversion_operations(reshape2::melt(get_stats(prop_data,
                                                                              keep.dimensions=c("year", "location"))) %>%
                                                         pivot_wider(names_from="metric"), is.percentage = T),
                            do_delta_conversion_operations(reshape2::melt(get_stats(prop_delta,
                                                                                    keep.dimensions="location")) %>%
                                                               pivot_wider(names_from="metric"), is.percentage=T))

# This column has to be padded with empty rows
total_prev_by_loc <- apply(total_results["2025",,"diagnosed.prevalence",,], "location", mean)
total_prev_column <- rep(prettyNum(c(sort(round(total_prev_by_loc), decreasing = T),
                                     total=round(sum(total_prev_by_loc))),
                                   big.mark = ",", preserve.width = "none"), each = 2)
total_prev_column[2 * seq_along(state_order)] <- ""

# Prepare for table formatting where median and CI are stacked vertically
csv_double_rows <- convert_to_double_rows(as.matrix(cbind(table_prop_over_55, table_num_over_55, table_med_age)))
csv_double_rows <- cbind(total_prev_column, csv_double_rows)

write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.csv", sep=",", row.names=F, col.names=F)
save(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.Rdata")
# the color.by needs to be the same size. One color per set of three columns

do_create_table_colors <- function(df) {
    my_cols <- df[2 * seq_along(state_order) - 1 , c(4, 7, 10)]
    my_cols[,2] <- gsub(",", "", my_cols[,2])
    my_cols[,1] <- gsub("%", "", my_cols[,1])
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
write.table(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.csv", sep=",", row.names=F, col.names=F)
save(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.Rdata")
write.shaded.table(csv_double_rows,
                   file = "../jheem_analyses/applications/age_analysis/shaded_table.xlsx",
                   color.by = table_colors,
                   thresholds = c(-1, 0, 1),
                   colors = c("#2171b5", "white", "#fd8d3c"))


# Scatterplot for GA----

make_scatterplot <- function(x.param, y.param, x.lab, y.lab) {
    ggplot(data=scatterplot_data, mapping=aes(y=jitter(!!sym(y.param)), x=!!sym(x.param))) +
        geom_point() +
        ggtitle(paste0("Georgia")) +
        labs(y = y.lab, x = x.lab) +
        theme_bw() +
        sm_statCorr() +
        geom_smooth()
}

prev_arr <- age_results[c('2025', '2040'),,,c("diagnosed.prevalence"),"GA",]
ratio_arr <- total_results[c("2025", "2040"),,c("new"),"GA",] / total_results[c("2025", "2040"),,c("diagnosed.prevalence"),"GA",]
ratio_arr <- ratio_arr["2025",,simplify=F]
med_age_df <- reshape2::melt(get_med_age(prev_arr)) %>%
    pivot_wider(names_from=year) %>%
    mutate(medAge2040=`2040`, medAge2025=`2025`, deltaMedAge = `2040` - `2025`) %>%
    select(-`2040`, -`2025`)
ratio_df <- reshape2::melt(ratio_arr, drop=F) %>%
    pivot_wider(names_from=year) %>%
    mutate(ratio25 = `2025`) %>%
    select(sim, ratio25)
prop_arr <- get_prop_over_55(age_results[c('2025', '2040'),,,"diagnosed.prevalence", "GA",], digits=4)
prop_arr <- prop_arr["2040",,drop=F] - prop_arr["2025",,drop=F]
prop_df <- reshape2::melt(prop_arr) %>%
    mutate(deltaProp55 = value) %>%
    select(sim, deltaProp55)
prop35_arr <- get_prop_under_35(age_results['2025',,,"diagnosed.prevalence", "GA",], keep.dimensions = NULL, digits=4)
prop35_df <- data.frame(Prop35in2025=prop35_arr, sim=1:length(prop35_arr))

popProp_arr <- get_prop_over_55(age_results["2025",,,"population", "GA",], keep.dimensions = NULL, digits=4)
popProp_df <- data.frame(popProp55 = popProp_arr, sim=1:length(popProp_arr))

prop25_arr <- get_prop_over_55(age_results["2025",,,"diagnosed.prevalence", "GA",], keep.dimensions = NULL, digits=4)
prop25_df <- data.frame(prop55in25 = prop_arr, sim=1:length(prop_arr))
prop40_arr <- get_prop_over_55(age_results["2040",,,"diagnosed.prevalence", "GA",], keep.dimensions = NULL, digits=4)
prop40_df <- data.frame(prop55in40 = prop_arr, sim=1:length(prop_arr))

scatterplot_data <- med_age_df %>%
    merge(ratio_df, by="sim") %>%
    merge(prop_df, by="sim") %>%
    merge(prop35_df, by="sim") %>%
    merge(popProp_df, by="sim") %>%
    merge(prop25_df, by="sim") %>%
    merge(prop40_df, by="sim")
    # merge(merge(merge(med_age_df, ratio_df, by="sim"), prop_df, by="sim"), prop35_df, by="sim")

make_scatterplot("ratio25", "deltaMedAge", "Ratio New Diagnoses to Diagnosed Prevalence in 2025", "Delta Median Age")
make_scatterplot("ratio25", "medAge2040", "Ratio New Diagnoses to Diagnosed Prevalence in 2025", "Median Age 2040")
make_scatterplot("ratio25", "deltaProp55", "Ratio New Diagnoses to Diagnosed Prevalence in 2025", "Delta Proportion 55+")
make_scatterplot("Prop35in2025", "deltaMedAge", "Proportion Under 35 in 2025", "Delta Median Age")
make_scatterplot("popProp55", "deltaMedAge", "Proportion General Population 55+ in 2025", "Delta Median Age")
make_scatterplot("popProp55", "medAge2040", "Proportion General Population 55+ in 2025", "Median Age 2040")
make_scatterplot("deltaProp55", "deltaMedAge", "Delta Proportion 55+", "Delta Median Age")
make_scatterplot("prop55in25", "medAge2025",  "Proportion Diagnosed Prevalence 55+ in 2025", "Median Age 2025")
make_scatterplot("prop55in40", "medAge2040",  "Proportion Diagnosed Prevalence 55+ in 2040", "Median Age 2040")

# Total prevalence all ages
total_results <- get(load("../jheem_analyses/applications/age_analysis/total_results.Rdata"))
total_prev_arr <- apply(total_results[c("2025", "2040"),,"diagnosed.prevalence",,], c("year", "sim"), sum)
total_prev_stats <- get_stats(total_prev_arr)
