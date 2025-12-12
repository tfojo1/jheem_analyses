# Make supplemental table for median age by sex per state
# Three sets of columns with 2025, 2040, and change.

library(xlsx)
library(tidyverse)
library(locations)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")
source("../jheem_analyses/presentation/make_pretty_table.R")

# Source necessary objects ----
sex_totals <- get(load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_sex_data.Rdata"))
sex_delta_totals <- get(load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_sex_data.Rdata"))
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_sex_loc_data.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_sex_loc_data.Rdata")
sex_data <- rbind(med_age_timeline_sex_data,
                   sex_totals %>%
                       mutate(location="total") %>%
                       relocate(location, .after = year))
sex_delta_data <- rbind(med_age_delta_sex_data,
                         sex_delta_totals %>%
                             mutate(location="total") %>%
                             relocate(location, .before = sex))

# But we want to overwrite the statistical mean delta with the difference in the means so that the numbers on the
# table look coherent. Otherwise, the rounding makes them look off by one sometimes.
fake_data <- filter(sex_data, year%in%c('2025', '2040')) %>%
    select(year, location, sex, mean) %>%
    pivot_wider(names_from = year, values_from = mean) %>%
    mutate(fake_delta = `2040` - `2025`)
sex_delta_data <- sex_delta_data %>%
    mutate(mean = fake_data$fake_delta)

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
        mutate(int25 = paste0("[", lo25, "-", up25, "]")) %>%
        mutate(int40 = paste0("[", lo40, "-", up40, "]")) %>%
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

# Black, then Hispanic, then Other, with three columns each
table_states <- cbind(do_conversion_operations(filter(sex_data, sex=="msm")),
                      do_delta_conversion_operations(filter(sex_delta_data, sex=="msm")),
                      do_conversion_operations(filter(sex_data, sex=="heterosexual")),
                      do_delta_conversion_operations(filter(sex_delta_data, sex=="heterosexual")))

# Combine to make full table ----
csv_double_rows <- convert_to_double_rows(as.matrix(table_states))

# I want the table to be colored by the max overall in all delta columns, because they are all median age.
do_create_table_colors <- function(df) {
    my_cols <- matrix(as.numeric(df[2 * seq_along(state_order) - 1, 3 * (1:2)]), ncol = 2)
    pos <- my_cols > 0
    neg <- my_cols < 0
    pos_max <- if (sum(pos) > 0) max(my_cols[pos]) else 1
    neg_min <- if (sum(neg) > 0) abs(min(my_cols[neg])) else -1
    overall_max <- max(pos_max, neg_min)
    my_cols[pos] <- my_cols[pos] / overall_max
    my_cols[neg] <- my_cols[neg] / overall_max
    normalized <- t(apply(apply(my_cols, 2, function(col) {rep(col, each=2)}),
                          1, function(row) {rep(row, each=3)}))
}

# write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.csv", sep=",", row.names=F, col.names=F)
# save(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.Rdata")

# do_create_table_colors <- function(df, num.groups=2, groups.with.percent=numeric(0), groups.with.comma=numeric(0), with.total.prev=F) {
#     my_cols <- df[2 * seq_along(state_order) - 1 , c(with.total.prev + 3 * (1:num.groups))]
#     my_cols[,groups.with.comma] <- gsub(",", "", my_cols[,groups.with.comma])
#     my_cols[,groups.with.percent] <- gsub("%", "", my_cols[,groups.with.percent])
#     my_cols <- matrix(as.numeric(my_cols), ncol=ncol(my_cols))
#     normalized <- apply(my_cols, 2, function(col) {
#         pos <- col > 0
#         neg <- col < 0
#         pos_max <- if (sum(pos) > 0) max(col[pos]) else 1
#         neg_min <- if (sum(neg) > 0) abs(min(col[neg])) else -1
#         overall_max <- max(pos_max, neg_min)
#         col[pos] <- col[pos] / overall_max
#         col[neg] <- col[neg] / overall_max
#         col
#     })
#     normalized <- t(apply(apply(normalized, 2, function(col) {rep(col, each=2)}),
#                           1, function(row) {rep(row, each=3)}))
#     if (with.total.prev)
#         cbind(rep(0, nrow(df)), normalized)
#     else
#         normalized
# }

table_colors <- do_create_table_colors(csv_double_rows)

# write.table(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.csv", sep=",", row.names=F, col.names=F)
# save(table_colors, file="../jheem_analyses/applications/age_analysis/table1_colors.Rdata")
write.shaded.table(csv_double_rows,
                   file = "../jheem_analyses/applications/age_analysis/Figures/Manuscript/sex_shaded_table.xlsx",
                   color.by = table_colors,
                   thresholds = c(-1, 0, 1),
                   colors = c("#2171b5", "white", "#fd8d3c"),
                   write.row.names = F,write.col.names = F, debug=T)
