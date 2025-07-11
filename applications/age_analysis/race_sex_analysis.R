# race/sex analysis at Total region level
stacked_area_race_arr <- apply(race_results[as.character(2025:2040),,,,"diagnosed.prevalence",,],
                               c("year", "age", "race", "sim"),
                               sum)
stacked_area_race_plot_data <- reshape2::melt(get_stats(stacked_area_race_arr,
                                                        keep.dimensions=c("year", "age", "race"))) %>%
    pivot_wider(names_from="metric") %>%
    group_by(year, race) %>%
    mutate(percentage=100*median/sum(median)) %>%
    ungroup()

make_stacked_area_race_plots <- function(type = c("absolute", "percentage")[1]) {
    y_param <- if (type=="absolute") "median" else "percentage"
    ggplot(data=stacked_area_race_plot_data, aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=!!(sym(y_param))), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(race), scales="free_y") +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(stacked_area_race_plot_data$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}
make_stacked_area_race_plots()
make_stacked_area_race_plots("percentage")

race_prop55 <- reshape2::melt(get_stats(get_prop_over_55(race_results[c("2025", "2040"),,,,"diagnosed.prevalence",,],
                                          keep.dimensions=c("year", "race", "location")),
                         keep.dimensions = c("year", "race", "location"))) %>%
    pivot_wider(names_from="metric")
race_prop55_total <- reshape2::melt(get_stats(get_prop_over_55(apply(race_results[c("2025", "2040"),,,,"diagnosed.prevalence",,],
                                                                     c("year", "age", "race", "sim"),
                                                                     sum),
                                                               keep.dimensions=c("year", "race")),
                                              keep.dimensions = c("year", "race"))) %>%
    pivot_wider(names_from="metric")

# The prop55 of black in 2025 varies a lot [among states].
# AL lowest with 30 (25 to 34) and
# NY highest with 54 (50 to 57)
# By 2040, the discrepancy [among states] widens.
# AL still lowest with 30 (18 to 49) and
# FL highest with 62 (48 to 76)

# The prop55 of hispanic in 2025 varies as much.
# TX lowest with 29 (25 to 34) and
# NY highest with 57 (52 to 61).
# By 2040, the disparity [among states] is very wide.
# MO lowest with 32 (17 to 47) and
# NY highest with 72 (61 to 87)

# The prop55 of other in 2025 again varies.
# MS lowest with 41 (36 to 47) and
# CA highest with 65 (61 to 69).
# By 2040, the disparity [among states] is very wide!
# MS lowest with 27 (17 to 40) and
# CA highest with 82 (72 to 90).

# "black" is youngest, "hispanic" middle, and "other" oldest.
# All races will have more 55+ in 2040.
# prop 55+
# black: 2025: 41%, 2040: 50%
# hispanic: 2025: 42%, 2040: 59%
# other: 2025: 59%, 2040: 67%
# by total numbers of 55+, black > hispanic > other

stacked_area_sex_arr <- apply(race_sex_results[as.character(2025:2040),,,,,"diagnosed.prevalence",,],
                              c("year", "age", "sex", "sim"),
                              sum)
stacked_area_sex_plot_data <- reshape2::melt(get_stats(stacked_area_sex_arr,
                                                       keep.dimensions=c("year", "age", "sex"))) %>%
    pivot_wider(names_from="metric") %>%
    mutate(sex = fct_recode(sex, "not_msm" = "heterosexual_male")) %>%
    mutate(sex = fct_recode(sex, "not_msm" = "female")) %>%
    group_by(year, age, sex) %>% 
    summarise_all(sum) %>%
    group_by(year, sex) %>%
    mutate(percentage=100*median/sum(median)) %>%
    ungroup()
make_stacked_area_sex_plots <- function(type = c("absolute", "percentage")[1]) {
    y_param <- if (type=="absolute") "median" else "percentage"
    ggplot(data=stacked_area_sex_plot_data, aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=!!(sym(y_param))), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(sex), scales="free_y") +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(stacked_area_sex_plot_data$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}
make_stacked_area_sex_plots()
make_stacked_area_sex_plots("percentage")

sex_prop55_unagg_arr <- map_sex(apply(race_sex_results[c("2025","2040"),,,,,"diagnosed.prevalence",,],
                               c("year", "age", "sex", "sim"),
                               sum))
sex_prop55_data <- reshape2::melt(get_stats(get_prop_over_55(sex_prop55_unagg_arr,
                                                             keep.dimensions=c("year", "sex")),
                                            keep.dimensions=c("year", "sex"))) %>%
    pivot_wider(names_from="metric")

# These show that not_msm is, and will continue to be, much older than msm.
# Both categories are aging, but not_msm is aging more.
# prop 55+ in msm in 2025: 43%, 2040: 53%.
# prop 55+ in non_msm in 2025: 53%, 2040: 65%.