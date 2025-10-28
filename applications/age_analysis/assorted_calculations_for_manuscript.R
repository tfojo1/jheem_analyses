# Assorted calculations used for manuscript

# To get total in 2040 vs. 2025
get_stats(apply(total_results[c("2025", "2040"),,"diagnosed.prevalence",,], c("year", "sim"), sum))

# To show that Wisconsin and Tennessee are the only two states with more 25-34
# than 35-44 by 2040.
stacked_area_plot_data %>%
    filter(year==2040) %>%
    select(age, location, percentage) %>%
    pivot_wider(names_from = age, values_from = percentage) %>%
    filter(`25-34 years` > `35-44 years`)

# Proportion MSM age 55+?
msm_totals <- apply(race_sex_results[c("2025", "2040"),,,"msm",,"diagnosed.prevalence",,],
                    c("year", "age", "sim"), sum)

msm_prop <- get_prop_over_55(msm_totals)
get_stats(msm_prop)
non_msm_totals <- apply(race_sex_results[c("2025", "2040"),,,c("heterosexual_male", "female"),,"diagnosed.prevalence",,],
                        c("year", "age", "sim"), sum)
non_msm_prop <- get_prop_over_55(non_msm_totals)
get_stats(non_msm_prop)

# Proportion age 55+ by race
race_totals <- apply(race_sex_results[c("2025", "2040"),,,,,"diagnosed.prevalence",,],
                     c("year", "age", "race", "sim"), sum)
race_prop <- get_prop_over_55(race_totals, keep.dimensions = c("year", "race"))
get_stats(race_prop, keep.dimensions = c("year", "race"))

# Proportion of Medicare eligible (age 65+ in population) who are diagnosed with HIV rises?
medicare_eligible_population <- get_65_estimates(apply(age_results[c("2025", "2040"),,,"population",,],
                                                       c("year", "age", "sim"), sum),
                                                 top.age=100)
load("../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")
fraction_medicare_eligible_diagnosed <- get_stats(apply(sixty_five_plus_estimates["65_plus",,,],
                                                        c("year", "sim"),
                                                        sum) /
                                                      medicare_eligible_population["65_plus",,],
                                                  digits = 4,
                                                  multiply.by.100 = T)

# Why we used these 24 states - show percent national prevalence covered
region_total_prevalence = sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location["2021",my_states])

national_prevalence = sum(SURVEILLANCE.MANAGER$data$diagnosed.prevalence$estimate$cdc.hiv$cdc.national$year__location__age__race__sex__risk["2021",,,,,],na.rm = T)

region_total_prevalence / national_prevalence # 86%

# Median age by sex (meaning heterosexual vs. MSM)?

# Median age by race?