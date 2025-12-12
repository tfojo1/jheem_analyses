# Assorted calculations used for manuscript
load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_race_arr.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_sex_arr.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_arr.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_data.Rdata")
source("../jheem_analyses/applications/age_analysis/helpers.R")

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
race_medAge <- get_stats(med_age_timeline_race_arr[c("2025", "2040"),,], keep.dimensions = c("year", "race"))

# Median age by sex
sex_medAge <- get_stats(med_age_timeline_sex_arr[c("2025", "2040"),,], keep.dimensions = c("year", "sex"))

# I'll make my own "generate_supp_sex_table" and such for these.

# For comparison to Parastu median ages in 2030
(filter(med_age_timeline_data, location=="total"))

# shows 2025: 51 [51 - 52], 2030: 55 [54 - 57]
# which is a bigger increase than Parastu projects
# she also projects 23% of ART users older than 65 by 2030,
# but we start at 25% of diagnosed in 2025 and rise to 50% by 2040.

# total diagnosed in 2030
(get_stats(apply(total_results[c("2025", "2030"),,"diagnosed.prevalence",,], c("year", "sim"), sum)))
# 960,000 [950,000 to 970,000] in 2030 vs. Parastu's 910,000 project on ART that year.
# odd because in 2018, PEARL has around 60% of total PWH on ART. Does suppression
# rise from 60% up to >90% over that span?
# well, CDC comparison is prevalence, not just diagnosed.

# Did EHE goals of 75% incidence decrease between 2020 and 2025 happen?
# If so, that would push median age higher.

# Find our annual percent change in new diagnoses... looks like it's approximately 3%
xx <- apply(total_results[as.character(2025:2040),,"new",,], c("year", "location"), mean)
sort(apc_state <- apply(xx, 2, function(col) {mean(diff(col) / col[2:length(col)])}))
(yy <- apply(apply(total_results[as.character(2025:2040),,"new",,], c("year", "sim"), sum), "year", mean))
(mean(diff(yy)) / yy[2:length(yy)])

# Mortality results. Annual HIV deaths range from 16,700 in 2025 to 18,100 in 2037.
load("../jheem_analyses/applications/age_analysis/Rdata Objects/mortality_results.Rdata")
annual_hiv_deaths <- apply(apply(mortality_results, c("year", "location"), mean), "year", sum)

# # Find mortality per prevalence. Roughly 1.8% each year.
# annual_prev <- apply(apply(total_results[as.character(2025:2040),,"diagnosed.prevalence",,], c("year", "location"), mean), "year", sum)
# annual_hiv_deaths/annual_prev

# still gets 1.8% per year
annual_prev_by_sim <- apply(total_results[as.character(2025:2040),,"diagnosed.prevalence",,], c("year", "sim"), sum)
annual_deaths_by_sim <- apply(mortality_results[as.character(2025:2040),,,], c("year", "sim"), sum)
annual_mortality_rates <- apply(annual_deaths_by_sim / annual_prev_by_sim, "year", mean)

# versus CDC paper projects 1.9% in 2021 rising to 2.8% by 2040.
(cdc_mort_2021 <- 20000/1050000)
(cdc_mort_2040 <- 30000/1060000)

# we can estimate our mortality by age by finding the difference in prevalence between years, per age group. Also add new diagnoses.
tt <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
nn <- apply(age_results[as.character(2025:2040),,,"new",,], c("year", "age", "sim"), sum)
deaths_2025_by_age <- nn["2026",,] + tt["2025",,] - tt["2026",,]
mort_2025_by_age <- apply(deaths_2025_by_age / tt["2025",,], "age", mean)
deaths_2039_by_age <- nn["2040",,] + tt["2039",,] - tt["2040",,]
mort_2039_by_age <- apply(deaths_2039_by_age / tt["2039",,], "age", mean)
