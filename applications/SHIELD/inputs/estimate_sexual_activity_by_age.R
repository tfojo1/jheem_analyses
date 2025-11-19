##############################
# 1. Setup
##############################

library(ggplot2)
library(tidyverse)

##############################
# 2. Load GSS data
##############################

## Load data from GSS survey (https://gss.norc.org/)
#for % of population sexually active
GSS_bysex_sexfreq <- read_csv("../jheem_analyses/applications/SHIELD/data_files/sexual_activity_by_age/GSS_bysex_sexfreq.csv")
View(GSS_bysex_sexfreq)

#for average number of sexual partners by age
GSS_n_sexpartners <- read_csv("../jheem_analyses/applications/SHIELD/data_files/sexual_activity_by_age/GSS_n_sexpartners.csv")
View(GSS_n_sexpartners)

##############################
# 3. Define age brackets
##############################

# Define the age brackets
age_breaks <- c(0, 14, 19, 24, 29, 34, 39, 44, 49, 54, 64, Inf)
age_labels <- c("0-14", "15-19", "20-24", "25-29", "30-34",
                "35-39", "40-44", "45-49", "50-54", "55-64", "65+")

##############################
# 4. Add age groups to datasets
##############################

# Summarize total 'Any Sex' and total respondents by sex and age group
# Note GSS runs 18 and older have to approximate youngest age group
# manually add CDC YRBS result for % having sex before 13

GSS_anysex_by_age_sex <- GSS_bysex_sexfreq %>%
    mutate(
        age_group = cut(`AGE (age of respondent)`,
                        breaks = age_breaks,
                        labels = age_labels,
                        right = TRUE)
    ) %>%
    add_row(age_group = "0–14", `Percent Any Sex` = 6.2) # value is yearly average since 1991
View(GSS_anysex_by_age_sex)

GSS_n_sex_part_by_age_sex <- GSS_n_sexpartners %>%
    mutate(
        age_group = cut(`AGE (age of respondent)`,
                        breaks = age_breaks,
                        labels = age_labels,
                        right = TRUE)
    )

##############################
# 5. Boxplots of totals by age group
##############################

ggplot(GSS_anysex_by_age_sex, aes(x = age_group, y = Total, fill = SEX)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    labs(
        title = "General Social Survey (Annual GSS 1972-2024)",
        x = "Age Group",
        y = "Total respondants"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

ggplot(GSS_n_sex_part_by_age_sex, aes(x = age_group, y = Total, fill = Sex)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    labs(
        title = "General Social Survey (Annual GSS 1972-2024)",
        x = "Age Group",
        y = "Total respondants"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

##############################
# 6. Boxplots of percent values by age group
##############################

ggplot(GSS_anysex_by_age_sex, aes(x = age_group, y = `Percent Any Sex`, fill = SEX)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    labs(
        title = "General Social Survey (Annual GSS 1972-2024)",
        x = "Age Group",
        y = "Pecent having any sex (within last year)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

ggplot(GSS_n_sex_part_by_age_sex, aes(x = age_group, y = `Percent Any Partner`, fill = Sex)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    labs(
        title = "General Social Survey (Annual GSS 1972-2024)",
        x = "Age Group",
        y = "Pecent having a Sexual Partner (within last year)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

##############################
# 7. Weighted summaries by age group and sex
##############################

GSS_anysex_summary <- GSS_anysex_by_age_sex %>%
    group_by(SEX, age_group) %>%
    summarise(
        weighted_mean_anysex = weighted.mean(`Percent Any Sex`, w = Total, na.rm = TRUE),
        .groups = "drop"
    )

GSS_anypartner_summary <- GSS_n_sex_part_by_age_sex %>%
    group_by(Sex, age_group) %>%
    summarise(
        weighted_mean_anysex = weighted.mean(`Percent Any Partner`, w = Total, na.rm = TRUE),
        .groups = "drop"
    )

##############################
# 8. Boxplots with weighted means overlaid
##############################

ggplot(GSS_anysex_by_age_sex, aes(x = age_group, y = `Percent Any Sex`, fill = SEX)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    # Add weighted mean as a point
    geom_point(
        data = GSS_anysex_summary,
        aes(x = age_group, y = weighted_mean_anysex),
        position = position_dodge(width = 0.75),
        size = 1,
        shape = 21,
        stroke = 1.2
    ) +
    labs(
        title = "General Social Survey (Annual GSS 1972–2024)",
        x = "Age Group",
        y = "Percent having any sex (within last year)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

ggplot(GSS_n_sex_part_by_age_sex, aes(x = age_group, y = `Percent Any Partner`, fill = Sex)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 1.5, alpha = 0.7) +
    # Add weighted mean as a point
    geom_point(
        data = GSS_anypartner_summary,
        aes(x = age_group, y = weighted_mean_anysex),
        position = position_dodge(width = 0.75),
        size = 1,
        shape = 21,
        stroke = 1.2
    ) +
    labs(
        title = "General Social Survey (Annual GSS 1972–2024)",
        x = "Age Group",
        y = "Percent having any sex (within last year)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )

##############################
# 9. Clean up / standardize any-sex probability
##############################

GSS_anysex_summary <- GSS_anysex_summary %>%
    mutate(
        p_any = ifelse(weighted_mean_anysex > 1, weighted_mean_anysex/100, weighted_mean_anysex)
    )

##############################
# 10. Compute average number of partners by age and sex
##############################

GSS_with_avg <- GSS_n_sex_part_by_age_sex %>%
    mutate(
        weighted_sum =
            `No partners` * 0 +
            `1 partner` * 1 +
            `2 partners` * 2 +
            `3 partners` * 3 +
            `4 partners` * 4 +
            `5-10 partners` * 7.5 +
            `11+ partners` * 11,
        
        valid_responses = `No partners` + `1 partner` + `2 partners` + `3 partners` +
            `4 partners` + `5-10 partners` + `11+ partners`,
        
        avg_partners = weighted_sum / valid_responses
    ) %>%
    group_by(Sex, age_group) %>%
    summarise(
        avg_n_partners = mean(avg_partners, na.rm = TRUE),
        .groups = "drop"
    )

##############################
# 11. Construct sexual activity score (partners × any-sex probability)
##############################

GSS_with_avg$sexual_activity <- GSS_with_avg$avg_n_partners*(GSS_anysex_summary$weighted_mean_anysex/100)

##############################
# 12. Normalize sexual activity within sex (ref = age 20–24)
##############################

GSS_norm <- GSS_with_avg %>%
    group_by(Sex) %>%
    mutate(
        # Reference values within sex
        ref_act = sexual_activity[age_group == "20-24"],
        
        # Normalize
        act_norm_20_24 = sexual_activity / ref_act
    )

##############################
# 13. Join summaries and recompute sexual activity (final object)
##############################

#### 
GSS_activity <- GSS_with_avg %>%
    left_join(
        GSS_anysex_summary,
        by = c("Sex" = "SEX", "age_group" = "age_group")
    ) %>%
    mutate(
        sexual_activity = avg_n_partners * (weighted_mean_anysex / 100)
    )

##############################
# 14. Plot sexual activity index across age groups
##############################

ggplot(GSS_activity, aes(x = age_group, y = sexual_activity, color = Sex, group = Sex)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 2) +
    labs(
        title = "Sexual Activity Score Across Age Groups (GSS 1972–2024)",
        x = "Age Group",
        y = "Sexual Activity Index\n(avg partners × % sexually active)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
    )
