# Proportion symptomatic in primary 
# to inform the prior for this parameter among woman compared to MSM, 
# we use the ratio of diagnosis in the primary stage relative to primary plus secondary stage as a proxy


# we data on primary vs secondary diagnosis for male and female at a national level
# Let ratio_group = (primary diagnoses) / (primary + secondary diagnoses) for that group.
# What it represents: 
# Among all early symptomatic diagnoses (i.e., cases detected while still in primary or secondary stage), ratio_group is the share that are primary at diagnosis. 
# A higher ratio means more people are being caught in primary rather than progressing to secondary before diagnosis.
# If we assume: 
# • similar care‑seeking behavior when symptomatic in primary and secondary within the group (or we model these separately), 
# • similar testing access/delay across groups, 
# • and that most primary/secondary diagnoses are triggered by symptoms (not routine screening), 
# then a higher ratio_group suggests a higher effective probability of symptomatic detection during the primary stage (relative to secondary). 
# Under these assumptions, ratio_group is positively correlated with the proportion symptomatic in primary 
# (or with the product: symptomatic_in_primary × seek_care_in_primary).


s_m=SURVEILLANCE.MANAGER$data$secondary.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.sti$year__location__sex[,,"male"]
p_m=SURVEILLANCE.MANAGER$data$primary.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.sti$year__location__sex[,,"male"]
s_f=SURVEILLANCE.MANAGER$data$secondary.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.sti$year__location__sex[,,"female"]
p_f=SURVEILLANCE.MANAGER$data$primary.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.sti$year__location__sex[,,"female"]

# ratios for male and females
r_f=p_f/(p_f+s_f);r_f
r_m=p_m/(p_m+s_m);r_m

# ratio of ratios for female to male
ror_m_f=r_f/r_m; ror_m_f
range(ror_m_f)
print(mean(ror_m_f))
# This suggest that compared to male, a smaller proportion of females in the primary stage are symptomatic (clinically valid too)
