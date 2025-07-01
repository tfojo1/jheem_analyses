# For Melissa
# 6/30/2025

source("../jheem_analyses/applications/EHE/ehe_specification.R")
source("../jheem_analyses/presentation/make_pretty_table.R")

table_contents <- get(load("../jheem_analyses/applications/age_analysis/table1.R"))
table_color_by <- get(load("../jheem_analyses/applications/age_analysis/table1_colors.R"))
write.shaded.table(table_contents,
                   file = "../jheem_analyses/applications/age_analysis/shaded_table_test.xlsx",
                   color.by = table_color_by,
                   thresholds = c(-1, 0, 1),
                   colors = c("#006d2c", "white", "#e6550d"),)
