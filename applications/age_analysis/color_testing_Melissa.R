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
                   colors = c("#2171b5", "white", "#fd8d3c"),)

# option 1 (original): c("yellow", "white", "blue")
# option 2 (also yellow to blue): c("#fee08b", "white", "#3288bd") 
# option 3 (green to orange): c("#74c476", "white", "#fd8d3c")
# option 4 (deeper green to orange): c("#006d2c", "white", "#e6550d")
# option 5 (light blue to light orange): c("#6baed6", "white", "#fdcc8a")
    # REVISED: c("#2171b5", "white", "#fd8d3c")
# option 6 (green to purple): c("#33a02c", "white", "#984ea3")
# option 7 (green to lighter purple): c("#33a02c", "white", "#bc80bd")

