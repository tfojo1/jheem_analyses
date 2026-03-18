source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")

# Example for Baltimore:
sim.last=lastB
sim.first=simsetB$first.sim()
last20=B20
#' @Andrew: we want the path to be compatible with the calibration code and the city we inspect
plotting_path="shield/calibrationPlots/Baltimore/" #where to print: we ideally want to have city specific and calibrationcode spacific locations

# STAGE0 OUTCOMES ----
## POPULATION ----
for (outcome in c("population")){
    print(outcome)
    p=simplot(
        last20,
        outcomes=outcome,
        style.manager = source.style.manager
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome, ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    # One_way stratification ----
    for (strata in c("sex","race","age")){
        p=simplot(
            last20,
            outcomes=outcome,
            facet.by = strata,
            style.manager = source.style.manager
        )
        file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_",strata, ".png"))
        ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    }
    # Two_way stratification ----
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by = "age",
        split.by = "sex",
        style.manager = source.style.manager
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_","age_sex", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    #
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by = "age",
        split.by = "race",
        style.manager = source.style.manager
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_","age_race", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    #
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by = "sex",
        split.by = "race",
        style.manager = source.style.manager
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_","sex_race", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
}

#@Andrew: Please add plots for other outcomes in stage0 where we have the data ----
###

# STAGE1 OUTCOMES ----
## Totals
for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
    print(outcome)
    p=simplot(
        last20,
        outcomes=outcome,
        style.manager = source.style.manager
    )
    # Save in multiple formats (adjust width/height as needed)
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome, ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
}

# Oneway Stratification ----
for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
    for (strata in c("sex","race","age")){
        print(outcome)
        p=simplot(
            last20,
            outcomes=outcome,
            facet.by =strata,
            style.manager = source.style.manager
        )
        # Save in multiple formats (adjust width/height as needed)
        file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_",strata, ".png"))
        ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    }}
# Two_way Stratification ----
for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
    print(outcome)
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by ="age",
        split.by = "race",
        style.manager = source.style.manager 
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_age_race", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    #
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by ="age",
        split.by = "sex",
        style.manager = source.style.manager 
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_age_sex", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    #
    p=simplot(
        last20,
        outcomes=outcome,
        facet.by ="sex",
        split.by = "race",
        style.manager = source.style.manager 
    )
    file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_sex_race", ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
}


# # One_way- simonly for Sex ----
# for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
#     for (strata in c("sex")){
#         print(outcome)
#         p=simplot(
#             last20,
#             outcomes=outcome,
#             facet.by =strata,
#             style.manager = source.style.manager,
#             plot.which = "sim.only"
#         )
#         # Save in multiple formats (adjust width/height as needed)
#         file_base <- paste0("simplot_", outcome, "_", strata,"_simOnly")
#         file_png  <- file.path(get.jheem.root.directory(), paste0("shield/calibrationPlots/",file_base, ".png"))
#         ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
#     }
# }