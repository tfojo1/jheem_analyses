
#----------------------------------------------#
### AUTOMATICALLY GENERATE CALIBRATION PLOTS ###
#----------------------------------------------#

# See section "USAGE" for usage example.

# FUNCTIONS USERS CAN CALL ----

#' @description Prepare the simset data needed to feed into the plotting function. This is separated so that it doesn't have to be done every time the plots are tweaked.
#' @returns A list with an element per location. Each element is list containing that location's full simset, last twenty sims, and final sim, along with a title suffix to use.
prepare_simsets_for_plots <- function(calibration.code, locations, assemble.incomplete = F) {
    source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
    calib_info <- get.calibration.info(calibration.code)
    if (calib_info$n.burn != 0)
        stop("Error: This calibration uses burn > 0, so Andrew's calculation for its n.sim is wrong. Please inform him to rectify this issue")
    n_sim <- calib_info$n.iter / calib_info$thin
    
    # Check calibration progress so that we know the n.sim to use for assembly if we have any incomplete sims
    calib_progress <- get.calibration.progress(version = "shield",
                                               locations = locations,
                                               calibration.code = calibration.code)
    if (dim(calib_progress)[2] > 1)
        stop(paste0("Error: Plotting code only works for single-chain calibrations right now..."))
    
    simset_data <- setNames(lapply(locations, function(location) {
        
        # Check that a simset exists for this location and calibration code
        percent_completion <- calib_progress[location,]
        
        if (is.na(percent_completion))
        {print(paste0("Error: No simset found for '", location, "' and '", calibration.code, "'... Skipping")); return(NULL)}
        
        if (!assemble.incomplete && percent_completion < 100)
        {print(paste0("Error: Simset for '", location, "' and '", calibration.code, "' is incomplete and 'assemble.incomplete' is set to FALSE... Skipping")); return(NULL)}
        
        # Fetch or assemble simset (later: should have this done up front outside this function so that we never repeat it unless we have to)
        if (percent_completion < 100) {
            full_simset <- tryCatch({assemble.simulations.from.calibration(version = "shield",
                                                                           location = location,
                                                                           calibration.code = calibration.code,
                                                                           allow.incomplete = T)},
                                    error = function(e) {print(paste0(e, "... Skipping")); next})
        } else {
            full_simset <- tryCatch({retrieve.simulation.set(version = "shield",
                                                             location = location,
                                                             calibration.code = calibration.code,
                                                             n.sim = n_sim)},
                                    error = function(e) {print(paste0(e, "... Skipping")); next})
        }
        last20_sims <- full_simset$subset((n_sim - 20) : (n_sim - 1))
        last_sim <- full_simset$last.sim()
        
        # Give a title suffix for plots to indicate calibration code and percent completion, if needed
        title_suffix <- paste0(": ", calibration.code)
        if (percent_completion < 100)
            title_suffix <- paste0(title_suffix, " (", percent_completion, "% complete)")
        
        return(list(full_simset = full_simset,
                    last20_sims = last20_sims,
                    last_sim = last_sim,
                    title_suffix = title_suffix))
        
    }), locations)
}

#' @param create.dirs Recommended to start with this set to FALSE to make sure you're in the right working directory (jheem_analyses). Then, set to TRUE so that you can make new directories for city/calibration combinations.
#' @returns After generating plots, this function returns a vector of locations which succeeded so that you can record which failed.
create_plots_for_stage0_calibration <- function(calibration.code, simset.data, create.dirs = F) {
    
    # Will return a vector of successful locations
    successful_locations <- character(0)
    
    for (location in names(simset.data)) {
        
        # Check file path and create directories if needed
        plotting_path <- paste0(get.jheem.root.directory(), "shield/calibrationPlots/", calibration.code, "/", location, "/")
        if (!dir.exists(plotting_path)) {
            if (!create.dirs)
                stop(paste0("Error: directory for '", location, "' and '", calibration.code, "' does not exist. Check that get.jheem.root.directory() shows the right place, then try again with 'create.dirs' set to TRUE."))
            dir.create(plotting_path, recursive = T)
            print(paste0("Generating directories for '", location, "' and '", calibration.code, "'"))
        }
        
        last20_sims <- simset.data[[location]]$last20_sims
        last_sim <- simset.data[[location]]$last_sim
        title_suffix <- simset.data[[location]]$title_suffix
        
        if (is.null(last20_sims)) {
            print(paste0("No simset found for '", location, "' and '", calibration.code, "'... Skipping")); next
        }
        
        # Generate and save plots
        print(paste0("Generating stage 0 plots for '", location, "' and '", calibration.code, "'"))
        tryCatch({
            make_stage0_plots_for_location(last20_sims, last_sim, plotting_path, title.suffix = title_suffix)
            successful_locations <- c(successful_locations, location)},
                 # error = function(e) {browser()})
                 error = function(e) {print(paste0("Error generating stage 0 plots for '", location, "' and '", calibration.code, "'... Skipping"))})
    }
    
    # Return vector of successful locations
    successful_locations
}

# INTERNAL HELPERS ----
#' @title Make an Unstratified Plot
make_total_plot <- function(outcome, last20, lastsim, style.manager, plotting.path, title.suffix) {
    p <- simplot(
        last20, lastsim,
        outcomes = outcome,
        style.manager = style.manager,
        title.suffix = title.suffix
    )
    file_png  <- file.path(paste0(plotting.path , gsub("\\.", "-", outcome), ".png"))
    ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
}

#' @title Make a One-Way Stratified Plot
#' @param facet.vars Each will be used as the facet.by for one plot.
make_facet_plot <- function(outcome, last20, lastsim, facet.vars, style.manager, plotting.path, title.suffix) {
    for (facet_var in facet.vars) {
        p <- simplot(
            last20, lastsim,
            outcomes = outcome,
            facet.by = facet_var,
            style.manager = style.manager,
            title.suffix = title.suffix
        )
        file_png  <- file.path(paste0(plotting.path , gsub("\\.", "-", outcome),"_",facet_var, ".png"))
        ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    }
}

#' @title Make a Two-Way Stratified Plot
#' @param split.facet.pairs List with pairs of dimensions c(split_var, facet_var). Each pair is for one plot.
make_split_facet_plot <- function(outcome, last20, lastsim, split.facet.pairs, style.manager, plotting.path, title.suffix) {
    for (split_facet_pair in split.facet.pairs) {
        p <- simplot(
            last20, lastsim,
            outcomes = outcome,
            split.by = split_facet_pair[1],
            facet.by = split_facet_pair[2],
            style.manager = style.manager,
            title.suffix = title.suffix
        )
        file_png  <- file.path(paste0(plotting.path ,
                                      gsub("\\.", "-", outcome),
                                      "_",
                                      split_facet_pair[1],
                                      "_",
                                      split_facet_pair[2],
                                      ".png"))
        ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
    }
}

# STAGE 0 OUTCOMES ----
make_stage0_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix) {
    # POPULATION
    make_total_plot("population", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("population", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("population", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                                  c("race", "age"),
                                                                                  c("race", "sex")),
                          style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    # DEATHS
    make_total_plot("deaths", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    # FERTILITY RATE
    make_split_facet_plot("fertility.rate", last20, lastsim, split.facet.pairs = list(c("race", "age")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    # IMMIGRATION
    make_total_plot("immigration", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("immigration", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    # EMIGRATION
    make_total_plot("emigration", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("emigration", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    # PS.DIAGNOSIS
    make_total_plot("diagnosis.ps", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
}

# USAGE ----
if (1==2) {
    
    # Define style managers to use
    source.style.manager = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
    
    # Retrive and/or assemble simsets. Only need to run once per session.
    simset_data <- prepare_simsets_for_plots(calibration.code = "calib.3.22.stage0.az", locations = c("C.12580", "C.41860"), assemble.incomplete = T)
    
    # Create and save the plots. To change what plots are generated, go to the "STAGE 0 OUTCOMES" section.
    create_plots_for_stage0_calibration("calib.3.22.stage0.az", simset_data, create.dirs = T)
}

# stage 0 likelihoods:
# population (age/race/sex 0,1,2), deaths (0 currently), fertility (age/race 0,1,2), immigration (age/race/sex 0,1), emigration (age/race 0,1,2), ps.diagnosis (0)



# # STAGE1 OUTCOMES ----
# ## Totals
# for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
#     print(outcome)
#     p=simplot(
#         last20,
#         outcomes=outcome,
#         style.manager = source.style.manager
#     )
#     # Save in multiple formats (adjust width/height as needed)
#     file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome, ".png"))
#     ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
# }
# 
# # Oneway Stratification ----
# for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified","hiv.testing")){
#     for (strata in c("sex","race","age")){
#         print(outcome)
#         p=simplot(
#             last20,
#             outcomes=outcome,
#             facet.by =strata,
#             style.manager = source.style.manager
#         )
#         # Save in multiple formats (adjust width/height as needed)
#         file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_",strata, ".png"))
#         ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
#     }}
# # Two_way Stratification ----
# #@Andrew: we should only include outcomes with two way stratification data  (not hiv.testing)
# for (outcome in c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified")){
#     print(outcome)
#     p=simplot(
#         last20,
#         outcomes=outcome,
#         facet.by ="age",
#         split.by = "race",
#         style.manager = source.style.manager 
#     )
#     file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_age_race", ".png"))
#     ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
#     #
#     p=simplot(
#         last20,
#         outcomes=outcome,
#         facet.by ="age",
#         split.by = "sex",
#         style.manager = source.style.manager 
#     )
#     file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_age_sex", ".png"))
#     ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
#     #
#     p=simplot(
#         last20,
#         outcomes=outcome,
#         facet.by ="sex",
#         split.by = "race",
#         style.manager = source.style.manager 
#     )
#     file_png  <- file.path(get.jheem.root.directory(), paste0(plotting_path , outcome,"_sex_race", ".png"))
#     ggsave(file_png, plot = p, width = 8, height = 5, dpi = 300)
# }
# 
# #OTHER Plots of interest: