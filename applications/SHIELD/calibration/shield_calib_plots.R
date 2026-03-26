
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
        
        # Fetch or assemble simset
        if (percent_completion < 100) {
            full_simset <- tryCatch({assemble.simulations.from.calibration(version = "shield",
                                                                           location = location,
                                                                           calibration.code = calibration.code,
                                                                           allow.incomplete = T)},
                                    error = function(e) {print(paste0(e, "... Skipping")); return(NULL)})
            n_sim_effective <- full_simset$n.sim
        } else {
            full_simset <- tryCatch({retrieve.simulation.set(version = "shield",
                                                             location = location,
                                                             calibration.code = calibration.code,
                                                             n.sim = n_sim)},
                                    error = function(e) {print(paste0(e, "... Skipping")); return(NULL)})
            n_sim_effective <- n_sim
        }
        if (is.null(full_simset)) return(NULL)
        last20_sims <- full_simset$subset((n_sim_effective - 20) : (n_sim_effective - 1))
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

#' @param stage 0, 1, or 2
#' @param create.dirs Recommended to start with this set to FALSE to make sure you're in the right working directory (jheem_analyses). Then, set to TRUE so that you can make new directories for city/calibration combinations.
#' @returns After generating plots, this function returns a vector of locations which succeeded so that you can record which failed.
create_plots_for_calibration <- function(stage, calibration.code, simset.data, create.dirs = F) {
    
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
        print(paste0("Generating plots for '", location, "' and '", calibration.code, "'"))
        tryCatch({
            if (stage == 0) make_stage0_plots_for_location(last20_sims, last_sim, plotting_path, title.suffix = title_suffix)
            if (stage == 1) make_stage1_plots_for_location(last20_sims, last_sim, plotting_path, title.suffix = title_suffix)
            if (stage == 2) make_stage2_plots_for_location(last20_sims, last_sim, plotting_path, title.suffix = title_suffix)
            successful_locations <- c(successful_locations, location)},
            # error = function(e) {browser()})
            error = function(e) {print(paste0("Error generating plots for '", location, "' and '", calibration.code, "'... Skipping"))})
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
        title.suffix = title.suffix,
        dimension.values = list(year=2000:2030)
    )
    file_png  <- file.path(paste0(plotting.path , gsub("\\.", "-", outcome), ".png"))
    ggsave(file_png, plot = p, width = 12, height = 7, dpi = 300)
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
            title.suffix = title.suffix,
            dimension.values = list(year=2000:2030)
        )
        file_png  <- file.path(paste0(plotting.path , gsub("\\.", "-", outcome),"_",facet_var, ".png"))
        ggsave(file_png, plot = p, width = 12, height = 7, dpi = 300)
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
            title.suffix = title.suffix,
            dimension.values = list(year=2000:2030)
        )
        file_png  <- file.path(paste0(plotting.path ,
                                      gsub("\\.", "-", outcome),
                                      "_",
                                      split_facet_pair[1],
                                      "_",
                                      split_facet_pair[2],
                                      ".png"))
        ggsave(file_png, plot = p, width = 12, height = 7, dpi = 300)
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

# STAGE 1 OUTCOMES ----
make_stage1_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix) {
    # TOTAL DIAGNOSIS
    make_total_plot("diagnosis.total", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.total", last20, lastsim, facet.vars = c("sex", "race"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.total", last20, lastsim, split.facet.pairs = list(c("sex", "race")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # PS.DIAGNOSIS
    make_total_plot("diagnosis.ps", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.ps", last20, lastsim, facet.vars = c("sex", "race"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.ps", last20, lastsim, split.facet.pairs = list(c("sex", "race")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # EARLY LATENT
    make_total_plot("diagnosis.el.misclassified", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.el.misclassified", last20, lastsim, facet.vars = c("sex", "race"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.el.misclassified", last20, lastsim, split.facet.pairs = list(c("sex", "race")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # LATE LATENT
    make_total_plot("diagnosis.late.misclassified", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.late.misclassified", last20, lastsim, facet.vars = c("sex", "race"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.late.misclassified", last20, lastsim, split.facet.pairs = list(c("sex", "race")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # PROPORTION TESTED
    make_total_plot("hiv.testing", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("hiv.testing", last20, lastsim, facet.vars = c("sex", "race"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
}
# STAGE 2 OUTCOMES ----
make_stage2_plots_for_location <- function(last20, lastsim, plotting.path, title.suffix) {
    # TOTAL DIAGNOSIS
    make_total_plot("diagnosis.total", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.total", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.total", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                                       c("race", "age"),
                                                                                       c("race", "sex")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # PS.DIAGNOSIS
    make_total_plot("diagnosis.ps", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.ps", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.ps", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                                    c("race", "age"),
                                                                                    c("race", "sex")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # EARLY LATENT
    make_total_plot("diagnosis.el.misclassified", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.el.misclassified", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.el.misclassified", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                                                  c("race", "age"),
                                                                                                  c("race", "sex")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # LATE LATENT
    make_total_plot("diagnosis.late.misclassified", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("diagnosis.late.misclassified", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("diagnosis.late.misclassified", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                                                    c("race", "age"),
                                                                                                    c("race", "sex")), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
    # PROPORTION TESTED
    make_total_plot("hiv.testing", last20, lastsim, style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_facet_plot("hiv.testing", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
}

# USAGE ----
if (1==2) {
    
    # Define style managers to use. You'll need to reference them in the "STAGE 0 OUTCOMES" section.
    source.style.manager = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
    
    # Retrieve and/or assemble simsets. Only need to run once per session.
    simset_data <- prepare_simsets_for_plots(calibration.code = "calib.3.23.stage2.az", MSAS.OF.INTEREST, assemble.incomplete = F)
    
    # Create and save the plots. To change which plots are generated, go to the "STAGE 0 OUTCOMES" section.
    x <- create_plots_for_calibration(2, "calib.3.23.stage2.az", simset_data, create.dirs = T)
}