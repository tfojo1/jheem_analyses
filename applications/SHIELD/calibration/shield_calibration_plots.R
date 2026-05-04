# Load Required Libraries and Commoncode----
library(plotly)
library(patchwork)

# source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/calibration/shield_calibration_inspection_helpers.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")
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
        
        print(paste0("Retrieving/assembling simset for '", location, "' and '", calibration.code, "'"))
        
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
#' @param full.simset If FALSE (default), only the last twenty sims are plotted. Either way, the last sim is also plotted.
#' @returns After generating plots, this function returns a vector of locations which succeeded so that you can record which failed.
create_plots_for_calibration <- function(stage, calibration.code, simset.data, create.dirs = F, full.simset = F) {
    
    # Will return a vector of successful locations
    successful_locations <- character(0)
    
    for (location in names(simset.data)) {
        # Check file path and create directories if needed
        plotting_path <- paste0(get.jheem.root.directory(), "/shield/calibrationPlots/", calibration.code, "/", location, "/")
        if (!dir.exists(plotting_path)) {
            if (!create.dirs)
                stop(paste0("Error: directory for '", location, "' and '", calibration.code, "' does not exist. Check that get.jheem.root.directory() shows the right place, then try again with 'create.dirs' set to TRUE."))
            dir.create(plotting_path, recursive = T,showWarnings = F)
            print(paste0("Generating directories for '", location, "' and '", calibration.code, "'"))
        }
        
        last20_sims <- simset.data[[location]]$last20_sims
        last_sim <- simset.data[[location]]$last_sim
        title_suffix <- simset.data[[location]]$title_suffix
        
        if (is.null(last20_sims)) {
            print(paste0("No simset found for '", location, "' and '", calibration.code, "'... Skipping")); next
        }
        
        use_sims <- if (full.simset) simset.data[[location]]$full_simset else last20_sims
        
        # Generate and save plots
        print(paste0("Generating plots for '", location, "' and '", calibration.code, "'"))
        tryCatch({
            if (stage == 0) make_stage0_plots_for_location(use_sims, last_sim, plotting_path, title.suffix = title_suffix)
            if (stage == 1) make_stage1_plots_for_location(use_sims, last_sim, plotting_path, title.suffix = title_suffix)
            if (stage == 2) make_stage2_plots_for_location(use_sims, last_sim, plotting_path, title.suffix = title_suffix)
            successful_locations <- c(successful_locations, location)},
            error = function(e) {print(paste0("Error generating plots for '", location, "' and '", calibration.code, "'... Skipping"))})
    }
    
    # Return vector of successful locations
    successful_locations
}

#' @description Compare two calibrations side by side for a fixed set of outcomes (stage-independent).
#' @param create.dirs Recommended to start with this set to FALSE to make sure you're in the right working directory (jheem_analyses). Then, set to TRUE so that you can make new directories for city/calibration combinations.
#' @returns After generating plots, this function returns a vector of locations which succeeded so that you can record which failed.
create_plots_for_calibration_comparison <- function(calibration.code1, simset.data1,
                                                    calibration.code2, simset.data2,
                                                    create.dirs = F) {
    successful_locations <- character(0)
    
    for (location in names(simset.data1)) {
        plotting_path <- paste0(get.jheem.root.directory(),
                                "/shield/calibrationPlots/comparison/",
                                calibration.code1, "_vs_", calibration.code2,
                                "/", location, "/")
        if (!dir.exists(plotting_path)) {
            if (!create.dirs)
                stop(paste0("Error: directory for '", location, "' does not exist. ",
                            "Check that get.jheem.root.directory() shows the right place, ",
                            "then try again with 'create.dirs' set to TRUE."))
            dir.create(plotting_path, recursive = T, showWarnings = F)
            print(paste0("Generating directories for '", location, "'"))
        }
        
        last20_sims1 <- simset.data1[[location]]$last20_sims
        last_sim1    <- simset.data1[[location]]$last_sim
        last20_sims2 <- simset.data2[[location]]$last20_sims
        last_sim2    <- simset.data2[[location]]$last_sim
        title_suffix <- simset.data1[[location]]$title_suffix
        
        if (is.null(last20_sims1)) {
            print(paste0("No simset found for '", location, "' and '", calibration.code1, "'... Skipping")); next
        }
        if (is.null(last20_sims2)) {
            print(paste0("No simset found for '", location, "' and '", calibration.code2, "'... Skipping")); next
        }
        
        print(paste0("Generating comparison plots for '", location,
                     "': ", calibration.code1, " vs ", calibration.code2))
        
        tryCatch({
            make_comparison_plots_for_location(last20_sims1, last_sim1,
                                               last20_sims2, last_sim2,
                                               calib.code1 = calibration.code1,
                                               calib.code2 = calibration.code2,
                                               plotting.path = plotting_path,
                                               title.suffix = title_suffix)
            successful_locations <- c(successful_locations, location)
        },
        error = function(e) {
            print(paste0("Error generating comparison plots for '", location,
                         "': ", calibration.code1, " vs ", calibration.code2, "... Skipping"))
        })
    }
    
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

#' @title Make a Comparison Total Plot (two calibrations side by side)
make_comparison_total_plot <- function(outcome, last20_1, lastsim_1, last20_2, lastsim_2,
                                       calib.code1, calib.code2,
                                       style.manager, plotting.path, title.suffix) {
    p1 <- simplot(
        last20_1, lastsim_1,
        outcomes = outcome,
        style.manager = style.manager,
        title.suffix = paste0(": ", calib.code1),
        dimension.values = list(year = 2000:2030)
    )
    
    p2 <- simplot(
        last20_2, lastsim_2,
        outcomes = outcome,
        style.manager = style.manager,
        title.suffix = paste0(": ", calib.code2),
        dimension.values = list(year = 2000:2030)
    )
    
    combined <- p1 + p2 + plot_layout(ncol = 2)
    
    file_png <- file.path(paste0(plotting.path, gsub("\\.", "-", outcome), "_comparison.png"))
    ggsave(file_png, plot = combined, width = 20, height = 7, dpi = 300)
}

#' @title Make All Comparison Plots for a Single Location
make_comparison_plots_for_location <- function(last20_1, lastsim_1, last20_2, lastsim_2,
                                               calib.code1, calib.code2,
                                               plotting.path, title.suffix) {
    outcomes <- c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified",
                  "diagnosis.late.misclassified", "hiv.testing")
    
    for (outcome in outcomes) {
        make_comparison_total_plot(outcome,
                                   last20_1, lastsim_1,
                                   last20_2, lastsim_2,
                                   calib.code1, calib.code2,
                                   style.manager = source.style.manager,
                                   plotting.path = plotting.path,
                                   title.suffix = title.suffix)
    }
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
    make_facet_plot("deaths", last20, lastsim, facet.vars = c("sex", "race", "age"), style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    make_split_facet_plot("deaths", last20, lastsim, split.facet.pairs = list(c("sex", "age"),
                                                                              c("race", "age"),
                                                                              c("race", "sex")),
                          style.manager = source.style.manager, plotting.path = plotting.path, title.suffix = title.suffix)
    
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

# COMPARE by LOCATION ----#' @title Create Multi-Panel Location Comparison Plot
create_multipanel_location_comparison <- function(outcome,
                                                  simset.data,
                                                  calibration.code,
                                                  facet.by = NULL,
                                                  split.by = NULL,
                                                  style.manager = NULL,
                                                  summary.type = "median.and.interval",
                                                  plot.which="sim.and.data",
                                                  nrow = 2,
                                                  ncol = 5,
                                                  years = 2000:2030,
                                                  width = 24,
                                                  height = 10,
                                                  create.dirs = FALSE,
                                                  use.full.simset = FALSE) {
    
    if (is.null(style.manager)) {
        style.manager <- create.style.manager(shape.data.by = "source", color.data.by = "stratum")
    }
    
    # Set up save path
    plotting_path <- paste0(get.jheem.root.directory(), 
                            "/shield/calibrationPlots/", 
                            calibration.code, 
                            "/compare_by_location/")
    
    if (!dir.exists(plotting_path)) {
        if (!create.dirs) {
            stop(paste0("Error: directory '", plotting_path, "' does not exist. ",
                        "Check that get.jheem.root.directory() shows the right place, ",
                        "then try again with 'create.dirs' set to TRUE."))
        }
        dir.create(plotting_path, recursive = TRUE, showWarnings = FALSE)
        print(paste0("Created directory: ", plotting_path))
    }
    
    # Get valid locations (those with non-null simset data)
    valid_locations <- names(simset.data)[!sapply(simset.data, is.null)]
    
    if (length(valid_locations) == 0) {
        stop("No valid simset data found for any location")
    }
    
    # Create individual plots for each location
    plot_list <- lapply(valid_locations, function(location) {
        
        if (use.full.simset) {
            sims <- simset.data[[location]]$full_simset
        } else {
            sims <- simset.data[[location]]$last20_sims
        }
        last_sim <- simset.data[[location]]$last_sim
        
        # Get a nice location name for the title
        location_name <- get.location.name(location)
        
        # Build the simplot call based on whether facet.by and split.by are provided
        if (is.null(facet.by) && is.null(split.by)) {
            p <- simplot(
                sims,  
                outcomes = outcome,
                style.manager = style.manager,
                summary.type = summary.type,
                plot.which=plot.which,
                dimension.values = list(year = years)
            )
        } else if (is.null(split.by)) {
            p <- simplot(
                sims,  
                outcomes = outcome,
                facet.by = facet.by,
                style.manager = style.manager,
                summary.type = summary.type,
                plot.which=plot.which,
                dimension.values = list(year = years)
            )
        } else if (is.null(facet.by)) {
            p <- simplot(
                sims,  
                outcomes = outcome,
                split.by = split.by,
                style.manager = style.manager,
                summary.type = summary.type,
                plot.which=plot.which,
                dimension.values = list(year = years)
            )
        } else {
            p <- simplot(
                sims,  
                outcomes = outcome,
                split.by = split.by,
                facet.by = facet.by,
                style.manager = style.manager,
                summary.type = summary.type,
                plot.which=plot.which,
                dimension.values = list(year = years)
            )
        }
        
        p <- p + 
            ggtitle(location_name) +
            theme(plot.title = element_text(size = 10, hjust = 0.5))
        
        return(p)
    })
    
    # Create a clean version of plot.which for filename (replace dots with dashes)
    plot_which_suffix <- if (plot.which == "sim.and.data") "" else "_simOnly"
    
    # Build title and filename based on stratification (now including plot.which)
    if (is.null(facet.by) && is.null(split.by)) {
        title_text <- paste0("Comparison of '", outcome, "' Across Locations")
        filename <- paste0(gsub("\\.", "-", outcome), "_", plot_which_suffix, ".png")
    } else if (is.null(split.by)) {
        title_text <- paste0("Comparison of '", outcome, "' by ", facet.by, " Across Locations")
        filename <- paste0(gsub("\\.", "-", outcome), "_", facet.by, "_", plot_which_suffix, ".png")
    } else if (is.null(facet.by)) {
        title_text <- paste0("Comparison of '", outcome, "' by ", split.by, " Across Locations")
        filename <- paste0(gsub("\\.", "-", outcome), "_", split.by, "_", plot_which_suffix, ".png")
    } else {
        title_text <- paste0("Comparison of '", outcome, "' by ", split.by, " and ", facet.by, " Across Locations")
        filename <- paste0(gsub("\\.", "-", outcome), "_", split.by, "_", facet.by, "_", plot_which_suffix, ".png")
    }
    
    # Combine plots using patchwork
    combined_plot <- wrap_plots(plot_list, nrow = nrow, ncol = ncol) +
        plot_annotation(
            title = title_text,
            theme = theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
        )
    
    # Save the plot
    save_path <- file.path(plotting_path, filename)
    ggsave(save_path, plot = combined_plot, width = width, height = height, dpi = 300)
    print(paste0("Plot saved to: ", save_path))
    
    return(invisible(combined_plot))
}

# USAGE ----

# --- Single calibration plots ---
if (1==2) {
    
    stage=2
    calibname="calib.4.24.stage2.az"
    
    # Define style managers to use.
    source.style.manager = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
    
    # Retrieve and/or assemble simsets. Only need to run once per session.
    simset_data <- prepare_simsets_for_plots(calibration.code = calibname, 
                                             locations =    names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N","S","PH")], 
                                             assemble.incomplete = F)
    
    # Create and save the plots.
    x <- create_plots_for_calibration(stage, calibname, simset_data, create.dirs = T)
}

# --- Comparison plots (stage-independent) ---
if (1==2) {
    
    calibname1="calib.4.8.stage2.az"
    calibname2="calib.4.6.stage2.az"
    
    # Define style managers to use.
    source.style.manager = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
    
    # Retrieve and/or assemble simsets. Only need to run once per session.
    simset_data1 <- prepare_simsets_for_plots(calibration.code = calibname1, 
                                              names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")], 
                                              assemble.incomplete = F)
    simset_data2 <- prepare_simsets_for_plots(calibration.code = calibname2, 
                                              names(msa_var_names)[msa_var_names %in% c("B","M","A","H","C","L","N")], 
                                              assemble.incomplete = F)
    
    # Create and save the comparison plots.
    x <- create_plots_for_calibration_comparison(calibname1, simset_data1, calibname2, simset_data2, create.dirs = T)
}

# --- Multi-panel location comparison plots ---
if (1==2) {
    
    calibname <- "calib.4.24.stage2.az"
    # plot.which = c("sim.and.data", "sim.only")[1],
    # summary.type = c("individual.simulation", "mean.and.interval",
    #                  "median.and.interval")[1],
    # Retrieve and/or assemble simsets (same as before)
    simset_data <- prepare_simsets_for_plots(
        calibration.code = calibname, 
        locations = names(msa_var_names)[msa_var_names %in% c("P","B","M","A","H","C","L","N","S","PH")], 
        assemble.incomplete = F
    )
    
    # Example 1: Create multi-panel plots for selected outputs
    # outcomes <- c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified", 
    #               "diagnosis.late.misclassified", "hiv.testing")
    outcomes <- c("sti.screening")
    for( o in outcomes){
        create_multipanel_location_comparison(
            outcome = o,
            simset.data = simset_data,
            split.by = "sex",
            plot.which="sim.only", #"sim.and.data"
            calibration.code = calibname,
            summary.type = "mean.and.interval",
            # summary.type = "individual.simulation",
            nrow = 2, ncol = 5,
            # width = 36, height = 16,
            
            create.dirs = TRUE
        )
    }
    
}