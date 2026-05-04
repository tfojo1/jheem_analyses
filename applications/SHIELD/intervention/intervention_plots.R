
#' @param just.last.twenty If false, will do the whole simset, which takes longer.
#' @value A list for which each element includes the intervention simset and null simset for a city.
get_intervention_simsets <- function(simset.data, intervention, just.last.twenty=T) {
    setNames(lapply(cities, function(city) {
        browser()
        if (is.null(simset.data[[city]])) {
            print(paste0("No simset found for '", city, "': Skipping"))
            return(NULL)
        }
        used_simset <- if (just.last.twenty) simset.data[[city]]$last20_sims else simset.data[[city]]$full_simset
        
        print(paste0("Running intervention for '", city, "'..."))
        int_simset <- intervention$run(used_simset, start.year = 2020, end.year = 2035)
        
        print(paste0("Running null intervention for '", city, "'..."))
        null_simset <- get.null.intervention()$run(used_simset, start.year = 2020, end.year = 2035)
        print(paste0("Done with '", city, "'"))
        list(int_simset=int_simset,
             null_simset=null_simset)
        
    }), cities)}


# Visualize results ----
create_intervention_plots <- function(int.sim.data, create.dirs = F) {
    
    for (city in names(int.sim.data)) {
        
        if (is.null(int.sim.data[[city]]$int_simset)) next
        
        plotting_path <- paste0(get.jheem.root.directory(), "/shield/interventionPlots/", CALIB, "/", city, "/")
        if (!dir.exists(plotting_path)) {
            if (!create.dirs)
                stop(paste0("Error: directory for '", city, "' and '", CALIB, "' does not exist. Check that get.jheem.root.directory() shows the right place, then try again with 'create.dirs' set to TRUE."))
            dir.create(plotting_path, recursive = T,showWarnings = F)
            print(paste0("Generating directories for '", city, "' and '", CALIB, "'"))
        }
        
        # Early example
        plot <- simplot(int.sim.data[[city]]$int_simset,
                        int.sim.data[[city]]$null_simset,
                        "diagnosis.ps",
                        summary.type = "median.and.interval",
                        style.manager = create.style.manager(color.sim.by = "simset"))
        file_png  <- file.path(paste0(plotting_path ,"diagnosis_ps.png"))
        ggsave(file_png, plot = plot, width = 12, height = 7, dpi = 300)
        
    }
    print("Done creating intervention plots")
}

create_intervention_plots(int_sims, create.dirs = T)
