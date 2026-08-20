# To do:
# allow "updating" an object by passing the data into a new one and adopting new methods.


if (1==2) {
    source("../jheem_analyses/applications/SHIELD/intervention/intervention_definitions.R")
    xx <- load_simsets_from_collection(version = "shield",
                                       calibration.code = "calib.4.24.stage2.az",
                                       locations = c("C.12580", "C.35620"),
                                       interventions = c("noint", "doxypep.10"),
                                       n.sim = 300)
    yy <- update_simset_collection_data(xx)
    
    yy$make_plot_one_outcome_one_location(outcome = "diagnosis.ps",intervention.code = "doxypep.10",
                                          style.manager = create.style.manager(color.sim.by="simset",
                                                                               alpha.line = 0.8))
    
}

#' @details Should have a new argument that specifies which is the "null" intervention
load_simsets_from_collection <- function(version,
                                         calibration.code,
                                         locations,
                                         interventions,
                                         n.sim,
                                         sub.version = NULL,
                                         default.style.manager = get.default.style.manager(),
                                         root.dir = get.jheem.root.directory("Cannot create intervention plotting collection: ")) {
    SIMSET.COLLECTION.DATA$new(version = version,
                               calibration.code = calibration.code,
                               locations = locations,
                               interventions = interventions,
                               n.sim = n.sim,
                               sub.version = sub.version,
                               default.style.manager = default.style.manager,
                               root.dir = root.dir)
}

update_simset_collection_data <- function(old_object) {
    if (!R6::is.R6(old_object) || !is(old_object, "simset.collection.data"))
        stop(paste0("Can't update_simset_collection: 'old_object' has to be a 'simset.collection.data' object"))
    SIMSET.COLLECTION.DATA$new(version = old_object$version,
                               calibration.code = old_object$calibration.code,
                               locations = old_object$locations,
                               interventions = old_object$intervention.codes,
                               n.sim = old_object$n.sim,
                               sub.version = old_object$sub.version,
                               root.dir = old_object$root.dir,
                               default.style.manager = old_object$default.style.manager,
                               data = old_object$data)
}

SIMSET.COLLECTION.DATA <- R6::R6Class(
    "simset.collection.data",
    
    public = list(
        
        initialize = function(version,
                              sub.version,
                              calibration.code,
                              n.sim,
                              locations,
                              interventions,
                              default.style.manager,
                              root.dir = get.jheem.root.directory("Cannot create intervention plotting collection: "),
                              verbose = F,
                              stop.for.errors = F,
                              data = NULL) {
            
            #-- Validate arguments --#
            error.prefix <- "Cannot create intervention plotting collection: "
            
            # version
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # sub-version
            if (is.null(sub.version))
            {}
            else if (!is.character(sub.version) || length(sub.version)!=1 || is.na(sub.version))
                stop(paste0(error.prefix, "'sub.version' must be either NULL or a single, non-NA character value"))
            
            if (!is.character(version) || length(version)!=1 || is.na(version))
                stop(paste0(error.prefix, "'version' must be a single, non-NA character value"))
            
            # calibration.code
            if (is.null(calibration.code))
            {}
            else if (!is.character(calibration.code) || length(calibration.code)!=1 || is.na(calibration.code))
                stop(paste0(error.prefix, "'calibration.code' must be either NULL or a single, non-NA character value"))
            
            # n.sim
            if (!is.numeric(n.sim) || length(n.sim)!=1 || is.na(n.sim) || n.sim<=0 || round(n.sim)!=n.sim)
                stop(paste0(error.prefix, "'n.sim' must be a single, non-NA, positive integer value"))
            
            # locations
            if (!is.character(locations) || length(locations)==0 || any(is.na(locations)))
                stop(paste0(error.prefix, "'locations' must be a non-empty character vector with no NA values"))
            
            # interventions
            if (!is.character(interventions) || length(interventions)==0 || any(is.na(interventions)))
                stop(paste0(error.prefix, "'interventions' actually has to be just the intervention codes, because that's simpler to code"))
            intervention.codes <- interventions
            
            # Default style manager (TO DO)
            if (!R6::is.R6(default.style.manager) || !is(default.style.manager, "jheem.style.manager"))
                stop(paste0(error.prefix, "'default.style.manager' must be a JHEEM style manager"))
            
            #-- Store variables --#
            
            private$i.version <- version
            private$i.sub.version <- sub.version
            private$i.calibration.code <- calibration.code
            private$i.locations <- locations
            private$i.intervention.codes <- intervention.codes
            private$i.n.sim <- n.sim
            private$i.root.dir <- root.dir
            private$i.default.style.manager <- default.style.manager
            
            private$i.plotting.path <- paste0(private$i.root.dir,
                                              "/", version, "/",
                                              "interventionPlots/", 
                                              calibration.code,
                                              "/")
            
            # if (!is.null(default.style.manager)) {
            #     
            #     private$i.default.style.manager <- create.style.manager(
            #         color.sim.by = "simset"
            #     )
            # }
            
            #-- Load simsets --#
            
            # Data has structure list locations > interventions > simset
            if (!is.null(data)) private$i.data <- data
            else {
                private$i.data <- setNames(lapply(private$i.locations, function(loc) {
                    
                    setNames(lapply(private$i.intervention.codes, function(int.code) {
                        
                        rv <- private$do.get.simset(location = loc,
                                                    intervention.code = int.code,
                                                    verbose = verbose,
                                                    verbose.prefix = " ",
                                                    stop.for.errors = stop.for.errors)
                        if (is.null(rv))
                            warning(paste0("no simset was found for '", loc, "' and intervention '", int.code, "'"))
                        rv
                    }), private$i.intervention.codes)
                }), private$i.locations)
            }
            invisible(self)
            
        },
        
        # -- Plotting methods: These are what we'll add to ----
        #' @param locations If NULL, makes one plot for each location.
        make_plot_one_outcome_one_location = function(outcome,
                                                      intervention.code,
                                                      null.intervention.code = "noint",
                                                      locations=private$i.locations,
                                                      split.by=NULL,
                                                      facet.by=NULL,
                                                      summary.type = "median.and.interval",
                                                      style.manager = private$i.default.style.manager,
                                                      dimension.values = list(),
                                                      return.plot = F) {
            
            # Validate args
            # browser()
            
            # Generate plot
            # TO DO: Need to correct the names on these and add descriptive title
            for (location in locations) {
                plot <- simplot(private$i.data[[location]][[intervention.code]],
                                private$i.data[[location]][[null.intervention.code]],
                                outcomes = outcome,
                                split.by = split.by,
                                facet.by = facet.by,
                                summary.type = summary.type,
                                style.manager = style.manager,
                                dimension.values = dimension.values,
                                simset.names = c(intervention.code, null.intervention.code))
                
                # Save the plot
                browser()
                stop("Still working")
                plotting_path <- paste0(private$i.plotting.path, "/temp/", location, "_", intervention.code, "")
                save_path <- file.path(plotting_path, filename)
                ggsave(save_path, plot = plot, width = width, height = height, dpi = 300)
                print(paste0("Plot saved to: ", save_path))
            }
            
            return(invisible(plot))
            
        },
        
        check = function() {
            browser()
        }
    ),
    
    # -- Active bindings ----
    active = list(
        
        version = function(value)
        {
            if (missing(value))
                private$i.version
            else
                stop("Cannot modify 'version' for an intervention.plotting.collection - it is read-only")
        },
        
        sub.version = function(value)
        {
            if (missing(value))
                private$i.sub.version
            else
                stop("Cannot modify 'sub.version' for an intervention.plotting.collection - it is read-only")
        },
        
        calibration.code = function(value)
        {
            if (missing(value))
                private$i.calibration.code
            else
                stop("Cannot modify 'calibration.code' for an intervention.plotting.collection - it is read-only")
        },
        
        n.sim = function(value)
        {
            if (missing(value))
                private$i.n.sim
            else
                stop("Cannot modify 'n.sim' for an intervention.plotting.collection - it is read-only")
        },
        
        locations = function(value)
        {
            if (missing(value))
                private$i.locations
            else
                stop("Cannot modify 'locations' for an intervention.plotting.collection - they are read-only")
        },
        
        intervention.codes = function(value)
        {
            if (missing(value))
                private$i.intervention.codes
            else
                stop("Cannot modify 'intervention.codes' for an intervention.plotting.collection - it is read-only")
        },
        
        data = function(value)
        {
            if (missing(value))
                private$i.data
            else
                stop("Cannot modify 'data' for an intervention.plotting.collection - it is read-only")
        },
        
        plotting.path = function(value)
        {
            if (missing(value))
                private$i.plotting.path
            else
                stop("Cannot modify 'plotting.path' for an intervention.plotting.collection - it is read-only")
        },
        
        default.style.manager = function(value)
        {
            if (missing(value))
                private$i.default.style.manager
            else
                stop("Cannot modify 'default.style.manager' for an intervention.plotting.collection - it is read-only")
        },
        
        root.dir = function(value)
        {
            if (missing(value))
                private$i.root.dir
            else
                stop("Cannot modify 'root.dir' for an intervention.plotting.collection - it is read-only")
        }
        
        
    ),
    
    # -- Private params and methods ----
    private = list(
        i.version = NULL,
        i.sub.version = NULL,
        i.calibration.code = NULL,
        i.intervention.codes = NULL,
        i.locations = NULL,
        i.n.sim = NULL,
        i.plotting.path = NULL,
        i.root.dir = NULL,
        i.default.style.manager = NULL,
        
        i.data = NULL,
        
        #--More or less copied from JHEEM.FILE.BASED.SIMSET.COLLECTION--
        do.get.simset = function(location, intervention.code, verbose, verbose.prefix, stop.for.errors)
        {
            file = private$do.get.simset.file(location = location,
                                              intervention.code = intervention.code,
                                              error.prefix = 'Cannot load simset in collection: ')
            
            if (!file.exists(file))
            {
                if (stop.for.errors)
                {
                    if (verbose)
                        cat("\n")
                    
                    stop(paste0("Cannot load simset in collection: no simset has been saved at ", file))
                }
                else
                    NULL
            }
            else
            {
                if (verbose)
                    cat(verbose.prefix, "Loading ", 
                        tail(strsplit(file, "/")[[1]],1),
                        '...', sep='')
                
                simset = load.simulation.set(file) # From AZ: Does this need to be "retrieve.simulation.set"? Load is faster but doesn't work on updates.
                
                if (verbose)
                    cat("Done\n")
                
                simset
            }
        },
        
        do.get.simset.file = function(location, intervention.code, error.prefix)
        {
            get.simset.filename(version = private$i.version,
                                sub.version = private$i.sub.version,
                                calibration.code = private$i.calibration.code,
                                n.sim = private$i.n.sim,
                                location = location,
                                intervention.code = intervention.code,
                                include.path = T,
                                root.dir = private$i.root.dir,
                                error.prefix = error.prefix)
        }
        
    )
)