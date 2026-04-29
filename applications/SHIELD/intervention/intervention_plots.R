source("../jheem_analyses/applications/SHIELD/calibration/shield_calibration_plots.R")

# Intervention code----
# Single target population for all MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population') #MSM?

# Timing
DOXY.START <- 2022 + 9/12       # Oct 1, 2022
DOXY.END   <- 2030      #  Jan 1, 2030



# DOXY-PEP EFFICATY (Studies report: RR: Rate Ratio of incident syphilis cases in doxy vs no-doxy arms per person-time)
# we have pooled estiamtes from clinical trials to estimate the meanlog and sdlog
# Draw RR samples from the final lognormal distribution to use in the model
# log(HR) ~ Normal(mean,sd) >>> HR=lognormal(meanlog,sdlog)
draw_rr_lnorm <- function(n, 
                          rr_meanlog,rr_sdlog, # mean and sd log for a lognormal dist
                          cap_at_one = TRUE) {
    rr_samples <- rlnorm(n, meanlog = rr_meanlog, sdlog = rr_sdlog) 
    #
    if (cap_at_one) rr_samples <- pmin(rr_samples, 1)
    rr_samples
}

# generate 1000 values of Doxy effectiveness
rr_samples <- draw_rr_lnorm(n = 1000, rr_meanlog = -1.540424,rr_sdlog =0.2510223,cap_at_one = T )
effectiveness_samples=1-rr_samples

# draw_effectiveness_uniform<-function(n){
#     persistence_samples = runif(n,1,1)
#     persistence_samples
# }
# rr_samples<-draw_effectiveness_uniform(1000)
# effectiveness_samples=rr_samples


# PERSISTANCE and DISCONTINUATION RATE
# P: proportion of population, taking Doxy at the end of the year
# Discontinuation rate= -log(Peristance)

draw_persistance_uniform<-function(n){
    persistence_samples = runif(n,1,1)
    persistence_samples
}
persistence_samples<-draw_persistance_uniform(10000)
discont_rate_samples = -log(persistence_samples)

# Putting them together
DOXY.PARAMS <- rbind(effectiveness_samples,discont_rate_samples)
rownames(DOXY.PARAMS) <- c("doxy.effectiveness","doxy.discontinuationRate")

uptake.effect =  create.intervention.effect(
    quantity.name    = "doxy.uptake",
    start.time       = DOXY.START,
    effect.values    = 0.10,
    times            = DOXY.END, # when scale up ends
    scale            = "proportion",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)
clear.interventions() 

uptake_intervention <- create.intervention(
    uptake.effect,
    parameters = DOXY.PARAMS,
    WHOLE.POPULATION, 
    code = "doxyPEP.10"
)
no_intervention = get.null.intervention()


cities <- SHIELD.EIGHT.MSAS
CALIB <- "calib.4.22.stage2.WO.fut"

# Get simsets using the shield_calibration_plots code ----
simset_data <- prepare_simsets_for_plots(CALIB, cities)

# Run intervention ----

#' @param just.last.twenty If false, will do the whole simset, which takes longer.
#' @value A list for which each element includes the intervention simset and null simset for a city.
get_intervention_simsets <- function(simset.data, intervention, just.last.twenty=T) {
    setNames(lapply(cities, function(city) {
        
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

int_sims <- get_intervention_simsets(simset_data, uptake_intervention)

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
