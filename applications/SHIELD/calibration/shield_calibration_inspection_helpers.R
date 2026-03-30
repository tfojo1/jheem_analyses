# Way to systematically create simset variables to inspect


#' @description Creates variables for simsets, last sims, and last 20 sims for each location and calibration code
#' combination provided. Affixes will be created. For example, the last sim for Baltimore for a calibration with
#' a prefix provided in the calibration.codes argument as "k" will be titled "lastBk". Baltimore is called "B"
#' because that is what the mapping in 'msa_var_names' says it is.
#' For each location/calibration code combination, it will create three simsets in the
#' global environment:
#'  A full simset called "simset<affix>",
#'  A last sim called "last<affix>", and
#'  A simset of the last 20 sims called "<affix>20.
#' @param locations Cities to make simset variables for
#' @param calibration.codes A vector with prefixes to use in naming as the vector names. Can use "" as a blank.
#' @param n.sim Either a number that applies to all calibration codes, or a vector named by calibration code
assign_simset_vars <- function(locations,
                               calibration.codes,
                               n.sim = 400,
                               version="shield",
                               assemble.if.incomplete = T,
                               sim.id="") {
    
    error.prefix <- "Error assigning simset vars: "
    
    # If supply > 1 calibration code, they need to have different names
    if (length(calibration.codes) > 1) {
        if (is.null(names(calibration.codes)) ||
            any(is.na(names(calibration.codes))) ||
            any(duplicated(names(calibration.codes))))
            stop(paste0(error.prefix, "Must assign unique names to calibration.codes vector if using > 1 code"))
    } else if (is.null(names(calibration.codes))) {
        # If 1 code with no name, default to blank ""
        names(calibration.codes) <- ""
    }
    
    
    for (j in seq_along(calibration.codes)) {
        calib_progress <- get.calibration.progress(version = version,
                                                   locations = locations,
                                                   calibration.code = calibration.codes[j])
        for (i in seq_along(locations)) {
            
            if (!(locations[i] %in% names(msa_var_names))) {
                print(paste0("Skipping '", locations[i], "': MSA name not found in list for affixes"))
                next
            }
            
            print(paste0("On '", locations[i],"' for calibration '", calibration.codes[j], "'"))
            
            if (is.na(calib_progress[locations[i],1])) {
                print(paste0("Skipping '", locations[i], "' for calibration '", calibration.codes[j], "' -- no data"))
                next
            }
            
            tmp <- NULL
            # Assemble if necessary
            if (calib_progress[locations[i],1] != 100) {
                if (assemble.if.incomplete) {
                    tmp <- tryCatch({assemble.simulations.from.calibration(version = version,
                                                                           location = locations[i],
                                                                           calibration.code = calibration.codes[j],
                                                                           allow.incomplete = T)},
                                    error = function(e) {
                                        print(paste0("Skipping '", locations[i], "' for calibration '", calibration.codes[j], "' -- error assembling simulations from calibration"))
                                    })
                } else {
                    print(paste0("Skipping '", locations[i], "' for calibration '", calibration.codes[j], "' -- incomplete"))
                    next
                }
            } else {
                if (length(n.sim) == 1) n_sim_to_use <- n.sim else n.sim[calibration.codes[j]]
                tmp <- tryCatch({retrieve.simulation.set(version = version,
                                                         location = locations[i],
                                                         calibration.code = calibration.codes[j],
                                                         n.sim = n_sim_to_use)},
                                error = function(e) {
                                    print(paste0("Skipping '", locations[i], "' for calibration '", calibration.codes[j], "' -- error retrieving simset"))
                                })
            }
            
            if (is.null(tmp)) next
           
            affix <- paste0(msa_var_names[locations[i]], names(calibration.codes)[j],sim.id)
            
            # Make whole simset
            assign(x = paste0("simset", affix),
                   value = tmp,
                   envir = .GlobalEnv)
            
            # Make last sim
            assign(x = paste0("last", affix),
                   value = tmp$last.sim(),
                   envir = .GlobalEnv)
            
            # Make last 20 sim
            assign(x = paste0(affix, "20"),
                   value = tmp$subset((tmp$n.sim - 19) : tmp$n.sim),
                   envir = .GlobalEnv)
        }
    }
}

#' @description Used to preset simset affixes based on location.
#' So Philadelphia simsets always have "PH" in their names.
#' These affixes are short to make them as quick as possible to type!
msa_var_names <- c(
    N = "C.35620",
    M = "C.33100",
    L = "C.31080",
    A = "C.12060",
    H = "C.26420",
    D = "C.19100",
    C = "C.16980",
    DC = "C.47900",
    PH = "C.37980",
    O = "C.36740",
    SF = "C.41860",
    P = "C.38060",
    TM = "C.45300",
    R = "C.40140",
    DT = "C.19820",
    B = "C.12580",
    V = "C.29820",
    BO = "C.14460",
    SD = "C.41740",
    CT = "C.16740",
    SA = "C.41700",
    J = "C.27260",
    NO = "C.35380",
    ME = "C.32820",
    S = "C.42660",
    AU = "C.12420",
    IN = "C.26900",
    CI = "C.17140",
    CO = "C.18140",
    BR = "C.12940",
    SC = "C.40900",
    CD = "C.17460",
    SL = "C.41180",
    DV = "C.19740",
    PD = "C.38900",
    BI = "C.13820",
    MO = "C.33660",
    JN = "C.27140"
)
# Actually, flip it inside out
msa_var_names <- setNames(names(msa_var_names), msa_var_names)

# Test
if (1 == 2) {
    rm(simsetBk, lastBk, Bk20)
    assign_simset_vars("C.12580",
                       calibration.codes = c(k = "calib.2.19.stage0.az"))
    rm(simsetDC, lastDC, DC20,
       simsetA, lastA, A20,
       simsetB, lastB, B20)
    assign_simset_vars(c("C.47900", "C.12060", "C.12580"),
                       calibration.codes = "calib.3.10.stage0.az")
}

stg0simplot <- function(sim1,
                        sim2= NULL,
                        sim3= NULL,
                        split.by=NULL,
                        outcomes = c("population", "diagnosis.ps"),
                        facet.by=NULL,
                        dimension.values = list(year=1990:2035),
                        ...) {
    if (is.null(sim2) && is.null(sim3))
        simplot(sim1, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
    else if (is.null(sim3))
        simplot(sim1, sim2, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
    else
        simplot(sim1, sim2, sim3, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
}

stg1simplot <- function(sim1,
                        sim2= NULL,
                        sim3= NULL,
                        split.by=NULL,
                        outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified", "diagnosis.ll.misclassified", "hiv.testing", "sti.screening"),
                        facet.by=NULL,
                        dimension.values = list(year=1990:2035),
                        ...) {
    if (is.null(sim2) && is.null(sim3))
        simplot(sim1, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
    else if (is.null(sim3))
        simplot(sim1, sim2, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
    else
        simplot(sim1, sim2, sim3, outcomes, split.by=split.by, facet.by=facet.by, dimension.values=dimension.values, ...)
}