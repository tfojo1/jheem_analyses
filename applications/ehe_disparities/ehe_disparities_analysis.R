#Project: EHE Disparities
#Program: Run test interventions and output preliminary results.
#Code by Todd Fojo, Melissa Schnure & Lauren Zalla

#Note: this code has been run on fully calibrated simsets for 2 individual MSAs (Baltimore & Atlanta)

#######

### Source model specification, define interventions and parameters
source("applications/EHE/ehe_specification.R")
source("../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R")

CALIBRATION.CODE= "full.with.aids" #either "full.with.aids" or "init.transmission.ehe"
LOCATIONS=c("C.12580","C.12060") #Baltimore & Atlanta
INTERVENTIONS=c("noint", "testint", "fullint")


### Load calibrated simset for each individual MSA
load("../jheem_analyses/applications/ehe_disparities/simset_2024-05-16_C.12580.Rdata") # updated file 
simset$save()

load("../jheem_analyses/applications/ehe_disparities/simset_2024-05-19_C.12060.Rdata")
simset$save()

# this line takes ~5 min to run; don't need to run unless specification has changed since simset was created
#rerun.simulations(simset) 

#simplot(simset, "new", split.by="race")


### Run interventions and select relevant results
collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = INTERVENTIONS, n.sim=50)

collection$run(2025, 2035, verbose=TRUE) # stop.for.errors = T if I want to check error messages 

results = collection$get(outcomes = c("new", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))


### Transform and output results into a table
results
dim(results)

#Sum new infections and population across all MSAs
total_results <- apply(results, setdiff(names(dim(results)), "location"), sum)
total_results
dim.names = dimnames(results)
dim.names
dim.names$location = c(dim.names$location, "Total")
oldresults = results
results = array(0, dim = sapply(dim.names, length), dimnames = dim.names)
results[, , , "Total", ] = total_results
results[, , , dimnames(oldresults)$location, ] = oldresults
results

#Calculate incidence rates
ir_black <- apply(results["black", , "new", , ], c("sim", "location", "intervention"), sum) / apply(results["black", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_other <- apply(results["other", , "new", , ], c("sim", "location", "intervention"), sum) / apply(results["other", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_total <- apply(results[ , , "new", , ], c("sim", "location", "intervention"), sum) / apply(results[ , , "population", , ], c("sim", "location", "intervention"), sum) 

#Take the mean, 2.5th and 97.5th percentile of the IRs across simulations, for each race group (Black, Other, Total)
ir_black_mean = apply(ir_black, c("location", "intervention"), mean)
ir_black_lower = apply(ir_black, c("location", "intervention"), quantile, probs=0.025)
ir_black_upper = apply(ir_black, c("location", "intervention"), quantile, probs=0.975)
ir_black_meanci = paste0(round(ir_black_mean*100000, 0), " (", round(ir_black_lower*100000, 0), ", ", round(ir_black_upper*100000, 0), ")")
dim(ir_black_meanci) = dim(ir_black_mean)
dimnames(ir_black_meanci) = dimnames(ir_black_mean)
ir_black_meanci

ir_other_mean = apply(ir_other, c("location", "intervention"), mean)
ir_other_lower = apply(ir_other, c("location", "intervention"), quantile, probs=0.025)
ir_other_upper = apply(ir_other, c("location", "intervention"), quantile, probs=0.975)
ir_other_meanci = paste0(round(ir_other_mean*100000, 0), " (", round(ir_other_lower*100000, 0), ", ", round(ir_other_upper*100000, 0), ")")
dim(ir_other_meanci) = dim(ir_other_mean)
dimnames(ir_other_meanci) = dimnames(ir_other_mean)
ir_other_meanci

ir_total_mean = apply(ir_total, c("location", "intervention"), mean)
ir_total_lower = apply(ir_total, c("location", "intervention"), quantile, probs=0.025)
ir_total_upper = apply(ir_total, c("location", "intervention"), quantile, probs=0.975)
ir_total_meanci = paste0(round(ir_total_mean*100000, 0), " (", round(ir_total_lower*100000, 0), ", ", round(ir_total_upper*100000, 0), ")")
dim(ir_total_meanci) = dim(ir_total_mean)
dimnames(ir_total_meanci) = dimnames(ir_total_mean)
ir_total_meanci

#Calculate the incidence rate difference (Black vs. Other)
ird <- ir_black - ir_other
ird_mean = apply(ird, c("location", "intervention"), mean)
ird_lower = apply(ird, c("location", "intervention"), quantile, probs=0.025)
ird_upper = apply(ird, c("location", "intervention"), quantile, probs=0.975)
ird_meanci = paste0(round(ird_mean*100000, 0), " (", round(ird_lower*100000, 0), ", ", round(ird_upper*100000, 0), ")")
dim(ird_meanci) = dim(ird_mean)
dimnames(ird_meanci) = dimnames(ird_mean)
ird_meanci

#Calculate the incidence rate ratio (Black vs. Other)
log_irr <- log(ir_black / ir_other)
log_irr_mean = apply(log_irr, c("location", "intervention"), mean)
log_irr_lower = apply(log_irr, c("location", "intervention"), quantile, probs=0.025)
log_irr_upper = apply(log_irr, c("location", "intervention"), quantile, probs=0.975)
irr_meanci = paste0(round(exp(log_irr_mean), 2), " (", round(exp(log_irr_lower), 2), ", ", round(exp(log_irr_upper), 2), ")")
dim(irr_meanci) = dim(log_irr_mean)
dimnames(irr_meanci) = dimnames(log_irr_mean)
irr_meanci


#Combine and output to CSV
table = cbind("IR - No Intervention (Overall)" = ir_total_meanci[, "noint"],
              "IR - No Intervention (Black)" = ir_black_meanci[, "noint"],
              "IR - No Intervention (Other)" = ir_other_meanci[, "noint"],
              "IRD - No Intervention" = ird_meanci[, "noint"],
              "IRR - No Intervention" = irr_meanci[, "noint"],
              "IR - Intervention (Overall)" = ir_total_meanci[, "testdisp"],
              "IR - Intervention (Black)" = ir_total_meanci[, "testdisp"],
              "IR - Intervention (Other)" = ir_total_meanci[, "testdisp"],
              "IRD - Intervention" = ird_meanci[, "testdisp"],
              "IRR - Intervention" = irr_meanci[, "testdisp"]
)
table
write.csv(table, file="../../code/jheem_analyses/applications/ehe_disparities/table.csv")

