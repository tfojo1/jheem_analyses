#Transform model output into results tables
results
dim(results)
results[, , "incidence", "C.12580", "noint"]

#Sum new infections and population across all MSAs
total_results <- apply(results, setdiff(names(dim(results)), "location"), sum)
total_results

dim.names = dimnames(results)
dim.names
dim.names$location = c(dim.names$location, "Total")

oldresults = results
results = array(0, dim = sapply(dim.names, length), dimnames = dim.names)
results

results[, , , "Total", ] = total_results
results[, , , dimnames(oldresults)$location, ] = oldresults
results

#Incidence Rates
ir_black <- apply(results["black", , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results["black", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_other <- apply(results["other", , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results["other", , "population", , ], c("sim", "location", "intervention"), sum) 
ir_total <- apply(results[ , , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results[ , , "population", , ], c("sim", "location", "intervention"), sum) 

#average across simulations
ir_total_mean = apply(ir_total, c("location", "intervention"), mean)
ir_total_lower = apply(ir_total, c("location", "intervention"), quantile, probs=0.025)
ir_total_upper = apply(ir_total, c("location", "intervention"), quantile, probs=0.975)
ir_total_meanci = paste0(round(ir_total_mean*100000, 0), " (", round(ir_total_lower*100000, 0), ", ", round(ir_total_upper*100000, 0), ")")
dim(ir_total_meanci) = dim(ir_total_mean)
dimnames(ir_total_meanci) = dimnames(ir_total_mean)
ir_total_meanci

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

#IRD
ird <- ir_black - ir_other
ird_mean = apply(ird, c("location", "intervention"), mean)
ird_lower = apply(ird, c("location", "intervention"), quantile, probs=0.025)
ird_upper = apply(ird, c("location", "intervention"), quantile, probs=0.975)
ird_meanci = paste0(round(ird_mean*100000, 0), " (", round(ird_lower*100000, 0), ", ", round(ird_upper*100000, 0), ")")
dim(ird_meanci) = dim(ird_mean)
dimnames(ird_meanci) = dimnames(ird_mean)
ird_meanci

#IRR
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
write.csv(table, file="../../table.csv")
