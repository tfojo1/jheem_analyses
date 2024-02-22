#transform model output into results tables


results
dim(results)
results[, , "incidence", "C.12580", "noint"]
ir <- results[ , , "incidence", , ] / results[ , , "population", , ] 
ir
dim(ir)

irr_blackoth <- ir["black", , , ] / ir["other", , , ]
irr_blackoth

apply(irr_blackoth, c("location", "intervention"), mean)


ir_total <- apply(results[ , , "incidence", , ], c("sim", "location", "intervention"), sum) / apply(results[ , , "population", , ], c("sim", "location", "intervention"), sum) 

ir_total_mean = apply(ir_total, c("location", "intervention"), mean)
ir_total_mean

ir_total_lower = apply(ir_total, c("location", "intervention"), quantile, probs=0.025)
ir_total_lower

ir_total_upper = apply(ir_total, c("location", "intervention"), quantile, probs=0.975)
ir_total_upper

ir_total_meanci = paste0(round(ir_total_mean*100000, 0), " (", round(ir_total_lower*100000, 0), ", ", round(ir_total_upper*100000, 0), ")")
dim(ir_total_meanci) = dim(ir_total_mean)
dimnames(ir_total_meanci) = dimnames(ir_total_mean)
ir_total_meanci

table = cbind("Overall IR - No Intervention" = ir_total_meanci[, "noint"],
              "Overall IR - Intervention" = ir_total_meanci[, "testdisp"])
table
write.csv(table, file="../../table.csv")
