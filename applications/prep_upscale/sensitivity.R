
# Sensitivity Analyses

# source("../jheem_analyses/applications/EHE/ehe_specification.R") # re-source to wipe out codes

BLACK.MSM <- create.target.population(race = "black", sex = "msm", name = "Black MSM")
HISPANIC.MSM <- create.target.population(race = "hispanic", sex = "msm", name = "Hispanic MSM")
NON.BLACK.NON.HISPANIC.MSM <- create.target.population(race = "other", sex = "msm",
                                                       name = "Non-Black non-Hispanic MSM")

start.year <- 2025
end.year <- 2035

# lowering PrEP efficacy to 60%  ------
prep_eff_60 <- create.intervention.effect(
  quantity.name = "oral.prep.msm.rr",
  start.time = start.year,
  effect.values = 0.6, 
  times = end.year,
  scale = "ratio",
  apply.effects.as = "value",
  allow.values.less.than.otherwise = FALSE,
  allow.values.greater.than.otherwise = TRUE
)

extra.params.dist = join.distributions(
  black.black.idu.oe = Uniform.Distribution(9.11,9.13),
  hispanic.hispanic.idu.oe = Uniform.Distribution(1.04,1.06),
  other.other.idu.oe = Uniform.Distribution(1.04,1.06))

CALIBRATION.CODE = 'init.transmission.ehe' #for now, we are going to use 'uncalibrated' (ie, manually generated) simulations
LOCATIONS = c('C.26420','C.33100','C.16980') 

# BALTIMORE - 'C.12580'
# HOUSTON, MIAMI, CHICAGO

prep.eff <- create.intervention(c(BLACK.MSM, HISPANIC.MSM, NON.BLACK.NON.HISPANIC.MSM), 
                                    prep_eff_60,
                                    parameter.distribution=extra.params.dist,
                                    code = "prepeff") # can pass more than 1

sens.collection = create.simset.collection(version='ehe',
                                          calibration.code=CALIBRATION.CODE,
                                          locations = LOCATIONS,
                                          interventions = "prepeff",
                                          n.sim = 100)

sens.collection$run(start.year = 2025, end.year = 2035, 
              verbose = T,
              overwrite.prior = F) #NB: this last argument means it is not going to re-run interventions - so if you change the intervention, you need to set this to T to re-run

sens.results <- sens.collection$get(outcomes = c('incidence','population'),
                               keep.dimensions = c('race', 'sex','year'))

run_code <- "prepeff"
results_inc <- list()
results_df2 <- data.frame()

for(l in LOCATIONS){
  for (i in 1:length(run_code)) {
    for(yr in as.character(start.year:end.year)){
      bh.ir.sens <- (sum(rowMeans(sens.results[c("black","hispanic"),"msm",yr,,"incidence",l,1]))/
                  sum(rowMeans(sens.results[c("black","hispanic"),"msm",yr,,"population",l,1]))) * 100000
      other.ir.sens <- (mean(sens.results[c("other"),"msm",yr,,"incidence",l,1])/
                     mean(sens.results[c("other"),"msm",yr,,"population",l,1])) * 100000
      
      results_inc[[length(results_inc) + 1]] <- data.frame(
        year = yr,
        loc = l,
        # age_cat = age,
        intervention_code = run_code[i],
        bh_ir_sens = bh.ir.sens,
        other_ir_sens = other.ir.sens,
        irr = bh.ir.sens/other.ir.sens
      )
    }
    results_df2 <- do.call(rbind, results_inc)
  }
}

results_df2$loc <- ifelse(results_df2$loc == LOCATIONS[1], "Houston", 
                         ifelse(results_df2$loc == LOCATIONS[2],"Miami","Chicago"))

# Join results_df and results_df2 by year and loc
results_merge <- merge(results_df, results_df2, by = c("year", "loc"))

# Plot bh_ir_sens and bh_ir 
ggplot(results_merge, aes(x = year,  group = loc)) +
  geom_line(aes(y = bh_bl_ir, linetype = "Baseline", color = "Baseline")) +
  geom_point(aes(y = bh_bl_ir, group = loc, color = "Baseline"), shape = 20) +
  geom_line(aes(y = bh_ir_sens, linetype = "Lower PrEP Efficacy", 
                color = "Lower PrEP Efficacy")) +
  geom_point(aes(y = bh_ir_sens, group = loc, color = "Lower PrEP Efficacy"), 
             shape = 20) +
  facet_wrap(~loc, nrow = 3) +
  labs(title = "Black and Hispanic MSM Incidence Rate Sensitivity Analysis",
       x = "Year", 
       y = "Incidence Rate (per 100,000 population)",
       subtitle = "60% PrEP efficacy") +
  scale_linetype_manual(values = c("Baseline" = "dashed", 
                                   "Lower PrEP Efficacy" = "solid")) +
  scale_color_manual(values = c("Baseline" = "black", 
                                "Lower PrEP Efficacy" = "blue2"),
                     name = "Color") +
  guides(linetype = "none") +
  theme_minimal()

# Plot IRR
ggplot(results_merge, aes(x = year,  group = loc)) +
  geom_line(aes(y = irr.x, linetype = "Baseline", color = "Baseline")) +
  geom_point(aes(y = irr.x, group = loc, color = "Baseline"), shape = 20) +
  geom_line(aes(y = irr.y, linetype = "Lower PrEP Efficacy", 
                color = "Lower PrEP Efficacy")) +
  geom_point(aes(y = irr.y, group = loc, color = "Lower PrEP Efficacy"), 
             shape = 20) +
  facet_wrap(~loc, nrow = 3) +
  labs(title = "Black and Hispanic MSM Incidence Rate Ratio Sensitivity Analysis",
       x = "Year", 
       y = "Incidence Rate Ratio",
       subtitle = "60% PrEP efficacy") +
  scale_linetype_manual(values = c("Baseline" = "dashed", 
                                   "Lower PrEP Efficacy" = "solid")) +
  scale_color_manual(values = c("Baseline" = "black", 
                                "Lower PrEP Efficacy" = "blue2"),
                     name = "Color") +
  guides(linetype = "none") +
  theme_minimal()


# 
# results_merge <- results_merge %>%
#   dplyr::mutate(ird = bh_ir_sens-bh_bl_ir)
# 
# # plot difference between bh_ir_sens and bh_bl_ir
# ggplot(results_merge, aes(x = year, y = , group = loc)) +
#   geom_line() +
#   geom_point(shape = 20) +
#   facet_wrap(~loc, nrow = 3) +
#   labs(title = "Black and Hispanic MSM Incidence Rate Difference Sensitivity Analysis",
#        x = "Year", y = "Incidence Rate Difference") +
#   theme_minimal()

# repeat the plot for irr.x and irr.y and color it with normal or lower prep efficacy
ggplot(results_merge, aes(x = year, group = loc)) +
  geom_line(aes(y = irr.x, linetype = "Normal PrEP Efficacy", 
                color = "Normal PrEP Efficacy")) +
  geom_point(aes(y = irr.x, group = loc, color = "Normal PrEP Efficacy"),
             shape = 20) +
  geom_line(aes(y = irr.y, linetype = "Lower PrEP Efficacy",
                color = "Lower PrEP Efficacy")) +
  geom_point(aes(y = irr.y, color = "Lower PrEP Efficacy"), shape = 1) +
  labs(title = "Black and Hispanic MSM Incidence Rate Ratio Sensitivity Analysis",
       x = "Year", y = "Incidence Rate Ratio") +
  facet_wrap(~loc, nrow = 3) +
  scale_linetype_manual(values = c("dashed","solid"), name = "Line Type",
                        labels = c("Normal PrEP Efficacy","Lower PrEP Efficacy")) +
  scale_color_manual(values = c("black", "red"), name = "Legend",
                     labels = c("Normal PrEP Efficacy", "Lower PrEP Efficacy")) +
  theme_minimal()



# repeat the plot for IRR
ggplot(results_merge) +
  geom_line(aes(x = year, y = irr.x, group = loc, color = loc)) +
  geom_point(aes(x = year, y = irr.x, group = loc, color = loc)) +
  geom_line(aes(x = year, y = irr.y, group = loc, color = loc), linetype = "dashed") +
  geom_point(aes(x = year, y = irr.y, group = loc, color = loc)) +
  labs(title = "Black and Hispanic MSM Incidence Rate Ratio Sensitivity Analysis",
       x = "Year", y = "Incidence Rate Ratio") +
  theme_minimal()


#oral.prep.heterosexual.rr
#oral.prep.idu.rr
# correlation stuff 

# for each simset:
x <- simset$parameters
rowMeans(x)
# 
# make a scatterplot
# x = parameter
# y = percent.reduction


dimnames(simset$parameters)

x <- dimnames(simset$parameters)[[1]]

