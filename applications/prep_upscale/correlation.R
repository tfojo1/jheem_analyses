
# Looking at correlation between parameters and our outcomes

print_param <- function(simset){
  bh.ir <- simset$get(outcome = "incidence", 
                      dimension.values = list(year = '2035', 
                                              race = c("black","hispanic"),
                                              sex = "msm"),
                      keep.dimensions = character())/simset$get(
                        outcome = "population", 
                        dimension.values = list(year = '2035', 
                                                race = c("black","hispanic"), 
                                                sex = "msm"),
                        keep.dimensions = character())
  
  other.ir <- simset$get(outcome = "incidence", dimension.values = list(
    year = '2035', race = "other", 
    sex = "msm"),
    keep.dimensions = character())/simset$get(
      outcome = "population", 
      dimension.values = list(year = '2035', race = "other", 
                              sex = "msm"),
      keep.dimensions = character())
  
  irr <- bh.ir/other.ir
  
  corr.param <- apply(simset$parameters, 1, cor, irr)
  corr.param <- corr.param[!is.na(corr.param)]
  corr.param <- corr.param[order(abs(corr.param))]
  
  param.list <- names(corr.param[abs(corr.param) >= 0.5])
  param.list <- param.list[!grepl("idu", param.list)]
  
  return(param.list)
}

plot_correlation <- function(param.list, param.title, simset){  
  bh.ir <- simset$get(outcome = "incidence", 
                      dimension.values = list(year = '2035', 
                                              race = c("black","hispanic"),
                                              sex = "msm"),
                      keep.dimensions = character())/simset$get(
                        outcome = "population", 
                        dimension.values = list(year = '2035', 
                                                race = c("black","hispanic"), 
                                                sex = "msm"),
                        keep.dimensions = character())
  
  other.ir <- simset$get(outcome = "incidence", dimension.values = list(
    year = '2035', race = "other", 
    sex = "msm"),
    keep.dimensions = character())/simset$get(
      outcome = "population", 
      dimension.values = list(year = '2035', race = "other", 
                              sex = "msm"),
      keep.dimensions = character())
  
  irr <- bh.ir/other.ir
  
  # create a list of 10 ggplots of the parameters in param.list vs irr
  plot.list <- lapply(param.list, function(param){
    ggplot(data.frame(simset$parameters[param,], irr),
           aes(x = simset$parameters[param,], y = irr)) +
      geom_point() + 
      labs(x = param, y = "IRR", 
           title = param.title[param.list == param]) +
      theme_minimal()
  })
  
  # put the plots above in a grid
  cowplot::plot_grid(plotlist = plot.list, ncol = 2)
}

# Houston
load("../../files/simulations/ehe/init.transmission.ehe-100/C.26420/ehe_init.transmission.ehe-100_C.26420_prepu40p80msm.Rdata")

print(print_param(simset))

# in ascending order of correlation
param.title <- c(
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2010",
  "Hispanic MSM transmission rate in 2000",
  "Black MSM transmission rate in 2010",
  "Black MSM transmission rate in 2000",
  "Hispanic MSM transmission rate in 2010",
  "Global transmission rate",
  "Other/Other sexual transmission rate",
  "Black MSM transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate"
)

plot_correlation(print_param(simset), param.title, simset)

# Miami 

load("../../files/simulations/ehe/init.transmission.ehe-100/C.33100/ehe_init.transmission.ehe-100_C.33100_prepu40p80msm.Rdata")

print(print_param(simset))

param.title <- c(
  "Hispanic MSM transmission rate in 2010",
  "Black/Black sexual transmission rate",
  "Other/Other sexual transmission rate",
  "Hispanic/Hispanic sexual transmission rate",
  "Other heterosexual transmission rate in 2010",
  "Black MSM transmission rate in 2010",
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Other heterosexual transmission rate in 2000",
  "Global transmission rate",
  "Other heterosexual transmission rate in 2020"
)

plot_correlation(print_param(simset), param.title, simset)

# Chicago

load("../../files/simulations/ehe/init.transmission.ehe-100/C.16980/ehe_init.transmission.ehe-100_C.16980_prepu40p80msm.Rdata")

print(print_param(simset))

param.title <- c(
  "MSM peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Black MSM transmission rate in 2010",
  "Black MSM transmission rate in 2000",
  "Hispanic MSM transmission rate in 2000",
  "Other/Other sexual transmission rate",
  "Hispanic/Hispanic sexual transmission rate"
)
plot_correlation(print_param(simset), param.title, simset)


# cor(irr, simset$parameters[1,], method = "spearman")


