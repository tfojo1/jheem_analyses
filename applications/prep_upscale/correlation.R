
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

# Boxplot function -----
print_boxplot <- function(simset, city){
  
  ## top 25% quantile -----
  params <- as.data.frame(simset$parameters) 
  final_param <- print_param(simset)
  
  params <- params[final_param,]
  
  quart.3 <- apply(params, 1, quantile, probs = 0.75)
  
  top.25 <- data.frame(
    name = final_param,
    quart.3 = quart.3
  )
  
  print_irr <- function(simset){
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
    
    return(irr)
  }
  
  param_list <- list()
  
  for(name in final_param){
    index <- which(params[name,] >= top.25[name,2])
    param_list[[name]]$index <- t(params[name, index])
    param_list[[name]]$irr <- cbind(print_irr(simset)[index])
    param_list[[name]]$quantile <- rep("Third Quartile", length(index))
    param_list[[name]]$name <- rep(name, length(index))
  }
  
  # convert param_list to a data frame
  param_list_top25 <- lapply(param_list, function(x) as.data.frame(x))
  
  ## bottom 25% quantile -----
  quart.1 <- apply(params, 1, quantile, probs = 0.25)
  
  bottom.25 <- data.frame(
    name = final_param,
    quart.1 = quart.1
  )
  
  param_list <- list()
  
  for(name in final_param){
    index <- which(params[name,] >= bottom.25[name,2])
    param_list[[name]]$index <- t(params[name, index])
    param_list[[name]]$irr <- cbind(print_irr(simset)[index])
    param_list[[name]]$quantile <- rep("First Quartile", length(index))
    param_list[[name]]$name <- rep(name, length(index))
  }
  
  # convert param_list to a data frame
  param_list_bottom25 <- lapply(param_list, function(x) as.data.frame(x))
  
  
  ## combine the top 25% and bottom 25% -----
  
  # get row count of each param
  row.count <- sapply(param_list_top25, nrow)
  
  combined <- data.frame()
  
  # combining rows of each param 
  for(i in 1:length(param_list_top25)){
    # rename first column
    colnames(param_list_top25[[i]])[1] <- "parameter"
    colnames(param_list_bottom25[[i]])[1] <- "parameter"
    
    # combine them
    combined <- rbind(combined, param_list_top25[[i]])
    combined <- rbind(combined, param_list_bottom25[[i]])
  }
 
  
  ## calculate the difference and reduction in median projections -----
median.diff <- data.frame()
for(i in 1:length(param_list_top25)){
  median.diff <- rbind(median.diff, data.frame(
    name = final_param[i],
    title = param.title[i], 
    diff = median(param_list_top25[[i]]$irr) - median(param_list_bottom25[[i]]$irr),
    red = (median(param_list_top25[[i]]$irr) - median(param_list_bottom25[[i]]$irr))/(median(param_list_bottom25[[i]]$irr))))
}

# order the data frame by the difference in median
median.diff <- median.diff[order(median.diff$diff, decreasing = T),]

# add median.diff$diff to combined
for(name in unique(combined$name)){
  combined$diff[combined$name == name] <- median.diff$diff[median.diff$name == name]
  combined$red[combined$name == name] <- median.diff$red[median.diff$name == name]
  # add pretty title
  combined$title[combined$name == name] <- median.diff$title[median.diff$name == name]
}

# order combined by decreasing diff
combined <- combined %>% arrange(desc(diff))

# specify the desired order of levels
desired_order <- unique(median.diff$name)

# convert the name column to a factor with the desired order of levels
combined$name <- factor(combined$name, levels = desired_order)

# plotting boxplot
ggplot(combined,
       aes(x = fct_rev(name), y = irr, fill = as.factor(quantile))) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(labels = median.diff$title) +
  labs(x = "Parameter", y = "BH/W-Incidence Rate Ratio", 
       title = paste("IRR in first vs third quartiles of each parameter in",city),
       fill = "Parameter Quartile") +
  scale_fill_manual(values = c("turquoise4", "orange3")) + 
  theme_minimal()

}

# IRR function ----
print_irr <- function(simset){
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
  
  return(irr <- bh.ir/other.ir)
}


# PRCC ------

plot_prcc <- function(simset, city){
  
  params <- as.data.frame(simset$parameters) 
  final_param <- print_param(simset)
  
  params <- params[final_param,]
  
  prcc.df <- as.data.frame(t(params))
  prcc.df$irr <- as.data.frame(print_irr(simset))[[1]]
  
  prcc <- epi.prcc(prcc.df, sided.test = 2, conf.level = 0.95)
  
  prcc$name <- final_param
  
  prcc <- prcc %>% arrange(desc(est)) %>% mutate(
    name = factor(name, levels = name),
    title = param.title[match(name, final_param)])
  
  ggplot(data = prcc, aes(x = est, y = fct_rev(name))) +
    geom_bar(stat = "identity", fill = "pink1", color = "black") +
    labs(x = "Partial Rank Correlation Coefficient", 
         y = "Parameter", title = paste0("Partial Rank Correlation Coefficients in ", city)) +
    scale_y_discrete(labels = prcc$title) +
    theme_minimal() 
}


# Houston
load("../../files/simulations/ehe/init.transmission.ehe-100/C.26420/ehe_init.transmission.ehe-100_C.26420_prepblmsm.Rdata")

print(print_param(simset))

# in ascending order of correlation
param.title <- c(
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Black MSM transmission rate in 2020",
  "Hispanic MSM transmission rate in 2010",
  "Black MSM transmission rate in 2010",
  "Global transmission rate",
  "Hispanic MSM transmission rate in 2000",
  "Other/Other sexual transmission rate",
  "Black MSM transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate"
)

plot_correlation(print_param(simset), param.title, simset)

houston_box <- print_boxplot(simset, "Houston")

# in ascending order of correlation
param.title <- c(
  "MSM Peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Black MSM transmission rate in 2020",
  "Hispanic MSM transmission rate in 2010",
  "Black MSM transmission rate in 2010",
  "Global transmission rate",
  "Hispanic MSM transmission rate in 2000",
  "Other/Other sexual transmission rate",
  "Black MSM transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate"
)

houston_prcc <- plot_prcc(simset, "Houston")

cowplot::plot_grid(houston_prcc, houston_box, nrow = 2)

# Miami 

load("../../files/simulations/ehe/init.transmission.ehe-100/C.33100/ehe_init.transmission.ehe-100_C.33100_prepblmsm.Rdata")

print(print_param(simset))

param.title <- c(
  "Black/Black sexual transmission rate",
  "Hispanic MSM transmission rate in 2010",
  "Other/Other sexual transmission rate",
  "Other heterosexual transmission rate in 2010",
  "MSM Peak transmission rate multiplier",
  "Other heterosexual transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate",
  "Black MSM transmission rate in 2020",
  "Hispanic MSM transmission rate in 2020",
  "Global transmission rate",
  "Other heterosexual transmission rate in 2020"
)

plot_correlation(print_param(simset), param.title, simset)

boxplot_miami <- print_boxplot(simset, "Miami")

param.title <- c(
  "Black/Black sexual transmission rate",
  "Hispanic MSM transmission rate in 2010",
  "Other/Other sexual transmission rate",
  "Other heterosexual transmission rate in 2010",
  "MSM Peak transmission rate multiplier",
  "Other heterosexual transmission rate in 2000",
  "Hispanic/Hispanic sexual transmission rate",
  "Black MSM transmission rate in 2020",
  "Hispanic MSM transmission rate in 2020",
  "Global transmission rate",
  "Other heterosexual transmission rate in 2020"
)

prcc_miami <- plot_prcc(simset, "Miami")

cowplot::plot_grid(prcc_miami,boxplot_miami,  nrow = 2)

# Chicago

load("../../files/simulations/ehe/init.transmission.ehe-100/C.16980/ehe_init.transmission.ehe-100_C.16980_prepblmsm.Rdata")

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

boxplot_chicago <- print_boxplot(simset, "Chicago")

param.title <- c(
  "MSM peak transmission rate multiplier",
  "Hispanic MSM transmission rate in 2020",
  "Black MSM transmission rate in 2010",
  "Black MSM transmission rate in 2000",
  "Hispanic MSM transmission rate in 2000",
  "Other/Other sexual transmission rate",
  "Hispanic/Hispanic sexual transmission rate"
)

prcc_chicago <- plot_prcc(simset, "Chicago")

cowplot::plot_grid( prcc_chicago, boxplot_chicago,nrow = 2)

cowplot::plot_grid(houston_box, houston_prcc,
                   boxplot_miami, prcc_miami,
                   boxplot_chicago, prcc_chicago, nrow = 3)

