# prep persistence plots -----
ff3 <-  get.prep.persistence.functional.form(specification.metadata = metadata)
x <- ff3$project(2017:2030, alphas = NULL, dim.names = ff3$minimum.dim.names)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff$minimum.dim.names, list('year'=2017:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names

y2 <- apply(y, c('year','race'), mean)

# y2

df <- reshape2::melt(y2)
df.pts <- subset(p.msm.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
race.plot <- ggplot(df, aes(x=year, y=value, color=race)) + geom_line(linewidth = 1) +
  ylim(min(df$value),max(df$value)) + 
  
  # geom_point(aes(x=year, y = p, color=raceid), data = df.pts)
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal() 
# 
# df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
# sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) + 
#   ylim(min(df3$value),max(df3$value)) + 
#   scale_x_continuous(breaks = seq(2017, 2030, 1)) +
#   theme_minimal()
# 
# 
# df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
# risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) + 
#   ylim(min(df4$value),max(df4$value)) + 
#   scale_x_continuous(breaks = seq(2017, 2030, 1)) +
#   theme_minimal()

df5 <- reshape2::melt(apply(y, c('year','age'), mean))
age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
  ylim(min(df5$value),max(df5$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal() 

# arrange all 4 plots
combined.plot.pp <- ggpubr::ggarrange(race.plot, 
                                      #sex.plot, risk.plot, 
                                      age.plot,
                                      ncol = 1, nrow = 2, labels = c("Race","Age"))
combined.plot.pp


pdf("PrEP_Persistence_Plots.pdf", width = 18, height = 10)
combined.plot.pp
dev.off()