# prep persistence plots -----
ff3 <-  get.prep.persistence.functional.form(specification.metadata = metadata)
anchor.year <- 2015
x <- ff3$project(anchor.year:2030, alphas = NULL, dim.names = ff3$minimum.dim.names)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff3$minimum.dim.names, list('year'=anchor.year:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names

y2 <- apply(y, c('year','race'), mean)

# y2

df <- reshape2::melt(y2)
race.plot <- ggplot(df, aes(x=year, y=value, color=race)) + geom_line(linewidth = 1) +
  ylim(0,1) + 
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal() 

df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
# no sex stratified data in our dataframe
sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) +
  ylim(0,1) +
  scale_color_manual(values = c("#008080", "#FF8C00", "#9932CC")) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal()

df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) +
  ylim(0,1) +
  scale_color_manual(values = c("#008080", "#FF8C00", "#9932CC")) +
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal()

df5 <- reshape2::melt(apply(y, c('year','age'), mean))
age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
  ylim(0,1) + 
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  theme_minimal() 

# arrange all 4 plots
combined.plot.pp <- ggpubr::ggarrange(race.plot, 
                                      sex.plot, risk.plot,
                                      age.plot,
                                      ncol = 2, nrow = 2, 
                                      labels = c("Race","Sex", "Risk", "Age"))
combined.plot.pp


pdf("PrEP_Persistence_Plots.pdf", width = 18, height = 10)
combined.plot.pp
dev.off()
