
# prep use plots -----
ff <-  get.prep.use.functional.form(specification.metadata = metadata)
x <- ff$project(2017:2030, alphas = NULL, dim.names = ff$minimum.dim.names)

# length(x)
# x[[1]]
# sapply(x, max)
# sapply(x, min)
# sapply(x, mean)
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

df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) + 
  ylim(min(df3$value),max(df3$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal()


df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) + 
  ylim(min(df4$value),max(df4$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal()

df5 <- reshape2::melt(apply(y, c('year','age'), mean))
age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
  ylim(min(df5$value),max(df5$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal() 

# arrange all 4 plots
combined.plot <- ggpubr::ggarrange(race.plot, sex.plot, risk.plot, age.plot,
                                   ncol = 2, nrow = 2, labels = c("Race","Sex","Risk","Age"))
combined.plot

msm.race <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','race'), mean))
df.pts <- subset(p.msm.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + geom_line(linewidth = 1) +
  geom_point(aes(x=year, y = p, color=raceid), data = df.pts) +
  ylim(0,max(msm.race$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

msm.age <- reshape2::melt(apply(y[,,'msm',,], c('year','age'), mean))
df.pts <- subset(p.msm.df.long, ageid != "ALL") |> dplyr::mutate(year = year + anchor.year)

df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
msm.age$age <- factor(msm.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(msm.age$age) 

msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
  geom_line(linewidth = 1) +
  geom_point(aes(x=year, y = p, color=ageid), data = df.pts) +
  ylim(0,max(msm.age$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

msm.risk <- reshape2::melt(apply(y[,,'msm',,], c('year','risk'), mean))
# df.pts <- p.idu.df.long |> dplyr::mutate(year = year + anchor.year)
# df.pts$risk[df.pts$risk=="idu"] <- "active_IDU"
msm.risk.plot <- ggplot(msm.risk, aes(year, value, color = risk)) +
  geom_line(linewidth =1)  +
  # geom_point(aes(x=year+2, y = p, color=risk), data=df.pts) +
  ylim(min(msm.risk$value),max(msm.risk$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) + 
  ylab("PrEP use") +
  theme_minimal()
# msm.risk.plot

msm.plots <- ggpubr::ggarrange(msm.race.plot, msm.age.plot, msm.risk.plot,
                               ncol=1, nrow=3, labels = c("MSM - Race", "MSM - Age", "MSM - Risk")) 
msm.plots

# idu plots
idu.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'race'), mean))
df.pts <- subset(p.idu.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
idu.race.plot <- ggplot(idu.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x=year+2, y = p, color=raceid), data = df.pts) +
  ylim(0,max(idu.race$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

idu.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'age'), mean))
df.pts <- subset(p.idu.df.long, ageid != "ALL") |> dplyr::mutate(year = year + 2017)

df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
idu.age$age <- factor(idu.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(idu.age$age) 

idu.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = year+2, y = p, color=ageid), data = df.pts) +
  ylim(0,max(idu.age$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

idu.sex <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'sex'), mean))
df.pts <- subset(p.idu.df.long, sexid != "ALL") |> dplyr::mutate(year = year + anchor.year)
idu.sex.plot <- ggplot(idu.sex, aes(year, value, color=sex)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = year+2, y = p, color=sexid), data = df.pts) +
  ylim(0,max(idu.sex$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()  

idu.risk <- reshape2::melt(apply(y, c('year','risk'), mean))
df.pts <- subset(p.idu.df.long, risk == "idu") |> dplyr::mutate(year = year + anchor.year)
df.pts$risk <- rep("active_IDU", length(df.pts$p))
idu.risk.plot <- ggplot(idu.risk, aes(year, value, color=risk)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = year+2, y = p, color=risk), data = df.pts) +
  ylim(0,max(idu.risk$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()  

idu.plots <- ggpubr::ggarrange(idu.race.plot, idu.age.plot, idu.sex.plot, idu.risk.plot,
                               nrow = 2, ncol=2, labels = c("IDU - Race", "IDU - Age", 
                                                            "IDU - Sex", "IDU - Risk"))
idu.plots

het.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),"never_IDU",], c('year', 'race'), mean))
df.pts <- subset(p.het.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
het.race.plot <- ggplot(het.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x=year+2, y = p, color=raceid), data = df.pts) +
  ylim(0,max(het.race$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

het.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'never_IDU',], c('year', 'age'), mean))
df.pts <- subset(p.het.df.long, ageid != "ALL") |> dplyr::mutate(year = year + 2017)

df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
het.age$age <- factor(het.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(het.age$age) 

het.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = year+2, y = p, color=ageid), data = df.pts) +
  ylim(0,max(het.age$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

het.plots <- ggpubr::ggarrange(het.race.plot, het.age.plot, nrow = 2, ncol = 1, labels=c("Het - Race", "Het - Age"))

pdf("PrEP_Use_Plots.pdf", width = 18, height = 10)
combined.plot
msm.plots
idu.plots
het.plots
dev.off()