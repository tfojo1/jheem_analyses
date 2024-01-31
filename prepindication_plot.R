# prep indication plots -----
ff <-  get.prep.indication.functional.form(specification.metadata = metadata)
x <- ff$project(2017:2030, alphas = NULL, dim.names = ff$minimum.dim.names)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff$minimum.dim.names, list('year'=2017:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names

sapply(ff$project(2010:2020), mean)

y2 <- apply(y, c('year','race'), mean)

# y2

df <- reshape2::melt(y2)
df.pts <- subset(p.msm.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
race.plot <- ggplot(df, aes(x=year, y=value, color=race)) + geom_line(linewidth = 1) +
  ylim(0,max(df$value)) + 
  
  # geom_point(aes(x=year, y = p, color=raceid), data = df.pts)
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal() 

df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) + 
  ylim(0,max(df3$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal()


df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) + 
  ylim(0,max(df4$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal()

df5 <- reshape2::melt(apply(y, c('year','age'), mean))
age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
  ylim(0,max(df5$value)) + 
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  theme_minimal() 

# arrange all 4 plots
combined.plot.pi <- ggpubr::ggarrange(race.plot, sex.plot, risk.plot, age.plot,
                                      ncol = 2, nrow = 2, labels = c("Race","Sex","Risk","Age"))
combined.plot.pi

msm.race <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','race'), mean))
df.pts <- subset(msm.pi.df, raceid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year + 3)
df.pts$raceid <- factor(df.pts$raceid, levels = c("black","hisp","nbnh"))
msm.race$race <- factor(msm.race$race, levels = c("black","hispanic","other"))
levels(df.pts$raceid) <- levels(msm.race$race)
msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + geom_line(linewidth = 1) +
  geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
  ylim(0,max(msm.race$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

msm.age <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','age'), mean))
df.pts <- subset(msm.pi.df, ageid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year + 3)
df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
msm.age$age <- factor(msm.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(msm.age$age) 

msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
  geom_line(linewidth = 1) +
  geom_point(aes(x=years, y = pi, color=ageid), data = df.pts) +
  ylim(0,max(msm.age$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

# msm.risk <- reshape2::melt(apply(y[,,'msm',,], c('year','risk'), mean))
# # df.pts <- p.idu.df.long |> dplyr::mutate(year = year + anchor.year)
# # df.pts$risk[df.pts$risk=="idu"] <- "active_IDU"
# msm.risk.plot <- ggplot(msm.risk, aes(year, value, color = risk)) +
#   geom_line(linewidth =1)  +
#   # geom_point(aes(x=year+2, y = p, color=risk), data=df.pts) +
#   ylim(min(msm.risk$value),max(msm.risk$value)) +
#   scale_x_continuous(breaks = seq(2017, 2030, 1)) +
#   ylab("PrEP use") +
#   theme_minimal()
# msm.risk.plot

msm.plots.pi <- ggpubr::ggarrange(msm.race.plot, msm.age.plot,
                                  # msm.risk.plot,
                                  ncol=1, nrow=2, labels = c("MSM - Race", "MSM - Age", "MSM - Risk")) 
msm.plots.pi

# idu plots
idu.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'race'), mean))
df.pts <- subset(idu.pi.df, raceid!="ALL")  %>%
  dplyr::mutate(years = years + anchor.year + 8) 

df.pts$raceid <- factor(df.pts$raceid, levels = c("black", "hisp", "nbnh"))
idu.race$race <- factor(idu.race$race, levels = c("black","hispanic","other"))

levels(df.pts$raceid) <- levels(idu.race$race) 

idu.race.plot <- ggplot(idu.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

idu.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'age'), mean))
df.pts <- subset(idu.pi.df, ageid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year + 8) 

df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
idu.age$age <- factor(idu.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(idu.age$age) 

idu.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = years, y = pi, color=ageid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

idu.sex <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'active_IDU',], c('year', 'sex'), mean))
df.pts <- subset(idu.pi.df, sexid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year + 8)

df.pts$sexid <- factor(df.pts$sexid, levels = c("male", "female"))
idu.sex$sex <- factor(idu.sex$sex, levels = c("heterosexual_male", "female"))

levels(df.pts$sexid) <- levels(idu.sex$sex) 

idu.sex.plot <- ggplot(idu.sex, aes(year, value, color=sex)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = years, y = pi, color=sexid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()  

idu.plots.pi <- ggpubr::ggarrange(idu.race.plot, idu.age.plot, idu.sex.plot,
                               nrow = 3, ncol=1, labels = c("IDU - Race", "IDU - Age", 
                                                            "IDU - Sex"))
idu.plots.pi

het.race <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),"never_IDU",], c('year', 'race'), mean))
df.pts <- subset(het.pi.df, raceid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year + 1) 

df.pts$raceid <- factor(df.pts$raceid, levels = c("black", "hisp", "nbnh"))
het.race$race <- factor(het.race$race, levels = c("black","hispanic","other"))

levels(df.pts$raceid) <- levels(het.race$race) 

het.race.plot <- ggplot(het.race, aes(year, value, color=race)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
  ylim(0,max(het.race$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

het.age <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'never_IDU',], c('year', 'age'), mean))
df.pts <- subset(het.pi.df, ageid!="ALL") %>%
  dplyr::mutate(years = years + anchor.year + 1)

df.pts$ageid <- factor(df.pts$ageid, 
                       levels = c("age1", "age2", "age3", "age4", "age5"))
het.age$age <- factor(het.age$age, 
                      levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(het.age$age) 

het.age.plot <- ggplot(het.age, aes(year, value, color=age)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = years, y = pi, color=ageid), data = df.pts) +
  ylim(0,1) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()

het.sex <- reshape2::melt(apply(y[,,c("heterosexual_male","female"),'never_IDU',], c('year', 'sex'), mean))
df.pts <- subset(het.pi.df, sexid!="ALL" & 
                   raceid =="ALL" &
                   ageid == "ALL") %>%
  dplyr::mutate(years = years + anchor.year + 1)

df.pts$sexid <- factor(df.pts$sexid, levels = c("het-male", "het-female"))
het.sex$sex <- factor(het.sex$sex, levels = c("heterosexual_male", "female"))

levels(df.pts$sexid) <- levels(het.sex$sex) 

het.sex.plot <- ggplot(het.sex, aes(year, value, color=sex)) +
  geom_line(linewidth = 1) +
  geom_point(aes(x = years, y = pi, color=sexid), data = df.pts) +
  ylim(0,max(het.sex$value)) +
  scale_x_continuous(breaks = seq(2017, 2030, 1)) +
  ylab("PrEP use") +
  theme_minimal()  

het.plots.pi <- ggpubr::ggarrange(het.race.plot, het.age.plot, het.sex.plot,
                                  nrow = 3, ncol = 1, labels=c("Het - Race", "Het - Age", "Het - Sex"))


pdf("PrEP_Indication_Plots.pdf", width = 18, height = 10)
combined.plot.pi
msm.plots.pi
idu.plots.pi
het.plots.pi
dev.off()
