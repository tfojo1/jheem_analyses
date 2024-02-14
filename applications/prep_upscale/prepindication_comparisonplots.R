
# cdc only ------
pi.df.long <- rbind(pi.msm.cdc.long, pi.idu.df.long, pi.het.df.long)

pi.big.df <- pi.df.long |> dplyr::mutate(raceid = ifelse(group == "black", "black", 
                                                         ifelse(group == "hisp", "hisp", 
                                                                ifelse(group == "nbnh", "nbnh", "ALL"))),
                                         ageid = ifelse(group == "age1", "age1", 
                                                        ifelse(group == "age2", "age2", 
                                                               ifelse(group == "age3", "age3", 
                                                                      ifelse(group == "age4", "age4", 
                                                                             ifelse(group == "age5", "age5", "ALL"))))),
                                         sexid = ifelse(sexid == "msm", "msm",
                                                        ifelse(sexid == "het-male", "het-male",
                                                               ifelse(sexid == "het-female", "het-female",
                                                                      ifelse(group == "male", "male",
                                                                             ifelse(group == "female", "female",
                                                                                    "ALL")))) ))
pi.big.df$raceid <- relevel(factor(pi.big.df$raceid), ref = "ALL")
pi.big.df$ageid <- relevel(factor(pi.big.df$ageid), ref = "ALL")
pi.big.df$sexid <- relevel(factor(pi.big.df$sexid), ref = "ALL")

pi.big.df$sexid[pi.big.df$sexid=="msm"] <- "male"
pi.big.df$female <- ifelse(pi.big.df$sexid=="het-female", 1,
                           ifelse(pi.big.df$sexid=="female", 1, 0))

pi.big.df$nonmsm <- as.numeric(pi.big.df$riskid!="msm")
pi.big.df$idu <- as.numeric(pi.big.df$riskid=="idu")

msm.pi.df <- subset(pi.big.df, nonmsm==0)
# nonmsm.pi.df <- subset(pi.big.df, nonmsm==1)

# fit.pi.df <- lm(logit(pi) ~ years + raceid + ageid + sexrisk, data = pi.big.df)

pi.max <- 0.85

# fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid, data = msm.pi.df)
fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid + ageid, data = msm.pi.df)

fit.pi.msm


ff <-  get.prep.indication.functional.form(specification.metadata = metadata)
anchor.year <- 2009
x <- ff$project(anchor.year:2030)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff$minimum.dim.names, list('year'=anchor.year:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names


msm.pi.df$shape <- ifelse(msm.pi.df$dataid=="cdc", 1, 8)

msm.pi.df <- subset(msm.pi.df, dataid=="amis")
  
msm.race <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','race'), mean))
df.pts <- subset(msm.pi.df, raceid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$raceid <- factor(df.pts$raceid, levels = c("black","hisp","nbnh"))
msm.race$race <- factor(msm.race$race, levels = c("black","hispanic","other"))
levels(df.pts$raceid) <- levels(msm.race$race)
msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=years, y = pi, color=raceid, 
                 shape = as.factor(dataid)), 
             data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Race", shape = "Data Source") +
  theme_minimal() 

msm.age <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','age'), mean))
df.pts <- subset(msm.pi.df, ageid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
msm.age$age <- factor(msm.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(msm.age$age) 

msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  geom_point(aes(x=years, y = pi, color=ageid,
                 shape = as.factor(dataid)), data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Age", shape = "Data Source") +
  theme_minimal()

msm.plots.amis <- ggpubr::ggarrange(msm.race.plot, msm.age.plot,
                                  ncol=1, nrow=2, 
                                  labels = c("MSM PrEP Indications (AMIS) -Race", "MSM PrEP Indications (AMIS) -Age")) 
msm.plots.amis


# cdc only ------
pi.cdc.long <- rbind(pi.msm.cdc.long, pi.idu.df.long, pi.het.df.long)

pi.big.df <- pi.df.long |> dplyr::mutate(raceid = ifelse(group == "black", "black", 
                                                         ifelse(group == "hisp", "hisp", 
                                                                ifelse(group == "nbnh", "nbnh", "ALL"))),
                                         ageid = ifelse(group == "age1", "age1", 
                                                        ifelse(group == "age2", "age2", 
                                                               ifelse(group == "age3", "age3", 
                                                                      ifelse(group == "age4", "age4", 
                                                                             ifelse(group == "age5", "age5", "ALL"))))),
                                         sexid = ifelse(sexid == "msm", "msm",
                                                        ifelse(sexid == "het-male", "het-male",
                                                               ifelse(sexid == "het-female", "het-female",
                                                                      ifelse(group == "male", "male",
                                                                             ifelse(group == "female", "female",
                                                                                    "ALL")))) ))
pi.big.df$raceid <- relevel(factor(pi.big.df$raceid), ref = "ALL")
pi.big.df$ageid <- relevel(factor(pi.big.df$ageid), ref = "ALL")
pi.big.df$sexid <- relevel(factor(pi.big.df$sexid), ref = "ALL")

pi.big.df$sexid[pi.big.df$sexid=="msm"] <- "male"
pi.big.df$female <- ifelse(pi.big.df$sexid=="het-female", 1,
                           ifelse(pi.big.df$sexid=="female", 1, 0))

pi.big.df$nonmsm <- as.numeric(pi.big.df$riskid!="msm")
pi.big.df$idu <- as.numeric(pi.big.df$riskid=="idu")

msm.pi.df <- subset(pi.big.df, nonmsm==0)
# nonmsm.pi.df <- subset(pi.big.df, nonmsm==1)

# fit.pi.df <- lm(logit(pi) ~ years + raceid + ageid + sexrisk, data = pi.big.df)

pi.max <- 0.85

# fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid, data = msm.pi.df)
fit.pi.msm <- lm(logit(pi/pi.max) ~ years + raceid + ageid, data = msm.pi.df)

fit.pi.msm


ff <-  get.prep.indication.functional.form(specification.metadata = metadata)
anchor.year <- 2009
x <- ff$project(anchor.year:2030)

y <- sapply(x, function(z) {return(z)})
dim.names <- c(ff$minimum.dim.names, list('year'=anchor.year:2030))
dim(y) <- sapply(dim.names, length)
dimnames(y) <- dim.names


msm.pi.df$shape <- ifelse(msm.pi.df$dataid=="cdc", 1, 8)

msm.pi.df <- subset(msm.pi.df, dataid=="cdc")

msm.race <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','race'), mean))
df.pts <- subset(msm.pi.df, raceid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$raceid <- factor(df.pts$raceid, levels = c("black","hisp","nbnh"))
msm.race$race <- factor(msm.race$race, levels = c("black","hispanic","other"))
levels(df.pts$raceid) <- levels(msm.race$race)
msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a"))+
  geom_point(aes(x=years, y = pi, color=raceid, 
                 shape = as.factor(dataid)), 
             data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Race", shape = "Data Source") +
  theme_minimal() 

msm.age <- reshape2::melt(apply(y[,,'msm','never_IDU',], c('year','age'), mean))
df.pts <- subset(msm.pi.df, ageid != "ALL") %>%
  dplyr::mutate(years = years + anchor.year)
df.pts$ageid <- factor(df.pts$ageid, levels = c("age1", "age2", "age3", "age4", "age5"))
msm.age$age <- factor(msm.age$age, levels = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"))

levels(df.pts$ageid) <- levels(msm.age$age) 

msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#1f78b4", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  geom_point(aes(x=years, y = pi, color=ageid,
                 shape = as.factor(dataid)), data = df.pts) +
  scale_shape_manual(values = c(1,8)) +
  ylim(0,1) + 
  scale_x_continuous(breaks = seq(anchor.year, 2030, 1)) +
  labs(x = "Year", y = "PrEP indication", color = "Age", shape = "Data Source") +
  theme_minimal()

msm.plots.cdc <- ggpubr::ggarrange(msm.race.plot, msm.age.plot,
                                  ncol=1, nrow=2, 
                                  labels = c("MSM PrEP Indications (CDC) - Race", "MSM PrEP Indications (CDC) - Age")) 
msm.plots.cdc
