
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
  
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
  
    msm.race <- reshape2::melt(apply(y[,,'msm',,], c('year','race'), mean))
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
    idu.race <- reshape2::melt(apply(y[,,,'active_IDU',], c('year', 'race'), mean))
    df.pts <- subset(p.idu.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2017)
    idu.race.plot <- ggplot(idu.race, aes(year, value, color=race)) +
      geom_line(linewidth = 1) +
      geom_point(aes(x=year+2, y = p, color=raceid), data = df.pts) +
      ylim(0,max(idu.race$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()
    
    idu.age <- reshape2::melt(apply(y[,,,'active_IDU',], c('year', 'age'), mean))
    df.pts <- subset(p.idu.df.long, ageid != "ALL") |> dplyr::mutate(year = year + 2017)
    idu.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
      geom_line(linewidth = 1) +
      geom_point(aes(x = year+2, y = p, color=ageid), data = df.pts) +
      ylim(0,max(idu.age$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()
    
    idu.sex <- reshape2::melt(apply(y[,,,'active_IDU',], c('year', 'sex'), mean))
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
    
  pdf("PrEP_Use_Plots.pdf", width = 18, height = 5)
  combined.plot
  msm.plots
  idu.plots
  dev.off()
  
  # prep indication plots -----
    ff2 <-  get.prep.indication.functional.form(specification.metadata = metadata)
    x <- ff2$project(2017:2030, alphas = NULL, dim.names = ff2$minimum.dim.names)
    
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
    combined.plot.pi <- ggpubr::ggarrange(race.plot, sex.plot, risk.plot, age.plot,
                                       ncol = 2, nrow = 2, labels = c("Race","Sex","Risk","Age"))
    combined.plot.pi
    
    msm.race <- reshape2::melt(apply(y[,,'msm',,], c('year','race'), mean))
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
    
    msm.plots.pi <- ggpubr::ggarrange(msm.race.plot, msm.age.plot, msm.risk.plot,
                                   ncol=1, nrow=3, labels = c("MSM - Race", "MSM - Age", "MSM - Risk")) 
    msm.plots.pi
    
    pdf("PrEP_Indication_Plots.pdf", width = 18, height = 10)
    combined.plot.pi
    msm.plots.pi
    dev.off()
  
}

#-- The Functions to Implement --#


get.prep.use.functional.form <- function(specification.metadata)
{
    # Some variables you will use
    anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
    max.prep.coverage = 0.6 # modify as you see fit
      
    # Set up our intercept/slope arrays
    # We are going to define these on the LOGIT scale
    dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
    int = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    slope = array(0, dim=sapply(dim.names, length),
               dimnames = dim.names)
    
    
    # The code for how you can parse the age strata we need
    age.info = parse.age.strata.names(dim.names$age)
    age.spans = age.info$upper - age.info$lower
  
    # Do the work
    
    int[,,,] <- int[,,,] + coef(fit2.big.df)[1]
    
    int[,"black",,] <- int[,"black",,] + coef(fit2.big.df)[3]
    int[,"hispanic",,] <- int[,"hispanic",,] + coef(fit2.big.df)[4]
    int[,"other",,] <- int[,"other",,] + coef(fit2.big.df)[5]
    
    int["13-24 years",,,] <- int["13-24 years",,,] + coef(fit2.big.df)[6]
    int["25-34 years",,,] <- int["25-34 years",,,] + coef(fit2.big.df)[7]
    int["35-44 years",,,] <- int["35-44 years",,,] + coef(fit2.big.df)[8]
    int["45-54 years",,,] <- int["45-54 years",,,] + coef(fit2.big.df)[9]
    int["55+ years",,,] <- int["55+ years",,,] + coef(fit2.big.df)[10]
    
    int[,,"female",] <- int[,,"female",] + coef(fit2.big.df)[11] + coef(fit2.big.df)[13]
    int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit2.big.df)[11] + coef(fit2.big.df)[15] 
    int[,,"msm",] <- int[,,"msm",] + 0 # msm is sexrisk REF
    
    int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit2.big.df)[12] 
    int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit2.big.df)[14] 
    int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + 0 #coef(fit2.big.df)[14] (NA)
    
    slope[,,,] <- coef(fit2.big.df)[2]
    
    # Make and return the functional form object
    create.logistic.linear.functional.form(
        intercept = int,
        slope = slope,
        anchor.year = anchor.year,
        min = 0,
        max = max.prep.coverage, 
        parameters.are.on.logit.scale = T
    ) 
}

get.prep.indication.functional.form <- function(specification.metadata){
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.indication = 1 # modify as you see fit
  
  # Set up our intercept/slope arrays
  # We are going to define these on the LOGIT scale
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  int = array(0, dim=sapply(dim.names, length),
              dimnames = dim.names)
  slope = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
  
  
  # The code for how you can parse the age strata we need
  age.info = parse.age.strata.names(dim.names$age)
  age.spans = age.info$upper - age.info$lower
  
  # Intercepts and slopes
  int[,,,] <- int[,,,] + coef(fit.pi.df)[1]
  
  int[,"black",,] <- int[,"black",,] + coef(fit.pi.df)[3]
  int[,"hispanic",,] <- int[,"hispanic",,] + coef(fit.pi.df)[4]
  int[,"other",,] <- int[,"other",,] + coef(fit.pi.df)[5]
  
  int["13-24 years",,,] <- int["13-24 years",,,] + coef(fit.pi.df)[6]
  int["25-34 years",,,] <- int["25-34 years",,,] + coef(fit.pi.df)[7]
  int["35-44 years",,,] <- int["35-44 years",,,] + coef(fit.pi.df)[8]
  int["45-54 years",,,] <- int["45-54 years",,,] + coef(fit.pi.df)[9]
  int["55+ years",,,] <- int["55+ years",,,] + coef(fit.pi.df)[10]
  
  int[,,"female",] <- int[,,"female",] + coef(fit.pi.df)[11] + coef(fit.pi.df)[13]
  int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.pi.df)[11] + coef(fit.pi.df)[15] 
  int[,,"msm",] <- int[,,"msm",] + 0 # MSM is sexrisk REF
  
  int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.pi.df)[12] 
  int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.pi.df)[14]
  int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.pi.df)[14]
  
  slope[,,,] <- coef(fit.pi.df)[2]
  
  # Make and return the functional form object
  create.logistic.linear.functional.form(
    intercept = int,
    slope = slope,
    anchor.year = anchor.year,
    min = 0,
    max = max.prep.indication, 
    parameters.are.on.logit.scale = T
  ) 
  
}

get.prep.persistence.functional.form <- function(specification.metadata)
{
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.coverage = 0.6 # modify as you see fit
  
  # Set up our intercept/slope arrays
  # We are going to define these on the LOGIT scale
  dim.names = specification.metadata$dim.names[c('age','race','sex','risk')]
  int = array(0, dim=sapply(dim.names, length),
              dimnames = dim.names)
  slope = array(0, dim=sapply(dim.names, length),
                dimnames = dim.names)
  
  
  # The code for how you can parse the age strata we need
  age.info = parse.age.strata.names(dim.names$age)
  age.spans = age.info$upper - age.info$lower
}
