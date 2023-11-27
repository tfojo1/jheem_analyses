
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
  
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
      ylim(min(msm.race$value),max(msm.race$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()

    msm.age <- reshape2::melt(apply(y[,,'msm',,], c('year','age'), mean))
    df.pts <- subset(p.msm.df.long, ageid != "ALL") |> dplyr::mutate(year = year + 2017)
    msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + 
      geom_line(linewidth = 1) +
      geom_point(aes(x=year, y = p, color=ageid), data = df.pts) +
      ylim(min(msm.age$value),max(msm.age$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()

    msm.risk <- reshape2::melt(apply(y[,,'msm',,], c('year','risk'), mean))
    msm.risk.plot <- ggplot(msm.risk, aes(year, value, color = risk)) +
      geom_line(linewidth =1)  +
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
      geom_point(aes(x=year, y = p, color=raceid), data = df.pts) +
      ylim(min(idu.race$value),max(idu.race$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()
    
    idu.age <- reshape2::melt(apply(y[,,,'active_IDU',], c('year', 'age'), mean))
    df.pts <- subset(p.idu.df.long, ageid != "ALL") |> dplyr::mutate(year = year + 2017)
    idu.age.plot <- ggplot(idu.age, aes(year, value, color=age)) +
      geom_line(linewidth = 1) +
      geom_point(aes(x = year, y = p, color=ageid), data = df.pts) +
      ylim(min(idu.age$value),max(idu.age$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()
    
    idu.sex <- reshape2::melt(apply(y[,,,'active_IDU',], c('year', 'sex'), mean))
    df.pts <- subset(p.idu.df.long, sexid != "ALL") |> dplyr::mutate(year = year + 2017)
    idu.sex.plot <- ggplot(idu.sex, aes(year, value, color=sex)) +
      geom_line(linewidth = 1) +
      geom_point(aes(x = year, y = p, color=sexid), data = df.pts) +
      ylim(min(idu.sex$value),max(idu.sex$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()  
    
    idu.plots <- ggpubr::ggarrange(idu.race.plot, idu.age.plot, idu.sex.plot,
                      nrow = 3, ncol=1, labels = c("IDU - Race", "IDU - Age", "IDU - Sex"))
    idu.plots
    
    
  
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
    
    int[,,,] <- coef(fit.big.df)[1]
    
    slope[,"black",,] <- slope[,"black",,] + coef(fit.big.df)[3]
    slope[,"hispanic",,] <- slope[,"hispanic",,] + coef(fit.big.df)[4]
    slope[,"other",,] <- slope[,"other",,] + coef(fit.big.df)[5]
    
    slope["13-24 years",,,] <- slope["13-24 years",,,] + coef(fit.big.df)[6]
    slope["25-34 years",,,] <- slope["25-34 years",,,] + coef(fit.big.df)[7]
    slope["35-44 years",,,] <- slope["35-44 years",,,] + coef(fit.big.df)[8]
    slope["45-54 years",,,] <- slope["45-54 years",,,] + coef(fit.big.df)[9]
    slope["55+ years",,,] <- slope["55+ years",,,] + coef(fit.big.df)[10]
    
    slope[,,"female",] <- slope[,,"female",] + coef(fit.big.df)[11]
    slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit.big.df)[11] #coef(fit.big.df)[12]
    slope[,,"msm",] <- slope[,,"msm",] + coef(fit.big.df)[13]
    
    slope[,,,"active_IDU"] <- slope[,,,"active_IDU"] + coef(fit.big.df)[14]
    
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

get.prep.indication.functional.form <- function(specification.metadata)
{
  # Some variables you will use
  anchor.year = 2017 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
  max.prep.indication = 0.8 # modify as you see fit
  
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
  
  # Intercepts and Slopes
  int[,'black','msm',] <- int[,'black','msm',] + coef(fit.pi.msm.black)[1]
  int[,'hispanic','msm',] <- int[,'hispanic','msm',] + coef(fit.pi.msm.hisp)[1]
  int[,'other','msm',] <- int[,'other','msm',] + coef(fit.pi.msm.nbnh)[1]
  
  int["13-24 years",,'msm',] <- int["13-24 years",,'msm',] + coef(fit.pi.msm.age1)[1]
  int["25-34 years",,'msm',] <- int["25-34 years",,'msm',] + coef(fit.pi.msm.age2)[1]
  int["35-44 years",,'msm',] <- int["35-44 years",,'msm',] + coef(fit.pi.msm.age3)[1]
  int["45-54 years",,'msm',] <- int["45-54 years",,'msm',] + coef(fit.pi.msm.age4)[1]
  int["55+ years",,'msm',] <- int["55+ years",,'msm',] + coef(fit.pi.msm.age5)[1]
  
  
  
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
