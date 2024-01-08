
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
    source('prepuse_plot.R')
  
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
    df.pts <- subset(msm.pi.df, raceid != "ALL") |> dplyr::mutate(years = years + anchor.year)
    df.pts$raceid <- factor(df.pts$raceid, levels = c("black","hisp","nbnh"))
    msm.race$race <- factor(msm.race$race, levels = c("black","hispanic","other"))
    # levels(df.pts$raceid) <- levels(msm.race$race)
    msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + geom_line(linewidth = 1) +
      geom_point(aes(x=years, y = pi, color=raceid), data = df.pts) +
      ylim(0,max(msm.race$value)) +
      scale_x_continuous(breaks = seq(2017, 2030, 1)) +
      ylab("PrEP use") +
      theme_minimal()
    
    msm.age <- reshape2::melt(apply(y[,,'msm',,], c('year','age'), mean))
    df.pts <- subset(msm.pi.df, ageid != "ALL") |> dplyr::mutate(years = years + anchor.year)
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
    
    # int[,,"msm",] <- int[,,"msm",] + coef(fit.p.msm)["(Intercept)"]
    int[,,"msm","never_IDU"] <- int[,,"msm","never_IDU"] + coef(fit.p.msm)["(Intercept)"]
    int[,,"female",] <- int[,,"female",] + coef(fit.p.nonmsm)["(Intercept)"]
    int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.p.nonmsm)["(Intercept)"]
    
    # int[,"black","msm",] <- int[,"black","msm",] + coef(fit.p.msm)["raceidblack"]
    int[,"black","msm","never_IDU"] <- int[,"black","msm","never_IDU"] + coef(fit.p.msm)["raceidblack"]
    int[,"hispanic","msm","never_IDU"] <- int[,"hispanic","msm","never_IDU"] + coef(fit.p.msm)["raceidhispanic"]
    int[,"other","msm","never_IDU"] <- int[,"other","msm","never_IDU"] + coef(fit.p.msm)["raceidother"]
    
    int[,"black","heterosexual_male",] <- int[,"black","heterosexual_male",] + coef(fit.p.nonmsm)["raceidblack"]
    int[,"hispanic","heterosexual_male",] <- int[,"hispanic","heterosexual_male",] + coef(fit.p.nonmsm)["raceidhispanic"]
    int[,"other","heterosexual_male",] <- int[,"other","heterosexual_male",] + coef(fit.p.nonmsm)["raceidother"]
    
    int[,"black","female",] <- int[,"black","female",] + coef(fit.p.nonmsm)["raceidblack"]
    int[,"hispanic","female",] <- int[,"hispanic","female",] + coef(fit.p.nonmsm)["raceidhispanic"]
    int[,"other","female",] <- int[,"other","female",] + coef(fit.p.nonmsm)["raceidother"]
    
    int["13-24 years",,"msm","never_IDU"] <- int["13-24 years",,"msm","never_IDU"] + coef(fit.p.msm)["ageidage1"]
    int["25-34 years",,"msm","never_IDU"] <- int["25-34 years",,"msm","never_IDU"] + coef(fit.p.msm)["ageidage2"]
    int["35-44 years",,"msm","never_IDU"] <- int["35-44 years",,"msm","never_IDU"] + coef(fit.p.msm)["ageidage3"]
    int["45-54 years",,"msm","never_IDU"] <- int["45-54 years",,"msm","never_IDU"] + coef(fit.p.msm)["ageidage4"]
    int["55+ years",,"msm","never_IDU"] <- int["55+ years",,"msm","never_IDU"] + coef(fit.p.msm)["ageidage5"]
    
    int["13-24 years",,"heterosexual_male",] <- int["13-24 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage1"]
    int["25-34 years",,"heterosexual_male",] <- int["25-34 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage2"]
    int["35-44 years",,"heterosexual_male",] <- int["35-44 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage3"]
    int["45-54 years",,"heterosexual_male",] <- int["45-54 years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage4"]
    int["55+ years",,"heterosexual_male",] <- int["55+ years",,"heterosexual_male",] + coef(fit.p.nonmsm)["ageidage5"]
    
    int["13-24 years",,"female",] <- int["13-24 years",,"female",] + coef(fit.p.nonmsm)["ageidage1"]
    int["25-34 years",,"female",] <- int["25-34 years",,"female",] + coef(fit.p.nonmsm)["ageidage2"]
    int["35-44 years",,"female",] <- int["35-44 years",,"female",] + coef(fit.p.nonmsm)["ageidage3"]
    int["45-54 years",,"female",] <- int["45-54 years",,"female",] + coef(fit.p.nonmsm)["ageidage4"]
    int["55+ years",,"female",] <- int["55+ years",,"female",] + coef(fit.p.nonmsm)["ageidage5"]
    
    int[,,"female","never_IDU"] <- int[,,"female","never_IDU"] + coef(fit.p.nonmsm)["female"]
    # int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit2.big.df)["nonmsm"]
    # int[,,"msm",] <- int[,,"msm",] + 0 # msm is sexrisk REF
    
    # int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.p.nonmsm)["idu"]
    int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.p.nonmsm)["idu"]
    int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.p.nonmsm)["idu"] #coef(fit2.big.df)[14] (NA)

    slope[,,"msm","never_IDU"] <- slope[,,"msm","never_IDU"] + coef(fit.p.msm)["year"]
    slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit.p.nonmsm)["year"]
    slope[,,"female",] <- slope[,,"female",] + coef(fit.p.nonmsm)["year"]
    
    # slope[,,"female",] <- slope[,,"female",] + coef(fit2.big.df)["year:nonmsm"]
    # slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit2.big.df)["year:nonmsm"]
    
    # 
    # # Make and return the functional form object
    # create.logistic.linear.functional.form(
    #     intercept = int,
    #     slope = slope,
    #     anchor.year = anchor.year,
    #     min = 0,
    #     max = max.prep.coverage, 
    #     parameters.are.on.logit.scale = T
    # ) 
    
    create.logistic.tail.functional.form(
      # everything else the same
      intercept = int,
      slope = slope,
      anchor.year = anchor.year,
      min = 0,
      max = max.prep.coverage,
      # parameters.are.on.logit.scale = T,
      logistic.after.frac.of.span = 0.8
    )
    
    # create.linear.functional.form(
    #   intercept = int,
    #   slope = slope, anchor.year = anchor.year, min = 0, max = max.prep.coverage
    # )
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
  int[,,"msm",] <- int[,,"msm",] + coef(fit.pi.msm)["(Intercept)"]
  int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.pi.nonmsm)["(Intercept)"]
  int[,,"female",] <- int[,,"female",] + coef(fit.pi.nonmsm)["(Intercept)"]
  
  int[,"black","msm",] <- int[,"black","msm",] + coef(fit.pi.msm)["raceidblack"]
  int[,"hispanic","msm",] <- int[,"hispanic","msm",] + coef(fit.pi.msm)["raceidhisp"]
  int[,"other","msm",] <- int[,"other","msm",] + coef(fit.pi.msm)["raceidnbnh"]
  
  int[,"black","heterosexual_male",] <- int[,"black","heterosexual_male",] + coef(fit.pi.nonmsm)["raceidblack"]
  int[,"hispanic","heterosexual_male",] <- int[,"hispanic","heterosexual_male",] + coef(fit.pi.nonmsm)["raceidhisp"]
  int[,"other","heterosexual_male",] <- int[,"other","heterosexual_male",] + coef(fit.pi.nonmsm)["raceidnbnh"]
  
  int[,"black","female",] <- int[,"black","female",] + coef(fit.pi.nonmsm)["raceidblack"]
  int[,"hispanic","female",] <- int[,"hispanic","female",] + coef(fit.pi.nonmsm)["raceidhisp"]
  int[,"other","female",] <- int[,"other","female",] + coef(fit.pi.nonmsm)["raceidnbnh"]
  
  int["13-24 years",,"msm",] <- int["13-24 years",,"msm",] + coef(fit.pi.msm)["ageidage1"]
  int["25-34 years",,"msm",] <- int["25-34 years",,"msm",] + coef(fit.pi.msm)["ageidage2"]
  int["35-44 years",,"msm",] <- int["35-44 years",,"msm",] + coef(fit.pi.msm)["ageidage3"]
  int["45-54 years",,"msm",] <- int["45-54 years",,"msm",] + coef(fit.pi.msm)["ageidage4"]
  int["55+ years",,"msm",] <- int["55+ years",,"msm",] + coef(fit.pi.msm)["ageidage5"]
  
  int["13-24 years",,"heterosexual_male",] <- int["13-24 years",,"heterosexual_male",] + coef(fit.pi.nonmsm)["ageidage1"]
  int["25-34 years",,"heterosexual_male",] <- int["25-34 years",,"heterosexual_male",] + coef(fit.pi.nonmsm)["ageidage2"]
  int["35-44 years",,"heterosexual_male",] <- int["35-44 years",,"heterosexual_male",] + coef(fit.pi.nonmsm)["ageidage3"]
  int["45-54 years",,"heterosexual_male",] <- int["45-54 years",,"heterosexual_male",] + coef(fit.pi.nonmsm)["ageidage4"]
  int["55+ years",,"heterosexual_male",] <- int["55+ years",,"heterosexual_male",] + coef(fit.pi.nonmsm)["ageidage5"]
  
  int["13-24 years",,"female",] <- int["13-24 years",,"female",] + coef(fit.pi.nonmsm)["ageidage1"]
  int["25-34 years",,"female",] <- int["25-34 years",,"female",] + coef(fit.pi.nonmsm)["ageidage2"]
  int["35-44 years",,"female",] <- int["35-44 years",,"female",] + coef(fit.pi.nonmsm)["ageidage3"]
  int["45-54 years",,"female",] <- int["45-54 years",,"female",] + coef(fit.pi.nonmsm)["ageidage4"]
  int["55+ years",,"female",] <- int["55+ years",,"female",] + coef(fit.pi.nonmsm)["ageidage5"]
  
  int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.pi.nonmsm)["male"]
  int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.pi.nonmsm)["idu"] 
  
  # int[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.pi.df)[14]
  # int[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + coef(fit.pi.df)[14]
  
  slope[,,"msm",] <- slope[,,"msm",] + coef(fit.pi.msm)["years"]
  slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + coef(fit.pi.nonmsm)["years"]
  slope[,,"female",] <- slope[,,"female",] + coef(fit.pi.nonmsm)["years"]

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
