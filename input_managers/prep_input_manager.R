
#-- Test Code --#
if (1==2)
{
    source('source_code.R')
    source('applications/EHE/ehe_specification.R')
    metadata = get.specification.metadata('ehe', location ='c.12580')
    source('prep_data.R')
  
    ff <-  get.prep.use.functional.form(specification.metadata = metadata)
    x <- ff$project(2020:2030, alphas = NULL, dim.names = ff$minimum.dim.names)

    # length(x)
    # x[[1]]
    # sapply(x, max)
    # sapply(x, min)
    # sapply(x, mean)
    y <- sapply(x, function(z) {return(z)})
    dim.names <- c(ff$minimum.dim.names, list('year'=2020:2030))
    dim(y) <- sapply(dim.names, length)
    dimnames(y) <- dim.names
    
    y2 <- apply(y, c('year','race'), mean)
    # y2
    
    df <- reshape2::melt(y2)
    # df.pts <- subset(p.msm.df.long, raceid != "ALL") |> dplyr::mutate(year = year + 2020)
    race.plot <- ggplot(df, aes(x=year, y=value, color=race)) + geom_line(linewidth = 1) +
      ylim(min(df$value),max(df$value)) + 
      # geom_point(aes(x=year, y = p, color=raceid), data = df.pts)
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal() 
    
    df3 <- reshape2::melt(apply(y, c('year','sex'), mean))
    sex.plot <- ggplot(df3, aes(x=year, y=value, color=sex)) + geom_line(linewidth = 1) + 
      ylim(min(df3$value),max(df3$value)) + 
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal()
    
   
    df4 <- reshape2::melt(apply(y, c('year','risk'), mean))
    risk.plot <- ggplot(df4, aes(x=year, y=value, color=risk)) + geom_line(linewidth = 1) + 
      ylim(min(df4$value),max(df4$value)) + 
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal()
    
    df5 <- reshape2::melt(apply(y, c('year','age'), mean))
    age.plot <- ggplot(df5, aes(x=year, y=value, color=age)) + geom_line(linewidth = 1) + 
      ylim(min(df5$value),max(df5$value)) + 
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal() 
    
    # arrange all 4 plots
    ggpubr::ggarrange(race.plot, sex.plot, risk.plot, age.plot,
                      ncol = 2, nrow = 2, labels = c("Race","Sex","Risk","Age"))
  
    msm.race <- reshape2::melt(apply(y[,,'msm',,], c('year','race'), mean))
    msm.race.plot <- ggplot(msm.race, aes(year, value, color = race)) + geom_line(linewidth = 1) + 
      ylim(min(msm.race$value),max(msm.race$value)) + 
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal() 
    
    msm.age <- reshape2::melt(apply(y[,,'msm',,], c('year','age'), mean))
    msm.age.plot <- ggplot(msm.age, aes(year, value, color = age)) + geom_line(linewidth = 1) + 
      ylim(min(msm.age$value),max(msm.age$value)) + 
      scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal() 
    
    msm.sex <- reshape2::melt(apply(y[,,'msm',,], c('year'), mean))
    msm.sex$year <- rownames(msm.sex) 
    msm.sex.plot <- ggplot(msm.sex, aes(year, value, color = 'black')) + 
      geom_line(group = 1, linewidth =1)  + 
      ylim(min(msm.sex$value),max(msm.sex$value)) +
      # scale_x_continuous(breaks = seq(2020, 2030, 1)) +
      theme_minimal()
    msm.sex.plot
    
    ggpubr::ggarrange(msm.race.plot, msm.age.plot, msm.sex.plot,
                      ncol=3, nrow=1)
  
    
}

#-- The Functions to Implement --#


get.prep.use.functional.form <- function(specification.metadata)
{
    # Some variables you will use
    anchor.year = 2020 # "year" should be relative to this. ie, 2021 will be 1 (2021-anchor.year)
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
    slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + 0 #coef(fit.big.df)[12]
    slope[,,"msm",] <- slope[,,"msm",] + coef(fit.big.df)[13]
    
    slope[,,,"active_IDU"] <- slope[,,,"active_IDU"] + coef(fit.big.df)[14]
    
    # # MSM
    # 
    # int[,,'msm',] <- int[,,'msm',] + coef(fit.p.msm)[1]
    # 
    # slope[,'black','msm',] <- slope[,'black','msm',] + coef(fit.p.msm)[3]
    # slope[,'hispanic','msm',] <- slope[,'hispanic','msm',] + coef(fit.p.msm)[4]
    # slope[,'other','msm',] <- slope[,'other','msm',] + coef(fit.p.msm)[5]
    # 
    # slope["13-24 years",,'msm',] <- slope["13-24 years",,'msm',] + coef(fit.p.msm)[6]
    # slope["25-34 years",,'msm',] <- slope["25-34 years",,'msm',] + coef(fit.p.msm)[7]
    # slope["35-44 years",,'msm',] <- slope["35-44 years",,'msm',] + coef(fit.p.msm)[8]
    # slope["45-54 years",,'msm',] <- slope["45-54 years",,'msm',] + coef(fit.p.msm)[9]
    # slope["55+ years",,'msm',] <- slope["55+ years",,'msm',] + 0 #coef(fit.p.msm)[10]
    # 
    # # PWID 
    # 
    # int[,,,"active_IDU"] <- int[,,,"active_IDU"] + coef(fit.p.idu)[1]
    # 
    # slope[,'black',,'active_IDU'] <- slope[,'black',,'active_IDU'] + coef(fit.p.idu)[3]
    # slope[,'hispanic',,'active_IDU'] <- slope[,'hispanic',,'active_IDU'] + coef(fit.p.idu)[4]
    # slope[,'other',,'active_IDU'] <- slope[,'other',,'active_IDU'] + coef(fit.p.idu)[5]
    # 
    # slope["13-24 years",,,'active_IDU'] <- slope["13-24 years",,,'active_IDU'] + coef(fit.p.idu)[6]
    # slope["25-34 years",,,'active_IDU'] <- slope["25-34 years",,,'active_IDU'] + coef(fit.p.idu)[7]
    # slope["35-44 years",,,'active_IDU'] <- slope["35-44 years",,,'active_IDU'] + coef(fit.p.idu)[8]
    # slope["45-54 years",,,'active_IDU'] <- slope["45-54 years",,,'active_IDU'] + coef(fit.p.idu)[9]
    # slope["55+ years",,,'active_IDU'] <- slope["55+ years",,,'active_IDU'] + coef(fit.p.idu)[10]
    # 
    # slope[,,"heterosexual_male","active_IDU"] <- int[,,"heterosexual_male","active_IDU"] + 0 #coef(fit.p.idu)[12]
    # slope[,,"female","active_IDU"] <- int[,,"female","active_IDU"] + coef(fit.p.idu)[11]
    # 
    # # Het
    # 
    # int[,,"heterosexual_male",] <- int[,,"heterosexual_male",] + coef(fit.p.het)[1]
    # int[,,"female",] <- int[,,"female",] + coef(fit.p.het)[1]
    # 
    # slope[,,"heterosexual_male",] <- slope[,,"heterosexual_male",] + 0 #coef(fit.p.het)[12]
    # slope[,,"female",] <- slope[,,"female",] + coef(fit.p.het)[11]
    # 
    # slope[,"black","heterosexual_male",] <- slope[,"black","heterosexual_male",] + coef(fit.p.het)[3]
    # slope[,"hispanic","heterosexual_male",] <- slope[,"hispanic","heterosexual_male",] + coef(fit.p.het)[4]
    # slope[,"other","heterosexual_male",] <- slope[,"other","heterosexual_male",] + coef(fit.p.het)[5]
    # 
    # slope[,"black","female",] <- slope[,"black","female",] + coef(fit.p.het)[3]
    # slope[,"hispanic","female",] <- slope[,"hispanic","female",] + coef(fit.p.het)[4]
    # slope[,"other","female",] <- slope[,"other","female",] + coef(fit.p.het)[5]
    # 
    # slope["13-24 years",,"heterosexual_male",] <- slope["13-24 years",,"heterosexual_male",] + coef(fit.p.het)[6]
    # slope["25-34 years",,"heterosexual_male",] <- slope["25-34 years",,"heterosexual_male",] + coef(fit.p.het)[7]
    # slope["35-44 years",,"heterosexual_male",] <- slope["35-44 years",,"heterosexual_male",] + coef(fit.p.het)[8]
    # slope["45-54 years",,"heterosexual_male",] <- slope["45-54 years",,"heterosexual_male",] + coef(fit.p.het)[9]
    # slope["55+ years",,"heterosexual_male",] <- slope["55+ years",,"heterosexual_male",] + coef(fit.p.het)[10]
    # 
    # slope["13-24 years",,"female",] <- slope["13-24 years",,"female",] + coef(fit.p.het)[6]
    # slope["25-34 years",,"female",] <- slope["25-34 years",,"female",] + coef(fit.p.het)[7]
    # slope["35-44 years",,"female",] <- slope["35-44 years",,"female",] + coef(fit.p.het)[8]
    # slope["45-54 years",,"female",] <- slope["45-54 years",,"female",] + coef(fit.p.het)[9]
    # slope["55+ years",,"female",] <- slope["55+ years",,"female",] + coef(fit.p.het)[10]
    
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
