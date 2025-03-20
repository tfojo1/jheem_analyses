library(dplyr) 
library(reshape2)
library(ggplot2)

source("../jheem_analyses/applications/SHIELD/shield_specification.R")

# CLEANING DATA -----
clean.brfss.data <- function(specification.metadata) {
  load("../jheem_analyses/cached/brfss.subset.RData")
  df <- as.data.frame(appended)
  rm(appended)
  
  # MSM: We calculated this variable. 
  # It estimates men who have sex with men by encompassing anyone who reported their sex as ‘male’ and their sexual orientation 
  # as ‘gay’ or ‘bisexual.’
  
  # Tested.past.year: This is another variable that we calculated. BRFSS asks the month and year of a person’s last HIV test. 
  # Based on their answer and the year of the survey, we calculate the probability that their test was in the past year relative to when 
  # they took the survey. If year is reported but month is unknown, we assign June as the test month because it gives a 0.5 probability 
  # that their test was truly in the past year.
  
  # Weighting.var: This is the weight that is applied to each participant’s response, based on BRFSS’s sampling/weighting mechanisms 
  # (i.e., assigning higher weight to individuals who are underrepresented in their sample or less likely to be surveyed).
  
  # High.risk: There’s a question in BRFSS that directly asks if someone is at risk for HIV. This includes if they have used IV drugs in 
  # the past year, been treated for an STI in the past year, given or received money or drugs in exchange for sex in the past year, 
  # had anal sex without a condom in the past year, or had four or more sex partners in the past year. So this variable includes anyone 
  # who answered yes to any of those criteria.
  
  # Cleaning: 
  # Since missing data for MSM, sex, and race is less than 5%, we opted to remove NAs.
  # Data from 2020-2022 was removed to minimize the impact of COVID-19.
  df <- df%>%
    filter(!is.na(msm), !is.na(sex), !is.na(race), !(age=='Unknown')) %>%
    filter(year < 2020 | year > 2022) %>% 
    dplyr::select(year:race, msm, tested.past.year, weighting.var)
  
  
  # AGE: BRFSS has 14 age categories, whereas Shield has 11 specified categories for age.
  # Based on the Shield ages between 18-24 should be divided into: 15-19 and 20-24 age groups. 
  # There are no data between 0-14 and 15-17 age groups in the BRFSS data. In result, we can just have 18-19 and 20-24 age groups, 
  # in order to be align with Shield Specification.
  # Age Assumption: The age is distributed uniformly between 18-24.
  # Using Row-duplication by ratio:   
  ##   We physically split each row in the “18–24” category into two new rows:
  ##  one row labeled “18–19” with partial weight 2/7 of the original weight
  ##  another row labeled “20–24” with partial weight 5/7 of the original weight.
  df <- df %>%
    filter(age != "18-24 years") %>%
    bind_rows(
      df %>%
        filter(age == "18-24 years") %>%
        mutate(age = "18-19 years",
               weighting.var = weighting.var * (2/7))
    ) %>%
    bind_rows(
      df %>%
        filter(age == "18-24 years") %>%
        mutate(age = "20-24 years",
               weighting.var = weighting.var * (5/7))
    )
  # table(df$age)
  
  # Constructing an age map:
  given.ages = unique(df$age)
  age.map = get.age.bracket.mapping(given.ages, specification.metadata$dim.names$age)
  # using the age map to restratify ages to approperiate buckets:
  df$orig.age = df$age
  df$age = age.map[df$orig.age]
  # table(df$age)
  
  # Preparing categorical age variable for GLM:
  # If you do not specify any particular ordering, R will treat age as unordered, and it will randomly assign one of the levels as the reference level (often the first one).
  # for more interpretability, we do want to order the age categories:
  ## By default, the first level in the age factor is the reference level.
  ## R will assign a numeric rank to each age category (starting from  1 for the first category to 11 for the last one), 
  # and the regression model will estimate how the outcome changes as age increases.
  
  # ref_index <- ceiling(specification.metadata$n.ages / 2)    #index of the middle value
  ref_index <- 1 #'@Melissa: Shouldnt this start from smallest agegroup? 
  age_levels <- c(specification.metadata$dim.names$age[ref_index],
                  specification.metadata$dim.names$age[-ref_index])
  df <- df %>% 
    mutate(age = factor(age, levels = age_levels))
  
  
  # RACE
  df <- df %>% 
    mutate(orig_race = race)
  
  race_ontology_mapping <- get.ontology.mapping(
    from.ontology = list(race = tolower(unique(df$race))),
    to.ontology = specification.metadata$dim.names["race"]
  )
  
  race_map <- race_ontology_mapping$get.mapping.vector()
  
  df <- df %>% 
    mutate(race = race_map[tolower(orig_race)])
  
  race_levels <- union("other", specification.metadata$dim.names$race)
  df <- df %>% 
    mutate(race = factor(race, levels = race_levels)) #"other" is the first (reference) category. 
  # table(df$race)
  
  # SEX
  df <- df %>%
    mutate(orig_sex = sex,
           sex = case_when(
             msm == 1 ~ "msm",
             sex == "male" ~ "heterosexual_male",
             TRUE ~ sex
           )) %>% 
    mutate(sex = factor(sex, levels = c("msm", "heterosexual_male", "female"))) #msm is the first (ref) category. 
  # table(df$sex)
  
  # # YEAR: If we use the year in its original calendar format (e.g., 2000, 2001, etc.), 
  # the regression coefficient estimated for it may be highly sensitive to variations during Bayesian sampling because
  # it is multiplied by a large year value. To mitigate this issue, we use an anchor year, which effectively reduces the 
  # scale of the year variable by starting it from 1. 
  # This approach helps to stabilize the estimates and improve model convergence
  testing_anchor_year <- 2010 
  df <- df %>% 
    mutate(year = year - testing_anchor_year) # year range: 4-9
  
  # table(df$age): no data for 0-14 and 15-19 age groups. 
  # drop unused factor levels
  df <- df %>%
    mutate(across(c(age, race, sex), droplevels))
  
}

# MODEL FITTING ----
# get.testing.intercepts.and.slopes(...) function fits a logistic regression model to the data and returns 
# a list containing two items: a multidimensional array of intercepts (the predicted log-odds when year = 0) 
# and slopes (the change in log-odds per one unit change in year) for each combination of demographic strata.
get.testing.intercepts.and.slopes <- function(df, 
                                              specification.metadata,
                                              selected.model ) {
  
  if (selected.model == "two.way") {
    fit <- glm(tested.past.year ~ age + sex + race + year + 
                 age:sex + age:race + sex:race + year:age + year:sex + year:race,
               data = df,family = "binomial", weights = df$weighting.var)
  } else if (selected.model == "three.way.interacted") {
    fit <- glm(tested.past.year ~ age + sex + race + year +
                 age:sex + age:race + sex:race + year:age + year:sex + year:race +
                 year:age:sex+ year:age:race +year:sex:race + age:sex:race,
               data = df,family = "binomial", weights = df$weighting.var)
  } else if (selected.model == "fully.interacted") {
    fit <- glm(tested.past.year ~ age + sex + race + year +
                 age:sex + age:race + sex:race + 
                 year:age + year:sex + year:race + 
                 year:age:sex + year:age:race + year:sex:race + age:sex:race + 
                 year:age:sex:race,
               data = df,family = "binomial", weights = df$weighting.var)
  } else if (selected.model == "one.way") {
    fit <- glm(tested.past.year ~ age + sex + race + year + 
                 year:age + year:sex + year:race,
               data = df,family = "binomial", weights = df$weighting.var)
  } else {
    stop("model can only be two.way, fully.interacted, or one.way")
  }
  
  # to extract the intercept and slops with respect to one unit change in time, 
  # we set up a data frame with all possible combinations of the strata:
  age_levels_model  <- levels(df$age)
  race_levels_model <- levels(df$race)
  sex_levels_model  <- levels(df$sex)
  dim.names <- list(age = age_levels_model,
                    race = race_levels_model,
                    sex = sex_levels_model)
  iterated_values <- as_tibble(get.every.combination(dim.names))
  
  # predicting with year = 0 cancels out all of the year terms --> gives you the intercepts for each stratum 
  year0.data <- iterated_values %>% 
    mutate(year = 0)
  year1.data <- iterated_values %>% 
    mutate(year = 1)
  
  # this will work, even with interaction terms as long as you use a standard linear year term (not squared, splined, etc.), 
  # For a binomial GLM (which is often used for binary outcomes like yes/no or success/failure), the default link function is the logit link, which is the log-odds transformation.
  #(If you want probabilities (i.e., predicted outcomes on the probability scale), you would use type = 'response')
  #This would return the predicted probabilities of the outcome being 1 (for example, "tested" or "success") for each combination of age, sex, and race when year = 0.
  intercepts <- predict(fit, newdata = year0.data, type = "link")
  slopes     <- predict(fit, newdata = year1.data, type = "link") - intercepts
  
  dim(intercepts) = dim(slopes) = sapply(dim.names, length)
  dimnames(intercepts) = dimnames(slopes) = dim.names
  
  #'Log-odds of -1.27 means the odds of testing in the past year are about 0.280 to 1, or the event is about 28% as likely to happen as it is to not happen.
  # The probability of testing in the past year for this baseline group (before adjusting for other predictors) is about 21.9%.
  list(intercepts = intercepts,
       slopes = slopes )
}


# CHECKING MODEL PERFORMANCE ----
checking.model.performance<- function(df, specification.metadata, selected.model) {
  print(paste("checking model performance for ", selected.model, " model ...."))
  anchor.year= 2010
  proj.years <- 2010:2035
  #
  print(paste0("Testing the ", selected.model, " model..."))
  
  age_levels_model  <- levels(df$age)
  race_levels_model <- levels(df$race)
  sex_levels_model  <- levels(df$sex)
  dim.names <- list(age = age_levels_model,
                    race = race_levels_model,
                    sex = sex_levels_model)
  
  testing.prior <- get.testing.intercepts.and.slopes(df = df,
                                                     specification.metadata = specification.metadata,
                                                     selected.model)
  print("Model is fitted")
  testing.functional.form <- create.logistic.linear.functional.form(
    intercept = testing.prior$intercepts,
    slope = testing.prior$slopes,
    anchor.year = anchor.year,
    parameters.are.on.logit.scale = TRUE
  )
  
  # Projected values
  values <- testing.functional.form$project(proj.years)
  values <- array(unlist(values),
                  dim = c(sapply(dim.names, length), length(proj.years)),
                  dimnames = c(dim.names, list(year = proj.years)))
  
  # Data means: weighted mean
  brfss_means <- sapply((unique(df$year)), function(year) {
    sapply(dim.names$sex, function(sex) {
      sapply(dim.names$race, function(race) {
        sapply(dim.names$age, function(age) {
          weighted.mean(df$tested.past.year[df$year == year & df$sex == sex & df$race == race & df$age == age],w=df$weighting.var[df$year == year & df$sex == sex & df$race == race & df$age == age])
        })
      })
    })
  })
  dim(brfss_means) = c(sapply(dim.names, length), length(2014:2019))
  dimnames(brfss_means) = c(dim.names, list(year = 2014:2019))
  df_brfss <- as_tibble(as.data.frame.table(brfss_means, responseName = "value")) 
  
  print("Begin Plotting....")
  plots_age <-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c("age","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = age)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("age","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = age)) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Age) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  plots_race <-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c("race","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = race)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("race","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = race)) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Race) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  plots_sex <-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c("sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Sex) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  plots_race_sex <-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c( "race","sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("race","sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    facet_wrap(~race) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Sex & Race) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  plots_age_sex <-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c( "age","sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("age","sex","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = sex)) +
    facet_wrap(~age) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Sex & Age) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  plots_age_race<-  ggplot() +
    geom_line(data = reshape2::melt(apply(values, c( "age","race","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = race)) +
    geom_point(data = reshape2::melt(apply(brfss_means, c("age","race","year"),mean)), aes(x = as.numeric(as.character(year)), y = value, color = race)) +
    facet_wrap(~age) +
    ylim(0, 1) +
    ggtitle(paste("Testing Projection vs. BRFSS (Race & Age) -", selected.model)) +
    theme(plot.title = element_text(hjust = 0.5, size = 15))+xlab("Year") + ylab("Proportion")  
  
  PLOTS<-list(plots_age, plots_race,plots_sex,plots_race_sex,plots_age_sex,plots_age_race)
  PLOTS.Names<-list("plots_age", "plots_race","plots_sex","plots_race_sex","plots_age_sex","plots_age_race")
  lapply(1:length(PLOTS),function(x){
    ggsave(filename = paste0("prelim_results/testing_prior_",selected.model,"_",PLOTS.Names[x],".jpeg"),
           plot = PLOTS[[x]], 
           width = 10, height = 10, dpi = 300)
    print(paste0("Saved ", paste0("prelim_results/testing_prior_",selected.model,"_",PLOTS.Names[x],".jpeg")))
  })
  print("All plots are saved")
}



# CASHING FINAL MODELS ----
# creates a specification metadata for shield that includes all the necessary information on dimensions
specification.metadata <- get.specification.metadata(version = 'shield', location = 'US')

# reading and cleaning the BRFSS data:
df <- clean.brfss.data(specification.metadata)

## Checking performances: fitting each model takes a long time
checking.model.performance(df,specification.metadata,selected.model = "two.way")
checking.model.performance(df,specification.metadata,"three.way.interacted")
checking.model.performance(df,specification.metadata,"fully.interacted")

#fitting the final model 
selected.model="three.way.interacted"
print("final model is selected as three.way.interacted ....")
testing.prior <- get.testing.intercepts.and.slopes(df = df,
                                                   specification.metadata = specification.metadata,
                                                   selected.model = selected.model)
cache.object.for.version(object = testing.prior,
                         name = "hiv.testing.prior",
                         version = 'shield',
                         overwrite=T)
print("cashed testing prior for SHIELD")
