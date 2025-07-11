# Correlation plots

# Example x parameters:
# proportion over 55 in 2025
# proportion of new diagnoses under 35 in 2025
# proportion of new diagnoses vs diagnosed prevalence in 2025

# Example y parameters:
# change in median age 2040 vs 2025

dd <- Reduce(rbind, lapply(my_states, function(state) {
    # Get data
    arr <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                   keep.dimensions=c('year', 'age'),
                                   dimension.values=list(year=c(2025,2040)))
    new_arr <- noint_list[[state]]$get(outcomes='new',
                                       keep.dimensions=c('year', 'age'),
                                       dimension.values=list(year=c(2025,2040)))
    trans_arr <- noint_list[[state]]$get(outcomes='sexual.transmission.rates',
                                         keep.dimensions=c('year'),
                                         dimension.values=list(year=c(2025, 2040)))
    ratio_arr <- noint_list[[state]]$get(outcomes=c('incidence', 'infected'),
                                         keep.dimensions='year',
                                         dimension.values=list(year=c(2025,2040)))
    # browser()
    # Median Age and Delta Median Age
    med_age_df <- reshape2::melt(get_stats(get_med_age(arr))) %>%
        filter(metric=='median') %>%
        pivot_wider(names_from=year) %>%
        mutate(medAge2040=`2040`, deltaMedAge=`2040`-`2025`) %>%
        select(-`2040`, -`2025`)
    
    # Proportion Diagnosed Prev Over 55
    prop_over_55 <- reshape2::melt(get_stats(get_prop_over_55(arr), digits=2)) %>%
        filter(year=='2025', metric=='median')
    
    # Incidence to Prevalence Ratio
    ratio_arr <- ratio_arr[,,'incidence']/ratio_arr[,,'infected']
    ratio_df <- reshape2::melt(get_stats(ratio_arr, digits=3)) %>%
        filter(year=='2025', metric=='median')
    
    # Proportion New Diagnoses Under 35
    new_df <- reshape2::melt(get_stats(get_prop_under_35(new_arr), digits=2)) %>%
        filter(year=='2025', metric=='median')
    
    # Sexual transmission rates
    trans_df <- reshape2::melt(get_stats(trans_arr, digits=3)) %>%
        filter(year=='2025', metric=='median')
    
    # browser()
    # Merge with renames using merge/mutate/select combos
    df <- merge(med_age_df, prop_over_55, by="metric") %>%
        mutate(propOver55in2025=value) %>%
        select(-year, -value) %>%
        
        merge(new_df, by="metric") %>%
        mutate(propNewUnder35in2025=value) %>%
        select(-year, -value) %>%
        
        merge(ratio_df, by="metric") %>%
        mutate(ratioIncToPrev=value) %>%
        select(-year, -value) %>%
        
        merge(trans_df, by="metric") %>%
        mutate(sexTransRatein2025=value) %>%
        select(-year, -value) %>%
        
        bind_cols(state=state) %>%
        mutate(stateName = get.location.name(state))
}))

make_scatPlot <- function(x.param, y.param, states="GA") {
    ggplot(data=filter(dd, state%in%states), mapping=aes(y=!!sym(y.param), x=!!sym(x.param))) +
        geom_point() +
        ggtitle(paste0("Diagnosed Prevalence")) +
        # labs(y = "cases", x = "Year", fill="Age") +
        theme_bw() +
        geom_smooth(method="lm", col="blue")
}

make_scatPlot('propOver55in2025', 'medAge2040')
make_scatPlot('propOver55in2025', 'deltaMedAge')
make_scatPlot('propNewUnder35in2025', 'deltaMedAge')
make_scatPlot('sexTransRatein2025', 'deltaMedAge')
make_scatPlot('sexTransRatein2025', 'medAge2040')
make_scatPlot('ratioIncToPrev', 'deltaMedAge')
cor(dd$propOver55in2025, dd$medAge2040) # 0.77
cor(dd$propOver55in2025, dd$deltaMedAge) # 0.52
cor(dd$propNewUnder35in2025, dd$deltaMedAge) # -0.80
cor(dd$sexTransRatein2025, dd$deltaMedAge) # -0.51
cor(dd$sexTransRatein2025, dd$medAge2040) # -0.47
cor(dd$ratioIncToPrev, dd$deltaMedAge) # -0.62

# Change in incidence (ratio)

## 6-17-2025 Make regression?
model <- lm(data=dd, ratioIncToPrev ~ deltaMedAge)
summary(model)