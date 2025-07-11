# Figure for variation by state
# 4-6 panels, each with delta prop 55 on y axis
# one point per state (mean)
# size of points proportional to sqrt of prop 55 in 2025
# x axes:
# - new:prev ratio
# - current prop55
# - current num55
# - urbanicity of states
# - general population prop55

make_scatterplot <- function(x.param, x.lab, y.param="deltaProp", y.lab="Delta Proportion 55+") {
    ggplot(data=scatterplot_df,
           mapping=aes(y=!!sym(y.param), x=!!sym(x.param))) +
        geom_point(aes(size = pointSize)) +
        labs(y=y.lab, x=x.lab) +
        theme_bw() +
        sm_statCorr() +
        geom_text(aes(label=location), vjust=1.5, hjust=1.5)
}

prev_arr <- get_stats(total_results[c("2025", "2040"),,"diagnosed.prevalence",,],
                      keep.dimensions=c("year", "location"))
prev_df <- reshape2::melt(prev_arr) %>%
    filter(metric=="mean") %>%
    pivot_wider(names_from = year) %>%
    mutate(prev25 = `2025`, prev40 = `2025`) %>%
    mutate(pointSize = sqrt(prev25) / 100) %>%
    select(pointSize, prev25, prev40, location)

prop_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                             keep.dimensions=c("year", "location"))

prop_df <- reshape2::melt(get_stats(prop_arr[c("2025", "2040"),,],
                                    keep.dimensions = c("year", "location"),
                                    round = F)) %>%
    filter(metric == "mean") %>%
    pivot_wider(names_from = year) %>%
    mutate(prop25 = `2025`, prop40 = `2040`) %>%
    select(prop25, prop40, location)

deltaProp_df <- reshape2::melt(get_stats(prop_arr["2040",,,drop=F]-prop_arr["2025",,,drop=F],
                                         keep.dimensions = c("year", "location"),
                                         round = F)) %>%
    filter(metric == "mean") %>%
    mutate(deltaProp = value) %>%
    select(location, deltaProp)

genProp_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"population",,],
                             keep.dimensions=c("year", "location"))

deltaGenProp_df <- reshape2::melt(get_stats(genProp_arr["2040",,,drop=F]-genProp_arr["2025",,,drop=F],
                                         keep.dimensions = c("year", "location"),
                                         round = F)) %>%
    filter(metric == "mean") %>%
    mutate(deltaGenProp = value) %>%
    select(location, deltaGenProp)

num_arr <- get_num_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                           keep.dimensions=c("year", "location"))

num_df <- reshape2::melt(get_stats(num_arr[c("2025", "2040"),,],
                                   keep.dimensions = c("year", "location"),
                                   round = F)) %>%
    filter(metric == "mean") %>%
    pivot_wider(names_from = year) %>%
    mutate(num25 = `2025`, num40 = `2040`) %>%
    select(num25, num40, location)

ratio_arr <- get_stats(100 * total_results[c("2025", "2040"),,"new",,]/
                           total_results[c("2025", "2040"),,"diagnosed.prevalence",,],
                       keep.dimensions=c("year", "location"),
                       round=F)
ratio_df <- reshape2::melt(ratio_arr) %>%
    filter(metric=="mean") %>%
    pivot_wider(names_from = year) %>%
    mutate(ratio25 = `2025`, ratio40 = `2025`) %>%
    select(ratio25, ratio40, location)

popProp_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"population",,],
                                keep.dimensions=c("year", "location"))

popProp_df <- reshape2::melt(get_stats(popProp_arr[c("2025", "2040"),,],
                                       keep.dimensions = c("year", "location"),
                                       round = F)) %>%
    filter(metric == "mean") %>%
    pivot_wider(names_from = year) %>%
    mutate(popProp25 = `2025`, popProp40 = `2040`) %>%
    select(popProp25, popProp40, location)

# Had to fix up what I received over email to add location column
urbanicity_df <- get(load("../jheem_analyses/applications/age_analysis/urban_metric.RData"))

scatterplot_df <- prev_df %>%
    merge(deltaProp_df, by="location") %>%
    merge(prop_df, by="location") %>%
    merge(num_df, by="location") %>%
    merge(ratio_df, by="location") %>%
    #merge(popProp_df, by="location") %>%
    merge(urbanicity_df, by="location") %>%
    merge(deltaGenProp_df, by="location")

# ggarrange(make_scatterplot("prop25", "Proportion 55+ in 2025"),
#           make_scatterplot("ratio25", "Ratio new diagnoses to prevalence in 2025"),
#           make_scatterplot("num25", "Prevalence 55+ in 2025"),
#           make_scatterplot("popProp25", "Proportion 55+ in General Population 2025"),
#           make_scatterplot("urbanicity", "Urbanicity Metric"),)
make_scatterplot("prop25", "Proportion 55+ in 2025")
make_scatterplot("ratio25", "Ratio new diagnoses to prevalence in 2025")
make_scatterplot("num25", "Prevalence 55+ in 2025")
make_scatterplot("popProp25", "Proportion 55+ in General Population 2025")
make_scatterplot("urbanicity", "Urbanicity Metric")

scatter_figure_df <- scatterplot_df %>%
    select(-prev25, -prev40, -prop40, -ratio40, -num40) %>%
    pivot_longer(cols = prop25:deltaGenProp)

urbanicity_scatter_figure_df <- scatter_figure_df %>%
    pivot_wider(names_from = name, values_from = value) %>%
    pivot_longer(cols = c(deltaProp:deltaGenProp, -urbanicity))

labeller_labels <- c(num25 = "Diagnosed Prevalence in 2025 (cases)",
                     popProp25 = "Proportion General Population 55+ in 2025 (%)",
                     prop25 = "Proportion 55+ in 2025 (%)",
                     ratio25 = "Ratio New Diagnoses to Prevalence in 2025 (%)",
                     urbanicity = "Urbanicity Metric in 2025",
                     deltaGenProp = "Delta Proportion 55+, General Population (%)",
                     deltaProp = "Delta Proportion 55+ (%)")

make_scatterFigure <- function() {
    ggplot(data=scatter_figure_df,
           mapping=aes(y=deltaProp, x=value)) +
        geom_point(aes(size = pointSize)) +
        theme_bw() +
        facet_wrap(vars(name), scales="free_x", labeller = as_labeller(labeller_labels)) +
        sm_statCorr() +
        labs(y = "Delta Proportion 55+ (%)") +
        ggtitle("State Variation") +
        geom_text(aes(label=location), vjust=1.5, hjust=1.5)
}
make_scatterFigure()

make_urbanicity_scatterFigure <- function() {
    ggplot(data=urbanicity_scatter_figure_df,
           mapping=aes(y=value, x=urbanicity)) +
        geom_point(aes(size = pointSize)) +
        theme_bw() +
        facet_wrap(vars(name), scales="free_y", labeller = as_labeller(labeller_labels)) +
        sm_statCorr() +
        labs(x = "Urbanicity Metric in 2025") +
        ggtitle("State Variation") +
        geom_text(aes(label=location), vjust=1.5, hjust=1.5)
}
make_urbanicity_scatterFigure()
