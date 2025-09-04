# Compare delta median age to median age 2025

library(tidyverse)
library(smplot2)

make_scatterplot <- function(x.param, x.lab,
                             y.param="deltaMedAge", y.lab="Delta Median Age",
                             color.by = NULL,
                             data = scatter_df,
                             show.state.labels = T) {
    plot <- ggplot(data=data,
                   mapping=aes(y=!!sym(y.param), x=!!sym(x.param))) +
        labs(y=y.lab, x=x.lab) +
        theme_bw() +
        sm_statCorr()
    if (show.state.labels)
        plot <- plot + geom_text(aes(label=location), vjust=1.5, hjust=1.5)
    if (!is.null(color.by)) {
        plot <- plot +
            geom_point(aes(size = pointSize,
                           color = !!sym(color.by)))}
    else
        plot <- plot +
            geom_point(aes(size = pointSize))
    plot
}

# load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_dataOG.Rdata")
# load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_dataOG.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_data.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")

# prev25 <- apply(total_results["2025",,"diagnosed.prevalence",,], "location", mean)
# 
# scatter_df <- med_age_timeline_data %>%
#     filter(year=="2025") %>%
#     select(location, mean, stateName)
# 
# scatter_df <- merge(scatter_df,
#                     select(med_age_delta_data, location, mean),
#                     by="location") %>%
#     rename(medAge2025 = mean.x,
#            deltaMedAge = mean.y) %>%
#     filter(location != "total") %>%
#     cbind(pointSize = sqrt(prev25) / 100)
# make_scatterplot("medAge2025", "Median Age in 2025")

# This shows that states with "older" epidemics in 2025 tend to grow even older,
# but the correlation is only R=0.59. Illinois and Wisconsin start with nearly
# the same median age in 2025 (IL: 48, WI: 49), but Illinois gets 11 years older
# while Wisconsin gets 8 years younger.