# Replicating the Figure 3 of Melissa's Kenya paper
# Using EHE state-level simsets for Georgia
# Andrew 5/22/25

library(ggplot2)

if (!exists("sim")) {
    sim <- retrieve.simulation.set('ehe', 'GA', 'final.ehe.state', n.sim=100)
}


last_sim <- sim$last.sim()
arr <- last_sim$get(outcomes = 'diagnosed.prevalence',
                    keep.dimensions = c('year', 'age'),
                    dimension.values = list(year=c('2025')))

df <- reshape2::melt(arr)

# I'll plot thousands of people
df$value <- df$value / 1000

# Histogram by age
age_hist <- ggplot(data=df) + geom_bar(mapping=aes(x=age, y=value), stat='identity')

# Prettify
age_hist <- age_hist +
    ggtitle("Diagnosed Prevalence in 2025 (Georgia)") +
    ylab("Persons (in thousands)") +
    xlab("Age Group")