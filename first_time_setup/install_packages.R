

library(devtools)

# Packages requires for our custom packages
install.packages('ggmap')

# Our Custom Packages
install_github('tfojo1/distributions')
install_github('tfojo1/bayesian.simulations')
install_github('tfojo1/locations')

# Other packages required for analyses

# Archival packages - I hope to eventually remove these
install_github('tfojo1/jheem')
install.packages('sas7bdat')