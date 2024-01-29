

library(devtools)

# Packages requires for our custom packages
if (nchar(system.file(package='ggmap'))==0)
  install.packages('ggmap')

# Our Custom Packages
install_github('tfojo1/distributions')
install_github('tfojo1/bayesian.simulations')
install_github('tfojo1/locations')

# Other packages required for analyses

# Archival packages - I hope to eventually remove these
install_github('tfojo1/jheem')
if (nchar(system.file(package='sas7bdat'))==0)
  install.packages('sas7bdat')