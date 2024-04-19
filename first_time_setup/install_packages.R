

library(devtools)

# Packages required for our custom packages
if (nchar(system.file(package='ggmap'))==0)
  install.packages('ggmap')

# Our Custom Packages
install_github('tfojo1/distributions')
install_github('tfojo1/bayesian.simulations')
install_github('tfojo1/locations')

# JULIA
install.packages('JuliaCall')
julia_setup(installJulia = TRUE)
install.packages('diffeqr')
# On a mac - navigate to the directory where Julia is installed, right click on Julia icon, select open, and override Mac safety to allow it to open



# Other packages required for analyses

# Archival packages - I hope to eventually remove these
#install_github('tfojo1/jheem')
#if (nchar(system.file(package='sas7bdat'))==0)
#  install.packages('sas7bdat')

