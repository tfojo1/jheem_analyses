


# Packages required for our custom packages
if (nchar(system.file(package='ggmap'))==0)
  install.packages('ggmap')

# Our Custom Packages
devtools::install_github('tfojo1/distributions')
devtools::install_github('tfojo1/bayesian.simulations')
devtools::install_github('tfojo1/locations')

# JULIA
# install.packages('JuliaCall')
# julia_setup(installJulia = TRUE)
# install.packages('diffeqr')
# On a mac - navigate to the directory where Julia is installed, right click on Julia icon, select open, and override Mac safety to allow it to open



# Other packages required for analyses


devtools::install_github('thk686/odeintr')



# Archival packages - I hope to eventually remove these
#install_github('tfojo1/jheem')
#if (nchar(system.file(package='sas7bdat'))==0)
#  install.packages('sas7bdat')

