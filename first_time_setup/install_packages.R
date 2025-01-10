
# Try Installing JHEEM2 directly
if (nchar(system.file(package='jheem2'))==0)
    devtools::install_github('tfojo1/jheem2')

# CRAN Packages required for our custom packages
if (nchar(system.file(package='ggmap'))==0)
  install.packages('ggmap')

if (nchar(system.file(package='ggnewscale'))==0)
  install.packages('ggnewscale')

if (nchar(system.file(package='httr2'))==0)
    install.packages('httr2')

# Our Custom Packages
if (nchar(system.file(package='distributions'))==0)
    devtools::install_github('tfojo1/distributions')
if (nchar(system.file(package='bayesian.simulations'))==0)
    devtools::install_github('tfojo1/bayesian.simulations')
if (nchar(system.file(package='locations'))==0)
    devtools::install_github('tfojo1/locations')


# Other Github passages
if (nchar(system.file(package='odeintr'))==0)
  devtools::install_github('thk686/odeintr')


# JULIA - not using this right now
# install.packages('JuliaCall')
# julia_setup(installJulia = TRUE)
# install.packages('diffeqr')
# On a mac - navigate to the directory where Julia is installed, right click on Julia icon, select open, and override Mac safety to allow it to open
