
# A script to be run with "source <this file>"

ENV = Sys.getenv("R_LIBS_USER")
INSTALLED.PACKAGES = rownames(installed.packages(lib.loc=ENV))
CRAN.PACKAGES = c("ggmap", "httr2", "ggnewscale","deSolve")
GITHUB.PACKAGES = c("tfojo1/distributions", "tfojo1/Bayesian.simulations", "tfojo1/locations","tfojo1/jheem2")

print("Installing required packages")

for (package in setdiff(CRAN.PACKAGES, INSTALLED.PACKAGES)) {
    print(paste0("Installing '", package, "'..."))
    tryCatch(
        {install.packages(package, repo='https://cran.rstudio.com', lib=ENV)},
        error=function(e) {print(paste0("Error installing package '", package, "' -- skipping..."))})
}

for (package.url in setdiff(GITHUB.PACKAGES, INSTALLED.PACKAGES)) {
    print(paste0("Installing '", package.url, "'..."))
    tryCatch(
        {devtools::install_github(package.url, lib=ENV)},
        error=function(e) {print(paste0("Error installing package '", package.url, "' -- skipping..."))}
    )
}

print("Finished")

# devtools::install_github("tfojo1/distributions", lib=Sys.getenv("R_LIBS_USER"))
#if receving an error, it could be the devtool:
# install.packages("devtools", lib=Sys.getenv("R_LIBS_USER"))
