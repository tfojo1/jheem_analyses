
# A script to be run

CRAN.PACKAGES = c("ggmap", "httr2", "ggnewscale")
GITHUB.PACKAGES = c("tfojo1/distributions", "tfojo1/Bayesian.simulations", "tfojo1/locations", "thk686/odeintr")

print("Installing jheem2 required packages")

for (package in CRAN.PACKAGES) {
    print(paste0("Installing '", package, "'..."))
    trCatch(
        {install.packages(package, lib=Sys.getenv("R_LIBS_USER"))},
        error=function(e) {print(paste0("Error installing package '", package, "' -- skipping..."))})
}

for (package.url in GITHUB.PACKAGES) {
    print(paste0("Installing '", package.url, "'..."))
    tryCatch(
        {devtools::install_github(package.url, lib=Sys.getenv("R_LIBS_USER"))},
        error=function(e) {print(paste0("Error installing package '", package.url, "' -- skipping..."))}
    )
    
}

print("Finished")