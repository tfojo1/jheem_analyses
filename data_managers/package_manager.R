
CUSTOM.PACKAGES = c(
    'jheem',
    'distributions',
    'bayesian.simulations'
)

update.package.consistency.check <- function(packages = CUSTOM.PACKAGES,
                                             consistency.file = 'code/core_code/data_managers/package.consistency.check.Rdata',
                                             first.time=F,
                                             allow.to.run=first.time) # A flag you have to set manually so we don't run by accident
{
    if (!allow.to.run)
        stop("Make sure NOT TO RUN update.package.consistency.check BY ACCIDENT. You need to set the allow to run flag to be able to run this function.")
    
    if (first.time)
        prior.consistency.check = NULL
    else if (!file.exists(consistency.file))
        stop(paste0("We need a prior consistency.check to update. No consistency check exists at '",
                    consistency.file, "'"))
    else
    {
        load(consistency.file)
        prior.consistency.check = consistency.check
    }
    
    consistency.check = lapply(packages, function(pkg){
        asDateBuilt(packageDescription(pkg)$Built)
    })
    names(consistency.check) = packages
    
    if (!is.null(prior.consistency.check))
    {
        in.both = intersect(names(prior.consistency.check), names(consistency.check))
        not.newer.mask = as.numeric(consistency.check[in.both]) < as.numeric(prior.consistency.check[in.both])
        if (any(not.newer.mask))
            stop(paste0("Error updating consistency.check. The following package(s) were built before when is stated in the prior consistency.check: ",
                        paste0("'", in.both[not.newer.mask], "'", collapse=', ')))
    }
    
    save(consistency.check, file=consistency.file)
    
    print(paste0("Done Updating Package Consistency Check (", length(consistency.check), " packages)"))
}

load.packages <- function(packages.to.load = CUSTOM.PACKAGES,
                          consistency.file = 'code/core_code/data_managers/package.consistency.check.Rdata')
{
    missing.packages.mask = sapply(packages.to.load, function(pkg){
        length(find.package(pkg, quiet=T))==0
    })
    missing.packages = packages.to.load[missing.packages.mask]
    packages.to.load = packages.to.load[!missing.packages.mask]

    # Figure out when they were created
    times.packages.built = lapply(packages.to.load, function(pkg){
        asDateBuilt(packageDescription(pkg)$Built)
    })
    
    # Load the consistency check
    if (!file.exists(consistency.file))
        stop(paste0("No file exists at '", consistency.file, "'. Cannot load the package consistency check."))
    load(consistency.file)
    
    # Check if too old
    missing.from.consistency = setdiff(packages.to.load, names(consistency.check))
    if (length(missing.from.consistency)>0)
        stop(paste0("The following cache element(s) have no record in the consistency.check and cannot be loaded: ",
                    paste0("'", missing.from.consistency, "'", collapse=', '),
                    ". Use update.package.consistency.check() to create a record of these for consistency checking."))
    
    too.old.mask = as.numeric(times.packages.built) < as.numeric(consistency.check[packages.to.load])
    
    if (any(missing.packages.mask) || any(too.old.mask))
    {
        error.msg = NULL
        if (any(missing.packages.mask))
            error.msg = paste0("The following packages have not been installed: ",
                               paste0("'", missing.packages, "'", collapse=', '), ".")
        
        too.old.packages = packages.to.load[too.old.mask]
        if (any(too.old.mask))
        {
            if (any(missing.packages.mask))
                error.msg = paste0(error.msg, " Additionally, the")
            else
                error.msg = "The"
            
            error.msg = paste0(error.msg,
                               " following package(s) are out of date: ",
                               paste0("'", too.old.packages, "'", collapse=', '),
                               ".")
        }
        
        packages.to.get = c(missing.packages, too.old.packages)
        
        if (any(missing.packages.mask) && any(too.old.mask))
            verb = 'install/update'
        else if (any(missing.packages.mask))
            verb = 'install'
        else
            verb = 'update'
        
        error.msg = paste0(error.msg, "\n\nUse the following code to ", verb,
                           " packages:\n\nlibrary(devtools)",
                           paste0("\ninstall_github('tfojo1/", packages.to.get, "')", collapse=''))
        stop(error.msg)
    }
    
    # Load into the desired environment (by default, the global environment)
    for (pkg in packages.to.load)
    {
        library(pkg, character.only = T)
    }
}