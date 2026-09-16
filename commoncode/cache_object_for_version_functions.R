#' @details The maximum allowed file size is 10 MB.
cache.object.for.version <- function(object, name, version, overwrite=F)
{
    error.prefix = "Error caching object for version: "
    BASE.FILE.PATH = file.path(JHEEM.ANALYSES.PATH, "commoncode/object_for_version_cache")
    FILE.PATH = file.path(BASE.FILE.PATH, paste0(version, "_", name, ".Rdata"))
    
    if (!overwrite) {
        if (file.exists(FILE.PATH))
            stop(paste0(error.prefix, "file already exists"))
    }
    
    # We have to check file size AFTER saving the file
    save(object, file=FILE.PATH)
    if (file.size(FILE.PATH) > 1e7) {
        file.remove(FILE.PATH)
        stop(paste0("Cannot 'cache.object.for.version': file size would exceed limit of 10 MB"))
    }
}

get.cached.object.for.version <- function(name, version)
{
    BASE.FILE.PATH = file.path(JHEEM.ANALYSES.PATH, "commoncode/object_for_version_cache")
    
    error.prefix = "Error getting cached object for version: "
    FILE.PATH = file.path(BASE.FILE.PATH, paste0(version, "_", name, ".Rdata"))
    
    if (file.exists(FILE.PATH))
    {
        load(file=FILE.PATH) # will be called "object"
        return (object)
    }
    
    if (version.has.been.registered(version))
    {
        for (version.to.try in get.prior.versions(version))
        {
            FILE.PATH = file.path(BASE.FILE.PATH, paste0(version.to.try, "_", name, ".Rdata"))
        
            if (file.exists(FILE.PATH))
            {
                load(file=FILE.PATH) # will be called "object"
                return (object)
            }
        }
    
        # if we got here, we didn't find anything
        stop(paste0("Cannot get cached '", name, "' for version '", version, 
                    "' - no object by this name has been cached for the version or any ancestor versions"))
    }
    else
    {
      stop(paste0("Cannot get cached '", name, "' for version '", version, 
                  "' - no object by this name has been cached for the version. HOWEVER, '",
                  version, "' has NOT been registered, so we cannot check whether '", name, "' is present in prior versions"))
    }
}
