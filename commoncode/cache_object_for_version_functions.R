cache.object.for.version <- function(object, name, version, overwrite=F)
{
    error.prefix = "Error caching object for version: "
    BASE.FILE.PATH = '../jheem_analyses/commoncode/object_for_version_cache'
    FILE.PATH = paste0(BASE.FILE.PATH, "/", version, "_", name, ".Rdata")
    
    if (!overwrite) {
        if (file.exists(FILE.PATH))
            stop(paste0(error.prefix, "file already exists"))
    }
    save(object, file=FILE.PATH)
}

get.cached.object.for.version <- function(name, version)
{
    error.prefix = "Error getting cached object for version: "
    BASE.FILE.PATH = '../jheem_analyses/commoncode/object_for_version_cache'
    FILE.PATH = paste0(BASE.FILE.PATH, "/", version, "_", name, ".Rdata")
    load(file=FILE.PATH) # will be called "object"
    return (object)
}