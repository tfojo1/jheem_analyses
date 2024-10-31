
JHEEM.CACHE.DIR = NULL
if (dir.exists('../../cached'))
    JHEEM.CACHE.DIR = '../../cached'
if (dir.exists('../jheem_analyses/cached'))
    JHEEM.CACHE.DIR = '../jheem_analyses/cached'
DATA.MANAGER.CACHE.METADATA.FILE = '../jheem_analyses/commoncode/data_manager_cache_metadata.Rdata'

if (is.null(JHEEM.CACHE.DIR))
    stop("No 'cached' directory exists - you need to get this from Todd's One-Drive")

load.data.manager.from.cache <- function(file, set.as.default=F)
{
    # if (is.cached.object.out.of.date(file, error.prefix = 'Cannot read.data.manager.from.cache() - '))
    #     stop(paste0("The data manager '", file, "' is OUT OF DATE - you need to get this from Todd's One-Drive"))
    
    loaded.data.manager = load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
    # browser()
    if (is.cached.data.manager.out.of.date(file, loaded.data.manager, error.prefix = 'Cannot read data manager from cache - '))
        stop(paste0("The data manager '", file, "' is OUT OF DATE - you need to get this from Todd's One-Drive. Call 'get.data.manager.cache.metadata()' to see the required dates."))

    return(loaded.data.manager)
}

# Called internally
is.cached.data.manager.out.of.date <- function(file, data.manager, error.prefix = '')
{
    # browser()
    if (!R6::is.R6(data.manager) || !is(data.manager, 'jheem.data.manager')) {
        stop(paste0(error.prefix, "'load.data.manager.from.cache' can only be called on JHEEM Data Manager objects"))
    }
    
    if (!file.exists(DATA.MANAGER.CACHE.METADATA.FILE))
        stop(paste0(error.prefix, "The 'data.manager.cache.metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
    
    data.manager.cache.metadata = get(load(DATA.MANAGER.CACHE.METADATA.FILE))
    
    if (!(file %in% names(data.manager.cache.metadata)))
        stop(paste0(error.prefix, "'", file, "' is not one of our cached files. Call 'get.data.manager.cache.metadata()' to check what files are cached. File names are capitalization-sensitive."))

    # Check if the creation date and last modified date are both at least as new as the cached dates
    if (!is.null(data.manager.cache.metadata[[file]][['creation.date']]))
        if (!is.null(data.manager[['creation.date']]) && data.manager[['creation.date']] < data.manager.cache.metadata[[file]][['creation.date']])
            return (TRUE)
    if (!is.null(data.manager.cache.metadata[[file]][['last.modified.date']]))
        if (!is.null(data.manager[['last.modified.date']]) && data.manager[['last.modified.date']] < data.manager.cache.metadata[[file]][['last.modified.date']])
            return (TRUE)
    
    FALSE
}

get.data.manager.cache.metadata <- function(error.prefix = '')
{
    if (!file.exists(DATA.MANAGER.CACHE.METADATA.FILE))
        stop(paste0(error.prefix, "The 'data_manager_cache_metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
    
    data.manager.cache.metadata = get(load(DATA.MANAGER.CACHE.METADATA.FILE))
    
    data.manager.cache.metadata
}

reset.data.manager.cache.metadata <- function(allow.flag = F)
{
    if (!allow.flag)
        stop("Are you sure you want to reset.data.manager.cache.metadata()?  - you need to set the allow.flag to TRUE if you do")
    replacement.cache.metadata = list()
    save(replacement.cache.metadata, file=DATA.MANAGER.CACHE.METADATA.FILE)
}

sync.cached.data.manager <- function(file, allow.flag = F)
{

    if (!allow.flag)
        stop("Are you sure you want to sync.cached.data.manager()?  - you need to set the allow.flag to TRUE if you do")
    
    x = load(file.path(JHEEM.CACHE.DIR, file))
    if (length(x)!=1)
        stop(paste0("Cannot sync.cached.data.manager() - 'file' does not specify a single JHEEM Data Manager object"))
    data.manager = get(x)
    if (!R6::is.R6(data.manager) && !is(data.manager, 'jheem.data.manager'))
        stop(paste0("Cannot sync.cached.data.manager() - 'file' does not specify a single JHEEM Data Manager object"))

    data.manager.metadata = list(creation.data = data.manager[['creation.date']],
                                 last.modified.date = data.manager[['last.modified.date']])
    
    if (file.exists(DATA.MANAGER.CACHE.METADATA.FILE)) {
        cache.metadata = get(load(DATA.MANAGER.CACHE.METADATA.FILE))
        cache.metadata[[file]] = data.manager.metadata
    }
    else
        cache.metadata = setNames(list(data.manager.metadata), file)

    save(cache.metadata, file=DATA.MANAGER.CACHE.METADATA.FILE)
}

# is.cached.object.out.of.date <- function(file,
#                                          error.prefix = '')
# {
#     if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
#         stop("Cannot check is.cached.object.out.of.date() - 'file' must be a single, non-NA character value")
# 
#     if (!is.character(file) || length(file)!=1 || is.na(file))
#         stop(paste0(error.prefix, "'file' must be a single, non-NA character value"))
# 
#     if (all(file != CACHE.FILES.TO.TRACK))
#         stop(paste0(error.prefix,
#                     "'", file, "' - is not one of our cached files (which include ",
#                     paste0("'", CACHE.FILES.TO.TRACK, "'", collapse=', ')))
# 
#     if (!file.exists('../jheem_analyses/commoncode/cache_metadata.Rdata'))
#         stop(paste0(error.prefix, "The 'cache_metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
# 
#     load('../jheem_analyses/commoncode/cache_metadata.Rdata')
# 
#     if (!file.exists(file.path(JHEEM.CACHE.DIR, file)))
#         stop(paste0("The data manager '", file, "' is not present in the 'cached' directory ('",
#                     JHEEM.CACHE.DIR, "') - you need to get this from Todd's One-Drive"))
# 
#     file.metadata = file.info(file.path(JHEEM.CACHE.DIR, file))
# 
#     (as.numeric(file.metadata$mtime) - as.numeric(cache.metadata[file,'mtime'])) < -1
#     # this formulation approximates
#     # file.metadata$mtime < cache.metadata[file,'mtime']
#     # but allows for numeric rounding errors
# }


# CACHE.FILES.TO.TRACK = c(
#     'surveillance.manager.rdata',
#     'census.manager.rdata'
# )

# # Requires all cached files, when loaded, to be no older than the files in this computer
# sync.cache.metadata <- function(allow.flag = F)
# {
#     if (!allow.flag)
#         stop("Are you sure you want to sync.cache.metadata()?  - you need to set the allow.flag to TRUE if you do")
#     
#     cache.metadata = file.info(file.path(JHEEM.CACHE.DIR, CACHE.FILES.TO.TRACK))
#     dimnames(cache.metadata)[[1]] = CACHE.FILES.TO.TRACK
#     browser()
#     missing.mask = apply(is.na(cache.metadata), 1, any)
#     if (any(missing.mask))
#         stop(paste0("Cannot sync.cache.metadata() - the ",
#                     ifelse(sum(missing.mask)==1, "file ", "files "),
#                     paste0("'", CACHE.FILES.TO.TRACK[missing.mask], "'", collapse = ', '),
#                     ifelse(sum(missing.mask)==1, " is", " are"),
#                     " not present in the cached directory ('",
#                     JHEEM.CACHE.DIR, "')"))
#     
#     # save(cache.metadata, file='../jheem_analyses/commoncode/cache_metadata.Rdata')
# }