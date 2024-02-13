
JHEEM.CACHE.DIR = NULL
if (dir.exists('../../cached'))
    JHEEM.CACHE.DIR = '../../cached'
if (dir.exists('../jheem_analyses/cached'))
    JHEEM.CACHE.DIR = '../jheem_analyses/cached'

if (is.null(JHEEM.CACHE.DIR))
    stop("No 'cached' directory exists - you need to get this from Todd's One-Drive")

load.data.manager.from.cache <- function(file, set.as.default=F)
{
    if (is.cached.object.out.of.date(file, error.prefix = 'Cannot read.data.manager.from.cache() - '))
      stop(paste0("The data manager '", file, "' is OUT OF DATE - you need to get this from Todd's One-Drive"))
    
    load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
}

is.cached.object.out.of.date <- function(file,
                                         error.prefix = '')
{
  if (!is.character(error.prefix) || length(error.prefix)!=1 || is.na(error.prefix))
      stop("Cannot check is.cached.object.out.of.date() - 'file' must be a single, non-NA character value")
  
    if (!is.character(file) || length(file)!=1 || is.na(file))
        stop(paste0(error.prefix, "'file' must be a single, non-NA character value"))
    
    if (all(file != CACHE.FILES.TO.TRACK))
        stop(paste0(error.prefix,
                    "'", file, "' - is not one of our cached files (which include ",
                    paste0("'", CACHE.FILES.TO.TRACK, "'", collapse=', ')))
    
    if (!file.exists('../jheem_analyses/commoncode/cache_metadata.Rdata'))
        stop(paste0(error.prefix, "The 'cache_metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
    
    load('../jheem_analyses/commoncode/cache_metadata.Rdata')
    
    if (!file.exists(file.path(JHEEM.CACHE.DIR, file)))
        stop(paste0("The data manager '", file, "' is not present in the 'cached' directory ('",
                    JHEEM.CACHE.DIR, "') - you need to get this from Todd's One-Drive"))
    
    file.metadata = file.info(file.path(JHEEM.CACHE.DIR, file))
    
    file.metadata$mtime < cache.metadata[file,'mtime']
}

CACHE.FILES.TO.TRACK = c(
    'surveillance.manager.rdata',
    'smaller.census.manager.rdata'
)

# Requires all cached files, when loaded, to be no older than the files in this computer
sync.cache.metadata <- function(allow.flag = F)
{
    if (!allow.flag)
        stop("Are you sure you want to sync.cache.metadata()?  - you need to set the allow.flag to TRUE if you do")
  
    cache.metadata = file.info(file.path(JHEEM.CACHE.DIR, CACHE.FILES.TO.TRACK))
    dimnames(cache.metadata)[[1]] = CACHE.FILES.TO.TRACK
    
    missing.mask = apply(is.na(cache.metadata), 1, any)
    if (any(missing.mask))
        stop(paste0("Cannot sync.cache.metadata() - the ",
                    ifelse(sum(missing.mask)==1, "file ", "files "),
                    paste0("'", CACHE.FILES.TO.TRACK[missing.mask], "'", collapse = ', '),
                    ifelse(sum(missing.mask)==1, " is", " are"),
                    " not present in the cached directory ('",
                    JHEEM.CACHE.DIR, "')"))
    
    save(cache.metadata, file='../jheem_analyses/commoncode/cache_metadata.Rdata')
}