
CACHE.ELEMENTS = c(
    'msa.surveillance',
    'county.surveillance',
    'state.surveillance',
    'national.surveillance',
    'ALL.DATA.MANAGERS',
    'DEFAULT.LOCALE.MAPPING'
)

update.cache.consistency.check <- function(elements = gsub("\\.Rdata$", '', list.files(cache.dir)),
                                           consistency.file = 'code/core_code/data_managers/cache.consistency.check.Rdata',
                                           cache.dir='cached',
                                           first.time=F,
                                           allow.to.run=first.time) # A flag you have to set manually so we don't run by accident
{
    if (!allow.to.run)
        stop("Make sure NOT TO RUN update.cache.consistency.check BY ACCIDENT. You need to set the allow to run flag to be able to run this function.")
    
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
    
    files.to.load = file.path(cache.dir, paste0(elements, '.Rdata'))
    consistency.check = lapply(files.to.load, function(file){
        file.info(file)$ctime
    })
    names(consistency.check) = elements
    
    if (!is.null(prior.consistency.check))
    {
        in.both = intersect(names(prior.consistency.check), names(consistency.check))
        not.newer.mask = as.numeric(consistency.check[in.both]) < as.numeric(prior.consistency.check[in.both])
        if (any(not.newer.mask))
            stop(paste0("Error updating consistency.check. The following element(s) in the current cache are older than stated in the prior consistency.check: ",
                        paste0("'", in.both[not.newer.mask], "'", collapse=', ')))
    }
    
    save(consistency.check, file=consistency.file)
    
    print(paste0("Done Updating Cache Consistency Check (", length(consistency.check), " elements)"))
}

load.elements.from.cache <- function(elements.to.load = CACHE.ELEMENTS,
                            consistency.file = 'code/core_code/data_managers/cache.consistency.check.Rdata',
                            cache.dir='cached',
                            env = globalenv())
{
    # Load the files
    files.to.load = file.path(cache.dir, paste0(elements.to.load, '.Rdata'))
    missing.files = !sapply(files.to.load, file.exists)
    if (any(missing.files))
        stop(paste0("Cannot load the following element(s) from cache: ",
                    paste0("'", elements.to.load[missing.files], "'", collapse=', '),
                    ". File(s) do not exist in '", cache.dir, "'."))
    
    # Figure out when they were created
    times.files.created = lapply(files.to.load, function(file){
        file.info(file)$ctime
    })
    
    # Load the consistency check
    if (!file.exists(consistency.file))
        stop(paste0("No file exists at '", consistency.file, "'. Cannot load the cache consistency check."))
    load(consistency.file)
    
    # Check if too old
    missing.from.consistency = setdiff(elements.to.load, names(consistency.check))
    if (length(missing.from.consistency)>0)
        stop(paste0("The following cache element(s) have no record in the consistency.check and cannot be loaded: ",
                    paste0("'", missing.from.consistency, "'", collapse=', '),
                    ". Use update.cache.consistency.check() to create a record of these for consistency checking."))
    
    too.old.mask = as.numeric(times.files.created) < as.numeric(consistency.check[elements.to.load])
    if (any(too.old.mask))
        stop(paste0("The following cache element(s) are out of date: ",
                    paste0("'", elements.to.load[too.old.mask], "'", collapse=', '),
                    ". Get updated versions from the OneDrive folder or contact Todd."))
    
    # Load into the desired environment (by default, the global environment)
    for (file in files.to.load)
    {
        x=load(file)
        env[[x]] = get(x)
    }
}