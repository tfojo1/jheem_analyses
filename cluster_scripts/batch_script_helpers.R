# Get the absolute path to jheem_analyses directory
# JHEEM_DIR = file.path("/scratch4/pkasaie1", Sys.getenv("USER"), "jheem/code/jheem_analyses")
USER = 'azalesak'
JHEEM_DIR = file.path("/scratch4/pkasaie1", USER, "jheem/code/jheem_analyses")
OUTPUT.DIR = file.path(JHEEM_DIR, "cluster_scripts/outputs")
MODULE.LOAD.COMMANDS = c('source cluster_scripts/rockfish_module_loads.sh')

make.sbatch.script <- function(filename,
                               mem='16GB',
                               mem.per.cpu=NULL,
                               time.hours=NULL,
                               output=NULL,
                               job.name=NULL,
                               commands0 = MODULE.LOAD.COMMANDS,
                               partition=NULL,
                               account=NULL,
                               commands)
{
    sink(filename)
    
    cat("#!/bin/bash\n\n")
    if (!is.null(job.name))
        cat("#SBATCH --job-name=", job.name, '\n', sep='')
    
    if (!is.null(mem))
        cat("#SBATCH --mem=", mem, '\n', sep='')
    
    if (!is.null(mem.per.cpu))
        cat("#SBATCH --mem-per-cpu=", mem.per.cpu, '\n', sep='')
    
    if (!is.null(output))
        cat("#SBATCH --output=", output, '\n', sep='')
    
    if (!is.null(time.hours))
        cat("#SBATCH --time=", time.hours, ':00:00\n', sep='')
    
    if (!is.null(partition))
        cat("#SBATCH --partition=", partition, '\n', sep='')
    
    if (!is.null(account))
        cat("#SBATCH --account=", account, '\n', sep='')
    
    if (!is.null(commands0))
        cat('\n', paste0(commands0, collapse='\n'), '\n', sep='')
    
    if (!is.null(commands))
        cat(paste0(commands, collapse='\n'), sep='')
    
    sink()
}

# INDIVIDUAL SCRIPTS ----

#' @param specification.path,register.calibration.path Paths to file with this version's specification and file where this calibration code is registered.
make.setup.scripts <- function(locations,
                               version,
                               calibration.code,
                               specification.path,
                               register.calibration.path,
                               dir='cluster_scripts/setup_scripts',
                               partition='parallel',
                               account='tfojo1',
                               mem='16G')
{
    # Create output directories for each location
    for (location in locations) {
        output_path <- file.path(OUTPUT.DIR, version, location, get.setup.filename(calibration.code), get.setup.filename(calibration.code, extension=".out"))
        if (!dir.exists(output_path))
            dir.create(output_path, recursive=TRUE)
            
        # Create script directories
        script_path <- file.path(dir, version, location)
        if (!dir.exists(script_path))
            dir.create(script_path, recursive=TRUE)
            
        # Create the batch script
        make.sbatch.script(filename=file.path(dir, version, location, get.setup.filename(calibration.code)),
                           mem=mem,
                           output = output_path,
                           job.name = paste0('S_', location),
                           partition = partition,
                           time.hours = 12,
                           account=account,
                           commands= paste("Rscript cluster_scripts/set_up_calibration.R", version, location, calibration.code, specification.path, register.calibration.path))
    }
}

#' @inheritParams make.setup.scripts
make.run.scripts <- function(locations,
                             version,
                             calibration.code,
                             chains=1:4,
                             specification.path,
                             register.calibration.path,
                             dir='cluster_scripts/run_scripts',
                             partition="parallel",
                             account='tfojo1',
                             mem='16G')
{
    for (location in locations) {
        # Create output directories for each location/chain combination
        for (chain in chains) {
            output_path <- file.path(OUTPUT.DIR, version, location, get.run.filename(calibration.code, chain, extension=".out"))
            if (!dir.exists(output_path))
                dir.create(output_path, recursive=TRUE)
        }
        
        # Create script directories
        script_path <- file.path(dir, version, location)
        if (!dir.exists(script_path))
            dir.create(script_path, recursive=TRUE)
            
        # Create batch scripts for each chain
        for (chain in chains) {
            make.sbatch.script(filename=file.path(dir, version, location, get.run.filename(calibration.code, chain)),
                               job.name = paste0("R_", location, "_", chain),
                               mem=mem,
                               output = output_path,
                               partition=partition,
                               time.hours = 36, #Todd's said 7*24 but this made it hard to queue
                               account=account,
                               commands = paste("Rscript cluster_scripts/run_calibration.R", version, location, calibration.code, chain, specification.path, register.calibration.path))
        }
    }
}

#' @inheritParams make.setup.scripts
#' @param calibration.codes A vector of calibration codes in the order they should be attempted
make.multiphase.scripts <- function(locations,
                                    version,
                                    calibration.codes,
                                    chains=1,
                                    specification.path,
                                    register.calibration.path,
                                    dir='cluster_scripts/multiphase_scripts',
                                    partition="parallel",
                                    account="tfojo1",
                                    mem="16G")
{
    for (location in locations) {
        # Create output directories for each location/chain combination
        for (chain in chains) {
            output_path <- file.path(OUTPUT.DIR, version, location, get.multiphase.filename(calibration.codes, chain, extension=".out"))
            if (!dir.exists(output_path))
                dir.create(output_path, recursive=TRUE)
        }
        
        # Create script directories
        script_path <- file.path(dir, version, location)
        if (!dir.exists(script_path))
            dir.create(script_path, recursive=TRUE)
        
        # Create batch scripts for each chain
        for (chain in chains) {
            make.sbatch.script(filename=file.path(dir, version, location, get.multiphase.filename(calibration.codes, chain)),
                               job.name = paste0("M_", location, "_", chain),
                               mem=mem,
                               output = output_path,
                               partition=partition,
                               time.hours = 36,
                               account=account,
                               commands = paste("Rscript cluster_scripts/do_multiphase_calibration.R", version, location, calibration.codes, chain, specification.path, register.calibration.path))
        }
    }
}

# MASTER SCRIPTS ----

make.setup.master.script <- function(name.for.script,
                                     locations,
                                     version,
                                     calibration.code,
                                     master.dir="cluster_scripts/master_scripts/setup",
                                     dir="cluster_scripts/setup_scripts",
                                     overwrite=F) {
    # Create master directory if it doesn't exist
    if (!dir.exists(master.dir))
        dir.create(master.dir, recursive=TRUE)
        
    error.prefix = "Cannot make.setup.master.script': "
    filename.with.extension = paste0(name.for.script, ".bat")
    if (file.exists(file.path(master.dir, filename.with.extension)) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", filename.with.extension, "' at this location. Use 'overwrite=T' to proceed anyway"))
    sink(file.path(master.dir, filename.with.extension))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        cat("sbatch ", file.path(dir, version, location, get.setup.filename(calibration.code)), "\n", sep="")
    }
    sink()
}

make.run.master.script <- function(name.for.script,
                                   locations,
                                   version,
                                   calibration.code,
                                   chains=1:4,
                                   master.dir="cluster_scripts/master_scripts/run",
                                   dir="cluster_scripts/run_scripts",
                                   overwrite=F) {
    # Create master directory if it doesn't exist
    if (!dir.exists(master.dir))
        dir.create(master.dir, recursive=TRUE)
        
    error.prefix = "Cannot make.run.master.script': "
    filename.with.extension = paste0(name.for.script, ".bat")
    if (file.exists(file.path(master.dir, filename.with.extension)) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", filename.with.extension, "' at this location. Use 'overwrite=T' to proceed anyway"))
    sink(file.path(master.dir, filename.with.extension))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        for (chain in chains) {
            cat("sbatch ", file.path(dir, version, location, get.run.filename(calibration.code, chain)), "\n", sep="")
        }
    }
    sink()
}

make.multiphase.master.script <- function(name.for.script,
                                          locations,
                                          version,
                                          calibration.codes,
                                          chains=1:4,
                                          master.dir="cluster_scripts/master_scripts/multiphase",
                                          dir="cluster_scripts/multiphase_scripts",
                                          overwrite=F) {
    # Create master directory if it doesn't exist
    if (!dir.exists(master.dir))
        dir.create(master.dir, recursive=TRUE)
    
    error.prefix = "Cannot make.multiphase.master.script': "
    filename.with.extension = paste0(name.for.script, ".bat")
    if (file.exists(file.path(master.dir, filename.with.extension)) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", filename.with.extension, "' at this location. Use 'overwrite=T' to proceed anyway"))
    sink(file.path(master.dir, filename.with.extension))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        for (chain in chains) {
            cat("sbatch ", file.path(dir, version, location, get.multiphase.filename(calibration.codes, chain)), "\n", sep="")
        }
    }
    sink()
}

#' @description Make a script to assemble and process calibration simsets from multiple locations
#' @details This doesn't create one job per location but rather calls the assemble
#' on all locations in single job, because the run time is short and we don't want
#' to worry about queuing.
#' @inheritParams make.setup.scripts
#' @param name.for.result Since this handles multiple locations in one batch file output, a name is needed. Consider a username and date.
#' @param burn.keep,thin.keep See the documentation for simset.burn() and simset.thin() in the jheem2 package.
#' Here, 0's mean to skip the operation (burn or thin)
make.combined.assemble.script <- function(name.for.result,
                                          locations,
                                          version,
                                          calibration.code,
                                          burn.keep=0.5,
                                          thin.keep=0,
                                          specification.path,
                                          register.calibration.path,
                                          dir="cluster_scripts/assemble_scripts",
                                          partition="shared",
                                          account="tfojo1",
                                          mem="24G",
                                          overwrite=F) {
    # Create assemble directory if it doesn't exist
    if (!dir.exists(dir))
        dir.create(dir, recursive=TRUE)
        
    error.prefix = "Cannot make.combined.assemble.script': "
    output_path <- file.path(OUTPUT.DIR, paste0("assemble_", name.for.result, ".out"))
    if (!dir.exists(output_path))
        dir.create(output_path, recursive=TRUE)
    
    if (file.exists(file.path(dir, paste0("assemble_", name.for.result, ".bat"))) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", name.for.result, "' at this location. Use 'overwrite=T' to proceed anyway"))
    all.commands = sapply(locations, function(location) {
        paste("Rscript cluster_scripts/assemble_calibration.R", version, location, calibration.code, burn.keep, thin.keep, specification.path, register.calibration.path)
    })
    make.sbatch.script(filename=file.path(dir, paste0("assemble_", name.for.result, ".bat")),
                       job.name = paste0("assemble_", name.for.result),
                       mem=mem,
                       output = output_path,
                       partition=partition,
                       time.hours = 12,
                       account=account,
                       commands = all.commands)
}

# HELPERS ----

get.setup.filename <- function(calibration.code, extension=".bat") {
    paste0("setup_", calibration.code, extension)
}

get.run.filename <- function(calibration.code, chain, extension=".bat") {
    paste0("run_", calibration.code, "_", chain, extension)
}

get.multiphase.filename <- function(calibration.codes, chain, extension=".bat") {
    paste0("multiphase_", paste(calibration.codes, collapse="__"), chain, extension)
}