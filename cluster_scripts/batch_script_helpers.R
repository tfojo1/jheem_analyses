
OUTPUT.DIR = "../jheem_analyses/cluster_scripts"
MODULE.LOAD.COMMANDS = c('source cluster_scripts/Rockfish_module_loads.sh')

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

make.setup.scripts <- function(locations,
                               version,
                               calibration.code,
                               dir='cluster_scripts/setup_scripts',
                               partition='shared',
                               account='pkasaie1',
                               mem='16G')
{
    for (location in locations) {
        if (!dir.exists(file.path(dir, version, location)))
            dir.create(file.path(dir, version,location), recursive=T)
        make.sbatch.script(filename=file.path(dir, version, location, get.setup.filename(calibration.code)),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, version, location, get.setup.filename(calibration.code), get.setup.filename(calibration.code), extension=".out"),
                           partition = partition,
                           time.hours = 12,
                           account=account,
                           commands= paste("Rscript cluster_scripts/set_up_calibration.R", version, location, calibration.code))
    }
    
}

make.run.scripts <- function(locations,
                             version,
                             calibration.code,
                             chains=1:4,
                             dir='cluster_scripts/run_scripts',
                             partition="shared",
                             account='pkasaie1',
                             mem='16G')
{
    for (location in locations) {
        if (!dir.exists(file.path(dir, version, location)))
            dir.create(file.path(dir, version, location), recursive=T)
        for (chain in chains) {
            make.sbatch.script(filename=file.path(dir, version, location, get.run.filename(calibration.code, chain)),
                               job.name = paste0("run_", version, "_", location, "_", calibration.code, "_", chain),
                               mem=mem,
                               output = file.path(OUTPUT.DIR, version, location, get.run.filename(calibration.code, chain, extension=".out")),
                               partition=partition,
                               time.hours = 12, #Todd's said 7*24 but this made it hard to queue
                               account=account,
                               commands = paste("Rscript cluster_scripts/run_calibration.R", version, location, calibration.code, chain))
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
    error.prefix = "Cannot make.setup.master.script': "
    filename.with.extension = paste0(name.for.script, ".bat")
    if (file.exists(file.path(master.dir, filename.with.extension)) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", filename.with.extension, "' at this location. Use 'overwrite=T' to proceed anyway"))
    sink(file.path(master.dir, filename.with.extension))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        cat("sbatch ", file.path(dir, location, version, get.setup.filename(calibration.code)), "\n", sep="")
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
    error.prefix = "Cannot make.run.master.script': "
    filename.with.extension = paste0(name.for.script, ".bat")
    if (file.exists(file.path(master.dir, filename.with.extension)) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", filename.with.extension, "' at this location. Use 'overwrite=T' to proceed anyway"))
    sink(file.path(master.dir, filename.with.extension))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        for (chain in chains) {
            cat("sbatch ", file.path(dir, location, version, get.run.filename(calibration.code, chain)), "\n", sep="")
        }
    }
    sink()
}

#' @description Make a script to assemble and process calibration simsets from multiple locations
#' @details This doesn't create one job per location but rather calls the assemble
#' on all locations in single job, because the run time is short and we don't want
#' to worry about queuing.
#' @param name.for.result Since this handles multiple locations in one batch file output, a name is needed. Consider a username and date.
#' @param burn.keep,thin.keep See the documentation for simset.burn() and simset.thin() in the jheem2 package.
#' Here, 0's mean to skip the operation (burn or thin)
make.combined.assemble.script <- function(name.for.result,
                                          locations,
                                          version,
                                          calibration.code,
                                          burn.keep=0.5,
                                          thin.keep=0,
                                          dir="cluster_scripts/assemble_scripts",
                                          partition="shared",
                                          account="pkasaie1",
                                          mem="24G",
                                          overwrite=F) {
    error.prefix = "Cannot make.combined.assemble.script': "
    if (file.exists(file.path(dir, paste0("assemble_", name.for.result, ".bat"))) && !overwrite)
        stop(paste0(error.prefix, "there is already a '", name.for.result, "' at this location. Use 'overwrite=T' to proceed anyway"))
    all.commands = sapply(locations, function(location) {
        paste("Rscript cluster_scripts/assemble_calibration.R", version, location, calibration.code, burn.keep, thin.keep)
    })
    make.sbatch.script(filename=file.path(dir, paste0("assemble_", name.for.result, ".bat")),
                       job.name = paste0("assemble_", name.for.result),
                       mem=mem,
                       output = file.path(OUTPUT.DIR, paste0("assemble_", name.for.result, ".out")),
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