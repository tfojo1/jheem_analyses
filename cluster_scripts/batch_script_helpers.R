
OUTPUT.DIR = "../jheem_analyses/cluster_scripts"
MODULE.LOAD.COMMANDS = c('ml gfbf/2023b', 'ml R/4.3.3-gfbf-2023b', 'export R_LIBS_USER=$HOME/rlibs/R-4.3.3')

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

make.setup.scripts <- function(locations,
                               dir='cluster_scripts/setup_scripts/',
                               partition='shared',
                               account='pkasaie1',
                               mem='16G')
{
    for (location in locations) {
        make.sbatch.script(filename=file.path(dir, get.setup.filename(location)),
                           job.name = paste0(location, '_setup'),
                           mem=mem,
                           output = file.path(OUTPUT.DIR, paste0("init_", location, ".out")),
                           partition = partition,
                           time.hours = 12,
                           account=account,
                           commands= paste("Rscript cluster_scripts/set_up_calibration.R", "ehe", location, "init.pop.ehe"))
    }
    
}

make.run.scripts <- function(locations,
                             chains=1:4,
                             dir='cluster_scripts/run_scripts/',
                             partition="shared",
                             account='pkasaie1',
                             mem='16G')
{
    for (location in locations) {
        for (chain in chains) {
            make.sbatch.script(filename=file.path(dir, get.run.filename(location, chain)),
                               job.name = paste0("setup_", location, "_", chain),
                               mem=mem,
                               output = file.path(OUTPUT.DIR, paste0("run_", location, "_", chain, ".out")),
                               partition=partition,
                               time.hours = 7*24,
                               account=account,
                               commands = paste("Rscript cluster_scripts/run_calibration.R", "ehe", location, "init.pop.ehe", chain))
        }
    }
}

get.setup.filename <- function(location) {
    paste0("setup_", location, ".bat")
}

get.run.filename <- function(location, chain) {
    paste0("run_", location, "_", chain, ".bat")
}

make.setup.master.script <- function(filename,
                                     locations,
                                     master.dir="cluster_scripts/master_scripts",
                                     dir="cluster_scripts/setup_scripts") {
    sink(file.path(master.dir, filename))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        cat("sbatch ", file.path(dir, get.setup.filename(location)), "\n", sep="")
    }
    sink()
}

make.run.master.script <- function(filename,
                                   locations,
                                   chains=1:4,
                                   master.dir="cluster_scripts/master_scripts",
                                   dir="cluster_scripts/run_scripts") {
    sink(file.path(master.dir, filename))
    cat("#!/bin/bash\n\n")
    for (location in locations) {
        for (chain in chains) {
            cat("sbatch ", file.path(dir, get.run.filename(location, chain)), "\n", sep="")
        }
    }
    sink()
}

# assemble.simulations.from.calibration() # give it a list of city, etc., takes stuff in cache and makes simset(s). Burn half and then thin