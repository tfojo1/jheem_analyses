
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

make.setup.script <- function(location,
                              filename=paste0(location, '.r'),
                              dir='cluster_scripts/setup_scripts/',
                              partition='shared',
                              account='azalesak',
                              mem=NULL)
{
    make.sbatch.script(filename=file.path(dir, filename),
                       job.name = paste0(location, '_setup'),
                       mem=mem,
                       output = file.path(OUTPUT.DIR, paste0("init_", location, ".out")),
                       partition = 'shared',
                       time.hours = 48,
                       account=account,
                       commands= paste("Rscript set_up_calibration.r", "ehe", "C.12580", "init.pop.ehe"))
}