# Script to document a project (or sub-project)
# Work in progress! Send questions to Andrew Zalesak

generate_documentation <- function(project.name, ..., recursive=T)
{
    directories = list(...)
    error.prefix = "Error generating documentation for project: "
    
    # Argument validation
    if (!is.character(project.name) || length(project.name)!=1 || is.na(project.name))
        stop(paste0(error.prefix, "'project.name' must be a single character value"))
    for (directory in directories) {
        if (!is.character(directory) || length(directory)!=1 || is.na(directory))
            stop(paste0(error.prefix, "directories supplied in '...' must be single character values"))
        if (!dir.exists(directory))
            stop(paste0(error.prefix, "directory '", directory, "' could not be found"))
    }
    if (!is.logical(recursive) || length(recursive)!=1 || is.na(recursive))
        stop(paste0(error.prefix, "'recursive' must be a single logical value"))
    
    project.documentation.path = paste0("../documentation/", tolower(project.name)) # how do we find this from a user's current wd?
    
    if (dir.exists(project.documentation.path))
        stop(paste0(error.prefix, "A directory named '", project.documentation.path, "' exists where a temporary directory needs to go"))
    
    # Make this directory into a temporary package in documentation/PROJECT_NAME
    # For now, this means the DESCRIPTION file will be the default and this will be reflected in the output PDF.
    usethis::create_package(path = project.documentation.path,
                            check_name = F,
                            rstudio = F,
                            open = F)
    
    # Copy files over to its /R directory.
    # It does not matter what the file names are, so give each one a number so that there are never any identical file names.
    this.file.number = 1
    for (directory in directories) {
        files.to.copy = list.files(path=directory, recursive = recursive, full.names=T)
        for (file.path in files.to.copy) {
            file.copy(from = file.path, to = paste0(project.documentation.path, "/R/", as.character(this.file.number), ".R"))
            this.file.number = this.file.number + 1
        }
    }
    
    # Generate the documentation
    roxygen2::roxygenize(project.documentation.path)
    devtools::build_manual(pkg = project.documentation.path)
    
    # Delete the temporary package
    unlink(project.documentation.path, recursive = T)
}

