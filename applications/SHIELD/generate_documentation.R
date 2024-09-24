# Script to document a project (or sub-project)
# Work in progress! Send questions to Andrew Zalesak
JHEEM.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/"
SHIELD.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/applications/SHIELD/"
setwd(JHEEM.DIR)

#'@title Generate a PDF Manual Documenting Methods
#'@param ... File paths of directories containing R files to be documented. These directories should only contain R files and those files should only define functions to be documented
#'@param path.to.documentation.folder A file path to the shared documentation folder relative to the current working directory
#'@param recursive Should sub-directories of the directories specified in '...' be used as well? The sub-directories must meet the same conditions as described for '...'
#'
#'@description Generates a PDF manual documenting the functions in the directories passed in.
#'
#'@details This function makes a temporary project/package, copies your files into them, and later deletes the temporary project/package after generating the PDF.
#' Therefore, it is recommended that the location at which this occurs -- specified in the 'path.to.documentation.folder' -- is not within any other project.
#' If an agreed upon location is set up and used by everyone, it can be ensured this condition is met.
generate_documentation <- function(..., path.to.documentation.folder, recursive=T)
{
    directories = list(...)
    error.prefix = "Error generating documentation for project: "

    # Argument validation
    # if (!is.character(project.name) || length(project.name)!=1 || is.na(project.name))
    #     stop(paste0(error.prefix, "'project.name' must be a single character value"))
    for (directory in directories) {
        if (!is.character(directory) || length(directory)!=1 || is.na(directory))
            stop(paste0(error.prefix, "directories supplied in '...' must be single character values"))
        if (!dir.exists(directory))
            stop(paste0(error.prefix, "directory '", directory, "' could not be found"))
    }
    if (!is.character(path.to.documentation.folder) || length(path.to.documentation.folder)!=1 ||is.na(path.to.documentation.folder))
        stop(paste0(error.prefix, "'path.to.documentation.folder' must be a character value specifying the path to that folder from the current working directory"))
    if (!dir.exists(path.to.documentation.folder))
        stop(paste0(error.prefix, "Could not find a directory at that 'path.to.documentation.folder'. Is that the location of the shared documentation folder?"))
    if (!is.logical(recursive) || length(recursive)!=1 || is.na(recursive))
        stop(paste0(error.prefix, "'recursive' must be a single logical value"))

    # project.documentation.path = paste0("../documentation/", tolower(project.name)) # how do we find this from a user's current wd?
    project.documentation.path = paste0(path.to.documentation.folder, "temp")

    if (dir.exists(project.documentation.path))
        stop(paste0(error.prefix, "A directory named 'temp' already exists where a temporary directory needs to go"))

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

