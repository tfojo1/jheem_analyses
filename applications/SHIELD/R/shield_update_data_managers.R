# install.packages("httr")
library(httr)

#list of files to dowload
download_filenames <- c("surveillance.manager.rdata",  
                        "syphilis.manager.rdata")

# Define the OneDrive shared link
onedrive_link <- "https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A70f42746096a483692d66c187b1a0b60&sharingv2=true&fromShare=true&at=9&CID=80c32991%2Dabc8%2D455b%2D8923%2D247eb8522d91&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2%2Fcached&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0"

# Define the local download folder and filename
download_folder <- "~/OneDrive-JohnsHopkins/SHIELD/Simulation/code/jheem_analyses/cashed/"

# Create the directory if it doesn't exist
if (!dir.exists(download_folder)) {
  dir.create(download_folder, recursive = TRUE)
}

# Make an HTTP GET request to download the file
response <- GET(onedrive_link)

# Check if the request was successful
if (status_code(response) == 200) {
  lapply(1:length(download_filenames), function(i){
    download_path <- file.path(download_folder, download_filenames[i])
    # Write the downloaded content to a file
    writeBin(content(response, "raw"), download_path)
    cat("File downloaded successfully to:", download_path, "\n")
  })
} else {
  cat("Failed to download the file. HTTP Status:", status_code(response), "\n")
}
