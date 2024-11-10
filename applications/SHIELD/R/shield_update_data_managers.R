# install.packages("httr2")
library(httr2)

# Define the local download folder and filename
download_folder <- "../jheem_analyses/cached/temp"

# Create the directory if it doesn't exist
if (!dir.exists(download_folder)) {
    dir.create(download_folder, recursive = TRUE)
}

# list of files to download
download_filenames <- c("surveillance_manager.rdata",
                        "syphilis_manager.rdata")

# Define the Onedrive shared links
onedrive_links <- c(
    paste0(
        "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/EQS1u_TFlkpCrsv5vhMlufoB7UmfiE2TYW04-CXSdAG4Xw?e=Qdupwm",
        "&download=1"),
    paste0(
        "https://livejohnshopkins-my.sharepoint.com/:u:/g/personal/tfojo1_jh_edu/Ea6-pTjU9VZOqh4BFkEZjcEBr092GiFiU2KmBk5D3b6AvA?e=dhkQUF",
        "&download=1"
    ))

# Make an HTTP GET request to download the file and save response
for (i in seq_along(download_filenames))
{
    req = request(onedrive_links[i])
    resp = req |> req_perform()
    if (resp_status(resp)==200) {
        download_path = file.path(download_folder, download_filenames[i])
        writeBin(resp_body_raw(resp), download_path)
        cat("File downloaded successfully to:", download_path, "\n")
    } else {
        cat("Failed to download the file. HTTP Status:", resp_status(resp), "\n")
    }
}