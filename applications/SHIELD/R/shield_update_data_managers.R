# install.packages("httr2")
library(httr2)

# Define the local download folder and filename
download_folder <- JHEEM.CACHE.DIR#"../jheem_analyses/cached"

# Create the directory if it doesn't exist
if (!dir.exists(download_folder)) {
  dir.create(download_folder, recursive = TRUE)
}

# list of files to download
download_filenames <- c("surveillance.manager.rdata",
                        "syphilis.manager.rdata")

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
for (i in seq_along(download_filenames)){
  req = request(onedrive_links[i])
  resp = req |> req_perform()
  if (resp_status(resp)==200) {
    # Get the OneDrive file's modification date from the response header if available
    onedrive_mod_time <- resp |> resp_header("last-modified")
    onedrive_mod_time_parsed <- as.POSIXct(onedrive_mod_time, format="%a, %d %b %Y %H:%M:%S", tz="GMT")
    formatted_onedrive_mod_time <- format(onedrive_mod_time_parsed, "%Y-%m-%d %H:%M:%S %Z")
    cat("***",download_filenames[i]," last modified on:", formatted_onedrive_mod_time, "\n")
    
    # Now get the local time:
    download_path <- file.path(download_folder, download_filenames[i])
    if (file.exists(download_path)) {
      local_mod_time <- file.info(download_path)$mtime
      formatted_local_mod_time <- format(local_mod_time, "%Y-%m-%d %H:%M:%S %Z")
      cat("***", download_filenames[i], "last modified locally on:", formatted_local_mod_time, "\n")
      
      # Should we update the file?
      if (!is.na(local_mod_time) && 
          !is.null(onedrive_mod_time) && 
          as.POSIXct(onedrive_mod_time, format="%a, %d %b %Y %H:%M:%S", tz="GMT") <= local_mod_time) {
        cat("Local file is up to date:", download_path, "\n")
      } else {
        cat("Updating local file from OneDrive:", download_path, "\n")
        writeBin(resp_body_raw(resp), download_path)
        }
    } else {
      cat("Failed to download the file. HTTP Status:", resp_status(resp), "\n")
    }
  }
}
  