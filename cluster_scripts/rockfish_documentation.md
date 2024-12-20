# Rockfish Setup and Usage Instructions
Andrew Zalesak

December 19, 2024

## Documentation
Rockfish has decent documentation on its website, although it didn’t answer a few of the questions I had. The main User Guide is [here](https://www.arch.jhu.edu/guide/) and some other information can be found under the “Support” tab. The Rockfish tech support are kind and helpful and can be contacted at help@rockfish.jhu.edu with questions.

## Setup

### 1. Join Rockfish
Make sure you’ve got a Rockfish account and are on Parastu’s allocation (pkasaie1). If so, you can log in on a Windows PowerShell (or other shell) using `ssh -X <username>@login.rockfish.jhu.edu` and then supplying your Rockfish-specific password. This will start you on a Login Node, which is good for setting up file organization but shouldn’t be used for running R. You’ll want to be familiar with basic Unix commands to move between directories (`cd`) and view contents (`ls`).

###	2. Set up directories
My home directory shows a sub-directory called `scr4_pkasaie1`, which stands for “Parastu’s Scratch 4” directory. Under this directory, I have a directory called `azalesak` (my username), and then below that have the same structure we typically use on the team: `jheem` with two directories `code` and `files`. `files` will be where calibration data is written to. `code` will be where our code repositories like `jheem_analyses` will reside. Make these directories with `mkdir <directory_name>` and remove them with `rm <directory_name>` if you need. Do not create directories for any repositories yet, because we will create those automatically by cloning from Github.

### 3. Clone repositories
Follow the instructions [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) for cloning a repository using the “SSH” method to clone the `jheem_analyses` repository from Github (https://github.com/tfojo1/jheem_analyses). I can’t recall exactly what I did, but it probably required setting up an SSH key as described here. Note that to use Git operations on Rockfish, you’ll always have to load the `git` module, using the terminal command `module load git`. Our goal is to have everything on Rockfish using jheem2 in package form, but if that’s not available yet, then you’ll want to clone the jheem2 repository from Github as well (https://github.com/tfojo1/jheem2)

### 4. Prepare to install R packages
This was the most difficult step for me because Rockfish support had to do some background setup to make available various obscure dependencies our package has. Thankfully, a lot of that work is done if we use a particular R module they’ve made for us. Since installing R packages involves running R, you should be doing this on an Interact Node. You can request and switch to this with a command like `interact -t 8:00:00 -n 6`, which in this case is requesting six cores for eight hours. Our goal will be to install all the necessary packages as found in `jheem_analyses/first_time_setup/install_packages.R`. That is, we want to install the following packages:

- ggmap
- ggnewscale
- httr2
- tfojo1/distributions
- tfojo1/Bayesian.simulations
- tfojo1/locations
- thk686/odeintr

To do so, we will first need to set up a special location for the R library. At the HOME directory (top level), make a directory called `rlibs`. Then, set an environment variable to tell R to install packages to this location with the command `export R_LIBS_USER=$HOME/rlibs/R-4.3.3`. You’ll have to make this environment variable again on every session that you’re trying to install packages or run R outside of a batch script (the batch scripts we make run this automatically). Now you’ll load the R module, but you won’t be loading the obvious one (i.e., don’t `module load r`). Instead, you’ll load a custom R module that Rockfish support has outfitted with the dependencies we need, like certain C libraries. This takes two lines: `module load gfbf/2023b` and `module load R/4.3.3-gfbf-2023b`. Note that `ml` is a shortcut for `module load` and will do the same thing. Once these three steps are completed for this session, you can proceed to launch R and install the packages.

### 5. Install R packages
Launch an R session with the simple command `R` (quit it with `quit()`). Install the first three packages from CRAN with `install.packages(“ggmap”, lib=Sys.getenv(“R_LIBS_USER”))` or similar, then the latter four packages from Github with `devtools::install_github(“tfojo1/distributions”, lib=Sys.getenv(“R_LIBS_USER”))`. Hopefully, the various issues I got during this process won’t re-occur because the custom R module has the right dependencies now. Note that it is critical to specify the library location during these installations because installing to other places will either prompt an access denied error or simply place the package in a location where R won’t be able to find it. If you start a new interact session, the environment variable defining the library location won’t exist anymore, and you’ll have to run the `export R_LIBS_USER…` command again. If errors occur during installation, feel free to reach out to me.

### 6. Import required objects through SCP
There are several R data objects that most JHEEM model specifications use that are not transferred through Git, like data managers. Since these objects are not terribly large (at most around 100 MB), they can be imported easily from your local desktop to Rockfish with the `scp` command. Although we’ve written code to automatically download certain data managers from the Internet, I used SCP to bring these in on my first attempt since I was already doing so for some other cached objects. The full list of the required non-Git-controlled objects, as far as I know, is:

- census.manager.rdata
- DEFAULT.LOCALE.MAPPING.Rdata
- google_mobility_data.Rdata
- msa.surveillance.Rdata
- national.surveillance.Rdata
- surveillance.manager.rdata
- syphilis.manager.rdata

To put these in using SCP, use your PowerShell from outside of Rockfish (quit Rockfish using `exit` as many times as needed). You’ll want to know the path on your own computer to where you’ve stored these objects, and you’ll want to know where you’re going to put them on Rockfish. The rough syntax for SCP file transfer is `scp <path_to_local_object> <rockfish_address>:<rockfish_file_path>`. I recommend using `cd` or similar on your desktop to get straight to where these files are located, so that the `<path_to_local_object>` can just be the file name, like `census.manager.rdata`. The `<rockfish_address>` is going to be your Rockfish username followed by an “@” sign and then the address of the Rockfish data transfer node, which is `rfdtn1.rockfish.jhu.edu`. The destination file is going to look a little different from how it is when you’re in Rockfish, because some of the directories you see, like `scr4_pkasaie1`, are actually symbolic links to places like `scratch4/pkasaie1`. For me to import a `census.manager.rdata` object that I’ve navigated to locally into my Rockfish directory `HOME/scr4_pkasaie1/azalesak/jheem/code/jheem_analyses/cached`, I would use the following full SCP command:

`scp census.manager.rdata azalesak@rfdtn1.rockfish.jhu.edu:/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cached/census.manager.rdata`.

You’ll have to provide your Rockfish password to authorize this file transfer. Note that you can use the same command but with the source and destination flipped to get objects off Rockfish. That will come in handy when exporting calibration simsets.

## Usage

### 1. Use our R scripts to generate calibration batch scripts
The purpose of Rockfish is to allow us to run lengthy and memory-intensive R operations simultaneously across multiple cores so that calibrating doesn’t take an eternity. Rockfish receives requests for computation through the submission of SLURM “batch scripts”. These batch scripts contain some header information on how much memory and time should be devoted to a given job, followed by the requested operation itself in command-line form, like running a particular R script. Here is an example of a batch script generated to calibrate EHE population for Baltimore:

![](batch_script_screenshot.png)
 
Notice that lines 0, 1, and 2 load the R module and set the environment variable like we do when we join an interact session. Line 3 is a command line call to an R script we’ve written to do the calibration for version “ehe”, location “C.12580”, and calibration code “init.pop.ehe”. We want to generate batch scripts like this for many cities at the same time, so to do this we can use a script in `jheem_analyses/cluster_scripts` called `batch_script_helpers.R`. It contains a method called `make.setup.scripts` that can take a vector of locations as an argument. It will then fill the `cluster_scripts/setup_scripts/` directory (or another, if specified) with batch scripts like seen above that can be queued with SLURM on Rockfish. Note that there are analogs to this setup script for other steps like “running” a calibration.

### 2. Use another R script to generate a master script
Having dozens of scripts like the one above would require queueing each one individually (see step 3). Instead, you can make a bash script that, when run, will queue all your dozens of batch scripts at once. We call this a “master script”, and you can generate one with `make.setup.master.script` by supplying a name for the resulting master script and a vector of locations. The resulting script might look like this:

![](master_script_screenshot.png)
 
### 3. Import the generated scripts to Rockfish with Git
The previous steps could have been performed on your local desktop or on a Rockfish interact session, with pros and cons to each approach. If you did it locally, you can use a “pull/stage/commit/push” sequence to push the newly generated scripts to the remote repository on Github, and then pull it down in Rockfish using `git pull`. In Rockfish, your working directory will have to be somewhere in jheem_analyses for Git to know what repository you’re wanting to pull, and you will have had to module load Git with `module load git`. You’ll also need to supply the Git SSH password you made during setup for each Git operation you do. If you always generate these batch scripts outside of Rockfish, then all you’ll have to worry about inside Rockfish is `git pull`. It’s possible that generating them inside Rockfish will be more convenient under some circumstances. In such a case, you’ll have to take extra steps like staging, committing, and pushing to keep your Rockfish files synced with your local ones.

### 4. Pull Git to update model code
Before starting calibration, make sure that you’ve got the most recent set of files in jheem_analyses if desired. Do so by module loading Git and calling `git pull` while in your jheem_analyses directory.

### 5. Submit batch jobs
Start your calibration step (setup, run, or assembly) by queuing the relevant script or master script. Queue an individual batch script like the ones generated in Step 1 with `sbatch <batch_script_file_name>`. Queue a master script like the one generated in Step 2 with `bash <master_script_file_name.sh>`, which then runs its own `sbatch` commands for you. If successfully queued, you will see each job listed when you type the `sqme` command.

###	6. Debug failing jobs
The jobs might fail for a variety of reasons, such as a bug in the R code they call or a mistaken file path. You might notice that something is wrong either by seeing a job never start running or a job that disappears earlier than expected from the `sqme` list. You can kill a job that is never going to start running with `scancel <job ID>` where the job ID is the number listed on the `sqme` list. We’ve supplied each batch script with a dedicated output file to which R messages, such as those from R itself, are logged. In the screenshot of the individual batch script above, you can see the line `--output` and the file where this log will go. You can view these files using the `vi` command, which will open them in a text editor. To escape the “vi” text editor, type SHIFT+”:” and then `q`. Note that if you run the same job again and there are no errors, the old error log will still be there but won’t be relevant.

### 7. Conduct the entire calibration process
Calibration typically takes a short setup step, wherein a cache is generated under the `jheem/files` directory for the calibration, a long run step, wherein the simulations are run and saved to that cache, and a short assembly step, wherein the simulations are compiled into a single JHEEM simulation set for export. The `jheem_analyses/cluster_scripts/batch_script_helpers.R` file has methods for each of these steps. The final step of exporting an assembled simset is best done through SCP as in Step 6 of the Set Up instructions.

## Expected Workflow Improvements
1.	Some of the batch script-generating scripts are currently hardcoded for version ‘ehe’, but this will be made into an optional argument to allow for other versions.
2.	Some of module loading steps may be possible to bundle into a bash script to spare some typing.
3.	The `jheem_analyses/cluster_scripts` may benefit from greater organization as more people place generate their own batch scripts in it.
