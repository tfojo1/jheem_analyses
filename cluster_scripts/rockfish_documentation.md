# Rockfish Setup and Usage Instructions
Andrew Zalesak

Updated April 16th, 2025

## Documentation
Rockfish has decent documentation on its website, although it didn’t answer a few of the questions I had. The main User Guide is [here](https://www.arch.jhu.edu/guide/) and some other information can be found under the “Support” tab. The Rockfish tech support are kind and helpful and can be contacted at help@rockfish.jhu.edu with questions.

I **STRONGLY** recommend that you use a Graphical User Interface (GUI) for accessing the cluster instead of just the terminal, because you will find searching your directories and uploading/downloading files *VASTLY* easier that way. Examples include MobaXterm and VSCode.

## Setup

### 1. Join Rockfish
Make sure you’ve got a Rockfish account and are on Parastu’s allocation (pkasaie1). If so, you can log in on a Windows PowerShell (or your favorite GUI -- see above) using `ssh <username>@login.rockfish.jhu.edu` and then supplying your Rockfish-specific password. This will start you on a Login Node, which is good for setting up file organization but shouldn’t be used for running R. You’ll want to be familiar with basic Unix commands to move between directories (`cd`) and view contents (`ls`).

###	2. Set up directories
In your command line, run the following: `mkdir -p $HOME/src4_pkasaie1/$USER/jheem/{code,files}` to create the directory structure we all use, but specific to your user's subdirectory of `src4_pkasaie1`. Note that "src4_pkasaie1" is a shortcut for `scratch4/pkasaie1`.

### 3. Clone repositories
Navigate to your newly created directory "code" with `cd $HOME/src4_pkasaie1/$USER/jheem/code`. Follow the instructions [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) for cloning a repository using the “SSH” method to clone the `jheem_analyses` repository from Github (https://github.com/tfojo1/jheem_analyses). I can’t recall exactly what I did, but it probably required setting up an SSH key as described here. Note that to use Git operations on Rockfish, you may have to load the `git` module, using the terminal command `module load git`. Our goal is to have everything on Rockfish using jheem2 in package form, but if that’s not available yet, then you’ll want to clone the jheem2 repository from Github as well (https://github.com/tfojo1/jheem2)

### 4. Install R packages
Before you install, make sure you have the R modules loaded by typing `source cluster_scripts/Rockfish_module_loads.sh` from your `jheem_analyses` directory. Then, install the needed packages with `Rscript cluster_scripts/rockfish_install_packages.R`. If any installations fail, you should be able to see which ones they are and then reach out to me to solve any issues.

### 5. Import required objects through drag-and-drop... or SCP if you don't have a GUI
There are several R data objects that most JHEEM model specifications use that are not transferred through Git, like data managers. Since these objects are not terribly large (at most around 100 MB), they can be imported easily from your local desktop to Rockfish with the `scp` command. Although we’ve written code to automatically download certain data managers from the Internet, I used SCP to bring these in on my first attempt since I was already doing so for some other cached objects. The full list of the required non-Git-controlled objects, as far as I know, is:

- census.manager.rdata
- DEFAULT.LOCALE.MAPPING.Rdata
- google_mobility_data.Rdata
- msa.surveillance.Rdata
- national.surveillance.Rdata
- surveillance.manager.rdata
- syphilis.manager.rdata

*Note that if you have a nice GUI (see above), you can just drag and drop these files straight onto the cluster and avoid using SCP commands entirely!*

To put these in using SCP, use your PowerShell from outside of Rockfish (quit Rockfish using `exit` as many times as needed). You’ll want to know the path on your own computer to where you’ve stored these objects, and you’ll want to know where you’re going to put them on Rockfish. The rough syntax for SCP file transfer is `scp <path_to_local_object> <rockfish_address>:<rockfish_file_path>`. I recommend using `cd` or similar on your desktop to get straight to where these files are located, so that the `<path_to_local_object>` can just be the file name, like `census.manager.rdata`. The `<rockfish_address>` is going to be your Rockfish username followed by an “@” sign and then the address of the Rockfish data transfer node, which is `rfdtn1.rockfish.jhu.edu`. The destination file is going to look a little different from how it is when you’re in Rockfish, because some of the directories you see, like `scr4_pkasaie1`, are actually symbolic links to places like `scratch4/pkasaie1`. For me to import a `census.manager.rdata` object that I’ve navigated to locally into my Rockfish directory `HOME/scr4_pkasaie1/azalesak/jheem/code/jheem_analyses/cached`, I would use the following full SCP command:

`scp census.manager.rdata azalesak@rfdtn1.rockfish.jhu.edu:/scratch4/pkasaie1/azalesak/jheem/code/jheem_analyses/cached/census.manager.rdata`.

You’ll have to provide your Rockfish password to authorize this file transfer. Note that you can use the same command but with the source and destination flipped to get objects off Rockfish. That will come in handy when exporting calibration simsets.

## Usage

### 1. Use our R scripts to generate calibration batch scripts
The purpose of Rockfish is to allow us to run lengthy and memory-intensive R operations simultaneously across multiple cores so that calibrating doesn’t take an eternity. Rockfish receives requests for computation through the submission of SLURM “batch scripts”. These batch scripts contain some header information on how much memory and time should be devoted to a given job, followed by the requested operation itself in command-line form, like running a particular R script. Here is an example of a batch script generated to set up a calibration for Baltimore:

![](batch_script_screenshot.png)
 
Line 10 is a command line call to an R script we’ve written to do the calibration for version “ehe”, location “C.12580”, and calibration code “init.pop.ehe”. We want to generate batch scripts like this for many cities at the same time, so to do this we can use a script in `jheem_analyses/cluster_scripts` called `batch_script_helpers.R`. It contains a method called `make.setup.scripts` that can take a vector of locations as an argument. It will then fill the `cluster_scripts/setup_scripts/` directory (or another, if specified) with batch scripts like seen above that can be queued with SLURM on Rockfish. Note that there are analogs to this setup script for other steps like “running” a calibration.

### 2. Use another R script to generate a master script
Having dozens of scripts like the one above would require queuing each one individually (see Usage Step 5). Instead, you can make a bash script that, when run, will queue all your dozens of batch scripts at once. We call this a “master script”, and you can generate one with `make.setup.master.script` by supplying a name for the resulting master script and a vector of locations. The resulting script might look like this:

![](master_script_screenshot.png)
 
### 3. Import the generated scripts to Rockfish with Git
The previous steps could have been performed on your local desktop or on a Rockfish interact session, with pros and cons to each approach. If you did it locally, you can use a “pull/stage/commit/push” sequence to push the newly generated scripts to the remote repository on Github, and then pull it down in Rockfish using `git pull`. In Rockfish, your working directory will have to be somewhere in jheem_analyses for Git to know what repository you’re wanting to pull, and you may have had to module load Git with `module load git`. You’ll also need to supply the Git SSH password you made during setup for each Git operation you do. If you always generate these batch scripts outside of Rockfish, then all you’ll have to worry about inside Rockfish is `git pull`. It’s possible that generating them inside Rockfish will be more convenient under some circumstances. In such a case, you’ll have to take extra steps like staging, committing, and pushing to keep your Rockfish files synced with your local ones.

### 4. Pull Git to update model code
Before starting calibration, make sure that you’ve got the most recent set of files in jheem_analyses if desired. Do so by module loading Git and calling `git pull` while in your jheem_analyses directory.

### 5. Submit batch jobs
Start your calibration step (setup, run, or assembly) by queuing the relevant script or master script. Queue an individual batch script like the ones generated in Step 1 with `sbatch <batch_script_file_name>`. Queue a master script like the one generated in Step 2 with `source <master_script_file_name.sh>`, which then runs its own `sbatch` commands for you. If successfully queued, you will see each job listed when you type the `sqme` command.

###	6. Debug failing jobs
The jobs might fail for a variety of reasons, such as a bug in the R code they call or a mistaken file path. You might notice that something is wrong either by seeing a job never start running or a job that disappears earlier than expected from the `sqme` list. You can kill a job that is never going to start running with `scancel <job ID>` where the job ID is the number listed on the `sqme` list. We’ve supplied each batch script with a dedicated output file to which R messages, such as those from R itself, are logged. In the screenshot of the individual batch script above, you can see the line `--output` and the file where this log will go. You can view these files using the `vi` command, which will open them in a text editor. To escape the “vi” text editor, type SHIFT+”:” and then `q`. Note that if you run the same job again and there are no errors, the old error log will still be there but won’t be relevant.

### 7. Conduct the entire calibration process
Calibration typically takes a short setup step, wherein a cache is generated under the `jheem/files` directory for the calibration, a long run step, wherein the simulations are run and saved to that cache, and a short assembly step, wherein the simulations are compiled into a single JHEEM simulation set for export. The `jheem_analyses/cluster_scripts/batch_script_helpers.R` file has methods for each of these steps. The final step of exporting an assembled simset is best done through SCP as in Step 6 of the Set Up instructions.
