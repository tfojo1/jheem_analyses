# JHEEM/SHIELD Servers (`pearl1`, `shield1`, `shield2`) - User Guide

This guide provides instructions for accessing and using the JHEEM/SHIELD modeling servers: `pearl1.jhsph.edu`, `shield1.jhsph.edu`, and `shield2.jhsph.edu`.

**Note:** You must be connected to the JHU VPN to access any of these servers.

> **Important: DNS Not Yet Updated (January 2026)**
>
> The `shield1` and `shield2` servers were recently moved and assigned new IP addresses. The DNS records have **not yet been updated**, so connecting via hostname (e.g., `shield1.jhsph.edu`) will fail. **Use the IP addresses directly** until DNS is updated.

## Getting Started & Account Management

### Initial Login and Password Change

Upon receiving your username and temporary password, you should log in via SSH and immediately change your password.

1. Connect via SSH (see next section).
2. Once logged in, type the command:
   ```bash
   passwd
   ```
3. You will be prompted to enter your current (temporary) password and then your new password twice. Choose a strong, unique password.

### Concurrent Access

Multiple users can be logged in and working on the servers simultaneously. Be mindful of shared resources if running very computationally intensive tasks.

## Connecting to the Servers

### Server Addresses

| Server | Hostname | IP Address |
|--------|----------|------------|
| pearl1 | `pearl1.jhsph.edu` | `10.253.171.246` |
| shield1 | `shield1.jhsph.edu` | `10.253.170.91` |
| shield2 | `shield2.jhsph.edu` | `10.253.170.89` |

> **Reminder:** Until DNS is updated, use IP addresses for `shield1` and `shield2`.

### Method 1: Terminal (macOS/Linux) using SSH

Use the following command, replacing `YOUR_USERNAME` with your assigned username and `SERVER_ADDRESS` with the IP address (or hostname once DNS is fixed).

Type this in your local terminal:
```bash
ssh YOUR_USERNAME@10.253.170.91   # For shield1
ssh YOUR_USERNAME@10.253.170.89   # For shield2
ssh YOUR_USERNAME@pearl1.jhsph.edu   # For pearl1
```

### Method 2: VS Code with Remote - SSH Extension

This provides a rich editing environment directly on the server.

1. Ensure the "Remote - SSH" extension by Microsoft is installed in VS Code on your local machine.
2. Add the server(s) to your local SSH configuration file (`~/.ssh/config` on your machine). If the file and directory don't exist, create them first:
   ```bash
   mkdir -p ~/.ssh
   touch ~/.ssh/config
   chmod 700 ~/.ssh
   chmod 600 ~/.ssh/config
   ```

   Then edit the `~/.ssh/config` file (you can open it in VS Code) and add these entries:
   ```
   Host jhu-pearl1
       HostName pearl1.jhsph.edu
       User YOUR_USERNAME

   Host jhu-shield1
       HostName 10.253.170.91
       User YOUR_USERNAME

   Host jhu-shield2
       HostName 10.253.170.89
       User YOUR_USERNAME
   ```
   Replace `YOUR_USERNAME` with your assigned username on the servers.

   > **Note:** Once DNS is updated, you can change the `HostName` for shield1/shield2 back to `shield1.jhsph.edu` and `shield2.jhsph.edu`.

3. In VS Code, open the Command Palette (`Cmd+Shift+P` on macOS, `Ctrl+Shift+P` on Windows/Linux), type "Remote-SSH: Connect to Host...", and select the alias you configured (e.g., `jhu-shield1`).

### Method 3: RStudio Server (Web Browser)

RStudio Server provides a familiar R IDE accessible via your web browser.

1. Open a web browser and go to the appropriate URL:
   - For `pearl1`: `http://pearl1.jhsph.edu:8787`
   - For `shield1`: `http://10.253.170.91:8787`
   - For `shield2`: `http://10.253.170.89:8787`
2. Log in with your Linux username and password for that specific server.

> **Note:** Once DNS is updated, you can use the hostnames (e.g., `http://shield1.jhsph.edu:8787`).

### Recommended: SSH Key-Based Authentication (Passwordless Login)

Setting up SSH keys allows you to log in without typing your password each time, which is more convenient and secure.

#### A. Check for/Generate SSH Keys on Your Local Machine

In a terminal on your *local* machine:

1. Check for existing keys:
   ```bash
   ls -l ~/.ssh/id_ed25519 ~/.ssh/id_rsa
   ```
2. If no suitable key exists (e.g., `id_ed25519` and `id_ed25519.pub`), generate one:
   ```bash
   ssh-keygen -t ed25519 -C "your_email@example.com"
   ```
   (Press Enter to accept defaults for file location and if desired, a strong passphrase when prompted).

#### B. Copy Your Public Key to EACH Server

For *each server* (`pearl1`, `shield1`, `shield2`) you want passwordless access to, run this command from your *local* machine:

```bash
# For pearl1:
ssh-copy-id -i ~/.ssh/id_ed25519.pub YOUR_USERNAME@pearl1.jhsph.edu

# For shield1 (use IP until DNS is fixed):
ssh-copy-id -i ~/.ssh/id_ed25519.pub YOUR_USERNAME@10.253.170.91

# For shield2 (use IP until DNS is fixed):
ssh-copy-id -i ~/.ssh/id_ed25519.pub YOUR_USERNAME@10.253.170.89
```

You will be prompted for your password for that server one last time to authorize the key.

#### C. (Optional) Update SSH Config for Keys

If you used a non-default key name, add/update the IdentityFile line in your `~/.ssh/config` for each host entry:

```
Host jhu-pearl1
    HostName pearl1.jhsph.edu
    User YOUR_USERNAME
    IdentityFile ~/.ssh/your_private_key_name
```

If you used the default key (`id_ed25519` or `id_rsa`), SSH usually tries it automatically.

Now, `ssh YOUR_USERNAME@SERVER_ADDRESS` or connecting via VS Code should not ask for a password (it might ask for your SSH key's passphrase once per session, which your system's SSH agent can often remember).

## Setting Up Your JHEEM Workspace (First-Time Setup)

Organize your JHEEM-related files in your home directory on the server.

### Create Workspace Directories

Open a terminal on the server (e.g., shield1) and run:

```bash
cd ~  # Go to your home directory
mkdir -p jheem/code
mkdir -p jheem/files
```

This creates:
- `~/jheem/code/`: For storing cloned Git repositories.
- `~/jheem/files/`: For model outputs and certain input data dependencies.

### Clone Core Git Repositories

Navigate into your code directory:
```bash
cd ~/jheem/code
```

Then clone the necessary repositories:
```bash
git clone https://github.com/tfojo1/jheem_analyses.git
git clone https://github.com/tfojo1/jheem2.git
```

(Add other JHEEM-related Git repositories if needed). Note that `jheem2` is often run from source rather than as an installed package for development purposes.

### Set Up the cached Data Directory for jheem_analyses

The `jheem_analyses` model requires a cached data directory. This directory is ignored by Git due to its size.

Navigate into your `jheem_analyses` clone:
```bash
cd ~/jheem/code/jheem_analyses
```

Create the cached directory:
```bash
mkdir cached
```

Populate this `~/jheem/code/jheem_analyses/cached/` directory with the required files (these can be downloaded from OneDrive if you need them).

- If using VS Code, you can often drag and drop files/folders from your local machine directly into the cached directory visible in VS Code's file explorer connected to the server.
- Alternatively, use `scp` or other file transfer methods. For example, to copy all files from a local cached folder to the server's cached directory:
  ```bash
  scp -r /path/to/local/cached/* YOUR_USERNAME@10.253.170.91:~/jheem/code/jheem_analyses/cached/
  ```

## Running R and Model Scripts

**R Environment:** R and common JHEEM-related R packages are installed globally on the server. You generally do not need to install these for standard model runs.

You can run R scripts from the command line or use RStudio Server.

### From the Command Line

Navigate to your `jheem_analyses` directory (or the specific script location):
```bash
cd ~/jheem/code/jheem_analyses/
```

Run the script using Rscript:
```bash
Rscript applications/SHIELD/shield_calib_setup_and_run.R
```

## Managing Long-Running `Rscript` Jobs from the Terminal

When you run computationally intensive R scripts (like MCMC calibrations) using `Rscript` from the terminal, especially if you might disconnect from the server, here are some best practices and tools:

### 1. Preventing Jobs from Terminating on Disconnect & Capturing Output

If you simply run `Rscript my_model.R` and then your SSH connection (e.g., through VS Code or a terminal) drops or you close the window, the R script will typically be terminated. To prevent this and capture output:

#### Method A: Using `nohup` and Output Redirection (Simple & Effective)

`nohup` (no hangup) allows a command to keep running even after you log out. It also redirects standard output and standard error.

**To run a script and save output to a file:**
```bash
nohup Rscript applications/SHIELD/shield_calib_setup_and_run.R > shield_run_output.txt 2>&1 &
```

- `nohup`: Runs the command immune to hangups.
- `Rscript ...`: Your R script command.
- `>`: Redirects standard output (what normally prints to the terminal).
- `shield_run_output.txt`: The file where standard output will be saved. Choose a descriptive name.
- `2>&1`: Redirects standard error (error messages) to the same place as standard output. This is important for capturing R errors.
- `&`: Runs the command in the background, so you get your terminal prompt back immediately. The script will continue running on the server.

**Checking Output:**
- The output will be saved to `shield_run_output.txt` (or whatever you named it).
- If you didn't specify redirection with `>`, `nohup` typically creates a file named `nohup.out` in your current directory (or home directory if the current isn't writable) and appends output there.
- You can view the output file live (if it's being updated) using:
  ```bash
  tail -f shield_run_output.txt
  ```
  (Press `Ctrl+C` to stop `tail -f`).

#### Method B: Using Terminal Multiplexers (`screen` or `tmux`) (More Advanced, Very Powerful)

`screen` and `tmux` are terminal multiplexers that allow you to create persistent terminal sessions. You can start a job within a `screen` or `tmux` session, detach from it (leaving the job running on the server), log out, and then log back in later and reattach to the same session to see the live terminal and its history.

**Basic `screen` Usage:**
1. Start a new screen session: `screen -S my_r_job`
2. You'll get a new terminal prompt inside the screen session. Run your R script normally:
   ```bash
   Rscript applications/SHIELD/shield_calib_setup_and_run.R
   ```
   (You can still redirect output to a file here if you want a permanent log: `Rscript ... > output.txt 2>&1`)
3. **Detach from the screen session:** Press `Ctrl+A` then `d`. You'll be back in your original terminal, but `my_r_job` and the R script are still running inside the detached screen.
4. You can now log out of the server.
5. **Reattach later:** Log back into the server, then type: `screen -r my_r_job`
   (Or `screen -ls` to list sessions, then `screen -r <session_id_or_name>`).

`tmux` is similar but with different keybindings and features. Learning `screen` or `tmux` is highly recommended for managing long-running server tasks.

### 2. Finding and Monitoring Ongoing R Jobs

#### Method A: Using `ps` (Standard Linux Command)

To see your R processes:
```bash
ps -fu YOUR_USERNAME | grep '[R]script'
# Or more generally for any R process:
# ps -fu YOUR_USERNAME | grep '[R]'
```

Replace `YOUR_USERNAME` with your actual username (e.g., `ps -fu nsizemo1 | grep '[R]script'`).

The `[R]` in `grep '[R]'` is a trick to prevent grep from finding its own process line.

This will show you the Process ID (PID) and the command.

#### Method B: Using JHEEM's get.calibration.progress Function

If the R script is a JHEEM calibration that uses the project's caching mechanism, the `get.calibration.progress` R function can provide model-specific progress.

**How to use it:**
You would typically run this in a separate R session (e.g., in RStudio Server, or another terminal logged into the server running R).

```r
# Inside an R session:
# Ensure necessary JHEEM setup/source files are loaded for this function to be available
# and for it to know where to look for calibration cache files.

# Example:
# source("path/to/your/main_jheem_setup.R") # If needed

get.calibration.progress(version = "shield",
                         locations = "C.12580", # Or a vector of locations
                         calibration.code = "name_of_your_calibration")
                         # root.dir might need to be specified if not default
```

This function likely reads from the cache files being written by the ongoing MCMC run. The exact arguments (version, locations, calibration.code, root.dir) must match what the running calibration is using.

### 3. Checking Status of a Specific Calibration and Killing a Job

#### Checking Status:
- Use `ps` as described above to see if the Rscript process is still running.
- Use the `get.calibration.progress` R function as described above.
- Check the output file you created (e.g., `tail shield_run_output.txt`) or the `nohup.out` file.

#### Killing a Job (Terminating an R Process):

If you need to stop an R script that's running (e.g., if it's stuck or you started it with incorrect parameters):

1. **Find its Process ID (PID):**
   Use `ps -fu YOUR_USERNAME | grep '[R]script'` to find the PID of the specific `Rscript ...` command you want to kill.

2. **Send a Terminate Signal (Graceful Shutdown):**
   ```bash
   kill PID_OF_RSCRIPT
   ```
   (Replace `PID_OF_RSCRIPT` with the actual number). This asks the process to shut down cleanly.

3. **If it doesn't stop after a short while, Send a Kill Signal (Forceful Shutdown):**
   Use with caution, as this stops the process immediately without cleanup.
   ```bash
   kill -9 PID_OF_RSCRIPT
   ```

If the job was started in `screen` or `tmux`, you would typically reattach to that session and stop the R script from within its terminal using `Ctrl+C`.

### Using RStudio Server

1. Access RStudio Server for the specific server (e.g., `http://10.253.170.91:8787`) and log in.
2. Your R session will start in your home directory (`/home/YOUR_USERNAME`).
3. Use the Files pane in RStudio to navigate to your `jheem/code/jheem_analyses/` directory.
4. Open R scripts (e.g., `applications/SHIELD/shield_calib_setup_and_run.R`).
5. Run scripts using the "Source" button or by selecting lines and pressing Run.

**Model Outputs:** Model outputs are typically saved to subdirectories within your `~/jheem/files/` directory or within the `jheem_analyses` clone (e.g., `prelim_results/`), as specified by the R script.

### Personal R Packages

If you require an R package that is not already installed globally, you can install it into your personal R library. In an R console (either command-line R or RStudio Server console):

```r
install.packages('my_needed_package')
```

These packages will be installed in your home directory (usually in a subdirectory like `~/R/`) and will not affect other users.

## Using the Shared JHU NAS (jheem$ share)

The servers provide access to the `jheem$` share on `cloud.nas.jh.edu` for shared datasets, prior model outputs, etc.

**Local Mount Point on Servers:** `/mnt/jheem_nas_share`

### Availability

This NAS share is manually mounted by a server administrator when access is required. It may not always be active, especially after a server reboot.

Contact a server administrator if you need access and it appears unmounted.

### How to Check if Mounted

In a server terminal, run:
```bash
df -hT | grep /mnt/jheem_nas_share
```

If this command produces output, the share is mounted. You can also try:
```bash
ls /mnt/jheem_nas_share
```

### Permissions

- You should be a member of the `jheem` group on the server.
- When the NAS is mounted at `/mnt/jheem_nas_share`, members of the `jheem` group have read and write access to its contents.
- Other users on the server (not in the `jheem` group) have read-only access.

### Accessing Data

Once mounted, you can access files directly from the command line or RStudio Server, e.g.:
```
/mnt/jheem_nas_share/mcmc_runs/shield/C.12580/pop.demog.8/
```

## Group Membership

You have been added to the `jheem` group on the servers to facilitate shared access to resources like the NAS mount. You can confirm your active groups by typing:
```bash
groups
```
