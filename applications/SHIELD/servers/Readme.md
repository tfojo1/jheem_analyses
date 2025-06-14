# JHEEM/SHIELD Servers (`pearl1`, `shield1`, `shield2`) - User Guide

This guide provides instructions for accessing and using the JHEEM/SHIELD modeling servers: `pearl1.jhsph.edu`, `shield1.jhsph.edu`, and `shield2.jhsph.edu`.

**Note:** You must be connected to the JHU VPN to access any of these servers.

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

You can connect to the servers using their hostnames:
- `pearl1.jhsph.edu`
- `shield1.jhsph.edu`
- `shield2.jhsph.edu`

Or their respective IP addresses if needed:
- `pearl1`: `10.253.171.246`
- `shield1`: `10.253.171.232`
- `shield2`: `10.253.171.233`

### Method 1: Terminal (macOS/Linux) using SSH

Use the following command, replacing `YOUR_USERNAME` with your assigned username and `SERVER_HOSTNAME` with the specific server you are connecting to (e.g., `pearl1.jhsph.edu`).

Type this in your local terminal:
```bash
ssh YOUR_USERNAME@SERVER_HOSTNAME
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
   Host jhu-pearl1 # Alias for pearl1
       HostName pearl1.jhsph.edu
       User YOUR_USERNAME

   Host jhu-shield1 # Alias for shield1
       HostName shield1.jhsph.edu
       User YOUR_USERNAME

   Host jhu-shield2 # Alias for shield2
       HostName shield2.jhsph.edu
       User YOUR_USERNAME
   ```
   Replace `YOUR_USERNAME` with your assigned username on the servers.

3. In VS Code, open the Command Palette (`Cmd+Shift+P` on macOS, `Ctrl+Shift+P` on Windows/Linux), type "Remote-SSH: Connect to Host...", and select the alias you configured (e.g., `jhu-pearl1`).

### Method 3: RStudio Server (Web Browser)

RStudio Server provides a familiar R IDE accessible via your web browser.

1. Open a web browser and go to the appropriate URL:
   - For `pearl1`: `http://pearl1.jhsph.edu:8787`
   - For `shield1`: `http://shield1.jhsph.edu:8787`
   - For `shield2`: `http://shield2.jhsph.edu:8787`
2. Log in with your Linux username and password for that specific server.

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

For *each server* (`pearl1`, `shield1`, `shield2`) you want passwordless access to, run this command from your *local* machine, replacing `SERVER_HOSTNAME` and `YOUR_USERNAME`:

```bash
ssh-copy-id -i ~/.ssh/id_ed25519.pub YOUR_USERNAME@SERVER_HOSTNAME
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

Now, `ssh YOUR_USERNAME@SERVER_HOSTNAME` or connecting via VS Code should not ask for a password (it might ask for your SSH key's passphrase once per session, which your system's SSH agent can often remember).

## Setting Up Your JHEEM Workspace (First-Time Setup)

Organize your JHEEM-related files in your home directory on the server.

### Create Workspace Directories

Open a terminal on the server (e.g., pearl1) and run:

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
  scp -r /path/to/local/cached/* YOUR_USERNAME@pearl1.jhsph.edu:~/jheem/code/jheem_analyses/cached/
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

### Using RStudio Server

1. Access RStudio Server for the specific server (e.g., `http://pearl1.jhsph.edu:8787`) and log in.
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