

BIG.DESKTOP.ROOT.DIR = 'Q:'
MAC.ROOT.DIR = "/Volumes/jheem$"
JHU_SERVER_NAS_MOUNT_PATH = "/mnt/jheem_nas_share"

JHU_SERVER_HOSTNAMES = c(
    "pearl1.jhsph.edu", "pearl1",
    "shield1.jhsph.edu", "shield1",
    "shield2.jhsph.edu", "shield2"
)

LAPTOP.ROOT.DIR = '../../files'
if (file.exists(file.path("../test_runs", "mcmc_runs"))) # a hedge against early on when we kept the files in the code subdirectory
    LAPTOP.ROOT.DIR = '../test_runs'

RUNNING.ON.DESKTOP = file.exists(file.path(BIG.DESKTOP.ROOT.DIR, "mcmc_runs"))
RUNNING.ON.MAC = file.exists(file.path(MAC.ROOT.DIR, "mcmc_runs"))
RUNNING.ON.CLUSTER = F
RUNNING.ON.LAPTOP = !RUNNING.ON.DESKTOP && !RUNNING.ON.CLUSTER

ROOT.DIR = LAPTOP.ROOT.DIR
if (RUNNING.ON.DESKTOP)
    ROOT.DIR = BIG.DESKTOP.ROOT.DIR

if (RUNNING.ON.MAC)
    ROOT.DIR = MAC.ROOT.DIR

# Check for JHU Servers (pearl1, shield1, shield2)
IS_JHU_SERVER = (Sys.info()["nodename"] %in% JHU_SERVER_HOSTNAMES) &&
                dir.exists(JHU_SERVER_NAS_MOUNT_PATH) &&
                file.exists(file.path(JHU_SERVER_NAS_MOUNT_PATH, "mcmc_runs"))

if (IS_JHU_SERVER) {    
    ROOT.DIR = JHU_SERVER_NAS_MOUNT_PATH
}

