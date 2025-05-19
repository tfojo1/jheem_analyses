

BIG.DESKTOP.ROOT.DIR = 'Q:'
MAC.ROOT.DIR = "/Volumes/jheem$"
PEARL1.ROOT.DIR = "/mnt/jheem_nas_share"

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

if ((Sys.info()["nodename"] == "pearl1.jhsph.edu" || Sys.info()["nodename"] == "pearl1") &&
    dir.exists(PEARL1.ROOT.DIR) &&
    file.exists(file.path(PEARL1.ROOT.DIR, "mcmc_runs"))) {
    
    ROOT.DIR = PEARL1.ROOT.DIR
}

