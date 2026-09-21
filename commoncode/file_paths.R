

BIG.DESKTOP.ROOT.DIR = 'Q:'
MAC.ROOT.DIR = "/Volumes/jheem$"
JHU_SERVER_NAS_MOUNT_PATH = "/mnt/jheem_nas_share"

JHU_SERVER_HOSTNAMES = c(

    "shield1",
    "shield2",
    "shield3"
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


# =============================================================================
# LOCAL OVERRIDE
# =============================================================================
# 1. WHAT IT DOES
#    If the environment variable JHEEM_ROOT_DIR is set, it wins over every
#    choice made above. Everything jheem2 writes - mcmc_runs/ and simulations/ -
#    goes there instead of the NAS.
#
# 2. WHY YOU WOULD USE IT
#    The NAS share fills up, or you want a run to not depend on the network.
#    Nothing else moves: data managers still come from JHEEM.CACHE.DIR, which
#    is repo-relative, so reads are unaffected.
#
# 3. HOW TO USE IT
#    Export it before launching, in the same shell that starts the jobs:
#      export JHEEM_ROOT_DIR=/data/user/jheem
#    Every stage of a pipeline must see the same value. A run cannot find the
#    output of a setup step that wrote somewhere else.
#
# 4. UNSET IT TO GO BACK
#    With JHEEM_ROOT_DIR unset the logic above is untouched and runs return to
#    the NAS.
#
JHEEM.ROOT.DIR.OVERRIDE = Sys.getenv("JHEEM_ROOT_DIR", unset = "")

if (nzchar(JHEEM.ROOT.DIR.OVERRIDE)) {
    ROOT.DIR = path.expand(JHEEM.ROOT.DIR.OVERRIDE)
    
    # Create it on first use. mcmc_runs/ and simulations/ are made here rather
    # than left to jheem2, because the RUNNING.ON.* checks above test for
    # mcmc_runs/ and a bare directory would look like an unconfigured root.
    for (d in c(ROOT.DIR,
                file.path(ROOT.DIR, "mcmc_runs"),
                file.path(ROOT.DIR, "simulations"))) {
        if (!dir.exists(d))
            dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
    
    if (!dir.exists(ROOT.DIR))
        stop(paste0("JHEEM_ROOT_DIR is set to '", JHEEM.ROOT.DIR.OVERRIDE,
                    "' but that directory could not be created."))
    
    cat(paste0("JHEEM_ROOT_DIR override in effect: ", ROOT.DIR, "\n"))
}

