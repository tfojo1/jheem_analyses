

BIG.DESKTOP.ROOT.DIR = 'Q:'

LAPTOP.ROOT.DIR = '../../files'
if (file.exists(file.path("../test_runs", "mcmc_runs"))) # a hedge against early on when we kept the files in the code subdirectory
    LAPTOP.ROOT.DIR = '../test_runs'

RUNNING.ON.DESKTOP = file.exists(file.path(BIG.DESKTOP.ROOT.DIR, "mcmc_runs"))

ROOT.DIR = LAPTOP.ROOT.DIR
if (RUNNING.ON.DESKTOP)
    ROOT.DIR = BIG.DESKTOP.ROOT.DIR