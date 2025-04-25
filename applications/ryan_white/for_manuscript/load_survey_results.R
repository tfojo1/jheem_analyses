
source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

files = list.files("Q:results/ryan_white/", full.names = T)
load(files[length(files)])
