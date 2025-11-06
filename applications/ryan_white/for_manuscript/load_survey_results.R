
source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

files = list.files("Q:results/ryan_white/", full.names = T)

if (RW.IS.STATE.LEVEL)
{
    files = files[grepl('state', files)]
    files = files[!grepl('nanb', files)]
   # stop("need to do costing for state level")
}
if (!RW.IS.STATE.LEVEL)
{
    files = files[grepl('city', files)]
    # load('applications/ryan_white/rw_costs.Rdata')
    # RW.COSTS = RW.CITY.COSTS
}

load(files[length(files)])
