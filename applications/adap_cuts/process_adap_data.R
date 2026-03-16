source("applications/ryan_white/ryan_white_main.R")
source("applications/adap_cuts/fitting_income_distributions.R")
source("applications/ryan_white/process_rw_data_for_priors.R")
ROOT.DIR = "../jheem_analyses/applications/ryan_white/ryan_white_data/adap_clients"

# CHECK ALL OF THIS TOMORROW 

income.race = read.adap.income.data(ROOT.DIR,
                                    dimension = "race")

income.age = read.adap.income.data(ROOT.DIR,
                                    dimension = "age")

income.sex.1 = read.adap.income.data(ROOT.DIR,
                                     dimension = "sex",
                                     years=2020:2022)

income.sex.2 = read.adap.income.data(ROOT.DIR,
                                     dimension = "sex",
                                     years=2023)

income.service = read.adap.income.data(ROOT.DIR,
                                   dimension = "service")



