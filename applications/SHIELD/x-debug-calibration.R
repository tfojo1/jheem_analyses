# Steps to manage a bad fit:
# 1- check the fit using simplot()
# 2- check mixing 
# >>Mixing issue? downweighting the likelihoods 
# >>are we using correct variance correlation:  "autoregressive.1" vs "compound symmetry"
# 3- look at the likelihood.compute(debug=T):
# >>  are we looking at the correct values in the plot? data points and simulated points align? 
# 4- look for a manual sim that looks better
# >> does the likelihood also look better? likelihood.compare()
# >> how are the parameter values generating this fit compared to prior?
# >> do we need to revise the prior?
# 5- check the variable values inside the engine q=engine$extract.quantity.values()        
# 
# 6- Simultanous fit is not achieved:can we simplify the model more? 
#     
# >>>run a demographic calibration first model    
# >>>use it as a starting point for the next calibration focusing on total diagnosis (relax the demographic parameters)
# >>>strip away unrelated model dynamics to simplify the model (e.g., relapse=0)
# 
 