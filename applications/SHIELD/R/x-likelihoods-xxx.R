target=2
likelihood.sd=1
# The absolute value of lielihood dosnt matter, what is important is the relativelikelihood because when we decide to jum we compute lik1/lik2 to decide about the next step
dnorm(3,mean=target, s=likelihood.sd )

#relative fit between a simulation right on target vs one that is 1 sd away:
dnorm(3,mean=target, s=likelihood.sd)/dnorm(2,mean=target, s=likelihood.sd )

w=2
# w=1/8
w_sd= sqrt(likelihood.sd^2 * 1/w) #weighted likelihood sd (it takes inverse of variance)
dnorm(3,mean=target, s=w_sd)/dnorm(2,mean=target, s=w_sd )

# if w=2:we need a closer fit to the target, the weight reduces the sd around target
# alternative w<1 > downwighting the target, will allow for a more loose fit
#     

# if we have 2 seperate, independant points to fit: their likelihoods multiply
    # dnorm(3,mean=target, s=w_sd) * dnorm(3,mean=target, s=w_sd)

(dnorm(3,mean=target, s=likelihood.sd)^2)/dnorm(2,mean=target, s=likelihood.sd )^2

# if we have 2 points for the first target instead of 1 point, its essenstialy the same as having a weight based on inverse of data points
n=2
w=1/n 
w_sd= sqrt(likelihood.sd^2 * 1/w) #weighted likelihood sd (it takes inverse of variance)
dnorm(3,mean=target, s=w_sd)^n/dnorm(2,mean=target, s=w_sd )^n

# another point about normdistributions.equal
# as we get far from the target, we are loosing a lot of weigh , 
# and that means that improvemens achived by moving one sd dont impact the likelihood that much
# if simulation stuck in a point that is far away from that targets, we may get stuck because imprvement are not that important.
# this is why we can downweight hte likelihood to help paramter mixing 

#run the population : only demographic likelihoods and parameters 
# run the second stage calibration for transmission parameters: using the prior from population
## may need to simplify the likelihood in stages, and downweight to help mixing
## can use the demographic 

# run a full calibration with 1m points:


dnorm(3,mean=target, s=likelihood.sd)/dnorm(2,mean=target, s=likelihood.sd )
dnorm(4,mean=target, s=likelihood.sd)/dnorm(3,mean=target, s=likelihood.sd )
dnorm(5,mean=target, s=likelihood.sd)/dnorm(4,mean=target, s=likelihood.sd )


# if we have more data points for for one target  and fewer for another target: n1 for O1 vs n2 for O2 where n1<<nt
# we can leave them be: if there is no internal conflict betwen the O1 and O2 targets, the model should be able to fit both of them
# if there is some internal conflict where the model should choose between a better fit to one target over the other one, and we are OK with this behavior:
#     we can down weight the target with more data points w=1/additional weights=n2/n1 