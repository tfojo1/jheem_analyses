

get.approx.logitnorm.params <- function(expit.mean, expit.sd)
{
  logit.mean = log(expit.mean) - log(1-expit.mean)
  
  opt.result = optimise(f = function(logit.sd){
    interval = 1/(1+exp(-qnorm(c(0.025,0.975), logit.mean, logit.sd)))
    derived.sd = (interval[2]-interval[1])/(2*qnorm(.975))
    
    (derived.sd-expit.sd)^2
  },
  interval = c(0,10))
  
  list(logit.mean = logit.mean,
       logit.sd = opt.result$minimum)
}

# testing
if (1==2)
{
  
  z = get.approx.logitnorm.params(0.3447421, 0.08971986)
  logit.rand = rnorm(100000, z$logit.mean, z$logit.sd)
  rand = 1/(1+exp(-logit.rand))
  mean(rand)
  sd(rand)
  
  z = get.approx.logitnorm.params(0.9529412, 0.02296911)
  logit.rand = rnorm(100000, z$logit.mean, z$logit.sd)
  rand = 1/(1+exp(-logit.rand))
  mean(rand)
  sd(rand)
  
}



