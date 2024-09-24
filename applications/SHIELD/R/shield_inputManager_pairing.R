# This code uses actual data from the literature to set up the pairing matrices
# most of these matrices are presented as "from" and "to" matrices, which are used to create the pairing matrices
# the JHEEM focus is uninfected cases, so the matrices are estiamted such that sum of contacts to each group (columns) equals one
# example: for person age 15-20, what proportion of contacts are coming from different age groups and what's the prevalence of HIV in each of those groups (to estimate risk of transmission)

#
#' @title create.pairing.manager
#' @description creating pairing inputs for the model; data is in .../data_files/pairing"
#' @param dir the directory where the data files are stored
#' @return returns a list of inputs for the model
create.pairing.manager <- function(dir)
{
  rv = list()

  #-- SEXUAL PAIRINGS by SEX --# ----
  ##---- Msm with females ----

  #from CDC NHBS: https://www.cdc.gov/hiv/pdf/library/reports/surveillance/cdc-hiv-surveillance-special-report-number-22.pdf
  msm.sex.with.female.1 = (885+353)/(885+353+6422)
  #From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
  msm.sex.with.female.2 = (39+5+3+16) / (39+5+3+16 + 394+190+112+71)

  rv$msm.sex.with.female.estimates = c(msm.sex.with.female.1, msm.sex.with.female.2)

  #-- SEXUAL PAIRINGS by RACE --## ----
  # we have 4 studies to inform this:
  msm.sex.by.race.1 = msm.sex.by.race.2 = msm.sex.by.race.3 = msm.sex.by.race.4 =
    array(0, dim=c(race.from=3, race.to=3),
          dimnames=list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other')))

  ##---- MSM #1 - From Bohl 2011 --# ----
  raw.msm.sex.by.race = t(matrix(c(77,26,98,295,10,72,56,110,35,33,130,266,333,150,464,1318), nrow=4))

  raw.to.race.strata = c(3,1,2,3)
  for (r1 in 1:dim(raw.msm.sex.by.race)[1])
  {
    for (r2 in 1:dim(raw.msm.sex.by.race)[2])
      msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.1[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
        raw.msm.sex.by.race[r1,r2]
  }

  ##---- MSM #2 - Mustanski 2014 --# ----
  # From https://link.springer.com/article/10.1007%2Fs10461-014-0955-0
  # PMID = 25430501
  raw.msm.sex.by.race = matrix(c(94*2.07*c(82,5.7,4.3,7.8)/100,
                                 37*2.27*c(7.7,56.1,30.6,5.6)/100,
                                 24*1.79*c(2.6,14.7,75.0,7.8)/100,
                                 19*2.32*c(37.9,27.6,26.4,8.0)/100),
                               nrow=4)

  raw.to.race.strata = c(1,2,3,3)
  for (r1 in 1:dim(raw.msm.sex.by.race)[1])
  {
    for (r2 in 1:dim(raw.msm.sex.by.race)[2])
      msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.2[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
        raw.msm.sex.by.race[r1,r2]
  }

  ##---- MSM #3 - Fujimoto 2015 --# ----
  #From https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4312750/
  msm.sex.by.race.3['black','black'] = 610
  msm.sex.by.race.3['hispanic','black'] = 104
  msm.sex.by.race.3['other','black'] = 330
  msm.sex.by.race.3['black','hispanic'] = 8
  msm.sex.by.race.3['hispanic','hispanic'] = 49
  msm.sex.by.race.3['other','hispanic'] = 226
  msm.sex.by.race.3['black','other'] = 221
  msm.sex.by.race.3['hispanic','other'] = 191
  msm.sex.by.race.3['other','other'] = 1333

  ##---- MSM #4 - Raymond 2009 --# ----
  raw.msm.sex.by.race = matrix(c(79,10,333,35,
                                 28,76,151,33,
                                 303,115,1332,271,
                                 103,57,472,134),
                               nrow=4)
  #indexed Asian, Black, White, Latino
  raw.to.race.strata = c(3,1,3,2)
  for (r1 in 1:dim(raw.msm.sex.by.race)[1])
  {
    for (r2 in 1:dim(raw.msm.sex.by.race)[2])
      msm.sex.by.race.4[raw.to.race.strata[r1], raw.to.race.strata[r2]] = msm.sex.by.race.4[raw.to.race.strata[r1], raw.to.race.strata[r2]] +
        raw.msm.sex.by.race[r1,r2]
  }

  rv$msm.sex.by.race.oe = list(calculate.oe.ratios(msm.sex.by.race.1),
                               calculate.oe.ratios(msm.sex.by.race.2),
                               calculate.oe.ratios(msm.sex.by.race.3),
                               calculate.oe.ratios(msm.sex.by.race.4))

  # TODD: how did you use these 4 matrices to deriver the final values?
  # Convert the list to an array
  # array_matrices <- simplify2array(rv$msm.sex.by.race.oe)
  #
  # # Calculate the mean across all matrices (along the third dimension)
  # mean_matrix <- apply(array_matrices, c(1, 2), mean)
  # mean_matrix

  #-- Young Heterosexuals - Hamilton 2015 --# ----
  # https://www.sciencedirect.com/science/article/pii/S1755436515000080?via%3Dihub
  het.sex.by.race.1 = array(0, dim=c(race.from=3, race.to=3),
                            dimnames=list(race.from=c('black','hispanic','other'), race.to=c('black','hispanic','other')))
  het.sex.by.race.1['black','black'] = 1078 * (.89 + .74) / 2
  het.sex.by.race.1['hispanic','black'] = 1078 * (.08 + .14) / 2
  het.sex.by.race.1['other','black'] = 1078 * (.03 + .12) / 2
  het.sex.by.race.1['black','hispanic'] = 745 * (.10 + .03) / 2
  het.sex.by.race.1['hispanic','hispanic'] = 745 * (.56 + .64) / 2
  het.sex.by.race.1['other','hispanic'] = 745 * (.34 + .33) / 2
  het.sex.by.race.1['black','other'] = 3656 * (.05 + .02) / 2
  het.sex.by.race.1['hispanic','other'] = 3656 * (.16 + .17) / 2
  het.sex.by.race.1['other','other'] = 3656 * (.79 + .82) / 2
  rv$het.sex.by.race.oe = list(calculate.oe.ratios(het.sex.by.race.1))


  #---- SEXUAL PAIRINGS by AGE --## ----
  #From Chow 2016
  #http://www.publish.csiro.au/sh/Fulltext/SH16055
  raw.heterosexual.by.age = read.csv(paste0(dir, '/heterosexual_age.csv'))
  raw.msm.by.age = read.csv(paste0(dir, '/msm_age.csv'))

  fit.female.with.male = fit.age.model(raw.heterosexual.by.age[,1], raw.heterosexual.by.age[,2])
  fit.male.with.female = fit.age.model(raw.heterosexual.by.age[,2], raw.heterosexual.by.age[,1])
  fit.msm = fit.age.model(raw.msm.by.age[,1], raw.msm.by.age[,2])

  rv$sex.age.models = list(heterosexual_male = fit.male.with.female,
                           female = fit.female.with.male,
                           msm = fit.msm)

  # test MSM from Chow against Grey
  #    simmed.msm = simulate.from.fitted.age.model(rv$fit.msm.sex.by.age,
  #                                                ages=18:29,
  #                                                age.counts=rep(1, 29-18+1),
  #                                                cutoffs=c(18,20,25,30))

  #    diffs = simmed.msm$ages.from - simmed.msm$ages.to
  #    quantile(diffs[simmed.msm$ages.to>=18 & simmed.msm$ages.to<20], probs=c(.25,.5,.75))
  #    quantile(diffs[simmed.msm$ages.to>=20 & simmed.msm$ages.to<25], probs=c(.25,.5,.75))
  #    quantile(diffs[simmed.msm$ages.to>=25 & simmed.msm$ages.to<30], probs=c(.25,.5,.75))


  ##-- RETURN IT --##
  rv
}


##-- HELPER FUNCTIONS --## ----
# get.age.mixing.proportions ----
#' @title get.age.mixing.proportions
#' @description takes the model and calculates mixing proportions
#' Delta(ages)= N(mu=B0+B1 a , sd=L0+L1 a) where a is the age of person the contact is made to
#' @param age.delta.intercept.mean B0
#' @param age.delta.slope.mean B1
#' @param age.delta.intercept.sd L0
#' @param age.delta.slope.sd L1
#' @param age.cutoffs age brackets, the end can't be infinity (e.g., c(0,15,20,25,30,35,40,45,50,55,65,85))
#' @param age.labels how do you want to label the age brackets "lower_upper years"
#' @param single.year.age.counts the count of people in each year of age
#' @param sd.multiplier calibration parameter used to scale sd
#' @return n by n matrix of contact proportions (age.to columns summing to 1) - all of mixing matrixes care about who is RECEIVING the contact
#' @examples
# x= get.specification.metadata('shield','US')
# x$dim.names$age
get.age.mixing.proportions <- function(age.delta.intercept.mean,
                                       age.delta.slope.mean,
                                       age.delta.intercept.sd,
                                       age.delta.slope.sd,
                                       age.cutoffs,
                                       age.labels,
                                       single.year.age.counts,
                                       sd.multiplier)
{
  if (any(is.infinite(age.cutoffs)))
    stop("age.cutoffs cannot be infinite")

  n.age.brackets = length(age.cutoffs)-1
  age.lowers = age.cutoffs[-length(age.cutoffs)]
  age.uppers = age.cutoffs[-1]

  last.age.in.bracket = age.uppers
  last.age.in.bracket[-n.age.brackets] = last.age.in.bracket[-n.age.brackets]-1

  min.age = age.cutoffs[1]
  max.age = age.cutoffs[length(age.cutoffs)]

  rv = sapply(1:n.age.brackets, function(age.to){

    ages.in.bracket = age.lowers[age.to]:last.age.in.bracket[age.to]
    age.proportions.within.bracket = single.year.age.counts[as.character(ages.in.bracket)] /
      sum(single.year.age.counts[as.character(ages.in.bracket)])
    ages.in.bracket = ages.in.bracket + 0.5 #make the midpoint of the year the age for each

    age.means = ages.in.bracket + age.delta.intercept.mean + age.delta.slope.mean*ages.in.bracket
    age.sds = (age.delta.intercept.sd + age.delta.slope.sd*ages.in.bracket) * sd.multiplier

    sapply(1:n.age.brackets, function(age.from){
      sum(age.proportions.within.bracket *
            (pnorm(age.uppers[age.from], mean = age.means, sd = age.sds) -
               pnorm(age.lowers[age.from], mean = age.means, sd = age.sds)) /
            (pnorm(max.age, mean = age.means, sd = age.sds) -
               pnorm(min.age, mean = age.means, sd = age.sds))
      )
    })
  })

  dim.names = list(age.from = age.labels, age.to = age.labels)
  dim(rv) = sapply(dim.names, length)
  dimnames(rv) = dim.names

  rv
}

# calculate.oe.ratios -----
#' @title calculate.oe.ratios
#' @description generates the oe ratios from observed pairing counts of partnerships
#' @param pairing.counts observed pairing counts (n by n matrix of reported partnerships between each group)
#' @return A n by n matrix of oes
#' @examples
#' pairing.counts = matrix(c(72, 33, 176, 56, 130, 562, 120, 301, 2023), nrow=3, byrow=TRUE,dimnames = list(from=c("black", "hispanic", "other"), to=c("black", "hispanic", "other")))
#' calculate.oe.ratios(pairing.counts) returns the observed/expected ratio of partnerships between each pair
calculate.oe.ratios <- function(pairing.counts)
{
  marginals = rowSums(pairing.counts) / sum(pairing.counts) #marginal distribution of the contacts from each group
  col.counts = colSums(pairing.counts) #total number of available contacts by race
  expected = outer(marginals, col.counts) # if there is no preferetial mixing, this is the expected number of contacts between each pair
  pairing.counts / expected #observed/expected ratio of contacts between each pair
}

# get.pairing.proportions ----
#' @title get.pairing.proportions
#' @description using the oes and marginal.counts (population size), it computes the proportion of contacts
#' @param oe.ratios observed/expected ratios
#' @param marginal.counts popualtion count in each group
#' @return n by n matrix where columns (to) sum to 1
#' @examples
#' pairing.counts = matrix(c(72, 33, 176, 56, 130, 562, 120, 301, 2023), nrow=3, byrow=TRUE,dimnames = list(from=c("black", "hispanic", "other"), to=c("black", "hispanic", "other")))
#' oes = calculate.oe.ratios(pairing.counts) returns the observed/expected ratio of partnerships between each pair
#' marginal.counts = c(100, 200, 300)
#' get.pairing.proportions(oes, marginal.counts) returns the proportion of contacts between each pair
get.pairing.proportions <- function(oe.ratios, marginal.counts)
{
  if (dim(oe.ratios)[1] != dim(oe.ratios)[2])
    stop("The 'oe.ratios' argument must be a square matrix")
  if (dim(oe.ratios)[1] != length(marginal.counts))
    stop("The length of 'marginal.counts' must be the same as the dimension of 'oe.ratios")

  marginals = marginal.counts / sum(marginal.counts)
  rv = oe.ratios * marginals
  rv = rv / rep(colSums(rv), each=length(marginal.counts))

  rv
}


# matrix.to.scatter ----
#' @title matrix.to.scatter
#' @description Used in fitting the data (to fit 2 B's and L's) to data on ages of people and their partners-the papers only published aggregated data in the matrix forma and we had to decompose this back to individual data to fit the model. this function randomly samples ages from that age range and construct a list
#' @param mat summary matrix of contacts
#' @param age.cutoffs age cut offs to sample from
#' @return list of ages for contact pairs
#' @examples
#' n partnerships are reported between a person of age=[30-40] with a person of age=[60-70] (age1-age2). we sample n values from each age bracket and report the pairs
matrix.to.scatter <- function(mat,
                              age.cutoffs)
{
  rv = list(age.from=numeric(), age.to=numeric())
  for (i.from in 1:dim(mat)[1])
  {
    for (j.to in 1:dim(mat)[2])
    {
      n = mat[i.from,j.to]
      rv$age.from = c(rv$age.from, runif(n, age.cutoffs[i.from], age.cutoffs[i.from+1]))
      #            rv$age.from = c(rv$age.from, rep((age.cutoffs[i.from]+age.cutoffs[i.from+1])/2, n))
      rv$age.to = c(rv$age.to, runif(n, age.cutoffs[j.to], age.cutoffs[j.to+1]))
      #            rv$age.to = c(rv$age.from, rep((age.cutoffs[j.to]+age.cutoffs[j.to+1])/2, n))
    }
  }

  rv
}

# fit.age.model ----
#' @title fit.age.model
#' @description fits the age model to list of data points (Delta(ages)= N(mu=B0+B1 a , sd=L0+L1 a) where a is the age of person the contact is made to)
#' @param age.of.reference age1
#' @param age.of.partner age2
#' @return returning the B0, B1, L0, L1 values
fit.age.model <- function(age.of.reference, age.of.partner)
{
  diffs = age.of.partner - age.of.reference
  fit.for.mean = lm(diffs~age.of.reference)

  n.quantiles = 8
  cutoffs = quantile(age.of.reference, probs = seq(from=0,to=1,length=n.quantiles+1))
  midpoints = cutoffs[-1][-n.quantiles]
  cutoffs[n.quantiles] = Inf
  sds.for.fit = sapply(1:(n.quantiles-1), function(i){
    sd(diffs[age.of.reference>=cutoffs[i] & age.of.reference<cutoffs[i+2]])
  })
  fit.for.sd = lm(sds.for.fit~midpoints)

  rv=c(mean.intercept=as.numeric(fit.for.mean$coefficients[1]),
       mean.slope=as.numeric(fit.for.mean$coefficients[2]),
       sd.intercept=as.numeric(fit.for.sd$coefficients[1]),
       sd.slope=as.numeric(fit.for.sd$coefficients[2]))

  rv
}


