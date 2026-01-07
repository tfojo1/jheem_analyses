# taking out county-level population data from census manager
# finding a way to subset the datamanagers
# should we keep both managers or combine them?
#   
  # Fertility rates MSA




## Notes: ----


# interventions will be defined by parameters too (outside of specification)
# Element: a scalar value or a functional form
## Scalar: constant value or fix over over time
## Functional form: either vary by strata or changes over time (an equation that will be evaluated to create value of that parameter )
# QUANTITIES can be equal to another quantity or an element, or an expression (accepts elements) or a function of other quantities
# a quantity has to have a scale if we want to intervene in them


# We generally use additional parameters to model deviations from our prior knowledge and calibrate them to data
# Parameters affect elements, quantities are calculated from elements




# linear line on the log scale will be exponetial after transportation
# so in general we only trnasform knots in the log scale
# when we sample those parameters )sample alpha a change), we will multiple in the knot validate.sub.version.code(and since knows tare in the log scale I can add them in thelog scale
#                                                                                                                                                                                                                                 )


# There are two ways to capture outputs: via 1) compartments (e.g., population size of a compartment), or 2) transitions (e/g., number of people moving from one compartment to another)
#1::
## track.point.outcome(): a static outcome at a certain point in time 
### track.integrated.outcome(): integrates a point outcome over time
# cumulative vs point: any diagnosis that were made during a year or n at. a single time in that year
# infected + uninfected: these are reported at a single point in time Jan 1st of each year
# assuming that census take place at a random time during hte year, we use the integral under population curve could be used for calibration
# average of 2 points
# outcomes: 1.point (like infected unifected), 2. some event count (incidence)

# 2::  
## track.transition(): a dynamic outcome that starts and end from compartments (it has both a from- and to-)
## track.dynamic.outcome(): doesnt have a from or to compartment (e.g., count people as they enter the model)
### track.cumulative.outcome(): can add multiple dynamic outcomes together  
