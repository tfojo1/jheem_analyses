# Documentation Guidelines

An easy way for us all to get started documenting our code is to put the following annotations on your existing and newly created helper functions.
There's no need to actually render these annotations into R Markdown files or generate published documentation.
Just putting these annotations in our code will pay dividends immediately and make loftier documentation goals easier to implement down the road.

Annotations are special comments that can be recognized by the Roxygen2 package and always start with `#'`.

It might look like this:

```
#'@title Add Two Numbers
#'@param x,y Numeric values
add <- function(x, y)
{
    # I'm a regular comment, not an annotation
    x + y
}

```

See below for a more substantial example.


## Aim to have all of these:

### `@title`
Should be self-explanatory. You can also put a title without explicitly writing `@title` if it's the topmost annotation and is only one line long.

### `@description`
Describe what this function is for so that someone else could decide if they want to use it or could figure out why someone else has. Sometimes the purpose of a function is so clear that the title explains enough.

### `@param`
Every argument should have a short description and a clear statement of what type of object it requires. For example, can the input be NULL, or should it be a list of 'jheem.likelihood' objects? The syntax for this annotation is `@param` followed by your parameter name followed by your text to document it.
Putting an `@param` after every single parameter of every single helper function you write can quickly become overwhelming. Save yourself time and follow good coding practices ("do not repeat yourself") by inheriting documentation from other functions you've documented, with `@inheritParams`. This is described below.

### `@return`
One of the hardest things to do is to look at a function and predict what will come out of it. Help everyone out by describing the output of your function, like whether it is a 2x2 matrix with dimensions 'year' and 'age', a simple TRUE or FALSE, or something else.


## Exploit these to your advantage:

### `@inheritParams`
Don't document the same parameter a dozen times if you can just do it once! Identify which functions seem like special cases of others or that draw heavily on another function as a pre-cursor in some fashion. If they've got any arguments in common, let Roxygen2 do the hard work and just write `@inheritParams that_other_function` instead of defining them again. You'll still need to document remaining parameters with `@param`. If you've inherited a parameter definition but you actually want its definition to be slightly different here, you can override it with another `@param`. Down the road when we use Roxygen2 to make R Markdown files out of these annotations, Roxygen2 will be smart enough to track down all the parameter definitions you've inherited and copy them to all the functions that use them. Since another goal of documentation is to help understand functions when browsing the codebase, make sure you inherit parameters from a function that someone browsing the code can actually find. It doesn't help much to inherit all your parameters from some function with little apparent relevance that just happens to have some arguments in common. Use `@inheritParams` carefully but absolutely use it!

### `@param arg1,arg2`
There are a lot of parameters we make that come in pairs, like `min.age`/`max.age`, or `from.year`/`to.year`. You can document these arguments together in one line by listing both arguments after the `@param` annotation, separated with commas. This not only saves space but is very intuitive way to readers.

### `@rdname`
This is a more dramatic version of `@inheritParams` that lets you reuse *all* the documentation of one function for others. This is great if you've got multiple functions that are so similar to one another that it makes sense to describe them all at once. Pick what seems to be the central or "parent" function and do all your normal documentation there. Then just annotate the other functions with `@rdname name_of_the_parent_function` and leave it at that. It's okay if the parent function has more arguments than some of its children -- this is common when the children are just special cases of the parent, but it won't work if a child function has additional arguments.


## How it might look

Here's an example of a few functions being annotated. These functions are drawn from JHEEM2 but a lot of their arguments have been cut out to keep the example concise. Ignore the indentation you see -- copying R code into Markdown to make these guidelines didn't preserve it all.
Notice the use of `@inheritParams` to avoid documenting most of the arguments for the parent function ("create.basic.likelihood.instructions") again in the child function ("create.time.lagged.comparison.likelihood.instructions").

```
#'@title Create JHEEM Basic Likelihood Instructions
#'@description Creates instructions for a JHEEM basic likelihood
#'@param outcome.for.data,outcome.for.sim Single character vectors specifying outcomes to use to find data/pull from a simulation
#'@param dimensions A character vector of dimensions, excluding year, from which stratifications will be generated.
#'@param levels.of.stratification An integer vector specifying how the dimensions should be combined to form strata. '0' indicates totals (not stratified) while '1' indicates strata that are each stratified by one dimension at a time, '2' indicates strat that are each stratified by a combination of two dimensions at a time, etc. May not exceed the number of dimensions. Defaults to NULL, which is equivalent to '0'.
#'@param from.year,to.year Single integer values specifying the earliest and latest years for which data should be pulled. Defaults to -Inf and Inf.
#'@param weights A list containing only numeric vectors and objects of class 'jheem.likelihood.weights', which are generated by \code{\link{create.likelihood.weights}}
#'@return A JHEEM Basic Likelihood Instructions object
create.basic.likelihood.instructions <- function(outcome.for.data,
outcome.for.sim,
dimensions = character(0),
levels.of.stratification = NULL,
from.year = -Inf,
to.year = Inf,
weights = list()) {
...
}

#' Create Time-Lagged Comparison JHEEM Basic Likelihood Instructions
#'@description Creates instructions for a JHEEM basic likelihood for calibrating to the year-on-year change in an outcome's value.
#'@inheritParams create.basic.likelihood.instructions
#'@param use.lognormal.approximation Should a lognormal approximation be used when the likelihood is computed. Defaults to \code{TRUE}.
#'@return A JHEEM Basic Likelihood Instructions object
create.time.lagged.comparison.likelihood.instructions <- function(outcome.for.data,
outcome.for.sim,
dimensions = character(0),
levels.of.stratification = NULL,
from.year = -Inf,
to.year = Inf,
weights = list(),
use.lognormal.approximation = T) {
...
}
```


## Things not to stress about
At the Computational Epi Forum, we recommended keeping your helper functions separate from files where you just call these functions to do your analysis. This may be needed later on, depending on how we decide to generate documentation out of the annotations, but for now you can skip this step unless it's convenient and helps you stay organized. We'd love for everyone to get a start on using Roxygen2 annotations without stressing about re-structuring your projects.


## Further reading
To learn more about how to use Roxygen2 annotations, check out https://roxygen2.r-lib.org/articles/roxygen2.html. I'll (Andrew) be maintaining this document and serve as the contact point for related questions. I have a lot to learn about this tool and documentation best practices and would love to hear your thoughts. If you're curious about following a consistent style in your code, consider reading Hadley Wickham's manual on style here: https://style.tidyverse.org/documentation.html. He's the author of ggplot2 and other tidyverse packages, so he knows what he's doing!

Updated 9/17/2024