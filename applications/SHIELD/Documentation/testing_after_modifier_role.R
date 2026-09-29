# Testing after.modifier behavior in natural spline functional forms
# This code tests how create.natural.spline.functional.form() handles the after.modifier parameter when alpha values are supplied, with particular attention to the overwrite.modifiers.with.alphas option.
# Two otherwise identical functional forms are created:
#     
#       ff1 uses overwrite.modifiers.with.alphas = FALSE, so alpha values are added to the existing modifier.
#       ff2 uses overwrite.modifiers.with.alphas = TRUE, so the alpha value overwrites the existing modifier.
#
# Both functional forms specify an after.modifier of 0.5 beginning after 2030, with the modifier applied to changes in the spline. The code also verifies that the modifier is stored with an identity link.
# The helper function make.alphas() creates the alpha objects required by project() and assigns values to selected knots (2010 and 2022) and to after.modifier.

library(jheem2)
ff1 <- create.natural.spline.functional.form(
    knot.times  = c("1970"=1970, "1990"=1990, "1995"=1995, "2000"=2000, "2010"=2010, "2022"=2022),
    knot.values = list("1970"=0, "1990"=0, "1995"=0, "2000"=0, "2010"=0, "2022"=0),
    knots.are.on.transformed.scale = TRUE, knot.link = "log", link = "identity",
    after.time = 2030, after.modifier = 0.5,
    after.modifier.increasing.change.link = "identity",
    after.modifier.decreasing.change.link = "log",
    overwrite.modifiers.with.alphas = F,
    modifiers.apply.to.change = TRUE, min = 0
    )
ff1$betas$after.modifier                    # expect 0.5
ff1$alphas.are.additive["after.modifier"]   # TRUE = added (bug), FALSE = overwritten
ff1$alpha.links$after.modifier$type         # expect "identity"

ff2 <- create.natural.spline.functional.form(
    knot.times  = c("1970"=1970, "1990"=1990, "1995"=1995, "2000"=2000, "2010"=2010, "2022"=2022),
    knot.values = list("1970"=0, "1990"=0, "1995"=0, "2000"=0, "2010"=0, "2022"=0),
    knots.are.on.transformed.scale = TRUE, knot.link = "log", link = "identity",
    after.time = 2030, after.modifier = 0.5,
    after.modifier.increasing.change.link = "identity",
    after.modifier.decreasing.change.link = "log",
    overwrite.modifiers.with.alphas = T,
    modifiers.apply.to.change = TRUE, min = 0
    )
ff2$betas$after.modifier                    # expect 0.5
ff2$alphas.are.additive["after.modifier"]   # TRUE = added (bug), FALSE = overwritten
ff2$alpha.links$after.modifier$type         # expect "identity"


# alpha objects for every alpha of ff; values only where supplied
make.alphas <- function(ff, values) {
    al <- lapply(ff$alpha.names, function(nm) {
        a <- jheem2:::create.functional.form.alphas(ff, nm, maximum.dim.names = list())
        if (nm %in% names(values))
            a <- jheem2:::set.alpha.main.effect.values(a, dimension = "all",
                                                       dimension.values = "all",
                                                       values = values[[nm]])
        a
    })
    names(al) <- ff$alpha.names
    al
}

a1 <- make.alphas(ff1, list(after.modifier = 0.75, `2010` = 1.2, `2022` = 2.4))
a2 <- make.alphas(ff2, list(after.modifier = 0.75, `2010` = 1.2, `2022` = 2.4))
unlist(ff1$project(years = c(2022, 2030), alphas = a1))# added (as coded):  2022 = 2.40, 2030 = 2.4 + 1.25 * 1.2 = 3.90
unlist(ff2$project(years = c(2022, 2030), alphas = a2))# overwritten:       2022 = 2.40, 2030 = 2.4 + 0.75 * 1.2 = 3.30



