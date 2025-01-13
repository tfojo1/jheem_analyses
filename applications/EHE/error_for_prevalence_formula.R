
#file:///C:/Users/tfojo1/Downloads/cdc_156513_DS1.pdf
# from table 8, page 78
df = rbind(
    c(771468, .4),
    c(231618, .6),
    c(26640, 1.2),
    c(153098, 0.5),
    c(188697, .5),
    c(278764, .4),
    c(254947, .4),
    c(100940, .9),
    c(2746, 5.8),
    c(14181, 2.4),
    c(401158, 0.5),
    c(232610, 0.6),
    c(737, 10.9),
    c(292944, .6),
    c(58074, 1.2),
    c(566603, 0.4),
    c(115357, 1.1),
    c(66578, 1.5),
    c(48778, 1.5),
    c(259019, 0.6),
    c(77713, 1.1),
    c(181306, 0.7),
    c(227985, 0.7),
    c(119895, 0.9),
    c(455228, 0.4),
    c(199978, 0.7)
)

df = as.data.frame(df)
names(df) = c('n','rse')
df$se = df$rse/100 * df$n


exp.of.variance = lm(log(se) ~ log(n) + 0, data=df)$coefficients
df$calc.se = df$n^exp.of.variance

ggplot(df, aes(n, se)) + geom_point()
ggplot(df, aes(log(n), log(se))) + geom_point()
ggplot(df, aes(se, calc.se)) + geom_point()


coefs = lm(log(se) ~ log(n), data=df)$coefficients

calc.se = exp(coefs[1]) * df$n^coefs[2]

qplot(df$se, calc.se) + geom_abline(intercept = 0, slope=1)

qplot(df$se, sqrt(df$n)) + geom_abline(intercept = 0, slope=1)

qplot(df$se, df$n^.58378) + geom_abline(intercept = 0, slope=1)

lm(log(se) ~ log(n) + 0, data=df)$coefficients

library(ggplot2)
qplot(df[,1], df[,2])
qplot(log(df[,1]), log(df[,2]))

lm(log(df[,2]) ~ log(df[,1]))


qplot(sqrt(df[,1]), df[,2])


qplot(exp(5.6612 - 0.5071 * df[,1]), exp(df[,2]))

