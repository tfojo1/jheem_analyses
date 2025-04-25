
library(ggplot2)

#file:///C:/Users/tfojo1/Downloads/cdc_156513_DS1.pdf
# from table 8, page 78-79
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
    c(199978, 0.7),
    
    c(1003086, 0.3),
    
    c(789911, 0.4),
    c(235215, 0.6),
    c(25982, 1.4),
    c(156565, 0.6),
    c(192065, 0.5),
    c(265833, 0.4),
    c(270052, 0.4),
    c(114629, 0.9),
    c(2870, 5.9),
    c(14905, 2.4),
    c(409953, 0.5),
    c(240725, 0.6),
    c(781, 11.1),
    c(296737, 0.6),
    c(58523, 1.2),
    c(584334, 0.4),
    c(114649, 1.1),
    c(65959, 1.5),
    c(48690, 1.6),
    c(58752, 1.4),
    c(263773, 0.6),
    c(78811, 1.2),
    c(184962, 0.7),
    c(229466, 0.7),
    c(122601, 0.9),
    c(467870, 0.5),
    c(205189, 0.7),
    
    c(1025126, 0.3)
)

df = as.data.frame(df)
names(df) = c('n','rse')
df$se = df$rse/100 * df$n


exp.of.variance = lm(log(se) ~ log(n) + 0, data=df)$coefficients
df$calc.se = df$n^exp.of.variance

ggplot(df, aes(n, se)) + geom_point()
ggplot(df, aes(log(n), log(se))) + geom_point()
ggplot(df, aes(se, calc.se)) + geom_point()

print(paste0("Exponent of variance from regression with no CV = ", exp.of.variance))

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


optim.result = optim(
  par = c(0.05, 1),
  fn = function(par){
    
      projected.se = sqrt(par[1]^2 * df$n^2 + df$n^(2*par[2]))
    
      sse = sum( (df$se - projected.se)^2 )
  }
  lower = c(0,0),
  upper = c(1,5),
  method = 'L-BFGS-B'
)
