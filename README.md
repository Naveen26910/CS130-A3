# CS130-A3
install.packages("Matching")
library(Matching)
MatchBalance(D~V2+V3,data = df)

#calculating prima facie
p_f <- mean(df$Y[D == 1]) - mean(df$Y[D == 0])
p_f

#used code from forum workbook from Session 18 
#propensity score model
library(Matching)
prop_1 <- glm(data = df, formula = D ~ V2 + V3, family = "binomial")
ps <- predict(prop_1, type = "response")
matchout1 = Match(Y = df$Y, Tr = df$D, X = ps, M = 1)
mbout1 = MatchBalance(data = df, formul = D ~ V2 + V3, match.out = matchout1, nboots = 1000)

matchout1$est

#using sources shared in session 19
#https://nbviewer.org/gist/viniciusmss/a156c3f22081fb5c690cdd58658f61fa
#using rbounds library
library(rbounds)
psens(matchout1, Gamma=19, GammaInc = .1)

#using sources shared in session 19
#https://nbviewer.org/gist/viniciusmss/a156c3f22081fb5c690cdd58658f61fa
install.packages(“rgenoud”)
library(rgenoud)
X <- cbind(df$V2, df$V3) 
Y <- df$Y
Tr <- df$D
genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, pop.size=10, max.generations = 10, wait.generations = 2)
mout <- Match(Tr=Tr, X=X, Y=Y,M=1, estimand="ATT", Weight.matrix = genout)
mb <- MatchBalance(data = df, formul = D ~ V2 + V3, match.out = genout, nboots = 1000)

#using previous code
genout.caliper <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, pop.size=10, max.generations = 10, wait.generations = 2, caliper = c(1e-2, 1e5))
mout.caliper <- Match(Tr=Tr, X=X, Y=Y,M=1, estimand="ATT", Weight.matrix = genout.caliper, caliper = c(1e-2, 1e5))
summary(mout.caliper)

mout$est

library(rbounds)
psens(mout, Gamma = 20, GammaInc = .1)
