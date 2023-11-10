# 다항회귀모형(d-th order polynomial regression model)
# year(연도)    tcratio(교통범죄발생률)    motor(차량보급률)
trime <- read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/tcrime.txt", header = T)
head(trime, 3)
attach(trime)
plot(motor, tcratio, xlab = "motor", ylab = "crime", pch = 16, cex = 1.2)

trime.lm <- lm(tcratio ~ motor + I(motor^2), data = trime)
summary(trime.lm)


# 가변수회귀모형(dummy variable regression model)
# Faraway, JJ(2002), Practical Regression and Anova using R, Wiley,
# 2.3.2. Dummy variables
# faraway 패키지의 fruitfly 데이터를 이용하여 가변수회귀모형(dummy variable regression model)을 적합하고 해석하시오.
# 1) fruitfly 데이터를 불러오고, 데이터의 구조를 파악하시오.
# 2) fruitfly 데이터에서 1, 26, 51, 75, 101번째 행을 추출하시오.
# 3) fruitfly 데이터에서 1, 26, 51, 75, 101번째 행을 제외하고 추출하시오.
# 4) fruitfly 데이터에서 1, 26, 51, 75, 101번째 행을 제외하고 추출하고,
#    1, 26, 51, 75, 101번째 행을 제외한 데이터의 구조를 파악하시오.
# 1. 데이터 불러오기
install.packages("faraway")
library(faraway)
data(fruitfly)
head(fruitfly, 3)
fruitfly[c(1, 26, 51, 75, 101), ]

attach(fruitfly)
plot(fruitfly$thorax, fruitfly$longevity, type = "n", xlab = "thorax", ylab = "longley")
points(thorax[activity=="many"], longevity[activity=="many"], col = 1, pch = "m")
points(thorax[activity=="isolated"], longevity[activity=="isolate"], col = 2, pch = "i")
points(thorax[activity=="one"], longevity[activity=="one"], col = 3, pch = "o")
points(thorax[activity=="low"], longevity[activity=="low"], col = 4, pch = "w")
points(thorax[activity=="high"], longevity[activity=="high"], col = 5, pch = "h")

g = lm(longevity ~ thorax * activity, data = fruitfly)
summary(g)


