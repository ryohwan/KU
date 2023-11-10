market2 <- read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/market2.txt", header = TRUE)
head(market2)

X = market2[, c(2:3)]
X = cbind(1, X)
Y = market2[, 4]
X = as.matrix(X)
Y = as.matrix(Y)

# Beta = (X'X)^-1 X'Y
# XTX = X'X
# XTY = X'Y
# XTXI = (X'X)^-1
# Beta = XTXI XTY
# XTXI = solve(XTX)
# t(X) : X의 전치행렬
# t(X) %*% X : X의 전치행렬과 X의 행렬곱
# solve(XTX) : XTX의 역행렬
# t(X) %*% Y : X의 전치행렬과 Y의 행렬곱
XTX = t(X) %*% X
XTX
XTXI = solve(XTX)
XTY = t(X) %*% Y
XTY
beta = XTXI %*% XTY
beta = round(beta, 3)
beta

# Multiple Linear Regression
head(market2, 2)
market2.lm = lm(total_sales ~ advertise + store_size, data = market2)
summary(market2.lm)

# 변동의 분해(분산분석표) : anova
anova(market2.lm)

# 중상관계수
names(market2.lm)
yhat = market2.lm$fitted
cor(market2$total_sales, yhat)
cor(market2$total_sales, yhat)^2


# 2. Multiple Linear Regression
install.packages("readxl")
library(readxl)
chemical = read_excel("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/chemical.xlsx")
View(chemical)

