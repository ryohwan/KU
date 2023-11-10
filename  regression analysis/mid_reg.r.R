# 연습문제 1-7번
cost_y = read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/main_cost.txt", header = T)
head(cost_y)

# Path:  regression analysis/mid_reg.r.R
cost_y.lm = lm(cost ~ year, data = cost_y)
summary(cost_y.lm)
anova(cost_y.lm)

# cost = 29.107 + 13.637 * year
# R-squared:  0.9838
# F = 18.753, p-value = 0.0009779

library(ggplot2)
ggplot(cost_y, aes(year, cost)) + geom_point() + geom_smooth(method = "lm")

names(cost_y.lm)
cost_y.lm$residuals
sum(cost_y.lm$residuals)

cbind(cost_y, cost_y.lm$residuals, cost_y.lm$fitted)
plot(cost_y$cost, cost_y.lm$resid, pch=19)
abline(h=0, col="red", lwd=2, lty=2)
plot(cost_y.lm$fitted, cost_y.lm$resid, pch=19)
abline(h=0, col="red", lwd=2, lty=2)

# 추정값의 신뢰대 그리기
# 추정값의 신뢰대를 그리기 위해서는 predict() 함수를 사용한다.
p.x <- data.frame(cost=c(1:45))
pc <- predict(cost_y.lm, int="c", newdata=p.x)
pred.x <- p.x$cost
plot(cost_y$cost, cost_y.lm$year, ylim = range(cost_y.lm$year, pc), pch=19)
matlines(pred.x, pc, lty=c(1, 2, 2), col="BLUE")



#################
# 중회귀 모형
################
install.package("lm.beta")
library(lm.beta)
market2.lm = lm(Y ~ X1+X2,  data = market2)

install.packages("xlsx")


#############
# 2.3 분석 사례
#############
temperature = read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/temper.txt", header = T)
head(temperature)

install.packages("xlsx")
library(xlsx)

# 1. 데이터 불러오기
temperature = read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/temper.txt", header = T)
head(temperature)

# 2. 기술통계량 및 상관계수 보기
summary(temperature)
cor(temperature)

# 3. 회귀분석 적합하기
temperature.lm = lm(y ~ x1 + x2 + x3, data = temperature)
summary(temperature.lm)

library(car)
avPlot(temperature.lm)

# 4. 회귀분석 결과 해석하기
# 4-1. 분산분석표
anova(temperature.lm)

# 4-2. 잔차산점도 그리기 : y(물소비량 / 단위 1,000톤)
# x1(평균온도)과 y의 관계를 보기 위해 x1을 x축으로, 잔차를 y축으로 그린다.
plot(temperature$x1, temperature.lm$residuals, pch=19)
identify(temperature$x1, temperature.lm$residuals)
abline(h=0, col="red", lwd=2, lty=2)

# x2(작업일수 / 일)와 y의 관계를 보기 위해 x2를 x축으로, 잔차를 y축으로 그린다.
plot(temperature$x2, temperature.lm$residuals, pch=19)
identify(temperature$x2, temperature.lm$residuals, labels=temperature$y)
abline(h=0, col="red", lwd=2, lty=2)

# x3(작업량/단위 1,000톤)과 y의 관계를 보기 위해 x3를 x축으로, 잔차를 y축으로 그린다.
plot(temperature$x3, temperature.lm$residuals, pch=19)
identify(temperature$x3, temperature.lm$residuals)
abline(h=0, col="red", lwd=2, lty=2)




