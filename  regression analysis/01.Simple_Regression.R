library(ggplot2)
x <- c(4.2, 8.5, 9.3, 7.5 ,6.3, 12.2)
y <- c(9.3, 18.5, 22.8, 17.7, 14.6, 27.9)
market <- data.frame(x, y)
market

# 단순회귀모형
# Y = B0 + B1*X + E
# Y: 매출액, X: 광고료, E: 오차항
# B0: 절편, B1: 기울기
# B0 = 3.5, B1 = 2.5
# Y = 3.5 + 2.5*X + E
market.lm <- lm(y ~ x, data = market)
summary(market.lm)

# 산점도 그리기
plot(market$x, market$y, xlab = "광고료", ylab = "매출액", pch = 19)
title("광고료와 판매액의 산점도")
abline(market.lm, col = "red", lwd = 2, lty = 2)
identify(market$x, market$y)

# 잔차 확인하기
names(market.lm)
resid <- market.lm$residuals
fited <- market.lm$fitted
sum(resid)
sum(fited)
sum(market$y)
xbar <- mean(market$x)
ybar <- mean(market$y)
xbar
ybar
points(xbar, ybar, pch = 17, col = "red")
text(xbar, ybar, "(8, 18.47)", col = "red")
fx <- "Y-hat = -0.2761 + 2.3428x"
text(locator(1), fx)


# 분산분석표
# 회귀모형이 얼마나 유효한지 확인하는 방법
# F통계량이 크면 회귀모형이 유효하다고 할 수 있다.
# F통계량이 작으면 회귀모형이 유효하지 않다고 할 수 있다.
anova(market.lm)

# 유의수준 0.05에서 회귀모형이 유효하다고 할 수 있다.
# 회귀모형이 유효하다는 것은 회귀모형이 데이터를 잘 설명한다는 것을 의미한다.
qf(0.95, 1, 4)

# p-value 구하기
# p-value가 0.05보다 작으면 회귀모형이 유효하다고 할 수 있다.
1 - pf(242.42, 1, 4)

# ggplot2를 이용한 산점도 그리기
ggplot(data = market, mapping = aes(x = x, y = y)) +
  geom_point() +
  labs(title = "광고료와 판매액의 산점도", x = "광고료", y = "매출액", color = "red") +
  theme(plot.title = element_text(hjust=0.5)) +
  # geom_line(date = market.lm, aes(x, y), color = "red", size = 1.0, linetype = "dashed")
  geom_abline(intercept = -0.2761, slope = 2.3428, color = "red", size = 1.0, linetype = "dashed")

##################################################
# 단순회귀의 추정과 검정
##################################################
summary(market.lm)
# all:
#   lm(formula = y ~ x, data = market)
#
# Residuals:
#   1       2       3       4       5       6
# -0.2639 -1.1381  1.2876  0.4048  0.1162 -0.4066
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -0.2761     1.2607  -0.219    0.837
# x             2.3428     0.1505  15.570 9.93e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.9173 on 4 degrees of freedom
# Multiple R-squared:  0.9838,	Adjusted R-squared:  0.9797
# F-statistic: 242.4 on 1 and 4 DF,  p-value: 9.935e-05
# B1의 95% 신뢰구간
# B1의 추정치가 2.3428이고, 표준오차가 0.1505이다.
# B1의 95% 신뢰구간(1.923945, 2.761655)
q.val <- qt(0.975, 4)
2.3428 + q.val * 0.1505
2.3428 - q.val * 0.1505


# B0의 95% 신뢰구간
# B0의 추정치가 -0.2761이고, 표준오차가 1.2607이다.
# B0의 95% 신뢰구간은 (-3.776364, 3.224164)이다.
q.val <- qt(0.975, 4)
-0.2761 - q.val * 1.2607
-0.2761 + q.val * 1.2607

# 추정값의 신뢰구간
# X의 주어진 값에서 신뢰대 그리기
# X = 4, 12, 0.2

pred.frame <- data.frame(x = c(4, 12, 0.2))
pc <- predict(market.lm, int = "c", newdata = pred.frame) # 기댓값 신뢰구간
pp <- predict(market.lm, int = "p", newdata = pred.frame) # 새로운 값 신뢰구간, 예측구간
head(pc, 3)
head(pp, 3)

pred.X = pred.frame$x
pred.X

plot(market$x, market$y, ylim = range(market$y, pp))
matlines(pred.X, pc, lty = c(1, 2, 2), col = "BLUE")
matlines(pred.X, pp, lty = c(1, 3, 3), col = "RED")

# b1의 검정
# 귀무가설: b1 = 0
# 대립가설: b1 != 0
# p-value = 9.935e-05
# p-value < 0.05이므로 귀무가설을 기각한다.
# 즉, 광고료가 매출액에 영향을 미친다고 할 수 있다.
# 광고료가 매출액에 영향을 미친다고 할 수 있다.

# R 활용 예 :: 가중회귀
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 4, 8, 7)
w <- 1/x
w.lm <- lm(y ~ x, weights = w)
summary(w.lm)

####################################################
# 분석 사례
####################################################
# 어떤 슈퍼마켓에서 고객이 구입하는 상품의 금액과 카운터에서 값을 치르는데 걸리는
# 시간과의 관계를 조사하였다. 이를 통해 카운터에서 값을 치르는데 걸리는 시간이
# 상품의 금액에 영향을 미치는지 알아보고자 한다.
# 구매상품의 금액 : x
# 카운터에서 값을 치르는데 걸리는 시간 : y
# 1. 데이터를 읽어들이고 그래프를 그려라.

price <- c(6.4, 16.1, 42.1, 2.1, 30.7, 32.1, 7.2, 3.4, 20.8, 1.5)
time <- c(1.7, 2.7, 4.9, 0.3, 3.9, 4.1, 1.2, 0.5, 3.3, 0.2)
supermarket <- data.frame(price, time)
head(supermarket)

# 그래프 그리기
plot(price, time, xlab = "가격", ylab = "계산시간", pch = 19,main = "가격 대비 계산시간")
abline(lm(time ~ price), col = "red")

###########################################
# 2. 회귀모형을 적합하고 회귀모형의 결과를 해석하라.
# 회귀모형 적합하기
supermarket.lm <- lm(time ~ price)
summary(supermarket.lm)
# Call:
#   lm(formula = time ~ price)
#
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.37928 -0.32771 -0.04431  0.32231  0.56126
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 0.396460   0.191488    2.07   0.0722 .
# price       0.115982   0.008979   12.92 1.22e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 0.3925 on 8 degrees of freedom
# Multiple R-squared:  0.9542,	Adjusted R-squared:  0.9485
# F-statistic: 166.9 on 1 and 8 DF,  p-value: 1.221e-06
# 단순회귀방정식 : Y = 0.396460 + 0.115982 * X
# 기울기 검정 : t-value = 12.92, p-value = 1.22e-06
# p-value < 0.05이므로 귀무가설을 기각한다.
# 즉, 가격이 계산시간에 영향을 미친다고 할 수 있다.
# 결정계수 : 0.9542
# 회귀식의 설명력이 95.42%이다.
# F-value = 166.9, p-value = 1.22e-06
# p-value < 0.05이므로 귀무가설을 기각한다.

# 분산분석표 구하기
anova(supermarket.lm)
# Analysis of Variance Table
#
# Response: time
#           Df  Sum Sq Mean Sq F value    Pr(>F)
# price      1 25.7036 25.7036  166.85 1.221e-06 ***
# Residuals  8  1.2324  0.1541
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 잔차 및 주정값을 보기
names(supermarket.lm)
cbind(supermarket, supermarket.lm$resid, supermarket.lm$fitted)
# 잔차 그림 그리기
plot(supermarket$price, supermarket$resid, pch = 19)
abline(h = 25.7036, lty = 2)
plot(supermarket.lm$fitted, supermarket.lm$resid, pch = 19)
abline(h = 0, lty = 2)

# 3. 회귀모형의 추정값과 예측값을 구하고 그래프로 나타내라.
# 추정값
p.x <- data.frame(price = c(1, 45))
pc <- predict(supermarket.lm, int = "c", newdata = p.x)
pred.x <- p.x$price
plot(supermarket$price, supermarket$time,
     ylim = range(supermarket$time, pc),
     pch = 19, main = "가격 대비 계산시간")
matlines(pred.x, pc, lty = c(1, 2, 2), col = "BLUE")



