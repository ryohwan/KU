hospital <- read.table("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/ regression analysis/hospital.txt", header = T)
head(hospital)
hospital.lm <- lm(Y ~ ., data = hospital)
summary(hospital.lm)
anova(hospital.lm)

# 다중공산선(VIF),  확인
install.packages("fmsb")
library(fmsb)
VIF(lm(X1~X2+X3+X4+X5, data = hospital))
VIF(lm(X2~X1+X3+X4+X5, data = hospital))
VIF(lm(X3~X1+X2+X4+X5, data = hospital))
VIF(lm(X4~X1+X2+X3+X5, data = hospital))

# 설명변수간의 선형종속관계 확인
cor(hospital[, -6])

# X1을 제외하고 다중공산성이 높은 변수 제거, 다소 높게 나옴.
summary(lm(Y~X2+X3+X4+X5, data = hospital))
VIF(lm(X2~X3+X4+X5, data = hospital))
VIF(lm(X3~X2+X4+X5, data = hospital))
VIF(lm(X4~X2+X3+X5, data = hospital))

# All possible regression
install.packages("leaps")
library(leaps)
all.lm <- regsubsets(Y~., data = hospital)
(rs <- summary(all.lm))
names(rs)
rs$rsq
rs$adjr2
rs$cp
plot(rs$cp, xlab = "Number of Variables", ylab = "Cp")

# Stepwise selection
# stepwise selection은 AIC를 기준으로 함.
start.lm <- lm(Y~1, data = hospital)
full.lm <- lm(Y~., data = hospital)
step.lm <- step(start.lm, scope = list(lower = start.lm, upper =full.lm), direction = "forward")

# Backward selection
# backward selection은 AIC를 기준으로 함.
step(full.lm, direction = "backward")

# Stepwise selection
# stepwise selection은 AIC를 기준으로 함.
step(start.lm, scope = list(lower = start.lm, upper =full.lm), direction = "both")


