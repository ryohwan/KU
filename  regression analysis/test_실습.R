##################
### 대비와 직교 분해
#################
gang <- c(11, 18, 25, 1, 6, 14, 6, 15, 18)
wol <- c(rep(0, 3), rep(1, 3), rep(2, 3))
temp <- c(rep(c(0, 1, 2), 3))
plastic.data <- data.frame(gang, wol, temp)
plastic.data$wol <- factor(plastic.data$wol, levels = c(0, 1, 2), labels = c("a0", "a1", "a2"))
plastic.data$temp <- factor(plastic.data$temp, levels = c(0, 1, 2), labels = c("t0", "t1", "t2"))
anova <- aov(gang ~ wol + temp, data=plastic.data)
summary(anova)

###################
# 원인 규명을 위한 대비
###################
c1 <- c(1/6, 1/6, -1/3)
c2 <- c(1/3, -1/3, 0)
mat.wol <- cbind(c1, c2)
contrasts(plastic.data$wol) <- mat.wol
c3 <- c(-1, 0, 1)
c4 <- c(1, -2, 1)
mat.temp <- cbind(c3, c4)
contrasts(plastic.data$temp) <- mat.temp
mod.contrast <- aov(gang ~ wol + temp, data = plastic.data)
summary(mod.contrast, split = list(wol=list("국산과 외제" = 1, "자사와 국내 타회사"=2), temp = list("linear" = 1, "quadratic" = 2)))

#################################################################
# 요인 배치법
#################################################################
# 2^2 요인 배치법
gang <- c(4, 6, 3, 7, -2, 2, -4, -6)
temp <- c(0, 0, 0, 0, 1, 1, 1, 1)
humidity <- c(0, 0, 1, 1, 0, 0, 1, 1)
ex7.2data <- data.frame(temp, humidity, gang)
ex7.2data$a <- factor(ex7.2data$temp, levels = c(0, 1), labels = c("T0", "T1"))
ex7.2data$b <- factor(ex7.2data$humidity, levels = c(0, 1), labels = c("H0", "H1"))
attach(ex7.2data)
with(ex7.2data,
     interaction.plot(x.factor = temp, trace.factor=humidity,
                      response = gang, fun = mean, type="b", legend = T, ylab="강도",
                      main = "Interacrion Plot", pch=c(1, 19)))
boxplot(gang ~ temp)
boxplot(gang ~ humidity)
aov.out <- aov(gang ~ temp * humidity, data = ex7.2data)
summary(aov.out)

###############
# 2^n 요인배치법
##############
strength <- c(-1, 5, 9, 11, 0, 3, 4, 8, -1, -9, 1, -5, -9, -13, 5, -4)  # 강도
temp <- rep(c(rep(-1, 4), rep(1, 4)), 2)  # 온도
humid <- rep(c(-1, -1, 1, 1), 4) # 습도
press <- rep(c(-1, 1), 8)  # 압력
vib <- c(rep(-1, 8), rep(1, 8)) # 진동
strength.data <- data.frame(strength, temp, humid, press, vib)
temp.f <- as.factor(strength.data[,2])
humid.f <- as.factor(strength.data[, 3])
press.f <- as.factor(strength.data[, 4])
vib.f <- as.factor(strength.data[,5])
order <- sample(16)   # 전체 실험의 랜덤화
df <- data.frame(temp.f, humid.f, press.f, vib.f, order)
par(bg=rgb(1, 1, 0.8), mfrow = c(2, 2))  # 2 by 2 graph
qqnorm(strength)   # 반응변수인 강도를 정규확률지에 plot하여 이상치 있는지 확인
qqline(strength, col = 2)    # 점들이 가장 적합한 직선 그음
boxplot(strength, horizontal = TRUE, main = "BOX Plot", xlab = "Strength")
hist(strength, main = "Histogram", xlab = "Strength")
plot(order, strength, xlab = "Actual Run Order", ylab = "Strength",
     main = "Run Order Plot")   # 실험순서 별 강도에 이상치 있는지 확인
#####
# 상기 내용으로 봤을 때, 분석할 가치가 있어 분석함
par(bg = rgb(1, 1, 0.8), mfrow(2, 2))
boxplot(strength ~ temp, data = df, main = "Strength by temperature",
        xlab = "Temperature", ylab = "Strength")    # 온도 주효과 파악
boxplot(strength ~ humid, data = df, main = "Strength by humidity",
        xlab = "humidity", ylab = "Strength")     # 습도 주효과 파알
boxplot(strength ~ press, data = df, main = "Strength by press",
        xlab = "press", ylab = "Strength")      # 압력 주효과 파악
boxplot(strength ~ vib, data = df, main = "Strength by vibration",
        xlab = "vibration", ylab = "Strength")      # 진동 주효과 파악
par(mfrow=c(1,1))
# 실제 분석하기
upto3 <- aov(strength ~ (temp + humid + press + vib)^3, data = df)
summary(upto3)



