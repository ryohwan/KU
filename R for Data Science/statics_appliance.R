# 두 모집단의 비교
sample1 <- c(10.2, 10.5, 10.3, 10.8, 9.8, 10.6, 10.7, 10.2, 10.1, 10.2)
sample2 <- c(9.8, 9.6, 10.1, 10.2, 10.1, 9.7, 9.5, 9.6, 9.8, 9.9)
plot(density(sample1), lty = 1, ylim = c(0, 1.5))
lines(density(sample2), lty = 2, ylim = c(0, 1.5))
boxplot(sample1, sample2, ylab = "약효",
        names = c("약A", "약B"), col = c("red", "blue"),
    main = "생산 직후와 1년 이후의 약효 비교")
# 두 집단의 평균이 값이 같은지 검정
t.test(sample1, sample2, var.equal = TRUE)

# 짝지어진 데이터의 비교
# 운동화의 밑창에 사용되는 두 가지 재질의 내구성을 비교하기 위해
# 10쌍의 운동화를 만들어 각각의 내구성을 비교하였다.

# 랜덤화블록계획
y <- c(98, 99, 98.6, 97.6, 97.7, 98, 98.2, 97.3, 96.5, 97.9, 96.9, 96.7)
a <- c(rep(1, 4), rep(2, 4), rep(3, 4))
b <- c(rep(c(1, 2, 3, 4), 3))
strength <- data.frame(y, a, b)
strength$a <- factor(strength$a, levels = c(1, 2, 3), labels = c("a1", "a2", "a3"))
strength$b <- factor(strength$b, levels = c(1, 2, 3, 4), labels = c("b1", "b2", "b3", "b4"))
boxplot(y ~ a, data = strength, ylab = "강도", main = "플라스틱 제품 감도")
boxplot(y ~ b, data = strength, ylab = "강도", main = "플라스틱 제품 감도")

anova <- aov(y ~ a + b, data = strength)
summary(anova)

# 공분산분석
# 원사의 두께
du <- c(20, 25, 24, 25, 32, 22, 28, 22, 30, 28, 21, 23, 26, 21, 15)
gang <- c(36, 41, 39, 42, 49, 40, 48, 39, 45, 44, 35, 37, 42, 34, 32)
machine <- c('1', '2', '3')
machine <- rep(machine, c(5, 5, 5 ))
textile.data <- data.frame(du, gang, machine)
boxplot(gang ~ machine, data = textile.data, ylab = "원사의 두께", main = "원사의 두께에 따른 강도")
# 일원배치
oneway.out <- aov(gang ~ machine, data = textile.data)
summary(oneway.out)
# 공분산
anova <- aov(gang ~ du + machine, data = textile.data)
summary(anova)



# 일원배치
a1 <- c(15.4, 14.4, 15.0)
a2 <- c(15.9, 14.8, 14.1)
a3 <- c(15.5, 15.4, 15.0)
gang1 <- c(a1, a2, a3)
group <- c("a1", "a2", "a3")
group <- rep(group, c(3, 3, 3))
gang1.dat <- data.frame(group, gang1)
tapply(gang1, group, sum)
tapply(gang1, group, mean)
boxplot(gang1 ~ group)
aov.out <- aov(gang1 ~ group, data = gang1.dat)
summary(aov.out)

sqrt(0.3822/3)



