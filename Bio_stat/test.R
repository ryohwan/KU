# 모집단
# 우리가 알고 싶은 대상 전체
#
# 표본
# 모집단을 알기 위해서 실제로 관측한 모집단의 일부
#
# 모수
# 모집단 전체의 특성을 나타내는 값
#O
# 통계량
# 표본의 특성을 나타내는 값
#
# 평균
# 관찰값의 총합을 관찰값의 개수로 나눈 값. 분포의 균형을 이루는 무게중심의 위치
#
# 중앙값
# 데이터를 크기 순서대로 늘어놓았을 때 정확히 중앙에 위치하는 값
# 췌장암 데이터 : biostat_ex_data.csv(췌장암 환자의 생존기간 : https://www.kaggle.com/uciml/pancreatic-cancer-dataset)
# https://github.com/biostat82/biostatistics (데이터 다운로드)
#
# 1. 데이터를 읽어온다
# setwd("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat")
dat0 <- read.csv("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/biostat_ex_data.csv")
str(dat0)
summary(dat0)
library(dplyr)
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking,
                                obesity, Recur_1y,
                                post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor)
summary(dat1)
summary(dat1$stage)
table(dat1$stage)
aa <- table(dat1$stage, dat1$sex)
# ggplot(data = <DATA>) +
#    <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
library(ggplot2)
ggplot(data = dat1) +
  geom_bar(mapping = aes(x = stage)) +
    labs(title = "Stage") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(axis.text.y = element_text(size = 10)) +
    theme(axis.title = element_text(size = 15)) +
    theme(plot.title = element_text(size = 20)) +
    theme(legend.title = element_text(size = 15)) +
    theme(legend.text = element_text(size = 15))

# 표본 평균 모의 실험
m <- 10
xbar.vec < rep(NA, m)
for (i in 1:m) {
  set.seed(1000 + i)
  xx <- sample(pop, size = 4, replace = TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main = "n = 4, m = 10", xlab = bquote(bar(X)))
mean(xbar.vec)
sd(xbar.vec)

pop <- -4:4
pop
set.seed(666)
sam <- sample(pop, size = 4, replace = TRUE)
mean(sam)

# 표본 평균 모의 실험
m <- 1000000
vec <- rep(NA, m)
for (i in 1:m) {
  set.seed(m + i)
  sam <- sample(pop, size = 4, replace = TRUE)
  vec[i] <- mean(sam)
}
vec
table(vec)
hist(vec, main = "n = 4, m = 10000", xlab = bquote(bar(X)))
mean(vec)
sd(vec)

table(dat1$sex)
prop.test(x = sum(dat1$sex == 1), n = nrow(dat1))
binom.test(x = sum(dat1$sex == 1), n = nrow(dat1))


## 중간 과제(2023년 2학기_장기형_202135-157844)
sex<-as.factor(c(rep("F", 15), rep("M", 15)))
blood.type<-as.factor(c(rep("A", 5), rep("B", 4), rep("AB", 2), rep("O", 4),
                        rep("A", 6), rep("B", 4), rep("AB", 2), rep("O", 3)))
height<-c(161, 160, 164, 172, 157, 164, 166, 169, 166, 164,
          157, 159, 162, 166, 160, 166, 157, 171, 174, 170,
          172, 165, 182, 170, 168, 171, 170, 171, 178, 171)
dd<-data.frame(sex, blood.type, height)
summary(dd)

# 2-1. 혈액형별 빈도수를 구하고, 그래프를 그리시오.
library(ggplot2)
blood.type_df <- data.frame(table(dd$blood.type))
blood.type_df
ggplot(data = blood.type_df) +
  geom_bar(aes(x = Var1, y = Freq), stat = "identity") +
    labs(title = "Blood Type_202135-157844")

# 2-2. 30명 전체의 평균 신장을 구하시오.
mean(dd$height)

# 2-3. 30명 전체의 신장의 중앙값을 구하시오.
median(dd$height)

# 2-4. 30명이 대표하는 모집단의 평균 신장에 대한 95% 신뢰구간을 구하시오.
t.test(dd$height, conf.level = 0.95)

# 3-2. 이분산 t-검정을 실시하시오.(R은 기본이 이분산)
t.test(dd$height ~ dd$sex)

library(dplyr)
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking,
                                obesity, Recur_1y,
                                post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor)
library(ggplot2)
ggplot(dat1) +
  geom_histogram(aes(x=CEA), color="black", fill="skyblue") +
    labs(title = "CEA")
ggplot(dat1) +
  geom_histogram(aes(x=log(CEA)), color="black", fill="skyblue") +
    labs(title = "log(CEA)")
# anova f-검정
fit <- aov(log(CEA) ~ stage, data = dat1)
summary(fit)

# 웰치의 F-검정
oneway.test(log(CEA) ~ stage, data = dat1)

# 크루스칼-월리스 검정
kruskal.test(CEA ~ stage, data = dat1)

# CEA와 수술 전 CEA의 차이에 대한 t-검정
dat0 <- read.csv("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/biostat_ex_data.csv")
library(dplyr)
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking,
                                obesity, Recur_1y,
                                post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor)
library(ggplot2)
ggplot(dat1) +
  geom_histogram(aes(x=log(post.CEA) - log(CEA)), color="black", fill="skyblue") +
    labs(title = "log(post.CEA) - log(CEA)")

# 대응표본 t-검정
t.test(log(dat1$post.CEA), log(dat1$CEA), paired = TRUE)

# 윌콕슨의 부호순위검정
wilcox.test(log(dat1$post.CEA), log(dat1$CEA), paired = TRUE)


# 범주형 데이터의 분석
# 카이제곱 검정
library(dplyr)
dat0 <- read.csv("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/biostat_ex_data.csv")
dat2 <- dat0 %>% mutate_at(vars(smoking, obesity, Recur_1y,
                                post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor) %>%
    mutate(HTN = as.factor(ifelse(SBP >= 140, 1, 0)))
# 자료를 정리한 범주형 데이터 내용 확인하는 방법
tapply(dat2$SBP, dat2$HTN, summary)
# xtaple 함수를 이용한 교차표 만들기
xtabs(~smoking + HTN, data = dat2)

# 두 그룹의 비교(설명 : 흡연 여/부, 결과 : 고혈압 여/부)
# 역학 라이브러리 epiR(Odds Ratio, Relative Risk, Chi-square test)
install.packages("epiR")
library(epiR)
epi.2by2(xtabs(~smoking + HTN, data = dat2)[2:1, 2:1])

chisq.test(dat2$HTN, dat2$smoking)
chisq.test(dat2$HTN, dat2$smoking)$expected

# 기대빈도가 5이하인 경우가 20~25% 이상이면 Fisher's exact test를 사용
fisher.test(dat2$HTN, dat2$smoking)

# 맥니마 검정(McNemar's test) : 대응표본의 비교 짝을 이루는 것
# 설명 : 수술 전/후, 결과 : 재발 여/부
dat3 <- dat2 %>% mutate(
  CEA.grp = as.factor(ifelse(CEA > 5, 1, 0)),
    post.CEA.grp = as.factor(ifelse(post.CEA > 5, 1, 0))
)
xtabs(~CEA.grp + post.CEA.grp, data = dat3)
mcnemar.test(dat3$CEA.grp, dat3$post.CEA.grp)

# 췌장암 환자데이터
# 나이와 수추기혈압의 산점도와 상관계수
setwd("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/")
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)
dat3 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y,
                               post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor) %>%
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)),
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
  post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
library(ggplot2)
ggplot(dat3) +
  geom_point(aes(age, SBP))

cor(dat3$age, dat3$SBP) # 피어슨 상관계수
cor(dat3$age, dat3$SBP, method = "spearman")

#수술 전/후 log(CEA)의 산점ㄷ와 상관계수
ggplot(dat3) + geom_point(aes(log(CEA), log(post.CEA)))
cor(dat3$CEA, dat3$post.CEA)
cor(dat3$CEA, dat3$post.CEA, method = "spearman")

levels(dat3$stage)
model.1 <- lm(CEA~stage, data = dat3)
summary(model.1)

# 췌장암 환자 데이터로 민감도, 특이도, 양성 예측도, ROC커브, AUC
# 아래 정리해서 하나씩 적용해 봄
setwd("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/")
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)
dat4 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity,
      Recur_1y, post.CA19.9.binary, post.CA19.9.3grp),
  as.factor
)

head(dat4)
table(dat4$post.CA19.9.binary)
tapply(dat4$post.CA19.9, dat4$post.CA19.9.binary, summary)
xtabs(~post.CA19.9.binary+Recur_1y, data = dat4)

# 역학 함수를 사용하기 위한 라이브러리 설치
# install.packages("epiR")
library(epiR)
epi.tests(
  xtabs(~post.CA19.9.binary+Recur_1y,
        data = dat4)[2:1, 2:1]
  # [2:1, 2:1] 이 부분은 데이터 값이 0, 1 이여서 변환해 준 것임.
)

# pROC 패키지를 깔고 ROC 커브를 측정
# ROC 커브의 경유, 0.7 이상이여야 좋은 검사법임.
# install.packages("pROC")
library(pROC)
fit <- roc(Recur_1y ~ post.CA19.9, data = dat4)
fit
plot(fit)

coords(fit)   # 민감도와 특이도를 좌표를 목록으로
coords(fit, x = "best", best.method = "youden")
coords(fit, x = "best", best.method = "closest.topleft")

############
# 회귀분석 : 로지스틱 회귀
#####################
setwd("/Users/ryohwan/Downloads/6000 WorkSpace/Python_space/Bio_stat/")
dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)
dat4 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity,
                                Recur_1y, post.CA19.9.binary, post.CA19.9.3grp),
                           as.factor
)

head(dat4)
table(dat4$post.CA19.9.binary)
tapply(dat4$post.CA19.9, dat4$post.CA19.9.binary, summary)
xtabs(~post.CA19.9.binary+Recur_1y, data = dat4)

# 역학 함수를 사용하기 위한 라이브러리 설치
# install.packages("epiR")
library(epiR)
epi.tests(
  xtabs(~post.CA19.9.binary+Recur_1y,
        data = dat4)[2:1, 2:1]
  # [2:1, 2:1] 이 부분은 데이터 값이 0, 1 이여서 변환해 준 것임.
)

model1.2 <- glm(Recur_1y ~ age + sex + post.CA19.9, family = binomial, data = dat4)
summary(model1.2)
exp(coef(model1.2))
# exp(coefci(model1.2))
# 추정값 확인하기
predict(model1.2)
predict(model1.2, type = "response")
# 새로운 조건이 들어왔을 때, 예상되는 확률
# predict(구축된 모형, newdata = data.frame(조건), type = "respose")
predict(model1.2, newdata = data.frame(
  age = 60,
  sex = as.factor(0),
  post.CA19.9 = 30
), type = "response")

library(pROC)
roc(dat4$Recur_1y~predict(model1.2))

# 모형 진단
# 가능도비 검정478
