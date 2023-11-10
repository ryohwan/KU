#####################################
# 선형회귀분석
#####################################
# 상관계수의 종류
# 1. 피어슨 상관계수(Pearson's Correlation coefficient)
# 피어슨 상관계수는 두 연속형 변수의 선형관계의 강도와 방향을 나타내는 측도이다.
# 모집단/포본의 피어슨상관계수
# 피어슨 상관계수는 -1에서 1 사이의 값을 가지며,
# 1에 가까울수 록 강한 양의 상관관계를, -1에 가까울수록 강한 음의 선형관계를 나타낸다.
# 피어슨 상관계수가 0에 가까울수록 약한 선형관계를 의미한다.
###################
# 2. 스피어맨 상관계수 (Spearson's Correlation coefficient)
# 스피어맨 상관계수는 두 변수의 관계가 단조함수(monotonic function)로 얼마나 잘 설명될 수 있는지를 측정한다.
# 스피어맨 상관계수는 -1에서 1 사이의 값을 가지며,
# 1에 가까울수록 강한 양의 관계를, -1에 가까울수록 강한 음의 관계를 나타낸다.
# 관계가 선형이 아니더라도 상관관계의 방향이 일정한 경우 완벽한 상관관계인 것으로 평가하는 측도
# 두 변수 간의 함수적 관계를 알아보는 것을 회귀분석이라고 한다.
# 회귀분석을 수행할 때에는 데이터의 모형을 먼저 가정한 후, 데이터를 이용하여 모형의 회귀계수를 추정한다.
# 스피어맨 상관계수는 순위로 하기 때문에 모집단에서 할 수 없고, 표본만 가능함
###################
# 주요 용어
# 피어슨 상관계수 : 선형인 상관관계의 강도와 방향을 나타내는 측도
# 스피어맨 상관계수 :  두 변수의 관계가 단조함수로 얼마나 잘 설명될 수 있는지 측정하는 측도
# 회귀분석 : 독립변수와 반응변수 간의 함수적 관계를 알아보는 것
# 선형회귀분석 : 모형이 선형함수인 회귀분석
# 잔차 : 반응변수의 실제 관측값에서 회귀모형에 의해 추정된 값을 뺀 값
# 최소제곱법 : 잔차의 제곱의 합을 최소화하는 값으로 회귀계수를 추정하는 방법

#### Program 6-1
# 나이와 수축기혈압의 산점도와 상관계수 확인하기
setwd("/Volumes/Working/6000 WorkSpace/Git/Bio_stat/")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat3<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>% 
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)), 
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
library(ggplot2)
ggplot(dat3) + geom_point(aes(age, SBP))
cor(dat3$age, dat3$SBP)
cor(dat3$age, dat3$SBP, method="spearman")

#### Program 6-2
# 수술 전/후 log(CEA)의 산점도와 상관계수
ggplot(dat3) + geom_point(aes(log(CEA), log(post.CEA)))
cor(log(dat3$CEA), log(dat3$post.CEA))
cor(log(dat3$CEA), log(dat3$post.CEA), method="spearman")

#### Program 6-3
# 단순회귀분석(Simple  Linear Regression 또는 Univariate Linear Regression) : 독립변수 1개인 경우
# 중선형회귀(Multiple Liear Regression 또는 Multivariable Linear Regression) : 독립변수가 여러 개인 경우
# 회귀분석은 독립변수(=설명변수, X)와 반응변수(=종속변수=결과변수, Y) 간의 함수적 관계를 알아보는 것
# 보통 모형을 정해놓고 시작한다.
# 선형모형 : Y = bo + b1X
# 이차함수모형 : Y = b0 + b1X + b2X^2
# 지수함수모형 : Y = exp(bo + b1X)

# log(CEA)를 독립변수로, log(post.CEA)를 반응변수로 하는 단순선형회귀분석
dat4<-dat3 %>% mutate(log.CEA=log(CEA),
                      log.post.CEA=log(post.CEA))
obj<-lm(log.post.CEA~log.CEA, data=dat4)
summary(obj)

#### Program 6-4
ggplot(dat4, aes(log.CEA, log.post.CEA)) + geom_point() +
  geom_smooth(method="lm")

#### Program 6-5
dat.new<-data.frame(log.CEA=c(1, 2, 3))
predict(obj, newdata=dat.new)

#### Program 6-6
obj2<-lm(SBP~age+weight, data=dat4)
summary(obj2)

#### Program 6-7
anova(obj)

#### Program 6-8
anova(obj2)

#### Program 6-9
library(broom)
tidy(obj2, conf.int=TRUE)

#### Program 6-10
std.res<-rstandard(obj2)
yhat<-predict(obj2)
plot(yhat, std.res)
abline(h=0)
abline(h=2, lty=2)
abline(h=-2, lty=2)

#### Program 6-11
qqnorm(std.res)
qqline(std.res)

#### Program 6-12
levels(dat4$stage)

#### Program 6-13
model.1<-lm(log.CEA~stage, data=dat4)
summary(model.1)

#### Program 6-14
dat5<-dat4 %>% mutate(stage.new=relevel(stage, ref=2))
levels(dat5$stage.new)
model.2<-lm(log.CEA~stage.new, data=dat5)
summary(model.2)

#### Program 6-15
model.3<-lm(log.CEA~age+sex+stage, data=dat4)
summary(model.3)

#### Program 6-16
model.4<-lm(log.CEA~age+sex, data=dat4)
library(lmtest)
lrtest(model.3, model.4)

#### Program 6-17
anova(model.3)



