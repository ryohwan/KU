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

##############################################################
# 회귀분석 2번째 시간
# 1. 회귀직선의 적확도(goodness of fit),
# 2. 중선형회귀분석,
# 3. 회귀분석에서의 추론,
# 4. 잔차분석에 대해 알아봄
##############################################################
# 중요 용어
# 결정계수 : 회귀제곱합을 총제곱합으로 나눈 값
# 잔차도 : 세로축에 잔차 또는 조정된 잔차를, 가로축에 독립변수의 값, 관측 아이디, 또는 회귀직선에 의해 추정된 반응변수의 값을 나타낸 산점도
# 정규확률도 : 세로축에 표본 데이터에서 관측간 분위수, 가로축에 정규분포의 이론적 분위수를 나타낸 산점도
#######################
# 정리하기
#######################
# 잔차의 제곱의 합을 최소화하는 값으로 회귀계수를 추정하는 것을 최소제곱법이라 하고,
# 최소제곱법으로 추정한 회귀계수의 값을 최소제곱추정량이라고 한다.
# 총제곱합은 반응변수 변동의 총량을 나타낸다.
# 회귀제곱합은 반응변수의 변동 중 회귀직선이 설명하는 부분을,
# 오차제곱합은 회귀직선이 설명하는 부분을 나타낸다.
# 총제곱합은 회귀제곱합과 오차제곱합의 합이다.
# 회귀제곱합을 총제곱합으로 나눈 값을 결정계수라고 한다.
#
# 회귀의 분산분석에서 𝐹–검정은 회귀모형의 타당성에 대한 검정이다.
# 회귀계수에 대한 𝑡–검정은 각 독립변수의 회귀계수가 0인지 아닌지,
# 즉 그 독립변수가 모형에 포함되는 것이 타당한지에 대한 검정이다.
#
# 잔차분석이란,
# 선형회귀분석에서 오차에 대한 가정이 만족되는지 알아보기 위해
# 잔차의 분포를 그래프를 이용하여 살펴보는 것이다. 주로 잔차도와 정규확률도 등이 쓰인다.

# 1. 회귀직선의 적확도(goodness of fit)
# 회귀직선이 예측하는 반응변수의 값과 실제 반응변수의 관측값이 가까우면 가까울수록,
# 즉, 잔차들이 작을수록 회귀진석이 데이터를 잘 설명하나다는 뜻이 된다.
# 총제곱합(SST) : 반응변수의 변동의 총량
# 회귀제곱합(SSR) : 회귀직선이 설명하는 변동
# 오차제곱합(SSE) : 회귀직선이 설명하지 못하는 변동
# SST = SSR + SSE
# 결정계수(coefficient of determination, R-sqiared)
# 0~ 1 사이의 값을 가진다.
# 결정계수가 0이면 회귀직선이 데이터를 전혀 설명하지 못한다는 뜻
# 1이면 회귀직선이 데이터를 완벽하게 설명한다는 뜻
# R^2 = SSR / SST

setwd("/Volumes/Working/6000 WorkSpace/Git/Bio_stat/")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat3<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y,
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>%
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)),
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
dat4 <- dat3 %>% mutate(log.CEA = log(CEA), log.post.CEA=log(post.CEA))
head(dat4)
obj <- lm(log.post.CEA ~ log.CEA, data=dat4)
summary(obj)

################################
# 2. 중선형회귀분석
################################
# 독립변수의 개수가 2개 이상인 선형회귀모형
# 회귀보형에 독립변수를 새로 추가할 때마다 결정계수는 항상 커진다.
# 독립변수의 개수가 다른 2개 모형의 성능을 비교할 때 결정계수를 기준으로 삼는 것은 적절하지 않다.
# 조정된 결정계수(adjusted R-squared)
# 독립변수의 개수를 계산에 포함
# 결정계수의 증가분이 기대보다 훨씬 커야만 조정된 결정계수도 증가한다.
# 음수값을 취할 수 있다.
# 결정계수보다 작거나 같다.
# 수축기혈압(SBP)를 반응변수로, 나이(age)와 몸무게(weight)를 독립변수로 하는 중선형회귀모형
obj2 <- lm(SBP~age+weight, data=dat4)
summary(obj2)

####################
# 3. 회귀분석에서의 추론
####################
# 데이터로부터 적합된 회귀모형이 얼마나 '정확'한지 구간추정, 가설점정을 통해 수치화하는 것
# 회귀모형의 전체적인 설명력에 대한 추론 : 회귀의 분산분석의 F-검정
# 회귀계수에 대한 추론 : t-검정
# 제곱합과 평균제곱
# 독립변수가 k개 있을 때,
# 회귀평균제곱 : MSR = SSR/k
# 오차평균제곱 : MSE = SSE/(n-k-1)
# F-검정 : MSR/MSE
### 귀무가설 H0 : 회귀모형이 전혀 설명력이 없다.
###### 독립변수들과 반응변수들 간에 전혀 상관관계가 없다.
###### 반응변수의 값을 예측할 때 독립변수의 값이 전혀 도움이 되지 않으며,
###### 반응변수 관측값 전체의 평균으로 예측하는 것이 최선이다.
### 대립가설 H1 : 적어도 하나의 bj는 0이 아니다.
###### 적어도 하나의 독립변수를 이용한 회귀모형이, y bar보다는 반응변수의 관측값을 더 잘 설명한다.
# 회귀의 분산분석
# 요인  제곱합   자유도   평균제곱    F-통계량
# 회귀  SSR      k    MSR=SSR/k F=MSR/MSE
# 오차  SSE   n-k-1   MSE=SSE/(n-k-1)
# 전체  SST     n-1

#####################################3
# 회귀계수에 대한 t-검정
# 특정 독립변수 xj의 회귀계수 bj가 0인지 검정
# 귀무가설 H0 : bj = 0, 대립가설 H1 : bj not= 0
# 검정통계량 : t = bj / SE(bj)

####################################
# F-검정과 t-검정이 완벽하게 똑같을 때가 있음
# 단순선형회귀모형일 결우, E(Y|X) = b0 + b1X
# 모형 전체에 대한 검정 = 회귀계수 b1에 대한 검정
# 단순선형회귀분석에서는 F-검정과 t-검정이 수학적으로 완전히 동일하다.
summary(obj)    # 단순선형회귀분석
summary(obj2)   # 중선형회귀분석

anova(obj)      # 단순선형회귀분석의 분산분석표
anova(obj2)     # 중선형회귀분석의 분산분석표

##################################
# 회귀계수의 신뢰구간
# 회귀 계수의 값마다 t분포를 계산하고
#
# bj = 0 일 때, t = bj/se(bj)가 t분포를 따른다.
# 결국, 특정 계수가 t-분포를 따른다.
# bj = bj0 일 때, t = (bj-bj0)/se(bj)
# bj에 대한 100(1-a)% 신뢰구간 :
# [bj - t(n-k-1)/a/2*se(bj), bj + t(n-k-1)/a/2*se(bj)]
# R에서는 신뢰구간 찾는 패키지로 broom 패키지의 tidy() 함수 이용
library(broom)
tidy(obj2, conf.int = TRUE)   # 기본으로 신뢰구간 95% 계산

#########################
# 4. 잔차 분석
########################
# 최소제곱법 자체는 아무런 가정을 요구하지 않는다.
# 잔차의 제곱의 합을 최소화하는 직선을 구하는 방법일 뿐이기 때문이다.
# 독립변수와 반응변수 관계의 선형성
# 선형이 아니면 회귀직선이 데이터를 잘 설명하지 못한다.
# 선형이 아니어도 여전히 잔차의 제곱의 합을 최소화한다는 의미를 가진다.
# 선형회귀분석에서 오차에 대한 가정
# 가. 오차들은 서로 독립니다. <- 잔차도
# 나. 오차의 평균은 0이다.
# 다. 오차의 분산은 sigma^2이다(분산이 일정하다) <- 잔차도
# 라. 오차는 정규분포를 따른다. <- 정규확률도
# 오차와 잔차의 차이점 : 오차는 관측할 수 없으므로, 오차 대신 잔차를 이용하여 오차에 대한 가정을 확인한다.
# 그래서 잔차분석을 위해 잔차도(residual plot), 잔차의 정규확률도(Normal Q-Q plot) 활용한다.
##################
# 잔차 분석 이후
# 선형성이나 오차에 대한 가정이 어긋난 경우
## 가정이 심각하게 어긋나서 회귀직선이 쓸모가 없는 경우 : 회귀 모형 폐기
## 가정이 어긋났어도 회귀직선이 데이터의 변동의 상당 부분을 설명하는 경우
#### 목적에 따라 회귀직선을 그대로 사용할 수 있음.
#### 회귀모형에 대한 추론은 틀리게 됨
###### -> 추론 포기
###### -> 오차에 대한 가정을 요구하지 않은 추론 방법 사용
#######################3
# 잔차분석
std.res <- rstandard(obj2)    # 조정된 잔차를 입력
yhat <- predict(obj2)         # 회귀직선이 추정하는 값을 입력
plot(yhat, std.res)
abline(h = 0)
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

# 잔차의 정규확률도
qqnorm(std.res)
qqline(std.res)


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



