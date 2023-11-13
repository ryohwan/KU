###############################################
# 로지스틱 회귀분석-1(Logistic regression analysis)
###############################################
# 1. 로지스틱 회귀분석
# 2. 회귀계수와 오즈비
# 3. 가능도비(likelihood) 검정
# 4. 상대위험도의 추정
############
# 주요 용어
############
# 로지스틱 회귀분석 : 결과변수가 범주형 변수일 경우,
#                 설명변수와 결과변수와의 연관성을 분석하기 위해
#                 로지스틱 변환을 시행하여 하는 분석
# 오즈비 : 위험요소에 노출되었을 때의 오즈와 노출되지 않았을 때의 오즈의 비율
# 상대위험도 : 비교군 대비 관심 대상 환자군에서의 위험의 비율
#           (일반적으로, 위험요소에 노출된 경우의
#           질병발생 확률 p1과 위험요소에 노출되지 않은 경우의 질병발생 확률 p0간의 비)
# 가능도비 검정 : 해당 변수가 반응변수에 끼치는 영향을 검증
##########
# 주요 정리
##########
# 로지스틱 회귀분석에서 회귀계수는 가능도를 최대화하는 회귀계수를 찾는 최대가능도법으로 추정한다.
# 로지스틱 회귀모형의 회귀계수에 지수함수를 취한 값은 오즈비로 해석할 수 있다.
# 회지스틱 회귀분석에서도 독립변수에 대한 분포 가정은 요구되지 않으며,
# 범주형 독립변수는 가변수로 변환하여 모형에 포함된다.
# 오즈비가 아닌 상대위험도를 구해야 할 경우 로그-선형 모형이나 포아송 회귀분석을 사용할 수 있다.
# 로지스틱 회귀모형의 적합도는 가능도비 검정을 이용하여 평가할 수 있다.

#####################
# 1. 로지스틱 회귀분석
#####################
# 선형회귀분석에서의 반응변수는 연속형 변수
# 그러나, 반응변수가 번주형 변수일 경우
##### 범주형 자료 분석의 경우 아래와 같이 분석하였음.(7장)
# 연구가설 : 특정 위험인자가 결과변수 여부와 관련성이 있는가?
# -> 위험인자 : 질병의 위험에 영향을 끼치리라 생각되는 변수
# -> 결과변수 : 질병(유병) 여부 또는 관심 사건의 발생 여부
#    (예) 입원, 사망, 재발, 악화 등
# 반응변수가 범주형 변수일 경우
# -> 선형회귀분석에서의 기본 가정을 만족하지 않음!!
# 로지스틱 변환 : ln(p/(1-p))
# 일반 선형외귀모형 :           y = b0 + b1X, 그래프가 선형
# 로지스틱 회귀모형 : ln(p/(1-p)) = b0 + b1X, 그래프가 Luru
# -> 정의역은 실수 전체, 치역은 0 ~ 1

#####################
# 2. 회귀계수와 오즈비
#####################
# Y는 당뇨병이 있을 경우 1, 없을 경우 0
# X는 주 3회 시상 규칙적인 운동을 하는 경우 1, 하지 않는 경우 0
# 오즈비(OR, odds Ratio)
# -> p1 = 위험요소에 노출된 경우의 질병발생 확률
# -> p0 = 위험요소에 노출되지 않은 경우의 질병발생 확률
# 오즈(odds)는 어떤 질병이 발생할 확률 및 발생하지 않을 확률 비
# model <- glm(결과변수~설명변수, family=binomial, data=데이터명)
# -> family : 일반화 선형모형 중 어느 모형을 사용할 것인지 지정
# -> binomial : 로지스틱 회귀모형
# -> poisson : 포아송 회귀모형
# -> gaussian : 선형 회귀모형

setwd("/Volumes/Working/6000 WorkSpace/Git/Bio_stat/")
dat0<-read.csv("biostat_ex_data.csv")

library(dplyr)
dat4<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y,
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>%
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)),
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)),
         log.CEA=log(CEA),
         log.post.CEA=log(post.CEA))
colnames(dat4)
# 아래와 같이 분할표를 가지고 카이제곱 검정을 했었는데, 이 경우는
# X, Y 변수 간의 관련성이 있는가, 독립인가, 차이가 있는가를 검증한다.
table(dat4$Recur_1y)    # 2by2 분할표를 만들면
table(dat4$post.CA19.9.binary)

# 차이가 있다면 관련된 차이를 얼마나 있는지 확인하기 위해 로지스틱 회귀분석을 한다.
model.1<-glm(Recur_1y~post.CA19.9.binary, family=binomial, data=dat4)
summary(model.1)

# 위의 계산 결과에서 exp(b1X) 값을 구해야 한다.
exp(1.48)          # 직접 구하는 경우
coef(model.1)         # coef함수를 쓰면, bo와 b1를 확인할 수 있음
exp(coef(model.1))    # exp를 사용해서 값을 구한다.
library(lmtest)       # coefci를 사용하기 위해 라이브러리 불러온다.
exp(coefci(model.1))  # 95% 신뢰구간을 확인하려면, coefci를 사용한다.

# 역학 분야에서 많이 사용하는 epiR 사용해서 확인하는 방법
library(epiR)
epi.2by2(xtabs(~post.CA19.9.binary + Recur_1y, data=dat4)[2:1, 2:1])

# 환자-대조군에서 처럼 환자와 환자가 아닌 사람들을 나눠 놓고 분석할
model.2<-glm(Recur_1y~age+sex+post.CA19.9, family=binomial, data=dat4)
summary(model.2)
exp(coef(model.2))
exp(coefci(model.2))
# 요샌 상기 ratio를 이야기할 때는 유의수준을 같이 쓰지 않음.(중복 병기하지 않음)

# 비정상인 사람들 중에서도 risk가 더 놓은 사람을 알아보고 싶을 때
table(dat4$sex)
table(dat4$post.CA19.9.binary)
table(dat4$post.CA19.9.3grp)

model.3<-glm(Recur_1y~post.CA19.9.3grp+sex, family=binomial, data=dat4)
summary(model.3)
exp(coef(model.3))
# (Intercept) post.CA19.9.3grp2 post.CA19.9.3grp3 sex1
# 0.1374971         3.9658248         9.1472249   1.6677898
# 3가지 분류로 나눠져 있는 결과 변수에 어떻게 영향을 미치는가를 확인하기 위해서는
# 가능도비 검정을 통해 알아볼 수 있다.
#####################
# 3. 가능도비 검정
#####################
# post.CA19.9.3grp을 가변수로 검정한 결과
# post.CA19.9.3grp가 전체적으로 결과변수에 유의한 영향을 끼치는지 확인할 때 사용
exp(coef(model.3))
# > exp(coef(model.3))
# (Intercept) post.CA19.9.3grp2 post.CA19.9.3grp3              sex1
# 0.1374971         3.9658248         9.1472249         1.6677898
# 가능도비 검정
# -> 3개 이상의 범주를 가지는 범주형 변수에서
# -> 해당 변수가 전체적으로 반응변수에 끼치는 영향을 검정
# -> lrtest(해당 변수를 포함하는 모델, 해당 변수를 제외한 모델)
# --> likelihood ratio를 구하는 것
model.30<-glm(Recur_1y~sex, family=binomial, data=dat4)
lrtest(model.3, model.30)

#####################
# 4. 상대위험도의 추정
#####################
# 상대위험도(RR, Relative Risk)
# -> 흡연할 경우, 고혈압에 걸릴 확률과 흡연하지 않을 경우 고혈랍의 결릴 확률
# -> 위험(risk) : 어떤 인구집단에서 어떤 질병이 발생할 확률
# 오즈(odds)는 어떤 질병이 발생할 확률 및 발생하지 않을 확률 비
# RR = [d/(c+d)] / [b/(a+b)]
# OR = (d/b) / (c/a) = ad/bc

###############################################
# 로지스틱 회귀분석-2(Logistic regression analysis)
###############################################
# 1. 에측
# 2. 모형 진단
# 3. 변수 선택
# 4. 반응변수의 범주가 3개 이상인 경우
############
# 주요 용어
############
# ROC 커브 : 연속형 진단 검사의 성능을 나타낸 그래프로,
#           가능한 모든 기준점에 대하여,
#           가로축은 (1-특이도),
#           세로축은 민감도로 나타낸 점을 연결한 곡선
# AUC : ROC 커브 아래의 면적으로 진단 검사의 성능을 요약하는 지표
#############
# 정리하기
#############
# 적합된 로지스틱 회귀모형을 이용하여 새로운 단위에 대한
# 성공 확률을 예측할 수 있으며, 로지스틱 회귀모형의 예측능력을 평가하기 위해
# ROC 커브를 그리고 AUC를 구할 수 있다.
#
# 로지스틱 회귀분석의 과적합을 막기 위해,
# 반응변수의 값으로 표본 데이터를 나누었을 때
# 더 작은 그룹에 속한 단위의 개수가
# 독립변수 1개당 적어도 10~20개가 되어야 한다.
#
# 표본 크기가 작은 경우, 또는 반응변수의 분포가 편중되어서
# 로지스틱 회귀분석의 회귀계수가 추정되지 않을 경우에는
# 정확 로지스틱 회귀분석이나 Firth의 로지스틱 회귀분석 방법을 사용할 수 있다.
#
# 반응변수의 범주가 3개 이상인 경우 다항 로지스틱 회귀분석,
# 순서 로지스틱 회귀분석 등의 방법이 있다.

# 회귀분석
# 영국의 과학자 Sir Francis Galton(1822-1911)이 행한 유전에 관한 연구에서 비롯
# 부모의 신장과는 상관없이 2세의 신장이 일반 평균치에 복귀(revert)하는 특성을 발견
# Y = 33.73 + 0.516X
# X : 아버지의 키(inch)
# Y : 아들의 키(inch)
# 기울기 : 아들의 키는 인간전체의 평균치로 회귀!

###############################
# 1. 에측
###############################
setwd("/Volumes/Working/6000 WorkSpace/Git/Bio_stat/")
dat0<-read.csv("biostat_ex_data.csv")

library(dplyr)
dat4<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y,
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>%
  mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)),
         CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
         post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)),
         log.CEA=log(CEA),
         log.post.CEA=log(post.CEA))
model.2 <- glm(Recur_1y~age+sex+post.CA19.9, family=binomial, data=dat4)
summary(model.2)

predict(model.2)
predict(model.2, type="response")
# 에측 모형은 다음과 같이 표현함
# predict(구축된 모형, type="response")
# predict(구축된 모형, newdata=data.frame(조건), type="response")
predict(model.2, newdata=data.frame(age=60, sex=as.factor(0), post.CA19.9=30),
        type="response")

# Framingham Heart Study : 점수화된 위험 발생률 예측
# Framingham 지역에 지속된 데이터를 가지고 심장 질환의 위험도를 테스트(10년)
# MELD Score -> Nomogram : 세로줄의  단순 그래프를 통해 위험드를
# ROC 커브(ReceiverOperating Characteristic Curve)
library(pROC)
roc(dat4$Recur_1y~predict(model.2))

###############################
# 2. 모형 진단
###############################
# 가능도비 검정 : 한 모형이 다른 모형에 내포된 경우 두 모형을 비교
# c-index
# Hosmer-Lemeshow 적합도 검정(H-L goodness of fit test)
# -> 로지스틱 회귀모형이 예축한 '성공' 확률 크기를 기준으로 나열
# -> (통상 10개의) 그룹으로 분류 후,
# -> 각 그룹별 관측된 성공 확률과 모형에서 예측된 확률의 평균을 비교

###############################
# 3. 변수 선택
###############################
# 자동 변수 선책법 : stepwise, forward, backward method 등
# 연구가설에 따라서 선험적으로 선택
# 반응변수 기준으로 변수 하나에 그룹의 규모는 10~20개 정도
# 다중공선성, 과적합위험을 고려하여 변수 선택 필요

# 직접 로지스틱 회귀분석(exact logistic regression) : 매우 드문 경우
# library(elrm)
# elrm(formula=결과변수~설명변수,
#     interest=~설명변수, r=@, iter=@, dataset=자료명, burnin=@)

# Firth의 로지스틱 회귀분석
# library(logistf)
# logistf(결과변수~설명변수, data=자료명)

###############################
# 4. 반응변수의 범주가 3개 이상인 경우
###############################
# 범주형 변수(categorical variable) : 비 연속변수, 가감승제가 불가능한 변수
## 이분척도(Dichotomous scale) : 성별, 사망여부 등
# 여러 개의 범주형일 경우  #########################
## 명목척도(Nominal scale) : 성별, 혈액형, 인종 등
## 순위척도(Ordinal scale) : 병기, 만족도 등
##################################################
# 범주형 데이터 중 3개 이상일 때, 순서의 유무에 따른 분석 방법
# 다항 로지스틱 회귀분석(multinomial logistic regression) : 순서가 없을 경우의 범주형 데이터
# 순서 로지스틱 회귀분석(ordinal logistic regression) : 순서가 있는 경우






