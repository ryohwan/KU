###############################################
# 생존분석-1(survival analysis)
###############################################
# 1. 생존분석이란
# 2. 카플란-마이어 곡선
############
# 주요 용어
############
# 생존분석 : # 기준 시점(time zero)으로부터 사건이 일어날 때까지 걸린 시간에 대한 분석 방법
# (우)중도절단 : 마지막 추적관찰 시점까지 사건이 일어나지 않았으며, 그 이후에 사건 발생했는지, 발생했다면 언제 발생했는지 알 수 없는 경우
# time-to-event outcome :  기준 시점에서부터 사건이 일어날 때까지 걸린 시간을 나타내는 반응변수
# 생존함수 : 시간에 따른 생존확률(=사건이 아직 발생하지 않았을 확률)을 나타내는 함수
# 카플란-마이어 방법 : 중도절단된 데이터를 분석에서 제외하지 않고 모두 활용하여 생존곡선을 추정하는 방법 중 가장 흔히 쓰이는 방법. 데이터에서 관측된 사망 시점을 기준으로 시간을 여러 개의 구간으로 나눈 후, 각 구간에서의 생존확률을 조건부 확률을 이용하여 추정
# 카플란-마이어 곡선 : 카플란-마이어 방법으로 추정한 생존함수를 그래프로 그린 것
############
# 정리하기
############
# 관심 있는 사건이 일어날 때까지 걸린 시간을 나타내는 반응변수를
# time–to–event outcome이라고 하고,
# time–to–event outcome을 다루는 통계적인 분석 방법을 생존분석이라고 한다.
#
# 정확한 사건 발생 시점을 모르고 어느 시점 이후라는 것만 알 수 있는 경우
# (우)중도절단이 발생했다고 한다.
#
# 생존함수는 시간에 따른 생존확률을 나타내는 함수이다.
#
# 카플란–마이어 방법에서는 데이터에서 관측된 사망 시점을 기준으로
# 시간을 여러 개의 구간으로 나눈 후,
# 각 구간에서의 생존확률을 조건부 확률을이용하여 추정한다.
# 카플란–마이어 방법으로 추정한 생존함수를 그래프로 그린 것을
# 카플란–마이어 곡선이라고 한다.

######################
# 1. 생존분석이란
######################
# 동기부여 예제(Motivational example)
# 2020년부터 2022년까지 A병원에서 폐암 진단 받은 환자의 차트를
# 리뷰하여 데이터를 만들었다고 하자.
# 진단일로부터 사망일까지 걸린 시간에 대한 데이터
# 사망하지 않은 경우, 마지막 방문 날짜를 기록
# 여기서 사망률을 어떻게 알 수 있을까?
# 데이터에 포함된 100명 중 40명에서 사망을 관측했다면,
# 사망률이 40%인가?
# 사망률 = 사망할 확률은 언제나 100%이다. -> 모든 사람은 사망하기 때문
# 사망률은 death rate이므로, "시간 당"의 의미가 들어가야 한다.
# 입문 단계의 생존분석에서는 1가지 사건과 1번만 발생하는 사건만 고려
# -> 여러가지 사건에 대한 분석 : competing risk analysis
# -> 여러 번 발생할 수 있는 사건 : recurrent event analysis
# Time-to-event outcome을 반응변수로 한다.
# 시점을 정해서 확실히 말할 수 있는 사망률과
# 이후의 사망률은 바로 알 수 없는 경우가 발생함.
# 이에 카플라-마이어 방법으로 추정이 가능함.
######################
# 2. 카플란-마이어 곡선
######################
# 카플라-마이어 곡선은 생존함수를 추정하는 방법으로
# 생존 함수 : 시간에 따른 생존확률(=사건이 아직 발생하지 않았을 확률)을 나타내는 함수
# 확률 변수 T : time zero부터 사건 발생까지 걸리는 시간
# S(t) = P(T > t), 0 < t < 무한대
# 카플란-마이어 방법 :
## 중도절단된 데이터를 분석에서 제외하지 않고 모두 활용하여 생존곡선을 추정하는 방법 중 가장 흔히 쓰이는 방법
## 데이터에서 관측된 사망 시점을 기준으로 시간을 여러 개의 구간으로 나누 후, 각 구간에서의 생존확률을 조건부 확률을 이용하여 추정
# 카플란-마이어 곡선 : 카플란-마이어 방법으로 추정한 생존함수를 그래프로 그린 것
# 생존형태나 생존시간의 분포에 대한 어떤한 가정도 하지 않음 : 비모수적 방법

# 췌장암 환자 데이터의 생존 분석
# OP_date : 수술 날짜
# Recur_date : 재발 나짜 또는 마지막으로 재발하지 않았음을 확인한 날짜
# Recur : 재발 여부 (1 = 재발, 0=재발 안함)
# 수술 시점부터 재발할 때까지 걸리느 시간에 대한 생존분석
# -> 사전 = Recur
# -> 관측기간 = Recur_date - OP_date

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
# 날짜 데이터로 변환하기 위해서 as.Date에 OP_date, Recur_date 변환
dat5<-dat4 %>% mutate(OP_date=as.Date(OP_date, format="%Y-%m-%d"),
                      Recur_date=as.Date(Recur_date, format="%Y-%m-%d"),
                      rfs=as.double(Recur_date-OP_date)
                      )
head(dat5)
# R에서 생존분석은 survival 패키지를 사용한다.
# Surv() 함수 : time-to-event outcome 정의
# time : 관측기간(FU time)
# event : 사건(event)
library(survival)
Surv.obj<-Surv(time=dat5$rfs, event=dat5$Recur==1)

# survfit() 함수 : 생존함수 추정(default로 카픞란-마이로로 추정)
## 하나의 생존함수를 추정할 때 : Surv형 객체 ~ 1
## 여러 그룹의 생존함수를 추정할 때 : Surv형 객체 ~ 그룹을 나타내는 변수
fit<-survfit(Surv.obj~1, data=dat5)
fit
summary(fit)

# plot도 그려지지만,
# surminer 패키지의 ggsurvplot() 함수 :
## ->카플란 마이어 곡선을 그려준다.
# install.packages("survminer")
library(survminer)
ggsurvplot(fit)
plot(fit)

# ggsurvplot()의 옵션
## xscale : 가로축의 단위
## break.x.by : 가로축 눈금 단위
## risk.table : 추적관찰대상자 수
ggsurvplot(fit, xscale=365.25, break.x.by=365.25, xlab="Year", legend="none",
           risk.table = TRUE, conf.int=FALSE, title="Recurrence Free Survival")

###############################################
# 생존분석-2(survival analysis)
###############################################
# 1. 로그순위검정
# 2. 콕스 비례 위험 모형
############
# 주요 용어
############
# 콕스 비례 위험 모형 : 위험비에 로그를 취한 값을 독립변수의 선형결합으로 설명하는 모형
# 위험률 : 순간사망률
# 위험비 : 2개의 위험률의 비율
# 비례 위험 가정 : 위험비가 독립변수들의 값에만 의존하고 시간에 의존하지 않는다는 가정
##############
# 주요 정리
##############
# 로그-순위 검정은 그룹별로 생존함수가 다른지 검정하는 방법이다.

# time–to–event outcome을 반응변수로 하는 회귀모형 중
# 가장 대표적인 콕스 비례 위험 모형은 위험비(HR)에
# 로그를 취한 값을 독립변수의 선형결합으로 설명하는 모형이다.

# 콕스 비례 위험 모형의 회귀계수에 지수함수를 취한 값은
# 독립변수의 HR로 해석할 수 있다.

# 콕스 비례 위험 모형에서는
# 기저 위험(baseline hazard)을 추정하지 않으며,
# 결과적으로 생존함수를 직접적으로 추정하지 않는다.

# 콕스 비례 위험 모형의 성능을 평가하는 대표적인 방법은
# Harrell’s c-index 이다.

# 여러 그룹별로 생존함수가 다르다고 생각될 경우,
# 그룹별 생존함수가 통계적으로 유의하게 다른지 검정하려면?
# --> 로그 순위 검정

# 생존함수와 독립변수 간의 관계를 분석하려면? -> 콕스 비례 위험 모형


#########################
# 1. 로그순위검정(log-rank test)
#########################
# 생존함수를 비교하는 검정
# 비모수적 검정
# 귀무가설 : 그룹 별 생존함수가 같다.
# 대립가설 : 그룹 별 샌존함수가 다르다
# 가정 : 중도절단 여부와 사망확률은 관련이 없다.
# 로그 순위 검정을 하려면
# survival 패키지의 survdiff() 함수를 이용한다.
survdiff(Surv.obj~stage, data=dat5)

# ggsurvplot를 이용하면 자동으로 출력이 된다.
fit<-survfit(Surv.obj~stage, data=dat5)
library(survminer)
ggsurvplot(fit, xscale=365.25, break.x.by=365.25, xlab="Year",
           risk.table = TRUE, conf.int=FALSE, title="Recurrence Free Survival",
           pval=T)

#####################################################
# 2. 콕스 비례 위험 모형(Cox proportional hazards model)
######################################################
# time-to-event outcome에 대한 회귀모형 중 가장 대표적인 모형
# 위험비(hazard ratio, HR)에 로그를 취한 값을 독립변수의 선형결합으로 설명하는 모형
## 선형회귀분석 : 연속형 반응변수의 조건부 기댓값 E(Y|X)을 독립변수의 선형 결합으로 설명하는 변수
## 로지스틱 회귀모형 : 성공확률의 로짓을 독립변수의 션형결합으로 설명하는 모형
# 준모수적(semi-parametric) 생존 모형 : 생존함수의 형태를 가정하지 않지만, 비례 위험 가정을 요구
#
#
# 콕스 비례 위험 모형을 사용하기 위해서는
# survival 패키지의 coxph()함수를 사용한다.
m1<-coxph(Surv(time=rfs, event=Recur==1)~age+stage, data=dat5)
summary(m1)

# 가능도비 검정
m0<-coxph(Surv(time=rfs, event=Recur==1)~age, data=dat5)
anova(m0, m1)

# 비례위험 가정 확인
cox.zph(m1)
ggcoxzph(cox.zph(m1))
