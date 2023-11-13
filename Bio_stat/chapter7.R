∑##########################################
# 진단 검사의 평가
##########################################
# 1. 타당도 : 민감도, 특이도, 양성/음성 예측도
# 2. ROC곡선
###########
# 주요 용어
###########
# 민감도 : 실제 양성인 환자를 진단검사가 양성으로 예측하는 비율
# 특이도 : 실제 음성인 환자를 진단검사가 음성으로 예측하는 비율
# 양성예측도 : 진단검사 양성인 경우, 실제 질병 양성일 확률
# 음성예측도: 진단검사 음성인 경우, 실제 질병 음성일 확률
# ROC 커브 : 연속형 진단 검사의 성능을 나타낸 그래프로, 가능한 모든 기준점에 대하여, 가로축은 (1-특이도), 세로축은 민감도로 나타낸 점을 연결한 곡선이다.
# AUC : ROC 커브 아래의 면적으로 진단 검사의 성능을 요약하는 지표
#
###########
# 정리하기
###########
# 이분형 반응 변수를 예측하기 위한 진단 검사의 결과 역시 이분형일 경우,
# 민감도, 특이도, 양성예측도, 음성예측도 등으로 성능을 평가할 수 있다.
#
# 양성예측도와 음성예측도는 유병률의 영향을 받으므로,
# 연구디자인에 따라 추정이 불가능한 경우가 있다.
#
# 연속형으로 결과가 나오는 진단 검사의 성능은 ROC 커브로 나타낼 수 있다.
# ROC 커브는 가능한 모든 기준점에 대해 가로축은(1-특이도),
# 세로축은 민감도로 나타낸 점을 연결한 곡선이다.
#
# ROC 커브 아래쪽의 면적을 AUC라고 하며,
# 연속형 진단 검사의 성능을 요약하는 지표이다. AUC가 높을수록 예측력이 우수함을 의미한다.
#
# 연속형 진단 검사의 기준점은 임상적 배경지식을 활용하여,
# 양성을 음성으로 잘못 예측하는 오류와
# 음성을 양성으로 잘못 예측하는 오류의 비용을 고려하여 결정하는 것이 바람직하다.
#
#
# 1. 타당도 : 민감도, 특이도, 양성/음성 예측도
# 2*2 분할표
# 진단검사 결과(정상/비정상)를 기준으로 관심 사건 발생여부를 예측하려명?
# 설명변수 : 진단검사의 결과
# 결과변수 : 수술 후 재발 여부(에를 들면, 3년 기준)
# CA 19-9 > 37(양성)와 작을 경우(음성)
# 진단의 타당도를 알아보기 위해 분류결과표를 가지고 정리한다.
# 민감도 : 실제 양성인 환자를 진단검사가 양성으로 예측하는 비 = TP/(TP+FN)
# 특이도 : 실제 음성인 환자를 진단검사가 음성으로 예측하는 비 = TN/(TN+FP)
# 양성 예측도(positive predictive value) : 진단검사 양성인 경우, 실제 질병 양성일 확률 = TP/(TP+FP)
# 음성 예측도(negative predictive value) : 진단검사 음성인 경우, 실제 질병 음성일 확률 = TN/(TN+FN)

#### Program 7-1
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
table(dat4$post.CA19.9.binary)
tapply(dat4$post.CA19.9, dat4$post.CA19.9.binary, summary)
xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)
library(epiR)
epi.tests(xtabs(~post.CA19.9.binary+Recur_1y, data=dat4)[2:1, 2:1])

######################
# 2. ROC 곡선(Receiver Operating Characteristic Curve)

#### Program 7-2
library(pROC)
fit<-roc(Recur_1y~post.CA19.9, data=dat4)
fitplot(fit)
fit
plot(fit)

#### Program 7-3
coords(fit)

#### Program 7-4
coords(fit, x="best", best.method="youden")   # 민감도와 특이도의 합이 높아지는 것
coords(fit, x="best", best.method="closest.topleft")    # ROC의 꼭지점







