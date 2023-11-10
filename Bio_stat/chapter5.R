################################################
# 범주형 변수의 특성과 구 그룹의 비교하는 방식을 R로 실습함.
################################################

# Program 5-1
# 데이터를 분석하기 위해 범주형으로 변환하여 불러옴.
setwd("/Volumes/Working/6000 WorkSpace/Git/Bio_stat/")
dat0<-read.csv("biostat_ex_data.csv")
library(dplyr)
dat2<-dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity, Recur_1y, 
                              post.CA19.9.binary, post.CA19.9.3grp),
                         as.factor) %>% 
               mutate(HTN=as.factor(ifelse(SBP>=140, 1, 0)))

colnames(dat0)  # 고혈압 변수 없음
colnames(dat2)  # 범주형 데이터를 업데이트함. HTN
tapply(dat2$SBP, dat2$HTN, summary)   # summary by group using (by dplyr)

#### Program 5-2
# 2by2 테이블로 만들어 확인한다.
# 열은 결과 변수, 행은 설명로 놓여 있는지 확인
# 분할표를 기반으로 두 변수 간의 관련성을 파악하려면
# 칸 빈도(cell frequency), 칸 퍼센티지(cell Percentage)
# 행 퍼센티지(row percentage), 열 퍼센티지(collumn percentage)
# 등 연구의 특성에 따라 달라짐.

xtabs(~smoking+HTN, data=dat2)

# 위헙인자에 따라 결과변수 발생이 늘어나는가?

# 1. 코호트 연구
# 예) 질병이 없는 사람을 선택하고, 설명변수에 해당 부분을 추적 관찰함.
# 상기 사례에서는
#         HTN
# smoking  0  1
#       0 89 26
#       1 23 18
# 흡연할 경우, 고혈압에 걸릴 확률 : 18 / (23 + 18) = 0.44 --> p1
# 흡연을 하지 않을 경우, 고협압에 걸릴 확률 : 26 / (26 + 89) = 0.23 --> p0
# 위험(risk) : 어떤 인구집단에서 어떤 질병이 발생할 확률
# p1 = 위헙요소에 노출된 경우의 질병발생 확률
# p0 = 위헙요소에 노출되지 않은 경우의 질병발생 확률
# 상대위헙도(RR) = p1 / p0 = 0.44 / 0.23 = 1.9
# 흡연자는 비흡연자에 비해서 고혈압 위험이 1.9배
# 관찰 시작 시점에서 관찰된 정보여야 이런 판단이 옳다!!

# 2. 환자 - 대조군연구
# 임상의학에서 원인 및 위헙요인 구명함에 있어, 대표적으로 사용되는 역학적 연구설계의 하나
# 최근에 알게된 새로운 위험요소에 대한 알고 싶을 때, 사전에 조사가 불가능한 경우
# 관심이 있는 경우, 아닌 경우를 조사해서 봄
# 관찰시점 / 연구 시점
# 오즈비(OR, Odds Ratio)
# p1 = 위험요소에 노출된 경우의 질병발생 확률
# p0 = 위험요소에 노출되지 않은 경우의 질병발생 확률
# 오즈(odds)는 어떤 질병이 발생할 활률 및 발생하지 않을 확률 비
# 흡연군에서의 odds = [18/(23+18)] / [1-18/(23+18)] = 18/23
# 비흡연군에서의 odds = 26/89
# 오즈비(OR) = [p1(1-p1)]/[p0(1-p0)]
#           = (18*89)/(23*26) = 2.7

#### Program 5-3
# 역학 분석을 위한 R 패키지 에픽알(epiR)
library(epiR)
epi.2by2(xtabs(~smoking+HTN, data=dat2)[2:1, 2:1])

#### Program 5-4
chisq.test(dat2$HTN, dat2$smoking)
chisq.test(dat2$HTN, dat2$smoking)$expected
chisq.test(xtabs(~smoking+HTN, data=dat2))
chisq.test(dat2$HTN, dat2$smoking, correct = F)

#### Program 5-5
# 피셔의 직접검정 (Fisher'(s exact test)
# 피셔의 직접검정은 표본의 크기와 관계없이, 언제나 직접 유의확률을 제공
# 표본 크기가 커지면 계산이 복잡해지고, 타이제곱 검정 결과와 유사
# 일반적으로 표본 크기가 충분히 크면 카이제곱 검정을 사용
# 일반적으로, 기대빈도가 5미만 셀이 있는 경우 수행
fisher.test(dat2$HTN, dat2$smoking)

#### Program 5-6
# 짝지어진 데이터의 비교
# 췌장암에서 중요한 혈액 지표 중, CEA 값을 수술 전/후로 각각 측정함.
# 짝지어진 데이터로 서로 독립이 아니여서, 카이제곱(x^2), 피셔의 직접검정이 불가능함.
# 맥니마 검정(McNemar's test)
dat3<-dat2 %>% mutate(CEA.grp=as.factor(ifelse(CEA>5, 1, 0)),
                      post.CEA.grp=as.factor(ifelse(post.CEA>5, 1, 0)))
xtabs(~CEA.grp+post.CEA.grp, data=dat3)
mcnemar.test(xtabs(~CEA.grp+post.CEA.grp, data=dat3))
mcnemar.test(dat3$CEA.grp, dat3$post.CEA.grp)



