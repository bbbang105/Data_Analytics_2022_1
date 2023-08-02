
## 0. setwd 경로 설정
setwd('c:/ba')

## 1. 데이터 구조 확인

# 데이터 읽기
teens <- read.csv("snsdata.csv")

# 구조 확인
str(teens)

# gender 변수의 결측 데이터 확인
table(teens$gender)

# 결측값을 포함할 수 있도록 ifany 작성
table(teens$gender, useNA = "ifany")

# age 변수의 결측 데이터 확인
summary(teens$age)

# age 이상치(outliers) 제거
teens$age <- ifelse(teens$age>=13 & 
                      teens$age<20,teens$age, NA)

#age 변수의 데이터 확인
summary(teens$age)

# "unknown"인 성별값에 재부여
teens$female <- ifelse(teens$gender=="F"&
                          !is.na(teens$gender),1, 0)
teens$no.gender <- ifelse(is.na(teens$gender),1, 0)

# 재지정한 작업에 대한 확인
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no.gender, useNA = "ifany")

# 집단(cohort)별 나이 평균
mean(teens$age,na.rm = TRUE)

# 집단별 나이
aggregate(data = teens, age ~ gradyear, mean, 
          na.rm = TRUE)

# 각 개인에 대한 예측된 나이 계산
ave_age <- ave(teens$age, teens$gradyear, 
               FUN=function(x) mean(x, na.rm=TRUE))

teens$age <- ifelse(is.na(teens$age), ave_age,
                    teens$age)

# 제거한 결측치에 대한 요약 결과 확인
summary(teens$age)

## 2. kmeans 모형 구축

# 시드 설정
set.seed(2345)

# 다양한 관심사의 횟수를 표현하는 36개의 특징 추출
interests <- teens[5:40]

# Data scailing
interests_z <- as.data.frame(lapply(interests, scale))

# 자료에 대해 k=5로 나눠줌
teen_clusters <- kmeans(interests_z, 5)

# 군집의 크기 확인
teen_clusters$size

## 3. 분석 결과 확인

# 군집의 중앙점(centers) 확인
teen_clusters$centers

# 본래 데이터 프레임에 군집ID(cluster ID) 적용
teens$cluster <- teen_clusters$cluster

# 처음 5개 데이터 확인
teens[1:5, c("cluster", "gender", "age", "friends")]

# 군집 별 평균 나이
aggregate(data=teens, age~cluster, mean)

# 군집 별 여성 비율
aggregate(data=teens, female~cluster, mean)

# 군집 별 친구 수의 평균
aggregate(data=teens, friends~cluster, mean)
