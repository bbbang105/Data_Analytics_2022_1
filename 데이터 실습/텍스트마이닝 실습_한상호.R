## 0. 데이터 준비

# 라이브러리 불러오기
library(tm)
library(topicmodels)

# CSV 불러오기
setwd("C:/ba")
amazon <- read.csv('4gen_echo_dot.csv', stringsAsFactors = FALSE)

# Title 확인
# 크롤링한 시점에 따라 데이터 내용이 다를 수 있음
amazon$Title[1:3]

# multibyte 문제를 방지하기 위해 인코딩 변환 진행
# amazon의 Review를 UTF-8로 변환
# iconv(): 문자 벡터 인코딩 변환 함수
ecodot <- iconv(enc2utf8(amazon$Review), sub='bytes')

### 1. 데이터 전처리

## 영어 텍스트로 1차 전처리
ecodot[28]

# iconv의 인코딩에서 CJK code 등록이 안되어 있으므로, gsub에서 처리
# CJK: Chinese(중국어), Japanese(일본어), Korean(한국어)
# CJK unicode# : 4E00-9FFF(확장판)와 3000-303F(기존)
# 전처리 할 때, 라틴어 설치를 안 한 경우:
# 유니코드 숫자와 한국어 조합은 대부분 라틴 문자인 경우가 많음
ecodot <- gsub("[\U4E00-\U9FFF\U3000-\U303F]","",ecodot)

## 영어 텍스트로 2차 전처리
# iconv(): 문자 벡터 인코딩 변환 함수
# gsub에서는 아시아 국가 문자, iconv에서는 다른 나라의 문자들을 처리
# latin1 : 프랑스어, 독일어, 아이슬랜드어, 스페인어 문자
# ASCII : 미국인 대상의 표준 영어 문자
# sub : 대체 문자열 표기
ecodot <- iconv(ecodot, from="latin1", to="ASCII", sub="")

ecodot[28] # 깨진 문자가 어느정도 정리됨

# 구조 : text를 말뭉치(corpus)로 변환하여 문서 정보 파악
# text --> corpus
# 현재 데이터 : ecodot을 corpus로 변환하여 저장
# Vcorpus 이후부터는 inspect(corpus[숫자])로 내용 확인
corpus <- VCorpus(VectorSource(ecodot))

# 각자 크롤링 시점이 다르기 때문에 글이 다를 수 있음
inspect(corpus[28])

### 2. TDM

# 현재의 언어 상태 확인
Sys.setlocale()

# Multivalid 문제를 방지하기 위해서 Korean --> 영어로 변경
Sys.setlocale(category = "LC_ALL", locale = "us")

# TermDocumentMatrix를 tdm으로 생성
tdm <- TermDocumentMatrix(corpus, control=list( removePunctuation=T,
                                                stopwords= "SMART",
                                                tolower = T,
                                                removeNumbers = T,
                                                wordLength = c(5,5),
                                                stemming = F,
                                                stripWhitespace = T,
                                                weighting = weightTfIdf))

# 만든 tdm의 dimension?
dim(tdm)


### 3. LSA

# lsa 라이브러리 불러오기
library(lsa)

# tdm의 list형태 -> matrix -> textmatrix
txt_mat <- as.textmatrix(as.matrix(tdm))

# txt_mat을 10개의 차원으로 LSA 만들기
# sparsity 문제로 warning은 무시하고 진행
lsa_model <- lsa(txt_mat, dim=10)

# Terms X New LSA Space (U K)
dim(lsa_model$tk)

# Documents X New LSA Space (V K)
dim(lsa_model$dk)

# singular values (Sigma K)
length(lsa_model$sk)
