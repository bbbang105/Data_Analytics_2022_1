setwd("C:/ba")

# 감성사전을 불러 오기 위한 라이브러리 설치와 불러오기
install.packages("readr")
library(readr)

# 감성사전을 불러 오기 위한 라이브러리 설치와 불러오기
rev <- read_delim("review_dict.txt", delim="\t", col_names=c("word", "score"))

# 감성사전 확인
head(rev, 10)

# 감성분석을 위한 라이브러리 설치와 불러오기
install.packages("SentimentAnalysis")
library(SentimentAnalysis)

# sentdic에 감성분석 전용 사전의 단어에 따른 점수로 
# 가중치를 매길 수 있게 기준 설정
sentdic <- SentimentDictionaryWeighted(words = rev$word, scores = rev$score)

# 점수가 0보다 크면 긍정어(positive), 
# 0보다 작으면 부정어(negative)로 기준 설정
sentdic <- SentimentDictionary(rev$word[rev$score>0], rev$word[rev$score<0])

# 감성사전 기준 확인
summary(sentdic)

# 미리 크롤링 해놓은 review.txt 파일을 txt에 저장
txt <- readLines("review.txt", encoding = "UTF-8")
head(txt)

# stringr 패키지를 통해 텍스트 대체 실시
install.packages("stringr")
library(stringr)

# 마침표, 쉼표, 느낌표, 물음표 제거
txt_2 <- str_replace_all(txt, "([.,!?])","")
head(txt_2)

# txt_2 데이터 타입 확인
class(txt_2)

# 문서형 데이터 형태 변환을 위해 tm 패키지 설치 및 라이브러리 불러오기
install.packages("tm")
library(tm)

# txt_2 데이터를 Corpus 형태로 변환
co_txt <- Corpus(VectorSource(txt_2))
class(co_txt) # 데이터 타입 확인
inspect(co_txt) # co_txt 형태 살펴보기

# Corpus 형태에서 DocumentTermMatrix 형태로 변환
dtm_txt <- DocumentTermMatrix(co_txt)

# DocumentTermMatrix 형태 살펴보기
inspect(dtm_txt)

# dtm_txt를 위에서 만든 감성 기준 사전 sentdic을 이용해 분석
res <- analyzeSentiment(dtm_txt, language="korean", 
                        rules=list("sentiment"=list(ruleSentiment, sentdic)))
# 결과확인
head(res)

# sentiment가 0보다 크면 긍정, 0이면 중립, 0보다 작으면 부정으로 표
시
res$pn <- ifelse(res$sentiment>0,"Positive",
                 ifelse(res$sentiment==0,"Neutral","Negative"))
head(res)

# 결과 요약해서 보기
table(res$pn)

# 결과 별도 저장하되 데이터 프레임 형태로 변환
df_res <- as.data.frame(table(res$pn))

# 데이터 프레임 열이름 별도 지정
names(df_res) <- c("res","freq")

# 파이차트에 퍼센트 표시를 위해 pct 열 생성
df_res$pct <- round(df_res$freq/sum(df_res$freq)*100, 1)
df_res

# 파이차트로 감성 분석 결과 확인하기
pie(df_res$freq, labels = paste(df_res$res, df_res$pct, "%"),
    main = "생닭 판매 고객 리뷰 감성 분석 결과")
