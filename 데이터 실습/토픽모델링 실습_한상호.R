## 0. 데이터 준비

# 라이브러리 로딩

library(topicmodels)
library(tidytext)
library(tidyr)
library(ggplot2)
library(dplyr)

# DTM 예제 데이터 로딩
data("AssociatedPress")

# 예제 데이터 확인
AssociatedPress

## 1. LDA모델링

# k : 토픽 수
# method : "Gibbs" 선택
ap_lda <- LDA(AssociatedPress,
              k = 3,
              method = "Gibbs",
              control = list(seed = 1234))
ap_lda

## 2. 베타 탐색

# tidy() : LDA 모형 결과 확인
# 구축된 모형으로부터 beta (토픽 별 단어 확률분포) 도출
# 도출 기준 : beta / gamma(문서 별 토픽 확률분포)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# 토픽 별 베타 정렬
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms

# ggplot() : 그림 입력을 "+" 이용하여 scale까지 표현
# geom_col : column 에 대한 정보
# facet_wrap() : 그래프 함수
# coord_flip() : 데이터 포인트 그리기
# scale_x_reordered : x축 재정렬

# ap_top_terms를 이용하여 ggplot 그리기
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

geom_col(show.legend = FALSE)
geom_col(show.legend = TRUE)

## 3. 토픽 별 쌍대 비교

# spread() : value가 있는 다수 열을 선택
# beta_spread1 : topic 1과 topic2 비교
beta_spread1 <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread1

# ggplot을 이용하여 beta_spread1의 그래프 생성
beta_spread1 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# beta_spread2 : topic 1과 topic3 비교
beta_spread2 <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))

# beta_spread3 : topic 2과 topic3 비교
beta_spread3 <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic2))

beta_spread3

# ggplot을 이용하여 beta_spread3의 그래프 생성
beta_spread3 %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 3 / topic 2") +
  coord_flip()


## 4. 감마탐색

# 구축된 모형으로부터 gamma(문서 별 토픽 확률분포) 도출
ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

ap_top_documents <- ap_documents %>%
  group_by(document) %>%
  top_n(3, gamma) %>%
  ungroup() %>%
  arrange(document, -gamma)

ap_top_documents
