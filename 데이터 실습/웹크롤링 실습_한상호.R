library(tidyverse)
library(rvest)

####### 크롤링할 HTML 코드 #######

# ASIN => 아마존이 만든 10자리 고유 식별 번호
# paste0 => 문자열 결합 함수
scrap_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.com/All-New-Echo-Dot-4th-Gen/product-reviews/",ASIN,"/?pageNumber=",page_num)
  doc <- read_html(url_reviews)
  
  ## data-hook 또는 class로 불러오기
  
  ## 파이프 연산자 " %>%, %<% "
  # A %>% B => A ) B, 즉 A가 더 큰 개념
  # 화살표 순서대로 처리하면됨
  # 함수가 많아질수록 더 유용해짐
  
  # Review Date
  doc %>%  
    html_nodes("[data-hook='review-date']")%>%
    html_text() -> Data
  
  # Review Title
  doc %>%
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']")%>%
    html_text() -> Title
  
  # Review Text
  doc %>%
    html_nodes("[class='a-size-base review-text review-text-content']")%>%
    html_text() -> Review
  
  # Number of Stars in Review
  doc %>%
    html_nodes("[data-hook='review-star-rating']")%>%
    html_text() -> Rating
  
  # Return a tibble
  # 열 이름만 적어서 간편하게 Dataframe 생성 가능
  tibble(Data, Title, Review, Rating, Page = page_num)%>%
    return()
}

######## Page별 크롤링 시작 #########

# Product name = All-New-Echo-Dot-4th-Gen
# ASIN = B07XJ8C8F7
review_all <- vector( "list", length = 10)

# 스크랩 시작
# for문으로 위에서 만든 scrap_amazon함수에 ASIN, page_num
# 값을 부여해 10번 반복 후 review_all에 각 리스트로 저장
for (i in 1:10){
  review_all[[i]] <- scrap_amazon(ASIN = "B07XJ8C8F7", page_num = i)
}

# review_all의 10개의 각 리스트를 do.call을 이용하여 
# rbind 시켜 amazon에 리뷰를 저장

amazon <- do.call(rbind, review_all)

######## 텍스트 전처리 #########

# Rating에서 ".0 out of 5 stars" 지우기
# gsub => 텍스트를 지우거나 변경하는 함수
# gsub("변경하고 싶은 텍스트", "어떻게 변경할지", 변경할 변수명)
# 점수만 확인
amazon$Rating <- gsub(".0 out of 5 stars", "", amazon$Rating )

# Data에서 국가 및 날짜 나누기
# strsplit 함수를 사용해 “ on”과 “on ”을 기준으로 문장을 나누기
# Country와 Date 생성
amazon$Country <- strsplit(amazon$Data, " on")[[1]][1]
amazon$Country <- gsub("Reviewed in ", "", amazon$Country)
amazon$Date <- strsplit(amazon$Data, "on ")[[1]][2]

# Data 속성 지우기
amazon <- amazon[,-1]

# Pattern을 이용하여 newline을 의미하는 “\n” 지우고,
# 두 칸 white space 없애기
amazon$Title <- gsub(pattern="\\n", "", amazon$Title)
amazon$Review <- gsub(pattern="\\n", "", amazon$Review)
amazon$Title <- gsub("  ", "", amazon$Title)
amazon$Review <- gsub("  ", "", amazon$Review)

# 속성 중요도에 따라 reorder
amazon <- amazon[ , c(6,5,3,1,2,4)]

# CSV 저장하기
write.csv(amazon, file= "4gen_echo_dot.csv", row.names = 
            FALSE)
