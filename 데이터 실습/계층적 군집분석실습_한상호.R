
## 1. 데이터 구조 확인 및 전처리

# 데이터 읽기
setwd('C:/ba')
utilities.df <- read.csv("Utilities.csv")

# Company 벡터 내용 복제하기
row.names(utilities.df) <- utilities.df[,1]

# 기존의 Company 벡터 삭제
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
d <- dist(utilities.df, method = "euclidean")

# 정규화
utilities.df.norm <- sapply(utilities.df, scale) 

# 정규화 시킨 벡터의 행이름 변경
row.names(utilities.df.norm) <- row.names(utilities.df)

## 2. 모형 구축

# compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")

# hcluster를 이용하여 덴드로그램 설정
hc1 <- hclust(d.norm, method = "complete") #완전연결법
plot(hc1, hang = -1, ann = F)
rect.hclust(hc1, k = 6)

# 덴드로그램을 절단하여 멤버쉽으로 할당
memb <- cutree(hc1, k = 6)
memb

## 3. 분석 결과 확인

# 행이름에 할당된 멤버쉽(군집번호)를 부여
# "City" -> "memb:City"
row.names(utilities.df.norm) <- paste(memb, ": ", 
                                      row.names(utilities.df.norm),
                                      sep = "")

# Heatmap 설정
heatmap(as.matrix(utilities.df.norm),
        Colv = NA,
        hclustfun = hclust,
        col=rev(paste("gray", 1:99, sep = "" )))
