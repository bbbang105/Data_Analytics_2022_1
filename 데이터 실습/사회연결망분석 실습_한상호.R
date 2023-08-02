install.packages('igraph')
# 라이브러리 로딩
library(igraph)

# 초기화 : 방향성이 없고, edge와 노드가 없는 igraph 생성
g_star <- graph(edges=NULL,n=NULL,directed=FALSE)

# 그래프에 "A" 노드 추가
g_star <- g_star + vertex("A", shape="circle", size=40, 
                          color="yellow")
# 그래프에 "B", "C", "D", "E", "F" 노드 추가
# 색 설정을 안할 경우엔 흰색으로 진행됨
g_star <- g_star + vertices("B", "C", "D", 
                            "E", "F", shape="circle", size=40)

# 그래프에 "A", "B" 간 edge 추가
g_star <- g_star + edge("A", "B")

# 에지 추가 : "A", "C" / "A", "D" / "A", "E" / "A", "F"
g_star <- g_star + edges("A", "C", "A", "D", "A", "E", 
                         "A", "F")

# 스타형 그래프 출력
plot(g_star)

# 네트워크 내의 노드 수
vcount(g_star) 

# 노드 간 연결된 에지의 총수
ecount(g_star)

## Y자 그래프 만들기 
g_Y <- graph(edges=NULL,n=NULL,directed=FALSE)

g_Y <- g_Y + vertices("A", "B", "C", "D", "E", "F", 
                      shape="circle", size=30)

g_Y <- g_Y + edge("A", "B", "A", "C", "A", "D", "D", 
                  "E", "E", "F")
plot(g_Y)

# 원형으로 만들기
g_ring <- graph(edges=NULL,n=NULL,directed=FALSE)

g_ring <- g_ring + vertices("A", "B", "C", "D", "E", 
                            "F", shape="circle", size=30)

g_ring <- g_ring + edge("A", "B", "B", "C", "C", "D", 
                        "D", "E", "E", "F", "F", "A")
plot(g_ring)

## 연결정도중심성 비정규화와 정규화 비교
# 연결정도(g_star 그래프)
degree(g_star, normalized=FALSE)
degree(g_star, normalized=TRUE)

# 이론적 연결정도집중도 분모 최대값
tmax <- centr_degree_tmax(g_star)

# 연결정도집중도
centralization.degree(g_star, normalized=FALSE)$centralization / tmax

# 연결정도(g_Y 그래프)
degree(g_Y, normalized=FALSE)
degree(g_Y, normalized=TRUE)
tmax <- centr_degree_tmax(g_Y)
centralization.degree(g_Y, normalized=FALSE)$centralization / tmax

# 연결정도(g_ring 그래프)
degree(g_ring, normalized=FALSE)
degree(g_ring, normalized=TRUE)
tmax <- centr_degree_tmax(g_ring)
centralization.degree(g_ring, normalized=FALSE)$centralization / tmax

# 근접 (g_star 그래프)
closeness(g_star, normalized=FALSE)
closeness(g_star, normalized=TRUE)
tmax <- centralization.closeness.tmax(g_star)
centralization.closeness(g_star, 
                         normalized=FALSE)$centralization / tmax

# 근접 (g_Y 그래프)
closeness(g_Y, normalized=FALSE)
closeness(g_Y, normalized=TRUE)
tmax <- centralization.closeness.tmax(g_Y)
centralization.closeness(g_Y, 
                         normalized=FALSE)$centralization / tmax

# 근접 (g_ring 그래프)
closeness(g_ring, normalized=FALSE)
closeness(g_ring, normalized=TRUE)
tmax <- centralization.closeness.tmax(g_ring)
centralization.closeness(g_ring, 
                         normalized=FALSE)$centralization / tmax

# 매개(g_star 그래프)
betweenness(g_star, normalized=FALSE)
betweenness(g_star, normalized=TRUE)
tmax <- centralization.betweenness.tmax(g_star)
centralization.betweenness(g_star, 
                           normalized=FALSE)$centralization / tmax

# 매개(g_Y 그래프)
betweenness(g_Y, normalized=FALSE)
betweenness(g_Y, normalized=TRUE)
tmax <- centralization.betweenness.tmax(g_Y)
centralization.betweenness(g_Y, 
                           normalized=FALSE)$centralization / tmax

# 매개(g_ring 그래프)
betweenness(g_ring, normalized=FALSE)
betweenness(g_ring, normalized=TRUE)
tmax <- centralization.betweenness.tmax(g_ring)
centralization.betweenness(g_ring, 
                           normalized=FALSE)$centralization / tmax

# 네트워크 밀도
graph.density(g_star)


#####

## 실습 데이터: 페이스북 사용자 네트워크 분석
## 미국 스탠퍼드 대학의 Social networks 데이터 세트

setwd("C:/ba")

# 라이브러리 불러오기
library(igraph)

# 페이스북 사용자 데이터 세트 읽기
sn <- read.table("facebook_combined.txt", header=F)
head(sn)
tail(sn)

# sn 데이터를 그래프 형식의 데이터 프레임으로 변환
# directed 에 따라 지정
sn.df <- graph.data.frame(sn, directed=FALSE)
plot(sn.df)

# 페이스북 ID 1번과 연결된 사용자들의 데이터셋 연결
sn1 <- subset(sn, sn$V1==1)

# sn1.df 데이터를 그래프 형식의 데이터 프레임으로 변환
sn1.df <- graph.data.frame(sn1, directed=FALSE)

# 연결망 출력
plot(sn1.df)

# 노드 총 갯수 : 4039 
vcount(sn.df)

# 노드 간 연결된 에지 갯수 : 88234
ecount(sn.df)

# 네트워크에 있는 노드 이름
V(sn.df)$name

# 연결정도
degree(sn.df, normalized=TRUE)
tmax <- centralization.degree.tmax(sn.df)
centralization.degree(sn.df, 
                      normalized=FALSE)$centralization / tmax

# 노드 이름 중에서 연결정도 최대인 것
vmax <-V(sn.df)$name[degree(sn.df)==max(degree(sn.df))]

# 107 출력 예상
vmax

# vmax(107)에 해당하는 노드는 1045개의 연결 갖고 있음
degree(sn.df, vmax)

# 연결정도 요약
# 연결 최소 : 1 / 평균 : 43.69 / 최대 : 1045
summary(degree(sn.df))

# 시각화
plot(degree(sn.df), xlab="사용자 번호", ylab="연결 정도", 
     type='h')

# 연결정도에 대한 분포
sn.df.dist <- degree.distribution(sn.df)

plot(sn.df.dist)

# 근접
closeness(sn.df, normalized=TRUE)
tmax <- centralization.closeness.tmax(sn.df)
centralization.closeness(sn.df, 
                         normalized=FALSE)$centralization / tmax

# 매개
betweenness(sn.df, normalized=TRUE)
tmax <- centralization.betweenness.tmax(sn.df)
centralization.betweenness(sn.df, 
                         normalized=FALSE)$centralization / tmax

# 밀도
# 총 연결정도를 연결 가능한 수로 나눈 비율(1%)
graph.density(sn.df)
