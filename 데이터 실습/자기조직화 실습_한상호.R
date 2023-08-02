setwd('C:/ba')

## 1. 데이터 전처리

# 패키지 읽기
library(kohonen)
library(dummies)

# 팔레트 불러오기
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c',
            '#d62728', '#9467bd', '#8c564b', '#e377c2')

# 데이터 불러오기
data <- read.csv('dublin.csv', header = TRUE)

# 분석에 사용할 변수 설정
data_train <- data[ ,c(2,4,5,8)]

# 정규화 후, 매트릭스 만들기
data_train_matrix <- as.matrix(scale(data_train))

## 2. SOM 학습

som_grid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")

# Train the SOM model
# rlen => 학습횟수, alpha => 학습률
# 학습률은 보통 0.5를 기준으로 하고, 
# 횟수마다 설정한 숫자만큼씩 줄어듦
# keep.data => 원 자료를 저장할지 여부
set.seed(31)
som_model <- som(data_train_matrix,
                 grid = som_grid,
                 rlen = 100,
                 alpha = c(0.3,0.01),
                 keep.data = TRUE)

## 3. 시각화

# 색 부여 (필수적X)
source('coolBlueHotRed.R')

# 학습그래프(Training Progress) 파악
# 평평한 부분이 많을수록 학습이 잘 됨 
plot(som_model, type = "changes")

# counts within nodes
# 빨간색이 많으면 dim을 늘려주면 좋음
plot(som_model, type = "counts", main = "Node Counts",
     palette.name = coolBlueHotRed)
     
# map quality
# 파란색이 대부분이므로, Node간 유사도 높음
plot(som_model, type = "quality", main = "Node Quality/Distance",
     palette.name = coolBlueHotRed)

# neighbour distances
# 진한색이 많으므로, 유사도 높음
plot(som_model, type = "dist.neighbours", main = "SOM neighbour dist",
     palette.name = grey.colors)

# code spread
# 각 뉴런에 대한 학습 데이터 가중치 기여율
plot(som_model, type = "codes", main = "code",
     palette.name = coolBlueHotRed)

# plot the heatmap for a variable at scaled / normalised values
# 변수별로 분석
var <- 4 # 어떤 변수로 만들건지 설정

plot(som_model, 
     type = "property", 
     property = getCodes(som_model)[,var], 
     main=colnames(getCodes(som_model))[var], 
     palette.name=coolBlueHotRed)

#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps if a factor or 
# 너무 복잡한 함수라 그냥 소스로 불러옴
source('plotHeatMap.R')

# A menu of all variables should be displayed if variable=0 
# (note on Mac this will required working XQuartz installation.)
plotHeatMap(som_model, data, variable=2)
plotHeatMap(som_model, data, variable=3)
plotHeatMap(som_model, data, variable=4)
plotHeatMap(som_model, data, variable=5)
plotHeatMap(som_model, data, variable=6)
plotHeatMap(som_model, data, variable=7)
plotHeatMap(som_model, data, variable=8)
plotHeatMap(som_model, data, variable=9)
plotHeatMap(som_model, data, variable=10)
plotHeatMap(som_model, data, variable=11)
plotHeatMap(som_model, data, variable=12)
plotHeatMap(som_model, data, variable=13)
plotHeatMap(som_model, data, variable=14)

# 6개의 cluster부여
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

# cluster마다 다른색을 부여해서 HeatMap을 만듦
# cluster별 경계 부여
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], 
     main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)


plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], 
     main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

