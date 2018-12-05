
setwd("C:/R")
library(clValid)
library(plotrix)

#데이터 불러오기 및 전처리
subway <- read.csv("HierarchyClustering.csv")
subway_x <- subway[,-c(1,2,3,4,10,11,12,13,14,15,16,17,18)]
subway_name <-subway[,c(2)]
subway_df <- matrix(subway_x, ncol=5)
subway_x_scaled <- scale(subway_x, center = TRUE, scale = TRUE)

#군집 타당성 지표를 통한 최적의 군집 수 결정
subway_clValid <- clValid(subway_x_scaled, 2:10, clMethods = "hierarchical",
                          validation = c("internal", "stability"))
summary(subway_clValid)

#거리행렬 생성 (스피어만 순위상관계수 사용)
cor_Mat1 <- cor(t(subway_x_scaled), method = "spearman")
dist_subway <- as.dist(1-cor_Mat1)

#계층적 군집화 수행 (평균거리로 상향식 군집화)
hr1 <- hclust(dist_subway, method = "average", members = NULL)

#군집 생성
mycl <- cutree(hr1, k=2)

#덴드로그램 도시
plot(hr1,hang=-1)
rect.hclust(hr1,k=2,border='red')

#군집별 특징 비교 ( 종속변수 Disable , 장애인 이용 수 )
subway_hc <- data.frame(subway_x_scaled, subwayYN = subway[,13],
                        clusterID = as.factor(mycl))

hc_summary <- data.frame()
for (i in 1:(ncol(subway_hc)-1)) {
  hc_summary = rbind(hc_summary, tapply(subway_hc[,i], 
                                        subway_hc$clusterID, mean))
}
colnames(hc_summary) <- paste("cluster", c(1:2))
rownames(hc_summary) <- c(colnames(subway_x), "Disabled")
hc_summary

#군집별 특징 비교 ( Radar Chart 도시 )
par(mfrow = c(1,2))
for ( i in 1:2)
{
plot_title <- paste("CLUSTER", i, sep=" ")
radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
            radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
            line.col = "hotpink", lwd = 2, show.grid.labels=0)
}
dev.off()

# ttest를 이용한 장애인 이용수가 낮은 군집(1)과 높은 군집(2) 비교 

hc_cluster1 <- subway_hc[subway_hc$clusterID == 1, c(1:6)]
hc_cluster2 <- subway_hc[subway_hc$clusterID == 2, c(1:6)]

hc_t_result <- data.frame()
for (i in 1:6) {
  hc_t_result[i,1] <- t.test(hc_cluster1[,i], hc_cluster2[,i],
                             alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster1[,i], hc_cluster2[,i],
                             alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster1[,i], hc_cluster2[,i],
                             alternative = "less")$p.value
}

hc_t_result

hc_cluters <- cbind(subway, subway_hc$clusterID)
write.csv(hc_cluters, "orginal_and_cluster.xls")