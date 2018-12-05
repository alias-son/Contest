
setwd("C:/R")
library(clValid)
library(plotrix)

#������ �ҷ����� �� ��ó��
subway <- read.csv("HierarchyClustering.csv")
subway_x <- subway[,-c(1,2,3,4,10,11,12,13,14,15,16,17,18)]
subway_name <-subway[,c(2)]
subway_df <- matrix(subway_x, ncol=5)
subway_x_scaled <- scale(subway_x, center = TRUE, scale = TRUE)

#���� Ÿ�缺 ��ǥ�� ���� ������ ���� �� ����
subway_clValid <- clValid(subway_x_scaled, 2:10, clMethods = "hierarchical",
                          validation = c("internal", "stability"))
summary(subway_clValid)

#�Ÿ���� ���� (���Ǿ ���������� ���)
cor_Mat1 <- cor(t(subway_x_scaled), method = "spearman")
dist_subway <- as.dist(1-cor_Mat1)

#������ ����ȭ ���� (��հŸ��� ����� ����ȭ)
hr1 <- hclust(dist_subway, method = "average", members = NULL)

#���� ����
mycl <- cutree(hr1, k=2)

#����α׷� ����
plot(hr1,hang=-1)
rect.hclust(hr1,k=2,border='red')

#������ Ư¡ �� ( ���Ӻ��� Disable , ����� �̿� �� )
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

#������ Ư¡ �� ( Radar Chart ���� )
par(mfrow = c(1,2))
for ( i in 1:2)
{
plot_title <- paste("CLUSTER", i, sep=" ")
radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
            radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
            line.col = "hotpink", lwd = 2, show.grid.labels=0)
}
dev.off()

# ttest�� �̿��� ����� �̿���� ���� ����(1)�� ���� ����(2) �� 

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