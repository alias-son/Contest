
setwd("C:/R")
library(ggmap)
subway <- read.csv("Map.csv")

dat1 <- subset(subway, select = c("x��ǥ", "y��ǥ","name","Line"))

map_seoul <- get_map(location = c(lon=126.95, lat=37.55), zoom = 11,
                     maptype = "roadmap")

mm <- ggmap(map_seoul)
mm2 <- mm + geom_point(aes(x=y��ǥ, y=x��ǥ, color = as.factor(name)), 
                       stroke=2, data=dat1)

mm2  + scale_colour_discrete(name=c("Case 1�̸� �Ǵ� Transfer 1 �̸�"))
                    + labs(x="�浵",y="����")

