
setwd("C:/R")
library(ggmap)
subway <- read.csv("DataSet.csv")

dat1 <- subset(subway, case < 1,  select = c("xÁÂÇ¥", "yÁÂÇ¥", "case","name","Line"))

map_seoul <- get_map(location = c(lon=126.95, lat=37.55), zoom = 11, maptype = "roadmap")

mm <- ggmap(map_seoul)
mm2 <- mm + geom_point(aes(x=yÁÂÇ¥, y=xÁÂÇ¥, color = as.factor(Line)), 
                       stroke=2, data=dat1)

mm2 + scale_colour_discrete(name=c("Case 1 ¹Ì¸¸")) + labs(x="°æµµ", y="À§µµ")

