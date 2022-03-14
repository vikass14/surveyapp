install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("sp")
install.packages("RColorBrewer")
install.packages("rgeos")
install.packages("sf")
install.packages("rgdal")
install.packages("raster")
install.packages("stringr")
install.packages("tidyr")
install.packages("maptools")
library(maptools)
library(tidyr)
library(stringr)
library(raster)
library(sf)
library(rgdal)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
library(rgeos)
library(ggmap)
set.seed(8000)
getwd()
shp <- readOGR("E:/India_State_Shapefile/India_State_Boundary.shp")
plot(shp)
image <- read.csv("E:/export_dataframe.csv")
names(image)
names(image)[names(image) == "State.or.union.territory"] <- "id"
image[image == "Manipur[d]"] <- "Manipur"
image[image == "Tamil Nadu"] <- "Tamilnadu"
image[image == "Telangana"] <- "Telengana"
image[image == "Chhattisgarh"] <- "Chhattishgarh"
image[image == "0"] <- "Very Deficit"
image[image == "50"] <- "Deficit"
image[image == "100"] <- "Sufficient"
names(shp)
View(image)
shp.f <- fortify(shp, region = "Name")
View(shp.f)
#Merge shapefile with csv file#
merge.shp.coef<-merge(shp.f,image, by="id", all.x=TRUE)
View(merge.shp.coef)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]
final.plot[is.na(final.plot)]<-"Very Deficit"
View(final.plot)
cnames <- aggregate(cbind(long, lat) ~ id, data=final.plot, FUN=function(x) mean(range(x)))
india_map <- ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = Ethanol.Production), 
               color = "black", size = 0.25) + 
  coord_map()+
  labs(title="Production of Ethanol 2020-21")+
  xlab('Longitude')+
  ylab('Latitude')+
  geom_text(data=cnames, aes(long, lat, label = id), size=3, fontface="bold")
scale_fill_gradient2(name="image", limits=c(0,100), low = 'yellow',mid ='blue', high = 'green')
india_map + scale_fill_manual(values = c("lightblue", "green", "yellow"))
## Ethanol Prodction for 2025 ##
image$Ethanol.Production[which(image$id == "Assam")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Bihar")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "West Bengal")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Odisha")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Chhattishgarh")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Karnataka")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Uttarakhand")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Himachal Pradesh")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Jharkhand")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Madhya Pradesh")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Haryana")] <- "Sufficient"
image$Ethanol.Production[which(image$id == "Rajasthan")] <- "Deficit"
image$Ethanol.Production[which(image$id == "Jammu and Kashmir")] <- "Deficit"

merge.shp.coef1<-merge(shp.f,image, by="id", all.x=TRUE)
View(merge.shp.coef1)
final.plot1<-merge.shp.coef1[order(merge.shp.coef1$order), ]
final.plot1[is.na(final.plot1)]<-"Very Deficit"
View(final.plot1)
cnames <- aggregate(cbind(long, lat) ~ id, data=final.plot, FUN=function(x) mean(range(x)))
india_map1 <- ggplot() +
  geom_polygon(data = final.plot1,
               aes(x = long, y = lat, group = group, fill = Ethanol.Production), 
               color = "black", size = 0.25) + 
  coord_map()+
  labs(title="Production of Ethanol 2025-26", size = 16, face = "bold")+
  xlab('Longitude')+
  ylab('Latitude')+
  geom_text(data=cnames, aes(long, lat, label = id), size=3, fontface="bold")
india_map1 + scale_fill_manual(values = c("lightblue", "green", "yellow"))
# Default order
india_map1 + scale_fill_discrete(limits = c("Sufficient", "Deficit", "Very Deficit"))
india_map1 + scale_fill_manual(values = c("lightblue", "green", "yellow"))
## Bold and Increase in Title of Chart##
india_map + theme(
  plot.title = element_text(color = "black", size = 12, face = "bold"))
