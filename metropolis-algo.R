library(sp)
library(rgdal)
library(foreach)
library(ggplot2)
library(raster)
library(RColorBrewer)


set.seed(0)
dim=100
x <- raster(ncol=dim, nrow=dim)
values(x) <- sign(rnorm(ncell(x),mean=0,sd=0.3))
y <- raster(x)
values(y) <- sign(rnorm(ncell(x),mean=0,sd=0.3))
ei <- rnorm(ncell(x))

K=0.8
sig=0.4
G=-0.5


for (j in 1:20) {
  v <- getValuesFocal(x, 1, nrow(x), c(3,3))
  v <- v[, c(2,4,6,8)]
  a <- rowSums(v, na.rm=TRUE)
  values(x) <- sign(K*a+sig*ei+G)

  x_point <- rasterToPoints(x)
  x_df <- data.frame(x_point)
  x_df$cuts=cut(x_df$layer,breaks=c(-2,0.9,1,2))
  
  a <- ggplot(data=x_df)+
    geom_tile(aes(x=x,y=y,fill=cuts)) +
    scale_fill_manual(values = c("red", "green")) +
    coord_equal() +
    theme_bw()+theme(panel.grid.major = element_blank(),
                     legend.title = element_blank(), 
                     axis.title = element_blank(),
                     legend.position = "none")
  
  print(a)
  
  Sys.sleep(5)
}



