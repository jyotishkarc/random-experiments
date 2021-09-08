library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(scales)
library(maptools)
library(gridExtra)
library(rgeos)


states_shape = readShapeSpatial("IND_adm1.shp")



shapedata = fortify(states_shape, region = "ID_1")
labelpoints = aggregate(cbind(long, lat) ~ id, data=shapedata, 
                        FUN=function(x)mean(range(x)))
ss = merge(shapedata, labelpoints, by="id", all.x=TRUE)


sc = read.csv("statecritical.csv")
attach(sc)

sc = mutate(sc, id = 1:36, class =  as.numeric(Growth.Rate.lamda.t. >=0.1)
                                            + 2*as.numeric(0.09< Growth.Rate.lamda.t. & Growth.Rate.lamda.t. < 0.1)
                                            + 3*as.numeric(0.08< Growth.Rate.lamda.t. & Growth.Rate.lamda.t. <=0.09)
                                            + 4*as.numeric(Growth.Rate.lamda.t. < 0.08)        )



Merged_data = merge(ss, sc, by="id", all.x=TRUE)
Merged_data = Merged_data[order(Merged_data$order), ]

Merged_data = select(Merged_data,"long.x","lat.x","long.y","lat.y","group","id","order","class",everything())



Map =  ggplot(data=Merged_data, mapping=aes(x=long.x,y=lat.x)) +
      geom_polygon(mapping=aes(group=group,fill=class),color="black")+
      geom_text(data=Merged_data ,mapping=aes(x=long.y,y=lat.y,z=id,label=Growth.Rate.lamda.t.),color="yellow",size=2.5)+
      coord_fixed(0.8)+theme(legend.position="none")+labs(x="",y="")+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )




