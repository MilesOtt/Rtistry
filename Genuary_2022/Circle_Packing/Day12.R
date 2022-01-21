library(tidyverse)

#------ making one circle

make.circles<-function(radius, xplus, yplus, color.here){
  
  x<-sin(seq(0, 2*pi, length=800))*as.numeric(radius) +as.numeric(xplus)
  y<-cos(seq(0, 2*pi, length=800))*as.numeric(radius) +as.numeric(yplus)
  
  p1<-geom_polygon( 
    aes(x=x, y=y), 
    size=1,
    fill=color.here)
  
  
  return(list(p1))
}


# function to check if two circles intersect
intersect<-function(c1, c2){
  r1<-as.numeric(c1[1])
  x1<-as.numeric(c1[2])
  y1<-as.numeric(c1[3])
  
  r2<-as.numeric(c2[1])
  x2<-as.numeric(c2[2])
  y2<-as.numeric(c2[3])
  
  distance = sqrt((x1-x2)^2 +(y1-y2)^2)
  return(distance<(r1+r2))
}

#generate random circles
n<-8000
r<-runif(n/8, .1, .2)
r<-c(r, runif(n/8, .1, .3))
r<-c(r, runif(n*3/4, .005, .1))
x<-runif(n, 0, 5)
y<-runif(n, 0, 5)
color.here<-sample(c("cyan4", 
                     "mediumaquamarine", 
                     "lightseagreen", 
                     "indianred1",
                     "pink",
                     "deeppink4"), n, replace=TRUE)
circle.param<-cbind(r,x,y, color.here)

keep<-rep(TRUE, n)


#remove intersecting circles
for (i in 2:n){
  for (j in 2:i-1){
    
    if(keep[j]==TRUE){
    if (intersect(circle.param[i,], circle.param[j,])) keep[i]<-FALSE
    }
  }
  
}
circle.param<-as.data.frame(circle.param[keep==TRUE, ])


#trying to maximize size of circles------------------

for (i in 1: dim(circle.param)[1]){
  
  indices = (1: dim(circle.param)[1])[-i]
  sum.intersect =0
  circle.parami<-circle.param[i,]
  
  while (sum.intersect ==0){
    circle.parami[1]<-as.numeric(circle.parami[1])+.01
    for(j in indices){
      if (intersect(circle.parami, circle.param[j,]))
        sum.intersect= sum.intersect+1
    }
  }
  if (circle.parami[1]!=as.numeric(circle.param[i,1])){
    circle.parami[1]<-circle.parami[1]-.01
   circle.param[i,1]<-circle.parami[1]
  }
}


#----------------------------------------






#make circle objects to plot
shapes<-circle.param%>%
  purrr::pmap(., 
              ~make.circles(radius=..1, xplus=..2, yplus=..3, color=..4)
  )

g<-ggplot()+ theme(panel.background = element_rect(fill="#e5e5e3", colour='#e5e5e3'),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())


g<-g+shapes
g<-g+  coord_fixed()+
  scale_x_continuous(limits = c(-1,6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1,6), expand = c(0, 0))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

g

sum(keep)


png(filename="Genuary_2022/Circle_Packing/packedcircles12.png", width=6, height=6, res=800, units="in")
g
dev.off()
