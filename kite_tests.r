X <- read.table(textConnection("Species Depth Total
A 820 6
A 1208 15
A 1750 77
A 2209 41
B 820 11
B 1208 17
B 1750 17
B 2209 1
C 820 0
C 1208 13
C 1750 5
C 2209 4
D 820 32
D 1208 0
D 1750 0
D 2209 0
E 820  0
E 1208 11
E 1750 0
E 2209 0
F 820 0
F 1208 0
F 1750 0
F 2209 6"),header=TRUE)
library(reshape)
X2 <- recast(X,Depth~Species,id.var=1:2)
X3 <- as.matrix(X2[,-1])
rownames(X3) <- X2$Depth
colnames(X3) <- names(X2)[-1]
library(plotrix)
kiteChart(t(X3),xlab="Depth",ylab="Species", timepos=TRUE)

library(plotrix)
x2<-matrix(X$Total,ncol=4,byrow=TRUE)
colnames(x2)<-unique(X$Depth)
rownames(x2)<-unique(X$Species)
kiteChart(x2,xlab="Depth",ylab="Species")
