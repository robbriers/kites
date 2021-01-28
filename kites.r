# script for reading googlesheet and creating kite diagrams from data
# Rob Briers

# for now working with previous data

test<-read.csv("crustacea.csv", header=TRUE)
#head(test)
#str(test)

# produce sample codes from sheet set number of stations here
test$samples<-rep(1:18, each=4)

# remove original sample codes
test<-test[,-1]

# remove columns with zero values
test2<-test[colSums(!is.na(test))>0]

# produce totals by sample code
totals<-aggregate(test2, list(test2$samples), sum, na.rm=TRUE)

# find abundant taxa, setting minimum abundance to X (30 here)
#abundant<-totals[colSums(totals)>30]
abundant<-totals[,-1]

# remove sample index column
abundant<-subset(abundant, select=-samples)

# set up distances and add to df
distance<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
abundant<-cbind(distance, abundant)


# now for the kite bit
survey<-abundant
# set type of figure
graphType <- "raw"
#graphType <- "abundance"

if (graphType == "raw"){
  survey[,2:ncol(survey)] <- (survey[,2:ncol(survey)] / max(survey[,2:ncol(survey)]))/2
}
if (graphType == "abundance"){
  survey[,2:ncol(survey)] <- survey[,2:ncol(survey)] / 10
}
survey <- survey[order(survey[,1]),]
survey2 <- survey
survey2 <- survey2[order(survey2[,1], decreasing = TRUE),]
survey2[,2:ncol(survey2)] <- survey2[,2:ncol(survey2)] * -1
survey <- rbind(survey,survey2)
leftedge <- min(survey[,1])
rightedge <- max(survey[,1])
bottomedge <- 0
topedge <- ncol(survey)
oldMargins <- par()$mar
par(mar=c(5.1,7,4.1,2.1))
plot(c(leftedge,rightedge), c(bottomedge,topedge), type= "n", xlab="Distance from low water", frame.plot=F, yaxt="n", ylab="")
axis(2, labels=names(survey)[2:ncol(survey)], at=1:(ncol(survey)-1), las=2, lty=0)
xValues = survey[,1]
for (i in 2:ncol(survey)){
  yValues = i + survey[,i] - 1
  polygon(xValues,yValues, col=i)
}
#title(main="Kames Bay 2016")
par(mar=oldMargins)




library(googlesheets)

# register kames data
kames<-gs_url(url, lookup = NULL, visibility = NULL, verbose = TRUE)


# download sheet content
gs_read(kames, ws = 1, range = NULL, literal = TRUE, ..., verbose = TRUE)
