# kite diagram code
# need to set abundance limit for preprocessing of data - pass into function

survey <- read.csv("survey.csv", check.names = FALSE)

# set type of figure to to allow suitable rescaling
graphType <- "raw"
graphType <- "abundance"

# rescale data for plotting

if (graphType == "raw"){
    survey[,2:ncol(survey)] <- (survey[,2:ncol(survey)] / max(survey[,2:ncol(survey)]))/2
    }
# should be else
if (graphType == "abundance"){
    survey[,2:ncol(survey)] <- survey[,2:ncol(survey)] / 10
    }

# set up the order of stations (or distances)
survey <- survey[order(survey[,1]),]

# copy data
survey2 <- survey

# reverse order for polygons plotting
survey2 <- survey2[order(survey2[,1], decreasing = TRUE),]

# make values negative
survey2[,2:ncol(survey2)] <- survey2[,2:ncol(survey2)] * -1

# bind data
survey <- rbind(survey,survey2)

# work out plot dimensions
leftedge <- min(survey[,1])
rightedge <- max(survey[,1])
bottomedge <- 0
topedge <- ncol(survey)

# saveold margins
oldMargins <- par()$mar
par(mar=c(5.1,7,4.1,2.1))

# make blank plot
plot(c(leftedge,rightedge), c(bottomedge,topedge), type= "n", xlab=names(survey)[1], frame.plot=F, yaxt="n", ylab="")
axis(2, labels=names(survey)[2:ncol(survey)], at=1:(ncol(survey)-1), las=2, lty=0)

# loop through columns, creating polygons - could set up colours separately
xValues = survey[,1]
for (i in 2:ncol(survey)){
  yValues = i + survey[,i] - 1
  polygon(xValues,yValues, col=i)
}

par(mar=oldMargins)
}