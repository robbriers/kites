# changes to be implemented

# argument for number of replicates (kames 4, white bay sumsheet = 1)
# argument for whether data includes shore profile data
# argument for low water height to scale plot

googlekites<-function(sheet_ref, min_abund=50){
  require(gsheet)
  require(viridis)
  
  # turn off warnings temporarily, as columns do not have headers
  options(warn=-1)
  
  # import data from GoogleSheet
  survey<-gsheet2tbl(sheet_ref)
  
  # remove station row
  survey<-survey[-c(3), ]
  
  # extract taxon names etc (first column), use [[]] notation 
  # to convert to vector
  rn<-survey[[1]]
  
  # remove column from df
  survey<-survey[-1]
  
  # convert to numeric matrix
  survey<-data.matrix(survey)
  
  # transpose
  survey<-t(survey)
  
  # revert to dataframe and add column names
  survey<-as.data.frame(survey)
  names(survey)<-rn
  
  # work out summed abundance by station (col 2)
  # survey$Station
  surveysum<-aggregate(survey, by=list(survey[,2]), sum, na.rm=TRUE)
  
  # remove taxa that are not above min_abund
  surveysum<-surveysum[, -which(colSums(surveysum) < min_abund)]
  
  # remove group and station columns
  surveysum<-surveysum[, -c(1, 3)]
  
  # rescale the abundances for the plotting of the polygons
  # scaled based on the max abundance of any of the taxa being plotted
  
  surveysum[,2:ncol(surveysum)] <- (surveysum[,2:ncol(surveysum)] / max(surveysum[,2:ncol(surveysum)]))/2
  
  # copy data for polygon coordinate creation
  surveysum2 <- surveysum
  
  # reverse order for polygons plotting
  surveysum2 <- surveysum2[order(surveysum2[,1], decreasing = TRUE),]
  
  # make values negative
  surveysum2[,2:ncol(surveysum2)] <- surveysum2[,2:ncol(surveysum2)] * -1
  
  # bind data
  surveysum <- rbind(surveysum,surveysum2)
  
  # work out plot dimensions
  leftedge <- min(surveysum[,1])
  rightedge <- max(surveysum[,1])
  bottomedge <- 0
  topedge <- ncol(surveysum) #-1
  
  # save old margins
  oldMargins <- par()$mar
  par(mar=c(5.1,7,2,2.1))
  
  # make blank plot and add axes/ticks
  plot(c(leftedge,rightedge), c(bottomedge, topedge), type= "n", xlab=names(surveysum)[1], frame.plot=F, yaxt="n", ylab="")
  axis(1, at=surveysum[,1])
  axis(2, labels=names(surveysum)[2:ncol(surveysum)], font=3, at=1:(ncol(surveysum)-1), las=2, lty=0)
  
  # set up viridis colour sequence
  colours<-viridis(ncol(surveysum)-1, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
  
  # loop through columns, creating polygons
  xValues = surveysum[,1]
  for (i in 2:ncol(surveysum)){
    yValues = i + surveysum[,i] - 1
    polygon(xValues,yValues, col=colours[[i-1]], border=colours[[i-1]])
  }
  
  #  par(mar=oldMargins)
  
  # turn warnings back on
  options(warn=0)
}
# end of function


##############################

# profile

profile<-read.csv(file.choose())

profile

plot(profile$Distance, profile$Height, xlab="Distance above low water (m)", ylab="Height above c.d.")
lines(profile$Distance, profile$Height)


# work out plot dimensions
leftedge <- min(profile[,1])
rightedge <- max(profile[,1])
bottomedge <- 0
topedge <- max(profile[,2])+0.5 #-1

# save old margins
oldMargins <- par()$mar
par(mar=c(5.1,7,2,2.1))

# make blank plot and add axes/ticks
plot(c(leftedge,rightedge), c(bottomedge, topedge), type= "n", xlab="Distance from LW (m)", frame.plot=F, ylab="Height above LW (m)")
# yaxt="n"
axis(1, at=profile[,1], labels=profile[,1])
#axis(2, labels=names(surveysum)[2:ncol(surveysum)], font=3, at=1:(ncol(surveysum)-1), las=2, lty=0)
lines(profile[,1],profile[,2])
points(profile[,1],profile[,2], pch=12)
