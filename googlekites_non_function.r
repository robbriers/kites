# google kites function development

# link to spreadsheet https://docs.google.com/spreadsheets/d/1pv041th4inXsSRTy8afM0gE14dgeR9TvwmJWEnnC6LQ/edit#gid=0

googlekites<-function(sheetref, min_abund){
  require(gsheet)
  require(dplyr)
  require(viridis)
  
  # bring in data from googlesheet
  survey<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1pv041th4inXsSRTy8afM0gE14dgeR9TvwmJWEnnC6LQ/edit#gid=0')
 
  # remove station row
  survey<-survey[-c(3), ]
  
  # extract taxon names etc (first column), use [[]] notation 
  # to convert to vector
  rn<-survey[[1]]

  # remove column from df
  survey2<-survey[-1]
  
  # convert to numeric matrix
  survey3<-data.matrix(survey2)

  # transpose
  survey4<-t(survey3)
  
  # revert to dataframe and add column names
  survey5<-as.data.frame(survey4)
  names(survey5)<-rn
  
  # work out sums using summarise
  surveysum<-aggregate(survey5, by=list(survey5$Station), sum, na.rm=TRUE)

  ########## min abundance here!!!!
  
  min_abund<-70
  
  
  surveysum<-surveysum[, -which(colSums(surveysum) < min_abund)]
  # remove group and station columns
  surveysum<-surveysum[, -c(1, 3)]
  
  # check
  colSums(surveysum)
  
  
  # end up with a df of taxa as columns and summed abundances as rows
  # to pass to kite production function
  
  # which is this part

  # rescale the abundances for the plotting of the polygons
  # scaled based on the max abundance of any of the taxa being plotted
  
  surveysum[,2:ncol(surveysum)] <- (surveysum[,2:ncol(surveysum)] / max(surveysum[,2:ncol(surveysum)]))/2
  
  #surveysum3 <- (surveysum[,2:ncol(surveysum)] / max(surveysum[,2:ncol(surveysum)]))/2
  
#  surveysum
  
  # could order columns in relation to total abundance here, relates to colours
  
  # set up the order of stations (or distances)
  #surveysum <- surveysum[order(surveysum[,2]),]
  
  # copy data
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
  topedge <- ncol(surveysum)-1
  
  # save old margins
  oldMargins <- par()$mar
  par(mar=c(5.1,7,4.1,2.1))
  
  # make blank plot - need to add in y axis label (relative abundance)
  plot(c(leftedge,rightedge), c(bottomedge, topedge), type= "n", xlab=names(surveysum)[1], frame.plot=F, yaxt="n", ylab="")
  axis(1, at=surveysum[,1])
  axis(2, labels=names(surveysum)[2:ncol(surveysum)], font=3, at=1:(ncol(surveysum)-1), las=2, lty=0)
  
  # use viridis colours?
  colours<-viridis(ncol(surveysum)-1, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

  # loop through columns, creating polygons - could set up colours separately
  xValues = surveysum[,1]
  for (i in 2:ncol(surveysum)){
    yValues = i + surveysum[,i] - 1
    polygon(xValues,yValues, col=colours[[i-1]])  ## colours(i)) # was col=i
  }
  
  par(mar=oldMargins)
}
# end of function
