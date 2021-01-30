#' Plot kite diagram
#'
#' @param survey Survey data.
#' @param min_abund Minimum total abundance of taxa to be included on plot.
#' @return Kite diagram plot (base graphics).
#'
#' @importFrom viridisLite viridis
#' @importFrom graphics par plot axis polygon
#' @importFrom stats aggregate
#'
#' @export plot_kite
# kite plotting function
# -----------------------------------------------------------
plot_kite<-function(survey, min_abund=50){

  # turn off warnings temporarily, as columns do not have headers
  options(warn=-1)

  # calc totals
  survey <- aggregate(survey, by=list(c(survey[,1])), FUN=sum, na.rm=TRUE)

  # remove sum of distances
  survey <- survey[, -2]

  # calculate totals for each column (taxon)
  sums <- colSums(survey[2:ncol(survey)])

  # are there any columns < min abundance then remove them
  if (all(sums < min_abund)){
    stop("No species above 'min_abund' specified")
  }
  if (any(sums < min_abund)){
    survey<-cbind(survey[,1], survey[2:ncol(survey)][, -which(colSums(survey[2:ncol(survey)]) < min_abund)])
  }
  # add distance column name on
  colnames(survey)[1] <- "Distance"

    # rescale the abundances for the plotting of the polygons
  # scaled based on the max abundance of any of the taxa being plotted
  survey[,2:ncol(survey)] <- (survey[,2:ncol(survey)] / max(survey[,2:ncol(survey)]))/2

  # copy data for polygon coordinate creation
  survey2 <- survey

  # reverse order for polygons plotting
  survey2 <- survey2[order(survey2[,1], decreasing = TRUE),]

  # make values negative
  survey2[,2:ncol(survey2)] <- survey2[,2:ncol(survey2)] * -1

  # bind data
  surveysum <- rbind(survey,survey2)

  # work out plot dimensions
  leftedge <- min(surveysum[,1])
  rightedge <- max(surveysum[,1])
  bottomedge <- 0
  topedge <- ncol(surveysum) #-1

  # save old margins
  oldMargins <- par()$mar
  # first value was 5.1, 7
  par(mar=c(7.1,11,2,2.1))

  # make blank plot and add axes/ticks
  plot(c(leftedge,rightedge), c(bottomedge, topedge), type= "n", xlab=names(surveysum)[1], frame.plot=F, yaxt="n", ylab="")
  axis(1, at=surveysum[,1])
  axis(2, labels=names(surveysum)[2:ncol(surveysum)], font=3, at=1:(ncol(surveysum)-1), las=2, lty=0)

  # set up viridis colour sequence
  colours<-viridis(ncol(surveysum)-1, alpha = 1, begin = 0, end = 0.7, direction = 1, option = "D")

  # loop through columns, creating polygons
  xValues = surveysum[,1]
  for (i in 2:ncol(surveysum)){
    yValues = i + surveysum[,i] - 1
    polygon(xValues,yValues, col=colours[[i-1]], border=colours[[i-1]])
  }

  par(mar=oldMargins)
  # turn warnings back on
  options(warn=0)
}
# end of function


