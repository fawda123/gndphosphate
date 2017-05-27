# get ggplot legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# iscoplot2
iscoplot2 <- function(graph.dat, title) {
  
  # plot PO4
  plot(PO4~DateTime, data=graph.dat, axes=F, ylim=c(0, 0.25), 
       xlab="", ylab="", type="l", lwd = 2.5, col="black", main=title)

  #we just did two lines calling the y-axis because R will automatically
  #make tick marks at certain intervals, and R wants to stop the axis at a tick mark
  #but we want the line to extend above it, so we're making a second axis
  #without tick marks. We're telling it to go from 0-1000, but it will
  #really stop at the max of your data (because that's where the graph stops)
  
  #plot depth
  par(new=T)
  plot(Depth~DateTime, data=graph.dat, axes=F, ylim=c(0,1), xlab="", ylab="", 
       type="l",lty=2, main="",lwd=1.5, col="gray22")

  #plot salinity
  par(new=T)
  plot(Sal~DateTime, data=graph.dat, axes=F, ylim=c(0,25), xlab="", ylab="", 
       type="l",lty=3, main="",lwd=1.5, col="gray22")

  
  #plot x axis
  #make a text string with date/times
  axisdate <- seq(min(graph.dat$DateTime), max(graph.dat$DateTime), by = '5 hours')
  axisdate2 <- format(axisdate, '%m/%d')
  axisdate3 <- format(axisdate, '%H:%M')
  
  #draw the axis
  axis.POSIXct(1,range(graph.dat$DateTime), at=axisdate, tcl = -0.4, labels=F)
  axis(1, at=c(0,50000000000), labels=F)
  
  #add in the text string below the axis
  #par("usr")[3] is the y(min) of the plot or more accurately, the min of the most recent y-axis
  #text(x=(graph.dat$DateTime), par("usr")[3] - 1, srt = 45, adj = 1,labels = axisdate, xpd = TRUE)
  #so I made the y-coordinate an offset that just seems to work
  text(x=axisdate, par("usr")[3]-1.5, labels = axisdate2, xpd = TRUE, cex=1)
  text(x=axisdate, par("usr")[3]-2.8, labels = axisdate3, xpd = TRUE, cex=1)
  
  # backup:
  #   text(x=axisdate, par("usr")[3]-0.005, srt = 45, adj=c(1.125, 1.2), labels = axisdate2, xpd = TRUE, cex=0.9)
  
  #plot the legend
  legend(x = 'topright', inset = -0.04, legend=c(expression("PO"[4]),"Depth","Sal"),lty=c(1,2,3), xpd = TRUE, cex=0.9)
}

# iscoplot3
iscoplot3 <- function(graph.dat, title) {
  
  # plot PO4
  plot(PO4~DateTime, data=graph.dat, axes=F, ylim=c(0, 0.25), 
       xlab="", ylab="", type="l", lwd = 2.5, col="black", main=title)
  axis(2, at=c(0,1000),lwd=1, labels = F)
  axis(2, ylim=c(0,0.25), col="black",lwd=1, tcl = -0.4, cex.axis = 0.95, mgp = c(3, 0.6, 0))
  mtext(2,text=expression("PO"[4]*" (mg P/L)"), line=1.6)
  
  #we just did two lines calling the y-axis because R will automatically
  #make tick marks at certain intervals, and R wants to stop the axis at a tick mark
  #but we want the line to extend above it, so we're making a second axis
  #without tick marks. We're telling it to go from 0-1000, but it will
  #really stop at the max of your data (because that's where the graph stops)
  
  #plot depth
  par(new=T)
  plot(Depth~DateTime, data=graph.dat, axes=F, ylim=c(0,1), xlab="", ylab="", 
       type="l",lty=2, main="",lwd=1.5, col="gray22")
  axis(2, at=c(0,1000),line = 3.3, lwd = 1, labels = F)
  axis(2, ylim=c(0,1),lwd=1,line=3.3, tcl = -0.4, cex.axis = 0.95, mgp = c(3, 0.6, 0))
  mtext(2,text="Depth (m)",line=5.1)
  
  #plot salinity
  par(new=T)
  plot(Sal~DateTime, data=graph.dat, axes=F, ylim=c(0,25), xlab="", ylab="", 
       type="l",lty=3, main="",lwd=1.5, col="gray22")
  axis(2, at=c(0,1000), lwd=1, line = 6.7, labels = F)
  axis(2, ylim=c(0,25),lwd=1,line=6.7, tcl = -0.4, cex.axis = 0.95, mgp = c(3, 0.6, 0))
  mtext(2,text="Salinity",line=8.4)
  
  #plot x axis
  #make a text string with date/times
  axisdate <- seq(min(graph.dat$DateTime), max(graph.dat$DateTime), by = '5 hours')
  axisdate2 <- format(axisdate, '%m/%d')
  axisdate3 <- format(axisdate, '%H:%M')
  
  #draw the axis
  axis.POSIXct(1,range(graph.dat$DateTime), at=axisdate, tcl = -0.4, labels=F)
  axis(1, at=c(0,50000000000), labels=F)
  
  #add in the text string below the axis
  #par("usr")[3] is the y(min) of the plot or more accurately, the min of the most recent y-axis
  #text(x=(graph.dat$DateTime), par("usr")[3] - 1, srt = 45, adj = 1,labels = axisdate, xpd = TRUE)
  #so I made the y-coordinate an offset that just seems to work
  text(x=axisdate, par("usr")[3]-1.5, labels = axisdate2, xpd = TRUE, cex=1)
  text(x=axisdate, par("usr")[3]-2.8, labels = axisdate3, xpd = TRUE, cex=1)
  

}