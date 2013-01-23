
netchart = function(data, grouping, main="", col=NULL, axis.col="gray", label.col="gray", points =TRUE, centers = NULL, pch=16, total.pch = 20, ...) {
  require(grid)
  if(is.null(col)) col = rainbow(max(grouping))
  if(col=="light"||col=="medium"||col=="full"||col=="dark") col=flxColors(n=1:max(grouping), color=col)
  
  p = dim(data)[2] # number of variables. => Number of axis for the net.
  if(is.null(centers)) centers = rep("mean", p)
  a = sin(pi/p)	 # a = The length of one side of the polygon
  r = a/(2*tan(pi/p))	# the radius of the outer circle around the polygon
  phi = 2*pi/p 	 # inner arc 
  
  grid.newpage() # start the drawing
  vp.main = viewport(x=unit(0.,"npc"), y=unit(1.,"npc"), w=unit(1.,"npc"), h=unit(1.,"npc"), just=c("left","top"))
  pushViewport(vp.main) # Main viewport
  vp.header = viewport(x=0, y=1, w=unit(1.,"npc"), h=unit(2,"lines"), just=c("left","top"))
  pushViewport(vp.header) # Viewport for printing the title
  grid.text(main, vp=vp.header, gp=gpar(fontface="bold"), y=unit(1,"lines"), just=c("center","top"))
  upViewport(1)
  vp.plot = viewport(x=0, y=0, w=unit(1.,"npc"), h=unit(1.,"npc")-unit(2,"lines"), just=c("left","bottom"))
  pushViewport(vp.plot) # Viewport for drawing the chart
  for(i in 1:p) { # Draw the axis of the net
    x=unit.c(unit(0.5,"npc"),(unit(0.5,"npc")+unit(r*cos(phi*i),"npc"))) # calculate the x-start- and end-coordinate
    y=unit.c(unit(0.5,"npc"),(unit(0.5,"npc")+unit(r*sin(phi*i),"npc"))) # calculate the y-start- and end-coordinate
    grid.lines(x=x, y=y, vp=vp.plot, gp=gpar(col=axis.col)) # draw the lines and the variable names
    label.x=unit.c(unit(0.5,"npc"),(unit(0.5,"npc")+unit(1.05*r*cos(phi*i),"npc")))
    label.y=unit.c(unit(0.5,"npc"),(unit(0.5,"npc")+unit(1.05*r*sin(phi*i),"npc")))
    grid.text(colnames(data)[i], x=label.x[2], y=label.y[2], rot=phi*i*180/pi-90, gp=gpar(col=label.col), just="centre", vp=vp.plot)
    # Add the overall means
    range = 2*max(max(data[,i]), abs(min(data[,i]))) # The range of a variable is (-max(abs(variable)), max(abs(variable))) => 0 is in the center of the scale
    if(centers[i] == "mean") {
      x_p = mean(data[, i]) # variable mean over all observations in one cluster
    } else {
      x_p = median(data[, i]) # variable median over all observations in one cluster
    }
    r_p = 0.5 * r + r * x_p / range	# calculate the distance from the center of the net and this variable mean
    x = unit(0.5, "npc") + unit(r_p*cos(phi*i), "npc") # x coordinate
    y = unit(0.5, "npc") + unit(r_p*sin(phi*i), "npc") # y coordinate
    grid.points(x = x, y=y, pch=total.pch, gp=gpar(col=axis.col), vp=vp.plot)
  }	
  for(i in 1:max(grouping)) { # Add the variable means for each cluster
    first = NULL # save the first point to connect it to the last one for closing the polygon given through the means
    prev = NULL # save the previous coordinate for connections between the means
    for(j in 1:p) {
      range = 2*max(max(data[,j]), abs(min(data[,j]))) # The range of a variable is (-max(abs(variable)), max(abs(variable))) => 0 is in the center of the scale
      if(centers[j] == "mean") {
        x_p = mean(data[which(grouping==i), j]) # variable mean over all observations in one cluster
      } else {
        x_p = median(data[which(grouping==i), j]) # variable median over all observations in one cluster
      }
      r_p = 0.5 * r + r * x_p / range	# calculate the distance from the center of the net and this variable mean
      x = unit(0.5, "npc") + unit(r_p*cos(phi*j), "npc") # x coordinate
      y = unit(0.5, "npc") + unit(r_p*sin(phi*j), "npc") # y coordinate
      if(is.null(first)) first = unit.c(x,y) # if this is the first mean save the coordinates to first-variable
      if(points) grid.points(x=x, y=y, gp=gpar(col=col[i]), pch=pch, vp=vp.plot) # plot symbols at the variable means
      if(!is.null(prev)) {
        grid.lines(x=unit.c(prev[1],x), y=unit.c(prev[2],y), gp=gpar(col=col[i],lwd=2,...), vp=vp.plot)
      }
      prev=unit.c(x,y)
    }
    grid.lines(x=unit.c(prev[1],first[1]), y=unit.c(prev[2],first[2]), gp=gpar(col=col[i],lwd=2,...), vp=vp.plot)
    # close the polygon for each cluster
  }
  upViewport(1)
}