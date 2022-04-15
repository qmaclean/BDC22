# Create rink plot function
plot_rink = function(p_object){
  
  require(ggforce)
  require(cowplot)
  require(tidyverse)
  require(png)
  require(grid)
  
  
  #path<-"BigDataCup.jpg"
  #img<-readJPEG(path,native=TRUE)
  
  img <- readPNG("Viz/BigDataCup.png")
  g <- rasterGrob(img, interpolate=TRUE)
  
  upper_outline = data.frame(
    x = c(
      115,
      172 + 28*sin(seq(0,pi/2,length=20)),
      172 + 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  lower_outline = data.frame(
    x = c(
      115,
      100-72 - 28*sin(seq(0,pi/2,length=20)),
      100-72 - 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  p = p_object +
    annotation_custom(g, xmin=85, xmax=115, ymin=35, ymax=50) +
    ## FACEOFF CIRCLES ##
    geom_circle(data = data.frame(x0 = 100, y0 = 42.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "red", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 169, y0 = 20.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "red", inherit.aes = FALSE,fill="white") +
    geom_circle(data = data.frame(x0 = 169, y0 = 64.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "red", inherit.aes = FALSE,fill="white") +
    geom_circle(data = data.frame(x0 = 31, y0 = 64.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "red", inherit.aes = FALSE,fill="white") +
    geom_circle(data = data.frame(x0 = 31, y0 = 20.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "red", inherit.aes = FALSE,fill="white") +
    ## FACEOFF DOTS ## # original all white
    geom_point(inherit.aes = FALSE, aes(y = 42.5, x = 100), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 169), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 169), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 120), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 120), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 31), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 31), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 80), col = "red", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 80), col = "red", size = 1) +
    ## BLUE AND RED LINES ##
    annotate("segment", col = "dark blue",  x = 75, xend = 75, y = 0, yend = 85, lwd = 2,) +
    annotate("segment", col = "red", x = 100, xend = 100, y = 0, yend = 85, lwd = 0.5) +
    annotate("segment", col = "dark blue",  x = 125, xend = 125, y = 0, yend = 85, lwd = 2) +
    ## NET AND GOAL LINE ##
    geom_segment(col = "red", inherit.aes = FALSE, lwd = 0.5, aes(y = 79.25, x = 11, yend = 5.75, xend = 11)) + # originally white
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 7.5, yend = 45.5, xend = 7.5)) + 
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 7.5, yend = 39.5, xend = 11)) +  
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 45.5, x = 7.5, yend = 45.5, xend = 11)) +
    geom_segment(col = "red", inherit.aes = FALSE, lwd = 0.5, aes(y = 5.75, x = 189, yend = 79.25, xend = 189)) + # originally white
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 45.5, xend = 192.5)) + 
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 39.5, xend = 189)) +  
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 45.5, x = 192.5, yend = 45.5, xend = 189)) +
    ## OUTLINE ##
    geom_path(data = upper_outline, aes(x = x, y = y), colour = "light blue", inherit.aes = FALSE, lwd = 0.5) +
    geom_path(data = lower_outline, aes(x = x, y = y), colour = "light blue", inherit.aes = FALSE, lwd = 0.5) +
    ## ADDITIONAL SPECS ##
    scale_x_continuous(expand = c(0, 0), limits = c(100,200)) + scale_y_continuous(expand = c(0,0), limits = c(0,90)) +
    coord_fixed() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill="white"),
      panel.background = element_rect(fill="white"),
      legend.position = "none",
      axis.text.x=element_blank(), #remove x axis labels
      axis.ticks.x=element_blank(), #remove x axis ticks
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank()  #remove y axis ticks
    ) 
  
  return(p)
}
