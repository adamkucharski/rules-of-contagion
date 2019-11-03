# Model code to accompany "Rules of Contagion"
# Author: AJ Kucharski (2017-)

# Set plot characteristics

par(mar=c(3,3,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")

# Rough line

rough_line <- function(x, y, color) {
  len <- length(x);
  rg <- par("usr");
  yjitter <- (rg[4] - rg[3]) / 1000;
  xjitter <- (rg[2] - rg[1]) / 1000;
  x_mod <- x + rnorm(len) * xjitter;
  y_mod <- y + rnorm(len) * yjitter;
  #lines(x,y, col=color, lwd=2);
  lines(x_mod, y_mod, col=color, lwd=2);
}

# Plot bars
plot_lines <- function(xx,yy,widthf=1,colf="black"){
 
  for(ii in 1:length(xx)){
    polygon(c(xx[ii]-widthf/2,xx[ii]+widthf/2,xx[ii]+widthf/2,xx[ii]-widthf/2),c(0,0,yy[ii],yy[ii]),col=colf)
  }
  
}
