# Plotting code to accompany "Rules of Contagion"
# Author: AJ Kucharski (2020)

# Chapter 1 ---------------------------------------------------------------

# - - - - - - - - - - - - 
# Chapter 1: Independent happenings
C1_independent <- function(){
  
  # Plot Ross S-curve
  
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(4,4,1,1),las=1)
  
  kk = 0.1 # growth rate 1
  kk2 = 0.05 # growth rate 2
  x0 = 0.01 # initial infectious
  xx = seq(0,35,0.01)
  yy = 1-(exp(-kk*xx))
  yy2 = 1-(exp(-kk2*xx))
  
  plot(xx,100*yy,type="l",xlab="Years", ylab="",ylim=c(0,100),xaxs="i",yaxs="i",yaxt="n",bty="l",lwd=2)
  lines(xx,100*yy2,col=col.list[[3]],lwd=2)
  text(x=15,y=95,labels="10% chance per year",cex=0.8,adj=0,col=col.list[[1]])
  text(x=20,y=55,labels="5% chance per year",cex=0.8,adj=0,col=col.list[[3]])
  
  axis(2, at=seq(0,100,20),labels = sapply(seq(0,100,20),function(x){paste(x,"%",sep="")}),col = "black") 
  title(ylab="People affected", line=3, cex.lab=1)
  
  dev.copy(pdf,"plots/C1_independent.pdf",width=width.main,height=height.main) 
  dev.off()
  
  
  # Load home ownership data and plot
  data_home = read.csv("data/C1/C1_house_ownership_UK.csv",stringsAsFactors = F) %>% data.frame()
  xx = (data_home[1,3:10]) %>% as.numeric() # age since twenty
  data_homeA = data_home[-1,]
  data_1960 = data_homeA[data_homeA$Born.in==1960,3:10] %>% as.numeric()
  data_1970 = data_homeA[data_homeA$Born.in==1970,3:10] %>% as.numeric()
  data_1980 = data_homeA[data_homeA$Born.in==1980,3:10] %>% as.numeric()
  data_1990 = data_homeA[data_homeA$Born.in==1990,3:10] %>% as.numeric()
  
  # Plot data
  plot(xx,data_1960,type="l",xlab="Age",ylab="",ylim=c(0,100),yaxt="n",xaxs="i",yaxs="i",bty="l",lwd=2)
  axis(2, at=seq(0,100,20),labels = sapply(seq(0,100,20),function(x){paste(x,"%",sep="")}),col = "black") 
  title(ylab="Homeowners", line=3, cex.lab=1)
  
  lines(xx,data_1980,col=col.list[[2]],lwd=2)
  lines(xx,data_1990,col=col.list[[3]],lwd=2)
  text(x=35.5,y=45,labels="Born in 1980",cex=0.8,adj=0,col=col.list[[2]])
  text(x=49,y=73,labels="Born in 1960",cex=0.8,adj=0)
  text(x=25.5,y=9,labels="Born in 1990",cex=0.8,adj=0,col=col.list[[3]])
  
  dev.copy(pdf,"plots/C1_independent_homes.pdf",width=width.main,height=height.main) 
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 1: S-curve for adoption
C1_s_curve <- function(){
  
  # Plot Ross S-curve
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(2,4,1,1),las=1)
  
  ll = 1 # carrying capacity
  kk = 0.8 # growth rate
  kk2 = 0.4 # growth rate
  x0 = 0.004 # initial infectious
  xx = seq(0,25,0.01)
  yy = ll/(1+(ll/x0-1)*(exp(-ll*kk*(xx))))
  yy2 = ll/(1+(ll/x0-1)*(exp(-ll*kk2*(xx))))
  
  plot(xx,100*yy,type="l",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,101),xaxs="i",yaxs="i",bty="l",lwd=2)
  lines(xx,100*yy2,lwd=2,col=col.list[[3]])
  
  text(x=9.8,y=85,labels="More contagious",cex=0.8,adj=0,col=col.list[[1]])
  text(x=15,y=55,labels="Less contagious",cex=0.8,adj=0,col=col.list[[3]])
  
  
  title(xlab="Time", line=0.5, cex.lab=1)
  title(ylab="People affected", line=3, cex.lab=1)
  
  axis(2, at=seq(0,100,20),labels = sapply(seq(0,100,20),function(x){paste(x,"%",sep="")}),col = "black") 
  
  dev.copy(pdf,"plots/C1_adoption.pdf",width=width.main,height=height.main) 
  dev.off()

  # Load VCR ownership data and plot
  data_vcr = read.csv("data/C1/C1_VCR_ownership_US.csv",stringsAsFactors = F) %>% data.frame()
  
  plot(data_vcr$year,data_vcr$homes,type="l",yaxt="n",ylab="",xlab="",ylim=c(0,100),xaxs="i",yaxs="i",bty="l",lwd=2)
  #plot(xx,100*yy,type="l",xlab="time",xaxt="n",ylab="% affected",ylim=c(0,100),xaxs="i",yaxs="i",bty="l",lwd=2)
  
  axis(2, at=seq(0,100,20),labels = sapply(seq(0,100,20),function(x){paste(x,"%",sep="")}),col = "black") 
  title(ylab="Households owning a VCR", line=3, cex.lab=1)
  
  
  dev.copy(pdf,"plots/C1_adoption_VCR.pdf",width=width.main,height=height.main) 
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 1: Outbreak with exponential rise
C1_s_curve_exponential_limit <- function(){
  
  # Plot Ross S-curve
  par(mfrow=c(1,1),mar=c(2,4,1,1),mgp=c(2,0.7,0),las=1)
  
  ll = 1 # carrying capacity
  kk = 0.3 # growth rate
  x0 = 0.01 # initial infectious
  xx = seq(0,25,0.01)
  yy = sapply(x0*exp(kk*xx),function(x){min(x,1)})
  
  plot(xx,100*yy,type="l",xlab="",xaxt="n",yaxt="n",ylab="",ylim=c(0,101),xaxs="i",yaxs="i",bty="l",lwd=2)
  axis(2, at=seq(0,100,20),labels = sapply(seq(0,100,20),function(x){paste(x,"%",sep="")}),col = "black") 
  title(ylab="People affected", line=3, cex.lab=1)
  title(xlab="Time", line=0.2, cex.lab=1)
  
  
  
  dev.copy(pdf,"plots/C1_exponential_limit.pdf",width=width.main,height=height.main) 
  dev.off()
  
  
  
}

# - - - - - - - - - - - - 
# Chapter 1: Plague in Bombay

C1_plague_data <- function(){
  
  data_plague = read.csv("data/C1/C1_plague_data.csv",stringsAsFactors = F) %>% data.frame()
  source("R/set_plot.R")
  
  # Define epidemic curve from Kermack & McKendrick
  xx_weeks = 1:length(data_plague$cases)
  xx_weeks_model = seq(1,max(xx_weeks),0.01)
  model_prediction = 890*(sech(0.2*xx_weeks_model-3.4))^2
  
  # Plot simulations
  par(mar=c(3.5,4,1,1),las=1)
  
  plot(xx_weeks,data_plague$cases,pch=19,xlim=c(0,30.5),xaxs="i",yaxs="i",col='black',xlab="Weeks",ylab="",ylim=c(0,1100),bty="l",lwd=2)
  lines(xx_weeks_model,model_prediction)
  
  arrows(14.5,950,15.7,855,angle=30,length=0.07,col=col.list[[2]])
  arrows(22,950,20.3,820,angle=30,length=0.07,col=col.list[[2]])
  #lines(c(14.5,15.7),c(950,855),lty=2)
  #lines(c(22,20.2),c(950,815),lty=2)
  title(ylab="Number of deaths", line=2.5, cex.lab=1)
  
  text(x=13,y=1000,labels="Model",cex=0.8,adj=0)
  text(x=21,y=1000,labels="Real data",cex=0.8,adj=0)
  
  dev.copy(pdf,paste("plots/C1_plague_kermack.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 1: Basic SIR model

C1_SIR_model <- function(){
  
  source("R/functions_basic_sir.R")
  source("R/set_plot.R")
  
  popsize=10000
  dt = 1; time.vals=seq(0,120,1)
  
  # Set initial parameters
  theta = c(beta=NA,gamma=1/5,alpha=0.0,npop1=popsize,npop2=popsize,
            rep=1) #
  theta[["beta"]]=2*theta[["gamma"]]
  
  theta_init = c(n_init=theta[["npop1"]], s_init=NA,
                 i1_init=1,
                 r_init=0,
                 n2_init=theta[["npop2"]], s2_init=NA,i2_init=0,r2_init=0)
  theta_init[["s_init"]]= theta_init[["n_init"]]-theta_init[["i1_init"]]-theta_init[["r_init"]]
  
  
  output_sim = Run_simulation(dt,theta,theta_init,time.vals)
  
  # Plot simulations
  par(mar=c(3.5,4,1,1),las=1)
  plot(time.vals,output_sim$S_trace,type="l",xaxs="i",yaxs="i",col=col.list[[2]],xlab="Days",ylab="",ylim=c(0,1.05*theta_init[["n_init"]]),bty="l",lwd=2)
  lines(time.vals,output_sim$R_trace,col=col.list[[3]],lwd=2)
  lines(time.vals,output_sim$I_trace,col=col.list[[1]],lwd=2)
  text(x=60,y=3000,labels="Susceptible",cex=0.8,adj=0,col=col.list[[1]])
  text(x=60,y=1100,labels="Infectious",cex=0.8,adj=0)
  text(x=60,y=8500,labels="Recovered",cex=0.8,adj=0,col=col.list[[2]])
  
  title(ylab="Number of people", line=3, cex.lab=1)
  
  # Outbreak size
  print(tail(output_sim$R_trace,1)/theta_init[["n_init"]])
  
  dev.copy(pdf,paste("plots/C1_SIR_flu.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
}


# - - - - - - - - - 
# Chapter 1: 

C1_zika_data <- function(){
  
  gbs.data = read_csv("data/C1/C1_Zika_data_FP_GBS.csv")
  timeL = length(gbs.data$date)
  zika.data = gbs.data$ZIKV #*gbs.data$total_sites/gbs.data$site_reporting
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3.5,4,1,4),las=1)
  
  plot(gbs.data$date,zika.data,lwd=2,xlab="",xaxt="n",yaxs="i",ylab="",ylim=c(0,1200),bty="l",type="l")
  
  text(x=min(gbs.data$date)+40,y=260,labels="GBS",cex=0.8,adj=0)
  text(x=min(gbs.data$date)+25,y=750,labels="Zika",cex=0.8,adj=0)
  axis(1, at=as.Date(c("2013-11-01","2014-01-01","2014-03-01")),labels = c("November","January","March"),col = "black") 
  
  title(ylab="Reported Zika cases", line=3, cex.lab=1)
  
  par(new=TRUE)
  plot(gbs.data$date,gbs.data$GBS,ylim=c(0,20),yaxs="i",lwd=2,type="l",xaxt="n",bty="l",yaxt="n",xlab="",ylab="",col=col.list[[3]])
  axis(4,col=col.list[[2]],col.axis=col.list[[2]])
  par(las=0)
  mtext("GBS cases", side=4, line=2,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C1_Zika.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  
}

# Chapter 2 ---------------------------------------------------------------

# - - - - - - - - - - - - 
# Chapter 2: Erdos graph
C2_erdos_renyi <- function(){
  
  par(mfrow=c(1,2),mar=c(1,1,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")
  
  set.seed(15)
  g2 = erdos.renyi.game(n=40,0.1)
  coord_g2 = layout.fruchterman.reingold(g2)
  
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=6,vertex.color=network.color,edge.arrow.size=0.5,edge.width=1)
  title("Fully connected", adj=0.5,cex.main=1)
  
  set.seed(12)
  g2 = erdos.renyi.game(n=50,0.04)
  coord_g2 = layout.fruchterman.reingold(g2)
  
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=6,vertex.color=network.color,edge.arrow.size=0.5,edge.width=1)
  title("Broken network", adj=0.5,cex.main=1)
  
  dev.copy(pdf,paste("plots/C2_erdos_network.pdf",sep=""),width=width.main,height=height.main,useDingbats=F)
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 2: R0 plot
C2_generations <- function(){
  
  par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")
  
  # Plot R0
  network_data = read_csv("data/C2/R0_network.csv")
  network_coord = read_csv("data/C2/R0_network_coords.csv")
  set.seed(1)
  
  g2 = graph_from_edgelist(cbind(network_data$Node1,network_data$Node2))
  coord_g2 = cbind(network_coord$Col,network_coord$Row)
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=10,vertex.color=network.color,edge.arrow.size=0.5,edge.width=1)
  
  dev.copy(pdf,paste("plots/C2_generation_R0_2.pdf",sep=""),width=height.main,height=height.main,useDingbats=F)
  dev.off()
  
  # Compare effective R
  
  par(mfrow=c(1,2),mar=c(1,1,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")
  
  # Plot R without vaccination
  network_data = read_csv("data/C2/R0_vaccine.csv")
  network_coord = read_csv("data/C2/R0_vaccine_coords.csv")
  set.seed(1)
  
  g2 = graph_from_edgelist(cbind(network_data$Node1,network_data$Node2))
  coord_g2 = cbind(network_coord$Col,network_coord$Row)
  #coord_g2 = layout.fruchterman.reingold(g2)
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=10,vertex.color=network.color,edge.arrow.size=0.5,edge.width=1)
  title("Without vaccination", adj=0.5,cex.main=1)
  
  # Plot with vaccination
  g2 = graph_from_edgelist(cbind(network_data$Node1[1],network_data$Node2[1]))
  coord_g2 = cbind(network_coord$Col,network_coord$Row)
  
  col_g2 = rep(network.color,6); col_g2[3:6] = "white"
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=10,vertex.color=col_g2,edge.arrow.size=0.5,edge.width=1)
  title("With vaccination", adj=0.5,cex.main=1)
  
  dev.copy(pdf,paste("plots/C2_vaccination.pdf",sep=""),width=width.main+1,height=height.main,useDingbats=F)
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 2: South Sea bubble
C2_south_sea <- function(){
  
  data_south_sea = read_csv("data/C2/south_sea_company.csv") 
  data_south_sea$GregorianDate = data_south_sea$GregorianDate -365*300 + 2
  
  # Omit NA for plotting
  data_south_sea = data_south_sea[!is.na(data_south_sea$SouthSeaCompany),]
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3.5,4.5,1,1),las=1)
  
  plot(data_south_sea$GregorianDate,data_south_sea$SouthSeaCompany,lwd=2,xlab="",yaxt="n",xaxs="i",yaxs="i",ylab="",xlim=as.Date(c("1720-02-01","1720-12-01")),ylim=c(0,1000),bty="l",type="l")
  #axis(1, at=as.Date(c("1994-04-01","1994-05-01","1994-06-01")),labels = c("April","May","June"),col = "black") 
  
  title(ylab="Share price", line=3.5, cex.lab=1)
  axis(2, at=seq(0,1000,200),labels = sapply(seq(0,1000,200),function(x){paste("£",x,sep="")}),col = "black") 
  
  
  dev.copy(pdf,paste("plots/C2_south_sea.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 2: Common source outbreak
C2_common_source <- function(){
  
  data_typhoid = read_csv("data/C2/C2_typhoid.csv")
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3,3.5,1,1),las=1)
  
  #plot(data_typhoid$day,data_typhoid$cases,lwd=2,xlab="Day",xaxs="i",yaxs="i",col="white",ylab="Cases",xlim=c(-0.5,15.5),ylim=c(0,10),bty="l",type="l",main="")
  plot(data_typhoid$date,data_typhoid$cases,lwd=2,xlab="",xaxt="n",xaxs="i",yaxs="i",col="white",ylab="Cases",xlim=as.Date(c("1916-05-12","1916-05-30")),ylim=c(0,10),bty="l",type="l",main="")
  
  plot_lines(data_typhoid$date,data_typhoid$cases,widthf = 0.7)
  rug(x = 0:17 + min(data_typhoid$date), ticksize = -0.03, side = 1)
  axis(1, at=as.Date(c("1916-05-13","1916-05-20","1916-05-27")),labels = c("13th May","20th May","27th May"),col = "black") 
  
  line_date1 = as.Date("1916-05-13")
  lines(c(line_date1,line_date1),c(0,8),lty=1,col=col.list[[3]])
  text(x=line_date1,y=9,labels="Picnic",cex=0.8,adj=0.05,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C2_typhoid.pdf",sep=""),width=width.main,height=height.wide)
  dev.off()
  
}

# Chapter 3 ---------------------------------------------------------------

# - - - - - - - - - - - - 
# Chapter 3: 2009 pandemic

C3_pandemic_2009 <- function(){
  
  data_pandemic = read_csv("data/C3/pandemic_2009.csv")
  
  # Plot cases
  
  # Plot simulations
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(2.5,4,1,1),las=1)
  
  xxD = as.Date("2009-06-01")
  
  # Timeseries
  plot(data_pandemic$date,data_pandemic$cases_HPA/1000,type="l",xaxs="i",yaxs="i",col='black',xlim=as.Date(c("2009-03-28","2010-02-01")),xlab="",ylab="",ylim=c(0,1e2),bty="l",lwd=2)
  title(ylab="Cases (thousands)", line=2.5, cex.lab=1)
  
  line_date1 = as.Date("2009-04-13")
  #lines(c(line_date1,line_date1),c(0,10),lty=1,col=col.list[[2]])
  arrows(line_date1,10,line_date1,0,angle=30,length=0.07,col=col.list[[2]])
  
  text(x=as.Date("2009-04-06"),y=15,labels="Spark",cex=0.8,adj=0,col=col.list[[2]])
  text(x=-1+xxD,y=20,labels="Growth",cex=0.8,adj=0,col=col.list[[2]])
  text(x=35+xxD,y=83,labels="Peak",cex=0.8,adj=0,col=col.list[[2]])
  text(x=89+xxD,y=30,labels="Growth",cex=0.8,adj=0,col=col.list[[2]])
  text(x=57+xxD,y=50,labels="Decline",cex=0.8,adj=0,col=col.list[[2]])
  text(x=135+xxD,y=80,labels="Peak",cex=0.8,adj=0,col=col.list[[2]])
  text(x=180+xxD,y=25,labels="Decline",cex=0.8,adj=0,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C0_pandemic.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  source("R/set_plot.R")
  par(mar=c(2.5,4,1,1),las=1)
  
  # Plot data and terms
  term_date = as.Date(c("2009-07-17","2009-09-03"))
  term_date2 = as.Date(c("2009-10-26","2009-11-02"))
  cgrey = 0.85
  plot(data_pandemic$date,data_pandemic$cases_HPA/1000,pch=19,col="white",xlim=as.Date(c("2009-05-28","2009-12-24")),xaxs="i",yaxs="i",xlab="",ylab="",ylim=c(0,1e2),bty="l",lwd=2)
  
  polygon(x=c(term_date,rev(term_date)),y=c(0,0,90,90),col=rgb(cgrey,cgrey,cgrey),lty=0)
  polygon(x=c(term_date2,rev(term_date2)),y=c(0,0,90,90),col=rgb(cgrey,cgrey,cgrey),lty=0)
  lines(data_pandemic$date,data_pandemic$cases_HPA/1000,lwd=2)

  title(ylab="Cases (thousands)", line=2.5, cex.lab=1)

  text(x=term_date[1],y=94,labels="Summer holidays",cex=0.8,adj=0,col=col.list[[2]])
  text(x=term_date2[1],y=94,labels="Half term",cex=0.8,adj=0,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C3_pandemic_model.pdf",sep=""),width=width.main,height=height.main)
  dev.off()

  
}


# Alternative plos vs COVID

C3_pandemic_vs_covid <- function(){
  
  # Load data
  library(covidregionaldata)
  
  #all_countries <- get_national_data()  # SLOW function
  
  all_uk <- all_countries %>% filter(country == "United Kingdom")
  
  #all_uk2 <- all_uk %>% filter(date<=as.Date("2021-07-18"))
  #plot(all_uk2$date,all_uk2$cases_new,xlim=as.Date(c("2021-06-01","2021-07-18")),xlab="",ylab="cases")
  
  data_pandemic = read_csv("data/C3/pandemic_2009.csv")
  
  # Plot cases
  
  # Plot simulations
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(2.5,4,1,1),las=1)
  
  xxD = as.Date("2009-06-01")
  
  # Timeseries
  plot(data_pandemic$date,data_pandemic$cases_HPA/1000,type="l",xaxs="i",yaxs="i",col='black',xlim=as.Date(c("2009-02-28","2010-02-01")),xlab="",ylab="",ylim=c(0,1e2),bty="l",lwd=2)
  title(ylab="Cases (thousands)", line=2.5, cex.lab=1)
  
  line_date1 = as.Date("2009-04-13")
  #lines(c(line_date1,line_date1),c(0,10),lty=1,col=col.list[[2]])
  #arrows(line_date1,10,line_date1,0,angle=30,length=0.07,col=col.list[[2]])
  
  # text(x=as.Date("2009-04-06"),y=15,labels="Spark",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=-1+xxD,y=20,labels="Growth",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=35+xxD,y=83,labels="Peak",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=89+xxD,y=30,labels="Growth",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=57+xxD,y=50,labels="Decline",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=135+xxD,y=80,labels="Peak",cex=0.8,adj=0,col=col.list[[2]])
  # text(x=180+xxD,y=25,labels="Decline",cex=0.8,adj=0,col=col.list[[2]])
  
  #dev.copy(pdf,paste("plots/C0_pandemic.pdf",sep=""),width=width.main,height=height.main)
  dev.copy(png,paste0("plots/Extra_comparison.png"),units="cm",width=width.main*3,height=height.main*3,res=200)
  dev.off()
  
  source("R/set_plot.R")
  par(mar=c(2.5,4,1,1),las=1)
  
  # Plot data and terms
  term_date = as.Date(c("2009-07-17","2009-09-03"))
  term_date2 = as.Date(c("2009-10-26","2009-11-02"))
  cgrey = 0.85
  plot(data_pandemic$date,data_pandemic$cases_HPA/1000,pch=19,col="white",xlim=as.Date(c("2009-05-28","2009-12-24")),xaxs="i",yaxs="i",xlab="",ylab="",ylim=c(0,1e2),bty="l",lwd=2)
  
  polygon(x=c(term_date,rev(term_date)),y=c(0,0,90,90),col=rgb(cgrey,cgrey,cgrey),lty=0)
  polygon(x=c(term_date2,rev(term_date2)),y=c(0,0,90,90),col=rgb(cgrey,cgrey,cgrey),lty=0)
  lines(data_pandemic$date,data_pandemic$cases_HPA/1000,lwd=2)
  
  title(ylab="Cases (thousands)", line=2.5, cex.lab=1)
  
  text(x=term_date[1],y=94,labels="Summer holidays",cex=0.8,adj=0,col=col.list[[2]])
  text(x=term_date2[1],y=94,labels="Half term",cex=0.8,adj=0,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C3_pandemic_model.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  
}


# Chapter 4 ---------------------------------------------------------------


# - - - - - - - - - - - - 
# Chapter 4: Crimea deaths
C4_crimea <- function(){
  
  library(HistData)
  data(Nightingale)
  
  data_wound_disease = Nightingale[Nightingale$Date<as.Date("1855-01-01"),c("Disease","Wounds")]
  
  tally_cause = c(disease = sum(data_wound_disease$Disease), wounds = sum(data_wound_disease$Wounds))
  
  tally_cause["disease"]/tally_cause["wounds"]
  
}  

# - - - - - - - - - - - - 
# Function to convert cluster data to MLE estimates

mle_estimate <- function(network_data){
  
  # Log probability observe chain of size L - equation 9
  prob_chain <- function(LL,RR,kk) {
    p = lgamma(kk*LL+LL-1) - lgamma(kk*LL) - lgamma(LL+1) + (LL-1)*log(RR/kk) - (kk*LL+LL-1)*log(1+RR/kk) 
    return(p)
  }
  
  mean_size = sum(network_data$Size*network_data$Number/sum(network_data$Number))
  RR_calc = 1-1/mean_size
  
  # Likelihood function
  likelihood_NB <- function(RR,kk) {
    ll = network_data$Number*prob_chain(network_data$Size,RR,kk)
    return(-sum(ll))
  }
  
  # Estimate negative binomial
  kk_val=seq(0.001,1,0.001)
  kk_store = NULL
  
  for(ii in 1:length(kk_val)){
    kk_store = c(kk_store,likelihood_NB(RR_calc,kk_val[ii]))
  }
  
  kk_mle = kk_val[kk_store==min(kk_store)]
  
  c(RR = RR_calc,kk = kk_mle,mean = mean_size)
  
  
}



# - - - - - - - - - - - - 
# Chapter 4: Gun transmission clusters, Chicago

C4_gun_chain_distribution <- function(){
  
  # Gun violence
  network_data = read_csv("data/C4/Gun_cluster_size.csv")
  
  # Fit negative binomial model to get R and dispersion parameter
  print(mle_estimate(network_data)) 
  
  # Simulate secondary cases for gun violence
  nn_sim = 1e4
  simulate_secondary = rnbinom(nn_sim,size=0.096,mu=0.63)
  
  total_80 = 0.8*sum(simulate_secondary) # 80% of cases
  Gun_80 = sum(cumsum(sort(simulate_secondary,decreasing = T )) <= total_80)/nn_sim # What proportion cause them?
  
  print(paste0("80% contagion generated by ",100*Gun_80,"% events")) # proportion from top 50%
  
}

# - - - - - - - - - - - - 
# Chapter 4: Gun simulated network
C4_gun_network_simulated <- function(){
  
  par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")
  
  set.seed(1)
  ncases = c(1:50) # initial chains
  storeNet = NULL
  
  # WHILE
  while(!is.null(ncases) ){
    
    newcases = sapply(runif(ncases),function(x){ rnbinom(1,size=0.096,mu=0.63) })
    newCases0 = NULL
    maxNew = max(ncases)
    
    for(ii in 1:length(ncases)){
      if(newcases[ii] > 0){
        
        if(ii==1){
          newcaseID = max(ncases) + c(1:sum(newcases[ii])) # new case IDs
        }else{
          newcaseID = maxNew + c(1:sum(newcases[ii])) # new case IDs
        }
        infector = rep(ncases[ii],newcases[ii])
        storeNet = rbind(storeNet,cbind(infector,newcaseID) ) # Store network
        newCases0 = c(newCases0,newcaseID) # store new IDs
        maxNew = max(newCases0)
        
      }
    }
    
    ncases = newCases0
    
  }
  
  storeNet
  
  g2 = graph_from_edgelist(storeNet)
  coord_g2 = layout.fruchterman.reingold(g2) #cbind(network_coord$Col,network_coord$Row)
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=4,vertex.color=network.color,edge.arrow.size=0.2,edge.width=1)

  dev.copy(pdf,paste("plots/C4_gun_simulate.pdf",sep=""),width=width.main,height=width.main, useDingbats=F)
  dev.off()
  
}


# - - - - - - - - - - - - 
# Chapter 4: Cholera Snow time series
C4_cholera_timeseries <- function(){
  
  cholera.data = cholera::timeSeries()$data
  timeL = length(cholera.data$date)
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3.5,4,1,1),las=1)
  
  # Set up axes
  date_seq = seq(20,31,3); date_seq2 = seq(1,30,3)
  at_2 = c(as.Date("1854-08-01")+date_seq-1,as.Date("1854-09-01")+date_seq2-1)
  at_Lab = c(date_seq,date_seq2)
  plot(cholera.data$date,cholera.data$deaths,col="white",xlab="",xaxs="i",xaxt="n",yaxs="i",ylab="",ylim=c(0,130),bty="l",type="l")
  
  plot_lines(cholera.data$date,cholera.data$deaths)
  
  axis(1, at= at_2 ,labels = at_Lab,col = "black") 
  
  title(ylab="Daily cholera deaths", line=2.5, cex.lab=1)
  title(xlab="September", line=2, cex.lab=1)
  
  line_date1 = as.Date("1854-09-08")+0.5
  lines(c(line_date1,line_date1),c(0,100),lty=1,col=col.list[[3]])
  text(x=line_date1,y=110,labels="Handle removed",cex=0.8,adj=0.05,col=col.list[[2]])
  
  dev.copy(pdf,paste("plots/C4_cholera_time_series.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  
}

# - - - - - - - - - - - - 
# Chapter 4: Diphtheria delays
C4_diphtheria <- function(){
  
  data.daily1 = read_csv("data/C4/data.daily_2017-12-09.csv")
  data.daily2 = read_csv("data/C4/data.daily_2017-12-19.csv")
  data.daily3 = read_csv("data/C4/data.daily_2018-01-08.csv")
  
  source("R/set_plot.R")
  par(mfrow=c(1,1))
  plot(data.daily1$date_value,data.daily1$incidence,type="l",xaxt="n",bty="l",xlim=c(as.Date("2017-11-08"),as.Date("2018-01-11")),ylim=c(0,160),yaxs="i",ylab="Daily cases",col="white",xlab="")
  axis(1, at=as.Date(c("2017-11-15","2017-12-01","2017-12-15","2018-01-01")),labels = c("15th Nov","1st Dec","15th Dec","1st Jan"),col = "black") 
  
  lines(data.daily1$date_value,data.daily1$incidence,lwd=2,col=col.list[[3]])
  lines(data.daily2$date_value,data.daily2$incidence,lwd=2,col=col.list[[2]])
  lines(data.daily3$date_value,data.daily3$incidence,lwd=2,col=col.list[[1]])
  
  text(x = min(data.daily1$date_value)+31.5,y=15,labels="9th Dec",cex=0.8,adj=0,col=col.list[[3]])
  text(x = min(data.daily1$date_value)+41.5,y=15,labels="19th Dec",cex=0.8,adj=0,col=col.list[[2]])
  text(x = min(data.daily1$date_value)+58,y=15,labels="8th Jan",cex=0.8,adj=0,col=col.list[[1]])
  
  dev.copy(pdf,paste("plots/C4_diphtheria.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
}

# - - - - - - - - - - - - 
# Chapter 4: Ebola 1976
C4_ebola_1976 <- function(){
  
  ebola_data = read.csv("data/C4/ebola_1976.csv",stringsAsFactors = F) %>% data.frame()
  ebola_data$disease_onset = as.Date(ebola_data$disease_onset)
  ebola_data$disease_ended = as.Date(ebola_data$disease_ended)
  
  ebola_data = ebola_data[!is.na(ebola_data$disease_onset),] # Only people with dates
  x.dates = seq(as.Date("1976-08-15"),as.Date("1976-11-07"),7)
  
  #Lag between report and death
  mean(-(ebola_data$disease_onset - ebola_data$disease_ended)[ebola_data$status=="died"])
  
  yy.onset = NULL
  yy.deaths = NULL
  
  for(ii in 1:(length(x.dates)-1)){
    yy.onset = c(yy.onset, length(ebola_data[ebola_data$disease_onset>= x.dates[ii] & ebola_data$disease_onset< x.dates[ii+1],"disease_onset"]))
    yy.deaths = c(yy.deaths,sum(ebola_data[ebola_data$disease_ended>= x.dates[ii] & ebola_data$disease_ended< x.dates[ii+1],"status"]=="died"))
    
  }
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(3.5,4.5,1,1),las=1)
  plot(x.dates[-length(x.dates)],yy.onset,type="l",ylab="",ylim=c(0,80),xlab="Week of outbreak",lwd=2, col = col.list[[1]],xaxt="n",yaxs="i",bty="l")
  lines(x.dates[-length(x.dates)],yy.deaths,col = col.list[[3]],lwd=2)
  
  axis(1, at=x.dates,labels = seq(1,length(x.dates),1),col = "black") 
  
  text(x=min(x.dates) + 8,y=60,labels="New Ebola cases",cex=0.9,adj=0)
  text(x=min(x.dates) + 47,y=55,labels="New Ebola deaths",cex=0.9,adj=0,col=col.list[[2]])
  
  
  title(ylab="Number reported each week", line=2, cex.lab=1)
  
  dev.copy(pdf,paste("plots/C4_ebola_1976.pdf",sep=""),width=width.main,height=height.main)
  dev.off()
  
  
}


# Chapter 5 ---------------------------------------------------------------

# - - - - - - - - - - - - 
# Chapter 5: Higgs network
C5_higgs_network <- function(){
  
  par(mfrow=c(1,1),mar=c(1,1,1,1),mgp=c(2,0.7,0),las=0,family = "Palatino")
  
  # Gun violence
  network_data = read_delim("data/C5/higgs_activity_time.txt",delim=" ")
  names(network_data) = c("user1","user2","time","type")
  network_data = network_data[order(network_data$time),] # sort by time
  network_data = network_data[network_data$type=="RT",]
  
  # Note 1: the direction of links depends on the application, in general. For instance, if one is interested in building a network of how information flows, then the direction of RT should be reversed when used in the analysis. Nevertheless, the choice is left to the researcher and his/her own interpretation of the data, whereas we just provide the observed actions, i.e., who retweets/mentions/replies/follows whom.
  # Note 2: users mentioned in retweeted tweets are considered as mentions. For instance, if @A retweets the tweet “hello @C @D" sent by @B, then the following links are created: @A @B timeX RT, @A @C timeX MT, @A @D timeX MT, because @C and @D can be notified that they have been mentioned in a retweet. Similarly in the case of a reply. If for some reason the researcher does not agree with this choice, he/she can easily identify this type of links and remove the mentions, for instance. 

  set.seed(2)
  
  network_data2 = network_data[1:800,]
  
  xx = cbind(network_data2$user2,network_data2$user1)
  
  # Transform to 1:n indices
  xx2 = xx; dim(xx2)=NULL; xx2_unique = unique(xx2)
  xx_base = match(xx,xx2_unique); xx_base = matrix(xx_base,ncol=2)
  
  g2 = graph_from_edgelist(xx_base)
  g2 = igraph::simplify(g2, remove.multiple = TRUE, remove.loops = TRUE)
  #coord_g2 = cbind(network_coord$Row,network_coord$Col)
  coord_g2 = layout.fruchterman.reingold(g2)
  #coord_g2 = layout_with_lgl(g2)
  plot(g2,layout = coord_g2,vertex.label=NA,vertex.size=2,vertex.color=network.color,edge.arrow.size=0,edge.width=1)
  
  dev.copy(pdf,paste("plots/C5_higgs.pdf",sep=""),width=width.main,height=width.main,useDingbats=F)
  dev.off()

}

# Chapter 5: YouTube dynamics
C5_youtube <- function(){
  
  data_video_AK = read.csv("data/C5/C5_youtube_kucharski.csv",stringsAsFactors = F) %>% data.frame()
  data_video_AK$date = as.Date(data_video_AK$date)
  
  # Count first year
  data_video_AK[data_video_AK$date<(min(data_video_AK$date)+365),"views"] %>% sum()
  
  # Get peak period
  peak_day = which(data_video_AK$views==max(data_video_AK$views))

  # Google views 2018-04-28: 10,723 views
  
  # Plot data
  source("R/set_plot.R")
  par(mfrow=c(1,1),mar=c(2.5,4,1,1),las=1)
  
  plot(data_video_AK$date,data_video_AK$views,lwd=0,xlab="",xaxs="i",xaxt="n",yaxs="i",yaxt="n",ylab="",xlim=c(min(data_video_AK$date)-10,max(data_video_AK$date)),ylim=c(0,15500),bty="l",type="l")
  axis(2, at=seq(0,15000,5000),labels = seq(0,15000,5000),col = "black") 
  axis(1, at=as.Date(c("2016-01-01","2017-01-01","2018-01-01")),labels = c("2016","2017","2018"),col = "black") 
  title(ylab="daily views", line=3, cex.lab=1)
  
  line_date1 = as.Date("2016-08-02")
  lines(c(line_date1,line_date1),c(0,13500),lty=1,col=col.list[[2]])
  text(x=line_date1,y=14500,labels="First posted",cex=0.8,adj=0.05)
  
  line_date2 = as.Date("2017-08-20")
  lines(c(line_date2,line_date2),c(0,13500),lty=1,col=col.list[[2]])
  text(x=line_date2,y=14500,labels="Appears on homepage",cex=0.8,adj=0.05)
  
  lines(data_video_AK$date,data_video_AK$views,lwd=2)
  
  dev.copy(pdf,paste("plots/C5_youtube.pdf",sep=""),width=width.main,height=height.main, useDingbats = F)
  dev.off()
  
  
}
