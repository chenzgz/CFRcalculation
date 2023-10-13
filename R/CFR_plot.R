#' Plotting of CFR curves
#' @usage  CFR_plot (data,method,time=data$day,m=NULL,sd=NULL,distr=NULL,xmin,xmax,ymax=1,lab.x="Study time",lab.y="CFR",col=c(1:length(method)),cex.lab=1.5, cex.axis=1.5,lwd=c(1:length(method)),lty=c(1:length(method)))
#' @param data summarized survival data
#' @param method Different methods of calculating the CFR. specifically "Yip","Chen","Garske","Nishiura","Ejima"
#' @param time time Points of interest
#' @param m Mean survival time of dead individuals. Methods "Garske", "Nishiura" and "Ejima"are required.
#' @param sd Variance of survival time of dead individuals.Methods "Garske", "Nishiura" and "Ejima"are required.
#' @param distr Distribution of survival time of dead individuals.Methods "Garske", "Nishiura" and "Ejima"are required.
#' @param xmin Minimum value of the horizontal coordinate
#' @param xmax Maximum value of horizontal coordinate
#' @param ymax Maximum value of vertical coordinate
#' @param lab.x Horizontal Label
#' @param lab.y Vertical Label
#' @param col "col" parameters in function plot
#' @param cex.lab "cex.lab" parameters in function plot
#' @param cex.axis "cex.axis" parameters in function plot
#' @param lwd "cex.axis" parameters in function plot
#' @param lty "cex.axis" parameters in function plot
#'
#' @examples
#' data(individual)
#' data1<-sumdata(individual)
#' CFR_plot(data=datasum,method=c("Yip","Chen","Garske"),m=25.67,sd=71.74,distr=2,xmin=0,xmax=100)
#' CFR_plot(data=datasum,method=c("Yip","Chen"),xmin=0,xmax=100)
CFR_plot<-function(data,method,time=data$day,m=NULL,sd=NULL,distr=NULL,xmin,xmax,ymax=1,lab.x="Study time",lab.y="CFR",col=c(1:length(method)),
                   cex.lab=1.5, cex.axis=1.5,lwd=c(1:length(method)),lty=c(1:length(method))){
  n<-length(time)
  Yip_method<-c()
  Chen_method<-c()
  Garske_method<-c()
  Nishiura_method<-c()
  Ejima_method<-c()
  for (i in 1:n){
    if ("Yip" %in% method )
      Yip_method[i]<-Yip.cfr(data,time[i],B=10)$CFR
    if ("Chen" %in% method )
      Chen_method[i]<-Chen(data,time[i])$CFR
    if("Garske" %in% method )
      Garske_method[i]<-Garske(data,time[i],m,sd,distr)$CFR
    if ("Nishiura" %in% method )
      Nishiura_method[i]<-Nishiura(data,time[i],m,sd,distr)$CFR
    if ("Ejima"%in% method)
      Ejima_method[i]<-Ejima(data,time[i],m,sd,distr)$CFR
  }
  CFR<-cbind(Yip_method,Chen_method,Garske_method,Nishiura_method,Ejima_method)
  #colnames(CFR)<-c("Yip_method","Chen_method","Garske_method","Nishiura_method","Ejima_method")
  CFR<-as.data.frame(CFR)
  #CFR<-na.omit(CFR)
  n1<-ncol(CFR)
  if(n1==1){
    plot(time,CFR,type="l",xlim=c(xmin,xmax),ylim=c(0,ymax),xlab=lab.x,ylab=lab.y,
         cex.lab=cex.lab,cex.axis=cex.axis,lty=lty,lwd=lwd,col=col)}
  else{
    plot(time,CFR[,1],type="l",xlim=c(xmin,xmax),ylim=c(0,ymax),xlab=lab.x,ylab=lab.y,
         cex.lab=cex.lab,cex.axis=cex.axis,lty=lty[1],lwd=lwd[1],col=col[1])
    for(i in 2:n1){
      lines(time,CFR[,i],col=col[i],lwd=lwd[i],lty=lty[i])
    }
    legend("topright",legend=colnames(CFR),lwd=lwd,lty=lty,col=col,title="CFR method")
  }
}
