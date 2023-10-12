#' Plotting of CFR curves
#' @usage  CFR_plot (CFR,time,xmin,xmax,ymax=1,lab.x="Study time",lab.y="CFR",col=ifelse(is.vector(CFR),1,c(1:ncol(CFR))),cex.lab=1.5, cex.axis=1.5,lwd=ifelse(is.vector(CFR),1,c(1:ncol(CFR))),lty=ifelse(is.vector(CFR),1,c(1:ncol(CFR))))
#' @param CFR Vector or data frame, CFR values for different methods
#' @param time time Points of interest
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
#' @examples
#' ###single method
#' Yip_CFR<-c(0.198,0.048,0.531)
#' time<-c(60,80,93)
#' CFR_plot(Yip_CFR,time,60,100)
#' ###Multiple methods
#' CFRs<-data.frame(Chen=c(0.433,0.467,0.777),Yoshikura=c(0.157,0.149,0.122),Yip.cfr=c(0.198,0.048,0.531),Garske=c(0.246,0.252,0.298),Nishiura=c(0.308,0.312,0.318),Ejima=c(0.108,0.098,0.039))
#' time<-c(60,80,93)
#' CFR_plot(CFRs,time,60,100)
CFR_plot<-function(CFR,time,xmin,xmax,ymax=1,lab.x="Study time",lab.y="CFR",col=if(is.vector(CFR))1 else c(1:ncol(CFR)),
                   cex.lab=1.5, cex.axis=1.5,lwd=if(is.vector(CFR))1 else c(1:ncol(CFR)),lty=if(is.vector(CFR))1 else c(1:ncol(CFR))){
  if (is.vector(CFR)){
    plot(time,CFR,type="l",xlim=c(xmin,xmax),ylim=c(0,ymax),xlab=lab.x,ylab=lab.y,
         cex.lab=cex.lab,cex.axis=cex.axis,lty=lty,lwd=lwd,col=col)
  }
  else{
    n<-ncol(CFR)
    if(n==1){
      plot(time,CFR,type="l",xlim=c(xmin,xmax),ylim=c(0,ymax),xlab=lab.x,ylab=lab.y,
           cex.lab=cex.lab,cex.axis=cex.axis,lty=lty,lwd=lwd,col=col)}
    else{
      plot(time,CFR[,1],type="l",xlim=c(xmin,xmax),ylim=c(0,ymax),xlab=lab.x,ylab=lab.y,
           cex.lab=cex.lab,cex.axis=cex.axis,lty=lty[1],lwd=lwd[1],col=col[1])
      for(i in 2:n){
        col1=col[i]
        lwd1=lwd[i]
        lty1=lty[i]
        lines(time,CFR[,i],col=col1,lwd=lwd1,lty=lty1)
      }
      legend("topright",legend=colnames(CFR),lwd=lwd,lty=lty,col=col,title="CFR method")
    }
  }
}
