#' Ejima method
#' @usage Ejima(data,t,m,sd,distr=1)
#' @param data summarized survival data
#' @param t the study time.
#' @param m Mean survival time of dead individuals
#' @param sd Variance of survival time of dead individuals
#' @param distr Distribution of survival time of dead individuals
#'
#' @return A list containing the following components:
#' \item{CFR}{CFR}
#' \item{lower}{the lower of CFR's CI}
#' \item{upper}{the upper of CFR's CI}
#' \item{var}{the variance of CFR}
#' @examples
#' data(individual)
#' data1<-sumdata(individual)
#' Ejima(data1,60,25.67,71.74,1)
Ejima <-  function(data,t,m,sd,distr=1){
  if (is.element("Bhat", installed.packages()[,1])==FALSE){
    install.packages("Bhat")
  }
  require(Bhat) #same as library statement
  library(Bhat)
  #### estimate the exponential growth rate, r (Nishiura et al. 2009)
  C <- function(time) data[data$day==time,]$Conf
  CI <- 0
  for (i in data$day[1]:(t-1)) {CI <- CI+C(i)}
  f <- function(r)  {-CI*(1-exp(-r))+(C(t)-C(data$day[1]))*exp(-r)}
  r <- uniroot(f,lower=0,upper=1)[[1]]

  #### calculate the moment-generating function, which is referred as factor of
  #### underestimation in the paper (Nishiura et al. 2009)
  if (distr==1)		u <- 1/(1+r*m)	#	for exponential distribution
  if (distr==2)		{v<- sd/m		# 	coefficient of variation
  u=(1+r*m*v^2)^(-1/(v^2))}
  B <- ((exp(r*t)-1)*u)/r - data[data$day==t,]$Death
  Conf=data[data$day==t,]$Conf
  Death=data[data$day==t,]$Death

  nlogf <- function(x) # negative loglikelihood
  {loglik <- -(Death*log(x)+B*log(1-x))
  return (loglik)
  }
  x =list(label=c("p"),est=c(0.1),low=c(0),upp=c(0.99))
  r = dfp(x,f=nlogf)
  x$est<-r$est
  if (is.na(try( plkhci(x,nlogf,'p',prob=0.95))[2]))
  {CFR=NA;lower=NA;upper=NA;var=NA}
  else {
    interval<-plkhci(x,nlogf,'p',prob=0.95)
    lower=interval[1]
    upper=interval[2]
    CFR=x$est
  }
  list("CFR"=CFR,"lower"=lower,"upper"=upper,"var"=NA)
}
