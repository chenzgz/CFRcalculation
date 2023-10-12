#' Garske method
#' @usage  Garske(data,t,m,sd,distr)
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
#'
#' @examples
#' data(individual)
#' data1<-sumdata(individual)
#' Garske(data1,60,25.67,71.74,1)
Garske <- function(data,t,m,sd,distr){
  pointestimate=function(data,t,m,sd,distr)
  {
    if (distr==1)
    {
      L=length(data[data$day<=t,]$day)
      sum=0
      Death=data[data$day==t,]$Death
      for (i in 1:L)
      {sum=sum+data$n[i]*pexp(data$day[L]-data$day[i],rate=1/m)}
    }

    if (distr==2)
    {
      L=length(data[data$day<=t,]$day)
      sum=0
      Death=data[data$day==t,]$Death
      for (i in 1:L)
      {sum=sum+data$n[i]*pgamma(data$day[L]-data$day[i],shape=(m^2)/(sd^2),scale=(sd^2)/m)}
    }

    if (distr==3)
    {
      m=log(m)
      sd=log(sd)
      L=length(data[data$day<=t,]$day)
      sum=0
      Death=data[data$day==t,]$Death
      for (i in 1:L)
      {sum=sum+data$n[i]*plnorm(data$day[L]-data$day[i],m,sd)}
    }
    CFR=Death/sum
    return(CFR)
  }
  # variance based on boostrap method
  firsttime=data[data$Death!=0,]$day[1]  # time when there is first death
  time=firsttime
  samp=numeric(0) # construct sample
  i=1
  repeat{
    samp[i]=pointestimate(data,time,m,sd,distr)
    time=time+1;i=i+1
    if (time>t) break
  }
  Length=length(samp)
  # repeated k times
  s=numeric(0)
  k=1000
  for (i in 1:k)
  {
    set.seed(i)
    resamples=sample(samp[!is.infinite(samp)],length(samp), replace = T)
    s[i]=mean(resamples)
  }
  CFR=pointestimate(data,t,m,sd,distr)
  var=var(s,na.rm=T)
  upper=CFR+1.96*sd(s)
  lower=CFR-1.96*sd(s)
  list("CFR"=CFR,"lower"=lower,"upper"=upper,"var"=var)
}
