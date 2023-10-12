#' Chen-Nakamura method
#' @usage Chen(data,t)
#' @param data individual survival data transformed by sumdata
#' @param t the study time
#'
#' @return A list containing the following components:
#' \item{CFR}{CFR}
#' \item{lower}{the lower of CFR's CI}
#' \item{upper}{the upper of CFR's CI}
#' \item{var}{the variance of CFR}
#' @references Zheng Chen. Estimating the case fatality rate using a constant cure-death hazard ration. Lifetime Data Anal. 2009,15,316-329.
#' @examples
#' data(individual)
#' data1<-sumdata(individual)
#' Chen(data1,60)
Chen <- function(data,t){
  sumc=sum(data[data$day<=t,]$c)
  sumd=sum(data[data$day<=t,]$d)
  theta=sumc/sumd
  CFR=1/(1+theta)
  avartheta=sumc*(sumc+sumd)/((sumd)^3);
  avar=avartheta/((1+theta)^4)
  sd=avar^(1/2)
  upper=CFR+1.96*sd
  lower=CFR-1.96*sd
  list("CFR"=CFR,"lower"=lower,"upper"=upper,"var"=avar)
}
