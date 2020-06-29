weibull_parameters<-function(mean,median)
{
  k=seq(0,10,0.01)
  left=gamma(1+1/k)
  right=mean/median*log(2)^(1/k)
  loc=which.min(abs(left-right))
  k<-k[loc]
  lambdaw=median/log(2)^(1/k)
  return(c(k,lambdaw))
}

lognormal_parameter<-function(mean,median)
{
  mu=log(median)
  sigma2=2*(log(mean)-mu)
  return(c(mu,sigma2))
}