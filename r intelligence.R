# Script for simulating and analyzing 100 datasets with model parameter values as follows
n <- 6977

rpre <- 0.680
rpost <- 0.772
redu <- 0.525
aedu <- 3.238
apre <- 1.379
apost <- 0

Phi <- seq(0,1,length.out = n)

dfres <- data.frame(t(sapply(1:100, function(i){
 
  set.seed(i)
 
  Itrue <- sort(scale(rnorm(n)))
 
  edu <- scale(Itrue + rnorm(n)*sqrt(1-redu^2))
 
  Ipre <- rpre*Itrue + (1+apre*Phi)*rnorm(n)*sqrt((1-rpre^2)/(1+apre+(apre^2)/3))
  Ipost <- rpost*Itrue + (1+apost*Phi)*rnorm(n)*sqrt((1-rpost^2)/(1+apost+(apost^2)/3))
  Iedu <- redu*Itrue + (1+aedu*Phi)*rnorm(n)*sqrt((1-redu^2)/(1+aedu+(aedu^2)/3))
  Ix <- Iedu*Ipre - mean(Iedu*Ipre)
 
  change_analysis <- lm(Ipost-Ipre ~ Iedu)
  control_analysis <- lm(Ipost ~ Iedu + Ipre)
  interaction_analysis <- lm(Ipost ~ Iedu + Ipre + Ix)

  b_change <- change_analysis$coefficients[2]
  b_control <- control_analysis$coefficients[2]
  b_interaction <- interaction_analysis$coefficients[4]
 
  return(c(b_change, b_control, b_interaction))

})))

names(dfres) <- c("b_change", "b_control", "b_interaction")
summary(dfres)