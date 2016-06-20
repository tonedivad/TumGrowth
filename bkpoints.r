.hockey <- function(x,alpha1,beta1,beta2,brk,eps=diff(range(x))/100)
  ## alpha1 is the intercept of the left line segment
  ## beta1 is the slope of the left line segment
  ## beta2 is the slope of the right line segment
  ## brk is location of the break point
  ## 2*eps is the length of the connecting quadratic piece
  
  ## reference: Bacon & Watts "Estimating the Transition Between
  ## Two Intersecting Straight Lines", Biometrika, 1971
  
  ## Original function coded by Mary Lindstrom
  ## <lindstro@biostat.wisc.edu> and taken from
## S-NEWS Archive (Mon, 24 Apr 2000) available
## from (http://lib.stat.cmu.edu/s-news/Burst/15642). 
{
  x1 <- brk-eps
  x2 <- brk+eps
  b <- (x2*beta1-x1*beta2)/(2*eps)
  cc <- (beta2-b)/(2*x2)
  a <- alpha1+beta1*x1-b*x1-cc*x1^2
  alpha2 <- (a + b*x2 + cc*x2^2) - beta2*x2
  
  lebrk <- (x <= x1)
  gebrk <- (x >= x2)
  eqbrk <- (x > x1 & x < x2)
  
  result <- rep(0,length(x))
  result[lebrk] <- alpha1 + beta1*x[lebrk]
  result[eqbrk] <- a + b*x[eqbrk] + cc*x[eqbrk]^2
  result[gebrk] <- alpha2 + beta2*x[gebrk]
  result
}

#--------------------------------------------------------------------------------------------required for mergeScans
getInitPars <- function(x,y)
{
  lowLeg <- (x <= quantile(x,probs=0.5))
  init.lsfit <- lm(y~x, subset=lowLeg)$coef
  b0 <- init.lsfit[1]; b1 <- init.lsfit[2];
  init.lsfit2 <- lm(y~x, subset=!lowLeg)$coef
  b2 <- init.lsfit2[2];
  b3=-(init.lsfit[1]-init.lsfit2[1])/(init.lsfit[2]-init.lsfit2[2])
  return(c(b0, b1, b2, b3))
}

findThreshold <- function(x,y)
{
  n <- length(x)
  y=y[order(x)]
  x=x[order(x)]
  pars <- getInitPars(x=x,y=y)
  ###########################################           
  #           if(pars[1]>10)pars[1]<-7
  #           if(pars[4]>200)pars[4]<-190
  #           print(pars);
  ######################################
  alpha1 <- pars[1]; beta1 <- pars[2]; beta2 <- pars[3];
  brk <- pars[4];
  
  fit1 <- nls(y ~ .hockey(x=x,alpha1,beta1,beta2,brk),
              start=list(alpha1=alpha1,beta1=beta1,beta2=beta2,brk=brk),
              control=nls.control(maxiter=100))
  resid.scale <- sqrt(fit1$m$deviance()/(n-4))
  yhat <- fit1$m$fitted()
  Coeff <- fit1$m$getPars()
  # cat("\nCoefficients are: ",Coeff,"\n")
  brk <- Coeff[4]; beta2 <- Coeff[3]; beta1 <- Coeff[2]; alpha1 <- Coeff[1];
  Saturated <- y >= -(x - alpha1*beta1 - brk*(beta1*beta1 + 1))/beta1
  # Saturated <- Saturated | (x >= brk)
  high.row <- c(1:length(x))[Saturated]
  High.fit <- alpha1 + beta1*x[high.row]
  return(list(x=x,y=y,yhat=yhat,high.row=high.row,resid.scale = resid.scale,
              High.fit=High.fit))
}
