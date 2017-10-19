#' Shear modulus of 2-phase composites using DEM.  
#'
#' @description Generates the effective shear modulus of a mix of spheroidal inclusions of phase 2 (with a specific aspect ratio)
#' in a matrix of phase 1 using the differential effective medium (DEM) theory.
#' 
#' @param k1 Bulk modulus of phase 1.
#' @param mu1 Shear modulus of phase 1.
#' @param k2 Bulk modulus of phase 2.
#' @param mu2 Shear modulus of phase 2.
#' @param asp Aspect ratio of inclusions phase 2 in a matrix of phase 1. 'asp'< 1 implies oblate inclusions, 
#' 'asp' = 1 implies spherical inclusions and 'asp' > 1 implies prolate inclusions. 
#' @param phic Critical porosity/volume fraction of phase 2, endpoint for phase 2 scaled from this value.
#' @param phi Porosity/volume fraction of phase 2.
#' 
#' @return a named dataframe whose first column is the composite shear modulus computed by DEM, each of the following 
#' columns are one of the 7 inputs that went into the model after passing the model check.
#' Note: Instead of 'k1' and 'k2', we return 'k1' and 'conK' = k1/k2, the contrast in bulk modulus between the phases. 
#' Similarly, instead of 'mu1' and 'mu2', we return 'mu1' and 'conMu' = mu1/mu2.
#' 
#' @importFrom deSolve radau
#' 
#' @export
#' 
#' @examples
#' k1<- (rnorm(100, 36, 10))
#' mu1<-(rnorm(100, 45, 10))
#' k2<- (rnorm(100, 36, 10))
#' mu2<- (rnorm(100, 45, 10))
#' asp<- (rnorm(100, 1, .5))
#' phic<- (rnorm(100, .4, .1))
#' phi<- (rnorm(100, .25, .1))
#' df <- compositeShear(k1,mu1,k2,mu2,asp,phic,phi)
#'

##
## Begin Priyanka Dutta code
##

compositeShear <- function(k1,mu1,k2,mu2,asp,phic,phi){
  
    # check if all input vectors are of equal length
    if(!all(sapply(list(mu1,k2,mu2,asp,phic,phi), function(x) length(x)==length(k1)))){
      stop("Input vectors need to be of equal length. Please check inputs!")
    }
  
    # check for valid physical inputs. Discard ones that are not.
    cat("Checking inputs... \n" )
    idx = length(k1)
    k1 <- k1[k1>1];mu1 <- mu1[mu1>0];k2 <- k2[k2>1]; mu2 <- mu2[mu2>0]
    asp <- asp[asp>0.001];phic <- phic[phic>0];phi = phi[phi>0]
    l <- min(sapply(list(k1,mu1,k2,mu2,asp,phic,phi), function(x) length(x)))
    fin <- lapply(list(k1,mu1,k2,mu2,asp,phic,phi), function(x) x = x[1:l])
    names(fin) <- c("k1","mu1","k2","mu2","asp","phic","phi")
    if (l<idx) {cat("No. of input sets that did not pass check = ",idx-l,"\n" )}
      
  
    dem <- function(k1,mu1,k2,mu2,asp,phic,phi){
    
    yout <- radau(c(k1, mu1), seq(0, 0.9999, length.out = 100), demYprime,atol = 1e-10, parms = c(k1,mu1,k2,mu2,asp,phic,phi))
  
    # bulk modulus of composite
    k <- Re(yout[,2])
    # shear modulus of composite
    mu <- Re(yout[,3])
    
    # scale endpoint for phase 2 from 'phic'
    por <- phic*yout[,1]
    if (phi>phic) {phi <- phic}
    porc <- which.min(abs(por - phi))
  
    # return the final shear modulus 
    return(mu[porc])
    }
    
    
    demYprime <- function(t,y,p) {
    # This is the differntial equation solved by 'radau' to get DEM estimates of composite bulk and shear modulus
      
    k1 <- p[1]; mu1 <- p[2]; k2 <- p[3]; mu2 <- p[4]
    asp <- p[5]; phic <- p[6]
  
    krc <- k1*k2/((1-phic)*k2 + phic*k1)
    murc <- mu1*mu2/((1-phic)*mu2 + phic*mu1)
  
    ka <- k2; mua <- mu2
  
    k <- y[1]; mu <- y[2]
    yprime <- c(0,0);
    
    if (asp==1) {asp=0.99}
    
    # 'P' and 'Q' values: geometric factors for a spheroidal inclusions of phase 2 in a matrix of phase 1
    if (asp < 1) {
      theta <- (asp/((1-asp^2)^(3/2)))*(acos(asp) -asp*sqrt(1-asp^2))
      fn <- (asp^2/(1-asp^2))*(3*theta -2)
    }
  
    if (asp > 1) {
      theta <- (asp/((asp^2-1)^(3/2)))*(asp*sqrt(asp^2-1)-acosh(asp))
      fn <- (asp^2/(asp^2-1))*(2-3*theta)
    }
  
    nu <- (3*k-2*mu)/(2*(3*k+mu))
    r <- (1-2*nu)/(2*(1-nu))
    a <- mua/mu -1
    b <- (1/3)*(ka/k -mua/mu)
  
  
    f1a <- 1+a*((3/2)*(fn+theta)-r*((3/2)*fn+(5/2)*theta-(4/3)))
  
    f2a <- 1+a*(1+(3/2)*(fn+theta)-(r/2)*(3*fn+5*theta))+b*(3-4*r)
    f2a <- f2a+(a/2)*(a+3*b)*(3-4*r)*(fn+theta-r*(fn-theta+2*theta^2))
  
    f3a <- 1+a*(1-(fn+(3/2)*theta)+r*(fn+theta))
  
    f4a <- 1+(a/4)*(fn+3*theta-r*(fn-theta))
  
    f5a <- a*(-fn+r*(fn+theta-(4/3))) + b*theta*(3-4*r)
  
    f6a <- 1+a*(1+fn-r*(fn+theta))+b*(1-theta)*(3-4*r)
  
    f7a <- 2+(a/4)*(3*fn+9*theta-r*(3*fn+5*theta)) + b*theta*(3-4*r)
  
    f8a <- a*(1-2*r+(fn/2)*(r-1)+(theta/2)*(5*r-3))+b*(1-theta)*(3-4*r)
  
    f9a <- a*((r-1)*fn-r*theta) + b*theta*(3-4*r)
  
  
    pa <- 3*f1a/f2a
    qa <- (2/f3a) + (1/f4a) +((f4a*f5a + f6a*f7a - f8a*f9a)/(f2a*f4a))
    pa <- pa/3.; qa <- qa/5.
  
    krhs <- (ka-k)*pa
    yprime1 <- krhs/(1-t)
  
    murhs <- (mua-mu)*qa
    yprime2 <- murhs/(1-t)
  
    return(list(c(yprime1,yprime2)))
  }
  
  
    # compute composite shear modulus 'mu' for multiple sets of inputs
    cat("Computing 2-phase composite shear with ",l,"of ", idx, "inputs... \n" )
    mu <- unlist(lapply(1:l, function(i) dem(fin$k1[i],fin$mu1[i],fin$k2[i],fin$mu2[i],fin$asp[i],fin$phic[i],fin$phi[i])))
  
    # return a named data frame with the DEM composite shear modulus as the first column, followed by each of the 7 inputs that went into the model
    # instead of 'k1' and 'k2', we return 'k1' and 'conK' = k1/k2, the contrast in bulk modulus between the phases. Similarly, instead of 'mu1' and 'mu2', 
    # we return 'mu1' and 'conMu' = mu1/mu2
    return(data.frame(mu.out = mu, k1 = fin$k1, mu1 = fin$mu1, conK = fin$k1/fin$k2, conMu = fin$mu1/fin$mu2, asp = fin$asp, phic = fin$phic, phi = fin$phi))
  
}

##
## End Priyanka Dutta code
##


